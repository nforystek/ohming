//#define DEBUG
#define IRREMOTE 
#define DISPLAY


#ifdef DISPLAY
#include <SPI.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define SCREEN_WIDTH 128 // OLED display width, in pixels
#define SCREEN_HEIGHT 64 // OLED display height, in pixels

#define OLED_RESET    4 // Reset pin # (or -1 if sharing Arduino reset pin)
#define SCREEN_ADDRESS 0x3C ///< See datasheet for Address; 0x3D for 128x64, 0x3C for 128x32
Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, OLED_RESET);
#endif

#include <Stepper.h>
#ifdef IRREMOTE 
#include <IRremote.h>
#endif



long StepSpeed=64;
int StepRevol=16;

Stepper myStepper(360,8,10,9,11);
#ifdef IRREMOTE 
IRrecv irrecv(13); 
decode_results results; 
#endif

unsigned long elapse=millis(); // holding modifier for elapse periods of latency/adjust
long millisInSec=1000; //how many milliseconds in a second, 1000 or 1024 in a cpu evening
long goSpeed=64;  // RPM, or seconds in a minute, 60 or 64 in a cpu evening
long latency=0; // millisecond audit of the stepper call, latency+adjust = 1 loop cycle
long adjust=0; // millisecond audit of everything around call to call of the stepper
int nuSteps=0; // currency of steps used when calling the stepper step function
long delayMS = 16; //offset of prediction of two calls per loop cycle to include change
long ticker=0; // tracking for how many loop cycles happen with in one second of time
long tape=0; // accumulative mmilliseconds passed before reset at over a second of time
long tickerTape=0; // last, or total loops per one second of time that has occured

unsigned long timer=0;
int seconds=0;
int minutes=0;
int duration=0;
byte opmode=0;
byte dstate=0;
byte damper=0;
bool enable=false;

void setup() {
  myStepper.setSpeed(StepSpeed);
  #ifdef IRREMOTE 
  // Start the receiver
  irrecv.enableIRIn(); 
  #endif
  #ifdef DEBUG
  Serial.begin(2000000);
  #endif
  
  #ifdef DISPLAY
  if(!display.begin(SSD1306_SWITCHCAPVCC, SCREEN_ADDRESS)) {
    Serial.println(F("SSD1306 allocation failed"));
    for(;;); // Don't proceed, loop forever
  }  
  dispStatus();
  #endif 
}
#ifdef DISPLAY
void dispStatus() {
  display.clearDisplay();
 
  display.setTextSize(1);
  display.setTextColor(SSD1306_WHITE);
  display.setCursor(0,25);    
  if (enable) {
    switch (duration) {
      case 0:
        display.println(F("Mode: Holding"));
        break;
      case 2:
        display.println(F("Mode: Slow Start"));
        break;
      case 3:
        display.println(F("Mode: Slow Stop"));
        break;
      default:
        display.println(F("Mode:  Automatic"));
        break;  
    }
    display.setCursor(0,35);

    if (opmode==1) {      
      display.print(F("Time: "));
      
      if (((((int)(duration/10))*10)-(minutes+1))<10)
        display.print(F("0"));
      display.print(((((int)(duration/10))*10)-(minutes+1)));

      display.print(F(":"));   
         
      if ((-60-seconds+120)<10)
        display.print(F("0"));
      display.println((-60-seconds+120));
      display.setCursor(0,48);
    }
    
    display.print(F("Speed: "));
    display.println((
      (-9-damper+18)+1));

  } else {
    display.println(F("Mode: Off"));
  }
  display.display();  
}
#endif
void loop() {
   
  if (enable) {
   
    //get the adjust value based off the elapse, excluding elapsed since it
    adjust=(long)((((adjust+millis()-elapse)+(elapse-adjust)))-elapse);    
  
    //immediate after adjusted, start main elapse gain
    elapse = millis(); //reseting elapse for loop cycle
    
    //make a stepper call, changed to this with speed should
    myStepper.step(-(StepRevol)); // only be done with wiggle room opened 
    
    //get single call latency for the dummy call delay to open
    delayMS=(long)(millis()-elapse); //more wiggle room grace    
    
    #ifdef IRREMOTE 
    if (damper!=dstate)
    {
      myStepper.setSpeed(StepSpeed-((damper-1)*6));
      dstate = damper;
    }
    #endif
    
    //get double call latency for as a cycle
    latency=(long)((millis()-delayMS)-elapse);

    //should be at one or two depending on
    delay(latency);//the hardware yielding
    
    #ifdef DEBUG
    //these values should be under 10
    //and bouncing off to zero often
    Serial.print(delayMS);
    Serial.print(' ');
    Serial.print(StepRevol);
    Serial.print(' ');
    Serial.print(latency);
    Serial.print(' ');
    Serial.println(adjust);
    #endif

    if ((latency>0)||(adjust==0)) StepRevol++;
    if ((delayMS>0)&&(StepRevol>0)) StepRevol--;

    //get determinate of our wiggle room grace, when up, change is okay
    delayMS=(latency/2);// to be posture occurs

    if (elapse-timer>millisInSec) {
      timer=elapse;
      seconds++;
      if (seconds>=60) {
        seconds=0;
        minutes++;
        MinuteInterval();
        if (minutes>=10) minutes=0;
      }
      #ifdef DISPLAY
      dispStatus();
      #endif 
    }
  }
  
  #ifdef IRREMOTE 
  ReadKey();
  #endif

}
void MinuteInterval() {
  if (opmode==1) {        
    if ((duration==0)&&(damper==9))
      duration = 60;
    else if ((duration==0)&&(damper==0))
      opmode=0;
    else {
      if ((duration>49)&&(damper>0))
        damper--;
      else if ((duration<11)&&(damper<9))
        damper++;
      duration--;
      if ((duration==0)&(damper==9)) opmode=0;
    }
      
  } else if (opmode>1) {
    if ((opmode==2)&&(damper>0))
      damper--;
    else if ((opmode==3)&&(damper<9))
      damper++;
    else
      opmode=0;
  }
}
#ifdef IRREMOTE 
void ReadKey() {
    if (irrecv.decode(&results)) {
      switch(results.value) {
        //Direct to speed controls
        case 0xFF6897: damper=9;     break;
        case 0xFF30CF: damper=8;     break;
        case 0xFF18E7: damper=7;     break;
        case 0xFF7A85: damper=6;     break;
        case 0xFF10EF: damper=5;     break;
        case 0xFF38C7: damper=4;     break;
        case 0xFF5AA5: damper=3;     break;
        case 0xFF42BD: damper=2;     break;
        case 0xFF4AB5: damper=1;     break;
        case 0xFF52AD: damper=0;     break;
        case 0xFF9867: //EQ
          if (!enable) TogglePower();
          break;
        case 0xFF629D: //VOL+ - gradual increase to full
          opmode=2;
          if (!enable) TogglePower();
          break;
        case 0xFFA857: //VOL- - gradual decrease to stop
          opmode=3;
          break;          
        case 0xFF906F://UP - go faster one damper
          if (damper>0) damper = damper-1;
          break;
        case 0xFFE01F://DOWN - go slower one damper
          if (damper<9) damper = damper+1;
          break;
        case 0xFF22DD: //FAST FORWARD - increate run minutes
                        //for 3 stages of ST/REPT runtimes
          if (duration>0) duration++;
          break;
        case 0xFFC23D: //FAST BACK - decrease run minutes
                      //for 3 stages of ST/REPT runtimes
          if (duration>0) duration--;
          break;
        case 0xFF02FD: //PAUSE - Hold at current speed
                    //stops and ST/REPT from continues
          opmode=0;
          break;         
        case 0xFFB04F: //ST/REPT - slow to climb, hold for
                        //while slow to decline and stop                        
          opmode=1;
          if (duration==0) duration = 60;
          if (!enable) TogglePower();
          break;
        case 0xFFE21D: //FUNC/STOP - when ST/REPT is on
                        //this must be used to stop early  
          opmode=1; 
          if (duration>10) duration = 10;   
          break;
        case 0xFFA25D: //POWER - Immediate Power on/off
          TogglePower();
          break;
     } 
      #ifdef DISPLAY
      dispStatus();
      #endif
     irrecv.resume(); 
    }
}
#endif
void TogglePower() {
  if (enable) {
    myStepper.setSpeed(0);
    duration=0;
    damper=0;
    minutes=0;
    seconds=0;
    opmode=0;
    dstate=0;
  } else {
    myStepper.setSpeed(StepSpeed);
    damper=9;
  }
  enable=!enable;  
}
