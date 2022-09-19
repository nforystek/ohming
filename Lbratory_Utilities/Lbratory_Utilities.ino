
#include <Stepper.h>
#include <math.h>
#include "SR04.h"

SR04 sr04 = SR04(13,12);


const byte  mag410 =(byte)0;
const byte  mag1025= (byte)1;
const byte  mag4065= (byte)2;
const byte mag100125 =(byte)3;

const byte XAxisInc =(byte)1;
const byte XAxisDec=(byte)2;
const byte XAxis =(byte)0;

const byte YAxisInc =(byte)3;
const byte YAxisDec=(byte)4;
const byte YAxis =(byte)1;

const byte ZAxisInc =(byte)5;
const byte ZAxisDec =(byte)6;
const byte ZAxis =(byte)2;

const byte DAxisInc =(byte)7;
const byte DAxisDec =(byte)8;
const byte DAxis =(byte)3;

const byte LAxisInc =(byte)9;
const byte LAxisDec =(byte)10;
const byte LAxis =(byte)4;

bool soniccherp=false;
unsigned long lastTime;
unsigned long lastTime2;

unsigned long runtime;
long seconds;
long minutes=-1;
long hilominutes=6;
long runminutes=30;
unsigned long  elapse;
unsigned long  latency;
unsigned long adjust;
int state=3;
long current=0;

float nuSpeed;
float nuSteps;

const long iniSpeed =10; 
const long maxSpeed =31; 
const int stepRev =720; 

int wind=0;
int Winding1 = 0x1001001001001;
int Winding2 = 0x0100100100100;
int Winding3 = 0x0010010010010;

int tray = 0;
int maxTraySpeed=31;
byte activeMotor=0;
bool activeCont=true;
bool tumbler=false;
int thermistor=0;


// centrifudge

Stepper myStepper(stepRev, 11,10,9,8);// 4, 6, 5, 3);


int latchPin = 6;//11;
int clockPin = 5;//9;
int dataPin = 4;//12;

byte activeSet=0;


int getSpeed2(byte axis) {

    switch (activeSet) {
        case mag410:
            switch (axis)
            {
               case XAxis:
                  return 10;
                break;
               case YAxis:
                  return 10;
                break;
               case ZAxis:
                  return 20;
                break;
               case LAxis:
                  return 20;
                break;
               case DAxis:
                  return 10;
                break;
            }
          break;
        case mag1025:
            switch (axis)
            {
               case XAxis:
                  return 5;
                break;
               case YAxis:
                  return 5;
                break;
               case ZAxis:
                  return 15;
                break;
               case LAxis:
                  return 15;
                break;
               case DAxis:
                  return 5;
                break;
            }
          break;
        case mag4065:
            switch (axis)
            {
               case XAxis:
                  return 1;
                break;
               case YAxis:
                  return 1;
                break;
               case ZAxis:
                  return 10;
                break;
               case LAxis:
                  return 10;
                break;
               case DAxis:
                  return 1;
                break;
            }
          break;
        case mag100125:
            switch (axis)
            {
               case XAxis:
                  return 1;
                break;
               case YAxis:
                  return 1;
                break;
               case ZAxis:
                  return 5;
                break;
               case LAxis:
                  return 5;
                break;
               case DAxis:
                  return 1;
                break;
            }
          break;
    }

   /*unsigned long pastTime=millis()-lastTime;
   lastTime = millis();
   int nuSpd = -(maxTraySpeed+((maxTraySpeed/100)*(1000*(maxTraySpeed/1000)*(maxTraySpeed/100))))+(maxTraySpeed*2);
   if (nuSpd<1) return 1;
   else return nuSpd;  */
}

void setup() {
  
  pinMode(latchPin, OUTPUT);
  pinMode(dataPin, OUTPUT);  
  pinMode(clockPin, OUTPUT);

  
  activeMotor=0;
  activeSet=mag410;
  
  pinMode(2, OUTPUT);
  digitalWrite(2, LOW);
  Serial.begin(115200);


}

void loop() {

  ReadKey();
  Centrifuge();
  //Microscope();
 // SonicCherp();
 // Temperature();
}


void updateShiftRegister()
{

   digitalWrite(latchPin, LOW);
   byte motors=0;
   if (!activeMotor==0) bitSet(motors,activeMotor-1);   
   shiftOut(dataPin, clockPin, LSBFIRST, motors);
   digitalWrite(latchPin, HIGH);
}


void SonicCherp()
{

   //if( (soniccherp)&&((activeMotor==0)&&(tray==-1))) {
    if(soniccherp) {
    Serial.print('S');
    Serial.println(sr04.Distance());
   }
}

void Temperature(){
  if (thermistor==1) {
    Serial.print('T');
    int raw = analogRead(0);
    float val = map(raw, 0, 1023, 0.0, 150.0);
    Serial.println(val);
  }else if (thermistor==2) {
    Serial.println('T');
    thermistor=0;
  }
}
void ChangeMotor(byte motor) {
    if (activeMotor!=motor) {
      activeMotor=motor;        
      updateShiftRegister();
      if (motor!=7) tumbler=false;
    }

}

void Microscope() {
   unsigned long pastTime=millis()-lastTime;
   if (pastTime>0) {
      lastTime = millis();
      
      switch (tray) {
        case XAxisInc:
          ChangeMotor(2);
          myStepper.setSpeed(getSpeed2(XAxis));
          myStepper.step(10);
          if (!activeCont) tray=0;
          break;
        case XAxisDec:
          ChangeMotor(2); 
          myStepper.setSpeed(getSpeed2(XAxis));
          myStepper.step(-10);
          if (!activeCont) tray=0;
          break;
          
        case YAxisInc:
          ChangeMotor(3);
          myStepper.setSpeed(getSpeed2(YAxis));
          myStepper.step(-10);
          if (!activeCont) tray=0;
          break;
        case YAxisDec:
          ChangeMotor(3);
          myStepper.setSpeed(getSpeed2(YAxis));
          myStepper.step(10);
          if (!activeCont) tray=0;
          break;
    
        case LAxisInc:
          ChangeMotor(4);
          myStepper.setSpeed(getSpeed2(LAxis));
          myStepper.step(-70); 
          if (!activeCont) tray=0;
          break;
        case LAxisDec:
          ChangeMotor(4);
          myStepper.setSpeed(getSpeed2(LAxis));
          myStepper.step(70);
          if (!activeCont) tray=0;
          break;  
    
        case DAxisInc:
          ChangeMotor(5);
          myStepper.setSpeed(getSpeed2(DAxis));
          myStepper.step(-1);
          if (!activeCont) tray=0;
          break;
        case DAxisDec:
          ChangeMotor(5);
          myStepper.setSpeed(getSpeed2(DAxis));
          myStepper.step(1);
          if (!activeCont) tray=0;
          break;

        case ZAxisInc:
          ChangeMotor(6);
          myStepper.setSpeed(getSpeed2(ZAxis));
          myStepper.step(-1);
          if (!activeCont) tray=0;
          break;
        case ZAxisDec:
          ChangeMotor(6);
          myStepper.setSpeed(getSpeed2(ZAxis));
          myStepper.step(1);
          if (!activeCont) tray=0;
          break;
          
        default:
          
          if (tray==0) {
              tray--;
              ChangeMotor(0);
          }
          
          break;
      }
   }
   if ((tray>0)&&activeCont) {
      Serial.println('q');
   }
}


void Begin() {
  ChangeMotor(1);
   if ((state==0)||(state>2)) {
      elapse=millis();
      runtime=millis();
      adjust=0;
      seconds=0;
      minutes=0;
      state=0;
      current=1;
   }
}

void Finish() {
  ChangeMotor(0);
   if (state<=2) {
        current=0;
        state=2;  
        minutes=-1;
   }
}

void Tumbler() {
  tumbler=true;
  ChangeMotor(7);
  minutes=-maxSpeed;
  state=0;  
}


void FullPower() {
  ChangeMotor(1);
  minutes=-maxSpeed;
  state=0;  
}

void Increase() {
  if (minutes<-1) minutes--;
  else {
    minutes = -2;
    state=0;
  }
}

void Decrease() {
  //ChangeMotor(1);
  if (minutes<-1) minutes++;
  else {
      minutes = -2;
      state=0;
  }
}

void Automate() {
  ChangeMotor(1);
  if (minutes<0)
    Begin();
  else
    Finish();
}

void PowerOff() {
  ChangeMotor(1);
  Finish();
  tray=0;
}

void Centrifuge() {  
  adjust=millis()-adjust;
  switch (state)
  {
    case 0:
      stepMotor(0);
      state++;
      break;
    case -1:
    case 1:
      latency = stepMotor((millis()-elapse)/2);
      //adjust=(long)((((adjust+millis()-elapse)+(elapse-adjust)))-elapse);  
      elapse=(millis()-elapse);
      state--;
      break;
    case 2:
      myStepper.step(0);
      state++;
      break;
  }
  if ((state<2)&&(minutes>=0)) {
    if (((millis())-runtime)>1000) {
      seconds++;
      runtime=millis();
      if (seconds==60) {
        minutes++;
        seconds=0;
        if (minutes==runminutes) Finish();
      }
    }
  } else if (seconds!=0) seconds=0;
  
  //SerialPrint(" current ");
  //SerialPrint(current, 6);
  
  //SerialPrint(" state ");
  //SerialPrint(state, 2);
  
  //SerialPrint(" minutes ");
  //SerialPrint(minutes, 6);
  //SerialPrint(" seconds ");
  //SerialPrint(seconds, 6);
  
  //SerialPrint(" latency ");
  //SerialPrintln(latency, 6);
 // adjust=millis();
}

long getSpeed() {
  if (minutes<-1) {
      return (-minutes<=maxSpeed)?-minutes:maxSpeed;
  }else if (minutes<hilominutes) {
    if (iniSpeed+(long)((float)(maxSpeed-iniSpeed)*(float)((float)minutes/(float)hilominutes))>=1)
      return iniSpeed+(long)((float)(maxSpeed-iniSpeed)*(float)((float)minutes/(float)hilominutes));
     else
      return iniSpeed;
  } else if (minutes<hilominutes+runminutes) {
      return maxSpeed;
  } else if (minutes<(hilominutes*2)+runminutes) {
    if (iniSpeed+(long)((float)(maxSpeed-iniSpeed)*(float)(((float)((hilominutes*2)+runminutes)-minutes)/(float)hilominutes))>=1)
      return iniSpeed+(long)((float)(maxSpeed-iniSpeed)*(float)(((float)((hilominutes*2)+runminutes)-minutes)/(float)hilominutes));
  }
  return( (minutes>=-1)?1:0);
}

unsigned long stepMotor(unsigned long newLatent) {

  if (state<2) {

    if (state!=-1) {
        if ((latency==newLatent)||(newLatent<maxSpeed)) {
          current++;
        } else if (latency<newLatent) {
          current--;
        } else if (newLatent>(maxSpeed*2)) {
            current = (current/2);
            nuSteps--;
        }
    } else {
      nuSteps = (stepRev/1000)/(stepRev/(1000/((newLatent+latency)/2)));

    }

    if (minutes!=-1) {

      if (nuSteps+current<1) current=0;
      if (nuSteps<1) nuSteps=1; 
      if (state!=0) {        
        myStepper.setSpeed(getSpeed());
      }
     // SerialPrint(" speed ");
     // SerialPrint(getSpeed(),6);
  
    //  SerialPrint(" steps ");
    //  SerialPrint(nuSteps+current,6);
     // myStepper.step(tumbler?(nuSteps+current):-(nuSteps+current));
      myStepper.step((nuSteps+current));
    }

  } else{ Finish();}

  return newLatent;
}

void ReadKey() {

  if (Serial.available())
  {
    char ch=Serial.read();
    switch (ch)
    {   
      case 'n'://north - microscope
        /*
         * Moves the microscope tray forward orientated
         * to the microscope its self releative compass.
         */
         tray=(tray==YAxisInc)?0:YAxisInc;
         if (tray==0) ChangeMotor(0);
         break;
      case 's'://south - microscope
        /*
         * Moves the microscope tray backward orientated
         * to the microscope its self releative compass.
         */
         tray=(tray==YAxisDec)?0:YAxisDec;  
        if (tray==0) ChangeMotor(0);
         break;
      case 'w'://west - microscope
        /*
         * Moves the microscope tray left orientated
         * to the microscope its self releative compass.
         */
         tray=(tray==XAxisDec)?0:XAxisDec; 
         if (tray==0) ChangeMotor(0);
         break;
      case 'e'://east - microscope
        /*
         * Moves the microscope tray right orientated
         * to the microscope its self releative compass.
         */
         tray=(tray==XAxisInc)?0:XAxisInc; 
         if (tray==0) ChangeMotor(0);
         break;
      case 'i'://increase - centrifudge
        /*
         * Halts any automation maintaining current speed,
         * and/or increases the speed slight ly to stable.
         */
         tray=-1;
         Increase();
         break;
      case 'd'://decrease - centrifudge
        /*
         * Halts any automation maintaining current speed,
         * and/or decreases the speed slight ly to stable.
         */
         tray=-1;
         Decrease();
         break;
      case 'a'://automate - centrifudge
        /*
         * From stall, this is a slow gain for set amount
         * of pick up minutes, that will be same as slow
         * down length of time, after at high speed to a
         * duration of run time.  Should be intuitive up.
         */   
         tray=-1; 
         Automate();
         break;     
      case 'p'://poweroff - soniccherp-on
        /*
         * Any lab steppers should be fully powered off
         * and the soniccherp watching portion powered on.
         */   
         tray=-1;
         PowerOff();
         break;
      case 'f'://full power
        /*
         * Turns the centrifudge on full power from
         * any state, canceling any active automate.
         */  
         tray=-1;
        FullPower();
        break;
      case 'v'://tumbler
        /*
         * Turns the tumbler on full power on
         * a full power centrifuge, backwards.
         */  
         tray=-1;
        Tumbler();
        break;
      case 'r'://dialate - microscope
        /*
         * Dialate the lense light by moveing it up
         * and down to increase teh radius fractal.
         */   
         tray=(tray==DAxisInc)?0:DAxisInc;
         if (tray==0) ChangeMotor(0);
         break;
      case 'k'://dialate - microscope
        /*
         * Dialate the lense light by moveing it up
         * and down to increase teh radius fractal.
         */   
         tray=(tray==DAxisDec)?0:DAxisDec;
         if (tray==0) ChangeMotor(0);
         break;
      case 'b'://z-axis - microscope
        /*
         * Move the tray up in the visual perspective
         */   
         tray=(tray==ZAxisInc)?0:ZAxisInc;
         if (tray==0) ChangeMotor(0);
         break;
      case 'c'://z-axis - microscope
        /*
         * Move the tray down in the visual perspective
         */   
         tray=(tray==ZAxisDec)?0:ZAxisDec;
         if (tray==0) ChangeMotor(0);
         break;
      case 'h'://intensity - microscope
        /*
         * Increase the light intensity
         */   
         tray=(tray==LAxisInc)?0:LAxisInc;
         if (tray==0) ChangeMotor(0);
         break;
      case 'g'://intensity - microscope
        /*
         * Decrease the light intensity
         */   
         
         tray=(tray==LAxisDec)?0:LAxisDec;
         if (tray==0) ChangeMotor(0);
         break;
      case 'o'://magnify presets
        /*
         * Sets defaults values for speeds
         */            
         activeSet=mag410;
         break;
      case 'j'://magnify presets
        /*
         * Sets defaults values for speeds
         */            
         activeSet=mag1025;
         break;
      case 'l'://magnify presets
        /*
         * Sets defaults values for speeds
         */            
         activeSet=mag4065;
         break;
      case 'm'://magnify presets
        /*
         * Sets defaults values for speeds
         */            
         activeSet=mag100125;
         break;
      case 'q'://incremental or continuous
        /*
         * Toggles mode of microscope stepper
         * operation from continuously by action
         * one action per one message. speeds
         * are effected by this in repeat loops
         */            
         activeCont=!activeCont;
         break;
      case 'u'://sonic cherp toggle
        /*
         * turns on or off the output
         * and hardware fo the sonic
         * bounce back distance meter
         */            
         soniccherp=!soniccherp;
         break;
       case 'x'://stop all motors
        /*
         * Turns off all steppers immediately
         */    
         tray=0;
        ChangeMotor(0);
        break;
       case 't'://toggle temperature monitor
        /*
         * Changes the state of the thermistor reading on or off
         */    
         thermistor++;
        break;
      /*case 'y':
        minutes++;
        break;
      case 'z':
        minutes--;
        break;*/
    }
  }
}
