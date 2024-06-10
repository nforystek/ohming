 /*Arduino Nano*/

 //update https://1drv.ms/u/s!Aun7yHESMcYNgpZ9XS8Bp8lcnCQllQ?e=GKzmU9 
 //powering circuit https://everycircuit.com/circuit/5467547447853056
 //solar battery recharger for outlet power outage backup, with out a
 //internet alert or disk medum so the pwoer solid state is important 

//DEBUG DEFINES

 //#define DEBUGDISPLAY
 //#define DEBUGBUTTONS
 //#define DEBUGDIGITAL

//Other Define Options

//#define TOUCHBUTTONS  //use a 10Mohm resistor from
//digital pins to 3.3v to create touch sesnor butons
//not recommended for secuirty because of the sensor
//function used is rather slow, yielding alarm check
//if you free up a pin, defining the pin below it is
//much quicker this hardware setup has none free but
//use the pin defined for 10Mohm instead of the 3.3v 
//#define TOUCHSENSOR 0
 
 //pins setup
 
//74HC595 chip pins for a digital display quick view
//alert, requires the chip, and 8x 220 ohm resistors
#define latchPin 4
#define clockPin 3
#define dataPin 2

//normal switch pins requiring 4x 150ohm
//resistors and 3 diodes to make the 4th
//button be "all three" press difference
#define btn1Pin 5
#define btn2Pin 6
#define btn3Pin 13

//1k ohm resistor to base of a NPN to LCD on/off module power, also a active pezzio
#define buzzerPin 1  //shares with hardware serial, security plus for alerts on usb
#define backlitePin 0  //shares with hardware serial, lower power usage drastically

//latency milliseconds
#define BLINK_LATENCY (unsigned long)200 //digital display blinker and buzzer blink interval
#define MESSAGE_LATENCY (unsigned long)3000 //length of time a temp message posts on the lcd
#define RETURN_LATENCY (unsigned long)10000 //length of millis before setup menu auto closes
#define ACTIVATION_LATENCY (unsigned long)20000  //length of millis first trip is open for a activation
#define DEACTIVATION_LATENCY (unsigned long)30000  //length of millis before alarm sounds to deactivate
#define SCREENSAVER_LATENCY (unsigned long)30000  //length of millis idle before LCD backlite turns off

//states
#define SET_DEFAULTS 0  //initial boot/set terminal's NC/NO switch current as sealed
#define KEY_SEQUENCE 1 //first of two stage setting to the key sequence, must repeat
#define VALIDATE_KEY 2 //repeat stage of the key sequence, also menu option in setup
#define SETUP_MENU 3 //setup menu diplsay, during of which time_out returns inactive
#define INACTIVE_MODE 4 //normal silent alarm mode entering sequence brings to setup
#define ACTIVE_MODE 5  //mode of moinitoring, has a activation and deactivation time
#define ALERT_MODE 6 //mode of sensor tripped while in active mode, the alarm sounds
#define TIME_OUT 7 //some modes have a unique time out coupled with them by flagging

//buttons
#define LEFT 1 //left most button on the panel, a smaller of circle then the select
#define SELECT 2 //inbetween left and right this circle is slightly bigger a button
#define RIGHT 3 //opposite side of left and select, button size is the same as left
#define MODE 4 //smallest most protruding circle, right of the lcd for quick pushes

//2 rows by 16 colums LCD text siplay
#include <LiquidCrystal.h>  //Arduino
LiquidCrystal lcd(7, 8, 9, 10, 11, 12); 

//Digital Display settings
int state=0; //the state of operation with
byte alert=0; //in sensor alert, the digit solid lights up this number
              //which may also be a silent state, still showing sensor
              //alert>=10 is allowing alert trip oepn to/or just close
unsigned long timer=0; //used in the timer flags, elapse start latency

bool buttons[4]={false,false,false,false};  //programmer watch states
bool toggles[4]={false,false,false,false};  //1 press & release check

byte btnBuffer[10]={0,0,0,0,0,0,0,0,0,0};  //queue incase a interrupt
byte btnKeySeq[10]={0,0,0,0,0,0,0,0,0,0};  //disarm sequence user set
bool terminals[8] = {0,0,0,0,0,0,0,0};  //NO/NC/INP states
                                      //for each termCount
// LCD Display Variable Settings
String Row1Text="";  //the text to display with behavior set for 1st row
String Row2Text="";  //the text to display with behavior set for 2nd row
String TempText=""; //temporary still message auto word wrapped
                    //over both rows that lasts 3 seconds, then
                    //returns to the set row behaviors and text

bool flagRead(int flag) {
  return bitRead(state,flag);
}
void flagSet(int flag) {
  bitSet(state,flag);
}
void flagClear(int flag) {
  bitClear(state, flag);
}
void AlertDefaults() {
  terminals[0] = ((bool)(analogRead(A0)>128));
  terminals[1] = ((bool)(analogRead(A1)>128));
  terminals[2] = ((bool)(analogRead(A2)>128));
  terminals[3] = ((bool)(analogRead(A3)>128));
  terminals[4] = ((bool)(analogRead(A4)>128));
  terminals[5] = ((bool)(analogRead(A5)>128));
  terminals[6] = ((bool)(analogRead(A6)>128));
  terminals[7] = ((bool)(analogRead(A7)>128));    
}

void AlertHandler() {
 if (flagRead(INACTIVE_MODE)) alert=0;
 
 if (alert==0) {  
    if (((bool)(analogRead(A0)>128))!=terminals[0]) alert = 1;
    if (((bool)(analogRead(A1)>128))!=terminals[1]) alert = 2;
    if (((bool)(analogRead(A2)>128))!=terminals[2]) alert = 3;
    if (((bool)(analogRead(A3)>128))!=terminals[3]) alert = 4;
    if (((bool)(analogRead(A4)>128))!=terminals[4]) alert = 5;
    if (((bool)(analogRead(A5)>128))!=terminals[5]) alert = 6;
    if (((bool)(analogRead(A6)>128))!=terminals[6]) alert = 7;
    if (((bool)(analogRead(A7)>128))!=terminals[7]) alert = 8;  
  }
}
#ifdef TOUCHBUTTONS
#ifdef TOUCHSENSOR
int ButtonSensor(byte set, byte pin) {
  pinMode(set, OUTPUT);
  pinMode(pin, INPUT);
  digitalWrite(set,LOW);
  long val2=0;  
  static bool val1;
  static bool toggle;
  toggle=val1;
  for (int i = 0 ; i<200; i++) {
    val1=digitalRead(pin);
    if (val1!=toggle) {
      toggle=val1;
      digitalWrite(set, !val1);
      digitalWrite(pin, LOW);
      val2++;
    } else {
      digitalWrite(pin, LOW);
      digitalWrite(set, !toggle);      
    }
  }
  return (!(val2>45));  
}
#else
bool ButtonSensor(int btn) {
  for (int i = 0 ; i < 10; i++)
  {
    pinMode(btn, INPUT);
    if(digitalRead(btn)==LOW) return true;
    pinMode(btn, OUTPUT);
    digitalWrite(btn,LOW);
    pinMode(btn, INPUT);
    delay(1);    
  }
  return false;   
}
#endif
#endif

void ButtonClear() {
  for (byte i=0;i<10;i++) {    
    btnBuffer[i]=0;    
  }
}
byte ButtonRelease() {
  byte ret=btnBuffer[0];
  for (byte i=0;i<9;i++) {    
    btnBuffer[i]=btnBuffer[i+1];    
    if (btnBuffer[i]==0) break;
  }
  btnBuffer[9]=0;
  return ret;
}

bool ButtonPressed(byte btn) {
  if (btn==0)
    return btnBuffer[0];
  else
  for (byte i = 0 ; i<10; i++)
    if (btnBuffer[i]==btn) return true;
  return false;
}
void ButtonHandler() {
  #ifndef TOUCHBUTTONS
  buttons[0]=!digitalRead(btn1Pin);
  buttons[1]=!digitalRead(btn2Pin);
  buttons[2]=!digitalRead(btn3Pin);
  #else
  #ifdef TOUCHSENSOR
  buttons[0]=ButtonSensor(TOUCHSENSOR, btn1Pin);
  buttons[1]=ButtonSensor(TOUCHSENSOR, btn2Pin);
  buttons[2]=ButtonSensor(TOUCHSENSOR, btn3Pin);
  #else
  buttons[0]=ButtonSensor(btn1Pin);
  buttons[1]=ButtonSensor(btn2Pin);
  buttons[2]=ButtonSensor(btn3Pin);
  #endif
  #endif
  buttons[3]=(buttons[0]&&buttons[1]&&buttons[2]);
  if (buttons[3]) {
     buttons[0]=0;
     buttons[1]=0;
     buttons[2]=0;
  }
  
  for (byte i = 0; i<4;i++)
  {
    if (buttons[i]) {
      if (!toggles[i]) {
        toggles[i]=true;   
        for (byte j=0;j<10;j++) {    
          if (btnBuffer[j]==0) {
            btnBuffer[j]=i+1;
            break;
          } 
        } 
      }
    } else toggles[i]=false;
  } 

  #ifndef DEBUGDISPLAY
  if (!flagRead(ALERT_MODE)) {
    if (ButtonPressed(0)!=0){
      if (!flagRead(TIME_OUT)) {      
        timer=millis();
      } 
      LCDOn();    
    } else if (!flagRead(TIME_OUT)) {
      if (digitalRead(backlitePin)) {
        if (millis()-timer>SCREENSAVER_LATENCY) {      
          LCDOff();
        }    
      } else {
        timer=millis();
      }
    }
  }
  #else
  if (!flagRead(ALERT_MODE)) {
    if (!flagRead(TIME_OUT)) {
      if (millis()-timer>(BLINK_LATENCY*6)) {      
        timer=millis();
        LCDOn();    
      } else if (millis()-timer>(BLINK_LATENCY*3)) {      
        LCDOff();    
      }    
    } 
  }
  #endif
}
void KeySeqAppend(int btn) {
  for (int i =0;i<10;i++) {
    if (btnKeySeq[i]==0) {
      btnKeySeq[i]=btn;
      break;
    }
  }
}
void KeySeqClear() {
  for (int i=0;i<10;i++) {
    btnKeySeq[i]=0;
  }
}
bool KeySeqValid() {

  if (btnKeySeq[1]!=0) {
    for (int i = 0; i<10; i++)
    {
      if (btnBuffer[i]==SELECT) {
        ButtonClear();
        break;
      } else if (btnBuffer[i]!=btnKeySeq[i]) {
        break;
      } else if (i==9) {
        ButtonClear();
        return true;
      }        
    }   
  }
  return false;
}
void StateHandler() {

  if ((flagRead(SET_DEFAULTS))) {
    if (flagRead(SETUP_MENU)) 
      TempText ="Current Trips Set As Default";
    else
      flagSet(KEY_SEQUENCE);
    AlertDefaults();  
    flagClear(SET_DEFAULTS);
  }
  
  if (!flagRead(ALERT_MODE)) {
    
    if (flagRead(ACTIVE_MODE)) {
      Row1Text="Monitoring";
      Row2Text="";
      if (!flagRead(TIME_OUT)) {
        if (alert>0) {
          flagSet(TIME_OUT);
          timer=millis();
        }        
      } else {
        if (millis()-timer>DEACTIVATION_LATENCY) {
          flagSet(ALERT_MODE);
          flagClear(TIME_OUT);
          timer=millis();
        } else {
          Row1Text = "Alarm Trip Set";
          Row2Text = "Alert in ";
          Row2Text.concat((int)(((-DEACTIVATION_LATENCY + -(millis()-timer))+(DEACTIVATION_LATENCY*2))/1000));
          Row2Text.concat('s');
        }           
      }
    } else if (!flagRead(KEY_SEQUENCE)) {     
    
      if (flagRead(SETUP_MENU)) {
        Row1Text = "< Setup >";
        if (!flagRead(VALIDATE_KEY)) 
          Row2Text = "Set Trip Default";
         else
          Row2Text = "Set Key Sequence";
        if (!flagRead(TIME_OUT)) {          
           flagSet(TIME_OUT);
           timer=millis();   
        } else {
          if (btnBuffer[0]!=0) timer=millis();          
          if (millis()-timer>RETURN_LATENCY) {
            flagClear(TIME_OUT);
            timer=millis();
            flagClear(VALIDATE_KEY);
            flagClear(SET_DEFAULTS);
            flagClear(SETUP_MENU);
          }        
        }  
        if (flagRead(SETUP_MENU)) {
          switch (ButtonRelease()) {
            case LEFT:
            case RIGHT:
              if (flagRead(VALIDATE_KEY)) {
                flagClear(VALIDATE_KEY);
              } else {
                flagSet(VALIDATE_KEY);
              }
              break;
            case SELECT:
              if (flagRead(VALIDATE_KEY)) {
                flagSet(KEY_SEQUENCE);
                flagClear(VALIDATE_KEY);
                flagClear(SETUP_MENU);
                ButtonClear();
                KeySeqClear();
              } else {
                flagSet(SET_DEFAULTS);
              }
              
              break;
            case MODE:
              flagClear(TIME_OUT);
              timer=millis();
              flagClear(SETUP_MENU);
              flagSet(INACTIVE_MODE);
              flagClear(VALIDATE_KEY);
              flagClear(SET_DEFAULTS);
              ButtonClear();
              break;
          }     
        }        
      } else if (flagRead(INACTIVE_MODE)) {
        if (flagRead(TIME_OUT)) {
          if ((millis()-timer)>ACTIVATION_LATENCY) {
            flagSet(ACTIVE_MODE);
            flagClear(TIME_OUT);
            timer=millis();            
            flagClear(INACTIVE_MODE);
            ButtonClear();
          } else {
            Row1Text = "First Trip Open";
            Row2Text = "Seals in ";
            Row2Text.concat((int)(((-ACTIVATION_LATENCY + -(millis()-timer))+(ACTIVATION_LATENCY*2))/1000));
            Row2Text.concat('s');
          }          
        } else {
          Row1Text = "Ready";
          Row2Text = "";
        }
        if (ButtonPressed(MODE)) {
            ButtonRelease();
            
            if ((flagRead(TIME_OUT))&&(alert==0)) {
              flagSet(ACTIVE_MODE);
              flagClear(TIME_OUT);
              timer=millis();
              flagClear(INACTIVE_MODE);
              ButtonClear();              
            } else if (alert<2) {
              flagSet(TIME_OUT);
              timer=millis();
            } else {
              TempText = "Not All Trips Are Closed";
            }
        }
      }
    } 
  }
          
  if (((flagRead(VALIDATE_KEY)||flagRead(KEY_SEQUENCE))&&ButtonPressed(MODE))&&(!flagRead(SETUP_MENU))) {
    TempText = "Key Sequence Reset Try Again";
    ButtonClear();
    KeySeqClear();
    flagClear(VALIDATE_KEY);
    flagSet(KEY_SEQUENCE);
  }
  
  if ((!flagRead(VALIDATE_KEY))&&(flagRead(KEY_SEQUENCE))&&(!flagRead(SETUP_MENU))) {
    if (btnKeySeq[0]==0) {
      Row1Text="2-10 Push Define";
      Row2Text="Left & Right Seq";
    } else {
      Row1Text="Select Sets Key";
      Row2Text="Then Repeat It";
    }
    switch (ButtonRelease())
    {
      case LEFT:
        KeySeqAppend(LEFT);
        break;
      case RIGHT:
        KeySeqAppend(RIGHT);
        break;  
      case SELECT:
        if (btnKeySeq[1]==0) {
          TempText = "Key Too Short Start Entry Over";
          ButtonClear();
          KeySeqClear();
          flagClear(VALIDATE_KEY);
          flagSet(KEY_SEQUENCE);
        } else flagSet(VALIDATE_KEY);
        break;
    }         
  } 

  if (KeySeqValid()) {
    if (flagRead(KEY_SEQUENCE)) {
      TempText="Key Sequence Changed";
    } else {
      TempText="Validated";
    }
    flagClear(KEY_SEQUENCE);
    
    alert=0;
    digitalWrite(buzzerPin,LOW);

    flagClear(VALIDATE_KEY);
    if (flagRead(INACTIVE_MODE)&&(!flagRead(TIME_OUT))) {
      flagSet(SETUP_MENU);
      flagSet(VALIDATE_KEY);
    } else {
      flagSet(INACTIVE_MODE);
    }
    flagClear(TIME_OUT);
    timer=millis();
    flagClear(ACTIVE_MODE);
    flagClear(ALERT_MODE);        
  }  
  
  if (flagRead(ALERT_MODE)) {
    Row1Text = "!!!!ALERT!!!!";
    Row2Text = "";
    if ((timer==0)||(millis()-timer>BLINK_LATENCY)) {
      timer=millis();
      digitalWrite(buzzerPin,!digitalRead(buzzerPin));
    }
  }
}

void setup() {

  pinMode(buzzerPin, OUTPUT);
  digitalWrite(buzzerPin, LOW);

  pinMode(backlitePin, OUTPUT);
  digitalWrite(backlitePin,HIGH);
  
  pinMode(latchPin, OUTPUT);
  pinMode(clockPin, OUTPUT);
  pinMode(dataPin, OUTPUT);
  
  pinMode(btn1Pin, INPUT);
  pinMode(btn2Pin, INPUT);
  pinMode(btn3Pin, INPUT);

  digitalWrite(btn1Pin, LOW);
  digitalWrite(btn2Pin, LOW);
  digitalWrite(btn3Pin, LOW);
  
  pinMode(A0, INPUT);
  pinMode(A1, INPUT);
  pinMode(A2, INPUT);
  pinMode(A3, INPUT);
  pinMode(A4, INPUT);
  pinMode(A5, INPUT);
  pinMode(A6, INPUT);
  pinMode(A7, INPUT);

  lcd.begin(16, 2); 
  timer=millis();
  flagSet(SET_DEFAULTS);
}

void loop() {  

  AlertHandler();
      
  ButtonHandler();

  StateHandler();
  
  DigitalHandler();

  #ifdef DEBUGBUTTONS

  Row1Text = String(buttons[0]);
  Row1Text.concat(' ');
  Row1Text.concat(buttons[1]);
  Row1Text.concat(' ');
  Row1Text.concat(buttons[2]);
  Row1Text.concat(' ');
  Row1Text.concat(buttons[3]);

  #endif
  
  LCDHandler();
}

void LCDStillText(byte row, String txt) {
  int diff=(16-txt.length());
  for (int i=0;i<(diff/2);i++)
  {
    lcd.setCursor(i,row);
    lcd.print(' ');
  }
  if (txt.length()>16) {
    lcd.setCursor(0,row);
    lcd.print(txt.substring(0,16));
  } else if (txt.length()>0) {        
    lcd.setCursor(((diff>2)?(diff/2):0),row);
    lcd.print(txt);
  } 
  for (int i=(txt.length()+(diff/2));i<16;i++)
  {
    lcd.setCursor(i,row);
    lcd.print(' ');
  }
}

void LCDOn() {
  if (!digitalRead(backlitePin)) {
    digitalWrite(backlitePin, HIGH);
    lcd.display();
  }
}
void LCDOff() {
  if (digitalRead(backlitePin)) {
    lcd.noDisplay();
    digitalWrite(backlitePin, LOW);    
  }
}

void LCDHandler() {
  if (digitalRead(backlitePin)) {
    if (TempText.length()>0) {
      static unsigned long elapsed=0;
      if (elapsed==0) elapsed=millis();
      if ((millis()-elapsed)>MESSAGE_LATENCY) {
        TempText="";
        elapsed=0;
        lcd.clear();
      }
      if (TempText.length()>16) {
        int idx=-1;
        int tmp=0;
        do {
          tmp=TempText.substring(idx+1).indexOf(' ');
          idx=idx + (tmp+1);        
        } while ((idx+(tmp+1)<16)&&(tmp>-1));
        if (idx==-1) idx=16;
        LCDStillText(0,TempText.substring(0,idx));
        LCDStillText(1,TempText.substring(idx));
      } else {
        LCDStillText(0,TempText);
        LCDStillText(1,"                ");
      }
    } else {
      LCDStillText(0, Row1Text);
      LCDStillText(1, Row2Text);
    }
  }
}

void DigitalHandler() {
  byte bitOut=0;
  #ifdef DEBUGDIGITAL
    static byte temp=1;
    temp++;
    if (temp>=9) temp=1;
    for (int i=1;i<10;i++) {
      if (i==temp) 
        bitSet(bitOut,i);
      else
        bitClear(bitOut,i);
    }
    delay(100);
  #else
  static bool blinking=false;  
  static unsigned long elapsed=0;

  if (((millis()-elapsed)>BLINK_LATENCY)||(elapsed==0)) {
    elapsed=millis();
    if (state==ALERT_MODE) {
      blinking=!blinking;
    } else blinking=false;
    if (alert>9) alert--;
  }

  if ((((state==ALERT_MODE)&&(alert>0)&&(alert<9))&&blinking)||(state==ACTIVE_MODE))
    bitSet(bitOut,3);
  else
    bitClear(bitOut,3);  
        
  if ((!blinking)||(alert==0)) {
      bitClear(bitOut,4);
      bitClear(bitOut,5);
      bitClear(bitOut,6);
      bitClear(bitOut,7);
      bitClear(bitOut,2);
      bitClear(bitOut,0);
      bitClear(bitOut,1l);      
  }
  
  if (blinking||(alert!=0)) {
    switch (alert) {
      case 1:
        bitSet(bitOut,4);
        bitClear(bitOut,5);
        bitClear(bitOut,6);
        bitClear(bitOut,7);
        bitSet(bitOut,2);
        bitClear(bitOut,0);
        bitClear(bitOut,1);
        break;
      case 2:
        bitSet(bitOut,4);
        bitSet(bitOut,5);
        bitClear(bitOut,6);
        bitSet(bitOut,7);
        bitClear(bitOut,2);
        bitSet(bitOut,0);
        bitSet(bitOut,1);
        break;
      case 3:
        bitSet(bitOut,4);
        bitSet(bitOut,5);
        bitClear(bitOut,6);
        bitSet(bitOut,7);
        bitSet(bitOut,2);
        bitClear(bitOut,0);
        bitSet(bitOut,1);
        break;
      case 4:
        bitSet(bitOut,4);
        bitClear(bitOut,5);
        bitSet(bitOut,6);
        bitSet(bitOut,7);
        bitSet(bitOut,2);
        bitClear(bitOut,0);
        bitClear(bitOut,1);
        break;
      case 5:
        bitClear(bitOut,4);
        bitSet(bitOut,5);
        bitSet(bitOut,6);
        bitSet(bitOut,7);
        bitSet(bitOut,2);
        bitClear(bitOut,0);
        bitSet(bitOut,1);
        break;
      case 6:
        bitClear(bitOut,4);
        bitSet(bitOut,5);
        bitSet(bitOut,6);
        bitSet(bitOut,7);
        bitSet(bitOut,2);
        bitSet(bitOut,0);
        bitSet(bitOut,1);
        break;
      case 7:
        bitSet(bitOut,4);
        bitSet(bitOut,5);
        bitClear(bitOut,6);
        bitClear(bitOut,7);
        bitSet(bitOut,2);
        bitClear(bitOut,0);
        bitClear(bitOut,1);
        break;
      case 8:
        bitSet(bitOut,4);
        bitSet(bitOut,5);
        bitSet(bitOut,6);
        bitSet(bitOut,7);
        bitSet(bitOut,2);
        bitSet(bitOut,0);
        bitSet(bitOut,1);
        break;

    }
  }
  #endif
  digitalWrite(latchPin, LOW);
  shiftOut(dataPin, clockPin, LSBFIRST, bitOut);  
  digitalWrite(latchPin, HIGH);
}
