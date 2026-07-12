/*******************************
 *   All systems a go defines  *
 *******************************/
 
//Comment out NOUSB to preserve resource when
//operating, no serial debug will be outputted
//#define NOUSB

//Comment out NOESP to have serial input and
//output conole control connected to the USB.
#define NOESP

//Comment out NO74H to ignore the 74H chip
//absent auxiliary logics in operating mode
//#define NO74H

//Comment out no GPS to disable
//the Neo-GPS location chip                                                                                     
#define NOGPS

//Comment ou no MPU to disable the
//pitch/yaw/tilt motion sensor chip
//#define NOMPU

//Comment out NOPWM to disable the use of
//the four capable analogWrite digitals
//#define NOPWM

/************************
 *   Debugging defines  *
 ************************/
 
//The following DEBUG are visual serial console
//and only present when NOESP is defined also.
//#define DEBUGGPS
//#define DEBUGMPU

//The following DEBUG puts all 10 logics into
//a systems wiring check mode pulsing outputs

//#define DEBUG74H

//Uncomment for debugging the analogWrite PWM
//#define DEBUGPWM

//Uncomment for debugging the controller buttons.
//#define DEBUGESP

//LED on board
#define LED_BLINK_PIN 13

//Rate and Timer 
#define UNIFIED_BAUD_RATE 115200
#define BALANCE_INTERVAL 1000
#define NETWORK_INTERVAL 100
#define NETWORK_YEILDING 50

//ESP8266 IO0 and IO2 pins
#define ESP_SERIAL_RX_PIN 1
#define ESP_SERIAL_TX_PIN 0
#define ESP_CHIP_EN_PIN 9

//GPS RX/TX
#define GPS_SERIAL_RX_PIN 8
#define GPS_SERIAL_TX_PIN 7

//74H constants
#define LED_CLOCK_PIN 4
#define LED_LATCH_PIN 11
#define LED_SHIFT_PIN 12

#define MAX_74H_PULSE 250                                                                                                                //in milliseconds
#define MIN_74H_PULSE 10 //in milliseconds

#define PULSE_CHANGE_RATE 10 //in milliseconds

//Interrupt Pin for the MPU
#define MPU_INTR_PIN 2

//PWM constants 
#define PWM_MOTOR1_PIN 10
#define PWM_MOTOR2_PIN 5
#define PWM_MOTOR3_PIN 6
#define PWM_MOTOR4_PIN 3

#define PWM_MIN_SPEED 15 //slowest taxi state
#define PWM_MAX_SPEED 255 //byte maximum

#define PWM_CHANGE_RATE 5


#ifndef NOMPU
#include <Wire.h>
#endif

#ifndef NOESP
#include <SoftwareSerial.h>
SoftwareSerial Serial1(ESP_SERIAL_RX_PIN,ESP_SERIAL_TX_PIN);
#endif

#ifndef NOGPS
#ifndef NOESP
#else
#include <SoftwareSerial.h>
#endif
#include <TinyGPS.h> 
SoftwareSerial Serial2(GPS_SERIAL_RX_PIN, GPS_SERIAL_TX_PIN);
TinyGPS gps;
#endif

#ifndef NOPWM
float motor1=0;
float motor2=0;
float motor3=0;
float motor4=0;

bool poweredUp = false;

#endif

#ifndef NO74H
struct utility {
  //74H program properties
  bool enable; //whether the 74H port is enabled
  long elapse; //running time to a toggle enable
  long latency; //amount between enable toggling
};

struct utility m[8];
unsigned char utilityRegister = 0;
#endif

#ifndef NOMPU
float rotateX, rotateY, rotateZ;  //current values of the MPU rotation axis
float accelX, accelY, accelZ;  //current values of the MPU accellerator axis
float tempC; ///current temperature of the MPU, doesn't appear to be C or F
float offset1, offset2, offset3, offset4, offset5, offset6; //beginning values
#define setSize 4
float mpuChange[setSize*6]; /* horizontal change
stablizing similar to running averaging but discards the info as a actual value.
-0.01 and +0.01 are vertical variant and -/+0.03 Hori. or Vert. is a difference */
#endif

#ifndef NOGPS
float flat, flon;
unsigned long age;
#endif

long AxisX=0, AxisY=0, AxisZ=0;
bool Switch=false, Depress=false;


String RemoveNextArg(String *args, String delim) {
  String ret=String(*args);
  if (ret.indexOf(delim)>-1) {
      ret.remove(ret.indexOf(delim));
      args->remove(0,(args->indexOf(delim))+delim.length());
  } else {
    *args = String();
  }
  return ret;
}
String NextArg(String text, String delim) {
  return ((text.indexOf(delim)>-1)?text.substring(0,text.indexOf(delim)):text);
}
String RemoveArg(String text, String delim) {
  return ((text.indexOf(delim)>-1)?text.substring(text.indexOf(delim)+delim.length()):"");
}

bool IsALN(char ch) {
  switch (ch) {
    case '1':case '2':case '3':case '4':case '5':case '6':case '7':case '8':case '9':case '0':
      return true;break;default:return false;break;
  }
}
long Convert(String num) {
  while (num.charAt(0) == '0') num.remove(0,1);
  char ary[num.length()];
  num.toCharArray(ary,num.length());
  return Convert(num.length(),ary);  
}

long Convert(int len, char num[]) {
  long ret=0;
  long mul=0;
  for (long i=0;i<len;i++) {
    mul=1;
    for (long j=0;j<(((len-1)-(i+1))+1);j++) mul=mul*10;    
    switch (num[i]){
      case '9':
        ret=ret+mul;
      case '8':
        ret=ret+mul;
      case '7':
        ret=ret+mul;
      case '6':
        ret=ret+mul;
      case '5':
        ret=ret+mul;
      case '4':
        ret=ret+mul;
      case '3':
        ret=ret+mul;
      case '2':
        ret=ret+mul;
      case '1':
        ret=ret+mul;
      case '0':
        break;
    }
  }
  return ret;
}

#ifndef NO74H

/******************
 *   74H Private  *
 ******************/

void UpdateShiftRegister()
{
  static unsigned char lastState;
  if (lastState!=utilityRegister) {
    digitalWrite(LED_LATCH_PIN, LOW);
    shiftOut(LED_SHIFT_PIN, LED_CLOCK_PIN, LSBFIRST, utilityRegister);
    digitalWrite(LED_LATCH_PIN, HIGH);
    lastState = utilityRegister;
  } 
}

void Governer() {
  //a periodic function that advances the states of
  //the 74H eight ports based on the latency toggles
  //individually set by other 74H functions below
  bool ison;
  for (int i = 0; i<8;i++) {
    m[i].elapse--;
    if (m[i].elapse<=-m[i].latency) m[i].elapse=-m[i].elapse;
    if ((m[i].elapse<1)&&(m[i].latency>1)) {
      if (bitRead(utilityRegister,i)) {
        bitClear(utilityRegister,i);
        UpdateShiftRegister();
      }
    } else {
      if (!bitRead(utilityRegister,i)) {
         if (m[i].enable) {
          bitSet(utilityRegister,i);
          UpdateShiftRegister();
         }
      } else if (!m[i].enable) {
        bitClear(utilityRegister,i);
        UpdateShiftRegister();
      }  
    }
  }
}

void EnableDisable(bool IsEnable, int num) {
  for (int i = 0 ; i<8; i ++) {  
    m[i].enable = (((i==num)||(num==-1))?IsEnable:m[i].enable);  
    if (!m[i].enable) {
      bitClear(utilityRegister,i);      
    }  else {
      bitSet(utilityRegister,i);
    }
    UpdateShiftRegister();
  }  
}

void IncreaseDecrease(bool IsIncrease, int num, long val) {
  long newVal;
  for (int i = 0 ; i<8; i ++) {
    newVal= m[i].latency + (((i==num)||(num==-1))?(IsIncrease?val:-val):0);
    if (IsIncrease) {      
      if (newVal!=m[i].latency) {
        m[i].elapse=(m[i].elapse-(m[i].latency-newVal));
        m[i].latency=newVal;
      }
    } else {
      if (newVal!=m[i].latency)
      {
        if (newVal>=0) {
          m[i].elapse=(m[i].elapse-(m[i].latency-newVal));
          m[i].latency=newVal;            
        }
      }
    }
    if (m[i].elapse==0) m[i].elapse=m[i].latency;
  }
}

void RatePulseInterval( int num, long val) {
  long newVal;
  for (int i = 0 ; i<8; i ++) {
    newVal= (((i==num)||(num==-1))?val:m[i].latency);
    if ((newVal!=m[i].latency)&&(newVal>=0)&&((i==num)||(num==-1)))
    {
      m[i].elapse=-m[i].elapse;
      m[i].latency=newVal;            
    } 
  } 
  Governer();
}

/*****************
 *   74H Public  *
 *****************/
void LightIndicator(bool OnOff) {
  if (OnOff) {
    if (motor1!=0) EnableDisable(true,0);
    if (motor2!=0) EnableDisable(true,1);
    if (motor3!=0) EnableDisable(true,2);
    if (motor4!=0) EnableDisable(true,3);
  } else {
    if (motor1==0) EnableDisable(false,0);
    if (motor2==0) EnableDisable(false,1);
    if (motor3==0) EnableDisable(false,2);
    if (motor4==0) EnableDisable(false,3);
  }
}

void IncreasePulse() {
  for (int i =0;i<8;i++) {
     if (m[i].latency>(MIN_74H_PULSE+PULSE_CHANGE_RATE)) IncreaseDecrease(true,i,PULSE_CHANGE_RATE);  
  }
}

void DecreasePulse() {
  for (int i =0;i<8;i++) {
    if (m[i].latency<(MAX_74H_PULSE-PULSE_CHANGE_RATE)) IncreaseDecrease(false,i,PULSE_CHANGE_RATE);
  }
}

//void OddPower() {
//  for (int i=0;i<8;i++) {
//    EnableDisable(((i % 2)==1),i);
//  }
//}
//
//void EvenPower() {
//  for (int i =0;i<8;i++) {  
//    EnableDisable(((i % 2)==0),i);
//  }
//}
//void  AlternatePower() {
//  static bool first=!(m[0].elapse<1);
//  for (int i =0;i<8;i++) {  
//    if (first>0) 
//      m[i].elapse=-abs(m[i].elapse);
//    else
//      m[i].elapse=abs(m[i].elapse);
//    first=!first;
//  }
//}
//
//void RegulatedPower() {
//  for (int i =0;i<8;i++) {
//    m[i].elapse=0;
//  }
//}

void Setup74H() {

  pinMode(LED_LATCH_PIN, OUTPUT);
  pinMode(LED_CLOCK_PIN, OUTPUT);  
  pinMode(LED_SHIFT_PIN, OUTPUT); 
  
  for (int i=0;i<8;i++) {
    m[i].enable=false;
    m[i].elapse=1;
    m[i].latency=1;
  }

  utilityRegister=0;
  digitalWrite(LED_LATCH_PIN, LOW);
  shiftOut(LED_SHIFT_PIN, LED_CLOCK_PIN, LSBFIRST, utilityRegister);
  digitalWrite(LED_LATCH_PIN, HIGH);
}

#endif

/*****************************
 *   MPU specific functions  *
 *****************************/
#ifndef NOMPU 
#ifndef NOUSB
#ifdef DEBUGMPU
void DebugMPU()
{

    Serial.print("  Accel: ");
    Serial.print(Padding(6, String(accelX)));
    Serial.print(Padding(6, String(accelY)));
    Serial.print(Padding(6, String(accelZ)));
    Serial.print("  Rotate: ");
    Serial.print(Padding(7, String(rotateX)));
    Serial.print(Padding(7, String(rotateY)));
    Serial.print(Padding(7, String(rotateZ)));
    Serial.print("  Temperature: ");
    Serial.print(Padding(6, String(tempC)));
    
}
#endif
#endif

void Gyroscope1() {  /* this function is split into two parts because the
  elapsed timing is too long and the unit needs to be quickly responsive */
  digitalWrite(MPU_INTR_PIN, LOW);
  // === Read acceleromter data === //
  Wire.beginTransmission(0x68);
  Wire.write(0x3B); // Start with register 0x3B (ACCEL_XOUT_H)
  Wire.endTransmission(false);
  digitalWrite(MPU_INTR_PIN,HIGH);
}

void Gyroscope2() {  /* this function is split into two parts because the
  elapsed timing is too long and the unit needs to be quickly responsive */
  digitalWrite(MPU_INTR_PIN, LOW);
  Wire.requestFrom(0x68,14, true); // Read 6 registers total, each axis value is stored in 2 registers
  //For a range of +-2g, we need to divide the raw values by 16384, according to the datasheet

  rotateX = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset1); // X-axis value
  rotateY = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset2); // Y-axis value
  rotateZ = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset3); // Z-axis value
  tempC = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // gets the raw temperature
  accelX = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset4); // X-axis value
  accelY = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset5); // Y-axis value
  accelZ = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offset6); // Z-axis value


  digitalWrite(MPU_INTR_PIN,HIGH);
}
float GyroChange(int set, float val) {
  set = set * setSize;
  float avg=0;
  if (setSize>1) {
    for (int i = 0; i<(setSize-1); i++) {
      avg=avg+mpuChange[set+i];
      mpuChange[set+i]=mpuChange[set+(i+1)];
    }
    avg =(avg/setSize);
    mpuChange[set+(setSize-1)]=val;
  } else avg=val;
   return avg;   
}

void Gyroscope3() { /* used during routine looping in-place of part 2 above */

  Gyroscope2();

  Gyroscope4();

  rotateX = GyroChange(0, rotateX);
  rotateY = GyroChange(1, rotateY);
  rotateZ = GyroChange(2, rotateZ);

  accelX = GyroChange(3, accelX);
  accelY = GyroChange(4, accelY);
  accelZ = GyroChange(5, accelZ); 
}

void Gyroscope4() {
  mpuChange[0]=rotateX; mpuChange[1]=rotateX; mpuChange[2]=rotateX;
  mpuChange[3]=rotateY; mpuChange[4]=rotateY; mpuChange[5]=rotateY;
  mpuChange[6]=rotateZ; mpuChange[7]=rotateZ; mpuChange[8]=rotateZ;
  mpuChange[9]=accelX; mpuChange[10]=accelX; mpuChange[11]=accelX;
  mpuChange[12]=accelY; mpuChange[13]=accelY; mpuChange[14]=accelY;
  mpuChange[15]=accelZ; mpuChange[16]=accelZ; mpuChange[17]=accelZ;   
}

void SetupMPU() {
  pinMode(MPU_INTR_PIN,OUTPUT); 
  digitalWrite(MPU_INTR_PIN,LOW);  
  
  Wire.begin();                      // Initialize comunication
  Wire.beginTransmission(0x68);       // Start communication with MPU6050 // MPU=0x68
  Wire.write(0x6B);                  // Talk to the register 6B
  Wire.write(0x00);                  // Make reset - place a 0 into the 6B register
  Wire.endTransmission(true);        //end the transmission
  Wire.beginTransmission(0x68);
  Wire.write(0x3B); // Start with register 0x3B (ACCEL_XOUT_H)
  Wire.endTransmission(false);
  Wire.requestFrom(0x68, 14, true); // Read 14 registers total, each axis value is stored in 2 registers
  //Starting with offset, contrary to the example IDK how it will turn out
  offset1 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // X-axis value
  offset2 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Y-axis value
  offset3 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Z-axis value
  tempC = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); //get temperature
  offset4 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // X-axis value
  offset5 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Y-axis value
  offset6 = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Z-axis value

  digitalWrite(MPU_INTR_PIN,HIGH);

  for (int i;i<(setSize*6);i++) {
    mpuChange[i]=0;
  }

  //get a reading to fill the arrays
  Gyroscope1();
  Gyroscope2();
  Gyroscope4();

}
#endif

/*****************************
 *   GPS specific functions  *
 *****************************/
 

#ifndef NOUSB
#ifndef NOGPS
#ifdef DEBUGGPS

void DebugGPS() {
  unsigned long chars;
  unsigned short sentences, failed;

  Serial.print("LAT=");
  Serial.print(flat == TinyGPS::GPS_INVALID_F_ANGLE ? 0.0 : flat, 6);
  Serial.print(" LON=");
  Serial.print(flon == TinyGPS::GPS_INVALID_F_ANGLE ? 0.0 : flon, 6);
  Serial.print(" SAT=");
  Serial.print(gps.satellites() == TinyGPS::GPS_INVALID_SATELLITES ? 0 : gps.satellites());
  Serial.print(" PREC=");
  Serial.print(gps.hdop() == TinyGPS::GPS_INVALID_HDOP ? 0 : gps.hdop());
  
  gps.stats(&chars, &sentences, &failed);
  Serial.print(" CHARS=");
  Serial.print(chars);
  Serial.print(" SENTENCES=");
  Serial.print(sentences);
  Serial.print(" CSUM ERR=");
  Serial.print(failed);
  if (chars == 0)
    Serial.print("** No characters received from GPS: check wiring **");
    delay(1000);
}
#endif
#endif
#endif

#ifndef NOGPS
void Locator() {
  static bool newData;
  if(!newData) {       
      while (Serial2.available()) {
         if (gps.encode((char)Serial2.read())) newData=true;
      }
  } else {
      gps.f_get_position(&flat, &flon, &age);
      newData=!newData;
  }
}

void SetupGPS() {
  Serial2.begin(UNIFIED_BAUD_RATE);
}
#endif

/*****************************
 *   USB specific functions  *
 *****************************/

#ifndef NOUSB

String Padding(int Len, String Val) {
  String ret="";
  if ((Len-Val.length())>0)     
    for (int i = 0 ; i < (Len-Val.length()); i++)  ret.concat(' ');
  ret.concat(Val);
  return ret;
}

void Monitor() {
    if (Serial.available()) {
      char ch = Serial.read();     
      SerialRead(ch);
    }
}

void SerialRead(char ch) {

  switch (ch) {
    case '1':
      Serial.println("PowerOn");
      EnginesOn();
      break;
    case '2':
      Serial.println("Accelerate");
      Accelerate();
      break;
    case '3':
      Serial.println("Breaking");
      Breaking();
      break;
    case '4':
      Serial.println("LevelsOn");
      LevelsOn();
      break;

      #ifndef NO74H
    case '5':
      Serial.println("IncreasePulse");
      IncreaseDecrease(true, -1, PULSE_CHANGE_RATE);
      break;
    case '6':
      Serial.println("DecreasePulse");
      IncreaseDecrease(false, -1, PULSE_CHANGE_RATE);
      break;
    case '7':
      Serial.println("LevelHi");
      RatePulseInterval(-1,MIN_74H_PULSE);
      break;
    case '8':
      Serial.println("LevelLo");
      RatePulseInterval(-1,MAX_74H_PULSE);
      break;
     #endif
    case '9':
      Serial.println("FullSpeed");
      EngineFullSpeed();
      break;
    case '0':
      Serial.println("PowerOff");
      EnginesOff();
      break;
  }

}

#endif


void Driver() {
    
  String txt = Serial.readStringUntil('\n');

  while (txt.length()>0) {        
    switch (txt.charAt(0))
    {
      case 's': //middle switch on controller
        if (txt.length()>1)
          if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
            Switch = (txt.charAt(1)=='1');
        txt.remove(0,2);
        break;
      case 'd': //right axis depress switch on controller
        if (txt.length()>1)
          if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
            Depress = (txt.charAt(1)!='1');
        txt.remove(0,2);
        break;
      case 'x':  //right axis left or right
        if (txt.length()>4)
          AxisX = Convert(txt.substring(1,5));
        txt.remove(0,5);
        break;      
      case 'y': //right axis up or down
        if (txt.length()>4)
          AxisY = Convert(txt.substring(1,5));
        txt.remove(0,5);
        break;      
      case 'z': //left dial clockwise or counter clockwise   
        if (txt.length()>4)
          AxisZ = Convert(txt.substring(1,5));
        txt.remove(0,5);
        break;
      default:
        if (txt.length()>0)
          txt.remove(0,1);
        break;
    }        

    static bool toggler[2]={false,false};
    
    //switch turns all motor controlls on or off
    //axisz is altitude control fall/hold/climb
    //axisx is yaw, aim the drone to a direction
    //axisy forward/backward controls the drone
    //depress auto holds, twice returns to start
    
    if (Switch) {
      if (!toggler[0]) {
        toggler[0]=true;
        if ((motor1!=0)||(motor2!=0)||(motor3!=0)||(motor4!=0)) {
          EnginesOff();
        } else {
          EnginesOn();
        }
      }
    } else if (toggler[0]) toggler[0]=false;

    if (Depress) {
      if (!toggler[0]) toggler[0]=true;
    } else if (toggler[0]) toggler[0]=false;

  } 

  #ifdef DEBUGESP
    #ifndef NOUSB
  
      Serial.print(String(Switch));
      Serial.print(' ');
      Serial.print(String(Depress));
      Serial.print(' ');
      Serial.print(String(AxisX));
      Serial.print(' ');
      Serial.print(String(AxisY));
      Serial.print(' ');
      Serial.println(String(AxisZ));
  
    #endif
  #endif
  
}

#ifndef NOESP

void SetupESP() {
  #ifndef NOUSB
  Serial.begin(UNIFIED_BAUD_RATE);
  #else
  Serial.begin(UNIFIED_BAUD_RATE);
  pinMode(ESP_CHIP_EN_PIN,OUTPUT);
  digitalWrite(ESP_CHIP_EN_PIN,LOW);
  #ifndef NOESP
  Serial1.begin(UNIFIED_BAUD_RATE);  
  digitalWrite(ESP_CHIP_EN_PIN,HIGH);
  Serial.setTimeout(NETWORK_YEILDING);
  #endif
  #endif
}

#endif

#ifndef NOPWM

/*********************
 *   Engine Private  *
 *********************/
 
void Engines() {
  if (motor1>0) {
    if (motor1>PWM_MAX_SPEED)
      motor1=PWM_MAX_SPEED; 
    else if (motor1<PWM_MIN_SPEED)
      motor1=PWM_MIN_SPEED;
  } else if (motor1<0) motor1=0;
  if (motor2>0) {
    if (motor2>PWM_MAX_SPEED)
      motor2=PWM_MAX_SPEED;
    else if (motor2<PWM_MIN_SPEED)
      motor2=PWM_MIN_SPEED;
  } else if (motor2<0) motor2=0;
  if (motor3>0) {
    if (motor3>PWM_MAX_SPEED)
      motor3=PWM_MAX_SPEED;
    else if (motor3<PWM_MIN_SPEED)
      motor3=PWM_MIN_SPEED;
  } else if (motor3<0) motor3=0;
  if (motor4>0) {
    if (motor4>PWM_MAX_SPEED)
      motor4=PWM_MAX_SPEED;
    else if (motor4<PWM_MIN_SPEED)
      motor4=PWM_MIN_SPEED;
  } else if (motor4<0) motor4=0;

  analogWrite(PWM_MOTOR1_PIN,motor1);
  analogWrite(PWM_MOTOR2_PIN,motor2);
  analogWrite(PWM_MOTOR3_PIN,motor3);
  analogWrite(PWM_MOTOR4_PIN,motor4);

}

/********************
 *   Engine Public  *
 ********************/

void EngineShutdown() {   
  motor1=0;
  motor2=0;
  motor3=0;
  motor4=0;
  Engines();
  #ifndef NO74H
  LightIndicator(false);
  #endif
}
void EngineTaxiState() {
  motor1=PWM_MIN_SPEED;
  motor2=PWM_MIN_SPEED;
  motor3=PWM_MIN_SPEED;
  motor4=PWM_MIN_SPEED;
  Engines();
  #ifndef NO74H
  LightIndicator(true);
  #endif
}
void EngineFullSpeed() {
  motor1=PWM_MAX_SPEED;
  motor2=PWM_MAX_SPEED;
  motor3=PWM_MAX_SPEED;
  motor4=PWM_MAX_SPEED;
  Engines();
  #ifndef NO74H
  LightIndicator(true);
  #endif
}
void Accelerate() {
  motor1=motor1+PWM_CHANGE_RATE;
  motor2=motor2+PWM_CHANGE_RATE;
  motor3=motor3+PWM_CHANGE_RATE;
  motor4=motor4+PWM_CHANGE_RATE;
  Engines();
  #ifndef NO74H
  LightIndicator(true);
  #endif
}
void Breaking() {
  motor1=motor1-PWM_CHANGE_RATE;
  motor2=motor2-PWM_CHANGE_RATE;
  motor3=motor3-PWM_CHANGE_RATE;
  motor4=motor4-PWM_CHANGE_RATE;
  Engines();
  #ifndef NO74H
  LightIndicator(false);
  #endif
}
  
void SetupPWM() {

  pinMode(PWM_MOTOR1_PIN,OUTPUT);
  pinMode(PWM_MOTOR2_PIN,OUTPUT);
  pinMode(PWM_MOTOR3_PIN,OUTPUT);
  pinMode(PWM_MOTOR4_PIN,OUTPUT);

  analogWrite(PWM_MOTOR1_PIN,0);
  analogWrite(PWM_MOTOR2_PIN,0);
  analogWrite(PWM_MOTOR3_PIN,0);
  analogWrite(PWM_MOTOR4_PIN,0);
  motor1=0;
  motor2=0;
  motor3=0;
  motor4=0;

}

#endif

void LevelsOn() {
  #ifndef NO74H
    EnableDisable(true,4);
    EnableDisable(true,5);
    EnableDisable(true,6);
    EnableDisable(true,7);    
  #endif
}
void LevelsOff() {
  #ifndef NO74H
    EnableDisable(false,4);
    EnableDisable(false,5);
    EnableDisable(false,6);
    EnableDisable(false,7);  
  #endif
}

void EnginesOff() {
  if (poweredUp) {
    LevelsOff();
    #ifndef NOPWM
      EngineShutdown();
     #endif
    poweredUp = false;
  }
}

void EnginesOn() {
  if (!poweredUp) {
    #ifndef NOPWM
      EngineTaxiState();
    #endif
    poweredUp = true;
  }
}


void setup() 
{
 // pinMode(LED_BLINK_PIN,OUTPUT);
  #ifndef NOUSB 
   Serial.begin(UNIFIED_BAUD_RATE);
   #endif
   
  #ifndef NOESP
  SetupESP();
  #endif    
  
  #ifndef NOGPS
  SetupGPS();
  #endif

  #ifndef NO74H
  Setup74H();
  #endif

  #ifndef NOMPU
  SetupMPU();
  #endif
  
  #ifndef NOPWM
  SetupPWM();
  #endif 
}


  
void loop() 
{ 


  #ifndef NOUSB
  Monitor(); //handles input for output serial debugging
  #endif
  
  #ifndef NOESP
  Driver(); //reads from the network for the controller 
  #endif

  #ifndef NO74H
  Governer(); //updates the 74HC595 with elapsed timings
  #endif
  
  #ifndef NOMPU
  Gyroscope1();  //first half of retrieval of gyro info
  #endif



  #ifndef NOUSB
  Monitor(); //handles input for output serial debugging
  #endif
  
  #ifndef NOESP
  Driver(); //reads from the network for the controller 
  #endif
  
  #ifndef NO74H
  Governer(); //updates the 74HC595 with elapsed timings
  #endif
  
  #ifndef NOMPU
  Gyroscope3(); //second half of retrieval of gyro info
  #endif



  #ifndef NOUSB
  Monitor(); //handles input for output serial debugging
  #endif
  
  #ifndef NOESP
  Driver(); //reads from the network for the controller 
  #endif

  #ifndef NO74H
  Governer(); //updates the 74HC595 with elapsed timings
  #endif
  
  #ifndef NOGPS
  Locator(); //reads the GPS information
  #endif



  #ifndef NOUSB

    #ifndef NOPWM
      #ifdef DEBUGPWM
        DebugPWM();
        Serial.println();
      #endif
    #endif
    
    #ifndef NOMPU
      #ifdef DEBUGMPU    
        DebugMPU();
        Serial.println();
      #endif
    #endif
   
    #ifndef NOGPS
      #ifdef DEBUGGPS
        DebugGPS();
        Serial.println(); 
      #endif
    #endif

  #endif

}
