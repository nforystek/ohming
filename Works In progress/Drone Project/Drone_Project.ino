/*******************************
 *   All systems a go defines  *
 *******************************/
 
//Comment out NOUSB to preserve resource when
//operating, no serial debug will be outputted
#define NOUSB

//Comment out NOESP to have serial input and
//output conole control connected to the USB.
//#define NOESP

//Comment out NO74H to ignore the 74H chip
//absent auxiliary logics in operating mode
//#define NO74H

//Comment out no GPS to disable
//the Neo-GPS location chip
//#define NOGPS

//Comment ou no MTU to disable the
//pitch/yaw/tilt motion sensor chip

//#define NOMTU

//Comment out NOPWM to disable the use of
//the four capable analogWrite digitals
//#define NOPWM

/************************
 *   Debugging defines  *
 ************************/
 
//The following DEBUG are visual serial console
//and only present when NOESP is defined also.
//#define DEBUGGPS
//#define DEBUGMTU

//The following DEBUG puts all 10 logics into
//a systems wiring check mode pulsing outputs
//#define DEBUG74H

//Uncomment for debugging the analogWrite PWM
//#define DEBUGPWM


/******************
 *   PIN defines  *
 ******************/

//LED on board
#define LED_BLINK_PIN 13
 
//GPS RX/TX
#define GPS_SERIAL_RX_PIN 8
#define GPS_SERIAL_TX_PIN 7

//ESP8266 IO0 and IO2 pins
#define ESP_SERIAL_RX_PIN 1
#define ESP_SERIAL_TX_PIN 0
#define ESP_CHIP_EN_PIN 9

//PIN constants for PWM
#define PWM_MOTOR1_PIN 10
#define PWM_MOTOR2_PIN 5
#define PWM_MOTOR3_PIN 6
#define PWM_MOTOR4_PIN 3

//PIN constants for 74H ICS
#define LED_CLOCK_PIN 4
#define LED_LATCH_PIN 11
#define LED_SHIFT_PIN 12

//Interrupt Pin for the MTU
#define MTU_INTR_PIN 2

/********************************
 *   Includes and initializers  *
 ********************************/

#ifndef NOMTU
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

/********************************
 *   Program operation defines  *
 ********************************/

#define UNIFIED_BAUD_RATE 115200

#define PWM_MIN_SPEED 15
#define PWM_MAX_SPEED 255

#define BALANCE_INTERVAL 1000

#define NETWORK_INTERVAL 100
#define NETWORK_YEILDING 50

/*****************
 *   Structures  *
 *****************/
 
struct utility {
  //74H program properties
  bool enable;
  long elapse;
  long latency;
};

/**************
 *   Globals  *
 **************/
 
float actualX, actualY, actualZ;
float offsetX, offsetY, offsetZ;
float pickupX, pickupY, pickupZ;
float medianX, medianY, medianZ;
float targetX, targetY, targetZ;
float virtueX, virtueY, virtueZ;

long AxisX=0, AxisY=0, AxisZ=0;
bool Switch=false, Depress=false;

unsigned long elapseTarget;
unsigned long networkLatent;
unsigned long networkElapse;

float flat, flon;
unsigned long age;

int avgcnt=0;
                  
float motor1=0;
float motor2=0;
float motor3=0;
float motor4=0;
struct utility m[8];
unsigned char utilityRegister = 0;

static char incoming[255];

static bool powersOn=false;


/*****************************
 *   Non specific functions  *
 *****************************/

#ifndef NOUSB

String Padding(int Len, String Val) {
  String ret="";
  if ((Len-Val.length())>0)     
    for (int i = 0 ; i < (Len-Val.length()); i++)  ret.concat(' ');
  ret.concat(Val);
  return ret;
}
#endif

byte IncreaseMotor(byte motor, byte inc) {
  if (((byte)motor+inc)<=255) 
    return ((byte)motor+inc);
  else
    return 255;
}
byte DecreaseMotor(byte motor, byte dec) {
  if (motor-dec>=0)
    return motor=motor-dec;
  else
    return 0; 
}

byte* ChangeMotor(byte motor, byte inc) {
  if ((((int)motor+(int)inc)<=255)&&(((int)motor+(int)inc)>=0))
    return (byte)((int)motor)+((int)inc);
  else if ((((int)motor)+((int)inc))>=255)
    return 255;
  else 
    return 0;
}

bool Range(float val1, float val2) {
  return ((val1>=val2-0.01)&&(val1<=val2+0.01));
}

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

/*****************************
 *   74h specific functions  *
 *****************************/
 
#ifndef NO74H


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

void Setup74H() {

  pinMode(LED_LATCH_PIN, OUTPUT);
  pinMode(LED_CLOCK_PIN, OUTPUT);  
  pinMode(LED_SHIFT_PIN, OUTPUT); 
  
  for (int i=0;i<8;i++) {
    m[i].enable=false;
    m[i].elapse=0;
    m[i].latency=0;
  }
    
  bitClear(utilityRegister,0);
  bitClear(utilityRegister,1);
  bitClear(utilityRegister,2);
  bitClear(utilityRegister,3);
  bitClear(utilityRegister,4);
  bitClear(utilityRegister,5);
  bitClear(utilityRegister,6);
  bitClear(utilityRegister,7);

  digitalWrite(LED_LATCH_PIN, LOW);
  shiftOut(LED_SHIFT_PIN, LED_CLOCK_PIN, LSBFIRST, utilityRegister);
  digitalWrite(LED_LATCH_PIN, HIGH);
}


void EnableDisable(bool IsEnable, int num) {
  for (int i = 0 ; i<8; i ++) {  
    m[i].enable = (((i==num)||(num==-1))?IsEnable:m[i].enable);  
    if (!m[i].enable)
      bitClear(utilityRegister,i);
    else
      bitSet(utilityRegister,i);   
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
    Governer();
  } 
}

void ZeroPower() {
  for (int i=0;i<8;i++) EnableDisable(false,i);
//  motor1=0;
//  motor2=0;
//  motor3=0;
//  motor4=0;
//  digitalWrite(PWM_MOTOR1_PIN,LOW);
//  digitalWrite(PWM_MOTOR2_PIN,LOW);
//  digitalWrite(PWM_MOTOR3_PIN,LOW);
//  digitalWrite(PWM_MOTOR4_PIN,LOW);
}

void FullPower() {
  for (int i=0;i<8;i++) EnableDisable(true,i);
//  motor1=255;
//  motor2=255;
//  motor3=255;        
//  motor4=255;
//  digitalWrite(PWM_MOTOR1_PIN,HIGH);
//  digitalWrite(PWM_MOTOR2_PIN,HIGH);
//  digitalWrite(PWM_MOTOR3_PIN,HIGH);
//  digitalWrite(PWM_MOTOR4_PIN,HIGH);
}

void OddPower() {
  for (int i=0;i<8;i++) EnableDisable(((i % 2)==1),i);
}

void EvenPower() {
  for (int i=0;i<8;i++) EnableDisable(((i % 2)==0),i);
}

void IncreasePulse() {
  for (int i =0;i<8;i++) {
     if (m[i].latency>1) IncreaseDecrease(true,i,50);  
  }
}

void DecreasePulse() {
  for (int i =0;i<8;i++) {
    if (m[i].latency<1001) IncreaseDecrease(false,i,50);
  }
}

void  AlternatePower() {
  static bool first=!(m[0].elapse<1);
  for (int i =0;i<8;i++) {  
    if (first>0) 
      m[i].elapse=-abs(m[i].elapse);
    else
      m[i].elapse=abs(m[i].elapse);
    first=!first;
  }    
}

void RegulatedPower() {
  for (int i =0;i<8;i++) m[i].elapse=0;
}

void QuarterTrimsPower() {
  motor1=64;
  motor2=64;
  motor3=64;
  motor4=64;
  digitalWrite(PWM_MOTOR1_PIN,HIGH);
  digitalWrite(PWM_MOTOR2_PIN,HIGH);
  digitalWrite(PWM_MOTOR3_PIN,HIGH);
  digitalWrite(PWM_MOTOR4_PIN,HIGH); 
}

void HalfTrimsPower() {
  motor1=128;
  motor2=128;
  motor3=128;        
  motor4=128;
  digitalWrite(PWM_MOTOR1_PIN,HIGH);
  digitalWrite(PWM_MOTOR2_PIN,HIGH);
  digitalWrite(PWM_MOTOR3_PIN,HIGH);
  digitalWrite(PWM_MOTOR4_PIN,HIGH);
}

#endif

/*****************************
 *   MTU specific functions  *
 *****************************/
 
#ifndef NOUSB
#ifdef DEBUGMTU
void DebugMTU()
{
    Serial.print("Offset: ");
    Serial.print(Padding(6, String(offsetX)));
    Serial.print(Padding(6, String(offsetY)));
    Serial.print(Padding(6, String(offsetZ)));
    Serial.print("  Actual: ");
    Serial.print(Padding(6, String(actualX)));
    Serial.print(Padding(6, String(actualY)));
    Serial.print(Padding(6, String(actualZ)));
    Serial.print("  Virtue: ");
    Serial.print(Padding(6, String(virtueX)));
    Serial.print(Padding(6, String(virtueY)));
    Serial.print(Padding(6, String(virtueZ)));
    Serial.print("  Target: ");
    Serial.print(Padding(6, String(targetX)));
    Serial.print(Padding(6, String(targetY)));
    Serial.print(Padding(6, String(targetZ))); 
    Serial.print("  Pickup: ");
    Serial.print(Padding(6, String(pickupX)));
    Serial.print(Padding(6, String(pickupY)));
    Serial.print(Padding(6, String(pickupZ)));
    Serial.print("  Median: ");
    Serial.print(Padding(6, String(medianX)));
    Serial.print(Padding(6, String(medianY)));
    Serial.print(Padding(6, String(medianZ)));
}
#endif
#endif

#ifndef NOMTU  
void Accellerator1() {

  digitalWrite(MTU_INTR_PIN, LOW);
  // === Read acceleromter data === //
  Wire.beginTransmission(0x68);
  Wire.write(0x3B); // Start with register 0x3B (ACCEL_XOUT_H)
  Wire.endTransmission(false);
  digitalWrite(MTU_INTR_PIN,HIGH);
}

void Accellerator2() {

  digitalWrite(MTU_INTR_PIN, LOW);
  Wire.requestFrom(0x68, 6, true); // Read 6 registers total, each axis value is stored in 2 registers
  //For a range of +-2g, we need to divide the raw values by 16384, according to the datasheet
  actualX = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offsetX); // X-axis value
  actualY = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offsetY); // Y-axis value
  actualZ = (((Wire.read() << 8 | Wire.read()) / (16384.0*2))-offsetZ); // Z-axis value
  digitalWrite(MTU_INTR_PIN,HIGH);

}
void Accellerator3() {

  pickupX = (targetX-actualX);
  pickupY = (targetY-actualY);
  pickupZ = (targetZ-actualZ);

  medianX=(medianX+actualX);
  medianY=(medianY+actualY);
  medianZ=(medianZ+actualZ);
  if (avgcnt<=10) {
    avgcnt=avgcnt+1;
    if (avgcnt==10) {
      medianX=(medianX/10);
      medianY=(medianY/10);
      medianZ=(medianZ/10);
    }
  }
  if (avgcnt==11) {
      medianX=(medianX/2);
      medianY=(medianY/2);
      medianZ=(medianZ/2);
  }

  virtueX=((((-(actualX-medianX))+pickupX)+(targetX-pickupX))/2);
  virtueY=((((-(actualY-medianY))+pickupY)+(targetY-pickupY))/2);
  virtueZ=((((-(actualZ-medianZ))+pickupZ)+(targetZ-pickupZ))/2);  

  float checkX=targetX-actualX;
  float checkY=targetY-actualY;
  float checkZ=targetZ-actualZ;  

  if ((millis()-elapseTarget)>BALANCE_INTERVAL) {
    elapseTarget=millis();

    if ((targetX!=0)&&(targetX/4!=0)) 
      targetX=targetX-(targetX/4);
    else
      targetX=0;

    if ((targetY!=0)&&(targetY/4!=0)) 
      targetY=targetY-(targetY/4);
    else
      targetY=0;

    if ((targetZ!=0)&&(targetZ/4!=0)) 
      targetZ=targetZ-(targetZ/4);
    else
      targetZ=0;
  }

  if ((pickupZ!=0)&&(checkZ!=0)) {
    motor1+=((float)pickupZ);
    motor2+=((float)pickupZ);
    motor3+=((float)pickupZ);
    motor4+=((float)pickupZ);
  }

  if ((virtueX!=0)&&(checkX!=0)) {
    motor1+=(((float)virtueX)/2);
    motor2+=(((float)virtueX)/2);
    motor3-=(((float)virtueX)/2);
    motor4-=(((float)virtueX)/2);
  }

  if ((virtueY!=0)&&(checkY!=0)) {
    motor1+=(((float)virtueY)/2);
    motor4+=(((float)virtueY)/2);
    motor2-=(((float)virtueY)/2);
    motor3-=(((float)virtueY)/2);
  }

  if (motor1>PWM_MAX_SPEED)
    motor1=PWM_MAX_SPEED; 
  else if (motor1<0)
    motor1=0;

  if (motor2>PWM_MAX_SPEED)
    motor2=PWM_MAX_SPEED;
  else if (motor2<0)
    motor2=0;

  if (motor3>PWM_MAX_SPEED)
    motor3=PWM_MAX_SPEED;
  else if (motor3<0)
    motor3=0;

  if (motor4>PWM_MAX_SPEED)
    motor4=PWM_MAX_SPEED;
  else if (motor4<0)
    motor4=0;

  Accellerator();
}

#endif

static void Accellerator() {
  
  motor1 = map(AxisZ, 0, 1024, 0, 255);
  motor2 = map(AxisZ, 0, 1024, 0, 255);
  motor3 = map(AxisZ, 0, 1024, 0, 255);
  motor4 = map(AxisZ, 0, 1024, 0, 255);
  
  if (motor1==0)
    digitalWrite(PWM_MOTOR1_PIN,LOW);
  else
    analogWrite(PWM_MOTOR1_PIN,motor1);
  if (motor2==0)
    digitalWrite(PWM_MOTOR2_PIN,LOW);
  else
    analogWrite(PWM_MOTOR2_PIN,motor2);
  if (motor3==0)
    digitalWrite(PWM_MOTOR3_PIN,LOW);
  else
    analogWrite(PWM_MOTOR3_PIN,motor3);
  if (motor4==0)
    digitalWrite(PWM_MOTOR4_PIN,LOW);
  else
    analogWrite(PWM_MOTOR4_PIN,motor4);
}

#ifndef NOMTU  
void SetupMTU() {
  pinMode(MTU_INTR_PIN,OUTPUT); 
  digitalWrite(MTU_INTR_PIN,LOW);  
  
  Wire.begin();                      // Initialize comunication
  Wire.beginTransmission(0x68);       // Start communication with MPU6050 // MPU=0x68
  Wire.write(0x6B);                  // Talk to the register 6B
  Wire.write(0x00);                  // Make reset - place a 0 into the 6B register
  Wire.endTransmission(true);        //end the transmission
  Wire.beginTransmission(0x68);
  Wire.write(0x3B); // Start with register 0x3B (ACCEL_XOUT_H)
  Wire.endTransmission(false);
  Wire.requestFrom(0x68, 6, true); // Read 6 registers total, each axis value is stored in 2 registers
  //Starting with offset, contrary to the example IDK how it will turn out
  offsetX = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // X-axis value
  offsetY = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Y-axis value
  offsetZ = ((Wire.read() << 8 | Wire.read()) / (16384.0*2)); // Z-axis value

  digitalWrite(MTU_INTR_PIN,HIGH);  
}
#endif

/*****************************
 *   PWM specific functions  *
 *****************************/

#ifndef NOPWM
void SetupPWM() {

  pinMode(PWM_MOTOR1_PIN,OUTPUT);
  pinMode(PWM_MOTOR2_PIN,OUTPUT);
  pinMode(PWM_MOTOR3_PIN,OUTPUT);
  pinMode(PWM_MOTOR4_PIN,OUTPUT);

  motor1=0;
  motor2=0;
  motor3=0;
  motor4=0;

  analogWrite(PWM_MOTOR1_PIN,0);
  analogWrite(PWM_MOTOR2_PIN,0);
  analogWrite(PWM_MOTOR3_PIN,0);
  analogWrite(PWM_MOTOR4_PIN,0);

  digitalWrite(PWM_MOTOR1_PIN,LOW);
  digitalWrite(PWM_MOTOR2_PIN,LOW);
  digitalWrite(PWM_MOTOR3_PIN,LOW);
  digitalWrite(PWM_MOTOR4_PIN,LOW);

}
#endif

#ifndef NOUSB

  #ifdef DEBUGPWM
  void DebugPWM() {
    Serial.print("Motors: ");
    Serial.print((byte)motor1);
    Serial.print(' ');
    Serial.print((byte)motor2);
    Serial.print(' ');
    Serial.print((byte)motor3);
    Serial.print(' ');
    Serial.print((byte)motor4);

  }
  #endif

/*****************************
 *   GPS specific functions  *
 *****************************/
 
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

void SerialRead(char ch) {

  static int chmode=1;

  switch (ch) {
    case 'c':
      chmode=2;
      Serial.println("Conrolling Mode");
      break;
    case 'i':
      chmode=1;
      Serial.println("Individual Mode");
      break;
    case 'f':
      chmode=0;
      Serial.println("Functional Mode");
      break;
    case '\r':
    case '\n':
      break;
    default:
      switch (chmode) {
        case 2:
          switch (ch) {
            case 'w':
              motor1++;
              motor2++;
              motor3++;
              motor4++;
              Serial.print("Motor seepd: ");
              Serial.println(motor1);
              break;
            case 'a':
              motor1--;
              motor2--;
              motor3--;
              motor4--;
              Serial.print("Motor seepd: ");
              Serial.println(motor1);
              break;
            case 's':
              targetX=0;
              targetY=0;
              targetZ=0;
              break;
            case 'u':
              targetZ=100;
              break;
            case 'd':
              targetZ=-100;
              break;
            case 'l':
              targetX=-1;
              break;
            case 'r':
              targetX=1;
              break;
            case 'g':
              targetY=1;
              break;
            case 'b':
              targetY=-1;
              break;
          }
          break;
  #ifndef NO74H

        case 1:

          EnableDisable(!m[ch-'0'].enable,ch-'0');
          Serial.print("Logic ");
          Serial.print((ch-'0'));
          Serial.print(" Set ");
          Serial.println(((m[ch-'0'].enable)?"On":"Off"));

          break;
        case 0:

          switch (ch) {
            case '0':
              for (int i=0;i<8;i++) EnableDisable(false,i);
              Serial.println("Zero Power");
              motor1=0;
              motor2=0;
              motor3=0;
              motor1=0;
              break;
            case '1':
              for (int i=0;i<8;i++) EnableDisable(true,i);
              Serial.println("Full Power");
              break;
            case '2':
              for (int i=0;i<8;i++) EnableDisable(((i % 2)==1),i);
              Serial.println("Even Power");
              break;
            case '3':
              for (int i=0;i<8;i++) EnableDisable(((i % 2)==0),i);
              Serial.println("Odd Power");
              break;
            case '4':
              for (int i =0;i<8;i++) IncreaseDecrease(true,i,15);
              Serial.println("Increased Pulse");
              break;
            case '5':
              for (int i =0;i<8;i++) IncreaseDecrease(false,i,15);
              Serial.println("Decreased Pulse");
              break;
            case '6':
              for (int i =0;i<8;i++)
                if ((i % 2)==0) {
                  m[i].elapse=-m[i].latency;
                } else {
                  m[i].elapse=0;
                }          
              Serial.println("Alternate Pulse");
              break;
            case '7':
              for (int i =0;i<8;i++) m[i].elapse=0;
              Serial.println("Regulated Pulse");
            case '8':
              motor1=127;
              motor2=127;
              motor3=127;
              motor4=127;
              Serial.println("Half Power");
            case '9':
              motor1=255;
              motor2=255;
              motor3=255;
              motor4=255;
              Serial.println("Full Power");
              break;
          }  
          break;
  #endif 
      } 
      break;
  }  

}
#endif

/*****************************
 *   ESP specific functions  *
 *****************************/
 
#ifndef NOESP

void Driver() {
    
  String txt = Serial.readStringUntil('\n');
      
  while (txt.length()>0) {        
    switch (txt.charAt(0))
    {
      case 's':
        if (txt.length()>1)
          if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
            Switch = (txt.charAt(1)=='0');
        txt.remove(0,2);
        break;
      case 'd':
        if (txt.length()>1)
          if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
            Depress = (txt.charAt(1)=='1');
        txt.remove(0,2);
        break;
      case 'x':    
        if (txt.length()>4)
          AxisX = Convert(txt.substring(1,5));
        txt.remove(0,5);
        break;      
      case 'y':    
        if (txt.length()>4)
          AxisY = Convert(txt.substring(1,5));
        txt.remove(0,5);
        break;      
      case 'z':    
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
    static int oddEven=-1;
    
    if (Switch) {
      if (!toggler[0]) {
        toggler[0]=true;
        powersOn=!powersOn;
        oddEven=-1;
        for (int i=0;i<8;i++) EnableDisable(powersOn,i);
        if (!powersOn) {
          motor1=0;
          motor2=0;
          motor3=0;
          motor1=0;
        }
      }
    } else if (toggler[0]) toggler[0]=false;
    
    if (Depress) {
      if (!toggler[1]) {
        toggler[1]=true;
        oddEven++;
        if (oddEven==2) {
          oddEven=-1;
          for (int i=0;i<8;i++) EnableDisable(powersOn,i);          
        } else for (int i=0;i<8;i++) 
          EnableDisable(((bool)((i % 2)==oddEven)),i);
      }
    } else if (toggler[1]) toggler[1]=false;
  
    static long lastAxisZ=0;
    if ((lastAxisZ!=AxisZ)&&powersOn) {   
      motor1=map(AxisZ,0,1024, 0,255);
      motor2=map(AxisZ,0,1024, 0,255);
      motor3=map(AxisZ,0,1024, 0,255);
      motor4=map(AxisZ,0,1024, 0,255);
      lastAxisZ=AxisZ;
    }
  } 

}

void SetupESP() {
  #ifndef NOUSB
  Serial.begin(UNIFIED_BAUD_RATE);
  #else
  Serial.begin(UNIFIED_BAUD_RATE);
  pinMode(ESP_CHIP_EN_PIN,OUTPUT);
  digitalWrite(ESP_CHIP_EN_PIN,LOW);
  Serial1.begin(UNIFIED_BAUD_RATE);  
  digitalWrite(ESP_CHIP_EN_PIN,HIGH);
  Serial.setTimeout(NETWORK_YEILDING);
  #endif
}

#endif

/*******************************
 *   Setup and Loop Functions  *
 *******************************/
 
void setup() 
{
  pinMode(LED_BLINK_PIN, OUTPUT);

  #ifndef NOESP
  SetupESP();
  #endif    
  
  #ifndef NOGPS
  SetupGPS();
  #endif

  #ifndef NO74H
  Setup74H();
  #endif

  #ifndef NOMTU
  SetupMTU();
  #endif
  
  #ifndef NOPWM
  SetupPWM();
  #endif 
 
}

void loop()
{

  #ifndef NOMTU       
  Accellerator1();
  #endif

  #ifndef NO74H
  Governer();
  #endif

  #ifndef NOUSB
    if (Serial.available()) {
      char ch = Serial.read();
      SerialRead(ch);
    }
  #endif
  
  #ifndef NOESP
  Driver();
  #endif

  #ifndef NO74H
  Governer();
  #endif
  
  #ifndef NOMTU
  Accellerator2();
  #endif
  
  #ifndef NO74H
  Governer();
  #endif
 
  #ifndef NOMTU
  Accellerator3();
  #else
  Accellerator();
  #endif  

  #ifndef NO74H
  Governer();
  #endif

  #ifndef NOGPS
  Locator();  
  #endif      

  #ifndef NO74H
  Governer();
  #endif

  #ifndef NOUSB
   #ifndef NOPWM
    #ifdef DEBUGPWM
      DebugPWM();
      #ifdef DEBUGMTU
        Serial.print(' ');
       #else
         #ifdef DEBUGGPS
          Serial.print(' ');
         #else
          Serial.println();
         #endif
       #endif
      #endif
     #endif
     #ifndef NOMTU
      #ifdef DEBUGMTU    
        DebugMTU();
        #ifdef DEBUGGPS
          Serial.print(' ');
        #else
          Serial.println();
        #endif
      #endif
      #ifndef NOGPS    
        #ifdef DEBUGGPS
          DebugGPS();
          Serial.println(); 
        #endif
      #endif
    #else
      #ifndef NOGPS
        #ifdef DEBUGGPS
          DebugGPS();
          Serial.println(); 
        #endif
      #endif
    #endif
  #endif
}
