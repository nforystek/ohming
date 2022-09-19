// read that it has 100% duty cycle, meaning no room for available() function in the cycles
//as it touches the buffer to get the number of characters waiting, that would chrunch the
//cycle we have to have the Arduino preforming with or it would cause data loss for the ESP
//proceeding in instructions, it also isn't entirely reliable at 115200 I think 57600 shows
//more reliable to elimante garbage characters occationally, and the delay 1, in the while(1)
//it's something you'de have to play with depending on whats sent as to how to do checks if any
//it can be dificult to organize all the chips doiing their functions with the ESP being so needy
//so how the sepak goes matters, the ESP BASIC firmware that I went with for ESP-01 labaled moduels,
//preforms serial like an Arduino nicely, but requires extra processing on the firmare, that gets
//heavy quick, not much room or execution before it's sluggish, and it's startup is 30 seconds that
//doesnt' catch station to AP so easily because the station tries first, but so I depreciated this.

#include <Wire.h>
#include <SoftwareSerial.h>
#include <TinyGPS.h> 


//#define UNO

//Comment out NOSERIAL to preserve resource when
//operating, no serial debug will be outputted
#define NOSERIAL

//Comment out NOWIFI to have serial input and
//output conole control connected to the USB.
//#define NOWIFI

//Comment out NO74H to ignore the 74H chip
//absent auxiliary logics in operating mode
//#define NO74H

//Comment out no GPS to disable the Neo-GPS
//#define NOGPS

//The following DEBUG are visual serial console
//and only present when NOWIFI is defined also.
//#define DEBUGGPS
//#define DEBUGMTU

//The following DEBUG puts all 10 logics into
//a systems wiring check mode pulsing outputs
//#define DEBUG74H

//Uncomment for debugging the mototrs PWM

//#define DEBUGPWM

//Constants for the WiFi's AT comm verb reply
#define RESPONSE_COUNT 5
#define MAX_RESPONSE_LEN 5

//PIN constants for PWM
#define PWM_MOTOR1_PIN 5
#define PWM_MOTOR2_PIN 9
#define PWM_MOTOR3_PIN 10
#define PWM_MOTOR4_PIN 11

#define PWM_MIN_SPEED 15
#define PWM_MAX_SPEED 255

//PIN constants for 74H ICS
#define LED_CLOCK_PIN 4
#define LED_LATCH_PIN 13
#define LED_SHIFT_PIN 12

//Interrupt Pin for the MTU
#define MTU_INTR_PIN 7

struct response {
  byte len;
  bool err;
  char val[MAX_RESPONSE_LEN];
};
struct utility {
  //74H program properties
  bool enable;
  long elapse;
  long latency;
};


SoftwareSerial Serial2(8 ,6);

#ifndef NOGPS
TinyGPS gps; // create gps object 
#endif

float actualX, actualY, actualZ;
float offsetX, offsetY, offsetZ;
float pickupX, pickupY, pickupZ;
float medianX, medianY, medianZ;
float targetX, targetY, targetZ;
float virtueX, virtueY, virtueZ;

unsigned long elapseTarget;

float flat, flon;
unsigned long age;

int avgcnt=0;

bool lastError=false;
static char Incoming[MAX_RESPONSE_LEN];
char* Reponses[]={"ready\0", 
                  "+IPD,\0",
                  "OK\0",
                  "FAIL\0",
                  "ERROR\0"};
float motor1=0;
float motor2=0;
float motor3=0;
float motor4=0;

String inBuffer="";
String outBuffer="";

struct utility m[8];
unsigned char utilityRegister = 0;

byte state= 0; //0=off, 1=up, 2=hoover, 4=down, 8=left, 16=forward, 32=right, 64=backward

bool Range(float val1, float val2) {
     return ((val1>=val2-0.01)&&(val1<=val2+0.01));
}
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

byte ChangeMotor(byte motor, byte inc) {
  if ((((int)(motor+inc))<=255)&&(((int)(motor+inc))>=0))
    return ((byte)motor+inc);
  else if (((int)(motor+inc))>=255)
    return 255;
  else 
    return 0;
}

#ifndef NOSERIAL

String Padding(int Len, String Val) {
  String ret="";
  if ((Len-Val.length())>0)     
    for (int i = 0 ; i < (Len-Val.length()); i++)  ret.concat(' ');
  ret.concat(Val);
  return ret;
}
#endif
void AppendToQue(String *queue, String cmd) {
  String newQue=String(*queue);
  newQue.concat(String(((char)cmd.length())));
  newQue.concat(cmd);
  *queue=newQue;
}
String PopFromQue(String *queue) {
  String newQue = String(*queue);   
  byte b =(byte)newQue.charAt(0); 
  newQue.remove(0,1);
  String cmd=newQue.substring(0,b);
  newQue.remove(0,b);
  *queue=newQue;
  return cmd;
}

String UnloadQue(String *queue) {
  String newQue=String(*queue);
  *queue=String();
  return newQue;
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
    if (m[i].elapse<-m[i].latency) m[i].elapse=-m[i].elapse;
    if (m[i].elapse<1) {
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

  #ifdef DEBUG74H
  
    bitSet(utilityRegister,0);
    bitSet(utilityRegister,1);
    bitSet(utilityRegister,2);
    bitSet(utilityRegister,3);
    bitSet(utilityRegister,4);
    bitSet(utilityRegister,5);
    bitSet(utilityRegister,6);
    bitSet(utilityRegister,7);

  #else
  
    for (int i=0;i<8;i++) {
      m[i].enable=false;
      m[i].elapse=1;
      m[i].latency=1;
    }
    
    bitClear(utilityRegister,0);
    bitClear(utilityRegister,1);
    bitClear(utilityRegister,2);
    bitClear(utilityRegister,3);
    bitClear(utilityRegister,4);
    bitClear(utilityRegister,5);
    bitClear(utilityRegister,6);
    bitClear(utilityRegister,7);

  #endif

  UpdateShiftRegister();
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

void IncreaseDecrease(bool IsIncrease, int num, int val) {
  int newVal;
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
#endif

#ifndef NOSERIAL
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

  if ((millis()-elapseTarget)>100) {
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


  if (state==1) {
    analogWrite(PWM_MOTOR1_PIN,(byte)motor1);
    analogWrite(PWM_MOTOR2_PIN,(byte)motor2);
    analogWrite(PWM_MOTOR3_PIN,(byte)motor3);
    analogWrite(PWM_MOTOR4_PIN,(byte)motor4);
  } else {
    analogWrite(PWM_MOTOR1_PIN,0);
    analogWrite(PWM_MOTOR2_PIN,0);
    analogWrite(PWM_MOTOR3_PIN,0);
    analogWrite(PWM_MOTOR4_PIN,0);
  }

}
#endif


#ifndef NOMTU  
void SetupMTU() {
  pinMode(MTU_INTR_PIN,OUTPUT);  
  digitalWrite(MTU_INTR_PIN, LOW);
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

#ifndef NOSERIAL

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

  Serial2=SoftwareSerial(8,6);

  Serial2.begin(115200);
}
#endif

#ifndef NOSERIAL

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
              targetZ=1;
              break;
            case 'd':
              targetZ=-1;
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
              Serial.println("Half Power");
              break;
            case '3':
              for (int i=0;i<8;i++) EnableDisable(((i % 2)==0),i);
              Serial.println("Hold Power");
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

#ifndef NOWIFI

void DataRead(String data) {
  if (data.length()>0) {  
    if (NextArg(data, " ") == "AT") {
      DataSend("IP");
    } else if ((data.indexOf("[")>0)&&(data.indexOf("]")>0)) {
      data.remove(0,data.indexOf("[")+1);
      data.remove(data.indexOf("]"));

      for (int i=0;i<data.length();i++) {
        switch (data.charAt(i))
        {
          case 'u':
            targetZ=-1;
            state=1;
            break;
          case 'd':
            targetZ=1;
            state=1;
            break;
          case 'l':
            targetX=-1;
            state=1;
            break;
          case 'r':
            targetX=1;
            state=1;
            break;
          case 'f':
            targetY=1;
            state=1;
            break;
          case 'b':
            targetY=-1;
            state=1;
            break;
          case 'x':
            targetY=0;
            targetX=0;
            targetZ=0;
            motor1=0;
            motor2=0;
            motor3=0;
            motor4=0;
            state=0;
            break;
        } 
      } 
    }
  }
}

struct response NewResponse(String val, bool err) {
  struct response ret;
  for (int i=0;i<val.length();i++)
    ret.val[i]=val.charAt(i);
  ret.len=val.length();
  ret.err = err;
  return ret;
}

bool ResponseCheck() {

  lastError=false;
  int j=0;
  while (1) {
    Incoming[j]=Serial1.read();
    for (int i = 0; i<RESPONSE_COUNT;i++)
    {      
      while (Incoming[j]==Reponses[i][j])
      {
        j++;
        if (Reponses[i][j]=='\0'){
          lastError = (i>2);
          return (i<2);
        }
        Incoming[j]=Serial1.read();
      }
      j=0;
    }
    delay(1);    
  }
}

void DataOut(String out) {
    String temp = "AT+CIPSEND=0,";       
    temp.concat(String(out.length(), DEC));
    Serial1.println(temp);
    ResponseCheck();
    Serial1.println(out); 
    ResponseCheck();
}

void DataSend(String text) {
  AppendToQue(&outBuffer,text);
}

void Driver() {
  int i=0;
  if (ResponseCheck())  {
    char data[10];
    char ch = ((char)Serial1.read());
    while (ch!=',') ch=((char)Serial1.read());
    ch=((char)Serial1.read());
    while((i<10)&&(IsALN(ch))) {
      data[i]=ch;
      i++;
      ch =((char)Serial1.read());
    }
    if (ch==':') {
      long len = Convert(i, data);
      if (len>0) {
        for (int i=0;i<len;i++) inBuffer.concat(((char)Serial1.read()));
      } 
      while (inBuffer.length()>0)        
        DataRead(PopFromQue(&inBuffer));
    }
  } 
  while (outBuffer.length()>0)
    DataOut(UnloadQue(&outBuffer));
}

void SetupESP() {  

  Serial1.begin(115200);

  //Serial.begin(115200);

 // Serial.println("AT+RST");   
 // delay(1000);

  
  Serial1.println("AT+CWMODE_DEF=3"); 
  ResponseCheck();

  Serial1.println("AT+CWSAP=\"YAADD\",\"OMITTE\",11,2");
  ResponseCheck();

  Serial1.println("AT+CIPMUX=1"); 
  ResponseCheck();

  Serial1.println("AT+CIPSERVER=1"); 
  ResponseCheck();

  Serial.println("Initialize the server!");
  delay(5000);
  state=1;
}
#endif

#ifndef NOPWM
void SetupPWM() {
  #ifdef DEBUGPWM
  analogWrite(PWM_MOTOR1_PIN,40);
  analogWrite(PWM_MOTOR2_PIN,40);
  analogWrite(PWM_MOTOR3_PIN,40);
  analogWrite(PWM_MOTOR4_PIN,40);
  #else
  analogWrite(PWM_MOTOR1_PIN,0);
  analogWrite(PWM_MOTOR2_PIN,0);
  analogWrite(PWM_MOTOR3_PIN,0);
  analogWrite(PWM_MOTOR4_PIN,0);
  #endif
}
#endif


void setup() 
{     
  
  #ifndef NOWIFI
  SetupESP();
  #endif
  
  #ifndef NOPWM
  SetupPWM();
  #endif
  
  #ifndef NOGPS
  SetupGPS();
  #endif

  #ifndef NOMTU
  SetupMTU();
  #endif

  #ifndef NO74H
  Setup74H();
  #endif

  #ifndef NOSERIAL
  Serial.begin(15200);
  #endif

}

void loop()
{

  #ifndef NOMTU       
  Accellerator1();
  #endif

  #ifndef NOSERIAL
    if (Serial.available()) {
      char ch = Serial.read();
      SerialRead(ch);
    }
  #endif
  
  #ifndef NOWIFI
  Driver();
  #endif
  
  #ifndef NOMTU
  Accellerator2();
 #endif
 
  #ifndef NO74H
  Governer();
  #endif
 
  #ifndef NOMTU
  Accellerator3();
  #endif
 
  #ifndef NOGPS
  Locator();  
  #endif
      
  #ifndef NOSERIAL
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
    #ifdef DEBUGGPS
      DebugGPS();
      Serial.println(); 
    #endif
   #endif
  #endif

}
