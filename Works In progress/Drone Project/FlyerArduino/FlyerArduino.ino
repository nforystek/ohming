
 /*******************************
  *   All s go defines  *
 ********************************/
 
//Comment out NOUSB to preserve resource when
//operating, no serial debug will be outputted
#define NOUSB

//Comment out NOESP to have serial input and
//output conole control connected to the USB.
//#define NOESP

//Comment ou no MPU to disable the
//pitch/yaw/tilt motion sensor chip
//#define NOMPU

/************************
 *   Debugging defines  *
 ************************/
 
//The following DEBUG are visual serial console
//and only present when NOESP is defined also.
#define DEBUGMPU

//Uncomment for debugging the controller buttons.
//#define DEBUGESP                                                                                                                                                                

//Uncomment for debugging the motors
//#define DEBUGPWM

//Rate and Timer 
#define UNIFIED_BAUD_RATE 115200
#define BALANCE_INTERVAL 1000
#define NETWORK_INTERVAL 100
#define NETWORK_YEILDING 50

//ESP8266 IO0 and IO2 pins
#define ESP_SERIAL_RX_PIN 1
#define ESP_SERIAL_TX_PIN 0
#define ESP_CHIP_EN_PIN 4

//Interrupt Pin for the MPU
#define MPU_ADDRESS 0x68
#define MPU_INTR_PIN 13


//PWM constants 
#define PWM_MOTOR1_PIN 5 //front left
#define PWM_MOTOR2_PIN 6 //front right
#define PWM_MOTOR3_PIN 9 //back left
#define PWM_MOTOR4_PIN 10 //back right


#define PWM_MIN_SPEED 1 //slowest taxi state
#define PWM_MAX_SPEED 255 //byte maximum
#define PWM_CHANGE_RATE 15


#include <Wire.h>
#ifndef NOESP
#include <SoftwareSerial.h>
SoftwareSerial Serial1(ESP_SERIAL_RX_PIN,ESP_SERIAL_TX_PIN);
#endif



bool toggler[6]={false,false,false,false,false,false};

bool joy1_depress=false;
bool joy2_depress=false;

bool btn1_depress=false;
bool btn2_depress=false;
bool btn3_depress=false;
bool btn4_depress=false;

float joy1_axisx =0;
float joy1_axisy =0;

float joy2_axisx =0;
float joy2_axisy =0;

float alt_axisz = 0;

bool powered=false;
float motor1=0;
float motor2=0;
float motor3=0;
float motor4=0;

float lastMotor1 = lastMotor1 + motor1;
float lastMotor2 = lastMotor2 + motor2;
float lastMotor3 = lastMotor3 + motor3;
float lastMotor4 = lastMotor4 + motor4;
int lastMotorCount =0;
unsigned long motorElapse=millis();


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
 *   MPU specific functions  *
 *****************************/
#ifndef NOMPU 


float yaw = 0.0;
float pitch = 0.0;
float roll = 0.0;

#ifndef NOUSB
#ifdef DEBUGMPU

      #ifndef DEBUG
        #define DEBUG
      #endif
void DebugMPU()
{

    Serial.print("  Yaw: ");
    Serial.print(Padding(6, String(yaw)));
    Serial.print(" Pitch: ");
    Serial.print(Padding(6, String(pitch)));
    Serial.print(" Roll: ");
    Serial.print(Padding(6, String(roll)));

}
#endif
#endif


unsigned long lastTime = 0;

float gyroBiasZ = 0;

void calibrateGyro()
{
    long sum = 0;
    const int samples = 500;

    for (int i = 0; i < samples; i++)
    {
        Wire.beginTransmission(0x68);
        Wire.write(0x47);  // GYRO_ZOUT_H
        Wire.endTransmission(false);
        Wire.requestFrom(0x68, 2);

        int16_t raw = (Wire.read() << 8) | Wire.read();
        sum += raw;

        delay(2);
    }

    gyroBiasZ = (sum / samples) / 131.0;  // deg/sec
}


void getYawDeg()
{
  digitalWrite(MPU_INTR_PIN, LOW);
  
  Wire.beginTransmission(MPU_ADDRESS);
  Wire.write(0x47);  // GYRO_ZOUT_H
  Wire.endTransmission(false);
  Wire.requestFrom(MPU_ADDRESS, 2);
    
  int16_t rawGyroZ = (Wire.read() << 8) | Wire.read();
  float gyroZ = rawGyroZ / 131.0;
  gyroZ -= gyroBiasZ;   // remove drift


  unsigned long now = micros();
  float dt = (now - lastTime) / 1e6;
  lastTime = now;
  yaw += gyroZ * dt;

  if (yaw >= 360.0) yaw -= 360.0;
  if (yaw < 0)      yaw += 360.0;

  digitalWrite(MPU_INTR_PIN,HIGH);
}


void getPitchRollDeg()
{
  digitalWrite(MPU_INTR_PIN, LOW);

  Wire.beginTransmission(MPU_ADDRESS);
  Wire.write(0x3B);  // ACCEL_XOUT_H
  Wire.endTransmission(false);
  Wire.requestFrom(MPU_ADDRESS, 6);    

  int16_t ax = (Wire.read() << 8) | Wire.read();
  int16_t ay = (Wire.read() << 8) | Wire.read();
  int16_t az = (Wire.read() << 8) | Wire.read();
  
  float x = ax / 16384.0;  
  float y = ay / 16384.0;
  float z = az / 16384.0;

  roll = atan2(x, z) * 180.0 / PI;
  if (roll < 0) roll += 360.0;

  pitch = atan2(y, z) * 180.0 / PI;
  if (pitch < 0) pitch += 360.0;

  digitalWrite(MPU_INTR_PIN,HIGH);
}



void SetupMPU() {
  pinMode(MPU_INTR_PIN,OUTPUT);  // setup the interrupt pin
  digitalWrite(MPU_INTR_PIN,LOW);  // trun of the interupt, similar to chip enabled (CE) = HIGH

  
  Wire.begin();                      // Initialize comunication
  Wire.beginTransmission(MPU_ADDRESS);  // Start communication with MPU6050 // MPU=0x68
  Wire.write(0x6B);                  // Talk to the register 6B
  Wire.write(0x00);                  // Make reset - place a 0 into the 6B register
  Wire.endTransmission(true);        //end the transmission

  calibrateGyro();

  digitalWrite(MPU_INTR_PIN,HIGH);  //interupt the chip so other chips may use A4 and A5
}
#endif



#ifndef NOUSB
#ifdef DEBUGPWM
      #ifndef DEBUG
        #define DEBUG
      #endif
void DebugPWM()
{

      Serial.print(' ');
      Serial.print(String(motor1));
      Serial.print(' ');
      Serial.print(String(motor2));
      Serial.print(' ');
      Serial.print(String(motor3));
      Serial.print(' ');
      Serial.print(String(motor4));
}
#endif
#endif
/*********************
 *   Engine Private  *
 *********************/
 
void UpdateEngines() {
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

  if (powered) {
    analogWrite(PWM_MOTOR1_PIN,motor1);
    analogWrite(PWM_MOTOR2_PIN,motor2);
    analogWrite(PWM_MOTOR3_PIN,motor3);
    analogWrite(PWM_MOTOR4_PIN,motor4);
  } else {
    analogWrite(PWM_MOTOR1_PIN,0);
    analogWrite(PWM_MOTOR2_PIN,0);
    analogWrite(PWM_MOTOR3_PIN,0);
    analogWrite(PWM_MOTOR4_PIN,0);
  }
}

void PowerOff() {
  motor1=0;
  motor2=0;
  motor3=0;
  motor4=0;
  UpdateEngines();
  powered=false;
}

void PowerOn() {
  motor1=PWM_MIN_SPEED;
  motor2=PWM_MIN_SPEED;
  motor3=PWM_MIN_SPEED;
  motor4=PWM_MIN_SPEED;
  UpdateEngines();
  powered=true;
}


void SetupPWM() {
  
  motor1=0; //back right
  motor2=0; //front left
  motor3=0; //front right
  motor4=0; //back left
  powered=false;
  
  pinMode(PWM_MOTOR1_PIN,OUTPUT);
  pinMode(PWM_MOTOR2_PIN,OUTPUT);
  pinMode(PWM_MOTOR3_PIN,OUTPUT);
  pinMode(PWM_MOTOR4_PIN,OUTPUT);

  UpdateEngines();


}

/*****************************
 *   USB specific functions  *
 *****************************/

#ifndef NOUSB

#ifdef DEBUGESP

      #ifndef DEBUG
        #define DEBUG
      #endif
void DebugESP()
{

      Serial.print(String(btn1_depress));
      Serial.print(' ');
      Serial.print(String(btn2_depress));
      Serial.print(' ');
      Serial.print(String(btn3_depress));
      Serial.print(' ');
      Serial.print(String(btn4_depress));
      Serial.print(' ');
      Serial.print(String(joy1_depress));
      Serial.print(' ');
      Serial.print(String(joy2_depress));
      Serial.print(' ');
      Serial.print(String(joy1_axisx));
      Serial.print(' ');
      Serial.print(String(joy1_axisy));
      Serial.print(' ');
      Serial.print(String(joy2_axisx));
      Serial.print(' ');
      Serial.print(String(joy2_axisy));
      Serial.print(' ');
      Serial.print(String(alt_axisz));

}
#endif


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

      break;

  }

}



#ifdef DEBUGMPU


#endif

#endif


#ifndef NOESP

void AdjustMotors(float *m1, float *m2, float by, float mu) {
  *m1 = ((*m1) + ((by / 100) * PWM_CHANGE_RATE));
  *m2 = ((*m2) + ((by / 100) * PWM_CHANGE_RATE));
}
void Driver() {

  if (Serial.available()) {
    String txt=Serial.readStringUntil('\n');

    if (txt.length()>=37) {
      while (txt.length()>0) {     
        switch (txt.charAt(0))
        {
          case 'a': //front left switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                btn1_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break;
          case 'b': //front right switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                btn2_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break;
          case 'c': //back left switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                btn3_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break;
          case 'd': //middle switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                joy2_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break;
          case 'e': //middle switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                btn4_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break; 
          case 'f': //middle switch on controller
            if (txt.length()>1)
              if ((txt.charAt(1)=='1')||(txt.charAt(1)=='0'))
                joy1_depress = (txt.charAt(1)=='0');
            txt.remove(0,2);        
            break;

 
          case 'i':  //right axis left or right
            if (txt.length()>4)
              joy1_axisx = Convert(txt.substring(1,5));
            txt.remove(0,5);
            break;      
          case 'j': //right axis up or down
            if (txt.length()>4)
              joy1_axisy = Convert(txt.substring(1,5));
            txt.remove(0,5);
            break;  


          case 'g':  //right axis left or right
            if (txt.length()>4)
              joy2_axisx = Convert(txt.substring(1,5));
            txt.remove(0,5);
            break;      
          case 'h': //right axis up or down
            if (txt.length()>4)
              joy2_axisy = Convert(txt.substring(1,5));
            txt.remove(0,5);
            break;     
 

          case 'k': //right axis up or down
            if (txt.length()>4)
              alt_axisz = Convert(txt.substring(1,5));
            txt.remove(0,5);
            break; 
                     
          case 'x':
          default:
            if (txt.length()>0)
              txt.remove(0,1);
            break;
        }        
  
               
        //switch turns all motor controlls on or off
        //axisz is altitude control fall/hold/climb
        //axisx is yaw, aim the drone to a direction
        //axisy forward/backward controls the drone
        //depress auto holds, twice returns to start
        
        if (btn1_depress) {
          if (!toggler[0]) {
            toggler[0]=true;
            if (!powered) {
              PowerOn();
            } else {
              PowerOff();
            }
          }
        } else if (toggler[0]) toggler[0]=false;
    
        if (btn2_depress) {
          if (!toggler[1]) toggler[1]=true;
        } else if (toggler[1]) toggler[1]=false;

        if (btn3_depress) {
          if (!toggler[2]) toggler[1]=true;
        } else if (toggler[2]) toggler[1]=false;

        if (btn4_depress) {
          if (!toggler[3]) toggler[1]=true;
        } else if (toggler[3]) toggler[1]=false;

        if (joy1_depress) {
          if (!toggler[4]) toggler[1]=true;
        } else if (toggler[4]) toggler[1]=false;

        if (joy2_depress) {
          if (!toggler[5]) toggler[1]=true;
        } else if (toggler[5]) toggler[1]=false;

        if (powered) {
  
//          lastMotor1 = lastMotor1 + motor1;
//          lastMotor2 = lastMotor2 + motor2;
//          lastMotor3 = lastMotor3 + motor3;
//          lastMotor4 = lastMotor4 + motor4;
//          lastMotorCount=lastMotorCount+1;
          
          if (alt_axisz>980) alt_axisz=980;
          motor1= map(alt_axisz, 1, 980, PWM_MIN_SPEED, PWM_MAX_SPEED);
          motor2= map(alt_axisz, 1, 980, PWM_MIN_SPEED, PWM_MAX_SPEED);
          motor3= map(alt_axisz, 1, 980, PWM_MIN_SPEED, PWM_MAX_SPEED);
          motor4= map(alt_axisz, 1, 980, PWM_MIN_SPEED, PWM_MAX_SPEED);
          
  
//          if (joy1_axisx<500) {
//            //fly up
//                        
//            //motor1 & motor2 & motor3 & motor4 speed up
//  
//           AdjustMotors(&motor1, &motor2, map(joy1_axisx, 500, 0,1,255), 1);
//           AdjustMotors(&motor3, &motor4, map(joy1_axisx, 500, 0,1,255), 1);
//            
//          } else if (joy1_axisx>500)  {
//            //fly down
//                        
//            //motor1 & motor2 & motor3 & motor4 speed down
//  
//           AdjustMotors(&motor1, &motor2, map(joy1_axisx, 500, 1020,1,255), -1);
//           AdjustMotors(&motor3, &motor4, map(joy1_axisx, 500, 1020,1,255), -1);
//          }
  
          if (joy1_axisy>510) {
            //fly down
              
            //motor1 & motor3 slow down                     
            //-or-
            //motor2 & motor4 speed up
  
            AdjustMotors(&motor1, &motor3, map(joy1_axisy, 510, 1020,0,100), 1);
            AdjustMotors(&motor2, &motor4, map(joy1_axisy, 510, 1020,0,100), -1);
  
  
          } else if (joy1_axisy<510)  {
            //fly up
  
            //motor2 & motor4 slow down 
            //-or-
            //motor1 & motor3 speed up
  
            AdjustMotors(&motor2, &motor4, map(joy1_axisy,510, 0 ,0,100), 1);
            AdjustMotors(&motor1, &motor3, map(joy1_axisy,510, 0 ,0,100), -1);
  
          }
  
          if (joy2_axisx<510) {
            //move forward
  
            //motor1 & motor2 slow down 
            //-or-
            //motor3 & motor4 speed up
  
            AdjustMotors(&motor1, &motor2, map(joy2_axisx, 510, 0 ,0,100), 1);
            AdjustMotors(&motor3, &motor4, map(joy2_axisx, 510, 0 ,0,100), -1);
            
          } else if (joy2_axisx>510)  {
            //mvove backward
  
            //motor3 & motor4 slow down 
            //-or-
            //motor1 & motor2 speed up     
  
  
            AdjustMotors(&motor3, &motor4, map(joy2_axisx, 510, 1020,0,100), 1);
            AdjustMotors(&motor1, &motor2, map(joy2_axisx, 510, 1020,0,100), -1);
  
          }
  
          if (joy2_axisy>500) {
            //rotate left
  
            //motor2 & motor3 slow down 
            //-or-
            //motor1 & motor4 speed up
  
            AdjustMotors(&motor2, &motor3, map(joy2_axisy, 500, 1020,0,100), 1);
            AdjustMotors(&motor1, &motor4, map(joy2_axisy, 500, 1020,0,100), -1);
            
          } else if (joy2_axisy<500)  {
            //rotate right
  
            //motor1 & motor4 slow down 
            //-or-
            //motor2 & motor3 speed up
  
            AdjustMotors(&motor1, &motor4, map(joy2_axisy, 500, 0,0,100), 1);
            AdjustMotors(&motor2, &motor3, map(joy2_axisy, 500, 0,0,100), -1);
          }

        } else {
          motor1=0;
          motor2=0;
          motor3=0;
          motor4=0;
        }


        
//        if (motorElapse-millis()>100) {
//          motorElapse=millis();
//
//          lastMotor1=lastMotor1/lastMotorCount;
//          lastMotor2=lastMotor2/lastMotorCount;
//          lastMotor3=lastMotor3/lastMotorCount;
//          lastMotor4=lastMotor4/lastMotorCount;
//          lastMotorCount=1;
//
//          //dampers from changing too fast
//          if ((motor1-lastMotor1)>PWM_CHANGE_RATE) motor1 = lastMotor1;//-PWM_CHANGE_RATE;
//          if ((motor1-lastMotor1)<-PWM_CHANGE_RATE) motor1 = lastMotor1;//+PWM_CHANGE_RATE;
//          
//          if ((motor2-lastMotor2)>PWM_CHANGE_RATE) motor2 = lastMotor2;//-PWM_CHANGE_RATE;
//          if ((motor2-lastMotor2)<-PWM_CHANGE_RATE) motor2 = lastMotor2;//+PWM_CHANGE_RATE;
//
//          if ((motor3-lastMotor3)>PWM_CHANGE_RATE) motor3 = lastMotor3;//-PWM_CHANGE_RATE;
//          if ((motor3-lastMotor3)<-PWM_CHANGE_RATE) motor3 = lastMotor3;//+PWM_CHANGE_RATE;
//          
//          if ((motor4-lastMotor4)>PWM_CHANGE_RATE) motor4 = lastMotor4;//-PWM_CHANGE_RATE;
//          if ((motor4-lastMotor4)<-PWM_CHANGE_RATE) motor4 = lastMotor4;//+PWM_CHANGE_RATE; 
//          
//        }


        UpdateEngines();
        
  //    int  left = map(AxisX, -1024, 0, MIN_74H_PULSE, MAX_74H_PULSE);
  //    int  right = map(AxisX, 0, 1024, MIN_74H_PULSE, MAX_74H_PULSE);
  //
  //    int  up = map(AxisY, -1024, 0, MIN_74H_PULSE, MAX_74H_PULSE);
  //    int  down = map(AxisY, 0, 1024, MIN_74H_PULSE, MAX_74H_PULSE);
  //
  //    RatePulseInterval(4,((left+up)/2));
  //    RatePulseInterval(5,((left+down)/2));
  //    RatePulseInterval(6,((right+up)/2));
  //    RatePulseInterval(7,((right+down)/2));
  

        
        
      }
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
  #ifndef NOESP
  Serial1.begin(UNIFIED_BAUD_RATE);  
  digitalWrite(ESP_CHIP_EN_PIN,HIGH);
  Serial.setTimeout(NETWORK_YEILDING);
  #endif
  #endif
}

#endif


void setup() 
{
   
  #ifndef NOUSB
  Serial.begin(UNIFIED_BAUD_RATE);
  #endif

  #ifndef NOESP
  SetupESP();
  #endif    
  
  #ifndef NOMPU
  SetupMPU();
  #endif  

  SetupPWM();

}
  
void loop() 
{ 

  #ifndef NOUSB
  Monitor(); //handles input for output serial debugging
  #endif  
  
  #ifndef NOESP
  Driver(); //reads from the network for the controller 
  #endif
  
  #ifndef NOMPU
  getYawDeg();  //first half of retrieval of gyro info
  #endif

  #ifndef NOUSB
  Monitor(); //handles input for output serial debugging
  #endif
  
  #ifndef NOESP
  Driver(); //reads from the network for the controller 
  #endif
  
  #ifndef NOMPU
  getPitchRollDeg(); //second half of retrieval of gyro info
  #endif

  #ifndef NOUSB
    
    #ifdef DEBUGESP
      DebugESP();
    #endif
    
    #ifdef DEBUGPWM
      DebugPWM();
    #endif
  
    #ifdef DEBUGMPU
      DebugMPU();
    #endif
  
    #ifdef DEBUG
      Serial.println();  
  
  
    #endif
  #endif
}