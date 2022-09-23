
#define NOUSB

/******************
 *   PIN defines  *
 ******************/

//LED on board
#define LED_BLINK_PIN 13

//ESP8266 IO0 and IO2 pins
#define ESP_SERIAL_RX_PIN 1
#define ESP_SERIAL_TX_PIN 0
#define ESP_CHIP_EN_PIN 9

//JOYSTICK
#define JOY_AXISX_PIN A1
#define JOY_AXISY_PIN A2
#define JOY_SWITCH_PIN 7

//Button
#define BTN_NO_PUSH_PIN 8

//Altimeter potentiometer
#define ALT_AXISZ_PIN A3

/********************************
 *   Includes and initializers  *
 ********************************/
#include <Wire.h>   
#include <SoftwareSerial.h>
SoftwareSerial Serial1(ESP_SERIAL_RX_PIN,ESP_SERIAL_TX_PIN);
//HardwareSerial Serial1(Serial);
#include <TinyGPS.h> 
/********************************
 *   Program operation defines  *
 ********************************/

#define UNIFIED_BAUD_RATE 115200

/**************
 *   Globals  *
 **************/

bool Switch=false, Depress=false;
int AxisX=0, AxisY=0, AxisZ=0;

/*******************************
 *   Setup and Loop Functions  *
 *******************************/

void setup() {
  pinMode(LED_BLINK_PIN, OUTPUT); 
  
  #ifndef NOUSB
    Serial.begin(UNIFIED_BAUD_RATE);
  #else

    Serial1.begin(UNIFIED_BAUD_RATE);  
    
    pinMode(ESP_CHIP_EN_PIN,OUTPUT);
    digitalWrite(ESP_CHIP_EN_PIN,LOW);     
    Serial.begin(UNIFIED_BAUD_RATE);
    digitalWrite(ESP_CHIP_EN_PIN,HIGH);
    
  #endif

  //blance analog to the value of the variable resistor
  analogWrite(ALT_AXISZ_PIN, analogRead(ALT_AXISZ_PIN));
  
  pinMode(BTN_NO_PUSH_PIN, INPUT);
  digitalWrite(BTN_NO_PUSH_PIN, LOW);
  
  pinMode(JOY_SWITCH_PIN, INPUT);
  digitalWrite(JOY_SWITCH_PIN, HIGH);

  

}

void loop() {

  int temp;
  String txt="";
  bool sendit=false;
  
  temp=digitalRead(BTN_NO_PUSH_PIN);
  if (temp!=Switch) {
    Switch=temp;
    sendit=true;    
  }
  temp=analogRead(JOY_AXISX_PIN);
  if (temp!=AxisX) {
    AxisX=temp;
    sendit=true;
  }
  temp=analogRead(JOY_AXISY_PIN);
  if (temp!=AxisY) {
    AxisY=temp;
    sendit=true;
  }
  temp=map(analogRead(ALT_AXISZ_PIN), 0, 881, 0, 1024);
  if (temp!=AxisZ) {
    AxisZ=temp;
    sendit=true;
  }
  temp=digitalRead(JOY_SWITCH_PIN);
  if (temp!=Depress) {
    Depress=temp;
    sendit=true;
  }
 
  txt.concat('s');
  txt.concat(Switch);

  txt.concat('x'); 
  if (AxisX<1000)
    txt.concat('0');
  if (AxisX<100)
    txt.concat("0");
  if (AxisX<10) 
    txt.concat("0");
  txt.concat(AxisX);

  txt.concat('y');  
  if (AxisY<1000)
    txt.concat('0');
  if (AxisY<100)
    txt.concat("0");
  if (AxisY<10) 
    txt.concat("0");
  txt.concat(AxisY);

  txt.concat('z'); 
  if (AxisZ<1000)
    txt.concat('0');
  if (AxisZ<100)
    txt.concat("0");
  if (AxisZ<10) 
    txt.concat("0");
  txt.concat(AxisZ);
  
  txt.concat('d');
  txt.concat(Depress);

  Serial.println(txt);
  delay(50);

}
