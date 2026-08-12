
#define NOUSB

/******************
 *   PIN defines  *
 ******************/

//LED on board
#define LED_BLINK_PIN 13

//ESP8266 IO0 and IO2 pins
#define ESP_SERIAL_RX_PIN 1
#define ESP_SERIAL_TX_PIN 0
#define ESP_CHIP_EN_PIN 2

//JOYSTICK
#define JOY1_AXISX_PIN A1
#define JOY1_AXISY_PIN A2
#define JOY1_SWITCH_PIN 5

#define JOY2_AXISX_PIN A3
#define JOY2_AXISY_PIN A4
#define JOY2_SWITCH_PIN 4

//Altimeter potentiometer
#define ALT_AXISZ_PIN A0

#define BTN1_SWITCH_PIN 6
#define BTN2_SWITCH_PIN 7
#define BTN3_SWITCH_PIN 8
#define BTN4_SWITCH_PIN 9


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
bool joy1_depress=false;
bool joy2_depress=false;

bool btn1_depress=false;
bool btn2_depress=false;
bool btn3_depress=false;
bool btn4_depress=false;

int joy1_axisx =0;
int joy1_axisy =0;

int joy2_axisx =0;
int joy2_axisy =0;

int alt_axisz = 0;

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
  analogWrite(ALT_AXISZ_PIN, 0);  
  
  pinMode(BTN1_SWITCH_PIN, INPUT);
  digitalWrite(BTN1_SWITCH_PIN, LOW);

  pinMode(BTN2_SWITCH_PIN, INPUT);
  digitalWrite(BTN2_SWITCH_PIN, LOW);

  pinMode(BTN3_SWITCH_PIN, INPUT);
  digitalWrite(BTN3_SWITCH_PIN, LOW);
  
  pinMode(BTN4_SWITCH_PIN, INPUT);
  digitalWrite(BTN4_SWITCH_PIN, LOW);
  
  pinMode(JOY1_SWITCH_PIN, INPUT);
  digitalWrite(JOY1_SWITCH_PIN, HIGH);

  pinMode(JOY2_SWITCH_PIN, INPUT);
  digitalWrite(JOY2_SWITCH_PIN, HIGH);
  

}

void loop() {

  int temp;
  String txt="";
  
  temp=digitalRead(BTN1_SWITCH_PIN);
  if (temp!=btn1_depress) {
    btn1_depress=temp;
  }

  temp=digitalRead(BTN2_SWITCH_PIN);
  if (temp!=btn2_depress) {
    btn2_depress=temp;
  }
  
  temp=digitalRead(BTN3_SWITCH_PIN);
  if (temp!=btn3_depress) {
    btn3_depress=temp;
  }
  
  temp=digitalRead(BTN4_SWITCH_PIN);
  if (temp!=btn4_depress) {
    btn4_depress=temp;
  }
  
  temp=digitalRead(JOY1_SWITCH_PIN);
  if (temp!=joy1_depress) {
    joy1_depress=temp;
  }

  temp=digitalRead(JOY2_SWITCH_PIN);
  if (temp!=joy2_depress) {
    joy2_depress=temp;
  }



  
  temp=analogRead(JOY1_AXISX_PIN);
  if (temp!=joy1_axisx) {
    joy1_axisx=temp;
  }
  
  temp=analogRead(JOY1_AXISY_PIN);
  if (temp!=joy1_axisy) {
    joy1_axisy=temp;
  }

  temp=analogRead(JOY2_AXISX_PIN);
  if (temp!=joy2_axisx) {
    joy2_axisx=temp;
  }
  
  temp=analogRead(JOY2_AXISY_PIN);
  if (temp!=joy2_axisy) {
    joy2_axisy=temp;
  }
  
  temp=analogRead(ALT_AXISZ_PIN);
  if (temp!=alt_axisz) {
    alt_axisz=temp;
  }

    txt.concat('a');
    txt.concat(btn1_depress);
    txt.concat('b');
    txt.concat(btn2_depress);
    txt.concat('c');
    txt.concat(btn3_depress);
    txt.concat('d');
    txt.concat(btn4_depress);
    txt.concat('e');
    txt.concat(joy1_depress);
    txt.concat('f');
    txt.concat(joy2_depress);
    txt.concat('g'); 
    if (joy1_axisx<1000)
      txt.concat('0');
    if (joy1_axisx<100)
      txt.concat('0');
    if (joy1_axisx<10) 
      txt.concat('0');
    txt.concat(joy1_axisx);
    txt.concat('h');  
    if (joy1_axisy<1000)
      txt.concat('0');
    if (joy1_axisy<100)
      txt.concat('0');
    if (joy1_axisy<10) 
      txt.concat('0');
    txt.concat(joy1_axisy);
    txt.concat('i'); 
    if (joy2_axisx<1000)
      txt.concat('0');
    if (joy2_axisx<100)
      txt.concat('0');
    if (joy2_axisx<10) 
      txt.concat('0');
    txt.concat(joy2_axisx);
    txt.concat('j');  
    if (joy2_axisy<1000)
      txt.concat('0');
    if (joy2_axisy<100)
      txt.concat('0');
    if (joy2_axisy<10) 
      txt.concat('0');
    txt.concat(joy2_axisy);
    txt.concat('k'); 
    if (alt_axisz<1000)
      txt.concat('0');
    if (alt_axisz<100)
      txt.concat('0');
    if (alt_axisz<10) 
      txt.concat('0');
    txt.concat(alt_axisz);
 
  Serial.println(txt);
  delay(50);

}