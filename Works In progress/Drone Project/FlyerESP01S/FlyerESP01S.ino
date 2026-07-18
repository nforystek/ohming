//This ESP-01's SSID/KEY
#define WIFIAP_SSID           "FLYER"
#define WIFIAP_KEY            "yourflyerpassword"

//Pair ESP-01's SSID/KEY
#define WIFISTA_SSID           "RADIO"
#define WIFISTA_KEY            "yourradiopassword"

//This ESP-01's IP
#define STATIC_IP           {192,168,4,2}
#define STATIC_GW           {192,168,0,1}
#define STATIC_SUBNET       {255,255,255,0}

//Pair ESP-01's IP
#define SERVERIP            "192.168.4.3"
byte mac[] = {  0x00, 0x00, 0x00, 0x00, 0x00, 0x00};


#include <ESP8266WiFi.h>
#include <Ethernet.h>
#include <WiFiUdp.h>

const char* stassid     = WIFISTA_SSID;
const char* stapassword = WIFISTA_KEY;
const char *apssid = WIFIAP_SSID;
const char *appassword = WIFIAP_KEY;
unsigned int localPort = 2342; 

char packetBuffer[255];
String inc = "";
String out = "";

WiFiUDP Udp;

static void ConnectWiFi() {
  static bool init=false;
  if (init) {    
    Udp.stop();
    WiFi.disconnect();
    init=false;
  }
  WiFi.begin(stassid, stapassword); 
  unsigned long start = millis();
  while (WiFi.status() != WL_CONNECTED) {
    if (millis() - start > 15000) return;
    delay(100);        
  }
  WiFi.config(STATIC_IP, STATIC_GW, STATIC_SUBNET);
  if (WiFi.status() == WL_CONNECTED) {
    WiFi.macAddress(mac);
    Ethernet.begin(mac, WiFi.localIP());
    Udp.begin(localPort);
    init=true;
    return;
  }
}
void WiFiEvent(WiFiEvent_t event) {
    switch(event) {
        //case WIFI_EVENT_STAMODE_GOT_IP:
        //    break;
        case WIFI_EVENT_STAMODE_DISCONNECTED:
            ConnectWiFi();
            break;
    }
}
void setup() {
  Serial.begin(115200);  
  WiFi.softAP(apssid, appassword);
  //WiFi.onEvent(WiFiEvent);
  ConnectWiFi();
}
void loop() {
  while (inc.indexOf('\n')>0) {
    Serial.println(inc.substring(0,inc.indexOf('\n')));
    inc.remove(0,inc.indexOf('\n')+1);    
  }
  if (Serial.available()) {
    out.concat(Serial.readStringUntil('\n'));
    out.concat('\n');
  }  
  while ((out.indexOf('\n')>0)&&(WiFi.status() == WL_CONNECTED)) {
    Udp.beginPacket(SERVERIP, localPort);
    Udp.println(out.substring(0,out.indexOf('\n')));
    Udp.endPacket();
    out.remove(0,out.indexOf('\n')+1);
  }
  if (WiFi.status() == WL_CONNECTED) {
    if (Udp.parsePacket()) {
      int len = Udp.read(packetBuffer, 255);
      if (len>0) {
        packetBuffer[len]='\0';
        for (int i = 0; i<len;i++)
          inc.concat(packetBuffer[i]);      
      }    
    }
  } else ConnectWiFi();
}
