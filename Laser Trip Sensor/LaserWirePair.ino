bool toggle=false;
bool last=false;
int aval=0;
void setup() {
  Serial.begin(115200);  
  pinMode(13,OUTPUT);
  digitalWrite(13, HIGH);  
  last=(analogRead(A0)<1000);
}
void loop() {
  aval = analogRead(A0);  
  if((aval<1000)&&(!last)) {
      if (!toggle) {
        Serial.print("1");
        toggle=true;
        last=true;
      }
  } else {
    toggle=false;
  }
  if((aval>=1000)&&(last)) {
      if (!toggle) {
        Serial.print("2");
        toggle=true;
        last=false;
      }
  } else {
    toggle=false;
  }
}
