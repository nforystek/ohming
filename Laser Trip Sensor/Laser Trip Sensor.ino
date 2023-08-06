#define analog1 A0
#define digital1 12
#define analog2 A1
#define digital2 11
#define analog3 A2
#define digital3 10
#define analog4 A3
#define digital4 9
#define tripAtVal 980

bool last1=false;bool last2=false;
bool last3=false;bool last4=false;
int aval1=0;int aval2=0;
int aval3=0;int aval4=0;

void setup() {
  Serial.begin(115200);   
  pinMode(digital1,OUTPUT);
  digitalWrite(digital1, HIGH);
  last1=(analogRead(analog1)<tripAtVal);  
  pinMode(digital2,OUTPUT);
  digitalWrite(digital2, HIGH);
  last2=(analogRead(analog2)<tripAtVal);
  pinMode(digital3,OUTPUT);
  digitalWrite(digital3, HIGH);
  last3=(analogRead(analog3)<tripAtVal);
  pinMode(digital4,OUTPUT);
  digitalWrite(digital4, HIGH);
  last4=(analogRead(analog4)<tripAtVal);    
}
void loop() {
  aval1 = analogRead(analog1);  
  aval2 = analogRead(analog2);  
  aval3 = analogRead(analog3); 
  aval4 = analogRead(analog4);  
  if (aval1<tripAtVal) {
      if (!last1) {
        Serial.print('1');
        last1=true;
      }
  } else if (aval1>=tripAtVal) {
      if (last1) {
        Serial.print('5');
        last1=false;
      }
  }  
  if (aval2<tripAtVal) {
      if (!last2) {
        Serial.print('2');
        last2=true;
      }
  } else if (aval2>=tripAtVal) {
      if (last2) {
        Serial.print('6');
        last2=false;
      }
  }  
  if (aval3<tripAtVal) {
      if (!last3) {
        Serial.print('3');
        last3=true;
      }
  } else if (aval3>=tripAtVal) {
      if (last3) {
        Serial.print('7');
        last3=false;
      }
  }  
  if (aval4<tripAtVal) {
      if (!last4) {
        Serial.print('4');
        last4=true;
      }
  } else if (aval4>=tripAtVal) {
      if (last4) {
        Serial.print('8');
        last4=false;
      }
  }

}
