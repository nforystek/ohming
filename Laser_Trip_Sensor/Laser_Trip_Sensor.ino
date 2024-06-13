//this is a setup for one laser trip


//shared defines
#define tripAtVal 995
#define tripDelay 5


//per trip defines
#define analog1 A0
#define digital1 2
//per trip variables
int aval1=0;
int last1=0;
int prior1=tripAtVal;
int grace1 =tripAtVal;
//end trip declares


void setup() {
  Serial.begin(115200);

  //setup for the trip
  pinMode(analog1, INPUT);  
  pinMode(digital1,OUTPUT);
  digitalWrite(digital1, HIGH);
  last1=(analogRead(analog1)<tripAtVal);
  //end trip setup

}

void loop() {

  //loop for the trip
  aval1 = analogRead(analog1);
  if (aval1<tripAtVal) {
    if ((aval1<=prior1)||(aval1<=grace1)) {
      grace1=aval1;
      if ((last1<=0)&&(last1>-tripDelay)) last1--;      
    } else if (aval1>grace1) {
      if (last1<=-tripDelay) {
        Serial.print('1');
        last1=-last1;
      }
    }
  } else if (aval1>tripAtVal) {
    if ((aval1>=prior1)||(aval1>=grace1)) {
      grace1=aval1;
      if ((last1>=0)&&(last1<-tripDelay)) last1++;
    } else if (aval1<grace1) {
      if (last1>=tripDelay) {
        Serial.print('2');
        last1=-last1;
      }
    }
  }
  prior1=aval1;
  //end trip loop

}