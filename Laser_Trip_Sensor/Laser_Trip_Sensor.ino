//this is a bit more indepth sensor check than
//what is in the schematic picture showing code.
//if your Arduino has zero static intereferences
//and you get clean exact numerical repeat values
//for reading your analog ports this isn't better.
//for those of us that fondle our Arduino with out
//hands, this code monitors the minor changes each
//read accomodating for toggle leans and acurate
//trip readings with out repeating for both open
//and close actions that must happen in toggling.
//never the less it is two more int variables to
//each trip, so if you got a better idea, use it!

//shared defines
#define tripAtVal 1000

//per trip defines
#define analog1 A0
#define digital1 12
//per trip variables
int aval1=0;
bool last1=false;
int prior1=tripAtVal;
int grace1 =tripAtVal;
//end trip declares

////per trip defines
//#define analog2 A1
//#define digital2 11
////per trip variables
//int aval2=0;
//bool last2=false;
//int prior2=tripAtVal;
//int grace2 =tripAtVal;
////end trip declares

void setup() {
  Serial.begin(115200);

  //setup for the trip
  pinMode(analog1, INPUT);  
  pinMode(digital1,OUTPUT);
  digitalWrite(digital1, HIGH);
  last1=(analogRead(analog1)<tripAtVal);
  //end trip setup

//  //setup for the trip
//  pinMode(analog2, INPUT);  
//  pinMode(digital2,OUTPUT);
//  digitalWrite(digital2, HIGH);
//  last2=(analogRead(analog2)<tripAtVal);
//  //end trip setup
}

void loop() {

  //loop for the trip
  aval1 = analogRead(analog1);
  if (aval1<tripAtVal) {
    if ((aval1<=prior1)||(aval1<=grace1)) {
      grace1=aval1;
    } else if (aval1>grace1) {
      if (!last1) {
        Serial.print('1');          
        last1=true;      
      }
    }
  } else if (aval1>tripAtVal) {
    if ((aval1>=prior1)||(aval1>=grace1)) {
      grace1=aval1;
    } else if (aval1<grace1) {
      if (last1) {
        Serial.print('2');
        last1=false;  
      }
    }
  }
  prior1=aval1;
  //end trip loop

//  //loop for the trip
//  aval2 = analogRead(analog2);
//  if (aval2<tripAtVal) {
//    if ((aval2<=prior2)||(aval2<=grace2)) {
//      grace2=aval2;
//    } else if (aval2>grace2) {
//      if (!last2) {
//        Serial.print('3');          
//        last2=true;      
//      }
//    }
//  } else if (aval2>tripAtVal) {
//    if ((aval2>=prior2)||(aval2>=grace2)) {
//      grace2=aval2;
//    } else if (aval2<grace2) {
//      if (last2) {
//        Serial.print('4');
//        last2=false;  
//      }
//    }
//  }
//  prior2=aval2;
//  //end trip loop
  
}
