
int potPin = 0;
//int latchPin = 5;
//int clockPin = 6;
//int dataPin = 4;
 
int oxredPot = 0;

//Reference electrode must be connected to analog ground
//Redox electrode connects to high input of diff channel
//Reference electrode connects to low input

void setup() {
  Serial.begin(9600);
  //pinMode(latchPin, OUTPUT);
  //pinMode(dataPin, OUTPUT);  
  //pinMode(clockPin, OUTPUT);
}

void loop() {
  //This line is used to read in analog values from A0
  int reading = analogRead(potPin);
  //int oxredPot = reading / 114;  //1023 / 9
 Serial.println(reading);
 delay(1000);
  //updateShiftRegister();
}

//This function sets the latchPin to low, calls shiftOut() before
//setting it back to high
//shiftOut([data], [clock], [start point data], [actual data])
//void updateShiftRegister()
//{
   //digitalWrite(latchPin, LOW);
   //Least significant bit (LSBFIRST)
   //shiftOut(dataPin, clockPin, LSBFIRST, oxredPot);
   //digitalWrite(latchPin, HIGH);
//}

//Use of variable 10 kOhm resistor (potentiometer aka. pot) 
//connected to the Arduino board allows use of knob to change the 
//value being printed in the serial monitor
