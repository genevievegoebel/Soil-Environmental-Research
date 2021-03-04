/*
########################
#        OVERVIEW      #
########################
 Example D: Checks all addresses for active sensors, and logs data for each sensor every minute.
 Stores to an SD card, works with SDI-12 Sensors, works with RTC and interrupts.

 To address a sensor, please see Example B: b_address_change.ino

#########################
#      RESOURCES        #
#########################
Modified by Owen Krol in 2018 & 2019. Few modifications from the code adapted by Lutz and Gurnee

 Written by Kevin M. Smith in 2013.
 Modified by David Lutz and Meredith Gurnee in 2017 and 2018
 For help, please contact: meredith.a.gurnee.th@dartmouth.edu or david.a.lutz@dartmouth.edu
 When using this code, please contact David Lutz to ensure proper acknowledgement is given to authors: david.a.lutz@dartmouth.edu 
 
 The SDI-12 specification is available at: http://www.sdi-12.org/
 The library is available at: https://github.com/EnviroDIY/Arduino-SDI-12
*/

/*
#########################
#      LIBRARIES        #
#########################
*/

//including necessary libraries
#include <SDI12_PCINT3.h>      //include MayFly SDI12 library adjusted to fix conflict with PC_INT regarding RTC interrupt. Only controls PcInt 3.
#include <sleep.h>       //include arduino library for sleep modes and interupts ####### Needed to delete "/" in lib name
#include <wdt.h>      //include adruino library to reset arduino. Helps save battery during sleep mode. ####### Needed to delete "/" in lib name
#include <SPI.h>      //include library allows you to connect to serial interface
#include <SD.h>       //include library to write to SD card
#include <RTCTimer.h>       //include library for RTC timer
#include <Sodaq_DS3231.h>       //include library for the RTC
#include <Sodaq_PcInt_PCINT0.h>      //include library to connect RTC to interupts 
//#include <Wire.h>

//defining necessary pins
#define RTC_PIN A7      //on the Mayfly RTC is in analog pin 7 
#define RTC_INT_PERIOD EveryMinute      //dictate what time scale on  which the interrupt occurs 
#define SD_SS_PIN 12      //Digital pin 12 is the MicroSD slave select pin on the Mayfly
#define POWERPIN 22       // change to the proper pin (this is 22 on the Mayfly)
#define DATAPIN 7         // change to the proper pin (probably either digital pin 6 or 7 on the Mayfly)
#define FILE_NAME "SWaN2.txt"     //name the data log file

//defining data labels
#define LOGGERNAME "SWaN2" //name logger for data purposes
#define DATA_HEADER "Date/Time,SensorID,VWC,Temp,BEC" //adjust as needed for particular sensors

//setting up RTC
RTCTimer timer;       //define the RTC as a timer
int currentminute;      //define the minute as an interger
long currentepochtime = 0;      //define epoch time as a long entry
String dataRec = "";      //define dataRec as a string output

SDI12 mySDI12(DATAPIN);     //define the Datapin as an SDI12 input


/*
######################### 
#      SETUP        #
#########################
*/

void setup(){
  Serial.begin(57600);      //initiate serial connection
  rtc.begin();      //start the RTC
  
  delay(100);     //delay 0.1 seconds
  setupTimer();        //Setup timer event
  setupSleep();        //Setup sleep mode
 
 // Serial.println("Power On"); //DEBUG 
 showTime(getNow()); //print current time to serial


  //This is what was preventing the file from being created on SD
 // while(!Serial); //allows serial compatability with Leonardo/Arduino boards //
 
  
  // Power the sensors;
  #if POWERPIN > 0
    pinMode(POWERPIN, OUTPUT);
    digitalWrite(POWERPIN, HIGH);
    pinMode(8, OUTPUT);         // declare the Green ledPin as an OUTPUT
    digitalWrite(8, HIGH);      // turn the Green ledPin on
    delay(200);     //allow for transmission time
  #endif

  mySDI12.begin(); //initialize data collection from SDI12 sensor
  delay(500); // allow things to settle

  //Serial.println("Scanning all addresses, please wait..."); //DEBUG

/*
  Keep track of active addresses each bit represents an address: 1 is active (taken), 0 is inactive (available) 
  setTaken('A') will set the proper bit for sensor 'A'
  
  Quickly Scan the Address Space
*/

  for(byte i = '0'; i <= '9'; i++) if(checkActive(i)) setTaken(i);   // scan address space 0-9

  for(byte i = 'a'; i <= 'z'; i++) if(checkActive(i)) setTaken(i);   // scan address space a-z

  for(byte i = 'A'; i <= 'Z'; i++) if(checkActive(i)) setTaken(i);   // scan address space A-Z
 
    //  See if there are any active sensors and print to serial. 
 
  boolean found = false;
  for(byte i = 0; i < 62; i++){
    if(isTaken(i)){
      found = true;
      Serial.print("Found address:  ");
      Serial.println(i);
      break;
    }
  }

  if(!found) {
    Serial.println("No sensors found, please check connections and restart the Arduino.");
    while(true);
  } // stop here

 //intitialize file for SD card
 setupLogFile();
 
  //setup serial for data viewing
  Serial.println();
  Serial.println(DATA_HEADER);
  Serial.println("-------------------------------------------------------------------------------");

}

/*
######################### 
#      LOOP             #
#########################
*/

void loop()
{
 
timer.update();     //starting timer to time=0
  
 if(currentminute % 60 == 0)   // change to wake up logger every 'x' minutes
     {   
   //Serial.println("Multiple of 2!   Initiating sensor reading and logging data to SDcard....");       //DEBUG
   //Serial.println(currentminute);    //
   
   // scan address space 0-9
  for(char i = '0'; i <= '9'; i++) if(isTaken(i)){

   // String dateTime = getDateTime();
    //Serial.print(dateTime);
    //Serial.print(",");
    printInfo(i);
    Serial.print("\t\t,");
    takeMeasurement(i);
    Serial.println();
  }

  // scan address space a-z
  for(char i = 'a'; i <= 'z'; i++) if(isTaken(i)){
    
    //String dateTime = getDateTime();
    //Serial.print(dateTime);
    //Serial.print(",");
    printInfo(i);
    Serial.print("\t\t,");
    takeMeasurement(i);
    Serial.println();
  }

  // scan address space A-Z
  for(char i = 'A'; i <= 'Z'; i++) if(isTaken(i)){
    
   // String dateTime = getDateTime();
    //Serial.print(dateTime);
    //Serial.print(",");
    printInfo(i);
    Serial.print("\t\t,");
    takeMeasurement(i);
    Serial.println();
  };

  delay(5000); // wait ten seconds between measurement attempts.   
   
     }
  
  delay(1000);
  //Sleep
  systemSleep();
}

/*
##############################
#      SDI 12 FUNCTIONS        #
#############################
*/

//sets up register for SDI 12
byte addressRegister[8] = {
  0B00000000,
  0B00000000,
  0B00000000,
  0B00000000,
  0B00000000,
  0B00000000,
  0B00000000,
  0B00000000
};

// converts allowable address characters '0'-'9', 'a'-'z', 'A'-'Z',
// to a decimal number between 0 and 61 (inclusive) to cover the 62 possible addresses
byte charToDec(char i){
  if((i >= '0') && (i <= '9')) return i - '0';
  if((i >= 'a') && (i <= 'z')) return i - 'a' + 10;
  if((i >= 'A') && (i <= 'Z')) return i - 'A' + 37;
  else return i;
}

// THIS METHOD IS UNUSED IN THIS EXAMPLE, BUT IT MAY BE HELPFUL.
// maps a decimal number between 0 and 61 (inclusive) to
// allowable address characters '0'-'9', 'a'-'z', 'A'-'Z',
char decToChar(byte i){
  if((i >= 0) && (i <= 9)) return i + '0';
  if((i >= 10) && (i <= 36)) return i + 'a' - 10;
  if((i >= 37) && (i <= 62)) return i + 'A' - 37;
  else return i;
}

// gets identification information from a sensor, and prints it to the serial port
// expects a character between '0'-'9', 'a'-'z', or 'A'-'Z'.
void printInfo(char i){
  int j;
  String command = "";
  command += (char) i;
  command += "I!";
  for(j = 0; j < 1; j++){
    mySDI12.sendCommand(command);
    delay(30);
    if(mySDI12.available()>1) break;
    if(mySDI12.available()) mySDI12.read();
  }

  while(mySDI12.available()){
    char c = mySDI12.read();
    if((c!='\n') && (c!='\r')) 
    Serial.write(c);
    delay(5);
  }
}

/*
#########################
#      RTC FUNCTIONS     #
#########################
*/
void showTime(uint32_t ts)
{
  //Retrieve and display the current date/time
  String dateTime = getDateTime();
  //Serial.println(dateTime);
}


void setupTimer()
{
  
    //Schedule the wakeup every minute
  timer.every(1, showTime);
  
  //Instruct the RTCTimer how to get the current time reading
  timer.setNowCallback(getNow);
 
}
 
void wakeISR()
{
  //Leave this blank
}
 
void setupSleep()
{
  pinMode(RTC_PIN, INPUT_PULLUP);
  PcInt::attachInterrupt(RTC_PIN, wakeISR);
 
  //Setup the RTC in interrupt mode
  rtc.enableInterrupts(RTC_INT_PERIOD);
  
  //Set the sleep mode
  set_sleep_mode(SLEEP_MODE_PWR_DOWN);
}
 
void systemSleep()
{
  //This method handles any sensor specific sleep setup
  sensorsSleep();
  
  //Wait until the serial ports have finished transmitting
  Serial.flush();
  Serial1.flush();
  
  //The next timed interrupt will not be sent until this is cleared
  rtc.clearINTStatus();
    
  //Disable ADC
  ADCSRA &= ~_BV(ADEN);
  
  //Sleep time
  noInterrupts();
  sleep_enable();
  interrupts();
  sleep_cpu();
  sleep_disable();
 
  //Enbale ADC
  ADCSRA |= _BV(ADEN);
  
  //This method handles any sensor specific wake setup
 // sensorsWake();
}
 
void sensorsSleep()
{
  //Add any code which your sensors require before sleep
}
 
//void sensorsWake()
//{
//  //Add any code which your sensors require after waking
//}
 
String getDateTime()
{
  String dateTimeStr;
  
  //Create a DateTime object from the current time
  DateTime dt(rtc.makeDateTime(rtc.now().getEpoch()));
 
  currentepochtime = (dt.get());    //Unix time in seconds 
 
  currentminute = (dt.minute());
  //Convert it to a String
  dt.addToString(dateTimeStr); 
  return dateTimeStr;  
}
 
uint32_t getNow()
{
  currentepochtime = rtc.now().getEpoch();
  return currentepochtime;
}

/*
##############################
#      SD CARD FUNCTIONS      #
#############################
*/


void setupLogFile()       //setting up the file
{
  //Initialise the SD card
  if (!SD.begin(SD_SS_PIN))
  {
    Serial.println("Error: SD card failed to initialise or is missing.");
    //Hang
 // while (true); 
  }
  
  //Check if the file already exists
  bool oldFile = SD.exists(FILE_NAME);  
  
  //Open the file in write mode
  File logFile = SD.open(FILE_NAME, FILE_WRITE);
  
  //Add header information if the file did not already exist
  if (!oldFile)
  {
    logFile.println(LOGGERNAME);
    logFile.println(DATA_HEADER);
  }
  
  //Close the file to save it
  logFile.close();  
}

void logData(String buffer)     //print buffer to txt
{
  //Re-open the file
  File logFile = SD.open(FILE_NAME, FILE_WRITE);
  
  //Write the txt data
  logFile.println(buffer); 
  
  //Close the file to save it
  logFile.close();  
}

void printBufferToScreen(char i)      //creating the buffer
{
  String buffer = "";     //define buffer
  String dateTime = getDateTime();
  buffer += (dateTime);
  buffer += " \t\t ";
  buffer += (char) i; //puts address of sensor on buffer
  buffer += " \t\t ";
  mySDI12.read(); // consume address
  mySDI12.read(); // consume address
  while(mySDI12.available()){
    char c = mySDI12.read();
    if(c == '+' || c == '-'){
      buffer += " \t\t ";
      if(c == '-') buffer += '-';
    }
    else if ((c != '\n') && (c != '\r')) {
      buffer += c;
    }
    
    delay(50);
  }
 
 logData(buffer);
 Serial.print(buffer);
}

void takeMeasurement(char i){
 
  String command = "";
  command += i;
  command += "M!"; // SDI-12 measurement command format  [address]['M'][!]
  mySDI12.sendCommand(command);
  // wait for acknowlegement with format [address][ttt (3 char, seconds)][number of measurments available, 0-9]
  String sdiResponse = "";
  delay(30);
  while (mySDI12.available())  // build response string
  {
    char c = mySDI12.read();
    if ((c != '\n') && (c != '\r'))
    {
      sdiResponse += c;
      delay(5);
    }
  }
  mySDI12.clearBuffer();

  // find out how long we have to wait (in seconds).
  unsigned int wait = 0;
  wait = sdiResponse.substring(1,4).toInt();

  // Set up the number of results to expect
  // int numMeasurements =  sdiResponse.substring(4,5).toInt();

  unsigned long timerStart = millis();
  while((millis() - timerStart) < (1000 * wait)){
    if(mySDI12.available())  // sensor can interrupt us to let us know it is done early
    {
      mySDI12.clearBuffer();
      break;
    }
  }
  // Wait for anything else and clear it out
  delay(30);
  mySDI12.clearBuffer();

  // in this example we will only take the 'DO' measurement
  command = "";
  command += i;
  command += "D0!"; // SDI-12 command to get data [address][D][dataOption][!]
  mySDI12.sendCommand(command);
  while(!mySDI12.available()>1); // wait for acknowlegement
  delay(300); // let the data transfer
  printBufferToScreen(i);
  mySDI12.clearBuffer();
}

// this checks for activity at a particular address
// expects a char, '0'-'9', 'a'-'z', or 'A'-'Z'
boolean checkActive(char i){

  String myCommand = "";
  myCommand = "";
  myCommand += (char) i;                 // sends basic 'acknowledge' command [address][!]
  myCommand += "!";

  for(int j = 0; j < 3; j++){            // goes through three rapid contact attempts
    mySDI12.sendCommand(myCommand);
    if(mySDI12.available()>1) break;
    delay(30);
  }
  if(mySDI12.available()>2){       // if it hears anything it assumes the address is occupied
    mySDI12.clearBuffer();
    return true;
  }
  else {   // otherwise it is vacant.
    mySDI12.clearBuffer();
  }
  return false;
}

// this quickly checks if the address has already been taken by an active sensor
boolean isTaken(byte i){
  i = charToDec(i); // e.g. convert '0' to 0, 'a' to 10, 'Z' to 61.
  byte j = i / 8;   // byte #
  byte k = i % 8;   // bit #
  return addressRegister[j] & (1<<k); // return bit status
}

// this sets the bit in the proper location within the addressRegister
// to record that the sensor is active and the address is taken.
boolean setTaken(byte i){
  boolean initStatus = isTaken(i);
  i = charToDec(i); // e.g. convert '0' to 0, 'a' to 10, 'Z' to 61.
  byte j = i / 8;   // byte #
  byte k = i % 8;   // bit #
  addressRegister[j] |= (1 << k);
  return !initStatus; // return false if already taken
}

// THIS METHOD IS UNUSED IN THIS EXAMPLE, BUT IT MAY BE HELPFUL.
// this unsets the bit in the proper location within the addressRegister
// to record that the sensor is active and the address is taken.
boolean setVacant(byte i){
  boolean initStatus = isTaken(i);
  i = charToDec(i); // e.g. convert '0' to 0, 'a' to 10, 'Z' to 61.
  byte j = i / 8;   // byte #
  byte k = i % 8;   // bit #
  addressRegister[j] &= ~(1 << k);
  return initStatus; // return false if already vacant
}
