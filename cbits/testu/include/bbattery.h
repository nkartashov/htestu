 
/* bbattery.h for ANSI C */
#ifndef BBATTERY_H
#define BBATTERY_H
 
#include <stdlib.h>
#include "unif01.h"


extern int bbattery_NTests;

  typedef struct BatteryResults {
    double* pValues;
    int testNumber;
  } BatteryResult;

  BatteryResult* wrap(double* pValues, int testNumber) {
    BatteryResult* result = (BatteryResult*) malloc(sizeof(BatteryResult));
    result->pValues = pValues;
    result->testNumber = testNumber;
    return result;
  }



extern double bbattery_pVal[];



extern char *bbattery_TestNames[];


BatteryResult* bbattery_SmallCrush (unif01_Gen *gen);

void bbattery_SmallCrushFile (char *filename);



void bbattery_RepeatSmallCrush (unif01_Gen *gen, int rep[]);



BatteryResult* bbattery_Crush (unif01_Gen *gen);



void bbattery_RepeatCrush (unif01_Gen *gen, int rep[]);



BatteryResult* bbattery_BigCrush (unif01_Gen *gen);



void bbattery_RepeatBigCrush (unif01_Gen *gen, int rep[]);



void bbattery_Rabbit (unif01_Gen *gen, double nb);


void bbattery_RabbitFile (char *filename, double nb);


void bbattery_RepeatRabbit (unif01_Gen *gen, double nb, int rep[]);



void bbattery_Alphabit (unif01_Gen *gen, double nb, int r, int s);



void bbattery_AlphabitFile (char *filename, double nb);


void bbattery_RepeatAlphabit (unif01_Gen *gen, double nb, int r, int s,
                              int rep[]);



void bbattery_BlockAlphabit (unif01_Gen *gen, double nb, int r, int s);
void bbattery_BlockAlphabitFile (char *filename, double nb);



void bbattery_RepeatBlockAlphabit (unif01_Gen *gen, double nb, int r, int s,
                                   int rep[], int w);



BatteryResult* bbattery_pseudoDIEHARD (unif01_Gen *gen);



void bbattery_FIPS_140_2 (unif01_Gen *gen);
void bbattery_FIPS_140_2File (char *filename);

 
#endif
 

