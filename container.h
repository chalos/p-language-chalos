typedef union value {
	int value;
	double dvalue;
	float fvalue;
	char* text;
}value;

typedef struct container {
	value V;
	int type;
} container;
