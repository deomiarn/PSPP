# Pi.cu wird für Tessla T4 uebersetzt
FILE=Pi
OPTIM=-O3
ARCH=-arch=sm_52
GENCODE=-gencode=arch=compute_52,code=sm_52 -gencode=arch=compute_60,code=sm_60 -gencode=arch=compute_61,code=sm_61 -gencode=arch=compute_70,code=sm_70 -gencode=arch=compute_75,code=sm_75 -gencode=arch=compute_80,code=sm_80 -gencode=arch=compute_80,code=compute_80 
all: build  
build: $(FILE).cu
	nvcc $(FILE).cu $(ARCH) $(GENCODE) $(OPTIM) -o $(FILE)
clean: 
	rm -f $(FILE)
