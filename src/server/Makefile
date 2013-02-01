MESSAGES=mess
MESSAGES_ATD=$(MESSAGES).atd

.PHONY: all clean $(MESSAGES)

all: $(MESSAGES)

$(MESSAGES): $(MESSAGES_ATD)
	atdgen $(MESSAGES_ATD) -t
	atdgen $(MESSAGES_ATD) -j

clean:
	rm $(MESSAGES)_{t,j}.ml{,i}
