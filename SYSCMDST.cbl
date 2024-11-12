IDENTIFICATION DIVISION.
       PROGRAM-ID. SYSCMDST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-VARIABLES.
          05 WS-RETURN-CODE           PIC S9(8) COMP-5.
          05 INP-COMMAND              PIC X(5000) VALUE SPACES.
          05 NULL-TERMINATED-COMMAND.
             10 COMMAND               PIC X(2048).
             10 FILLER                PIC X VALUE X"00".
          05 INDX                     PIC 9(02) VALUE 1.
          05 CMD-INDX                 PIC 9(02) VALUE 1.
          05 LINE-LEN                 PIC 9(02) VALUE 0.
          05 CMD-LEN                  PIC 9(04) VALUE 0.
          05 WS-LEN                   PIC 9(02) VALUE 1.	
          05 OUT-LEN                  PIC 9(04) VALUE 1.
          05 WS-TOT-LEN               PIC 9(04) VALUE 1.
          05 INPUT-LEN                PIC 9(04) VALUE 0.	   
          05 DONE-FLAG                PIC X VALUE "N".
             88                DONE   VALUE "Y".

       01 WS-INP-COMMANDS.
          05 WS-COMMAND-LIST     OCCURS 40 TIMES.
             10 WS-COMMAND       PIC X(2048).
			 
       LINKAGE SECTION.
       01  PARM-BUFFER.
           05  PARM-LENGTH         PIC S9(4)   COMP.
           05  PARM-DATA           PIC X(08).
       PROCEDURE DIVISION  USING PARM-BUFFER.
            DISPLAY "GETTING INPUT FROM SYSIN....".

            DISPLAY "ENTER COMMAND TO BE EXECUTED BY SHELL PGM".
            DISPLAY "EACH COMMAND CAN NOT BE MORE THAN 2048 CHAR"
            DISPLAY " (ENTER 'DONE' TO END COMMAND)".
           
            ACCEPT INP-COMMAND FROM SYSIN.
            COMPUTE INPUT-LEN =  
                FUNCTION LENGTH(FUNCTION TRIM(INP-COMMAND TRAILING)).
           
            DISPLAY "COMMAND-LEN: " INPUT-LEN.
            DISPLAY "                                          "
      *     DISPLAY "COMMAND: " FUNCTION TRIM(INP-COMMAND TRAILING).
      *
            MOVE 1 TO INDX.
           
            PERFORM UNTIL WS-TOT-LEN > INPUT-LEN
      *        DISPLAY 'INP:' INP-COMMAND(WS-TOT-LEN:80)
               
               COMPUTE LINE-LEN = FUNCTION LENGTH(
                FUNCTION TRIM(INP-COMMAND(WS-TOT-LEN:80) TRAILING))
               
               IF FUNCTION TRIM(INP-COMMAND(WS-TOT-LEN:80)) = "DONE"    
			      MOVE  1               TO OUT-LEN
				  ADD   1               TO INDX                  
               ELSE
       	          MOVE FUNCTION 
                            TRIM(INP-COMMAND(WS-TOT-LEN:80) TRAILING)
			               TO WS-COMMAND(INDX)(OUT-LEN:LINE-LEN)
                  ADD LINE-LEN TO OUT-LEN
                  ADD 1        TO OUT-LEN
                  MOVE SPACE TO WS-COMMAND(INDX)(OUT-LEN :1)
			   END-IF
               
               ADD 80 TO WS-TOT-LEN
               
            END-PERFORM.			   

            MOVE 1 TO CMD-INDX.
      **************************************************************
      * PROCESS EACH COMMAND FROM THE LIST WS-COMMAND
      **************************************************************
            PERFORM UNTIL CMD-INDX >= INDX
                
      *        DISPLAY "WS-COMMAND: " 
      *                   FUNCTION TRIM(WS-COMMAND(CMD-INDX))
               DISPLAY "                                         "
               MOVE SPACES         TO COMMAND
			   MOVE FUNCTION TRIM(WS-COMMAND(CMD-INDX)) TO COMMAND
               COMPUTE  CMD-LEN = 
                          FUNCTION LENGTH(FUNCTION TRIM(COMMAND))
               
      *        DISPLAY 'CMD-LEN:' CMD-LEN
               MOVE X'00' TO COMMAND(CMD-LEN + 1: 1)
               
               DISPLAY  "COMMAND EXECUTING........."
               DISPLAY COMMAND(1: CMD-LEN + 2)
               
               CALL "SYSTEM"    USING     COMMAND
                                RETURNING WS-RETURN-CODE
           
               IF WS-RETURN-CODE = 0 OR 1024
                  DISPLAY "RETURN CODE FROM SHELL IS: " 
                                           WS-RETURN-CODE
               ELSE 
                  DISPLAY "COMMAND ERROR: " COMMAND(1: CMD-LEN + 2)     
                                " - " WS-RETURN-CODE
                  MOVE 8 TO RETURN-CODE
               END-IF
               ADD 1 TO CMD-INDX
            END-PERFORM.
            GOBACK.
