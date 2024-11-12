       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYSCMDSP.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01 WS-INP-COMMANDS.
          05 WS-COMMAND-LIST     OCCURS 40 TIMES.
             10 WS-COMMAND       PIC X(2048).
       01  FILE-STATUS.
           05  FILE-STATUS-X1          PIC X(1).
           05  FILE-STATUS-X2.
               10  FILE-STATUS-N2      PIC 9(1).
           05  FILE-STATUS-X2-BINARY
                   REDEFINES FILE-STATUS-X2
                                       PIC 99 COMP-X.
       01  FILE-STATUS-9 REDEFINES FILE-STATUS.
           05  FILE-STATUS-9-ERR       PIC 9(4) COMP.
	   01 CONTROL-REC-UPPER            PIC X(80).
	   01 CONTROL-STATUS               PIC X(02).
       01 WS-APP-BASE            PIC X(200).
	   01 CONTROL-EOF                  PIC X(1) VALUE 'N'. 
	   01 WS-DD-CNT                    PIC 9(2) VALUE 0.
	   01 WS-DD-NAME                   PIC X(8) VALUE SPACES.
	   01 SCRIPT-NAME                  PIC X(100) VALUE SPACES.
	   01 FS-LINE.
          05 FS-LINE-TEXT              PIC X(200) VALUE SPACES.
		  05 FS-LF                     PIC X(1) VALUE X'0A'.
	   01 DIR-NAME                     PIC X(50).
	   01 WS-REMAINING                 PIC X(80).
	   01 WS-CTL-NAME                  PIC X(08) VALUE SPACES.
	   01 WS-CTL-NAME1                 PIC X(100).
	   01 WS-CTL-NAME2                 PIC X(20) VALUE SPACES.
	   01 WS-CONTROL-FILE              PIC X(100) VALUE SPACES.
	   01 WS-FILE-NAME                 PIC X(100) VALUE SPACES.
       01 WS-EOF                 PIC X(1) VALUE 'N'. 
       01 WS-VARIABLES.
          05 WS-RETURN-CODE           PIC S9(8) COMP-5.
		  05 SAVED-RETURN-CODE        PIC S9(8) COMP-5 VALUE ZEROS.
		  05 WS-EMPTY-COUNT           PIC 9(3) VALUE ZEROS.
		  05 WS-TAB-REP               PIC X(4) VALUE SPACES.
          05 INP-COMMAND              PIC X(5000) VALUE SPACES.
          05 NULL-TERMINATED-COMMAND.
             10 COMMAND               PIC X(2048).
             10 FILLER                PIC X VALUE X"00".
          05 INDX                     PIC 9(02) VALUE 1.
          05 CMD-INDX                 PIC 9(02) VALUE 1.
          05 LINE-LEN                 PIC 9(02) VALUE 0.
		  05 REC-LEN                  PIC 9(03) VALUE 1.
          05 CMD-LEN                  PIC 9(04) VALUE 0.
          05 WS-LEN                   PIC 9(02) VALUE 1.	
          05 OUT-LEN                  PIC 9(04) VALUE 1.
          05 WS-TOT-LEN               PIC 9(04) VALUE 1.
          05 INPUT-LEN                PIC 9(04) VALUE 0.	
		  05 LINE-REC                 PIC X(121) VALUE SPACES.
          05 DONE-FLAG                PIC X VALUE "N".
             88                DONE   VALUE "Y".
		  05 RD-PARAMETERS.
			10 RD-HANDLE         PIC X(4) COMP-X.
			10 RD-OFFSET         PIC X(8) COMP-X VALUE ZEROES.
			10 RD-COUNT          PIC X(4) COMP-X.
			10 RD-FLAGS          PIC X(1) COMP-X VALUE ZEROES.
			10 RD-BUFF           PIC X(1) VALUE SPACES.
			10 RD-ACCESS-MODE    PIC X(1) COMP-X VALUE 3.
			10 RD-DENY-MODE      PIC X(1) COMP-X VALUE 3.
			10 RD-DEVICE         PIC X(1) COMP-X VALUE ZEROES.
		  05 FILE-SIZE           PIC X(8) COMP-X VALUE ZEROES. 

       COPY MFJCTLBC REPLACING ==(TAG)== BY ==CTLB==. 
	   
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
      ********************************************************************		    
      *  THIS PART OF CODE WILL GET THE SYSOUT FILE NAME AND
      *  CREATE A TEMPORARY FILE FOR SPOOL
      ********************************************************************
		    MOVE "SYSOUT" TO CTLB-DDNAME.
			PERFORM P2000-GET-FILE-FOR-DD THRU P2009-EXIT.
		  
      *      DISPLAY "FILE-NAME=" CTLB-FILENAME.
			
			STRING CTLB-FILENAME DELIMITED BY SPACES
			    ".TXT" DELIMITED BY SIZE
				INTO CTLB-FILENAME.
			
			MOVE CTLB-FILENAME TO WS-FILE-NAME.

      **************************************************************
      * PROCESS EACH COMMAND FROM THE LIST WS-COMMAND
      **************************************************************
            PERFORM UNTIL CMD-INDX >= INDX
                
      *        DISPLAY "WS-COMMAND: " 
      *                   FUNCTION TRIM(WS-COMMAND(CMD-INDX))
               DISPLAY "                                         "
               MOVE SPACES         TO COMMAND

			   STRING FUNCTION TRIM(WS-COMMAND(CMD-INDX)) 
			          DELIMITED BY SIZE
			    " >" DELIMITED BY SIZE
				CTLB-FILENAME DELIMITED BY SIZE
				INTO COMMAND
               
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
				  MOVE 0 TO WS-RETURN-CODE
               ELSE 
                  DISPLAY "COMMAND ERROR: " WS-RETURN-CODE
				  MOVE 8 TO WS-RETURN-CODE
               END-IF
			   
			   IF WS-RETURN-CODE > SAVED-RETURN-CODE THEN
			      MOVE WS-RETURN-CODE TO SAVED-RETURN-CODE
			   END-IF
			   
			   PERFORM P1000-SYSOUT-PRINT THRU P1009-EXIT
			   
               ADD 1 TO CMD-INDX
            END-PERFORM.

      *     THIS WILL DELETE THE TEMPORARY LOG FILE
		    PERFORM P11000-DELETE-MF-FILE THRU P11009-EXIT.

			IF SAVED-RETURN-CODE > RETURN-CODE THEN
			   MOVE SAVED-RETURN-CODE TO RETURN-CODE.
			
            GOBACK.

	   P1000-SYSOUT-PRINT.
      ********************************************************
      ** OPEN THE LOG FILE TO DISPLAY IN SPOOL
      ********************************************************
			MOVE 1 TO RD-ACCESS-MODE.
			MOVE 'N' TO WS-EOF.
			PERFORM P8000-OPEN-FILE THRU P8009-EXIT.
             
           IF RETURN-CODE NOT = 0 THEN
             DISPLAY '!!OPEN RETURN CODE ->' RETURN-CODE
             DISPLAY '!!OPEN FILE STATUS ->' FILE-STATUS
             MOVE 'Y' TO WS-EOF
           END-IF.
           
           MOVE 1 TO RD-COUNT.
           MOVE 0 TO RD-OFFSET.
           MOVE ' ' TO LINE-REC.
           
           PERFORM UNTIL WS-EOF NOT = 'N'
		   
              PERFORM P10000-READ-MF-FILE THRU P10009-EXIT
			  
      *       DISPLAY "RD-BUFF=" RD-BUFF
              IF RETURN-CODE NOT = 0 THEN
                DISPLAY '!!READ RETURN CODE ->' RETURN-CODE
                DISPLAY '!!READ FILE STATUS ->' FILE-STATUS
                MOVE 'Y' TO WS-EOF
              ELSE                
                IF RD-BUFF = X'00' THEN
                  MOVE 'Y' TO WS-EOF
                ELSE
                    IF RD-BUFF <> X'0A' THEN
                        IF RD-BUFF <> X'09' THEN 
                            STRING LINE-REC(1:REC-LEN) DELIMITED BY SIZE    
                                RD-BUFF DELIMITED BY SIZE
                                INTO LINE-REC
                    		END-STRING
                    		COMPUTE  REC-LEN = REC-LEN + 1
                    	ELSE
                    		STRING LINE-REC(1:REC-LEN) DELIMITED BY SIZE    
                                WS-TAB-REP DELIMITED BY SIZE
                                INTO LINE-REC
                    		END-STRING
                    		COMPUTE  REC-LEN = REC-LEN + 4
                    	END-IF
                    	
                    ELSE
                        IF FUNCTION 
                            LENGTH(FUNCTION TRIM(LINE-REC)) > 0
                            MOVE ZEROS TO WS-EMPTY-COUNT
                        ELSE
                            ADD 1 TO WS-EMPTY-COUNT
                        END-IF
                        
                        DISPLAY LINE-REC
                        MOVE ' ' TO LINE-REC
                        
                        IF WS-EMPTY-COUNT > 10 THEN
      *                     DISPLAY "EMPTY LINE COUNT:" 
                           MOVE 'Y' TO WS-EOF
                        END-IF
                        MOVE ' ' TO LINE-REC
                        MOVE 1 TO REC-LEN
                    END-IF
                    
                  COMPUTE RD-OFFSET = RD-OFFSET + RD-COUNT
                      
                  IF RD-OFFSET > 120000 THEN
                    MOVE 'Y' TO WS-EOF
                  END-IF
                END-IF
              END-IF
              
           END-PERFORM.	
		   
		   PERFORM P4000-CLOSE-MF-FILES THRU P4009-EXIT.
		   
	   P1009-EXIT.
	       EXIT.
		   
      **************************************************************
      ** GET THE PHYSICAL FILE NAME FOR DD NAME IN JCL.
      **************************************************************		   
       P2000-GET-FILE-FOR-DD.  
	   
		   SET CTLB-FUNC-GET-DD TO TRUE
           SET CTLB-DD-VERS-CUR TO TRUE
      *    MOVE "SYSOUT" TO CTLB-DDNAME.
		  
           CALL 'MFJCTLBP' USING CTLB-FUNCTION
                      CTLB-RETCODE
                      CTLB-DD-AREA. 
					  
		   IF RETURN-CODE > 0 THEN
		      DISPLAY "CAN'T GET THE FILE NAME FOR DD: " CTLB-DDNAME
			  PERFORM P9999-GOBACK
		   END-IF.
		   
		   IF CTLB-FILENAME = " " THEN
		      DISPLAY "FILE NAME FOR DD NAME NOT FOUND:" CTLB-DDNAME
		   END-IF.
		   
	   P2009-EXIT.
		   EXIT.	
		   
      **************************************************************
      ** This para will open the FILE 
      **************************************************************
	    P8000-OPEN-FILE.

            CAll "CBL_OPEN_FILE" 
             USING CTLB-FILENAME, RD-ACCESS-MODE, 
              RD-DENY-MODE, RD-DEVICE, RD-HANDLE
             RETURNING FILE-STATUS. 
			
			IF RETURN-CODE > 0 THEN
			   DISPLAY "FILE OPEN FAILED=" CTLB-FILENAME
			   DISPLAY "UNABLE TO PROCEED FURTHER.."
			   PERFORM P9999-GOBACK
			END-IF.
			
		P8009-EXIT.
		    EXIT.
      **************************************************************
      ** This para will READ MF the FILE 
      **************************************************************
	    P10000-READ-MF-FILE.

            CALL "CBL_READ_FILE" 
                   USING RD-HANDLE, RD-OFFSET, 
                   RD-COUNT, RD-FLAGS, RD-BUFF
                   RETURNING FILE-STATUS
			
			IF RETURN-CODE > 0 THEN
			   DISPLAY "FILE READ FAILED=" CTLB-FILENAME
               DISPLAY '!!READ RETURN CODE ->' RETURN-CODE
               DISPLAY '!!READ FILE STATUS ->' FILE-STATUS
      *        PERFORM P9999-GOBACK
			END-IF.
			
		P10009-EXIT.
		    EXIT.
			
      **************************************************************
      ** CLOSE THE FILE OPENED THROUG CBL_OPEN_FILE METHOD.
      **************************************************************		   
       P4000-CLOSE-MF-FILES.  
	   
           CAll "CBL_CLOSE_FILE" 
             USING RD-HANDLE
             RETURNING FILE-STATUS.
			
		   IF RETURN-CODE > 0 THEN
		      DISPLAY "FILE CLOSING FAILED: " WS-FILE-NAME
			  PERFORM P9999-GOBACK
		   END-IF.
			 
	   P4009-EXIT.
		   EXIT.
		   
      **************************************************************
      ** This para will DELETE MF the FILE 
      **************************************************************
	    P11000-DELETE-MF-FILE.
            DISPLAY "DELETING LOG FILE: " CTLB-FILENAME.
            CALL "CBL_DELETE_FILE" 
                 USING CTLB-FILENAME,
                 GIVING FILE-STATUS.
			
			IF RETURN-CODE > 0 THEN
			   DISPLAY "FILE DELETE FAILED=" CTLB-FILENAME
               DISPLAY '!!READ RETURN CODE ->' RETURN-CODE
               DISPLAY '!!READ FILE STATUS ->' FILE-STATUS
      *        PERFORM P9999-GOBACK
			END-IF.
			
		P11009-EXIT.
		    EXIT.	
			
       P9999-GOBACK.           
           GOBACK.
               			   
