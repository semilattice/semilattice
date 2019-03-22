       IDENTIFICATION DIVISION.
       PROGRAM-ID. rain.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      ******************************************************************
      * The log file stores all events in chronological order. The     *
      * magic event is also stored in the log file, at key 1.          *
      ******************************************************************
       SELECT OPTIONAL fd-log
           ASSIGN TO DYNAMIC ws-log-path OF ws-config
           ORGANIZATION IS RELATIVE
           ACCESS MODE IS RANDOM
           RELATIVE KEY IS ws-key OF ws-log.

       DATA DIVISION.

       FILE SECTION.

      ******************************************************************
      * See the corresponding file control entry for more information. *
      ******************************************************************
       FD fd-log.
       01 fs-current-event.
           02 fs-class                 PIC X(20).
           02 fs-value                 FLOAT-SHORT.

       WORKING-STORAGE SECTION.

      ******************************************************************
      * Variables read at start up, configuring the application.       *
      ******************************************************************
       01 ws-config.
           02 ws-log-path              PIC X(200).

      ******************************************************************
      * Variables for working with the log file.                       *
      ******************************************************************
       01 ws-log.
           02 ws-key                   PIC 9(10) BINARY.

      ******************************************************************
      * The magic event is used as a sanity check. It is the first     *
      * event in every log file. When a log file is opened, the first  *
      * event is compared against the magic event and it should be the *
      * same. If it is different, the log file may have been written   *
      * on a platform with an incompatible encoding of text or         *
      * floating-point numbers.                                        *
      ******************************************************************
       01 ws-magic-event.
           02 ws-class                 PIC X(20)   VALUE 'ABCDEFGTUVWXYZ
      -                                                  '012789'.
           02 ws-value                 FLOAT-SHORT VALUE 0.5.

      ******************************************************************
      * The current event of interest.                                 *
      ******************************************************************
       01 ws-current-event.
           02 ws-class                 PIC X(20).
           02 ws-value                 FLOAT-SHORT.

      ******************************************************************
      * The pictures in this record are suitable for debug-printing    *
      * events using the following statement:                          *
      *     DISPLAY ws-debug-event                                     *
      ******************************************************************
       01 ws-debug-event.
           02 ws-class                 PIC X(20).
           02 FILLER                   PIC X.
           02 ws-value                 PIC 9.99.

       PROCEDURE DIVISION.
       pa-main.
           PERFORM pa-read-config
           PERFORM pa-ensure-magic-event
           STOP RUN
           .

       pa-read-config.
           MOVE '/tmp/rain-log' TO ws-log-path OF ws-config
           .

      ******************************************************************
      * If the file is empty, create the magic event. Otherwise,       *
      * verify that the file starts with the magic event.              *
      ******************************************************************
       pa-ensure-magic-event.
      * TODO: Move file opening and closing to elsewhere.
      * TODO: Handle I/O errors.
           OPEN I-O fd-log


      ******************************************************************
      * Read the event at the position where the magic event is        *
      * supposed to be. If there is no event there, write the magic    *
      * event at this position.                                        *
      ******************************************************************
           MOVE 1 TO ws-key OF ws-log
           READ fd-log
               INVALID KEY
                   WRITE fs-current-event FROM ws-magic-event
           END-READ

      ******************************************************************
      * Verify that the event is equal to the magic event.             *
      ******************************************************************
           MOVE fs-current-event TO ws-current-event
           IF ws-current-event IS NOT EQUAL TO ws-magic-event THEN
      * TODO: Correctly handle this error and do not continue.
               DISPLAY 'OOPS: Corrupt log file!'
           END-IF


           CLOSE fd-log
           .
