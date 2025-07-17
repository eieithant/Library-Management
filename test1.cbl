      ******************************************************************
      * Author:HL
      * Date:7/7/2025
      * Purpose:container
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. Main-Container.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT BOOK-FILE ASSIGN TO "Books.csv"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS BOOK-STATUS.

            SELECT MemberFile ASSIGN TO 'Members.csv'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL.
      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD BOOK-FILE.
       01 BOOK-RECORD PIC X(200).
       01 b_id pic x(10).

       FD  MemberFile.
       01  MemberRecord      PIC X(200).
      *-----------------------
       WORKING-STORAGE SECTION.
      *-----------------------
       01  WS-CHOICE PIC 9.
       01 WS-SUBCHOICE PIC 9.  *>List all books- options
       01 WS-End PIC x value "Y".
       01  WS-CSV-LINE       PIC X(200).
       01  WS-MAX-MEMBER-ID  PIC 9(5) VALUE 0.
       01  WS-TEMP-MEMBER-ID PIC 9(5).

              01  member_record.
           05  member_id         PIC 9(5) VALUE 0.
           05  member_name       PIC X(30).
           05  member_gender     PIC X.
           05  member_email      PIC X(35).
           05  member_address    PIC X(50).
           *> 05  member_flag       PIC X.
           *> -Y/N-

       01  member_id_disp PIC 9(5).

       01  cm_choice PIC 9(1).

       01  WS-SEARCH-TERM       PIC X(30).
       01  TEMP-FILE            PIC X(200).

       01 WS-FOUND PIC X VALUE "N".  *> Search book
       01 SEARCH-CRITERIA.
          05 SC-ID              PIC X(10).
          05 SC-NAME            PIC X(30).
          05 SC-AUTHOR          PIC X(25).
          05 SC-GENRE           PIC X(15).

       01  ws-book-line            PIC X(200).
       01  add-book-confirm-choice PIC 9(1).
       01  last-book-id            PIC 9(4) VALUE 0.
       01  eof-flag                PIC X VALUE 'N'.

       01  BOOK-STATUS          PIC XX.
       01  BOOK-HEADER.
           05 FILLER            PIC X(10) VALUE "BOOK ID".
           05 FILLER            PIC X(2)  VALUE SPACES.
           05 FILLER            PIC X(30) VALUE "BOOK NAME".
           05 FILLER            PIC X(2)  VALUE SPACES.
           05 FILLER            PIC X(25) VALUE "AUTHOR".
           05 FILLER            PIC X(1)  VALUE SPACES.
           05 FILLER            PIC X(5)  VALUE "COUNT".
           05 FILLER            PIC X(4)  VALUE SPACES.
           05 FILLER            PIC X(15) VALUE "GENRE".



       01  BOOK-DETAIL.
           05 book_id              PIC X(10).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_name            PIC X(30).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_author          PIC X(25).
           05 FILLER               PIC X(2)  VALUE SPACES.
           05 book_count           PIC 9(3).
           05 FILLER               PIC X(5)  VALUE SPACES.
           05 book_genre           PIC X(15).


       01  HEADER-LINE          PIC X(100) VALUE ALL '-'.

       *> update books section
       01  WS-BOOK-ID         PIC X(5).
       01  WS-BOOK-NAME       PIC X(30).
       01  WS-BOOK-AUTHOR     PIC X(30).
       01  WS-BOOK-GENRE      PIC X(30).
       01  WS-BOOK-COUNT      PIC 9(2).
       01  TEMP-ID            PIC X(5).
       01  TEMP-NAME          PIC X(30).
       01  TEMP-AUTHOR        PIC X(30).
       01  TEMP-GENRE         PIC X(30).
       01  TEMP-COUNT         PIC 9(2).
       01  INPUT-STR          PIC X(200).
       01  FILE-END           PIC X VALUE 'N'.
       01  FOUND-FLAG         PIC X VALUE 'N'.
       01  USER-ID            PIC X(5).
       01  NEW-NAME           PIC X(30).
       01  NEW-AUTHOR         PIC X(30).
       01  NEW-GENRE          PIC X(30).
       01  NEW-COUNT          PIC 9(2).
       01  LINE-TABLE.
           05  LINE-ENTRY OCCURS 100 TIMES.
               10  LINE-CONTENT  PIC X(200).

       77  LINE-ID            PIC 9(3) VALUE 1.
       77  I                  PIC 9(3) VALUE 1.

       LINKAGE SECTION.
       01 USER-CHOICE PIC 9(2).

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      **
      * The main procedure of the program
      **
           DISPLAY "1. Member"
           DISPLAY "2. Books Info"
           DISPLAY "3. Book Add/Return"
           DISPLAY "4. Records"
           DISPLAY "Enter your choice (1-4): "
           ACCEPT WS-CHOICE

           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM MEMBER-PARA
               WHEN 2
                   PERFORM BOOKS-INFO-PARA
               WHEN 3
                   PERFORM BOOKS-OPERATION-PARA
               WHEN 4
                   PERFORM RECORDS-PARA
               WHEN OTHER
                   DISPLAY "INVALID CHOICE"

           END-EVALUATE.

            STOP RUN.

      ** add other procedures here


       MEMBER-PARA.

           OPEN EXTEND MemberFile

           *> auto generate nat a sarr htoe yan
           ADD 1 TO member_id
           MOVE member_id TO member_id_disp

           MOVE 0 TO WS-MAX-MEMBER-ID.
           *> SET NOT-EOF TO TRUE.
           ADD 1 TO WS-MAX-MEMBER-ID GIVING member_id.
           MOVE member_id TO member_id_disp.
           DISPLAY "Generated Member ID: " member_id_disp.


           *> -------------------------------
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "* New Member Registration                     *"
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "* Enter Name       : "  ACCEPT member_name
           DISPLAY "* Enter Gender(M/F): "  ACCEPT member_gender
           DISPLAY "* Enter Email      : "  ACCEPT member_email
           DISPLAY "* Enter Address    : "  ACCEPT member_address
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "Enter 1. to create, 0. to exit:  "
           ACCEPT cm_choice

           IF cm_choice = 1 THEN
               STRING
                   member_id_disp DELIMITED BY SIZE
                   ","  DELIMITED BY SIZE
                   FUNCTION TRIM(member_name) DELIMITED BY SIZE
                   ","  DELIMITED BY SIZE
                   FUNCTION TRIM(member_gender) DELIMITED BY SIZE
                   ","  DELIMITED BY SIZE
                   FUNCTION TRIM(member_email) DELIMITED BY SIZE
                   ","  DELIMITED BY SIZE
                   FUNCTION TRIM(member_address) DELIMITED BY SIZE
                   ","  DELIMITED BY SIZE
                   *> FUNCTION TRIM(member_flag) DELIMITED BY SIZE
                   INTO WS-CSV-LINE
               END-STRING

               MOVE WS-CSV-LINE TO MemberRecord
               WRITE MemberRecord

               DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"
               DISPLAY "* Member Name  :  " FUNCTION TRIM(member_name)
               DISPLAY "* Member ID    :  " member_id_disp
               DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-"

               DISPLAY "Member created successfully."
           ELSE
               DISPLAY "New Member is not created."
           END-IF
           CLOSE MemberFile.

       BOOKS-INFO-PARA.
            MOVE 1 TO WS-SUBCHOICE
            PERFORM UNTIL WS-SUBCHOICE = 5
               DISPLAY " "
               DISPLAY "BOOK INFORMATION MENU"
               DISPLAY "1. List All Books"
               DISPLAY "2. Search Books"
               DISPLAY "3. Add New Book"
               DISPLAY "4. Update Book Info"
               DISPLAY "5. Exit"
               DISPLAY "Enter your choice (1-5): "
               ACCEPT WS-SUBCHOICE

               EVALUATE WS-SUBCHOICE
                   WHEN 1 PERFORM LIST-ALL-BOOKS
                   WHEN 2 PERFORM SEARCH-BOOKS
                   WHEN 3 PERFORM ADD-NEW-BOOK
                   WHEN 4 PERFORM UPDATE-BOOK-INFO
                   WHEN 5 DISPLAY "Program exits."
                   WHEN OTHER DISPLAY "INVALID CHOICE"
               END-EVALUATE
           END-PERFORM.

           LIST-ALL-BOOKS.
           OPEN INPUT BOOK-FILE
           IF BOOK-STATUS NOT = '00'
               DISPLAY "ERROR OPENING BOOKS FILE: " BOOK-STATUS
           ELSE
               DISPLAY " "
               DISPLAY "LIST OF ALL BOOKS"
               DISPLAY HEADER-LINE
               DISPLAY BOOK-HEADER
               DISPLAY HEADER-LINE

               PERFORM UNTIL BOOK-STATUS = '10'  *> End of file
                   READ BOOK-FILE INTO BOOK-RECORD
                       AT END DISPLAY " "
                       NOT AT END
                           UNSTRING BOOK-RECORD DELIMITED BY ','
                           INTO book_id, book_name, book_author
                           , book_count, book_genre
                           DISPLAY BOOK-DETAIL
                   END-READ
               END-PERFORM
               DISPLAY HEADER-LINE
               CLOSE BOOK-FILE
           END-IF.

           SEARCH-BOOKS.
           DISPLAY " "
           DISPLAY "Enter criteria (leave blank to skip):"
           DISPLAY "Book ID: "   ACCEPT SC-ID
           DISPLAY "Book Name: " ACCEPT SC-NAME
           DISPLAY "Author: "    ACCEPT SC-AUTHOR
           DISPLAY "Genre: "     ACCEPT SC-GENRE.
           MOVE "N" TO WS-FOUND

           OPEN INPUT BOOK-FILE
           IF BOOK-STATUS = '00'
               PERFORM UNTIL BOOK-STATUS = '10'
                   READ BOOK-FILE INTO BOOK-RECORD
                       AT END CONTINUE
                       NOT AT END
                           UNSTRING BOOK-RECORD DELIMITED BY ','
                               INTO book_id, book_name, book_author,
                               book_count, book_genre

                          IF (SC-ID = SPACES OR book_id = SC-ID) AND
                       (SC-NAME = SPACES OR book_name = SC-NAME) AND
                 (SC-AUTHOR = SPACES OR book_author = SC-AUTHOR) AND
                 (SC-GENRE = SPACES OR book_genre = SC-GENRE)

                               IF WS-FOUND = "N"
                                   DISPLAY " "
                                   DISPLAY "SEARCHED BOOK RESULTS"
                                   DISPLAY HEADER-LINE
                                   DISPLAY BOOK-HEADER
                                   DISPLAY HEADER-LINE
                                   MOVE "Y" TO WS-FOUND
                               END-IF
                               DISPLAY BOOK-DETAIL
                           END-IF
                   END-READ
               END-PERFORM

               IF WS-FOUND = "N"
                   DISPLAY "No books found matching: " SEARCH-CRITERIA
               ELSE
                   DISPLAY HEADER-LINE
               END-IF
               CLOSE BOOK-FILE
           ELSE
               DISPLAY "Error accessing books file: " BOOK-STATUS
           END-IF.

           ADD-NEW-BOOK.

           OPEN INPUT BOOK-FILE
           PERFORM UNTIL eof-flag = 'Y'
               READ BOOK-FILE INTO BOOK-RECORD
                   AT END
                       MOVE 'Y' TO eof-flag
                   NOT AT END
                       PERFORM GET-LAST-BOOK-ID
               END-READ
           END-PERFORM
           CLOSE BOOK-FILE

           ADD 1 TO last-book-id
           MOVE last-book-id TO book_id

           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"
           DISPLAY "*         Add New Book to Library           *"
           DISPLAY "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*"

           DISPLAY "Enter Book Name     : " ACCEPT book_name
           DISPLAY "Enter Author Name   : " ACCEPT book_author
           DISPLAY "Enter Book Count    : " ACCEPT book_count
           DISPLAY "Enter Genre         : " ACCEPT book_genre

           DISPLAY "*------------------------------------------*"
           DISPLAY "Enter 1 to Save, 0 to Cancel: "
           ACCEPT add-book-confirm-choice

           IF add-book-confirm-choice = 1 THEN
               STRING
                   book_id           DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_name)    DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_author)  DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   book_count        DELIMITED BY SIZE
                   ","               DELIMITED BY SIZE
                   FUNCTION TRIM(book_genre)   DELIMITED BY SIZE
                   INTO ws-book-line
               END-STRING

               OPEN EXTEND BOOK-FILE
               MOVE ws-book-line TO BOOK-RECORD
               WRITE BOOK-RECORD
               CLOSE BOOK-FILE

               DISPLAY "*------------------------------------------*"
               DISPLAY "Book successfully added to books.csv."
               DISPLAY "Book ID   : " book_id
               DISPLAY "Book Name : " book_name
               DISPLAY "*------------------------------------------*"
           ELSE
               DISPLAY "Book entry cancelled."
           END-IF.

           STOP RUN.

           GET-LAST-BOOK-ID.
           UNSTRING BOOK-RECORD DELIMITED BY "," INTO book_id
           MOVE book_id TO last-book-id.


           UPDATE-BOOK-INFO.
               DISPLAY " ".
               OPEN INPUT BOOK-FILE
           PERFORM UNTIL FILE-END = 'Y'
           READ BOOK-FILE
               AT END
                   MOVE 'Y' TO FILE-END
               NOT AT END
                   MOVE BOOK-RECORD TO INPUT-STR
                   UNSTRING INPUT-STR DELIMITED BY ","
                       INTO TEMP-ID, TEMP-NAME, TEMP-AUTHOR, TEMP-GENRE,
                       TEMP-COUNT
                   MOVE INPUT-STR TO LINE-CONTENT(LINE-ID)
                   ADD 1 TO LINE-ID
           END-READ
           END-PERFORM
           CLOSE BOOK-FILE

            DISPLAY "Enter Book ID to update: "
            ACCEPT USER-ID

                    PERFORM VARYING I FROM 1 BY 1 UNTIL I >= LINE-ID OR
       FOUND-FLAG = 'Y'
           MOVE LINE-CONTENT(I) TO INPUT-STR
           UNSTRING INPUT-STR DELIMITED BY ","
               INTO TEMP-ID, TEMP-NAME, TEMP-AUTHOR, TEMP-GENRE,
               TEMP-COUNT
           IF TEMP-ID = USER-ID
               DISPLAY "Current Name  : " TEMP-NAME
               DISPLAY "Current Author: " TEMP-AUTHOR
               DISPLAY "Current Genre : " TEMP-GENRE
               DISPLAY "Current Count : " TEMP-COUNT
               DISPLAY "Enter new name (or press ENTER to skip): "
               ACCEPT NEW-NAME
               IF NEW-NAME = SPACES THEN
                   MOVE TEMP-NAME TO NEW-NAME
               END-IF
               DISPLAY "Enter new author (or press ENTER to skip): "
               ACCEPT NEW-AUTHOR
               IF NEW-AUTHOR = SPACES THEN
                   MOVE TEMP-AUTHOR TO NEW-AUTHOR
               END-IF
               DISPLAY "Enter new genre (or press ENTER to skip): "
               ACCEPT NEW-GENRE
               IF NEW-GENRE = SPACES THEN
                   MOVE TEMP-GENRE TO NEW-GENRE
               END-IF
               DISPLAY "Enter new count (or press ENTER to skip): "
               ACCEPT NEW-COUNT
               IF NEW-COUNT = ZERO THEN
                   MOVE TEMP-COUNT TO NEW-COUNT
               END-IF
               STRING TEMP-ID DELIMITED BY SIZE ","
                      NEW-NAME DELIMITED BY SIZE ","
                      NEW-AUTHOR DELIMITED BY SIZE ","
                      NEW-GENRE DELIMITED BY SIZE ","
                      NEW-COUNT DELIMITED BY SIZE
                   INTO LINE-CONTENT(I)
               MOVE 'Y' TO FOUND-FLAG
           END-IF
       END-PERFORM

       IF FOUND-FLAG = 'N'
           DISPLAY "Book ID not found."
           STOP RUN
       END-IF

       OPEN OUTPUT BOOK-FILE
       PERFORM VARYING I FROM 1 BY 1 UNTIL I >= LINE-ID
           MOVE LINE-CONTENT(I) TO BOOK-RECORD
           WRITE BOOK-RECORD
       END-PERFORM
       CLOSE BOOK-FILE

       DISPLAY "Book info updated successfully."
       STOP RUN.


       BOOKS-OPERATION-PARA.
           DISPLAY "This is book issuance display.".

       RECORDS-PARA.
           DISPLAY "This is book return display.".


       END PROGRAM Main-Container.
