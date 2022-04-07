       identification division.

       program-id. Edit.
       author. Connor Simmonds-Parke, Nicholas Sturch-flint.
       date-written. 03-29-2021.
      *Description: The Edit program is responsible for editing the 
      *             input records. Records will be output into two files
      *             - valid and invalid. An error report file will be
      *             created.

       environment division.
       configuration section.

       input-output section.
       file-control.

      *    Input Files
           select input-file
               assign to "../../../../data/project6.dat"
               organization is line sequential.

      *    Output Files          
           select error-file
               assign to "../../../../data/ErrorReport.out"
               organization is line sequential. 

           select valid-file
               assign to "../../../../data/ValidRecords.dat"
               organization is line sequential. 

           select invalid-file
               assign to "../../../../data/InvalidRecords.dat"
               organization is line sequential. 


       data division.
       file section.

      *Input Records
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-code                       pic x.
               88 il-code-S                 value 'S'.
               88 il-code-R                 value 'R'.
               88 il-code-L                 value 'L'.
           05 il-amount                     pic 9(5)v99.
           05 il-type                       pic x(2).
               88 il-type-CA                value "CA".
               88 il-type-CR                value "CR".
               88 il-type-DB                value "DB".
           05 il-store-num                  pic 99.
               88 il-valid-store-num1       value 01 thru 05.
               88 il-valid-store-num2       value 12.
           05 il-invoice-num.
               10 il-invoice-letter1        pic x.
                   88 il-valid-letter1      value 'A' thru 'E'.
               10 il-invoice-letter2        pic x.
                   88 il-valid-letter2      value 'A' thru 'E'.
               10 il-invoice-dash           pic x.
                   88 il-dash               value '-'.
               10 il-invoice-numbers        pic 9(6).
                   88 il-valid-invoice-num  value 100000 thru 900000.
           05 il-SKU                        pic x(15).

      *Output Records
       fd error-file
           data record is error-line
           record contains 79 characters.

       01 error-line                        pic x(79). 

       fd valid-file
           data record is valid-line
           record contains 36 characters.

       01 valid-line                        pic x(36). 

       fd invalid-file
           data record is invalid-line
           record contains 36 characters.

       01 invalid-line                      pic x(36). 


       working-storage section.

      *Repeats each page, main displays page number
       01 ws-header1-main.
           05 filler                        pic x(10)
               value spaces.
           05 filler                        pic x(12)
               value "ERROR REPORT".
           05 filler                        pic x(4)
               value spaces.
           05 filler                        pic x(5)
               value "PAGE ".
           05 ws-header-page                pic z9.
           05 filler                        pic x(46)
               value spaces.

       01 ws-header2-headings.
           05 filler                        pic x(6)
               value "Record".
           05 filler                        pic x(10)
               value spaces.
           05 filler                        pic x(8)
               value "Raw Data".
           05 filler                        pic x(55)
               value spaces.

       01 ws-header3-headings.
           05 filler                        pic x(6)
               value "Number".
           05 filler                        pic x(6)  
               value spaces.
           05 filler                        pic x(18)
               value "and Error Messages".
           05 filler                        pic x(49)
               value spaces.

       01 ws-header4-underlines.
           05 filler                        pic x(6)
               value "------".
           05 filler                        pic x(6)
               value spaces.
           05 filler                        pic x(18)
               value "------------------".
           05 filler                        pic x(49)
               value spaces.

      *Detail Line
       01 ws-detail-line.
           05 filler                        pic x
               value space.
           05 ws-record-num                 pic zz9.
           05 filler                        pic x(4)
               value spaces.
           05 ws-input-data                 pic x(36).
           05 filler                        pic x(35)
               value spaces. 

      *Error Line
       01 ws-error-lines.
           05 filler                        pic x(8)
               value spaces.
           05 ws-error-message              pic x(71).

      *Summary Lines
       01 ws-summary1-total.
           05 filler                        pic x(28)
               value "Number of Records         = ".
           05 ws-total-records              pic zz9.
           05 filler                        pic x(48)
               value spaces.

       01 ws-summary2-valid.
           05 filler                        pic x(28)
               value "Number of Valid Records   = ".
           05 ws-total-valid                pic zz9.
           05 filler                        pic x(48)
               value spaces.

       01 ws-summary3-invalid.
           05 filler                        pic x(28)
               value "Number of Invalid Records = ".
           05 ws-total-invalid              pic zz9.
           05 filler                        pic x(48)
               value spaces.

      *Error Messages
       01 ws-error-messages.
           05 ws-code-error                 pic x(39)
               value "- TRANSACTION CODE NOT 'S', 'R', OR 'L'".
           05 ws-amount-error               pic x(32)
               value "- TRANSACTION AMOUNT NOT NUMERIC".
           05 ws-payment-error              pic x(32)
               value "- PAYMENT TYPE NOT CA, CR, OR DB".
           05 ws-store-num-error            pic x(46)
               value "- STORE NUMBER NOT ANY OF 01 THROUGH 05, OR 12".
           05 ws-invoice-format-error       pic x(34)
               value "- INVOICE FORMAT MUST BE XX-000000".
           05 ws-invoice-letter-error       pic x(47)
               value "- INVOICE NUMBER XX NOT 'A','B','C','D', OR 'E'".
           05 ws-duplicate-error            pic x(47)
               value "- INVOICE NUMBER XX CAN'T HAVE THE SAME LETTERS".
           05 ws-invoice-number-error       pic x(71)
               value "- INVOICE NUMBER 000000 MUST BE BETWEEN 100000 AND
      -              " 900000 INCLUSIVE".
           05 ws-invoice-dash-error         pic x(45)
               value "- NO DASH IN POSITION THREE OF INVOICE NUMBER".
           05 ws-SKU-error                  pic x(26)
               value "- SKU CODE CANNOT BE EMPTY".

      *Error Checks for each record (0 = no error, 1 = error)
       77 ws-errors                         pic 9
           value 0.
       77 ws-code-check                     pic 9
           value 0.
       77 ws-amount-check                   pic 9
           value 0.
       77 ws-payment-check                  pic 9
           value 0.
       77 ws-store-num-check                pic 9
           value 0.
       77 ws-format-check                   pic 9
           value 0.
       77 ws-letter-check                   pic 9
           value 0.
       77 ws-duplicate-check                pic 9
           value 0.
       77 ws-number-check                   pic 9
           value 0.
       77 ws-dash-check                     pic 9
           value 0.
       77 ws-SKU-check                      pic 9
           value 0.

      *Page and Line Variables
       77 ws-line-count                     pic 99 
           value 0.
       77 ws-page-count                     pic 99 
           value 0.
       77 ws-lines-per-page                 pic 99 
           value 5.

      *Total Counts
       77 ws-record-position                pic 999
           value 0. 
       77 ws-tot-invalid                    pic 999
           value 0.
       77 ws-tot-valid                      pic 999
           value 0.

      *General Constants
       77 ws-eof-flag                       pic x
           value "n".


       procedure division.
       000-main.

      *    Open input and output files
           open input input-file,
                output invalid-file,
                       error-file,
                       valid-file.

      *    Check to see if input file is empty
           read input-file 
               at end move 'y'              to ws-eof-flag.

      *    Peform until end of file
           perform 100-process-pages
               varying ws-page-count from 1 by 1
               until   ws-eof-flag = 'y'.  

      *    Write Error Summary
           perform 500-print-summary.

      *    Close input and output files
           close input-file,
                 error-file,
                 invalid-file,
                 valid-file.

      *    End of 000-main (end of Edit program)
           stop run. 


       100-process-pages.

      *    Reset line count
           move 1                           to ws-line-count.

      *    Print headers
           perform 150-print-headings.

      *    Detail lines + calculations performed until max lines per
      *    page limit is hit
           perform 200-process-lines  
               until ws-line-count > ws-lines-per-page 
                   or ws-eof-flag = 'y'.

      *    Start a new page unless last record
           if ws-eof-flag = 'n' then
               write error-line from spaces
                   after page
           end-if. 


       150-print-headings.

      *    Clear Detail Line
           move spaces                      to ws-detail-line.
           move ws-page-count               to ws-header-page. 

      *    Write the headers
           write error-line from ws-header1-main
               after advancing 1 lines
           write error-line from ws-header2-headings
               after advancing 2 lines.
           write error-line from ws-header3-headings
               after advancing 1 line.
           write error-line from ws-header4-underlines
               after advancing 1 line. 


       200-process-lines.

      *    Clear Detail Line and record checks
           move spaces                      to ws-detail-line
           move 0                           to ws-errors.
           move 0                           to ws-code-check.
           move 0                           to ws-amount-check.
           move 0                           to ws-payment-check.
           move 0                           to ws-store-num-check.
           move 0                           to ws-format-check.
           move 0                           to ws-letter-check.
           move 0                           to ws-duplicate-check.
           move 0                           to ws-number-check.
           move 0                           to ws-dash-check.
           move 0                           to ws-SKU-check.

      *    Add 1 to the next record number
           add 1                            to ws-record-position.

      *    Check each record for any errors
           perform 300-validation.

      *    If there are errors write them to error report and write the
      *    record to invalid records file
           if ws-errors > 0 then
               add 1                        to ws-tot-invalid
               perform 400-print-errors
               write invalid-line from input-line
           end-if.

      *    If no errors write to the valid records file
           if ws-errors = 0 then 
               add 1                        to ws-tot-valid
               write valid-line from input-line
           end-if.

      *    Check for end of file.
           read input-file 
               at end move 'y'              to ws-eof-flag.

       
       300-validation.

      *    1.Check Transaction Code Sales(S), Returns(R), Layaways(L)
           if not il-code-L and not il-code-R and not il-code-S then
               move 1                       to ws-errors
               move 1                       to ws-code-check
           end-if.

      *    2.Check if Transaction Amount is numeric
           if il-amount is not numeric then
               move 1                       to ws-errors
               move 1                       to ws-amount-check
           end-if.

      *    3.Check the Payment Type Cash(CA), Credit(CR), Debit(DB)
           if not il-type-CA and not il-type-CR and not il-type-DB then
               move 1                       to ws-errors
               move 1                       to ws-payment-check
           end-if.

      *    4.Check the Store Number 01 through 05 and 12
           if not il-valid-store-num1 and not il-valid-store-num2 then
               move 1                       to ws-errors
               move 1                       to ws-store-num-check
           end-if.

      *    5.Check Invoice format XX-000000 (all lead to the same error,
      *      used 'else if' to help seperate what is being checked)
           if il-invoice-letter1 is not alphabetic or
              il-invoice-letter2 is not alphabetic then
               move 1                       to ws-errors
               move 1                       to ws-format-check
           else if not il-dash then
               move 1                       to ws-errors
               move 1                       to ws-format-check
           else if il-invoice-numbers not numeric then
               move 1                       to ws-errors
               move 1                       to ws-format-check
           end-if.
              
      *    6.Check Invoice Number XX contains only A,B,C,D,E
           if not il-valid-letter1 or not il-valid-letter2 then
               move 1                       to ws-errors
               move 1                       to ws-letter-check
           end-if.

      *    7.Check if Inovice Number XX has duplicate letters
           if il-invoice-letter1 = il-invoice-letter2 then
               move 1                       to ws-errors
               move 1                       to ws-duplicate-check
           end-if.

      *    8.Check if Invoice Number 000000 not between 100000 and
      *      900000 inclusive
           if not il-valid-invoice-num then
               move 1                       to ws-errors
               move 1                       to ws-number-check
           end-if.

      *    9.Check to see if position 3 in Invoice Number is a dash '-'
           if not il-dash then
               move 1                       to ws-errors
               move 1                       to ws-dash-check
           end-if.

      *    10.Check SKU to make sure it is not empty
           if il-SKU = spaces then
               move 1                       to ws-errors
               move 1                       to ws-SKU-check
           end-if.


       400-print-errors.

      *    Add 1 to line count
           add 1                            to ws-line-count

      *    Move the input record data to detail line
           move ws-record-position          to ws-record-num.
           move input-line                  to ws-input-data.

      *    Write the record position and data
           write error-line from ws-detail-line
               after advancing 2 lines. 

      *    Write the specific error(s) under the record
      *    1.Transaction Code
           if ws-code-check = 1 then
               move ws-code-error           to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    2.Transaction Amount
           if ws-amount-check = 1 then
               move ws-amount-error         to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    3.Payment Type
           if ws-payment-check = 1 then
               move ws-payment-error        to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    4.Store Number
           if ws-store-num-check = 1 then
               move ws-store-num-error      to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    5.Invoice Format XX-000000
           if ws-format-check = 1 then
               move ws-invoice-format-error to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    6.Invoice Letters XX
           if ws-letter-check = 1 then
               move ws-invoice-letter-error to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    7.Invoice Duplicate Letters
           if ws-duplicate-check = 1 then
               move ws-duplicate-error      to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    8.Invoice Number Range
           if ws-number-check = 1 then
               move ws-invoice-number-error to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    9.Invoice Dash
           if ws-dash-check = 1 then
               move ws-invoice-dash-error   to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.

      *    10.SKU Empty
           if ws-SKU-check = 1 then
               move ws-SKU-error            to ws-error-message
               write error-line from ws-error-lines
                   after advancing 1 line
           end-if.


       500-print-summary.

      *    Move totals
           move ws-record-position          to ws-total-records.
           move ws-tot-valid                to ws-total-valid.
           move ws-tot-invalid              to ws-total-invalid.

      *    Write Summary Lines
           write error-line from ws-summary1-total
               after advancing 2 lines.
           write error-line from ws-summary2-valid
               after advancing 1 line.
           write error-line from ws-summary3-invalid
               after advancing 1 line.


       end program Edit.