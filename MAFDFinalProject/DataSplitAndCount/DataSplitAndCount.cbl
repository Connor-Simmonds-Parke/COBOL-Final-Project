      ******************************************************************
      * Author:    Nick Sturch-Flint & Connor Simmonds-Parke
      * Date:      2021-03-31
      * Purpose:   This program will take the ValidRecords.dat file
      *            from the Edit.cbl program, and will sort it into two
      *            additional .dat files to be processed by another .cbl
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       program-id. DataSplitAndCount.
       author. Connor Simmonds-Parke, Nicholas Sturch-flint.
       date-written. 03-31-2021.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    Input Files
           select input-file
               assign to "../../../../data/ValidRecords.dat"
               organization is line sequential.
      *    Output Files
           select s-l-file
               assign to "../../../../data/S&LRecords.dat"
               organization is line sequential.

           select returns-file
               assign to "../../../../data/ReturnsRecords.dat"
               organization is line sequential.

           select totals-file
               assign to "../../../../data/CountsAndControlTotals.out"
               organization is line sequential.
      *
       DATA DIVISION.
       FILE SECTION.
      *    Input Records
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-code                pic x.
               88 il-code-S                    value 'S'.
               88 il-code-R                    value 'R'.
               88 il-code-L                    value 'L'.
           05 il-amount              pic 9(5)v99.
           05 il-type                pic x(2).
               88 il-type-CA                   value "CA".
               88 il-type-CR                   value "CR".
               88 il-type-DB                   value "DB".
           05 il-store-num           pic 99.
               88 il-store-1                   value 01.
               88 il-store-2                   value 02.
               88 il-store-3                   value 03.
               88 il-store-4                   value 04.
               88 il-store-5                   value 05.
               88 il-store-12                  value 12.
           05 il-invoice-num.
               10 il-invoice-letter1 pic x.
                   88 il-valid-letter1         value 'A' thru 'E'.
               10 il-invoice-letter2 pic x.
                   88 il-valid-letter2         value 'A' thru 'E'.
               10 il-invoice-dash    pic x.
                   88 il-dash                  value '-'.
               10 il-invoice-numbers pic 9(6).
                   88 il-valid-invoice-num     value 100000 thru 900000.
           05 il-SKU                 pic x(15).

      *    Output Records
       fd s-l-file
           data record is output-line
           record contains 36 characters.

       01 s-l-line                   pic x(36).

       fd returns-file
           data record is output-line
           record contains 36 characters.

       01 returns-line               pic x(36).

       fd totals-file
           data record is output-line
           record contains 89 characters.

       01 totals-line                pic x(89).
      *
       WORKING-STORAGE SECTION.
      *
       01 ws-header.
           05 filler                 pic x(25) value spaces.
           05 filler                 pic x(33)
               value "COUNTS AND CONTROL TOTALS REPORT".

       01 ws-detail-line.
           05 filler                 pic x(32) value spaces.
           05 filler                 pic x(15) value "TOTAL RECORDS: ".
           05 ws-dl-total-count      pic 999.

       01 ws-report-header.
           05 filler                 pic x(5)  value "TRANS".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "# OF ".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "% OF ".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "% OF ".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "% OF ".
           05 filler                 pic x(9)  value spaces.
           05 filler                 pic x(5)  value "% OF ".
           05 filler                 pic x(6)  value spaces.
           05 filler                 pic x(5)  value "TOTAL".

       01 ws-report-header2.
           05 filler                 pic x(5)  value "NAME ".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "TRANS".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "TRANS".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "CASH ".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(6)  value "CREDIT".
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(5)  value "DEBIT".
           05 filler                 pic x(6)  value spaces.
           05 filler                 pic x(6)  value "AMOUNT".

       01 ws-s-l-detail.
           05 filler                 pic x(13) value "S&L Records".
           05 ws-sl-count            pic 999.
           05 filler                 pic x(10) value spaces.
           05 ws-sl-per              pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-sl-ca-per           pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-sl-cr-per           pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(8)  value spaces.
           05 ws-sl-db-per           pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(5)  value spaces.
           05 ws-sl-total-amt        pic $zzz,zz9.99.

       01 ws-s-detail.
           05 filler                 pic x(13) value "Sales".
           05 ws-s-d-count           pic 999.
           05 filler                 pic x(10) value spaces.
           05 ws-s-per               pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-s-ca-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-s-cr-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(8)  value spaces.
           05 ws-s-db-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(5)  value spaces.
           05 ws-s-total-amt         pic $zzz,zz9.99.

       01 ws-l-detail.
           05 filler                 pic x(13) value "Layaways".
           05 ws-l-d-count           pic 999.
           05 filler                 pic x(10) value spaces.
           05 ws-l-per               pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-l-ca-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-l-cr-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(8)  value spaces.
           05 ws-l-db-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(5)  value spaces.
           05 ws-l-total-amt         pic $zzz,zz9.99.

       01 ws-r-detail.
           05 filler                 pic x(13) value "Returns".
           05 ws-r-count             pic 999.
           05 filler                 pic x(10) value spaces.
           05 ws-r-per               pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-r-ca-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(7)  value spaces.
           05 ws-r-cr-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(8)  value spaces.
           05 ws-r-db-per            pic zz9.9.
           05 filler                 pic x     value "%".
           05 filler                 pic x(5)  value spaces.
           05 ws-r-total-amt         pic $zzz,zz9.99.

       01 ws-store-header.
           05 filler                 pic x(5)  value spaces.
           05 filler                 pic x(7)  value "STORE #".
           05 filler                 pic x(2)  value spaces.
           05 filler                 pic x(13) value "S/L TRANS AMT".
           05 filler                 pic x(2)  value spaces.
           05 filler                 pic x(17)
               value "RETURNS TRANS AMT".


       01 ws-store1-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x     value "1".
           05 filler                 pic x(5)  value spaces.
           05 ws-s1-trans            pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s1-r-trans          pic $zz,zz9.99.

       01 ws-store2-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x     value "2".
           05 filler                 pic x(5)  value spaces.
           05 ws-s2-trans            pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s2-r-trans          pic $zz,zz9.99.

       01 ws-store3-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x     value "3".
           05 filler                 pic x(5)  value spaces.
           05 ws-s3-trans            pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s3-r-trans          pic $zz,zz9.99.
           05 filler                 pic x(14) value spaces.
           05 filler                 pic x(17)
               value "S/L GRAND TOTAL= ".
           05 ws-sl-grand-ttl        pic $$,$$$,$$9.99.

       01 ws-store4-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x     value "4".
           05 filler                 pic x(5)  value spaces.
           05 ws-s4-trans            pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s4-r-trans          pic $zz,zz9.99.
           05 filler                 pic x(10) value spaces.
           05 filler                 pic x(21)
               value "RETURNS GRAND TOTAL= ".
           05 ws-r-grand-ttl         pic $$$,$$9.99.

       01 ws-store5-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x     value "5".
           05 filler                 pic x(5)  value spaces.
           05 ws-s5-trans            pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s5-r-trans          pic $zz,zz9.99.


       01 ws-store12-details.
           05 filler                 pic x(8)  value spaces.
           05 filler                 pic x(2)  value "12".
           05 filler                 pic x(4)  value spaces.
           05 ws-s12-trans           pic $zz,zz9.99.
           05 filler                 pic x(5)  value spaces.
           05 ws-s12-r-trans         pic $zz,zz9.99.


       77 ws-cnst-100                pic 999   value 100.
       77 ws-cnst-0                  pic 9     value 0.
       77 ws-cnst-1                  pic 9     value 1.
       77 ws-cnst-2                  pic 9     value 2.
       77 ws-cnst-3                  pic 9     value 3.
       77 ws-cnst-open-file          pic x     value "o".
       77 ws-cnst-file-empty         pic x     value "x".

       01 ws-calculations.
           05 ws-calc-sl-percent     pic 999v9(4).
           05 ws-calc-returns-perc   pic 999v9(4).
           05 ws-calc-s-percent      pic 999v9(4).
           05 ws-calc-l-percent      pic 999v9(4).
           05 ws-cash-percent        pic 999v9(4).
           05 ws-credit-percent      pic 999v9(4).
           05 ws-debit-percent       pic 999v9(4).


       01 ws-running-totals.
           05 ws-s-l-count           pic 999   value 0.
           05 ws-returns-count       pic 999   value 0.
           05 ws-total-count         pic 999   value 0.
           05 ws-s-count             pic 999   value 0.
           05 ws-l-count             pic 999   value 0.
           05 ws-s-transactions      pic 9(5)v99.
           05 ws-l-transactions      pic 9(5)v99.
           05 ws-r-transactions      pic 9(5)v99.
           05 ws-sl-cash-count       pic 99    value 0.
           05 ws-sl-credit-count     pic 99    value 0.
           05 ws-sl-debit-count      pic 99    value 0.
           05 ws-s-cash-count        pic 99    value 0.
           05 ws-s-credit-count      pic 99    value 0.
           05 ws-s-debit-count       pic 99    value 0.
           05 ws-l-cash-count        pic 99    value 0.
           05 ws-l-credit-count      pic 99    value 0.
           05 ws-l-debit-count       pic 99    value 0.
           05 ws-r-cash-count        pic 99    value 0.
           05 ws-r-credit-count      pic 99    value 0.
           05 ws-r-debit-count       pic 99    value 0.

       01 ws-store-variables.
           05 ws-store1-sl-trans     pic 9(5)v99.
           05 ws-store1-r-trans      pic 9(5)v99.
           05 ws-store2-sl-trans     pic 9(5)v99.
           05 ws-store2-r-trans      pic 9(5)v99.
           05 ws-store3-sl-trans     pic 9(5)v99.
           05 ws-store3-r-trans      pic 9(5)v99.
           05 ws-store4-sl-trans     pic 9(5)v99.
           05 ws-store4-r-trans      pic 9(5)v99.
           05 ws-store5-sl-trans     pic 9(5)v99.
           05 ws-store5-r-trans      pic 9(5)v99.
           05 ws-store12-sl-trans    pic 9(5)v99.
           05 ws-store12-r-trans     pic 9(5)v99.

       01 ws-flags.
           05 ws-eof-flag            pic x.
               88 eof-open                     value "o".
               88 eof-empty                    value "x".

      *
       PROCEDURE DIVISION.
      *
       000-main.
           perform 100-open-files.
           perform 200-read-files.
           perform 300-perform-sorting
               until eof-empty.
           perform 400-print-totals.
           perform 500-close-files.
           goback.

       100-open-files.
           open output returns-file, s-l-file, totals-file.
           open input input-file.
           move ws-cnst-open-file              to ws-eof-flag.

       200-read-files.
           read input-file
               at END
                   move ws-cnst-file-empty     to ws-eof-flag.

       300-perform-sorting.

           IF il-code-R THEN
               perform 310-perform-returns
           END-IF.

           IF il-code-S THEN
               perform 320-perform-sales
           END-IF.

           IF il-code-L THEN
               perform 330-perform-layaways
           END-IF.

           perform 200-read-files.

       310-perform-returns.
           add ws-cnst-1                       to ws-returns-count
           add il-amount                       to ws-r-transactions
           write returns-line from input-line
               before advancing ws-cnst-1 line

      *        DETERMINE THE STORE FOR RETURNS
               IF il-store-1 THEN
                   add il-amount               to ws-store1-r-trans
               ELSE IF il-store-2 THEN
                   add il-amount               to ws-store2-r-trans
               ELSE IF il-store-3 THEN
                   add il-amount               to ws-store3-r-trans
               ELSE IF il-store-4 THEN
                   add il-amount               to ws-store4-r-trans
               ELSE IF il-store-5 THEN
                   add il-amount               to ws-store5-r-trans
               ELSE
                   add il-amount               to ws-store12-r-trans
               END-IF.

      *        CHECK FOR TRANSACTION TYPE FOR RETURNS
               IF il-type-CA THEN
                  add ws-cnst-1                to ws-r-cash-count
               ELSE IF il-type-CR THEN
                  add ws-cnst-1                to ws-r-credit-count
               ELSE IF il-type-DB THEN
                  add ws-cnst-1                to ws-r-debit-count
               END-IF.

       320-perform-sales.
           add ws-cnst-1                       to ws-s-count
           add ws-cnst-1                       to ws-s-l-count
           write s-l-line from input-line
               before advancing ws-cnst-1 line

      *        DETERMINE THE STORE
               IF il-store-1 THEN
                   add il-amount               to ws-store1-sl-trans
               ELSE IF il-store-2 THEN
                   add il-amount               to ws-store2-sl-trans
               ELSE IF il-store-3 THEN
                   add il-amount               to ws-store3-sl-trans
               ELSE IF il-store-4 THEN
                   add il-amount               to ws-store4-sl-trans
               ELSE IF il-store-5 THEN
                   add il-amount               to ws-store5-sl-trans
               ELSE
                   add il-amount               to ws-store12-sl-trans
               END-IF.

               add il-amount                   to ws-s-transactions
      *        CHECK FOR TRANSACTION TYPE
               IF il-type-CA THEN
                       add ws-cnst-1           to ws-sl-cash-count
                       add ws-cnst-1           to ws-s-cash-count
               ELSE IF il-type-CR THEN
                       add ws-cnst-1           to ws-sl-credit-count
                       add ws-cnst-1           to ws-s-credit-count
               ELSE IF il-type-DB THEN
                       add ws-cnst-1           to ws-sl-debit-count
                       add ws-cnst-1           to ws-s-debit-count
               END-IF.

       330-perform-layaways.
           add ws-cnst-1                       to ws-s-l-count
               write s-l-line from input-line
                 before advancing ws-cnst-1 line
               add ws-cnst-1                   to ws-l-count
               add il-amount                   to ws-l-transactions

      *        DETERMINE THE STORE FOR LAWAYS
               IF il-store-1 THEN
                   add il-amount               to ws-store1-sl-trans
               ELSE IF il-store-2 THEN
                   add il-amount               to ws-store2-sl-trans
               ELSE IF il-store-3 THEN
                   add il-amount               to ws-store3-sl-trans
               ELSE IF il-store-4 THEN
                   add il-amount               to ws-store4-sl-trans
               ELSE IF il-store-5 THEN
                   add il-amount               to ws-store5-sl-trans
               ELSE
                   add il-amount               to ws-store12-sl-trans
               END-IF.


      *        CHECK FOR TRANSACTION TYPE FOR LAYAWAYS
               IF il-type-CA THEN
                   add ws-cnst-1               to ws-l-cash-count
                   add ws-cnst-1               to ws-sl-cash-count
               ELSE IF il-type-CR THEN
                   add ws-cnst-1               to ws-sl-credit-count
                   add ws-cnst-1               to ws-l-credit-count
               ELSE IF il-type-DB THEN
                   add ws-cnst-1               to ws-sl-debit-count
                   add ws-cnst-1               to ws-l-debit-count
               END-IF.



       400-print-totals.

           perform 420-calc-totals.

           write totals-line from ws-header
               after advancing ws-cnst-1 line.

           write totals-line from ws-detail-line
               after advancing ws-cnst-2 lines.

           write totals-line from ws-report-header
               after advancing ws-cnst-2 lines.

           write totals-line from ws-report-header2
               after advancing ws-cnst-1 lines.

           write totals-line from ws-s-l-detail
               after advancing ws-cnst-2 lines.

           write totals-line from ws-s-detail
               after advancing ws-cnst-2 lines.

           write totals-line from ws-l-detail
               after advancing ws-cnst-2 lines.

           write totals-line from ws-r-detail
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store-header
               after advancing ws-cnst-3 lines.

           write totals-line from ws-store1-details
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store2-details
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store3-details
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store4-details
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store5-details
               after advancing ws-cnst-2 lines.

           write totals-line from ws-store12-details
               after advancing ws-cnst-2 lines.

       420-calc-totals.
      *    Determine Total Records
           add ws-s-l-count
             to ws-returns-count
             giving ws-total-count.

      *    Move Count Variables
           move ws-s-l-count                   to ws-sl-count.
           move ws-s-count                     to ws-s-d-count.
           move ws-l-count                     to ws-l-d-count.
           move ws-returns-count               to ws-r-count.
           move ws-total-count                 to ws-dl-total-count.

           move ws-s-transactions              to ws-s-total-amt.
           move ws-l-transactions              to ws-l-total-amt.
           move ws-r-transactions              to ws-r-total-amt.
           move ws-r-transactions              to ws-r-grand-ttl.

           add ws-s-transactions
               to ws-l-transactions
               giving ws-sl-grand-ttl.
           add ws-s-transactions
               to ws-l-transactions
               giving ws-sl-total-amt.

           move ws-store1-sl-trans             to ws-s1-trans.
           move ws-store1-r-trans              to ws-s1-r-trans.
           move ws-store2-sl-trans             to ws-s2-trans.
           move ws-store2-r-trans              to ws-s2-r-trans.
           move ws-store3-sl-trans             to ws-s3-trans.
           move ws-store3-r-trans              to ws-s3-r-trans.
           move ws-store4-sl-trans             to ws-s4-trans.
           move ws-store4-r-trans              to ws-s4-r-trans.
           move ws-store5-sl-trans             to ws-s5-trans.
           move ws-store5-r-trans              to ws-s5-r-trans.
           move ws-store12-sl-trans            to ws-s12-trans.
           move ws-store12-r-trans             to ws-s12-r-trans.



      *    S&L Percent Calculation
           divide ws-s-l-count
               by ws-total-count
               giving ws-calc-sl-percent rounded.
           multiply ws-calc-sl-percent
               by ws-cnst-100
               giving ws-calc-sl-percent.
           move ws-calc-sl-percent             to ws-sl-per.

      *    Returns Percent Calculation
           divide ws-returns-count
               by ws-total-count
               giving ws-calc-returns-perc rounded.
           multiply ws-calc-returns-perc
               by ws-cnst-100
               giving ws-calc-returns-perc.
           move ws-calc-returns-perc           to ws-r-per.

      *    Sales Percent Calculation
           divide ws-s-count
               by ws-total-count
               giving ws-calc-s-percent rounded.
           multiply ws-calc-s-percent
               by ws-cnst-100
               giving ws-calc-s-percent.
           move ws-calc-s-percent              to ws-s-per.

      *    Layaway Percent Calculation
           divide ws-l-count
               by ws-total-count
               giving ws-calc-l-percent rounded.
           multiply ws-calc-l-percent
               by ws-cnst-100
               giving ws-calc-l-percent.
           move ws-calc-l-percent              to ws-l-per.

      *    Cash Transaction Calculations
           divide ws-sl-cash-count
             by ws-sl-count
             giving ws-cash-percent rounded.
           multiply ws-cash-percent
             by ws-cnst-100
             giving ws-sl-ca-per.

           divide ws-s-cash-count
             by ws-s-count
             giving ws-cash-percent rounded.
           multiply ws-cash-percent
             by ws-cnst-100
             giving ws-s-ca-per.

           divide ws-l-cash-count
             by ws-l-count
             giving ws-cash-percent rounded.
           multiply ws-cash-percent
             by ws-cnst-100
             giving ws-l-ca-per.

           divide ws-r-cash-count
             by ws-r-count
             giving ws-cash-percent rounded.
           multiply ws-cash-percent
             by ws-cnst-100
             giving ws-r-ca-per.

      *    Credit Transaction Calculations
           divide ws-sl-credit-count
             by ws-sl-count
             giving ws-credit-percent rounded.
           multiply ws-credit-percent
             by ws-cnst-100
             giving ws-sl-cr-per.

           divide ws-s-credit-count
             by ws-s-count
             giving ws-credit-percent rounded.
           multiply ws-credit-percent
             by ws-cnst-100
             giving ws-s-cr-per.

           divide ws-l-credit-count
             by ws-l-count
             giving ws-credit-percent rounded.
           multiply ws-credit-percent
             by ws-cnst-100
             giving ws-l-cr-per.

           divide ws-r-credit-count
             by ws-r-count
             giving ws-credit-percent rounded.
           multiply ws-credit-percent
             by ws-cnst-100
             giving ws-r-cr-per.

      *    Debit Transaction Calculations
           divide ws-sl-debit-count
             by ws-sl-count
             giving ws-debit-percent rounded.
           multiply ws-debit-percent
             by ws-cnst-100
             giving ws-sl-db-per.

           divide ws-s-debit-count
             by ws-s-count
             giving ws-debit-percent rounded.
           multiply ws-debit-percent
             by ws-cnst-100
             giving ws-s-db-per.

           divide ws-l-debit-count
             by ws-l-count
             giving ws-debit-percent rounded.
           multiply ws-debit-percent
             by ws-cnst-100
             giving ws-l-db-per.

           divide ws-r-debit-count
             by ws-r-count
             giving ws-debit-percent rounded.
           multiply ws-debit-percent
             by ws-cnst-100
             giving ws-r-db-per.

       500-close-files.
           close s-l-file, returns-file, totals-file.
           close input-file.

       END PROGRAM DataSplitAndCount.
