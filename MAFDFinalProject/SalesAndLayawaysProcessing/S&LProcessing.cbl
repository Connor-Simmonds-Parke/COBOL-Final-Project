      ******************************************************************
      * Author:    Connor Simmonds-Parke, Nicholas Sturch-flint
      * Date:      2021-04-07
      * Purpose:   Produces a detail report of Sales and Layaways
      *            with some summary statistics at the end.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SalesAndLayawaysProcessing.
       author. Connor Simmonds-Parke, Nicholas Sturch-flint.
       date-written. 2021-04-07.


       environment division.
       configuration section.

       input-output section.
       file-control.

      *    Input Files
           select input-file
               assign to "../../../../data/S&LRecords.dat"
               organization is line sequential.

      *    Output Files
           select output-file
               assign to "../../../../data/S&LReport.out"
               organization is line sequential.


       data division.
       file section.

      *Input Records
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-code                    pic x.
               88 il-code-S              value 'S'.
               88 il-code-L              value 'L'.
           05 il-amount                  pic 9(5)v99.
           05 il-type                    pic x(2).
               88 il-type-CA             value "CA".
               88 il-type-CR             value "CR".
               88 il-type-DB             value "DB".
           05 il-store-num               pic 99.
               88 il-store-1             value 01.
               88 il-store-2             value 02.
               88 il-store-3             value 03.
               88 il-store-4             value 04.
               88 il-store-5             value 05.
               88 il-store-12            value 12.
           05 il-invoice-num.
               10 il-invoice-letter1     pic x.
                   88 il-valid-letter1   value 'A' thru 'E'.
               10 il-invoice-letter2     pic x.
                   88 il-valid-letter2   value 'A' thru 'E'.
               10 il-invoice-dash        pic x.
                   88 il-dash            value '-'.
               10 il-invoice-numbers     pic 9(6).
                   88 il-valid-invoice-num
                                         value 100000 thru 900000.
           05 il-SKU                     pic x(15).

      *Output Records
       fd output-file
           data record is output-line
           record contains 87 characters.

       01 output-line                    pic x(87).


       working-storage section.

      *Repeats each page, main displays page number
       01 ws-header1-main.
           05 filler                     pic x(25)   value spaces.
           05 filler                     pic x(23)
               value "SALES & LAYAWAYS REPORT".
           05 filler                     pic x(32)   value spaces.
           05 filler                     pic x(5)    value "PAGE".
           05 ws-header-page             pic z9.

       01 ws-header2-headings.
           05 filler                     pic x(14)
               value "Transaction".
           05 filler                     pic x(14)
               value "Transaction".
           05 filler                     pic x(10)   value "Payment".
           05 filler                     pic x(10)   value "Store".
           05 filler                     pic x(16)   value "Invoice".
           05 filler                     pic x(16)   value "SKU".
           05 filler                     pic x(7)    value "Tax".

       01 ws-header3-headings.
           05 filler                     pic x(16)   value "   Code".
           05 filler                     pic x(13)   value "Amount".
           05 filler                     pic x(9)    value "Type".
           05 filler                     pic x(10)   value "Number".
           05 filler                     pic x(16)   value "Number".
           05 filler                     pic x(16)   value "Code".
           05 filler                     pic x(7)    value "Owed".

       01 ws-header4-underlines.
           05 filler                     pic x(14)
               value "-----------".
           05 filler                     pic x(14)
               value "-----------".
           05 filler                     pic x(10)   value "-------".
           05 filler                     pic x(9)    value "------".
           05 filler                     pic x(12)   value  "---------".
           05 filler                     pic x(18)
               value "---------------".
           05 filler                     pic x(10)   value "----------".

      *Detail Line
       01 ws-detail-line.
           05 filler                     pic x(5)    value spaces.
           05 ws-dl-code                 pic x.
           05 filler                     pic x(8)    value spaces.
           05 ws-dl-amount               pic $zz,zz9.99.
           05 filler                     pic x(6)    value spaces.
           05 ws-dl-payment              pic xx.
           05 filler                     pic x(8)    value spaces.
           05 ws-dl-store-num            pic 99.
           05 filler                     pic x(5)    value spaces.
           05 ws-dl-invoice-num          pic x(9).
           05 filler                     pic x(3)    value spaces.
           05 ws-dl-SKU                  pic x(15).
           05 filler                     pic x(3)    value spaces.
           05 ws-dl-tax                  pic $zz,zz9.99.

      *SUMMARY LINES
       01 ws-summary-header.
           05 filler                     pic x(30)   value spaces.
           05 filler                     pic x(20)
               value "SUMMARY REPORT".
           05 filler                     pic x(37)   value spaces.

       01 ws-total-tax-owing.
           05 filler                     pic x(31)
               value "Total Tax Owing for All Stores=".
           05 filler                     pic x(3).
           05 ws-total-tax-sl            pic $zzzz9.99.
           05 filler                     pic x(44)   value spaces.

       01 ws-s-l-summary.
           05 filler                     pic x(29)
               value "Number of S&L Records".
           05 ws-sl-count                pic 99.
           05 filler                     pic x(5)    value spaces.
           05 filler                     pic x(26)
               value "Total Transaction Amount= ".
           05 filler                     pic x(3)    value spaces.
           05 ws-sl-total-amt            pic $zz,zz9.99.
           05 filler                     pic x(12)   value spaces.

       01 ws-s-summary.
           05 filler                     pic x(29)
               value "Number of Sales Records".
           05 ws-s-d-count               pic 99.
           05 filler                     pic x(5)    value spaces.
           05 filler                     pic x(26)
               value "Total Transaction Amount= ".
           05 filler                     pic x(3)    value spaces.
           05 ws-s-total-amt             pic $zz,zz9.99.
           05 filler                     pic x(12)   value spaces.

       01 ws-l-summary.
           05 filler                     pic x(29)
               value "Number of Layaways".
           05 ws-l-d-count               pic 99.
           05 filler                     pic x(5)    value spaces.
           05 filler                     pic x(26)
               value "Total Transaction Amount= ".
           05 filler                     pic x(3)    value spaces.
           05 ws-l-total-amt             pic $zz,zz9.99.
           05 filler                     pic x(12)   value spaces.

       01 ws-payment-types-ca.
           05 filler                     pic x(29)
               value "Number of Cash Transactions".
           05 ws-cash-count              pic 99.
           05 ws-filler                  pic x(5)    value spaces.
           05 filler                     pic x(33)
               value "Percents of Transaction in Cash".
           05 ws-cash-percentage         pic zz9.99.
           05 filler                     pic x       value "%".
           05 filler                     pic x(11)   value spaces.

       01 ws-payment-types-cr.
           05 filler                     pic x(29)
               value "Number of Cash Transactions".
           05 ws-credit-count            pic 99.
           05 ws-filler                  pic x(5)    value spaces.
           05 filler                     pic x(33)
               value "Percents of Transaction in Cash".
           05 ws-credit-percentage       pic zz9.99.
           05 filler                     pic x       value "%".
           05 filler                     pic x(11)   value spaces.

       01 ws-payment-types-db.
           05 filler                     pic x(29)
               value "Number of Cash Transactions".
           05 ws-debit-count             pic 99.
           05 ws-filler                  pic x(5)    value spaces.
           05 filler                     pic x(33)
               value "Percents of Transaction in Cash".
           05 ws-debit-percentage        pic zz9.99.
           05 filler                     pic x       value "%".
           05 filler                     pic x(11)   value spaces.

       01 ws-highest-earning-store.
           05 filler                     pic x(29)
               value "Highest Earning Store=".
           05 ws-high-store-name         pic x(8).
           05 filler                     pic x(3)    value spaces.
           05 ws-hes                     pic $zzz9.99.
           05 filler                     pic x(39)   value spaces.

       01 ws-lowest-earning-store.
           05 filler                     pic x(29)
               value "Lowest Earning Store=".
           05 ws-low-store-name          pic x(8).
           05 filler                     pic x(3)    value spaces.
           05 ws-les                     pic $zzz9.99.
           05 filler                     pic x(39)   value spaces.

      *Page and Line Variables
       01 ws-calculations.
           05 ws-calc-sl-percent         pic 999v9(4).
           05 ws-calc-returns-perc       pic 999v9(4).
           05 ws-calc-s-percent          pic 999v9(4).
           05 ws-calc-l-percent          pic 999v9(4).
           05 ws-cash-percent            pic 999v9(4).
           05 ws-credit-percent          pic 999v9(4).
           05 ws-debit-percent           pic 999v9(4).

       01 ws-temp-variables.
           05 ws-temp-tax-amount         pic 9(5)v99.
           05 ws-total-tax               pic 9(5)v99.
           05 ws-tbl-stn1                pic 9(5)v99.
           05 ws-tbl-stn2                pic 9(5)v99.
           05 ws-tbl-stn3                pic 9(5)v99.
           05 ws-tbl-stn4                pic 9(5)v99.
           05 ws-tbl-stn5                pic 9(5)v99.
           05 ws-tbl-stn12               pic 9(5)v99.
           05 ws-highest-amt             pic 9(5)v99 value 0.
           05 ws-lowest-amt              pic 9(5)v99 value 90000.

       01 ws-running-totals.
           05 ws-page-counter            pic 99      value 0.
           05 ws-line-counter            pic 99      value 0.
           05 ws-sl-trans-amt            pic 9(5)v99.
           05 ws-s-trans-amt             pic 9(5)v99.
           05 ws-l-trans-amt             pic 9(5)v99.

       01 ws-counters.
           05 ws-tbl-rc-sl               pic 999     value 0.
           05 ws-tbl-rc-s                pic 999     value 0.
           05 ws-tbl-rc-l                pic 999     value 0.
           05 ws-tbl-tt-ca               pic 99      value 0.
           05 ws-tbl-tt-cr               pic 99      value 0.
           05 ws-tbl-tt-db               pic 99      value 0.
           05 ws-counter                 pic 99      value 0.

      *General Constants
       77 ws-cnst-100                    pic 999     value 100.
       77 ws-cnst-0                      pic 9       value 0.
       77 ws-cnst-1                      pic 9       value 1.
       77 ws-cnst-2                      pic 9       value 2.
       77 ws-cnst-3                      pic 9       value 3.
      * 77 ws-cnst-6                      pic 9       value 6.
       77 ws-cnst-open-file              pic x       value "o".
       77 ws-cnst-file-empty             pic x       value "x".
       77 ws-cnst-tax-percentage         pic 9v99    value 0.13.
       77 ws-lines-per-page              pic 99      value 20.
       77 ws-cnst-store-1                pic x(8)    value "STORE 1".
       77 ws-cnst-store-2                pic x(8)    value "STORE 2".
       77 ws-cnst-store-3                pic x(8)    value "STORE 3".
       77 ws-cnst-store-4                pic x(8)    value "STORE 4".
       77 ws-cnst-store-5                pic x(8)    value "STORE 5".
       77 ws-cnst-store-12               pic x(8)    value "STORE 12".

       01 ws-flags.
           05 ws-eof-flag                pic x.
               88 eof-open               value "o".
               88 eof-empty              value "x".


       procedure division.
       000-main.
           perform 100-open-files.
           perform 200-read-files.
           perform 400-perform-details
               until eof-empty.
           perform 500-print-totals.
           perform 600-close-files.
           goback.

       100-open-files.
      *    Open input and output files
           open input input-file,
                output output-file.
           move ws-cnst-open-file              to ws-eof-flag.

       200-read-files.
      *    Check to see if input file is empty
           read input-file
               at END
                   move ws-cnst-file-empty     to ws-eof-flag.

       300-print-headings.
      *    INIALIZE COUNTERS
           add ws-cnst-1                       to ws-page-counter.

           IF ws-page-counter > ws-cnst-1 THEN
               write output-line from spaces
               write output-line
                   after advancing page
               perform 310-prepare-headings
      *        write output-line from spaces
           ELSE
               perform 310-prepare-headings
           END-IF.

       310-prepare-headings.

           move ws-page-counter                to ws-header-page.

           write output-line from ws-header1-main
               after advancing ws-cnst-1 lines.
           write output-line from ws-header2-headings
               after advancing ws-cnst-2 lines.
           write output-line from ws-header3-headings
               after advancing ws-cnst-1 lines.
           write output-line from ws-header4-underlines
               after advancing ws-cnst-1 lines.

       400-perform-details.
           perform 300-print-headings.
           perform 410-prepare-details
               varying ws-line-counter from ws-cnst-1 by ws-cnst-1
                   until (ws-line-counter > ws-lines-per-page
                   or eof-empty).

       410-prepare-details.
           move il-code                        to ws-dl-code.
           move il-amount                      to ws-dl-amount.
           move il-type                        to ws-dl-payment.
           move il-store-num                   to ws-dl-store-num.
           move il-invoice-num                 to ws-dl-invoice-num.
           move il-SKU                         to ws-dl-SKU.

           add ws-cnst-1                       to ws-tbl-rc-sl.
           add il-amount                       to ws-sl-trans-amt.

      *    CALCULATIONS
           multiply il-amount
               by ws-cnst-tax-percentage
               giving ws-temp-tax-amount rounded.
           add ws-temp-tax-amount              to ws-total-tax.

      *    DETERMINE SALE OR LAYAWAY
           IF il-code-S THEN
               add ws-cnst-1                   to ws-tbl-rc-s
               add il-amount                   to ws-s-trans-amt
           ELSE IF il-code-L THEN
               add ws-cnst-1                   to ws-tbl-rc-l
               add il-amount                   to ws-l-trans-amt
           END-IF.

      *    DETERMINE TRANSACTION TYPE
           IF il-type-CA THEN
               add ws-cnst-1                   to ws-tbl-tt-ca
           ELSE IF il-type-CR THEN
               add ws-cnst-1                   to ws-tbl-tt-cr
           ELSE IF il-type-DB THEN
               add ws-cnst-1                   to ws-tbl-tt-db
           END-IF.

      *    DETERMINE THE STORE
           IF il-store-1 THEN
               add il-amount                   to ws-tbl-stn1
           ELSE IF il-store-2 THEN
               add il-amount                   to ws-tbl-stn2
           ELSE IF il-store-3 THEN
               add il-amount                   to ws-tbl-stn3
           ELSE IF il-store-4 THEN
               add il-amount                   to ws-tbl-stn4
           ELSE IF il-store-5 THEN
               add il-amount                   to ws-tbl-stn5
           ELSE
               add il-amount                   to ws-tbl-stn12
           END-IF.

      *    FINAL MOVES BEFORE WRITING THE LINES
           move ws-temp-tax-amount to ws-dl-tax.

           write output-line from ws-detail-line
               after advancing ws-cnst-1 lines.

           perform 200-read-files.

       500-print-totals.
           perform 510-calculate-totals.

           write output-line from ws-summary-header
               after advancing ws-cnst-3 lines.
           write output-line from ws-s-l-summary
               after advancing ws-cnst-2 lines.
           write output-line from ws-s-summary
               after advancing ws-cnst-1 line.
           write output-line from ws-l-summary
               after advancing ws-cnst-1 line.
           write output-line from ws-total-tax-owing
               after advancing ws-cnst-2 lines.

           write output-line from ws-payment-types-ca
               after advancing ws-cnst-2 lines.
           write output-line from ws-payment-types-cr
               after advancing ws-cnst-1 lines.
           write output-line from ws-payment-types-db
               after advancing ws-cnst-1 lines.

           write output-line from ws-highest-earning-store
               after advancing ws-cnst-2 lines.
           write output-line from ws-lowest-earning-store
               after advancing ws-cnst-2 lines.


       510-calculate-totals.
      *        DETERMINE HIGHEST PRODUCING STORE
               IF ws-tbl-stn1 > ws-highest-amt
                   move ws-tbl-stn1            to ws-highest-amt
                   move ws-cnst-store-1        to ws-high-store-name
               END-IF.
               IF ws-tbl-stn2 > ws-highest-amt
                   move ws-tbl-stn2            to ws-highest-amt
                   move ws-cnst-store-2        to ws-high-store-name
               END-IF.
               IF ws-tbl-stn3 > ws-highest-amt
                   move ws-tbl-stn3            to ws-highest-amt
                   move ws-cnst-store-3        to ws-high-store-name
               END-IF.
               IF ws-tbl-stn4 > ws-highest-amt
                   move ws-tbl-stn4            to ws-highest-amt
                   move ws-cnst-store-4        to ws-high-store-name
               END-IF.
               IF ws-tbl-stn5 > ws-highest-amt
                   move ws-tbl-stn5            to ws-highest-amt
                   move ws-cnst-store-5        to ws-high-store-name
               END-IF.
               IF ws-tbl-stn12 > ws-highest-amt
                   move ws-tbl-stn12           to ws-highest-amt
                   move ws-cnst-store-12       to ws-high-store-name
               END-IF.
      *        DETERMINE LOWEST PRODUCING STORE
               IF ws-tbl-stn1 < ws-lowest-amt
                   move ws-tbl-stn1            to ws-lowest-amt
                   move ws-cnst-store-1        to ws-low-store-name
               END-IF.
               IF ws-tbl-stn2 < ws-lowest-amt
                   move ws-tbl-stn2            to ws-lowest-amt
                   move ws-cnst-store-2        to ws-low-store-name
               END-IF.
               IF ws-tbl-stn3 < ws-lowest-amt
                   move ws-tbl-stn3            to ws-lowest-amt
                   move ws-cnst-store-3        to ws-low-store-name
               END-IF.
               IF ws-tbl-stn4 < ws-lowest-amt
                   move ws-tbl-stn4            to ws-lowest-amt
                   move ws-cnst-store-4        to ws-low-store-name
               END-IF.
               IF ws-tbl-stn5 < ws-lowest-amt
                   move ws-tbl-stn5            to ws-lowest-amt
                   move ws-cnst-store-5        to ws-low-store-name
               END-IF.
               IF ws-tbl-stn12 < ws-lowest-amt
                   move ws-tbl-stn12           to ws-lowest-amt
                   move ws-cnst-store-12       to ws-low-store-name
               END-IF.


           move ws-tbl-rc-sl                   to ws-sl-count.
           move ws-tbl-rc-s                    to ws-s-d-count.
           move ws-tbl-rc-l                    to ws-l-d-count.
           move ws-sl-trans-amt                to ws-sl-total-amt.
           move ws-s-trans-amt                 to ws-s-total-amt.
           move ws-l-trans-amt                 to ws-l-total-amt.
           move ws-tbl-tt-ca                   to ws-cash-count.
           move ws-tbl-tt-cr                   to ws-credit-count.
           move ws-tbl-tt-db                   to ws-debit-count.
           move ws-total-tax                   to ws-total-tax-sl.
      *    MOVE HIGHEST AND LOWEST VARIABLES TO THEIR PLACE
           move ws-highest-amt                 to ws-hes.
           move ws-lowest-amt                  to ws-les.

           divide ws-tbl-tt-ca
             by ws-tbl-rc-sl
             giving ws-cash-percent rounded.
           multiply ws-cash-percent
             by ws-cnst-100
             giving ws-cash-percent rounded.
           move ws-cash-percent                to ws-cash-percentage.

           divide ws-tbl-tt-cr
             by ws-tbl-rc-sl
             giving ws-credit-percent rounded.
           multiply ws-credit-percent
             by ws-cnst-100
             giving ws-credit-percent rounded.
           move ws-credit-percent              to ws-credit-percentage.

           divide ws-tbl-tt-db
             by ws-tbl-rc-sl
             giving ws-debit-percent rounded.
           multiply ws-debit-percent
               by ws-cnst-100
               giving ws-debit-percent rounded.
           move ws-debit-percent               to ws-debit-percentage.



       600-close-files.
           close output-file.
           close input-file.

       end program SalesAndLayawaysProcessing.
