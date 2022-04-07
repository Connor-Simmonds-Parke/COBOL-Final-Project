       identification division.

       program-id. ReturnsProcessing.
       author. Connor Simmonds-Parke, Nicholas Sturch-flint.
       date-written. 04-02-2021.
      *Description: Produces a detail report of the Returns with some  
      *             summary statistics at the end.

       environment division.
       configuration section.

       input-output section.
       file-control.

      *    Input Files
           select input-file
               assign to "../../../../data/ReturnsRecords.dat"
               organization is line sequential.

      *    Output Files          
           select output-file
               assign to "../../../../data/ReturnsReport.out"
               organization is line sequential.  


       data division.
       file section.

      *Input Records
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-code                       pic x.
           05 il-amount                     pic 9(5)v99.
           05 il-type                       pic x(2).
           05 il-store-num                  pic 99.
               88 il-store-num-1            value 01.
               88 il-store-num-2            value 02.
               88 il-store-num-3            value 03.
               88 il-store-num-4            value 04.
               88 il-store-num-5            value 05.
               88 il-store-num-12           value 12.
           05 il-invoice-num                pic x(9).
           05 il-SKU                        pic x(15).

      *Output Records
       fd output-file
           data record is valid-line
           record contains 87 characters.

       01 output-line                       pic x(87). 


       working-storage section.

      *Repeats each page, main displays page number
       01 ws-header1-main.
           05 filler                        pic x(28)
               value spaces.
           05 filler                        pic x(25)
               value "RETURNS PROCESSING REPORT".
           05 filler                        pic x(27)
               value spaces.
           05 filler                        pic x(5)
               value "PAGE ".
           05 ws-header-page                pic z9.

       01 ws-header2-headings.
           05 filler                        pic x(14)
               value "Transaction   ".
           05 filler                        pic x(14)
               value "Transaction	".
           05 filler                        pic x(10)
               value "Payment   ".
           05 filler                        pic x(10)
               value "Store     ".
           05 filler                        pic x(16)
               value "Invoice         ".
           05 filler                        pic x(16)
               value "SKU             ".
           05 filler                        pic x(7)
               value "Tax    ".

       01 ws-header3-headings.
           05 filler                        pic x(16)
               value "   Code         ".
           05 filler                        pic x(13)
               value "Amount       ".
           05 filler                        pic x(9)
               value "Type     ".
           05 filler                        pic x(10)
               value "Number    ".
           05 filler                        pic x(16)
               value "Number          ".
           05 filler                        pic x(16)
               value "Code            ".
           05 filler                        pic x(7)
               value "Owed   ".

       01 ws-header4-underlines.
           05 filler                        pic x(14)
               value "-----------   ".
           05 filler                        pic x(14)
               value "-----------   ".
           05 filler                        pic x(10)
               value "-------   ".
           05 filler                        pic x(9)
               value "------   ".
           05 filler                        pic x(12)
               value "---------   ".
           05 filler                        pic x(18)
               value "---------------   ".
           05 filler                        pic x(10)
               value "----------".

      *Detail Line
       01 ws-detail-line.
           05 filler                        pic x(5)
               value spaces.
           05 ws-code                       pic x.
           05 filler                        pic x(8)
               value spaces.
           05 ws-amount                     pic $zz,zz9.99.
           05 filler                        pic x(6)
               value spaces.
           05 ws-payment                    pic xx.
           05 filler                        pic x(8)
               value spaces.
           05 ws-store-num                  pic 99.
           05 filler                        pic x(5)
               value spaces.
           05 ws-invoice-num                pic x(9).
           05 filler                        pic x(3)
               value spaces.
           05 ws-SKU                        pic x(15).
           05 filler                        pic x(3)
               value spaces.
           05 ws-tax                        pic $zz,zz9.99.

      *Summary Lines
       01 ws-summary1-main.
           05 filler                        pic x(28)
               value spaces.
           05 filler                        pic x(22)
               value "RETURNS SUMMARY REPORT".
           05 filler                        pic x(37)
               value spaces.

      *Store Variables (Used as a Summary Line after main)
       01 ws-store-returns-table            occurs 6 times.
           05 filler                        pic x(36)
               value "Total Number of Returns for Store # ".
           05 ws-tbl-num                    pic 99.
           05 filler                        pic x(2)
               value ": ".
           05 ws-tbl-cnt                    pic 99
               value 0.
           05 ws-tbl-cnt-print              pic z9
               redefines ws-tbl-cnt.
           05 filler                        pic x(14)
               value spaces.
           05 filler                        pic x(21)
               value "Store Return Amount: ".
           05 ws-tbl-tot                    pic 9(5)v99
               value 0.
           05 ws-tbl-tot-print              pic $zz,zz9.99
               redefines ws-tbl-tot.

      *Total Store Returns Count + Amount Summary Line
       01 ws-summary3-totals.
           05 filler                        pic x(40)
               value "Total Number of Returns for All Stores: ".
           05 ws-ret-cnt                    pic z9.
           05 filler                        pic x(14)
               value spaces.
           05 filler                        pic x(21)
               value "Total Return Amount: ".
           05 ws-ret-tot                    pic $zz,zz9.99.

      *Total Tax Owed Us Summary Line
       01 ws-summary4-tax.
           05 filler                        pic x(56) 
               value spaces.
           05 filler                        pic x(21)
               value "Total Tax Owed Us  : ".
           05 ws-tax-us                     pic $zz,zz9.99.

      *Page and Line Variables
       77 ws-line-count                     pic 99 
           value 0.
       77 ws-page-count                     pic 99 
           value 0.
       77 ws-lines-per-page                 pic 99 
           value 20. 

      *Store Number Constants
       77 ws-store-num-literal              pic x(12) 
           value "010203040512".
       77 ws-store-nums                     pic x(2) occurs 6 times
           redefines ws-store-num-literal.
       77 ws-num-stores                     pic 9
           value 6.

      *Total Return Count + Amount
       77 ws-return-cnt                     pic 99
           value 0.
       77 ws-return-amount                  pic 9(5)v99
           value 0.

      *Tax Calculations
       77 ws-tax-calc                       pic 9(5)v99
           value 0.
       77 ws-tax-cnst                       pic 99
           value 13.
       77 ws-tax-tot                        pic 9(5)v99
           value 0.
       
      *General Constants
       77 ws-eof-flag                       pic x
           value "n". 
       77 ws-cnt                            pic 9
           value 0.


       procedure division.
       000-main.

      *    Open input and output files
           open input input-file,
                output output-file.

      *    Check to see if input file is empty
           read input-file 
               at end move 'y'              to ws-eof-flag.

      *    Move Store Numbers into the Store Table
           perform varying ws-cnt from 1 by 1
             until ws-cnt > ws-num-stores
               move ws-store-nums(ws-cnt)   to ws-tbl-num(ws-cnt)
           end-perform.
                                          
      *    Process records until end of file
           perform 100-process-pages
               varying ws-page-count from 1 by 1
               until   ws-eof-flag = 'y'.

      *    Print Summary Report Lines
           perform 400-print-summary-lines.

      *    Close input and output files
           close input-file,
                 output-file. 

      *    End of 000-main (end of ReturnsProcessing program)
           stop run. 


       100-process-pages.

      *    Print headers
           perform 150-print-headings.

      *    Detail lines + calculations performed until max lines per
      *    page limit is hit
           perform 200-process-lines
               varying ws-line-count from 1 by 1
               until ws-line-count > ws-lines-per-page 
                   or ws-eof-flag = 'y'.
           
      *    Start a new page unless last record
           if ws-eof-flag = 'n' then
               write output-line from spaces
                   after page
           end-if.


       150-print-headings.

      *    Clear Detail Line
           move spaces                      to ws-detail-line.
           move ws-page-count               to ws-header-page.

      *    Write the headers
           write output-line from ws-header1-main
               after advancing 1 lines
           write output-line from ws-header2-headings
               after advancing 2 lines.
           write output-line from ws-header3-headings
               after advancing 1 line.
           write output-line from ws-header4-underlines
               after advancing 1 line. 


       200-process-lines.

      *    Calculate Store Totals
           perform 300-store-calculations.

      *    Calculate Tax for each record
           compute ws-tax-calc rounded =
                   (il-amount * ws-tax-cnst) / 100.

      *    Add 1 to Total Return count + Add Amount to Total + Total Tax
           add 1                            to ws-return-cnt.
           add il-amount                    to ws-return-amount.
           add ws-tax-calc                  to ws-tax-tot. 

      *    Move raw input record data to detail line
           move il-code                     to ws-code.
           move il-amount                   to ws-amount.
           move il-type                     to ws-payment.
           move il-store-num                to ws-store-num.
           move il-invoice-num              to ws-invoice-num.
           move il-SKU                      to ws-SKU.
           move ws-tax-calc                 to ws-tax.

      *    Write Detail Line
           write output-line from ws-detail-line
               after advancing 1 line.

      *    Check for end of file.
           read input-file 
               at end move 'y'              to ws-eof-flag.


       300-store-calculations.

      *    Add 1 to the store counter and add total amount to store
           perform varying ws-cnt from 1 by 1
             until ws-cnt > ws-num-stores
               if ws-tbl-num(ws-cnt) = il-store-num then
                   add 1                    to ws-tbl-cnt(ws-cnt)
                   add il-amount            to ws-tbl-tot(ws-cnt)
               end-if
           end-perform.


       400-print-summary-lines.

      *    Write Summary Title
           write output-line from ws-summary1-main
               after advancing 2 lines.
      *    Write blank line    
           write output-line from spaces
               after advancing 1 line.

      *    Loops through the stores, moves and prints for each
           perform varying ws-cnt from 1 by 1
             until ws-cnt > ws-num-stores
      *        Move the raw data to edited pic
               move ws-tbl-cnt(ws-cnt)      to ws-tbl-cnt-print(ws-cnt)
               move ws-tbl-tot(ws-cnt)      to ws-tbl-tot-print(ws-cnt)
      *        Write the Summary Line for each store
               write output-line from ws-store-returns-table(ws-cnt) 
           end-perform.

      *    Move Total Store Count + Amount + Total Tax Amount
           move ws-return-cnt               to ws-ret-cnt.
           move ws-return-amount            to ws-ret-tot.
           move ws-tax-tot                  to ws-tax-us.

      *    Write Total Store Count + Amount Summary Line
           write output-line from ws-summary3-totals
               after advancing 1 line.
      *    Total Tax Owed Us Summary Line
           write output-line from ws-summary4-tax
               after advancing 2 lines.
           

       end program ReturnsProcessing.