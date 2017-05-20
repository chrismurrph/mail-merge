# mail-merge
Generating many PDFs based on a letter

Really three small applications in here:
1. Label printing that uses iText directly. Can be used for addressing envelopes
2. Generating lots of PDF letters from a list of recipients
3. Generating a CV from edn and text files

I'll want to do signing with an image (not digital signing), so leaving this here:

dotemacs [9:39 PM]
Jump
@cjmurphy see this: https://svn.apache.org/viewvc/pdfbox/trunk/examples/src/main/java/org/apache/pdfbox/examples/pdmodel/AddImageToPDF.java?view=markup it shows you how to do just that. Aaaand when you do it, please consider creating a PR so that the functionality can be in `pdfboxing`. :slightly_smiling_face:

The reason it isn’t there yet, is because I never needed to do this so I never ported it. Hope it helps

xiongtx [12:08 PM]
Jump
cjmurphy: Java interop? https://pdfbox.apache.org/
