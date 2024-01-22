import PyPDF2
import os

# Define the directory and the PDF files to merge
your_directory = 'yourDirectory'
pdf_files = ['exampleFile1.pdf', 'exampleFile2.pdf', 'exampleFile3.pdf']
output_file_name = 'exampleMergedFileName.pdf'

# Change the working directory
os.chdir(your_directory)

# Initialize PdfWriter
writer = PyPDF2.PdfWriter()

# Loop through each PDF file
for pdf_file in pdf_files:
    with open(pdf_file, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        for page in reader.pages:
            writer.add_page(page)

# Write the merged PDF
with open(output_file_name, 'wb') as output_file:
    writer.write(output_file)
