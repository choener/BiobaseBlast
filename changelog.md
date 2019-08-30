0.3.1.0
-------

- Blast datatype supports optional taxonomy id

0.3.0.1
-------

- Fix for newly ambigous ? operator export from attoparsec and aeson>=1.4.4.0 
  in Import module 

0.3.0.0
-------

- generalized IO-based functions to MonadIO. This should be non-breaking in
  most cases.
- Added parsing functionality for JSON2 NCBI BLAST+ output
- currently tags all DNA/AA letters with ()

0.2.0.0
-------

- Added parsing functionality for tabular NCBI BLAST+ output

0.1.0.0
-------

- more efficient tables (using PrimitiveArray)
- updated to newest PrimitiveArray & BiobaseXNA
- travis.yml added

0.0.0.1
-------

- initial commit
