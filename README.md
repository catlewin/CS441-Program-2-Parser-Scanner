# CS441-Program-2-Parser-Scanner
This is a parser for a simple grammar written in Racket through prompting AI models.


**Output**


    File 1:
    A = 6; 
    B = 5; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    endif; 
    write A + B - X; 
    $$ 

<img width="574" alt="Screen Shot 2025-03-31 at 4 14 30 PM" src="https://github.com/user-attachments/assets/f18bce7e-7cb9-47c6-a72c-3bf7f6a5556f" />


    File 2
    A = 6; 
    B = 5; 
    X = -2 
    if (A + B > 10)
       X = A + 2;
    endif; 
    write A + B - X; 
    $$

<img width="427" alt="Screen Shot 2025-03-31 at 4 14 44 PM" src="https://github.com/user-attachments/assets/c8a8737b-a459-4278-9567-6de0e19ccbe9" />


    File 3
    A = 6; 
    read B; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    write A + B - X; 
     $$ 

<img width="612" alt="Screen Shot 2025-03-31 at 4 14 55 PM" src="https://github.com/user-attachments/assets/69bf918c-1fb0-4c10-9a0d-32a72fb34526" />


    File 4
    A = 6; 
    B# = 5; 
    X = -2; 
    if (A + B > 10)
       X = A + 2;
    endif;
    write A + B - X; 

<img width="352" alt="Screen Shot 2025-03-31 at 4 15 14 PM" src="https://github.com/user-attachments/assets/6f39e794-e626-4079-a7a4-09f80b83990e" />
