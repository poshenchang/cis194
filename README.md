# CIS 194: Introduction to Haskell - Homework Solutions

Welcome to the repository for homework solutions for the [CIS 194: Introduction to Haskell](https://www.seas.upenn.edu/~cis194/spring13/) course. According to the [official Haskell documentation](https://www.haskell.org/documentation/), CIS 194 is a thorough, practical guide for Haskell beginners. This repository contains my solutions to the homework assignments of the course.

## Repository Structure

The repository is organized by homework assignments. Assignments are organized according to the week it was assigned (e.g., `hw01`, `hw02`, etc.). Within each directory, you will find the following files:

- `README.md` - A brief description of the homework assignment.
- `src/` - The source code for the solutions.
- `test/` - Test cases and scripts to verify the solutions.

## How to Use

To view the solutions, navigate to the directory of the respective homework assignment. Each directory contains the source code and any necessary documentation.

### Requirements

To run the Haskell code in this repository, you will need:

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- [Cabal](https://www.haskell.org/cabal/)

### Running the Code

To compile and run the Haskell code, follow these steps:

1. Clone the repository:
    ```sh
    git clone https://github.com/poshenchang/cis194-hw-solutions.git
    cd cis194-hw-solutions
    ```

2. Navigate to the desired homework directory:
    ```sh
    cd hw01
    ```

3. Compile and run the solution:
    ```sh
    ghc -o solution src/Main.hs
    ./solution
    ```

## Contributing

This repository is primarily for educational purposes, and contributions are welcome! If you have an alternative solution or improvements, feel free to open a pull request.

## License

This project is licensed under the MIT License.
