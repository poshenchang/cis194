# CIS 194: Introduction to Haskell - Homework Solutions

Welcome to the repository for homework solutions for the [CIS 194: Introduction to Haskell](https://www.seas.upenn.edu/~cis194/spring13/) course. According to the [official Haskell documentation](https://www.haskell.org/documentation/), CIS 194 is a thorough, practical guide for Haskell, guiding learners from the basics to advanced features of the language. This repository contains my solutions to the homework assignments of the course.

## Repository Structure

The repository is organized by homework assignments.

- `src/` - The source code for the solutions.
- `test/` - Test cases and scripts to verify the solutions.

In both directories `src/` and `test/`, source files are organized according to the week it was assigned (e.g., `Week01`, `Week02`, etc.).

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

2. Initialize Cabal environment:
    ```sh
    cabal configure --enable-test
    cabal build
    ```

3. Run the tests:
    ```sh
    cabal test
    ```
   Replace the source files in `src/` to test your own Haskell codes.

## Contributing

This repository is primarily for educational purposes, and contributions are welcome! If you have an alternative solution or improvements, feel free to open a pull request.

## License

This project is licensed under the MIT License.
