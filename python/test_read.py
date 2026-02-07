import sys

def main():
    print("Testing read...")
    sys.stdout.flush()
    lint_errors_file = 'outputs/lint_errors.txt'
    try:
        with open(lint_errors_file, 'r') as f:
            for line in f:
                print(line.strip())
                sys.stdout.flush()
    except Exception as e:
        print(f"Error reading file: {e}")
        sys.stdout.flush()
    print("Finished reading.")
    sys.stdout.flush()

if __name__ == "__main__":
    main()
