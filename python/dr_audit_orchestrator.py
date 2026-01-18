import subprocess
import os
import re
import google.generativeai as genai
from anthropic import Anthropic

# --- CONFIGURATION ---
PROLOG_PATH = "swipl" # Path to your SWI-Prolog executable
HARNESS_FILE = "test_harness.pl"
OUTPUT_DATA_FILE = "generated_dataset.pl"
MAX_RETRIES = 3

# Initialize APIs (Use your keys here)
genai.configure(api_key="YOUR_GEMINI_API_KEY")
anthropic = Anthropic(api_key="YOUR_CLAUDE_API_KEY")

def call_llm(prompt, model_type="gemini"):
    """Fetches Prolog data from the chosen LLM."""
    if model_type == "gemini":
        model = genai.GenerativeModel('gemini-1.5-pro')
        response = model.generate_content(prompt)
        return response.text
    elif model_type == "claude":
        message = anthropic.messages.create(
            model="claude-3-5-sonnet-20240620",
            max_tokens=4096,
            messages=[{"role": "user", "content": prompt}]
        )
        return message.content[0].text

def run_prolog_validation(interval_id):
    """
    Executes the Prolog test harness.
    Returns (Success Bool, Output String)
    """
    # Command: Load harness, run all tests for the interval, and halt.
    cmd = [
        PROLOG_PATH,
        "-q", "-g",
        f"consult('{HARNESS_FILE}'), run_all_tests({interval_id}), halt.",
        "-t", "halt(1)."
    ]

    result = subprocess.run(cmd, capture_output=True, text=True)
    return "ERROR" not in result.stdout and "[FAIL]" not in result.stdout, result.stdout

def clean_prolog_code(raw_text):
    """Extracts code blocks from LLM markdown response."""
    code_blocks = re.findall(r"```prolog\n(.*?)\n```", raw_text, re.DOTALL)
    return "\n".join(code_blocks) if code_blocks else raw_text

def orchestrate_audit(domain_topic, interval_id):
    print(f"\n[1] Initiating Audit for: {domain_topic}")

    # Load your prompt.md instructions
    with open("prompt.md", "r") as f:
        system_instructions = f.read()

    current_prompt = f"{system_instructions}\n\nTOPIC: {domain_topic}\nINTERVAL_ID: {interval_id}"

    for attempt in range(MAX_RETRIES):
        print(f"[Attempt {attempt + 1}] Generating Dataset...")
        raw_response = call_llm(current_prompt, model_type="gemini")
        prolog_code = clean_prolog_code(raw_response)

        # Write to file for Prolog to ingest
        with open(OUTPUT_DATA_FILE, "w") as f:
            f.write(prolog_code)

        # Validate using your stack
        success, logs = run_prolog_validation(interval_id)

        if success:
            print("[SUCCESS] Dataset validated and audited.")
            print(logs) # This contains your Executive Summary
            return True
        else:
            print("[!] Validation failed. Initiating self-correction loop...")
            # Extract the error messages to feed back to the LLM
            errors = "\n".join([line for line in logs.split('\n') if "ERROR" in line or "[FAIL]" in line])
            current_prompt = f"Your previous Prolog output had errors:\n{errors}\n\nPlease fix the predicates and ensure arity matches narrative_ontology.pl."

    print("[FAILURE] Could not generate a valid dataset after maximum retries.")
    return False

# --- EXECUTION ---
if __name__ == "__main__":
    # Example: Auditing a new domain
    orchestrate_audit("The 1986 Chernobyl Exclusion Zone Logic", "chernobyl_audit")
