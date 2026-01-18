import subprocess
import os
import google.generativeai as genai
from anthropic import Anthropic

# --- SYSTEM SETUP ---
PROLOG_EXECUTABLE = "swipl"
HARNESS_FILE = "test_harness.pl"
TEMP_DATA_FILE = "automated_audit_data.pl"

class DRAuditOrchestrator:
    def __init__(self, gemini_key, claude_key):
        genai.configure(api_key=gemini_key)
        self.claude = Anthropic(api_key=claude_key)
        self.gemini = genai.GenerativeModel('gemini-1.5-pro')

    def pre_scan_omegas(self, source_text):
        """Step 1: Identify Reasoning Blockers (Omegas) before code generation."""
        prompt = f"""Analyze this text for Deferential Realism (DR) Omega Variables:
        - Empirical (missing data)
        - Conceptual (ambiguous terms)
        - Preference (value judgments)

        TEXT: {source_text}
        Output ONLY a list of omega_variable(ID, Type, Description) predicates."""

        response = self.gemini.generate_content(prompt)
        return response.text

    def generate_dataset(self, source_text, omegas, interval_id):
        """Step 2: Generate the full Prolog dataset using the predefined Omegas."""
        with open("prompt.md", "r") as f:
            system_prompt = f.read()

        full_prompt = f"{system_prompt}\n\nUSE THESE OMEGAS:\n{omegas}\n\nSOURCE TEXT:\n{source_text}\nINTERVAL_ID: {interval_id}"

        # Use Claude for high-precision Prolog syntax
        message = self.claude.messages.create(
            model="claude-3-5-sonnet-20240620",
            max_tokens=4000,
            messages=[{"role": "user", "content": full_prompt}]
        )
        return message.content[0].text

    def run_prolog_audit(self, interval_id):
        """Step 3 & 4: Execute the Prolog validation and catch specific DRL errors."""
        # Use -q for quiet and -g to call the test harness
        cmd = [
            PROLOG_EXECUTABLE, "-q", "-t", "halt", "-g",
            f"consult('{HARNESS_FILE}'), run_all_tests({interval_id}), halt."
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        return result.stdout

    def run_audit_cycle(self, topic_text, interval_id):
        print(f"[*] Starting DR-Audit for {interval_id}...")

        # 1. Pre-Scan Omegas
        omegas = self.pre_scan_omegas(topic_text)

        # 2. Initial Generation
        prolog_code = self.generate_dataset(topic_text, omegas, interval_id)

        # Clean markdown wrappers if present
        prolog_code = prolog_code.replace("```prolog", "").replace("```", "")

        with open(TEMP_DATA_FILE, "w") as f:
            f.write(prolog_code)

        # 3. Validate and Refine (Loop if necessary)
        logs = self.run_prolog_audit(interval_id)

        if "! ALERT" in logs or "ERROR" in logs:
            print("[!] Ontological Fraud or Schema Error detected. Refining...")
            # Extract error lines to provide to the LLM for correction
            error_feedback = "\n".join([l for l in logs.split("\n") if "ALERT" in l or "ERROR" in l])
            # Re-run generate_dataset with error_feedback as a correction prompt...
            # (Loop logic would go here)

        print("[*] Final Executive Summary Generated:")
        print(logs)

# --- INITIATION ---
# orchestrator = DRAuditOrchestrator(GEMINI_KEY, CLAUDE_KEY)
# orchestrator.run_audit_cycle(source_document_text, "audit_2026_chernobyl")
