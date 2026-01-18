import subprocess
import os
import re
import json

# --- COMPONENT 1: NARRATIVE CHUNKING HANDLER ---
class NarrativeContextHandler:
    def __init__(self, raw_text):
        self.raw_text = raw_text
        self.intervals = []

    def chunk_by_narrative(self, anchor_pattern=r"(Chapter \d+|Section \d+|ARTICLE [IVX]+)"):
        """Chunks document by structural headers to preserve ontological coherence."""
        splits = [m.start() for m in re.finditer(anchor_pattern, self.raw_text)]
        if not splits:
            splits = [0, len(self.raw_text) // 2]
        splits.append(len(self.raw_text))

        for i in range(len(splits) - 1):
            chunk = self.raw_text[splits[i]:splits[i+1]]
            interval_id = f"audit_interval_{i}"
            # Maps to Prolog integer requirements for Start and End
            self.intervals.append({
                "id": interval_id,
                "text": chunk.strip(),
                "t_range": (i * 10, (i + 1) * 10)
            })
        return self.intervals

# --- COMPONENT 2: THE BATCH RUNNER ---
class DRBatchRunner:
    def __init__(self, orchestrator):
        self.orchestrator = orchestrator
        self.unified_audit_log = []
        self.master_prolog_db = "unified_audit_database.pl"

    def run_full_document_audit(self, document_text):
        print("[*] Initiating Narrative-Driven Batch Audit...")

        # 1. Chunking
        handler = NarrativeContextHandler(document_text)
        narrative_intervals = handler.chunk_by_narrative()

        with open(self.master_prolog_db, "w") as master_file:
            master_file.write(":- style_check(-discontiguous).\n") # Prevent Prolog warnings

            for interval in narrative_intervals:
                print(f"\n>>> PROCESSING: {interval['id']} (T:{interval['t_range'][0]}-{interval['t_range'][1]})")

                # 2. Run the Audit Cycle (Omega Scan -> Generation -> Validation)
                # This leverages dr_mismatch/3 and verify_all/0
                success, report_output, prolog_data = self.orchestrator.run_audit_cycle(
                    interval['text'],
                    interval['id']
                )

                # 3. Aggregate results
                self.unified_audit_log.append({
                    "interval": interval['id'],
                    "success": success,
                    "summary": report_output
                })

                # Save facts to unified DB
                master_file.write(f"\n% --- DATA FOR {interval['id']} ---\n")
                master_file.write(prolog_data + "\n")

        self.generate_final_report()

    def generate_final_report(self):
        print("\n" + "="*50)
        print("   UNIFIED DOCUMENT AUDIT COMPLETE")
        print("="*50)
        for entry in self.unified_audit_log:
            status = "[OK]" if entry['success'] else "[FAILED]"
            print(f"{status} {entry['interval']}")

        # Save JSON log for further analysis (optional)
        with open("audit_results.json", "w") as f:
            json.dump(self.unified_audit_log, f, indent=4)
        print(f"\n[*] Unified facts saved to: {self.master_prolog_db}")
