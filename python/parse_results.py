import re
import json
import os

def parse_validation_log(log_path, output_json):
    with open(log_path, 'r') as f:
        content = f.read()

    # Split the log by domain entries
    entries = re.split(r'\[\d+\] DOMAIN:', content)
    structured_data = {}

    for entry in entries[1:]:
        # Extract Domain Name and Path
        header_match = re.search(r'^\s*(\S+)\s+\(([^)]+)\)', entry)
        if not header_match: continue

        domain_id = header_match.group(1)
        file_path = header_match.group(2)

        # Initialize record
        record = {
            "path": file_path,
            "repaired_vectors": [],
            "ontological_fraud": [],
            "omegas": [],
            "perspectival_gaps": False,
            "kappas": {}
        }

        # 1. Capture Repaired Vectors (Manual or Imputed)
        repaired = re.findall(r'\[FIXED\] Imputed ([\d\.]+) for ([^ ]+) at T=([\d\.]+)', entry)
        for val, metric, time in repaired:
            record["repaired_vectors"].append({"metric": metric, "t": time, "val": val})

        # 2. Capture Ontological Fraud (Schema/Validation Errors)
        fraud = re.findall(r'ERROR: Invalid ([^\n]+)', entry)
        record["ontological_fraud"].extend(fraud)

        # 3. Capture Omegas (Irreducible Uncertainties)
        omegas = re.findall(r'omega_variable\(\s*([^,]+),\s*"([^"]+)"', entry)
        for o_id, description in omegas:
            record["omegas"].append({"id": o_id, "desc": description})

        # 4. Check for Perspectival Gaps (Type1 \= Type2 in tests)
        if "perspectival_gap" in entry and "Passed" in entry:
            record["perspectival_gap"] = True

        # 5. Capture Kappas/Confidence (if printed in your feedback generator)
        # Note: Adjust regex based on your report_generator:generate_llm_feedback format
        kappas = re.findall(r'confidence_without_resolution\(([^)]+)\)', entry)
        if kappas:
            record["kappas"]["default"] = kappas[0]

        structured_data[domain_id] = record

    with open(output_json, 'w') as f:
        json.dump(structured_data, f, indent=4)

    return structured_data

if __name__ == "__main__":
    parse_validation_log("../outputs/output.txt", "../outputs/structured_analysis.json")
    print("✓ Analysis complete: outputs/structured_analysis.json generated.")
