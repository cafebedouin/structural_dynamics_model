#!/usr/bin/env python3
"""B0 blinded-packet builder (OQ-259 item 3, origin-stability measurement).

Reads the three pinned manifests (origin + two Arm-0 redraws), extracts every
candidate carrier item (each omega's `description` text; each manifest's
`fracture_scan.notes` text), strips ids/sources/positions, applies the pinned
redactions (see PREREGISTRATION.md), shuffles under the recorded seed, and writes:

  - PACKET.md            (committed BEFORE adjudication)
  - b0_mapping.json      (label -> (manifest, item) mapping; md5 pinned in the
                          prereg, file itself committed only AFTER the calls)

Deterministic: fixed seed, fixed input order. Re-running reproduces both outputs.
"""
import hashlib
import json
import random
import sys

SEED = 259  # recorded in PREREGISTRATION.md

MANIFESTS = {
    "ORIGIN": "audits/2026-08-03_kritik_ingest/biopower_k_nhi_debate_2026_20260803_102652.manifest.json",
    "RUN1": "audits/2026-08-05_oq259_emphasis_discriminator/biopower_healthcare_kernel_2026_20260805_144612.manifest.json",
    "RUN2": "audits/2026-08-05_oq259_emphasis_discriminator/biopower_nhi_debate_2026_20260805_144823.manifest.json",
}

# Pinned redactions (each verified scorable-after-redaction in the prereg):
REDACTIONS = [
    ("(NDI 2026)", "([tournament/institute identifier redacted])"),
    ("omega_non_western_traditions_absent", "[omega-id redacted]"),
    ("omega_reading_coherence_check", "[omega-id redacted]"),
    ("omega_historicist_reading_asymmetric_weight", "[omega-id redacted]"),
]


def redact(text):
    for old, new in REDACTIONS:
        text = text.replace(old, new)
    return text


def main(outdir, mapping_path):
    items = []  # (manifest_label, item_key, text)
    for label, path in MANIFESTS.items():
        d = json.load(open(path))
        for i, om in enumerate(d.get("omegas", [])):
            items.append((label, f"omega[{i}]:{om['id']}", redact(om["description"])))
        notes = (d.get("fracture_scan") or {}).get("notes", "")
        if notes:
            items.append((label, "fracture_scan.notes", redact(notes)))

    rng = random.Random(SEED)
    rng.shuffle(items)

    labels = [f"ITEM-{chr(ord('A') + i)}" for i in range(len(items))]
    mapping = {
        lab: {"manifest": m, "item": k} for lab, (m, k, _) in zip(labels, items)
    }
    with open(mapping_path, "w") as f:
        json.dump({"seed": SEED, "mapping": mapping}, f, indent=2, sort_keys=True)

    lines = ["# B0 Blinded Packet (OQ-259 item 3)", "",
             "Each item below is the verbatim text of one candidate carrier "
             "(an omega description or a fracture-scan note) from one of three "
             "manifests, with identifiers stripped and pinned redactions applied. "
             "Order is randomized.", ""]
    for lab, (_, _, text) in zip(labels, items):
        lines.append(f"## {lab}")
        lines.append("")
        lines.append(text)
        lines.append("")
    packet = "\n".join(lines)
    with open(f"{outdir}/PACKET.md", "w") as f:
        f.write(packet)

    print(f"items: {len(items)}")
    print(f"PACKET.md md5:      {hashlib.md5(packet.encode()).hexdigest()}")
    print(f"mapping file md5:   {hashlib.md5(open(mapping_path,'rb').read()).hexdigest()}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
