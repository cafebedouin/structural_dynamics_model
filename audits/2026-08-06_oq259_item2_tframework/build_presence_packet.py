#!/usr/bin/env python3
"""Blinded kernel-presence packet builder (OQ-259 item 2, Part C presence clause).

Four items: the three T Framework same-input draws' kernel_description texts plus one
planted different-subject control (AT Fiat's kernel_description — same broad domain,
genuinely different contested commitment). Ids stripped, order randomized (seed
recorded). Writes PRESENCE_PACKET.md + mapping (withheld until calls commit).
Instrument-validity rule (pinned): the adjudicator's calls are usable ONLY if the
planted control is called DIFFERENT from every T Framework item; a call sheet that
groups the plant with the others is an instrument failure, not a presence verdict.
"""
import hashlib
import json
import random
import sys

SEED = 2592

SOURCES = [
    ("TF_RUN1", "audits/2026-08-06_oq259_item2_tframework/policy_debate_framework_2026_20260806_182706.manifest.json"),
    ("TF_RUN2", "audits/2026-08-06_oq259_item2_tframework/policy_debate_topicality_framework_20260806_182916.manifest.json"),
    ("TF_RUN3", "audits/2026-08-06_oq259_item2_tframework/framework_bcfpml_2026_20260806_183116.manifest.json"),
    ("PLANT_ATFIAT", "audits/2026-08-06_oq264_kredraw_variance/fiat_efficacy_kernel_2026_20260806_142314.manifest.json"),
]


def main(outdir, mapping_path):
    items = []
    for label, path in SOURCES:
        d = json.load(open(path))
        items.append((label, d["commitment_system_recognition"]["kernel_description"]))
    rng = random.Random(SEED)
    rng.shuffle(items)
    labels = [f"ITEM-{c}" for c in "PQRS"]
    mapping = {lab: src for lab, (src, _) in zip(labels, items)}
    with open(mapping_path, "w") as f:
        json.dump({"seed": SEED, "mapping": mapping}, f, indent=2, sort_keys=True)
    lines = ["# Blinded kernel-presence packet (Part C presence clause)", "",
             "Each item is one kernel_description text, identifiers stripped, order "
             "randomized.", ""]
    for lab, (_, text) in zip(labels, items):
        lines += [f"## {lab}", "", text, ""]
    packet = "\n".join(lines)
    with open(f"{outdir}/PRESENCE_PACKET.md", "w") as f:
        f.write(packet)
    print(f"PRESENCE_PACKET.md md5: {hashlib.md5(packet.encode()).hexdigest()}")
    print(f"mapping md5:            {hashlib.md5(open(mapping_path,'rb').read()).hexdigest()}")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
