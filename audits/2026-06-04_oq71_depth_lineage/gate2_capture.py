#!/usr/bin/env python3
"""Step-2 regression gate harness (OQ-71): capture the batch-request payloads
run_no_scope constructs, WITHOUT submitting to the API.

Stubs get_client(); the fake client's messages.batches.create records the
requests and raises CaptureDone before any network call. Run with identical
toy seeds pre-change and post-change (flag-off) for byte-identity, and
post-change flag-on for payload-invariance (routing-only change).

Usage: python3 audits/2026-06-04_oq71_depth_lineage/gate2_capture.py <label> [--run-tag TAG]
Writes audits/2026-06-04_oq71_depth_lineage/gate2_<label>.json
"""
import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(REPO_ROOT))

import agent.generate_kernel_corpus as g  # noqa: E402


class CaptureDone(Exception):
    pass


class FakeBatches:
    def __init__(self, sink):
        self.sink = sink

    def create(self, requests):
        self.sink["requests"] = requests
        raise CaptureDone()


class FakeClient:
    def __init__(self, sink):
        self.messages = type("M", (), {"batches": FakeBatches(sink)})()


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("label")
    ap.add_argument("--run-tag", default=None)
    args = ap.parse_args()

    toy = [{
        "constraint_id": "oq71_routing_gate_toy",
        "kernel_id": "oq71_gate_kernel",
        "reading_id": "toy_reading",
        "human_readable": "Routing-gate toy seed (never generated)",
        "topic_domain": "test",
        "family_id": "oq71_gate",
        "sibling_reading_ids": ["other_reading"],
        "expected_structural_delta": "none",
        "summary": "Fixed toy summary for payload capture.",
    }]
    seeds_path = REPO_ROOT / "audits" / "2026-06-04_oq71_depth_lineage" / "gate2_toy_seeds.json"
    seeds_path.write_text(json.dumps(toy, indent=2), encoding="utf-8")

    sink = {}
    g.get_client = lambda: FakeClient(sink)
    g._client = None

    ns = argparse.Namespace(seeds=str(seeds_path), n=0, poll_interval=1)
    if hasattr(g, "run_dirs") and args.run_tag is not None:
        ns.run_tag = args.run_tag
    else:
        ns.run_tag = None

    try:
        g.run_no_scope(ns)
    except CaptureDone:
        pass

    out = REPO_ROOT / "audits" / "2026-06-04_oq71_depth_lineage" / f"gate2_{args.label}.json"
    out.write_text(json.dumps(sink.get("requests"), indent=2, sort_keys=True),
                   encoding="utf-8")
    print(f"captured {len(sink.get('requests', []))} request(s) -> {out}")


if __name__ == "__main__":
    main()
