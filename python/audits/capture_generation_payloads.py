"""Capture _step_generate_batch's assembled request payloads (PURE, to a file).
Usage: capture_payloads.py <manifest.json> <out.json>
Pins generation output via a fixed canned story so wave-2+ upstream_context is deterministic.
Writes ONLY the per-wave request list (custom_id + messages) — no save/lint side effects in the
artifact — so old==new diffs compare payloads, not processing noise."""
import sys, json, copy, os, contextlib, io
from types import SimpleNamespace as NS
sys.path.insert(0, '.')
import importlib.util
spec = importlib.util.spec_from_file_location('co', 'agent/c-orchestrator.py')
co = importlib.util.module_from_spec(spec); spec.loader.exec_module(co)
CANNED = json.load(open('json/manpower_exhaustion_trap.json'))
captured = []
class FakeBatches:
    def __init__(self): self._r = {}
    def create(self, requests):
        captured.append([{"custom_id": r["custom_id"], "messages": r["params"]["messages"]} for r in requests])
        out = []
        for r in requests:
            tail = r["params"]["messages"][0]["content"][1]["text"]
            cids = [l.split(": ",1)[1] for l in tail.splitlines() if l.startswith("CONSTRAINT: ")]
            cid = cids[0] if cids else r["custom_id"]
            s = copy.deepcopy(CANNED); s["header"]["constraint_id"] = cid
            out.append(NS(custom_id=r["custom_id"], result=NS(type="succeeded",
                message=NS(content=[NS(type="text", text=json.dumps(s))],
                           usage=NS(input_tokens=1, output_tokens=1), stop_reason="end_turn"))))
        bid = "b"+str(len(self._r)); self._r[bid] = out
        return NS(id=bid, processing_status="ended", request_counts=NS(succeeded=len(out), errored=0, processing=0))
    def retrieve(self, bid): return NS(id=bid, processing_status="ended", request_counts=NS(succeeded=len(self._r[bid]), errored=0, processing=0, canceled=0, expired=0))
    def results(self, bid): return iter(self._r[bid])
co._anthropic_client = NS(messages=NS(batches=FakeBatches()))
manifest = json.load(open(sys.argv[1]))
o = co.DRAuditOrchestrator(run_tag="_paycap_tmp", skip_search=True, skip_corpus_update=True, skip_essay=True)
o._progress = lambda *a, **k: None
with contextlib.redirect_stdout(io.StringIO()):   # swallow process_batch_results save/lint noise
    o._step_generate_batch(manifest)
with open(sys.argv[2], "w") as fh:
    json.dump(captured, fh, indent=1, sort_keys=True, ensure_ascii=False)
