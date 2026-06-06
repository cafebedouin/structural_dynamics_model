"""Capture generate_from_manifests's assembled payloads (NEW path) under a fixed fake client,
with c-orchestrator's request defaults, so it can be byte-diffed against the OLD baseline."""
import sys, json, copy
from types import SimpleNamespace as NS
sys.path.insert(0, '.')
import importlib.util
gspec = importlib.util.spec_from_file_location('gkc', 'agent/generate_kernel_corpus.py')
gkc = importlib.util.module_from_spec(gspec); gspec.loader.exec_module(gkc)
import agent.story_generator_base as sgb

CANNED = json.load(open('json/manpower_exhaustion_trap.json'))
captured = []
class FakeBatches:
    def __init__(self): self._r = {}
    def create(self, requests):
        captured.append([{"custom_id": r["custom_id"], "params": r["params"]} for r in requests])
        out = []
        for r in requests:
            tail = r["params"]["messages"][0]["content"][1]["text"]
            cids = [l.split(": ",1)[1] for l in tail.splitlines() if l.startswith("CONSTRAINT: ")]
            cid = cids[0] if cids else r["custom_id"]
            s = copy.deepcopy(CANNED); s["header"]["constraint_id"] = cid
            out.append(NS(custom_id=r["custom_id"], result=NS(type="succeeded",
                message=NS(content=[NS(type="text", text=json.dumps(s))],
                           usage=NS(input_tokens=1, output_tokens=1), stop_reason="end_turn"))))
        bid="b"+str(len(self._r)); self._r[bid]=out
        return NS(id=bid, processing_status="ended", request_counts=NS(succeeded=len(out),errored=0,processing=0))
    def retrieve(self,b): return NS(id=b,processing_status="ended",request_counts=NS(succeeded=len(self._r[b]),errored=0,processing=0,canceled=0,expired=0))
    def results(self,b): return iter(self._r[b])
gkc.get_client = lambda: NS(messages=NS(batches=FakeBatches()))

from pathlib import Path
jd = Path("json/_new_cap_tmp"); td = Path("prolog/testsets/_new_cap_tmp"); md = Path("outputs/kernel_manifests/_new_cap_tmp")
for d in (jd, td, md): d.mkdir(parents=True, exist_ok=True)
manifest = json.load(open(sys.argv[1]))
import contextlib, io
with contextlib.redirect_stdout(io.StringIO()):
    gkc.generate_from_manifests([manifest], jd, td, md/"processed.txt",
        model="claude-sonnet-4-5-20250929", max_tokens=16384,
        system=sgb._SYSTEM_INSTRUCTION, temperature=0.2, progress=lambda *a, **k: None)
with open(sys.argv[2], "w") as fh:
    json.dump(captured, fh, indent=1, sort_keys=True, ensure_ascii=False)
