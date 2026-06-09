"""
OQ-80 witness — token_acc threading through process_batch_results.

Before the fix, the unified backend (generate_from_manifests -> process_batch_results)
discarded batch-result usage, so c-orchestrator._step_generate reported tokens_in/out=0:
absence presenting as a measured zero (Build-Discipline spine). The fix threads an
optional mutable token_acc out-param; this test witnesses, against a MOCKED batch
results iterator (no API spend):

  1. usage from succeeded results is summed into token_acc;
  2. accumulation happens AT RECEIPT — a result whose body fails JSON parse still
     contributes its usage (the spend is real even when the story is rejected);
  3. errored results (no message) contribute nothing;
  4. backward compatibility: calling without token_acc still works (default None).

Run: python3 python/tests/test_token_acc_threading.py
"""

import sys
import tempfile
from pathlib import Path
from types import SimpleNamespace

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from agent.generate_kernel_corpus import process_batch_results  # noqa: E402


def _succeeded_result(custom_id, text, tin, tout):
    return SimpleNamespace(
        custom_id=custom_id,
        result=SimpleNamespace(
            type="succeeded",
            message=SimpleNamespace(
                usage=SimpleNamespace(input_tokens=tin, output_tokens=tout),
                content=[SimpleNamespace(type="text", text=text)],
            ),
        ),
    )


def _errored_result(custom_id):
    return SimpleNamespace(custom_id=custom_id,
                           result=SimpleNamespace(type="errored"))


class _MockClient:
    """Mimics client.messages.batches.results(batch_id) -> iterator of results."""

    def __init__(self, results):
        self.messages = SimpleNamespace(
            batches=SimpleNamespace(results=lambda _batch_id: iter(results)))


def _run(results, token_acc):
    with tempfile.TemporaryDirectory() as td:
        tdp = Path(td)
        (tdp / "json").mkdir()
        (tdp / "testsets").mkdir()
        return process_batch_results(
            _MockClient(results), "batch_mock", tdp / "json", tdp / "testsets",
            tdp / "processed.txt", token_acc=token_acc)


def test_usage_summed_including_parse_failures():
    results = [
        # Both bodies are invalid stories (parse/validation fails) — by design:
        # tokens were SPENT regardless, so they must still be counted (at-receipt).
        _succeeded_result("a", "not json at all", tin=100, tout=10),
        _succeeded_result("b", "{also broken", tin=250, tout=33),
        _errored_result("c"),  # no message -> no usage contribution
    ]
    acc = {"input_tokens": 0, "output_tokens": 0}
    _run(results, acc)
    assert acc == {"input_tokens": 350, "output_tokens": 43}, f"got {acc}"
    print(f"PASS: usage summed at receipt incl. parse-failures, errored excluded: {acc}")


def test_negative_control_zero_when_no_usage():
    # Positive control for the absence claim: with ONLY an errored result the
    # accumulator must stay 0 — proves test 1's nonzero totals came from the
    # mocked usage, not from a default.
    acc = {"input_tokens": 0, "output_tokens": 0}
    _run([_errored_result("only")], acc)
    assert acc == {"input_tokens": 0, "output_tokens": 0}, f"got {acc}"
    print("PASS: errored-only batch leaves accumulator at 0")


def test_backward_compatible_without_token_acc():
    out = _run([_succeeded_result("a", "not json", 5, 5)], token_acc=None)
    assert isinstance(out, tuple) and len(out) == 4, f"got {out!r}"
    print("PASS: token_acc=None path unchanged (4-tuple returned, no crash)")


if __name__ == "__main__":
    test_usage_summed_including_parse_failures()
    test_negative_control_zero_when_no_usage()
    test_backward_compatible_without_token_acc()
