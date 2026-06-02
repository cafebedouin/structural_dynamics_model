#!/usr/bin/env python3
"""Sum actual token usage of a completed Anthropic message batch and estimate its cost.

Usage:
    python3 python/batch_cost.py --batch-id msgbatch_xxx --model sonnet
    python3 python/batch_cost.py --batch-id msgbatch_xxx --model haiku

Prices are batch-tier (50% of standard), per million tokens. Cache-read is 0.1x and
cache-write 1.25x of the base input rate. Estimates only — confirm against the console.
"""
import argparse
import anthropic

# (input, output, cache_write, cache_read) USD per MTok, BATCH tier (= 50% of standard)
PRICES = {
    "sonnet": (1.50, 7.50, 1.875, 0.15),   # claude-sonnet-4-5
    "haiku":  (0.50, 2.50, 0.625, 0.05),   # claude-haiku-4-5
}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--batch-id", required=True)
    ap.add_argument("--model", required=True, choices=list(PRICES))
    args = ap.parse_args()

    client = anthropic.Anthropic()
    inp = out = cw = cr = 0
    n_ok = n_fail = 0
    for r in client.messages.batches.results(args.batch_id):
        if r.result.type != "succeeded":
            n_fail += 1
            continue
        n_ok += 1
        u = r.result.message.usage
        inp += u.input_tokens or 0
        out += u.output_tokens or 0
        cw += getattr(u, "cache_creation_input_tokens", 0) or 0
        cr += getattr(u, "cache_read_input_tokens", 0) or 0

    pi, po, pcw, pcr = PRICES[args.model]
    cost = (inp * pi + out * po + cw * pcw + cr * pcr) / 1_000_000
    print(f"batch {args.batch_id} ({args.model}): ok={n_ok} fail={n_fail}")
    print(f"  input={inp:,}  output={out:,}  cache_write={cw:,}  cache_read={cr:,}")
    print(f"  est. batch-tier cost: ${cost:.4f}  "
          f"(${cost/max(n_ok,1):.4f}/request over {n_ok} ok)")
    return cost, n_ok


if __name__ == "__main__":
    main()
