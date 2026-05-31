"""
ab_test/score.py — A/B test scoring for threshold-stripping experiment.

Usage (2-arm):
    python3 ab_test/score.py <arm_a_json_dir> <arm_b_json_dir>

Usage (3-arm with noise floor):
    python3 ab_test/score.py <arm_a_json_dir> <arm_b_json_dir> <arm_a2_json_dir>

  arm_a and arm_b are the experimental arms (sighted vs stripped).
  arm_a2 is a second sighted run (same settings as arm_a) used as a noise-floor
  control: A vs A′ delta should be ~0 at temp=0; if A vs A′ ≈ A vs B, the strip
  signal is in the noise.

Primary output: paired per-constraint delta tables.
Secondary: BPI per metric and KS distance (batch-only — not interpretable at n≤10).

CAVEAT: n=3 throughout. BPI values land in {0, .33, .67, 1.0}. KS is nearly content-free.
Do not draw conclusions from aggregate statistics. Read the paired delta tables instead.
"""
import json
import pathlib
import sys

CUTPOINTS = {
    "extractiveness":          [0.10, 0.25, 0.30, 0.45, 0.46, 0.70],
    "suppression":             [0.05, 0.40, 0.60],
    "theater_ratio":           [0.70],
    "accessibility_collapse":  [0.85],
    "resistance":              [0.15],
}
BPI_WINDOW = 0.03
METRICS = list(CUTPOINTS.keys())


def load_stories(dirpath: pathlib.Path) -> dict[str, dict]:
    stories = {}
    for f in sorted(dirpath.glob("*.json")):
        data = json.loads(f.read_text())
        cid = data.get("header", {}).get("constraint_id") or f.stem
        stories[cid] = data
    return stories


def get_metric(story: dict, field: str) -> float | None:
    bp = story.get("base_properties", {})
    val = bp.get(field)
    if val is None:
        return None
    try:
        return float(val)
    except (TypeError, ValueError):
        return None


def fmt(v) -> str:
    if isinstance(v, float):
        return f"{v:.3f}"
    return str(v)[:7]


def bpi_for_value(value: float, field: str) -> bool:
    return any(abs(value - cp) <= BPI_WINDOW for cp in CUTPOINTS.get(field, []))


def bpi_arm(stories: dict) -> dict[str, float]:
    counts = {m: {"near": 0, "total": 0} for m in METRICS}
    for story in stories.values():
        for m in METRICS:
            v = get_metric(story, m)
            if v is not None:
                counts[m]["total"] += 1
                if bpi_for_value(v, m):
                    counts[m]["near"] += 1
    result = {}
    for m in METRICS:
        t = counts[m]["total"]
        result[m] = counts[m]["near"] / t if t > 0 else float("nan")
    return result


def ks_distance(vals_a: list[float], vals_b: list[float]) -> float:
    if not vals_a or not vals_b:
        return float("nan")
    all_pts = sorted(set(vals_a + vals_b))
    max_diff = 0.0
    for pt in all_pts:
        cdf_a = sum(1 for x in vals_a if x <= pt) / len(vals_a)
        cdf_b = sum(1 for x in vals_b if x <= pt) / len(vals_b)
        max_diff = max(max_diff, abs(cdf_a - cdf_b))
    return max_diff


def print_delta_table(label: str, stories_x: dict, stories_y: dict,
                      x_name: str, y_name: str):
    all_ids = sorted(set(stories_x) | set(stories_y))
    print(f"\n{'─'*72}")
    print(f"DELTA TABLE: {label}  ({x_name} vs {y_name})")
    print(f"{'─'*72}")
    hdr = (f"{'constraint_id':<38} {'arm':<8} {'ε':>5} {'sup':>5} {'thtr':>5} "
           f"{'AC':>5} {'res':>5} {'msmt':>4} {'ωs':>3} {'claimed':>14}")
    print(hdr)
    print("-" * 96)

    total_deltas = []
    for cid in all_ids:
        for arm_label, arm_stories in [(x_name, stories_x), (y_name, stories_y)]:
            s = arm_stories.get(cid)
            if s is None:
                print(f"  {cid:<36} {arm_label:<8} [MISSING]")
                continue
            bp = s.get("base_properties", {})
            eps  = bp.get("extractiveness", "—")
            sup  = bp.get("suppression", "—")
            thr  = bp.get("theater_ratio", "—")
            ac   = bp.get("accessibility_collapse", "—")
            res  = bp.get("resistance", "—")
            claimed = bp.get("claimed_type", "—")
            msmt_present = "yes" if s.get("measurements") else "no"
            omega_present = "yes" if s.get("omegas") else "no"
            print(f"  {cid:<36} {arm_label:<8} {fmt(eps):>5} {fmt(sup):>5} {fmt(thr):>5} "
                  f"{fmt(ac):>5} {fmt(res):>5} {msmt_present:>4} {omega_present:>3} "
                  f"{str(claimed):<14}")

        sx = stories_x.get(cid)
        sy = stories_y.get(cid)
        if sx and sy:
            row_deltas = []
            for field in ["extractiveness", "suppression", "theater_ratio",
                          "accessibility_collapse", "resistance"]:
                vx = get_metric(sx, field)
                vy = get_metric(sy, field)
                if vx is not None and vy is not None:
                    delta = vy - vx
                    row_deltas.append(abs(delta))
                    arrow = "→" if abs(delta) < 0.001 else ("↑" if delta > 0 else "↓")
                    print(f"    Δ {field}: {vx:.3f} → {vy:.3f}  ({arrow}{abs(delta):.3f})", end="")
            ct_x = sx.get("base_properties", {}).get("claimed_type", "?")
            ct_y = sy.get("base_properties", {}).get("claimed_type", "?")
            match = "MATCH" if ct_x == ct_y else f"FLIP: {ct_x}→{ct_y}"
            print(f"  | claimed_type: {match}")
            mx = bool(sx.get("measurements"))
            my = bool(sy.get("measurements"))
            if mx != my:
                print(f"    *** measurements_present diverges: {x_name}={mx}, {y_name}={my} "
                      f"(evidence-forcing-function difference)")
            if row_deltas:
                total_deltas.extend(row_deltas)
        print()

    if total_deltas:
        max_d = max(total_deltas)
        mean_d = sum(total_deltas) / len(total_deltas)
        print(f"  Summary: max|Δ|={max_d:.3f}  mean|Δ|={mean_d:.3f}  across {len(total_deltas)} paired metric values")


def main():
    if len(sys.argv) not in (3, 4):
        print(f"Usage: {sys.argv[0]} <arm_a_dir> <arm_b_dir> [arm_a2_dir]")
        sys.exit(1)

    dir_a  = pathlib.Path(sys.argv[1])
    dir_b  = pathlib.Path(sys.argv[2])
    dir_a2 = pathlib.Path(sys.argv[3]) if len(sys.argv) == 4 else None

    stories_a  = load_stories(dir_a)
    stories_b  = load_stories(dir_b)
    stories_a2 = load_stories(dir_a2) if dir_a2 else None

    n_a  = len(stories_a)
    n_b  = len(stories_b)
    n_a2 = len(stories_a2) if stories_a2 else 0

    print("=" * 72)
    print("A/B TEST SCORING REPORT")
    print("=" * 72)
    caveat_n = max(n_a, n_b, n_a2)
    print(f"\n⚠  CAVEAT: n_A={n_a}  n_B={n_b}  n_A2={n_a2}.")
    print(f"   BPI values land in {{{', '.join([f'{i/max(caveat_n,1):.2f}' for i in range(caveat_n+1)])}}}.")
    print("   KS is near-content-free. DO NOT draw conclusions from aggregate stats.")
    print("   Read the paired delta tables. Verdict: 'suggestive' or 'in noise'.\n")

    # ── Experimental comparison: A vs B ──────────────────────────────────────
    print_delta_table("EXPERIMENTAL: sighted vs stripped",
                      stories_a, stories_b, dir_a.name, dir_b.name)

    # ── Noise floor: A vs A′ (if provided) ───────────────────────────────────
    if stories_a2 is not None:
        print_delta_table("NOISE FLOOR: sighted vs sighted-control",
                          stories_a, stories_a2, dir_a.name, dir_a2.name)
        print("\n  Interpretation:")
        print("  • If max|Δ| A/A′ ≈ 0 and max|Δ| A/B >> 0  → strip effect visible above noise")
        print("  • If max|Δ| A/A′ ≈ max|Δ| A/B             → signal in noise; n must rise")

    # ── Secondary: BPI ───────────────────────────────────────────────────────
    print(f"\n{'─'*72}")
    print("SECONDARY (BATCH-ONLY — NOT INTERPRETABLE AT n≤10): BPI PER METRIC")
    print(f"{'─'*72}")
    bpi_a = bpi_arm(stories_a)
    bpi_b = bpi_arm(stories_b)
    header_parts = [f"{'Metric':<26}", f"{'BPI_A':>6}", f"{'BPI_B':>6}", f"{'Δ BPI':>8}"]
    if stories_a2:
        bpi_a2 = bpi_arm(stories_a2)
        header_parts += [f"{'BPI_A2':>7}", f"{'Δ noise':>8}"]
    print("\n" + "  " + " ".join(header_parts))
    print("  " + "-" * 55)
    for m in METRICS:
        parts = [f"  {m:<24}", f"{bpi_a[m]:>6.2f}", f"{bpi_b[m]:>6.2f}",
                 f"{bpi_b[m]-bpi_a[m]:>+8.2f}"]
        if stories_a2:
            delta_noise = bpi_a2[m] - bpi_a[m]
            parts += [f"{bpi_a2[m]:>7.2f}", f"{delta_noise:>+8.2f}"]
        print("".join(parts))

    eps_a  = [get_metric(s, "extractiveness") for s in stories_a.values()
              if get_metric(s, "extractiveness") is not None]
    eps_b  = [get_metric(s, "extractiveness") for s in stories_b.values()
              if get_metric(s, "extractiveness") is not None]
    ks_ab  = ks_distance(eps_a, eps_b)
    print(f"\n  KS(A,B) epsilon: {ks_ab:.3f}   A={eps_a}   B={eps_b}")
    if stories_a2:
        eps_a2 = [get_metric(s, "extractiveness") for s in stories_a2.values()
                  if get_metric(s, "extractiveness") is not None]
        ks_noise = ks_distance(eps_a, eps_a2)
        print(f"  KS(A,A′) epsilon: {ks_noise:.3f}  A′={eps_a2}  (noise floor)")
    print("\n  ⚠  KS at n≤10 is near-content-free.")

    # ── Verdict guidance ─────────────────────────────────────────────────────
    print(f"\n{'─'*72}")
    print("VERDICT GUIDANCE")
    print(f"{'─'*72}")
    print("\nHonest n=3 verdicts only:")
    print("  • A/B diverges AND A/A′ ≈ 0         → 'suggestive, escalate to batch'")
    print("  • A/B ≈ A/A′ (both non-zero)         → 'signal in noise, n must rise'")
    print("  • A/B ≈ 0                             → 'consistent with null, batch required'")
    print("\n  'BPI_A > BPI_B → threshold shapes authoring' is NOT a licensable n=3 conclusion.")
    print()


if __name__ == "__main__":
    main()
