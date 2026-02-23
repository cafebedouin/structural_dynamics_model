"""Meta-reporter — query function for pipeline health dashboard.

Reads pipeline_output.json for classification/diagnostic data and
orbit_data.json for orbit families.  Also reads output.txt directly
for Prolog test pass/fail counts (not available in the JSON).
"""

import re
from collections import Counter

from shared.loader import OUTPUT_DIR


def _parse_test_results():
    """Read output.txt ONLY for pass/fail counts and failure details."""
    output_txt = OUTPUT_DIR / 'output.txt'
    passed = 0
    failed = 0
    failures = []

    if not output_txt.exists():
        return passed, failed, failures

    with open(output_txt, 'r', encoding='utf-8') as f:
        content = f.read()

    passed = len(re.findall(r'\[PASS\]', content))
    failed = len(re.findall(r'\[(AUDIT FAIL|FAIL)\]', content))

    for match in re.finditer(r"  -\s\[(\w+)\]\s(.+?)\n\s+Reason:\s(.+)", content):
        ftype, path, detail = match.groups()
        failures.append({
            'type': ftype, 'path': path.strip(), 'detail': detail.strip()
        })

    return passed, failed, failures


def query(data: dict) -> dict:
    """Pipeline/orbit data -> template context for meta report."""
    pipeline = data["pipeline"]
    orbit_raw = data["orbit"]

    # Test results from output.txt
    passed_tests, failed_tests, test_failures = _parse_test_results()

    diag = pipeline.get('diagnostic', {})
    val = pipeline.get('validation', {})
    per_constraint = pipeline.get('per_constraint', [])

    # Corpus overview
    corpus_size = diag.get('corpus_size', len(per_constraint))
    type_distribution = diag.get('type_distribution', {})

    # Omegas
    omegas = []
    seen = set()
    for c in per_constraint:
        for o in (c.get('omegas') or []):
            oid = o['id']
            if oid not in seen:
                seen.add(oid)
                omegas.append((oid, o.get('type', 'conceptual')))

    omega_types = Counter(otype for _, otype in omegas)

    # Purity
    purity_scores = []
    for c in per_constraint:
        score = c.get('purity_score')
        band = c.get('purity_band')
        if score is not None:
            purity_scores.append((score, band))

    purity_summary = diag.get('purity_summary', {})
    drift_event_counts = diag.get('drift_event_counts', {})
    coupling_summary = diag.get('coupling_summary', {})
    boltzmann_summary = diag.get('boltzmann_summary', {})
    signature_distribution = val.get('signature_distribution', {})
    network_stability = diag.get('network_stability')

    # Purity avg
    purity_avg = None
    if purity_scores:
        scores = [s for s, _ in purity_scores]
        purity_avg = sum(scores) / len(scores)

    # Boltzmann signature counts
    fnl = signature_distribution.get('false_natural_law', 0)
    ci_rope = signature_distribution.get('coupling_invariant_rope', 0)
    fcr = signature_distribution.get('false_ci_rope', 0)
    total_sigs = fnl + ci_rope + fcr
    total_coupling = sum(coupling_summary.values()) if coupling_summary else 0
    total_drift = sum(drift_event_counts.values())

    # Orbit families
    orbit_families = {}
    orbit_total = 0
    if orbit_raw:
        for cid, entry in orbit_raw.items():
            sig = tuple(entry.get('orbit_signature', []))
            if sig not in orbit_families:
                orbit_families[sig] = []
            orbit_families[sig].append(cid)
        orbit_total = len(orbit_raw)

    singleton_count = sum(
        len(members) for sig, members in orbit_families.items()
        if len(sig) == 1
    )
    multi_count = orbit_total - singleton_count

    sorted_families = sorted(
        orbit_families.items(),
        key=lambda x: len(x[1]),
        reverse=True
    )

    unknown_families = [
        (sig, members) for sig, members in orbit_families.items()
        if 'unknown' in sig or 'naturalized' in sig
    ]
    unknown_count = sum(len(m) for _, m in unknown_families)

    return {
        "passed_tests": passed_tests,
        "failed_tests": failed_tests,
        "test_failures": test_failures,
        "total_tests": passed_tests + failed_tests,
        "pass_rate": (passed_tests / (passed_tests + failed_tests) * 100) if (passed_tests + failed_tests) > 0 else 0,
        "corpus_size": corpus_size,
        "type_distribution": type_distribution,
        "omegas": omegas,
        "omega_types": dict(omega_types.most_common()),
        "purity_scores": purity_scores,
        "purity_avg": purity_avg,
        "purity_summary": purity_summary,
        "drift_event_counts": drift_event_counts,
        "total_drift": total_drift,
        "coupling_summary": coupling_summary,
        "total_coupling": total_coupling,
        "signature_distribution": signature_distribution,
        "fnl": fnl,
        "ci_rope": ci_rope,
        "fcr": fcr,
        "total_sigs": total_sigs,
        "network_stability": network_stability,
        "orbit_families": orbit_families,
        "orbit_total": orbit_total,
        "singleton_count": singleton_count,
        "multi_count": multi_count,
        "sorted_families": sorted_families[:5],
        "unknown_families": unknown_families,
        "unknown_count": unknown_count,
    }
