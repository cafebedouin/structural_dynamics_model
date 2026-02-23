"""Classification audit — query function.

Triages Engine 1 findings into actionable categories. Reads false_mountain
JSON + corpus data + config. Moves AuditConstraint/AuditFinding dataclasses
and all triage logic from the original classification_audit.py.
"""

import re
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional

from shared.loader import read_config

BASE = Path(__file__).resolve().parent.parent.parent.parent
TESTSET_DIR = BASE / "prolog" / "testsets"

# =============================================================================
# DATA STRUCTURES
# =============================================================================

@dataclass
class AuditConstraint:
    """Per-constraint audit record combining corpus_data, .pl, and false_mountain data."""
    constraint_id: str = ""
    claimed_type: Optional[str] = None
    domain: Optional[str] = None
    extractiveness: Optional[float] = None
    suppression: Optional[float] = None
    emerges_naturally: Optional[bool] = None
    requires_enforcement: Optional[bool] = None
    beneficiaries: List[str] = field(default_factory=list)
    victims: List[str] = field(default_factory=list)
    classifications: List[dict] = field(default_factory=list)
    omegas: List[dict] = field(default_factory=list)
    variance_ratio: Optional[float] = None
    perspectival_types: Dict[str, str] = field(default_factory=dict)
    has_mountain_classification: bool = False
    theater_ratio: Optional[float] = None
    pl_claim_value: Optional[str] = None
    template_version: Optional[str] = None
    has_pl_file: bool = False
    is_false_mountain: bool = False
    gap_pattern: Optional[str] = None
    fm_severity: Optional[str] = None


@dataclass
class AuditFinding:
    """A single audit finding for one constraint."""
    constraint_id: str
    category: str
    severity: str
    summary: str
    details: Dict = field(default_factory=dict)


# Valid claim values (E1 check)
VALID_CLAIM_VALUES = {
    'natural_law', 'coordination', 'constructed', 'enforcement',
    'mountain', 'rope', 'snare', 'tangled_rope', 'scaffold', 'piton',
}


# =============================================================================
# IMPORT LAYER
# =============================================================================

def load_corpus_constraints(corpus_raw):
    """Build AuditConstraint records from corpus_data.json."""
    constraints = {}
    for cid, cdata in corpus_raw.get('constraints', {}).items():
        ac = AuditConstraint(constraint_id=cid)
        ac.claimed_type = cdata.get('claimed_type')
        ac.domain = cdata.get('domain')

        metrics = cdata.get('metrics', {})
        ac.extractiveness = metrics.get('extractiveness')
        ac.suppression = metrics.get('suppression')
        ac.emerges_naturally = metrics.get('emerges_naturally')
        ac.requires_enforcement = metrics.get('requires_enforcement')

        ac.beneficiaries = cdata.get('beneficiaries', []) or []
        ac.victims = cdata.get('victims', []) or []

        for clf in cdata.get('classifications', []):
            ac.classifications.append(clf)
            ctype = clf.get('type', '')
            ctx = clf.get('context', '')
            if isinstance(ctx, str):
                m = re.search(r'agent_power\((\w+)\)', ctx)
                if m:
                    power = m.group(1)
                    ac.perspectival_types[power] = ctype
            if ctype == 'mountain':
                ac.has_mountain_classification = True

        analysis = cdata.get('analysis', {})
        ac.omegas = analysis.get('omegas', []) or []
        ac.variance_ratio = analysis.get('variance_ratio')

        constraints[cid] = ac

    return constraints


def supplement_from_pl_files(constraints, testsets_dir):
    """Scan prolog/testsets/*.pl files to supplement theater_ratio, claim, version."""
    pl_count = 0
    for pl_file in sorted(testsets_dir.glob('*.pl')):
        pl_count += 1
        stem = pl_file.stem
        content = pl_file.read_text(errors='replace')

        ac = constraints.get(stem)
        if ac is None:
            for cid in constraints:
                if cid.lower() == stem.lower():
                    ac = constraints[cid]
                    break

        if ac is not None:
            ac.has_pl_file = True

            tm = re.search(r'theater_ratio\(\w+,\s*([\d.]+)\)', content)
            if not tm:
                tm = re.search(
                    r'constraint_metric\(\w+,\s*theater_ratio,\s*([\d.]+)\)',
                    content)
            if tm:
                ac.theater_ratio = float(tm.group(1))

            cm = re.search(
                r'constraint_claim\(\w+,\s*(\[?[\w,\s]+\]?)\)', content)
            if cm:
                ac.pl_claim_value = cm.group(1).strip()

            vm = re.search(r'Version:\s*([\d.]+)', content)
            if vm:
                ac.template_version = vm.group(1)

    return pl_count


def annotate_false_mountains(constraints, fm_data):
    """Annotate constraints with false mountain data from JSON sidecar."""
    fm_count = 0
    for entry in fm_data.get("false_mountains", []):
        fm_count += 1
        cid = entry["id"]
        ac = constraints.get(cid)
        if ac is None:
            continue
        ac.is_false_mountain = True
        ac.gap_pattern = entry.get("gap_pattern", "unknown")
        sev = entry.get("severity")
        if sev:
            ac.fm_severity = sev
    return fm_count


# =============================================================================
# TRIAGE RULES
# =============================================================================

def triage_category_a(ac, cfg):
    if not ac.has_mountain_classification:
        return None
    if ac.extractiveness is None:
        return None
    if ac.extractiveness <= cfg["MOUNTAIN_MAX_EXTRACTIVENESS"]:
        return None
    if not ac.requires_enforcement:
        return None

    is_a_plus = (ac.theater_ratio is not None
                 and ac.theater_ratio > cfg["THEATER_NATURALIZATION_THRESHOLD"])
    category = 'A+' if is_a_plus else 'A'

    details = {
        'extractiveness': ac.extractiveness,
        'suppression': ac.suppression,
        'requires_enforcement': ac.requires_enforcement,
        'theater_ratio': ac.theater_ratio,
        'claimed_type': ac.claimed_type,
        'domain': ac.domain,
        'is_false_mountain': ac.is_false_mountain,
        'gap_pattern': ac.gap_pattern,
    }
    mountain_perspectives = [p for p, t in ac.perspectival_types.items() if t == 'mountain']
    details['mountain_from_perspectives'] = mountain_perspectives

    summary = f"Mountain naturalization: \u03b5={ac.extractiveness:.2f}, enforcement=True"
    if is_a_plus:
        summary += f", theater={ac.theater_ratio:.2f}"

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category=category, severity='critical',
        summary=summary, details=details,
    )


def triage_category_b(ac, a_ids, cfg):
    if ac.constraint_id in a_ids:
        return None
    if not ac.has_mountain_classification:
        return None
    if ac.theater_ratio is None or ac.theater_ratio <= cfg["THEATER_CONFLICT_THRESHOLD"]:
        return None

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='B', severity='warning',
        summary=f"Theater-Mountain conflict: theater={ac.theater_ratio:.2f} but classified as mountain",
        details={
            'theater_ratio': ac.theater_ratio,
            'extractiveness': ac.extractiveness,
            'claimed_type': ac.claimed_type,
            'domain': ac.domain,
        },
    )


def triage_category_c(ac, cfg):
    if not ac.is_false_mountain:
        return None
    if ac.extractiveness is None:
        return None
    if ac.extractiveness > cfg["MOUNTAIN_MAX_EXTRACTIVENESS"]:
        return None
    if ac.requires_enforcement:
        return None

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='C', severity='info',
        summary=f"Legitimate gap: \u03b5={ac.extractiveness:.2f}, no enforcement \u2014 mountain defensible",
        details={
            'extractiveness': ac.extractiveness,
            'suppression': ac.suppression,
            'requires_enforcement': ac.requires_enforcement,
            'gap_pattern': ac.gap_pattern,
            'domain': ac.domain,
        },
    )


def triage_category_d(ac, cfg):
    if ac.extractiveness is None or ac.extractiveness <= cfg["WHO_MIN_EXTRACTIVENESS"]:
        return None
    if not ac.beneficiaries and not ac.victims:
        return None

    has_powerless = 'powerless' in ac.perspectival_types
    has_institutional = 'institutional' in ac.perspectival_types
    if has_powerless and has_institutional:
        return None

    missing = []
    if not has_powerless:
        missing.append('powerless')
    if not has_institutional:
        missing.append('institutional')

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='D', severity='warning',
        summary=f"WHO suspect: \u03b5={ac.extractiveness:.2f}, has beneficiary/victim but missing {', '.join(missing)} perspective",
        details={
            'extractiveness': ac.extractiveness,
            'beneficiaries': ac.beneficiaries,
            'victims': ac.victims,
            'present_perspectives': list(ac.perspectival_types.keys()),
            'missing_perspectives': missing,
            'domain': ac.domain,
        },
    )


def triage_category_e(ac, cfg):
    findings = []

    # E1: Invalid claim values
    if ac.pl_claim_value is not None:
        claim = ac.pl_claim_value.strip('[]').strip()
        claims = [c.strip() for c in claim.split(',')]
        for c in claims:
            if c and c not in VALID_CLAIM_VALUES:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E1', severity='warning',
                    summary=f"Invalid claim value: '{c}'",
                    details={'pl_claim_value': ac.pl_claim_value,
                             'valid_values': sorted(VALID_CLAIM_VALUES)},
                ))

    # E2: Missing theater_ratio where .pl file exists
    if ac.has_pl_file and ac.theater_ratio is None:
        findings.append(AuditFinding(
            constraint_id=ac.constraint_id,
            category='E2', severity='info',
            summary="Missing theater_ratio in .pl file",
            details={'has_pl_file': True},
        ))

    # E3: Missing core metrics
    missing_metrics = []
    if ac.extractiveness is None:
        missing_metrics.append('extractiveness')
    if ac.suppression is None:
        missing_metrics.append('suppression')
    if missing_metrics:
        findings.append(AuditFinding(
            constraint_id=ac.constraint_id,
            category='E3', severity='warning',
            summary=f"Missing core metrics: {', '.join(missing_metrics)}",
            details={'missing': missing_metrics},
        ))

    # E4: Classification-metric inconsistency
    for clf in ac.classifications:
        ctype = clf.get('type', '')
        ctx = clf.get('context', '')
        if isinstance(ctx, list):
            continue

        if ctype == 'snare' and ac.extractiveness is not None:
            if ac.extractiveness < cfg["SNARE_MIN_EXTRACTIVENESS"]:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E4', severity='warning',
                    summary=f"Snare classification but \u03b5={ac.extractiveness:.2f} < {cfg['SNARE_MIN_EXTRACTIVENESS']}",
                    details={
                        'classification': ctype, 'context': ctx,
                        'extractiveness': ac.extractiveness,
                        'threshold': cfg["SNARE_MIN_EXTRACTIVENESS"],
                    },
                ))
                break

        if ctype == 'mountain' and ac.extractiveness is not None:
            if ac.extractiveness > cfg["MOUNTAIN_MAX_EXTRACTIVENESS"]:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E4', severity='warning',
                    summary=f"Mountain classification but \u03b5={ac.extractiveness:.2f} > {cfg['MOUNTAIN_MAX_EXTRACTIVENESS']}",
                    details={
                        'classification': ctype, 'context': ctx,
                        'extractiveness': ac.extractiveness,
                        'threshold': cfg["MOUNTAIN_MAX_EXTRACTIVENESS"],
                    },
                ))
                break

    return findings


def triage_category_f(constraints, cfg):
    findings = []

    domain_total = {}
    domain_mountain = {}
    domain_theater_present = {}
    high_ext_claims = {}

    for ac in constraints.values():
        d = ac.domain or 'unknown'
        domain_total[d] = domain_total.get(d, 0) + 1
        if ac.has_mountain_classification:
            domain_mountain[d] = domain_mountain.get(d, 0) + 1
        if ac.theater_ratio is not None:
            domain_theater_present[d] = domain_theater_present.get(d, 0) + 1
        if (ac.extractiveness is not None
                and ac.extractiveness > cfg["WHO_MIN_EXTRACTIVENESS"]
                and ac.claimed_type):
            high_ext_claims[ac.claimed_type] = high_ext_claims.get(ac.claimed_type, 0) + 1

    # F1
    f1_details = {}
    for d in sorted(domain_total.keys()):
        total = domain_total[d]
        mountains = domain_mountain.get(d, 0)
        rate = mountains / total if total > 0 else 0
        f1_details[d] = {
            'total': total, 'mountain_count': mountains,
            'naturalization_rate': round(rate, 3),
        }
    findings.append(AuditFinding(
        constraint_id='_corpus', category='F1', severity='research',
        summary='Naturalization rate by domain', details=f1_details,
    ))

    # F2
    f2_details = {}
    for d in sorted(domain_total.keys()):
        total = domain_total[d]
        covered = domain_theater_present.get(d, 0)
        rate = covered / total if total > 0 else 0
        f2_details[d] = {
            'total': total, 'theater_covered': covered,
            'coverage_rate': round(rate, 3),
        }
    findings.append(AuditFinding(
        constraint_id='_corpus', category='F2', severity='research',
        summary='Theater coverage rate by domain', details=f2_details,
    ))

    # F3
    findings.append(AuditFinding(
        constraint_id='_corpus', category='F3', severity='research',
        summary='Claim distribution for high-extraction constraints (\u03b5 > 0.46)',
        details=high_ext_claims,
    ))

    return findings


def run_triage(constraints, cfg):
    """Run all triage categories and return findings."""
    findings = []

    a_ids = set()
    for ac in constraints.values():
        f = triage_category_a(ac, cfg)
        if f:
            findings.append(f)
            a_ids.add(ac.constraint_id)

    for ac in constraints.values():
        f = triage_category_b(ac, a_ids, cfg)
        if f:
            findings.append(f)

    for ac in constraints.values():
        f = triage_category_c(ac, cfg)
        if f:
            findings.append(f)

    for ac in constraints.values():
        f = triage_category_d(ac, cfg)
        if f:
            findings.append(f)

    for ac in constraints.values():
        findings.extend(triage_category_e(ac, cfg))

    findings.extend(triage_category_f(constraints, cfg))

    return findings


# =============================================================================
# QUERY FUNCTION
# =============================================================================

def query(data: dict) -> dict:
    """Loaded data -> template context for classification audit."""
    fm_data = data["false_mountain"]
    corpus_raw = data["corpus"]
    config = data["config"]

    cfg = {
        "MOUNTAIN_MAX_EXTRACTIVENESS": config.get('mountain_extractiveness_max', 0.25),
        "MOUNTAIN_MAX_SUPPRESSION": config.get('mountain_suppression_ceiling', 0.05),
        "SNARE_MIN_EXTRACTIVENESS": config.get('snare_epsilon_floor', 0.46),
        "SNARE_MIN_SUPPRESSION": config.get('snare_suppression_floor', 0.60),
        "TANGLED_MIN_SUPPRESSION": config.get('tangled_rope_suppression_floor', 0.40),
        "PITON_MIN_THEATER": config.get('piton_theater_floor', 0.70),
        "SCAFFOLD_MAX_THEATER": config.get('piton_theater_floor', 0.70),
        "THEATER_NATURALIZATION_THRESHOLD": config.get('audit_theater_naturalization_threshold', 0.50),
        "THEATER_CONFLICT_THRESHOLD": config.get('audit_theater_conflict_threshold', 0.50),
        "WHO_MIN_EXTRACTIVENESS": config.get('snare_epsilon_floor', 0.46),
    }

    # Load and supplement constraints
    constraints = load_corpus_constraints(corpus_raw)
    pl_count = supplement_from_pl_files(constraints, TESTSET_DIR)
    fm_count = annotate_false_mountains(constraints, fm_data)

    # Run triage
    findings = run_triage(constraints, cfg)

    # Partition by category
    by_cat = {}
    for f in findings:
        by_cat.setdefault(f.category, []).append(f)

    # Count false mountains triaged
    fm_in_a = sum(1 for f in by_cat.get('A+', []) + by_cat.get('A', [])
                  if constraints.get(f.constraint_id, AuditConstraint()).is_false_mountain)
    fm_in_c = len(by_cat.get('C', []))
    fm_in_other = sum(
        1 for ac in constraints.values()
        if ac.is_false_mountain
        and ac.constraint_id not in {f.constraint_id for f in by_cat.get('A+', []) + by_cat.get('A', [])}
        and ac.constraint_id not in {f.constraint_id for f in by_cat.get('C', [])}
    )

    now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')

    cat_meta = [
        ('A+', 'Severe Naturalization (auto-regen)', 'critical'),
        ('A', 'Naturalization Errors (auto-regen)', 'critical'),
        ('B', 'Theater-Mountain Conflicts', 'warning'),
        ('C', 'Legitimate Gaps (exonerated)', 'info'),
        ('D', 'WHO Suspects (human review)', 'warning'),
        ('E1', 'Invalid Claim Values', 'warning'),
        ('E2', 'Missing Theater Ratio', 'info'),
        ('E3', 'Missing Core Metrics', 'warning'),
        ('E4', 'Classification-Metric Inconsistency', 'warning'),
        ('F1', 'Naturalization Rate by Domain', 'research'),
        ('F2', 'Theater Coverage by Domain', 'research'),
        ('F3', 'Claim Distribution (high-\u03b5)', 'research'),
    ]

    # Precompute cat counts for exec summary
    cat_rows = []
    for cat, desc, sev in cat_meta:
        count = len(by_cat.get(cat, []))
        cat_rows.append({"cat": cat, "desc": desc, "count": count, "severity": sev})

    return {
        "now": now,
        "n_constraints": len(constraints),
        "pl_count": pl_count,
        "fm_count": fm_count,
        "fm_in_a": fm_in_a,
        "fm_in_c": fm_in_c,
        "fm_in_other": fm_in_other,
        "cat_rows": cat_rows,
        "by_cat": by_cat,
        "constraints": constraints,
        # Pre-sorted lists for each section
        "a_plus": sorted(by_cat.get('A+', []), key=lambda x: x.constraint_id),
        "a_list": sorted(by_cat.get('A', []), key=lambda x: x.constraint_id),
        "b_list": sorted(by_cat.get('B', []), key=lambda x: x.constraint_id),
        "c_list": sorted(by_cat.get('C', []), key=lambda x: x.constraint_id),
        "d_list": sorted(by_cat.get('D', []), key=lambda x: x.constraint_id),
        "e1_list": sorted(by_cat.get('E1', []), key=lambda x: x.constraint_id),
        "e2_list": sorted(by_cat.get('E2', []), key=lambda x: x.constraint_id),
        "e3_list": sorted(by_cat.get('E3', []), key=lambda x: x.constraint_id),
        "e4_list": sorted(by_cat.get('E4', []), key=lambda x: x.constraint_id),
        "f1": by_cat.get('F1', []),
        "f2": by_cat.get('F2', []),
        "f3": by_cat.get('F3', []),
    }
