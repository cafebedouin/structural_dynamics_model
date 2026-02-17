#!/usr/bin/env python3
"""
Classification Audit Engine — Triages Engine 1 findings into actionable categories.

Layer 3 in the pipeline:
  1. Structural linter: Can this file load?
  2. Classification engine (Prolog): What does the logic find?
  3. Audit engine (this): Which findings are actionable?

Reads: outputs/corpus_data.json, outputs/false_mountain_report.md, prolog/testsets/*.pl
Writes: outputs/classification_audit_report.md
"""

import argparse
import json
import re
import sys
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# =============================================================================
# THRESHOLDS — loaded from prolog/config.pl (single source of truth)
# =============================================================================

def _read_config():
    """Read param/2 values from prolog/config.pl."""
    config_path = Path(__file__).resolve().parent.parent / "prolog" / "config.pl"
    thresholds = {}
    try:
        with open(config_path, 'r', encoding='utf-8') as f:
            for line in f:
                match = re.search(r"param\((\w+),\s*(-?[\d.]+)\)", line)
                if match:
                    param_name = match.group(1)
                    try:
                        thresholds[param_name] = float(match.group(2))
                    except ValueError:
                        pass
    except Exception as e:
        print(f"Warning: Could not read {config_path}: {e}", file=sys.stderr)
    return thresholds

_CFG = _read_config()

MOUNTAIN_MAX_EXTRACTIVENESS = _CFG.get('mountain_extractiveness_max', 0.25)
MOUNTAIN_MAX_SUPPRESSION = _CFG.get('mountain_suppression_ceiling', 0.05)
SNARE_MIN_EXTRACTIVENESS = _CFG.get('snare_epsilon_floor', 0.46)
SNARE_MIN_SUPPRESSION = _CFG.get('snare_suppression_floor', 0.60)
TANGLED_MIN_SUPPRESSION = _CFG.get('tangled_rope_suppression_floor', 0.40)
PITON_MIN_THEATER = _CFG.get('piton_theater_floor', 0.70)
SCAFFOLD_MAX_THEATER = _CFG.get('piton_theater_floor', 0.70)
THEATER_NATURALIZATION_THRESHOLD = _CFG.get('audit_theater_naturalization_threshold', 0.50)
THEATER_CONFLICT_THRESHOLD = _CFG.get('audit_theater_conflict_threshold', 0.50)
WHO_MIN_EXTRACTIVENESS = _CFG.get('snare_epsilon_floor', 0.46)

# Valid claim values (E1 check)
VALID_CLAIM_VALUES = {
    'natural_law', 'coordination', 'constructed', 'enforcement',
    'mountain', 'rope', 'snare', 'tangled_rope', 'scaffold', 'piton',
}


# =============================================================================
# DATA STRUCTURES
# =============================================================================

@dataclass
class AuditConstraint:
    """Per-constraint audit record combining corpus_data, .pl, and false_mountain data."""
    # From corpus_data.json
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

    # Derived from classifications
    perspectival_types: Dict[str, str] = field(default_factory=dict)
    has_mountain_classification: bool = False

    # From .pl file supplements
    theater_ratio: Optional[float] = None
    pl_claim_value: Optional[str] = None
    template_version: Optional[str] = None
    has_pl_file: bool = False

    # From false_mountain_report.md
    is_false_mountain: bool = False
    gap_pattern: Optional[str] = None  # 'snare_masked_as_rope' or 'rope_appears_as_mountain'
    fm_severity: Optional[str] = None


@dataclass
class AuditFinding:
    """A single audit finding for one constraint."""
    constraint_id: str
    category: str  # A+, A, B, C, D, E1, E2, E3, E4, F1, F2, F3
    severity: str  # critical, warning, info, research
    summary: str
    details: Dict = field(default_factory=dict)


# =============================================================================
# IMPORT LAYER — 3 LOADERS
# =============================================================================

def load_corpus_data(path: Path) -> Dict[str, AuditConstraint]:
    """Load corpus_data.json into AuditConstraint records."""
    with open(path, 'r') as f:
        raw = json.load(f)

    constraints = {}
    for cid, cdata in raw.get('constraints', {}).items():
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

        # Parse classifications — extract perspectival_types
        for clf in cdata.get('classifications', []):
            ac.classifications.append(clf)
            ctype = clf.get('type', '')
            ctx = clf.get('context', '')

            # Parse agent_power from context string
            if isinstance(ctx, str):
                m = re.search(r'agent_power\((\w+)\)', ctx)
                if m:
                    power = m.group(1)
                    ac.perspectival_types[power] = ctype
            # Skip null-array contexts (these are duplicates from extraction)

            if ctype == 'mountain':
                ac.has_mountain_classification = True

        analysis = cdata.get('analysis', {})
        ac.omegas = analysis.get('omegas', []) or []
        ac.variance_ratio = analysis.get('variance_ratio')

        constraints[cid] = ac

    return constraints


def supplement_from_pl_files(constraints: Dict[str, AuditConstraint],
                             testsets_dir: Path) -> int:
    """Scan prolog/testsets/*.pl files to supplement theater_ratio, claim, version.
    Returns count of .pl files processed."""
    pl_count = 0
    for pl_file in sorted(testsets_dir.glob('*.pl')):
        pl_count += 1
        stem = pl_file.stem
        content = pl_file.read_text(errors='replace')

        # Find matching constraint (stem may match constraint_id)
        ac = constraints.get(stem)
        if ac is None:
            # Try case-insensitive or partial match
            for cid in constraints:
                if cid.lower() == stem.lower():
                    ac = constraints[cid]
                    break

        if ac is not None:
            ac.has_pl_file = True

            # theater_ratio
            tm = re.search(r'theater_ratio\(\w+,\s*([\d.]+)\)', content)
            if not tm:
                tm = re.search(
                    r'constraint_metric\(\w+,\s*theater_ratio,\s*([\d.]+)\)',
                    content)
            if tm:
                ac.theater_ratio = float(tm.group(1))

            # pl_claim_value
            cm = re.search(
                r'constraint_claim\(\w+,\s*(\[?[\w,\s]+\]?)\)', content)
            if cm:
                ac.pl_claim_value = cm.group(1).strip()

            # template_version
            vm = re.search(r'Version:\s*([\d.]+)', content)
            if vm:
                ac.template_version = vm.group(1)

    return pl_count


def load_false_mountains(path: Path,
                         constraints: Dict[str, AuditConstraint]) -> int:
    """Parse false_mountain_report.md and annotate matching constraints.
    Returns count of false mountains found."""
    if not path.exists():
        return 0

    text = path.read_text(errors='replace')
    fm_count = 0

    # Each false mountain entry: ### N. False Mountain: `constraint_id`
    entries = re.findall(
        r'###\s*\d+\.\s*False Mountain:\s*`([^`]+)`\s*\n(.*?)(?=###\s*\d+\.|$)',
        text, re.DOTALL)

    for cid, body in entries:
        fm_count += 1
        ac = constraints.get(cid)
        if ac is None:
            # Not in corpus_data — still count but can't annotate
            continue

        ac.is_false_mountain = True

        # Classify gap pattern
        if 'Snare" is masked as functional "Rope"' in body:
            ac.gap_pattern = 'snare_masked_as_rope'
        elif 'Rope" appears as "Mountain"' in body:
            ac.gap_pattern = 'rope_appears_as_mountain'
        else:
            ac.gap_pattern = 'unknown'

        # Severity
        sev_m = re.search(r'\*\*Severity:\*\*\s*`(\w+)`', body)
        if sev_m:
            ac.fm_severity = sev_m.group(1)

    return fm_count


# =============================================================================
# TRIAGE RULES — Categories A-F
# =============================================================================

def triage_category_a(ac: AuditConstraint) -> Optional[AuditFinding]:
    """Category A — Naturalization Errors.
    Mountain classification + ε > 0.15 + requires_enforcement = True.
    A+ upgrade: also theater_ratio > 0.50."""
    if not ac.has_mountain_classification:
        return None
    if ac.extractiveness is None:
        return None
    if ac.extractiveness <= MOUNTAIN_MAX_EXTRACTIVENESS:
        return None
    if not ac.requires_enforcement:
        return None

    is_a_plus = (ac.theater_ratio is not None
                 and ac.theater_ratio > THEATER_NATURALIZATION_THRESHOLD)

    category = 'A+' if is_a_plus else 'A'
    severity = 'critical' if is_a_plus else 'critical'

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

    mountain_perspectives = [
        p for p, t in ac.perspectival_types.items() if t == 'mountain'
    ]
    details['mountain_from_perspectives'] = mountain_perspectives

    summary = (
        f"Mountain naturalization: ε={ac.extractiveness:.2f}, "
        f"enforcement=True"
    )
    if is_a_plus:
        summary += f", theater={ac.theater_ratio:.2f}"

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category=category,
        severity=severity,
        summary=summary,
        details=details,
    )


def triage_category_b(ac: AuditConstraint,
                      a_ids: set) -> Optional[AuditFinding]:
    """Category B — Theater-Mountain Conflict.
    Mountain classification + theater > 0.50 + NOT in Category A."""
    if ac.constraint_id in a_ids:
        return None
    if not ac.has_mountain_classification:
        return None
    if ac.theater_ratio is None or ac.theater_ratio <= THEATER_CONFLICT_THRESHOLD:
        return None

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='B',
        severity='warning',
        summary=(
            f"Theater-Mountain conflict: theater={ac.theater_ratio:.2f} "
            f"but classified as mountain"
        ),
        details={
            'theater_ratio': ac.theater_ratio,
            'extractiveness': ac.extractiveness,
            'claimed_type': ac.claimed_type,
            'domain': ac.domain,
        },
    )


def triage_category_c(ac: AuditConstraint) -> Optional[AuditFinding]:
    """Category C — Legitimate Gaps (exoneration).
    In false_mountain_report + ε ≤ 0.15 + no enforcement.
    Mountain classification is defensible."""
    if not ac.is_false_mountain:
        return None
    if ac.extractiveness is None:
        return None
    if ac.extractiveness > MOUNTAIN_MAX_EXTRACTIVENESS:
        return None
    if ac.requires_enforcement:
        return None

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='C',
        severity='info',
        summary=(
            f"Legitimate gap: ε={ac.extractiveness:.2f}, "
            f"no enforcement — mountain defensible"
        ),
        details={
            'extractiveness': ac.extractiveness,
            'suppression': ac.suppression,
            'requires_enforcement': ac.requires_enforcement,
            'gap_pattern': ac.gap_pattern,
            'domain': ac.domain,
        },
    )


def triage_category_d(ac: AuditConstraint) -> Optional[AuditFinding]:
    """Category D — WHO Suspects (human review).
    ε > 0.46 + has beneficiary/victim declared + missing powerless OR
    institutional perspective."""
    if ac.extractiveness is None or ac.extractiveness <= WHO_MIN_EXTRACTIVENESS:
        return None
    if not ac.beneficiaries and not ac.victims:
        return None

    has_powerless = 'powerless' in ac.perspectival_types
    has_institutional = 'institutional' in ac.perspectival_types

    if has_powerless and has_institutional:
        return None  # Both perspectives present — not a WHO suspect

    missing = []
    if not has_powerless:
        missing.append('powerless')
    if not has_institutional:
        missing.append('institutional')

    return AuditFinding(
        constraint_id=ac.constraint_id,
        category='D',
        severity='warning',
        summary=(
            f"WHO suspect: ε={ac.extractiveness:.2f}, "
            f"has beneficiary/victim but missing {', '.join(missing)} perspective"
        ),
        details={
            'extractiveness': ac.extractiveness,
            'beneficiaries': ac.beneficiaries,
            'victims': ac.victims,
            'present_perspectives': list(ac.perspectival_types.keys()),
            'missing_perspectives': missing,
            'domain': ac.domain,
        },
    )


def triage_category_e(ac: AuditConstraint) -> List[AuditFinding]:
    """Category E — Structural Defects (fixable).
    E1: Invalid claim values
    E2: Missing theater_ratio where .pl file exists
    E3: Missing core metrics
    E4: Classification-metric inconsistency"""
    findings = []

    # E1: Invalid claim values
    if ac.pl_claim_value is not None:
        claim = ac.pl_claim_value.strip('[]').strip()
        # Handle list syntax like [psychological_projection]
        claims = [c.strip() for c in claim.split(',')]
        for c in claims:
            if c and c not in VALID_CLAIM_VALUES:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E1',
                    severity='warning',
                    summary=f"Invalid claim value: '{c}'",
                    details={'pl_claim_value': ac.pl_claim_value,
                             'valid_values': sorted(VALID_CLAIM_VALUES)},
                ))

    # E2: Missing theater_ratio where .pl file exists
    if ac.has_pl_file and ac.theater_ratio is None:
        findings.append(AuditFinding(
            constraint_id=ac.constraint_id,
            category='E2',
            severity='info',
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
            category='E3',
            severity='warning',
            summary=f"Missing core metrics: {', '.join(missing_metrics)}",
            details={'missing': missing_metrics},
        ))

    # E4: Classification-metric inconsistency
    for clf in ac.classifications:
        ctype = clf.get('type', '')
        ctx = clf.get('context', '')
        if isinstance(ctx, list):
            continue  # Skip null-array contexts

        if ctype == 'snare' and ac.extractiveness is not None:
            if ac.extractiveness < SNARE_MIN_EXTRACTIVENESS:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E4',
                    severity='warning',
                    summary=(
                        f"Snare classification but ε={ac.extractiveness:.2f} "
                        f"< {SNARE_MIN_EXTRACTIVENESS}"
                    ),
                    details={
                        'classification': ctype,
                        'context': ctx,
                        'extractiveness': ac.extractiveness,
                        'threshold': SNARE_MIN_EXTRACTIVENESS,
                    },
                ))
                break  # One E4 per constraint for this type

        if ctype == 'mountain' and ac.extractiveness is not None:
            if ac.extractiveness > MOUNTAIN_MAX_EXTRACTIVENESS:
                findings.append(AuditFinding(
                    constraint_id=ac.constraint_id,
                    category='E4',
                    severity='warning',
                    summary=(
                        f"Mountain classification but ε={ac.extractiveness:.2f} "
                        f"> {MOUNTAIN_MAX_EXTRACTIVENESS}"
                    ),
                    details={
                        'classification': ctype,
                        'context': ctx,
                        'extractiveness': ac.extractiveness,
                        'threshold': MOUNTAIN_MAX_EXTRACTIVENESS,
                    },
                ))
                break

    return findings


def triage_category_f(constraints: Dict[str, AuditConstraint]) -> List[AuditFinding]:
    """Category F — Corpus Bias (research output).
    F1: Naturalization rate by domain
    F2: Theater coverage rate by domain
    F3: Claim distribution among high-extraction constraints"""
    findings = []

    # Gather domain stats
    domain_total = {}
    domain_mountain = {}
    domain_theater_present = {}
    high_ext_claims = {}  # claim -> count for ε > 0.46

    for ac in constraints.values():
        d = ac.domain or 'unknown'
        domain_total[d] = domain_total.get(d, 0) + 1

        if ac.has_mountain_classification:
            domain_mountain[d] = domain_mountain.get(d, 0) + 1

        if ac.theater_ratio is not None:
            domain_theater_present[d] = domain_theater_present.get(d, 0) + 1

        if (ac.extractiveness is not None
                and ac.extractiveness > WHO_MIN_EXTRACTIVENESS
                and ac.claimed_type):
            high_ext_claims[ac.claimed_type] = (
                high_ext_claims.get(ac.claimed_type, 0) + 1)

    # F1: Naturalization rate by domain
    f1_details = {}
    for d in sorted(domain_total.keys()):
        total = domain_total[d]
        mountains = domain_mountain.get(d, 0)
        rate = mountains / total if total > 0 else 0
        f1_details[d] = {
            'total': total,
            'mountain_count': mountains,
            'naturalization_rate': round(rate, 3),
        }

    findings.append(AuditFinding(
        constraint_id='_corpus',
        category='F1',
        severity='research',
        summary='Naturalization rate by domain',
        details=f1_details,
    ))

    # F2: Theater coverage rate by domain
    f2_details = {}
    for d in sorted(domain_total.keys()):
        total = domain_total[d]
        covered = domain_theater_present.get(d, 0)
        rate = covered / total if total > 0 else 0
        f2_details[d] = {
            'total': total,
            'theater_covered': covered,
            'coverage_rate': round(rate, 3),
        }

    findings.append(AuditFinding(
        constraint_id='_corpus',
        category='F2',
        severity='research',
        summary='Theater coverage rate by domain',
        details=f2_details,
    ))

    # F3: Claim distribution among high-extraction constraints
    findings.append(AuditFinding(
        constraint_id='_corpus',
        category='F3',
        severity='research',
        summary='Claim distribution for high-extraction constraints (ε > 0.46)',
        details=high_ext_claims,
    ))

    return findings


# =============================================================================
# MAIN TRIAGE ORCHESTRATOR
# =============================================================================

def run_triage(constraints: Dict[str, AuditConstraint]) -> List[AuditFinding]:
    """Run all triage categories and return findings."""
    findings = []

    # Category A (includes A+)
    a_ids = set()
    for ac in constraints.values():
        f = triage_category_a(ac)
        if f:
            findings.append(f)
            a_ids.add(ac.constraint_id)

    # Category B (excludes A)
    for ac in constraints.values():
        f = triage_category_b(ac, a_ids)
        if f:
            findings.append(f)

    # Category C
    for ac in constraints.values():
        f = triage_category_c(ac)
        if f:
            findings.append(f)

    # Category D
    for ac in constraints.values():
        f = triage_category_d(ac)
        if f:
            findings.append(f)

    # Category E (can produce multiple per constraint)
    for ac in constraints.values():
        findings.extend(triage_category_e(ac))

    # Category F (corpus-level)
    findings.extend(triage_category_f(constraints))

    return findings


# =============================================================================
# REPORT GENERATION
# =============================================================================

def generate_report(constraints: Dict[str, AuditConstraint],
                    findings: List[AuditFinding],
                    pl_count: int,
                    fm_count: int) -> str:
    """Generate the markdown audit report."""
    now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    n_constraints = len(constraints)

    # Partition findings by category
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

    lines = []
    lines.append('# Classification Audit Report')
    lines.append(f'Generated: {now}  |  Corpus: {n_constraints} constraints  |  .pl files: {pl_count}')
    lines.append('')

    # --- Executive Summary ---
    lines.append('## Executive Summary')
    lines.append('')
    lines.append('| Category | Description | Count | Severity |')
    lines.append('|----------|-------------|------:|----------|')

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
        ('F3', 'Claim Distribution (high-ε)', 'research'),
    ]
    for cat, desc, sev in cat_meta:
        count = len(by_cat.get(cat, []))
        lines.append(f'| {cat} | {desc} | {count} | {sev} |')
    lines.append('')

    # --- 252-to-Triage Story ---
    lines.append('## The 252-to-Triage Story')
    lines.append('')
    lines.append(f'The classification engine found {fm_count} False Mountains.')
    lines.append(
        f'This audit partitions them: **{fm_in_a}** are parser errors (A/A+), '
        f'**{fm_in_c}** legitimate (C), **{fm_in_other}** uncategorized (need manual review).'
    )
    lines.append('')

    # --- Category A+ ---
    a_plus = by_cat.get('A+', [])
    lines.append(f'## Category A+: Severe Naturalization ({len(a_plus)} findings)')
    lines.append('')
    if a_plus:
        lines.append('Mountain classification with high extraction, active enforcement, AND high theater ratio.')
        lines.append('These are the strongest candidates for automated regeneration.')
        lines.append('')
        for f in sorted(a_plus, key=lambda x: x.constraint_id):
            _render_finding(lines, f, constraints)
    else:
        lines.append('No A+ findings.')
    lines.append('')

    # --- Category A ---
    a_list = by_cat.get('A', [])
    lines.append(f'## Category A: Naturalization Errors ({len(a_list)} findings)')
    lines.append('')
    if a_list:
        lines.append('Mountain classification with high extraction and active enforcement.')
        lines.append('Queue for classification regeneration.')
        lines.append('')
        for f in sorted(a_list, key=lambda x: x.constraint_id):
            _render_finding(lines, f, constraints)
    else:
        lines.append('No Category A findings.')
    lines.append('')

    # --- Category B ---
    b_list = by_cat.get('B', [])
    lines.append(f'## Category B: Theater-Mountain Conflicts ({len(b_list)} findings)')
    lines.append('')
    if b_list:
        lines.append('Mountain classification with high theater ratio but not meeting full Category A criteria.')
        lines.append('Flag for manual review.')
        lines.append('')
        for f in sorted(b_list, key=lambda x: x.constraint_id):
            _render_finding(lines, f, constraints)
    else:
        lines.append('No Category B findings.')
    lines.append('')

    # --- Category C ---
    c_list = by_cat.get('C', [])
    lines.append(f'## Category C: Legitimate Gaps — Exoneration ({len(c_list)} findings)')
    lines.append('')
    if c_list:
        lines.append('False mountains where Mountain classification is defensible:')
        lines.append('low extraction, no enforcement. These are correctly-identified perspectival gaps.')
        lines.append('')
        for f in sorted(c_list, key=lambda x: x.constraint_id):
            lines.append(f'- `{f.constraint_id}` — {f.summary}')
    else:
        lines.append('No Category C findings (no false mountains met exoneration criteria).')
    lines.append('')

    # --- Category D ---
    d_list = by_cat.get('D', [])
    lines.append(f'## Category D: WHO Suspects ({len(d_list)} findings)')
    lines.append('')
    if d_list:
        lines.append('High extraction with declared beneficiary/victim asymmetry but missing')
        lines.append('key perspectival coverage. Needs human review to determine if extraction is hidden.')
        lines.append('')
        for f in sorted(d_list, key=lambda x: x.constraint_id):
            _render_finding(lines, f, constraints)
    else:
        lines.append('No Category D findings.')
    lines.append('')

    # --- Category E ---
    e_cats = ['E1', 'E2', 'E3', 'E4']
    e_total = sum(len(by_cat.get(ec, [])) for ec in e_cats)
    lines.append(f'## Category E: Structural Defects ({e_total} findings)')
    lines.append('')

    for ec in e_cats:
        e_list = by_cat.get(ec, [])
        ec_labels = {
            'E1': 'Invalid Claim Values',
            'E2': 'Missing Theater Ratio',
            'E3': 'Missing Core Metrics',
            'E4': 'Classification-Metric Inconsistency',
        }
        lines.append(f'### {ec}: {ec_labels[ec]} ({len(e_list)} findings)')
        lines.append('')
        if e_list:
            for f in sorted(e_list, key=lambda x: x.constraint_id):
                lines.append(f'- `{f.constraint_id}` — {f.summary}')
        else:
            lines.append(f'No {ec} findings.')
        lines.append('')

    # --- Category F ---
    lines.append('## Category F: Corpus Bias Analysis')
    lines.append('')

    # F1
    f1 = by_cat.get('F1', [])
    if f1:
        lines.append('### F1: Naturalization Rate by Domain')
        lines.append('')
        lines.append('| Domain | Total | Mountain Count | Naturalization Rate |')
        lines.append('|--------|------:|--------------:|-------------------:|')
        for d, stats in sorted(f1[0].details.items()):
            lines.append(
                f"| {d} | {stats['total']} | {stats['mountain_count']} "
                f"| {stats['naturalization_rate']:.1%} |"
            )
        lines.append('')

    # F2
    f2 = by_cat.get('F2', [])
    if f2:
        lines.append('### F2: Theater Coverage Rate by Domain')
        lines.append('')
        lines.append('| Domain | Total | Theater Covered | Coverage Rate |')
        lines.append('|--------|------:|---------------:|-------------:|')
        for d, stats in sorted(f2[0].details.items()):
            lines.append(
                f"| {d} | {stats['total']} | {stats['theater_covered']} "
                f"| {stats['coverage_rate']:.1%} |"
            )
        lines.append('')

    # F3
    f3 = by_cat.get('F3', [])
    if f3:
        lines.append('### F3: Claim Distribution for High-Extraction Constraints (ε > 0.46)')
        lines.append('')
        lines.append('| Claimed Type | Count |')
        lines.append('|-------------|------:|')
        for claim, count in sorted(f3[0].details.items(),
                                   key=lambda x: -x[1]):
            lines.append(f'| {claim} | {count} |')
        lines.append('')

    lines.append('---')
    lines.append(f'*Report generated by classification_audit.py on {now}*')

    return '\n'.join(lines)


def _render_finding(lines: list, f: AuditFinding,
                    constraints: Dict[str, AuditConstraint]):
    """Render a single per-constraint finding as markdown."""
    ac = constraints.get(f.constraint_id)
    lines.append(f'### `{f.constraint_id}`')
    lines.append(f'- **Category:** {f.category} | **Severity:** {f.severity}')
    lines.append(f'- **Summary:** {f.summary}')
    if ac:
        lines.append(f'- **Domain:** {ac.domain or "unknown"}')
        lines.append(f'- **Claimed Type:** {ac.claimed_type or "unknown"}')
        if ac.perspectival_types:
            persp_str = ', '.join(
                f'{p}→{t}' for p, t in sorted(ac.perspectival_types.items()))
            lines.append(f'- **Perspectival Types:** {persp_str}')
        if ac.beneficiaries:
            lines.append(f'- **Beneficiaries:** {", ".join(str(b) for b in ac.beneficiaries)}')
        if ac.victims:
            lines.append(f'- **Victims:** {", ".join(str(v) for v in ac.victims)}')
        if ac.is_false_mountain:
            lines.append(f'- **False Mountain:** Yes (gap: {ac.gap_pattern})')
    lines.append('')


# =============================================================================
# CLI ENTRY POINT
# =============================================================================

def main():
    parser = argparse.ArgumentParser(
        description='Classification Audit Engine — triages Engine 1 findings')
    parser.add_argument(
        '--corpus-data',
        default='outputs/corpus_data.json',
        help='Path to corpus_data.json (default: outputs/corpus_data.json)')
    parser.add_argument(
        '--false-mountains',
        default='outputs/false_mountain_report.md',
        help='Path to false_mountain_report.md')
    parser.add_argument(
        '--testsets',
        default='prolog/testsets',
        help='Path to prolog testsets directory')
    parser.add_argument(
        '--output',
        default='outputs/classification_audit_report.md',
        help='Output report path')
    args = parser.parse_args()

    # Resolve paths relative to script location's parent (project root)
    project_root = Path(__file__).resolve().parent.parent
    corpus_path = Path(args.corpus_data)
    if not corpus_path.is_absolute():
        corpus_path = project_root / corpus_path
    fm_path = Path(args.false_mountains)
    if not fm_path.is_absolute():
        fm_path = project_root / fm_path
    testsets_path = Path(args.testsets)
    if not testsets_path.is_absolute():
        testsets_path = project_root / testsets_path
    output_path = Path(args.output)
    if not output_path.is_absolute():
        output_path = project_root / output_path

    # Load data
    print(f"Loading corpus data from {corpus_path}...")
    constraints = load_corpus_data(corpus_path)
    print(f"  Loaded {len(constraints)} constraints")

    print(f"Supplementing from .pl files in {testsets_path}...")
    pl_count = supplement_from_pl_files(constraints, testsets_path)
    pl_matched = sum(1 for ac in constraints.values() if ac.has_pl_file)
    theater_count = sum(1 for ac in constraints.values()
                        if ac.theater_ratio is not None)
    print(f"  Scanned {pl_count} .pl files, matched {pl_matched}, "
          f"theater_ratio extracted: {theater_count}")

    print(f"Loading false mountains from {fm_path}...")
    fm_count = load_false_mountains(fm_path, constraints)
    fm_matched = sum(1 for ac in constraints.values() if ac.is_false_mountain)
    print(f"  Found {fm_count} false mountains, matched {fm_matched} to corpus")

    # Triage
    print("Running triage rules...")
    findings = run_triage(constraints)

    # Count by category
    cat_counts = {}
    for f in findings:
        cat_counts[f.category] = cat_counts.get(f.category, 0) + 1

    print("\n--- Audit Summary ---")
    for cat in ['A+', 'A', 'B', 'C', 'D', 'E1', 'E2', 'E3', 'E4',
                'F1', 'F2', 'F3']:
        count = cat_counts.get(cat, 0)
        if count > 0:
            print(f"  Category {cat}: {count}")
    print(f"  Total findings: {len(findings)}")

    # Generate report
    print(f"\nWriting report to {output_path}...")
    report = generate_report(constraints, findings, pl_count, fm_count)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(report)
    print(f"Done. Report: {output_path}")

    return 0


if __name__ == '__main__':
    sys.exit(main())
