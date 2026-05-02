#!/usr/bin/env python3
"""
Cultural Cognition Disagreement Profile (CCDP) — Post-Processing Diagnostic

Maps Dan Kahan's Cultural Cognition framework onto DR's existing δ infrastructure
to predict which real-world cognitive orientations will disagree about constraint
classification at specific observer positions.

Epistemic status: STRUCTURAL. Generates interpretive hypotheses over strict diagnostic
data. Does not alter classification, χ computation, H¹, or any engine output.
Every CCDP annotation is labeled STRUCTURAL and flagged as empirically uncalibrated.

Usage:
    python3 python/cc_diagnostic.py                    # full corpus audit + report annotations
    python3 python/cc_diagnostic.py --audit-only       # audit report only, no report modifications
    python3 python/cc_diagnostic.py --dry-run          # show what would be done, change nothing
"""

import json
import math
import os
import sys
from pathlib import Path

# ---------------------------------------------------------------------------
# CCDP boundary parameters — NOT engine classification thresholds.
#
# ccdp_extraction_boundary corresponds to snare_epsilon_floor (0.46) in config.pl,
# chosen because it captures the ambiguous zone where cognitive orientation has
# classification leverage. The engine's snare_chi_floor (0.66) would trigger on
# nearly all H¹≥3 constraints, just restating positional disagreement in CC vocabulary.
# ---------------------------------------------------------------------------
CCDP_COORDINATION_BOUNDARY = 0.35   # = rope_chi_ceiling in config.pl
CCDP_EXTRACTION_BOUNDARY = 0.46     # = snare_epsilon_floor in config.pl
CCDP_BAND_WIDTH = 0.10
CCDP_F2D_THRESHOLD_FALLBACK = 3.0   # fallback if corpus has fewer than 4 f2_d values

# Sigmoid params from config.pl (constraint_indexing.pl:256-284)
SIGMOID_LOWER = -0.20
SIGMOID_UPPER = 1.50
SIGMOID_MIDPOINT = 0.50
SIGMOID_STEEPNESS = 6.00

# Cultural Cognition quadrant δ mapping (UNCALIBRATED structural estimates)
CC_DELTA_MAP = {
    'hierarchical_individualist':    {'delta': -0.08, 'sigma_mod': 0.95},
    'hierarchical_communitarian':    {'delta': -0.08, 'sigma_mod': 1.05},
    'egalitarian_individualist':     {'delta': +0.08, 'sigma_mod': 0.95},
    'egalitarian_communitarian':     {'delta': +0.08, 'sigma_mod': 1.05},
}

# Standard observer positions
OBSERVERS = ['powerless', 'moderate', 'institutional', 'analytical']

# Classification type ordering for shift descriptions
TYPE_ORDER = {'mountain': 0, 'rope': 1, 'tangled_rope': 2, 'snare': 3, 'piton': 4}

# ---------------------------------------------------------------------------
# Extended δ range for Margin Analysis (atypical observers)
#
# Standard CC range is ±0.08. Extended range models:
#   δ ≈ -0.15 to -0.25: institutional capture, motivated coordination narrative,
#                        narcissistic investment in benevolence framing, high-corruption
#                        normalization, authoritarian deference
#   δ ≈ +0.15 to +0.25: whistleblower perception, trauma-informed hypervigilance,
#                        systematic extraction awareness, anti-authoritarian orientation,
#                        lived experience of repeated institutional betrayal
#
# These are NOT Cultural Cognition scores. CC instruments measure normal-range
# cognitive orientation. Extended δ models personality structure, situational
# distortion, and experiential calibration that CC does not instrument.
# ---------------------------------------------------------------------------
EXTENDED_DELTA_NEGATIVE = [-0.15, -0.20, -0.25]
EXTENDED_DELTA_POSITIVE = [+0.15, +0.20, +0.25]

ALL_DELTA_POINTS = [-0.25, -0.20, -0.15, -0.08, 0.00, +0.08, +0.15, +0.20, +0.25]

REGIME_LABELS = {
    -0.25: 'deep capture',
    -0.20: 'capture/normalization',
    -0.15: 'mild capture',
    -0.08: 'CC hierarchical (standard)',
     0.00: 'BASELINE',
    +0.08: 'CC egalitarian (standard)',
    +0.15: 'vigilance',
    +0.20: 'hypervigilance',
    +0.25: 'deep vigilance',
}

# Evidence signature library — interpretive heuristics, NOT diagnostic criteria
EVIDENCE_SIGNATURES = {
    'deep_capture': {
        'profile': ('Institutional capture, narcissistic investment in benevolence '
                    'narrative, deep authoritarian deference, corruption normalization'),
        'language_patterns': [
            'Collectivizing without distribution ("we all benefit", "the family", "the organization")',
            'Naturalization of structure ("that\'s just how it works", "it\'s always been this way")',
            'Moral loading of compliance ("loyalty", "duty", "respect", "gratitude")',
            'Pathologizing dissent ("you\'re being difficult", "why can\'t you just go along")',
        ],
        'blind_spots': [
            'Cannot name who bears asymmetric costs',
            'Cannot distinguish between collective benefit and distributed extraction',
            'Treats exit as betrayal rather than legitimate option',
            'Invisible: contamination network (coupling to neighboring constraints)',
        ],
        'challenge_response': ('Frames objections as defection, disloyalty, or ingratitude. '
                               'Escalates emotional stakes rather than engaging with evidence. '
                               'May deploy guilt, shame, or relational threats.'),
        'intervention_feasibility': ('Low. δ at this magnitude is typically ego-syntonic '
                                     '(personality-structural) or environmentally reinforced '
                                     '(corruption normalization). Evidence-based persuasion '
                                     'ineffective because the perception is motivated. '
                                     'Structural ε reduction (changing the constraint itself) '
                                     'is the viable path.'),
    },
    'capture': {
        'profile': ('Moderate institutional identification, conflict-averse orientation, '
                    'harmony-seeking that defaults to existing structure as legitimate'),
        'language_patterns': [
            'Emphasizes stability and continuity over fairness',
            'Acknowledges costs but frames them as acceptable trade-offs',
            'Appeals to precedent and tradition',
            'Uncomfortable with but not hostile to dissent',
        ],
        'blind_spots': [
            'Underweights asymmetric cost distribution',
            'Treats stability as inherently valuable regardless of what it stabilizes',
            'Partially visible: contamination network (sees immediate neighbors, misses system)',
        ],
        'challenge_response': ('Engages but deflects to "bigger picture" or "net benefit" framing. '
                               'More persuadable than deep capture — evidence of specific harm '
                               'to specific people can shift classification.'),
        'intervention_feasibility': ('Moderate. Specific, individualized harm evidence can shift '
                                     'δ toward neutral. Network visibility can be increased by '
                                     'walking through coupling edges one at a time.'),
    },
    'standard_hierarchical': {
        'profile': ('Normal-range Cultural Cognition hierarchical orientation. '
                    'Respects structured authority, values social order, sees existing '
                    'institutions as legitimate coordination mechanisms.'),
        'language_patterns': [
            'References roles, responsibilities, and authority structures',
            'Frames compliance as reasonable and productive',
            'Sees rules as serving order rather than as impositions',
        ],
        'blind_spots': [
            'Underweights extraction that operates through legitimate-looking channels',
            'May not distinguish between authority that coordinates and authority that extracts',
        ],
        'challenge_response': ('Engages substantively. Can update classification given evidence. '
                               'Persuadable through institutional-quality arguments.'),
        'intervention_feasibility': ('High. Normal-range CC variation responds to evidence '
                                     'and reframing. The standard CCDP annotation covers this range.'),
    },
    'baseline': {
        'profile': ('No systematic orientation bias. Classification reflects '
                    'the engine\'s structural assessment without cognitive modulation.'),
        'language_patterns': [],
        'blind_spots': [],
        'challenge_response': '',
        'intervention_feasibility': 'N/A — baseline classification.',
    },
    'standard_egalitarian': {
        'profile': ('Normal-range Cultural Cognition egalitarian orientation. '
                    'Skeptical of hierarchy, attentive to power asymmetries, sees '
                    'existing institutions as potential extraction mechanisms.'),
        'language_patterns': [
            'Asks "who benefits?" and "who bears the cost?"',
            'Frames rules as potential impositions rather than neutral coordination',
            'Attentive to asymmetric enforcement and selective application',
        ],
        'blind_spots': [
            'May underweight genuine coordination value of structures that also extract',
            'Can miss that removing a tangled_rope leaves a coordination gap',
        ],
        'challenge_response': ('Engages substantively. Can update classification given evidence '
                               'of genuine coordination function.'),
        'intervention_feasibility': 'High. Normal-range CC variation.',
    },
    'vigilance': {
        'profile': ('Elevated extraction sensitivity. Whistleblower perception, '
                    'experience of institutional betrayal, systematic awareness '
                    'of extraction patterns across domains.'),
        'language_patterns': [
            'Pattern-matches across domains ("this is the same structure as...")',
            'Names specific extraction mechanisms with precision',
            'May over-attribute intentionality to emergent extraction',
            'Attentive to contamination network and coupling effects',
        ],
        'blind_spots': [
            'May classify genuine coordination as extraction',
            'Risk of contamination cascade: over-detecting extraction in '
            'neighboring constraints due to network coupling awareness',
        ],
        'challenge_response': ('Engages but demands structural evidence, not reassurance. '
                               'Responds to demonstrated ε reduction, not to reframing.'),
        'intervention_feasibility': ('Moderate. Experientially calibrated — perception is '
                                     'evidence-based, not dispositional. Demonstrated structural '
                                     'change (ε reduction) more effective than argument.'),
    },
    'hypervigilance': {
        'profile': ('Trauma-informed or systematically betrayed observer. '
                    'Extensive experience of extraction presented as coordination. '
                    'High sensitivity to false coordination narratives.'),
        'language_patterns': [
            'Cannot or will not articulate coordination value even when present',
            'Treats all institutional framing as suspect',
            'Names extraction with high specificity but resists acknowledging mixed cases',
            'Network visibility maximized — sees coupling everywhere, including false positives',
        ],
        'blind_spots': [
            'Cannot see genuine coordination even in low-ε constraints',
            'Contamination cascade risk: extraction classification propagates '
            'through coupling network beyond what the evidence supports',
            'May classify mountains as false mountains (naturalized extraction)',
        ],
        'challenge_response': ('Does not engage with institutional-source evidence. '
                               'Responds only to demonstrated structural change and '
                               'personal trust built over time.'),
        'intervention_feasibility': ('Low via persuasion. Requires trust-building and '
                                     'demonstrated ε reduction over repeated interactions. '
                                     'The hypervigilance is experientially justified — the '
                                     'observer has been right about extraction enough times '
                                     'that defaulting to extraction-detection is rational. '
                                     'Recalibration requires new evidence, not argument.'),
    },
}

# Domain-specific signature modifiers
DOMAIN_MODIFIERS = {
    'family_interpersonal': {
        'negative_delta_language': [
            '"the family" as unitary actor',
            'care/love framing for control behaviors',
            'guilt deployment as coordination enforcement',
            'boundary violation framed as closeness',
        ],
        'positive_delta_language': [
            'naming specific obligations and their asymmetric distribution',
            'distinguishing care from control by examining whose needs are met',
            'identifying guilt as enforcement mechanism rather than authentic emotion',
        ],
        'sigma_note': ('In family dynamics, σ elevation (communitarian framing) '
                       'is especially powerful because "the family" is a scope '
                       'that both parties genuinely value. The extraction hides '
                       'inside shared commitment to the collective.'),
    },
    'institutional_regulatory': {
        'negative_delta_language': [
            '"efficiency" and "cost savings" framing for rights elimination',
            '"voluntary agreement" framing for take-it-or-leave-it contracts',
            '"dispute resolution" framing for claim suppression infrastructure',
        ],
        'positive_delta_language': [
            'naming specific populations who lose access to enforcement',
            'distinguishing procedural form from substantive function',
            'tracking outcome disparities rather than accepting process descriptions',
        ],
        'sigma_note': ('In regulatory domains, σ elevation sees industry-wide '
                       'patterns (adoption rates, demographic concentration, '
                       'outcome disparities across the system). σ depression '
                       'sees individual contracts and individual disputes.'),
    },
    'organizational_workplace': {
        'negative_delta_language': [
            '"team player" and "culture fit" framing for compliance extraction',
            '"opportunity" framing for unpaid labor',
            '"flexibility" framing for boundary erosion',
        ],
        'positive_delta_language': [
            'naming specific hours, tasks, and compensation asymmetries',
            'distinguishing genuine mentorship from exploitation of junior labor',
            'tracking who captures value from "collaborative" work products',
        ],
        'sigma_note': ('In workplace dynamics, σ elevation sees organizational '
                       'patterns (who gets promoted, who burns out, which roles '
                       'are extraction sinks). σ depression sees individual '
                       'transactions and individual career outcomes.'),
    },
    'geopolitical': {
        'negative_delta_language': [
            '"stability" and "order" framing for authoritarian extraction',
            '"sovereignty" framing for suppression of internal dissent',
            '"development" framing for resource extraction arrangements',
        ],
        'positive_delta_language': [
            'naming specific populations who bear costs of "stability"',
            'distinguishing state capacity from state extraction',
            'tracking resource flows rather than accepting governance narratives',
        ],
        'sigma_note': ('In geopolitical analysis, σ elevation sees systemic '
                       'patterns (trade networks, alliance structures, resource '
                       'dependencies). σ depression sees bilateral relationships '
                       'and individual state actions.'),
    },
}


# ---------------------------------------------------------------------------
# Core computation functions
# ---------------------------------------------------------------------------

def sigmoid_f(d):
    """Compute f(d) using the DR sigmoid. Matches constraint_indexing.pl:sigmoid_f/2."""
    L = SIGMOID_LOWER
    U = SIGMOID_UPPER
    k = SIGMOID_STEEPNESS
    d0 = SIGMOID_MIDPOINT
    return L + (U - L) / (1.0 + math.exp(-k * (d - d0)))


def compute_chi(epsilon, d, scope_mod):
    """Compute χ = ε × f(d) × σ(S)."""
    return epsilon * sigmoid_f(d) * scope_mod


def infer_effective_d(chi_actual, epsilon, scope_mod):
    """
    Back-compute the effective directionality d that produced chi_actual.

    The engine's derive_directionality/3 may produce a constraint-specific d
    that differs from the canonical d stored in the JSON. This function
    recovers the effective d from the engine's actual chi output.

    Returns the effective d, or None if it cannot be inferred.
    """
    if epsilon is None or scope_mod is None or epsilon * scope_mod == 0:
        return None
    target_fd = chi_actual / (epsilon * scope_mod)
    # f(d) = L + (U-L)/(1+exp(-k*(d-d0)))
    # Solve for d: d = d0 - (1/k) * ln((U-L)/(f(d)-L) - 1)
    L = SIGMOID_LOWER
    U = SIGMOID_UPPER
    k = SIGMOID_STEEPNESS
    d0 = SIGMOID_MIDPOINT
    inner = target_fd - L
    if inner <= 0 or inner >= (U - L):
        return None  # f(d) outside sigmoid range
    ratio = (U - L) / inner - 1.0
    if ratio <= 0:
        return None
    d_eff = d0 - (1.0 / k) * math.log(ratio)
    return max(0.0, min(1.0, d_eff))


def compute_chi_shift(epsilon, d, delta, scope_mod, chi_actual=None):
    """
    Compute χ at d+δ and the resulting Δχ.

    If chi_actual is provided, back-computes the effective d from the engine's
    actual output (which may differ from the stored canonical d) and uses that
    as the baseline. This ensures Δχ predictions are anchored to the engine's
    real classification, not a potentially inaccurate recomputation.

    Returns (chi_new, delta_chi).
    """
    if chi_actual is not None:
        d_eff_base = infer_effective_d(chi_actual, epsilon, scope_mod)
        if d_eff_base is not None:
            d = d_eff_base

    d_shifted = max(0.0, min(1.0, d + delta))
    chi_new = epsilon * sigmoid_f(d_shifted) * scope_mod
    chi_baseline = epsilon * sigmoid_f(d) * scope_mod
    return chi_new, chi_new - chi_baseline


def compute_threshold_distances(chi_values, thresholds=None):
    """
    For each observer position, compute min distance to any CCDP boundary.

    Args:
        chi_values: {position: chi_value} or {position: {chi: value, ...}}
        thresholds: dict with 'coordination' and 'extraction' keys

    Returns:
        {position: (distance, threshold_name, direction)}
        direction is 'below' or 'above' relative to the nearest threshold.
    """
    if thresholds is None:
        thresholds = {
            'coordination': CCDP_COORDINATION_BOUNDARY,
            'extraction': CCDP_EXTRACTION_BOUNDARY,
        }

    result = {}
    for pos, val in chi_values.items():
        chi = val['chi'] if isinstance(val, dict) else val
        if chi is None:
            continue

        d_coord = chi - thresholds['coordination']
        d_extract = chi - thresholds['extraction']

        if abs(d_coord) <= abs(d_extract):
            direction = 'above' if d_coord >= 0 else 'below'
            result[pos] = (abs(d_coord), 'coordination (0.35)', direction)
        else:
            direction = 'above' if d_extract >= 0 else 'below'
            result[pos] = (abs(d_extract), 'extraction (0.46)', direction)

    return result


def ccdp_trigger(h1, threshold_distances, band_width=None):
    """Returns True if CCDP annotation should fire."""
    if band_width is None:
        band_width = CCDP_BAND_WIDTH
    if h1 < 3:
        return False
    return any(dist < band_width for dist, _, _ in threshold_distances.values())


def contamination_sensitivity_matters(intrinsic_purity, effective_purity):
    """IC axis matters when the purity gap is large enough to affect classification."""
    if intrinsic_purity is None or effective_purity is None:
        return False
    return abs(intrinsic_purity - effective_purity) > 0.10


def get_delta_band_positions(threshold_distances, band_width=None):
    """Return positions where threshold_distance < band_width."""
    if band_width is None:
        band_width = CCDP_BAND_WIDTH
    return {
        pos: info for pos, info in threshold_distances.items()
        if info[0] < band_width
    }


def classify_chi(chi):
    """Classify a χ value using CCDP boundaries (for shift descriptions)."""
    if chi <= CCDP_COORDINATION_BOUNDARY:
        return 'rope-range'
    elif chi >= CCDP_EXTRACTION_BOUNDARY:
        return 'snare-range'
    else:
        return 'tangled_rope-range'


# ---------------------------------------------------------------------------
# Domain mapping
# ---------------------------------------------------------------------------

def infer_domain_category(topic_domain):
    """
    Map topic_domain string to one of the 4 DOMAIN_MODIFIERS categories.
    Returns 'generic' if no match.
    """
    if not topic_domain:
        return 'generic'
    td = topic_domain.lower()
    prefix = td.split('/')[0]

    if any(kw in td for kw in ['family', 'interpersonal', 'relationship', 'psychological',
                                'kinship', 'marriage', 'parenting', 'domestic']):
        return 'family_interpersonal'

    if prefix in ('geopolitical', 'military', 'international_relations'):
        return 'geopolitical'
    if prefix == 'political' and 'economy' not in td:
        return 'geopolitical'

    if prefix in ('legal', 'institutional', 'regulatory', 'healthcare',
                  'public_health', 'education', 'governance'):
        return 'institutional_regulatory'
    if any(kw in td for kw in ['regulatory', 'legal', 'healthcare', 'public_health',
                                'education/', 'judicial', 'enforcement']):
        return 'institutional_regulatory'

    if prefix in ('organizational', 'labor', 'labor_economics', 'workplace'):
        return 'organizational_workplace'
    if any(kw in td for kw in ['workplace', 'employment', 'labor/', 'corporate',
                                'management', 'hiring']):
        return 'organizational_workplace'

    return 'generic'


# ---------------------------------------------------------------------------
# Margin Analysis computation
# ---------------------------------------------------------------------------

def compute_extended_range(epsilon, d_canonical, scope_mod, baseline_chi):
    """
    Compute χ at all δ points for margin analysis.

    Uses infer_effective_d() to back-compute the actual d from engine chi,
    then computes χ = ε × f(d_eff + δ) × σ(S) for each δ.
    """
    d_eff = infer_effective_d(baseline_chi, epsilon, scope_mod)
    if d_eff is None:
        d_eff = d_canonical

    result = {
        'baseline': {
            'delta': 0.0,
            'chi': baseline_chi,
            'classification': classify_chi(baseline_chi),
            'd_eff': d_eff,
        },
        'extended': [],
    }

    for delta in ALL_DELTA_POINTS:
        if delta == 0.0:
            continue
        d_shifted = max(0.0, min(1.0, d_eff + delta))
        chi_new = epsilon * sigmoid_f(d_shifted) * scope_mod
        regime = REGIME_LABELS.get(delta, f'δ={delta:+.2f}')

        entry = {
            'delta': delta,
            'chi': chi_new,
            'classification': classify_chi(chi_new),
            'regime': regime,
        }
        result['extended'].append(entry)

    # Sort by delta
    result['extended'].sort(key=lambda e: e['delta'])
    return result


def find_exact_crossing_delta(epsilon, d_eff, scope_mod, threshold, d_range=(-0.30, 0.30),
                               tolerance=0.001):
    """
    Binary search for the δ where χ exactly equals a CCDP boundary threshold.
    Returns the δ value, or None if no crossing exists in the range.
    """
    lo, hi = d_range

    def chi_at_delta(delta):
        d_shifted = max(0.0, min(1.0, d_eff + delta))
        return epsilon * sigmoid_f(d_shifted) * scope_mod

    chi_lo = chi_at_delta(lo)
    chi_hi = chi_at_delta(hi)

    # Check if threshold is between chi_lo and chi_hi
    if not ((chi_lo <= threshold <= chi_hi) or (chi_hi <= threshold <= chi_lo)):
        return None

    for _ in range(50):  # max iterations
        mid = (lo + hi) / 2.0
        chi_mid = chi_at_delta(mid)
        if abs(chi_mid - threshold) < tolerance:
            return mid
        if (chi_lo < threshold) == (chi_mid < threshold):
            lo = mid
            chi_lo = chi_mid
        else:
            hi = mid
    return (lo + hi) / 2.0


def classify_range_span(extended_range):
    """
    Determine the full classification span under margin analysis.
    """
    # Build ordered list: baseline + all extended, sorted by delta
    all_points = []
    bl = extended_range['baseline']
    all_points.append({'delta': 0.0, 'chi': bl['chi'], 'classification': bl['classification']})
    for e in extended_range['extended']:
        all_points.append(e)
    all_points.sort(key=lambda p: p['delta'])

    min_class = all_points[0]['classification']
    max_class = all_points[-1]['classification']

    # Find boundary crossings
    crossings = []
    for i in range(1, len(all_points)):
        prev_cls = all_points[i - 1]['classification']
        curr_cls = all_points[i]['classification']
        if prev_cls != curr_cls:
            # Determine which boundary was crossed
            prev_chi = all_points[i - 1]['chi']
            curr_chi = all_points[i]['chi']
            if (prev_chi <= CCDP_COORDINATION_BOUNDARY < curr_chi or
                    curr_chi <= CCDP_COORDINATION_BOUNDARY < prev_chi):
                boundary = f'coordination ({CCDP_COORDINATION_BOUNDARY})'
            else:
                boundary = f'extraction ({CCDP_EXTRACTION_BOUNDARY})'

            direction = 'increasing' if curr_chi > prev_chi else 'decreasing'
            crossing_delta = (all_points[i - 1]['delta'] + all_points[i]['delta']) / 2.0

            crossings.append({
                'boundary': boundary,
                'delta_approx': crossing_delta,
                'delta_lo': all_points[i - 1]['delta'],
                'delta_hi': all_points[i]['delta'],
                'direction': direction,
                'from_class': prev_cls,
                'to_class': curr_cls,
            })

    # Deduplicate crossings (same boundary crossed same direction)
    unique_boundaries = set()
    unique_crossings = []
    for c in crossings:
        key = (c['boundary'], c['direction'])
        if key not in unique_boundaries:
            unique_boundaries.add(key)
            unique_crossings.append(c)

    span = len(unique_crossings)

    if min_class == max_class:
        range_desc = f'{min_class} throughout'
    else:
        range_desc = f'{min_class} to {max_class}'

    return {
        'min_class': min_class,
        'max_class': max_class,
        'span': span,
        'boundary_crossings': unique_crossings,
        'range_description': range_desc,
    }


def _get_zone_for_delta(delta):
    """Map a delta value to an evidence signature zone key."""
    if delta <= -0.20:
        return 'deep_capture'
    elif delta <= -0.12:
        return 'capture'
    elif delta <= -0.04:
        return 'standard_hierarchical'
    elif delta <= 0.04:
        return 'baseline'
    elif delta <= 0.12:
        return 'standard_egalitarian'
    elif delta <= 0.18:
        return 'vigilance'
    else:
        return 'hypervigilance'


def generate_evidence_signatures(extended_range, contamination, range_span, domain_category):
    """
    Generate evidence signature block for each classification zone the range spans.
    """
    lines = []
    lines.append("  Evidence Signatures:")
    lines.append("")

    # Determine which zones the range covers
    all_points = [{'delta': 0.0, **{k: v for k, v in extended_range['baseline'].items() if k != 'delta'}}]
    all_points.extend(extended_range['extended'])
    all_points.sort(key=lambda p: p['delta'])

    # Group points by classification zone
    zone_groups = {}
    for pt in all_points:
        cls = pt['classification']
        zone_key = _get_zone_for_delta(pt['delta'])
        if cls not in zone_groups:
            zone_groups[cls] = {'deltas': [], 'zones': set()}
        zone_groups[cls]['deltas'].append(pt['delta'])
        zone_groups[cls]['zones'].add(zone_key)

    # Generate per-zone signatures
    for cls in ['rope-range', 'tangled_rope-range', 'snare-range']:
        if cls not in zone_groups:
            continue
        grp = zone_groups[cls]
        delta_min = min(grp['deltas'])
        delta_max = max(grp['deltas'])

        if delta_min == delta_max:
            delta_desc = f'δ ≈ {delta_min:+.2f}'
        else:
            delta_desc = f'{delta_min:+.2f} ≤ δ ≤ {delta_max:+.2f}'

        lines.append(f"    [ZONE: {cls} ({delta_desc})]")

        # Collect signatures from all sub-zones in this classification
        for zone_key in sorted(grp['zones'], key=lambda z: list(EVIDENCE_SIGNATURES.keys()).index(z)
                               if z in EVIDENCE_SIGNATURES else 99):
            sig = EVIDENCE_SIGNATURES.get(zone_key, {})
            if not sig.get('profile'):
                continue

            lines.append(f"      Observer profile: {sig['profile']}")

            if sig.get('language_patterns'):
                lines.append("      Conversational indicators:")
                for lp in sig['language_patterns'][:3]:
                    lines.append(f"        - {lp}")

            if sig.get('blind_spots'):
                lines.append(f"      Blind spot: {sig['blind_spots'][0]}")
                for bs in sig['blind_spots'][1:2]:
                    lines.append(f"        {bs}")

            # Domain-specific modifiers
            dm = DOMAIN_MODIFIERS.get(domain_category, {})
            if dm:
                if delta_min < -0.04 and dm.get('negative_delta_language'):
                    lines.append(f"      Domain-specific ({domain_category}):")
                    for dl in dm['negative_delta_language'][:2]:
                        lines.append(f"        - {dl}")
                elif delta_max > 0.04 and dm.get('positive_delta_language'):
                    lines.append(f"      Domain-specific ({domain_category}):")
                    for dl in dm['positive_delta_language'][:2]:
                        lines.append(f"        - {dl}")

            lines.append("")

    # Network visibility note
    if contamination:
        ip = contamination.get('intrinsic_purity')
        ep = contamination.get('effective_purity')
        if ip is not None and ep is not None and contamination_sensitivity_matters(ip, ep):
            lines.append("    Network visibility across range:")
            lines.append(f"      Negative δ (capture): classifies in isolation, "
                         f"intrinsic purity ({ip:.3f}) dominates")
            lines.append(f"      Positive δ (vigilance): sees full coupling network, "
                         f"effective purity ({ep:.3f}) dominates")
            dm = DOMAIN_MODIFIERS.get(domain_category, {})
            if dm.get('sigma_note'):
                lines.append(f"      σ note: {dm['sigma_note']}")
            lines.append("")

    return "\n".join(lines)


def generate_margin_analysis(constraint_name, chi_values, epsilon, perspective_chi_detail,
                              h1, threshold_distances, contamination, topic_domain):
    """
    Generate the full Margin Analysis block for a CCDP-triggered constraint.
    """
    lines = []
    lines.append("")
    lines.append("--- MARGIN ANALYSIS [STRUCTURAL] ---")
    lines.append("")
    lines.append("  Empirical status: UNCALIBRATED — extended δ values model atypical observers")
    lines.append("  (institutional capture, narcissistic investment, trauma-informed perception, etc.)")
    lines.append("  and are NOT Cultural Cognition scores. Evidence signatures are interpretive")
    lines.append("  heuristics, not diagnostic criteria.")
    lines.append("")
    lines.append(f"  Constraint:     {constraint_name}")

    domain_category = infer_domain_category(topic_domain)
    if domain_category != 'generic':
        lines.append(f"  Domain:         {domain_category} (from topic_domain: {topic_domain})")
    else:
        lines.append(f"  Domain:         generic (topic_domain: {topic_domain or 'unknown'})")

    # Get δ-band active positions
    band_positions = get_delta_band_positions(threshold_distances)

    for pos, (dist, thresh_name, direction) in sorted(band_positions.items()):
        chi_detail = perspective_chi_detail.get(pos, {})
        chi = chi_detail.get('chi', 0)
        eps = chi_detail.get('epsilon', epsilon)
        d = chi_detail.get('d', 0)
        scope_mod = chi_detail.get('scope_mod', 1.0)

        baseline_class = classify_chi(chi)
        lines.append(f"  Baseline:       {baseline_class} (χ = {chi:.4f} at {pos})")
        lines.append(f"  δ-band status:  ACTIVE at {pos} (distance to {thresh_name} = {dist:.4f})")
        lines.append("")

        # Compute extended range
        ext_range = compute_extended_range(eps, d, scope_mod, chi)
        range_span = classify_range_span(ext_range)

        # Classification range table
        lines.append("  Classification Range:")
        lines.append("  ┌──────────────────────────────────────────────────────────────────────┐")
        lines.append("  │  δ        χ       Class              Regime                        │")

        d_eff = ext_range['baseline']['d_eff']

        # Build table rows from all points
        all_pts = []
        for e in ext_range['extended']:
            all_pts.append(e)
        all_pts.append({
            'delta': 0.0, 'chi': chi, 'classification': baseline_class,
            'regime': '◄ BASELINE',
        })
        all_pts.sort(key=lambda p: p['delta'])

        for pt in all_pts:
            delta_str = f"{pt['delta']:+6.2f}"
            chi_str = f"{pt['chi']:7.4f}"
            cls_str = f"{pt['classification']:20s}"
            regime = pt.get('regime', REGIME_LABELS.get(pt['delta'], ''))
            if pt['delta'] == 0.0:
                regime = '◄ BASELINE'
            lines.append(f"  │  {delta_str}    {chi_str}   {cls_str} {regime:30s}│")

        lines.append("  └──────────────────────────────────────────────────────────────────────┘")
        lines.append("")

        # Range span
        lines.append(f"  Range span: {range_span['range_description']} "
                      f"({range_span['span']} boundary crossing(s))")

        for cx in range_span['boundary_crossings']:
            # Try exact crossing delta
            if 'coordination' in cx['boundary']:
                thresh_val = CCDP_COORDINATION_BOUNDARY
            else:
                thresh_val = CCDP_EXTRACTION_BOUNDARY
            exact_delta = find_exact_crossing_delta(eps, d_eff, scope_mod, thresh_val)
            if exact_delta is not None:
                lines.append(f"    {cx['from_class']} → {cx['to_class']} at "
                              f"{cx['boundary']}, δ ≈ {exact_delta:+.3f}")
            else:
                lines.append(f"    {cx['from_class']} → {cx['to_class']} at "
                              f"{cx['boundary']}, δ ≈ {cx['delta_approx']:+.3f}")
        lines.append("")

        # Evidence signatures
        ev_sig = generate_evidence_signatures(ext_range, contamination, range_span, domain_category)
        lines.append(ev_sig)

    # Case-Specific Guidance (once per constraint, not per position)
    lines.append("  Case-Specific Guidance:")
    lines.append("    To place a specific observer within this range, assess:")
    lines.append("    1. Cost attribution: Can they name who bears asymmetric costs?")
    lines.append("       No → negative δ indicator | Yes → positive δ indicator")
    lines.append("    2. Coordination value: Can they articulate genuine coordination function?")
    lines.append("       No → positive δ indicator | Yes → negative δ indicator")
    lines.append("    3. Scope framing: Do they use collective (\"we all benefit\") or individual")
    lines.append("       (\"this person is harmed\") language?")
    lines.append("       Collective → σ elevation (network invisible) | "
                  "Individual → σ depression (network visible)")
    lines.append("    4. Naturalization: Do they treat the constraint as inevitable/natural or")
    lines.append("       as chosen/constructed?")
    lines.append("       Natural → negative δ | Constructed → positive δ")
    lines.append("    5. Response to challenge: Do they frame objections as defection/disloyalty")
    lines.append("       or as legitimate disagreement?")
    lines.append("       Defection → deep negative δ | Legitimate → neutral or positive δ")
    lines.append("")

    # Intervention Implications
    lines.append("  Intervention Implications:")

    # Get the primary δ-band position's range for guidance
    primary_pos = sorted(band_positions.keys())[0] if band_positions else None
    if primary_pos:
        chi_detail = perspective_chi_detail.get(primary_pos, {})
        chi_p = chi_detail.get('chi', 0)
        eps_p = chi_detail.get('epsilon', epsilon)

        lines.append("    Changing δ (observer reorientation):")
        lines.append("      At negative extreme: ego-syntonic / motivated — reorientation requires")
        lines.append("      therapeutic intervention or environmental change, not evidence")
        lines.append("      At positive extreme: experientially calibrated — reorientation requires")
        lines.append("      trust-building and demonstrated structural change, not argument")
        lines.append(f"    Changing ε (structural modification): Current ε = {eps_p:.2f}")

        # Compute what ε would need to be to converge classifications
        target_chi = (CCDP_COORDINATION_BOUNDARY + CCDP_EXTRACTION_BOUNDARY) / 2.0
        d_eff_p = infer_effective_d(chi_p, eps_p, chi_detail.get('scope_mod', 1.0))
        if d_eff_p is not None:
            fd = sigmoid_f(d_eff_p)
            sm = chi_detail.get('scope_mod', 1.0)
            if fd * sm > 0:
                target_eps = target_chi / (fd * sm)
                lines.append(f"      Reducing ε to ~{target_eps:.2f} would center classifications "
                              f"in tangled_rope-range across the full δ range")

        # Contamination note
        if contamination and contamination_sensitivity_matters(
                contamination.get('intrinsic_purity'),
                contamination.get('effective_purity')):
            lines.append("    Changing network visibility (σ intervention):")
            lines.append("      Making individual coupling edges explicit without requiring")
            lines.append("      network-level integration — walking through the contamination")
            lines.append("      graph one edge at a time")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Annotation generators
# ---------------------------------------------------------------------------

def generate_ccdp_annotation(constraint_name, chi_values, h1, threshold_distances,
                              contamination, orbit_signature, active_theorems,
                              perspective_chi_detail):
    """Generate full CCDP annotation block for a triggered constraint."""
    lines = []
    lines.append("")
    lines.append("--- CULTURAL COGNITION DISAGREEMENT PROFILE [STRUCTURAL] ---")
    lines.append("")
    lines.append(f"  Constraint:         {constraint_name}")
    lines.append("  Empirical status:   UNCALIBRATED — δ magnitudes are structural estimates, not psychometric measurements")
    lines.append("")

    # δ-band positions
    band_positions = get_delta_band_positions(threshold_distances)
    lines.append("  δ-band positions:")

    for pos, (dist, thresh_name, direction) in sorted(band_positions.items()):
        chi_detail = perspective_chi_detail.get(pos, {})
        chi = chi_detail.get('chi', 0)
        epsilon = chi_detail.get('epsilon', 0)
        d = chi_detail.get('d', 0)
        scope_mod = chi_detail.get('scope_mod', 1.0)

        lines.append(f"    {pos} (χ = {chi:.4f}, threshold = {thresh_name}, distance = {dist:.4f})")

        # Hierarchy-Egalitarianism axis
        lines.append("      Hierarchy-Egalitarianism axis (→ δ):")

        # Hierarchical (δ ≈ -0.08)
        chi_hier, delta_hier = compute_chi_shift(epsilon, d, -0.08, scope_mod, chi_actual=chi)
        hier_class = classify_chi(chi_hier)
        base_class = classify_chi(chi)
        hier_shift = f"χ shifts to {chi_hier:.4f} ({hier_class})"
        if hier_class != base_class:
            hier_shift += f" — CROSSES boundary from {base_class}"

        lines.append(f"        Hierarchical (δ ≈ -0.08): {hier_shift}")

        # Egalitarian (δ ≈ +0.08)
        chi_egal, delta_egal = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)
        egal_class = classify_chi(chi_egal)
        egal_shift = f"χ shifts to {chi_egal:.4f} ({egal_class})"
        if egal_class != base_class:
            egal_shift += f" — CROSSES boundary from {base_class}"

        lines.append(f"        Egalitarian (δ ≈ +0.08):  {egal_shift}")

        # Note on whether δ crosses
        crosses_hier = hier_class != base_class
        crosses_egal = egal_class != base_class
        if crosses_hier and crosses_egal:
            note = "Both orientations cross the boundary — strong CC modulation"
        elif crosses_hier:
            note = "Only hierarchical orientation crosses — asymmetric CC effect"
        elif crosses_egal:
            note = "Only egalitarian orientation crosses — asymmetric CC effect"
        else:
            note = f"Neither orientation crosses (Δχ_hier = {delta_hier:+.4f}, Δχ_egal = {delta_egal:+.4f}) — CC modulates magnitude but not category"
        lines.append(f"      Note: {note}")
        lines.append("")

    # Contamination network sensitivity
    lines.append("  Contamination network sensitivity:")
    lines.append("    Individualism-Communitarianism axis (→ network scope):")

    if contamination:
        ip = contamination.get('intrinsic_purity')
        ep = contamination.get('effective_purity')
        pd = contamination.get('propagation_delta')
        nc = len(contamination.get('neighbors', []))

        lines.append(f"      Neighbor count:    {nc}")
        lines.append(f"      Intrinsic purity:  {ip:.4f}" if ip is not None else "      Intrinsic purity:  [unavailable]")
        lines.append(f"      Effective purity:   {ep:.4f}" if ep is not None else "      Effective purity:   [unavailable]")
        lines.append(f"      Propagation delta: {pd:+.4f}" if pd is not None else "      Propagation delta: [unavailable]")

        if contamination_sensitivity_matters(ip, ep):
            lines.append(f"      Communitarian prediction: sees contamination network, effective purity ({ep:.3f}) dominates classification")
            lines.append(f"      Individualist prediction: classifies in isolation, intrinsic purity ({ip:.3f}) dominates")
            gap = abs(ip - ep) if ip is not None and ep is not None else 0
            lines.append(f"      Note: Purity gap = {gap:.3f} (> 0.10 threshold) — IC axis is active")
        else:
            gap = abs(ip - ep) if ip is not None and ep is not None else 0
            lines.append(f"      Note: Purity gap = {gap:.3f} (≤ 0.10 threshold) — IC axis suppressed, gap too small to matter")
    else:
        lines.append("      [contamination data unavailable]")
    lines.append("")

    # Predicted disagreement pattern
    lines.append("  Predicted disagreement pattern:")
    disagreement = _compute_disagreement_pattern(band_positions, perspective_chi_detail, h1)
    lines.append(f"    {disagreement}")
    lines.append("")

    # Falsifiable predictions
    lines.append("  Falsifiable predictions:")
    predictions = _compute_falsifiable_predictions(band_positions, perspective_chi_detail, contamination)
    for pred in predictions:
        lines.append(f"    - {pred}")
    lines.append("")

    return "\n".join(lines)


def _compute_disagreement_pattern(band_positions, perspective_chi_detail, h1):
    """Generate 1-3 sentence disagreement pattern description."""
    parts = []

    # Which quadrants disagree at which positions
    for pos, (dist, thresh_name, direction) in sorted(band_positions.items()):
        chi_detail = perspective_chi_detail.get(pos, {})
        chi = chi_detail.get('chi', 0)
        epsilon = chi_detail.get('epsilon', 0)
        d = chi_detail.get('d', 0)
        scope_mod = chi_detail.get('scope_mod', 1.0)
        base_class = classify_chi(chi)

        chi_hier, _ = compute_chi_shift(epsilon, d, -0.08, scope_mod, chi_actual=chi)
        chi_egal, _ = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)
        hier_class = classify_chi(chi_hier)
        egal_class = classify_chi(chi_egal)

        if hier_class != egal_class:
            parts.append(
                f"At {pos} (χ={chi:.3f}), hierarchical observers ({hier_class}) "
                f"and egalitarian observers ({egal_class}) see different constraint categories."
            )
        elif hier_class != base_class:
            parts.append(
                f"At {pos} (χ={chi:.3f}), CC orientation shifts the perceived category "
                f"from {base_class} to {hier_class}/{egal_class}."
            )
        else:
            parts.append(
                f"At {pos} (χ={chi:.3f}), CC orientation modulates χ magnitude "
                f"(range {chi_hier:.3f}–{chi_egal:.3f}) without crossing a boundary."
            )

    if not parts:
        return "No δ-band positions identified."

    result = " ".join(parts[:3])
    if h1 >= 5:
        result += f" H¹={h1} indicates high perspectival fracture — CC disagreement compounds existing positional disagreement."
    return result


def _compute_falsifiable_predictions(band_positions, perspective_chi_detail, contamination):
    """Generate 1-2 specific falsifiable predictions."""
    predictions = []

    for pos, (dist, thresh_name, direction) in sorted(band_positions.items()):
        chi_detail = perspective_chi_detail.get(pos, {})
        chi = chi_detail.get('chi', 0)
        epsilon = chi_detail.get('epsilon', 0)
        d = chi_detail.get('d', 0)
        scope_mod = chi_detail.get('scope_mod', 1.0)

        chi_hier, _ = compute_chi_shift(epsilon, d, -0.08, scope_mod, chi_actual=chi)
        chi_egal, _ = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)
        hier_class = classify_chi(chi_hier)
        egal_class = classify_chi(chi_egal)

        if hier_class != egal_class:
            predictions.append(
                f"Subjects scoring high on hierarchy (CC H-E axis) at the {pos} "
                f"observer position should classify this constraint as {hier_class}, "
                f"while egalitarian subjects should classify as {egal_class}. "
                f"Predicted effect size: Δχ ≈ {abs(chi_egal - chi_hier):.3f}."
            )
            break
        elif hier_class != classify_chi(chi):
            predictions.append(
                f"Both hierarchical and egalitarian subjects at {pos} should shift "
                f"classification from {classify_chi(chi)} to {hier_class}, but egalitarian "
                f"subjects should show stronger shift (Δχ_egal = {abs(chi_egal - chi):+.3f} "
                f"vs Δχ_hier = {abs(chi_hier - chi):+.3f})."
            )
            break

    if not predictions:
        predictions.append(
            "CC orientation at δ-band positions modulates χ magnitude without crossing "
            "classification boundaries. A psychometric study should find correlation between "
            "H-E score and perceived extraction intensity, but not categorical disagreement."
        )

    if contamination and contamination_sensitivity_matters(
            contamination.get('intrinsic_purity'),
            contamination.get('effective_purity')):
        ip = contamination.get('intrinsic_purity', 0)
        ep = contamination.get('effective_purity', 0)
        predictions.append(
            f"Subjects scoring high on communitarianism (CC I-C axis) should weight "
            f"network context more heavily, tracking effective purity ({ep:.3f}) rather "
            f"than intrinsic purity ({ip:.3f}). Predicted purity perception gap: {abs(ip-ep):.3f}."
        )

    return predictions[:2]


def generate_deep_fracture_annotation(constraint_name, h1, chi_values,
                                       threshold_distances, perspective_chi_detail):
    """Generate annotation for H¹≥3 constraints where CCDP does NOT trigger."""
    # Find position nearest to any threshold
    nearest_pos = min(threshold_distances, key=lambda p: threshold_distances[p][0])
    nearest_dist, nearest_thresh, nearest_dir = threshold_distances[nearest_pos]
    chi_detail = perspective_chi_detail.get(nearest_pos, {})
    chi = chi_detail.get('chi', 0)
    epsilon = chi_detail.get('epsilon', 0)
    d = chi_detail.get('d', 0)
    scope_mod = chi_detail.get('scope_mod', 1.0)

    # Compute shifted χ at δ = +0.08 (egalitarian, largest positive shift)
    chi_egal, _ = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)

    # Determine which threshold is nearest for the margin computation
    if 'coordination' in nearest_thresh:
        thresh_val = CCDP_COORDINATION_BOUNDARY
    else:
        thresh_val = CCDP_EXTRACTION_BOUNDARY

    margin = abs(chi_egal - thresh_val)
    still_above = "above" if chi_egal > thresh_val else "below"

    # Identify mechanism
    if chi < 0:
        mechanism = "the institutional sign-flip producing negative χ at this position"
    elif chi < 0.15:
        mechanism = f"extreme directionality suppression (f(d)={chi_detail.get('f_d', 0):.3f}) at {nearest_pos}"
    else:
        mechanism = f"large positional divergence (all χ values far from CCDP boundaries)"

    lines = []
    lines.append("")
    lines.append("--- CCDP: Deep Fracture Regime [STRUCTURAL] ---")
    lines.append("")
    lines.append(f"  H¹ = {h1}, but minimum threshold distance = {nearest_dist:.3f} (at {nearest_pos}).")
    lines.append(f"  Cognitive orientation (Cultural Cognition) does not modulate classification at any observer position.")
    lines.append(f"  The perspectival fracture is driven by {mechanism}.")
    lines.append(f"  An egalitarian observer (δ = +0.08) at {nearest_pos} would see χ ≈ {chi_egal:.3f},")
    lines.append(f"  still {still_above} {nearest_thresh} by {margin:.3f}.")
    lines.append("")
    lines.append("  Implication: Disagreement about this constraint is structural-positional, not cognitive-dispositional.")
    lines.append("  Reform requires changing observer position (policy restructuring), not persuading observers")
    lines.append("  within a position (discourse, framing, education).")
    lines.append("")

    return "\n".join(lines)


def generate_no_trigger_line(h1, min_threshold_distance):
    """Generate single-line annotation for non-triggered constraints."""
    return f"\n--- CCDP: Not triggered (H¹ = {h1}, min threshold distance = {min_threshold_distance:.3f}) ---\n"


# ---------------------------------------------------------------------------
# Report insertion
# ---------------------------------------------------------------------------

INSERTION_MARKER_BEFORE = "═══ LEVEL 3: CORPUS POSITIONING ═══"

def append_to_report(report_path, annotation):
    """Insert CCDP block before LEVEL 3: CORPUS POSITIONING in a report file."""
    with open(report_path, 'r') as f:
        content = f.read()

    if "CULTURAL COGNITION DISAGREEMENT PROFILE" in content or "CCDP:" in content:
        return False  # Already has CCDP annotation

    # Find insertion point
    marker_idx = content.find(INSERTION_MARKER_BEFORE)
    if marker_idx == -1:
        return False  # Can't find insertion point

    # Insert before the LEVEL 3 marker
    new_content = content[:marker_idx] + annotation + "\n\n" + content[marker_idx:]

    with open(report_path, 'w') as f:
        f.write(new_content)

    return True


def append_margin_to_report(report_path, margin_annotation):
    """Insert Margin Analysis block after existing CCDP block, before LEVEL 3."""
    with open(report_path, 'r') as f:
        content = f.read()

    if "MARGIN ANALYSIS" in content:
        return False  # Already has margin analysis

    if "CULTURAL COGNITION DISAGREEMENT PROFILE" not in content:
        return False  # No CCDP block to append after

    # Find LEVEL 3 marker
    marker_idx = content.find(INSERTION_MARKER_BEFORE)
    if marker_idx == -1:
        return False

    # Insert margin analysis right before LEVEL 3
    new_content = content[:marker_idx] + margin_annotation + "\n\n" + content[marker_idx:]

    with open(report_path, 'w') as f:
        f.write(new_content)

    return True


# ---------------------------------------------------------------------------
# Corpus audit
# ---------------------------------------------------------------------------

def run_corpus_audit(enriched_pipeline_path):
    """Run full CCDP audit on corpus. Returns structured audit data."""
    with open(enriched_pipeline_path) as f:
        data = json.load(f)

    pc = data['per_constraint']
    results = []

    # Pre-pass: compute corpus p75 of |f2_d| for curvature threshold
    all_f2d_abs = []
    for item in pc:
        for pos in OBSERVERS:
            pchi0 = item.get('perspective_chi', {})
            if pos in pchi0 and isinstance(pchi0[pos], dict):
                f2d = pchi0[pos].get('f2_d')
                if f2d is not None:
                    all_f2d_abs.append(abs(f2d))
    all_f2d_abs.sort()
    f2d_p75 = (all_f2d_abs[int(0.75 * len(all_f2d_abs))]
               if len(all_f2d_abs) >= 4 else CCDP_F2D_THRESHOLD_FALLBACK)

    for item in pc:
        cid = item['id']
        h1 = item.get('h1_band', 0)
        pchi = item.get('perspective_chi', {})
        contamination = item.get('contamination_network', {})
        perspectives = item.get('perspectives', {})
        epsilon = item.get('base_extractiveness', 0)
        signature = item.get('signature', '')
        topic_domain = item.get('topic_domain', '')

        # Extract chi values (skip None)
        chi_values = {}
        for pos in OBSERVERS:
            if pos in pchi and isinstance(pchi[pos], dict) and pchi[pos].get('chi') is not None:
                chi_values[pos] = pchi[pos]['chi']

        if not chi_values:
            results.append({
                'id': cid, 'h1': h1, 'trigger': False,
                'reason': 'no chi data', 'threshold_distances': {},
                'delta_band_positions': {},
                'curvature_alert': False,
                'curvature_alert_observers': {},
                'f2d_abs_by_observer': {},
                'f2d_p75_threshold': f2d_p75,
            })
            continue

        # Compute threshold distances
        tdist = compute_threshold_distances(chi_values)
        min_dist = min(d for d, _, _ in tdist.values()) if tdist else float('inf')

        # Apply trigger
        triggered = ccdp_trigger(h1, tdist)
        band_positions = get_delta_band_positions(tdist) if triggered else {}

        # Determine reason for not triggering
        if not triggered:
            if h1 == 0:
                reason = "H¹ = 0, gauge-invariant"
            elif h1 < 3:
                reason = f"H¹ = {h1} < 3"
            else:
                reason = f"H¹ = {h1}, min threshold distance = {min_dist:.3f} (> {CCDP_BAND_WIDTH})"
        else:
            reason = "triggered"

        # Curvature alert: |f''(d)| > corpus p75 AND chi near boundary
        curvature_alerts = {}
        f2d_abs_by_obs = {}
        for pos in OBSERVERS:
            if pos in pchi and isinstance(pchi[pos], dict):
                f2d = pchi[pos].get('f2_d')
                if f2d is not None:
                    f2d_abs = abs(f2d)
                    f2d_abs_by_obs[pos] = f2d_abs
                    if pos in tdist and f2d_abs > f2d_p75 and tdist[pos][0] < CCDP_BAND_WIDTH:
                        curvature_alerts[pos] = {
                            'f2_d_abs': f2d_abs,
                            'chi_distance': tdist[pos][0],
                        }

        results.append({
            'id': cid,
            'h1': h1,
            'trigger': triggered,
            'reason': reason,
            'min_threshold_distance': min_dist,
            'threshold_distances': {pos: {'distance': d, 'threshold': t, 'direction': dr}
                                    for pos, (d, t, dr) in tdist.items()},
            'delta_band_positions': {pos: {'distance': d, 'threshold': t, 'direction': dr}
                                    for pos, (d, t, dr) in band_positions.items()},
            'chi_values': chi_values,
            'perspectives': perspectives,
            'epsilon': epsilon,
            'signature': signature,
            'topic_domain': topic_domain,
            'contamination': contamination,
            'perspective_chi_detail': {pos: pchi[pos] for pos in OBSERVERS if pos in pchi},
            'curvature_alert': bool(curvature_alerts),
            'curvature_alert_observers': curvature_alerts,
            'f2d_abs_by_observer': f2d_abs_by_obs,
            'f2d_p75_threshold': f2d_p75,
        })

    return results, {'f2d_p75': f2d_p75}


def generate_audit_report(results, report_dir, meta=None):
    """Generate the CCDP audit report markdown."""
    lines = []
    lines.append("# CCDP Audit Report")
    lines.append("")
    lines.append("Cultural Cognition Disagreement Profile — Full Corpus Diagnostic")
    lines.append("")
    lines.append("**Epistemic status**: STRUCTURAL — interpretive hypotheses over strict diagnostic data.")
    lines.append("All annotations are UNCALIBRATED and empirically untested.")
    lines.append("")

    # CCDP parameters
    lines.append("## Parameters")
    lines.append("")
    lines.append(f"- CCDP coordination boundary: {CCDP_COORDINATION_BOUNDARY} (= rope_chi_ceiling in config.pl)")
    lines.append(f"- CCDP extraction boundary: {CCDP_EXTRACTION_BOUNDARY} (= snare_epsilon_floor in config.pl, NOT snare_chi_floor which is 0.66)")
    lines.append(f"- Band width: {CCDP_BAND_WIDTH}")
    lines.append(f"- CC δ magnitude: ±0.08 (UNCALIBRATED)")
    lines.append(f"- Contamination sensitivity threshold: 0.10 (purity gap)")
    lines.append("")

    # Corpus-wide statistics
    total = len(results)
    h1_dist = {}
    for r in results:
        h1 = r['h1']
        h1_dist[h1] = h1_dist.get(h1, 0) + 1

    triggered = [r for r in results if r['trigger']]
    h1_ge3 = [r for r in results if r['h1'] >= 3]
    deep_fracture = [r for r in h1_ge3 if not r['trigger']]

    lines.append("## Corpus-Wide Statistics")
    lines.append("")
    lines.append(f"Total constraints: {total}")
    lines.append(f"H¹ distribution: {dict(sorted(h1_dist.items()))}")
    lines.append(f"H¹ ≥ 3 (CCDP-eligible): {len(h1_ge3)}")
    lines.append(f"**CCDP triggered: {len(triggered)}** ({100*len(triggered)/len(h1_ge3):.1f}% of eligible)")
    lines.append(f"Deep fracture (H¹ ≥ 3, not triggered): {len(deep_fracture)}")
    lines.append(f"Gauge-invariant (H¹ = 0): {h1_dist.get(0, 0)}")
    lines.append("")

    # Trigger distribution by H¹ band
    lines.append("### Triggers by H¹ Band")
    lines.append("")
    lines.append("| H¹ | Total | Triggered | % | Deep Fracture |")
    lines.append("|---:|------:|----------:|--:|--------------:|")
    for h1 in sorted(h1_dist.keys()):
        if h1 < 3:
            continue
        h1_total = h1_dist[h1]
        h1_triggered = len([r for r in triggered if r['h1'] == h1])
        h1_deep = len([r for r in deep_fracture if r['h1'] == h1])
        pct = 100 * h1_triggered / h1_total if h1_total > 0 else 0
        lines.append(f"| {h1} | {h1_total} | {h1_triggered} | {pct:.0f}% | {h1_deep} |")
    lines.append("")

    # δ-band population by observer position
    lines.append("### δ-Band Active Positions (Corpus-Wide)")
    lines.append("")
    pos_counts = {pos: 0 for pos in OBSERVERS}
    for r in triggered:
        for pos in r['delta_band_positions']:
            pos_counts[pos] = pos_counts.get(pos, 0) + 1
    lines.append("| Observer | δ-Band Active Count | % of Triggered |")
    lines.append("|----------|--------------------:|---------------:|")
    for pos in OBSERVERS:
        ct = pos_counts.get(pos, 0)
        pct = 100 * ct / len(triggered) if triggered else 0
        lines.append(f"| {pos} | {ct} | {pct:.1f}% |")
    lines.append("")

    # Curvature alert statistics
    curvature_alerted = [r for r in results if r.get('curvature_alert')]
    delta_and_curvature = [r for r in results if r.get('trigger') and r.get('curvature_alert')]
    curvature_not_delta = [r for r in results if r.get('curvature_alert') and not r.get('trigger')]
    lines.append("### Curvature Alert Statistics (|f′′(d)| > corpus p75 AND χ near boundary)")
    lines.append("")
    threshold = (meta or {}).get('f2d_p75') or next(
        (r.get('f2d_p75_threshold') for r in results if r.get('f2d_p75_threshold') is not None), None)
    if threshold is not None:
        lines.append(f"f′′(d) threshold (corpus p75): {threshold:.4f}")
    lines.append(f"Curvature alerts: {len(curvature_alerted)} / {total}")
    if triggered:
        lines.append(f"δ-band AND curvature (overlap): {len(delta_and_curvature)}"
                     f" ({100*len(delta_and_curvature)/len(triggered):.1f}% of δ-triggered)")
    lines.append(f"Curvature only (no δ-band trigger): {len(curvature_not_delta)}")
    lines.append("")

    # Detailed table for reported constraints
    reported_ids = set()
    if report_dir and os.path.isdir(report_dir):
        for f in os.listdir(report_dir):
            if f.endswith('_report.md'):
                cid = f.replace('_report.md', '')
                reported_ids.add(cid)

    reported_results = [r for r in results if r['id'] in reported_ids]
    reported_results.sort(key=lambda r: (not r['trigger'], r.get('min_threshold_distance', float('inf'))))

    lines.append(f"## Reported Constraints Detail ({len(reported_results)} of {len(reported_ids)} matched)")
    lines.append("")
    lines.append("| Constraint | H¹ | Trigger | δ-band Positions | Nearest Threshold | Min Distance | Notes |")
    lines.append("|------------|---:|---------|------------------|-------------------|-------------:|-------|")

    for r in reported_results:
        cid = r['id']
        h1 = r['h1']
        trig = "**YES**" if r['trigger'] else "no"
        min_d = r.get('min_threshold_distance', float('inf'))

        # δ-band positions
        bp = r.get('delta_band_positions', {})
        bp_str = ", ".join(sorted(bp.keys())) if bp else "—"

        # Nearest threshold info
        tdist = r.get('threshold_distances', {})
        if tdist:
            nearest_pos = min(tdist, key=lambda p: tdist[p]['distance'])
            nearest_info = tdist[nearest_pos]
            thresh_str = f"{nearest_info['threshold']} ({nearest_pos})"
        else:
            thresh_str = "—"

        # Notes
        notes = r.get('reason', '') if not r['trigger'] else ""

        lines.append(f"| {cid} | {h1} | {trig} | {bp_str} | {thresh_str} | {min_d:.3f} | {notes} |")

    lines.append("")

    # Validation Appendix
    lines.append("## Validation Appendix")
    lines.append("")

    # 1. Threshold arithmetic for triggered constraints (sample)
    lines.append("### 1. Threshold Arithmetic Verification (Sample)")
    lines.append("")
    sample_triggered = [r for r in reported_results if r['trigger']][:5]
    for r in sample_triggered:
        cid = r['id']
        lines.append(f"**{cid}** (H¹={r['h1']})")
        for pos, bp_info in sorted(r.get('delta_band_positions', {}).items()):
            pcd = r.get('perspective_chi_detail', {}).get(pos, {})
            chi = pcd.get('chi', 0)
            epsilon = pcd.get('epsilon', 0)
            d = pcd.get('d', 0)
            scope_mod = pcd.get('scope_mod', 1.0)
            f_d = sigmoid_f(d)

            # Use effective d (back-computed from engine chi) for shift predictions
            d_effective = infer_effective_d(chi, epsilon, scope_mod)
            d_used = d_effective if d_effective is not None else d

            chi_hier, d_hier = compute_chi_shift(epsilon, d, -0.08, scope_mod, chi_actual=chi)
            chi_egal, d_egal = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)
            f_d_used = sigmoid_f(d_used)
            f_d_hier = sigmoid_f(max(0, min(1, d_used - 0.08)))
            f_d_egal = sigmoid_f(max(0, min(1, d_used + 0.08)))

            d_note = "" if d_effective is None or abs(d - d_used) < 0.01 else f" [effective d={d_used:.3f}, canonical d={d}]"
            lines.append(f"  {pos}: ε={epsilon}, d_eff={d_used:.3f}, f(d)={f_d_used:.4f}, σ={scope_mod}, χ={chi:.4f}{d_note}")
            lines.append(f"    δ=-0.08: d_eff={max(0,min(1,d_used-0.08)):.3f}, f(d_eff)={f_d_hier:.4f}, χ_new={chi_hier:.4f}, Δχ={d_hier:+.4f}")
            lines.append(f"    δ=+0.08: d_eff={max(0,min(1,d_used+0.08)):.3f}, f(d_eff)={f_d_egal:.4f}, χ_new={chi_egal:.4f}, Δχ={d_egal:+.4f}")
            lines.append(f"    Base class: {classify_chi(chi)}, Hier class: {classify_chi(chi_hier)}, Egal class: {classify_chi(chi_egal)}")
        lines.append("")

    # 2. False-negative check
    lines.append("### 2. False-Negative Check (H¹ ≥ 3, Not Triggered)")
    lines.append("")
    deep_reported = [r for r in reported_results if r['h1'] >= 3 and not r['trigger']]
    lines.append(f"Total deep fracture in reported set: {len(deep_reported)}")
    lines.append("")
    # Show the closest-to-triggering cases
    deep_reported_sorted = sorted(deep_reported, key=lambda r: r.get('min_threshold_distance', float('inf')))
    lines.append("Closest to trigger threshold (top 10):")
    lines.append("")
    lines.append("| Constraint | H¹ | Min Distance | Position | Threshold |")
    lines.append("|------------|---:|------------:|---------:|-----------|")
    for r in deep_reported_sorted[:10]:
        tdist = r.get('threshold_distances', {})
        if tdist:
            nearest_pos = min(tdist, key=lambda p: tdist[p]['distance'])
            ni = tdist[nearest_pos]
            lines.append(f"| {r['id']} | {r['h1']} | {ni['distance']:.3f} | {nearest_pos} | {ni['threshold']} |")
    lines.append("")
    lines.append("Assessment: These constraints are in the deep fracture regime. Widening band_width beyond 0.10")
    lines.append("would capture them, but at δ=±0.08 the actual Δχ would be insufficient to cross boundaries.")
    lines.append("")

    # 3. False-positive check
    lines.append("### 3. False-Positive Check (Triggered but δ Cannot Cross)")
    lines.append("")
    false_positives = []
    for r in sample_triggered:
        all_trivial = True
        for pos, bp_info in r.get('delta_band_positions', {}).items():
            pcd = r.get('perspective_chi_detail', {}).get(pos, {})
            chi = pcd.get('chi', 0)
            epsilon = pcd.get('epsilon', 0)
            d = pcd.get('d', 0)
            scope_mod = pcd.get('scope_mod', 1.0)
            chi_hier, _ = compute_chi_shift(epsilon, d, -0.08, scope_mod, chi_actual=chi)
            chi_egal, _ = compute_chi_shift(epsilon, d, +0.08, scope_mod, chi_actual=chi)
            base_class = classify_chi(chi)
            if classify_chi(chi_hier) != base_class or classify_chi(chi_egal) != base_class:
                all_trivial = False
                break
        if all_trivial:
            false_positives.append(r['id'])

    if false_positives:
        lines.append(f"Found {len(false_positives)} false positives in sample (δ=±0.08 doesn't cross at any δ-band position):")
        for fp in false_positives:
            lines.append(f"  - {fp}")
        lines.append("")
        lines.append("These constraints are in-band but CC modulation is insufficient to cross. In production,")
        lines.append("consider suppressing the full CCDP block for these and using a reduced annotation.")
    else:
        lines.append("No false positives found in sample — all triggered constraints have at least one")
        lines.append("δ-band position where δ=±0.08 produces a meaningful classification shift.")
    lines.append("")

    # 4. Contamination sensitivity
    lines.append("### 4. Contamination Sensitivity Validation")
    lines.append("")
    ic_active = 0
    ic_suppressed = 0
    for r in triggered:
        cont = r.get('contamination', {})
        ip = cont.get('intrinsic_purity')
        ep = cont.get('effective_purity')
        if ip is not None and ep is not None:
            if contamination_sensitivity_matters(ip, ep):
                ic_active += 1
            else:
                ic_suppressed += 1
    lines.append(f"IC axis active (purity gap > 0.10): {ic_active} of {len(triggered)} triggered constraints")
    lines.append(f"IC axis suppressed (purity gap ≤ 0.10): {ic_suppressed}")
    lines.append("")

    # --- Margin Analysis Statistics ---
    lines.append("## Margin Analysis Statistics")
    lines.append("")

    # Range span statistics
    from collections import Counter
    span_counts = Counter()
    crossing_deltas = []
    domain_counts = Counter()
    hidden_to_rope = 0
    hidden_to_snare = 0
    monotonicity_violations = 0

    for r in triggered:
        pcd = r.get('perspective_chi_detail', {})
        bp = r.get('delta_band_positions', {})
        topic_domain = r.get('topic_domain', '')
        domain_counts[infer_domain_category(topic_domain)] += 1

        for pos in bp:
            chi_detail = pcd.get(pos, {})
            chi = chi_detail.get('chi', 0)
            eps = chi_detail.get('epsilon', r.get('epsilon', 0))
            d = chi_detail.get('d', 0)
            sm = chi_detail.get('scope_mod', 1.0)

            ext = compute_extended_range(eps, d, sm, chi)
            rspan = classify_range_span(ext)
            span_counts[rspan['span']] += 1

            for cx in rspan['boundary_crossings']:
                crossing_deltas.append(cx['delta_approx'])

            # Hidden range: tangled_rope at baseline reaching rope or snare
            baseline_cls = classify_chi(chi)
            if baseline_cls == 'tangled_rope-range':
                classes = [e['classification'] for e in ext['extended']]
                if 'rope-range' in classes:
                    hidden_to_rope += 1
                if 'snare-range' in classes:
                    hidden_to_snare += 1

            # Monotonicity check
            all_pts = sorted(ext['extended'] + [{'delta': 0.0, 'chi': chi}],
                             key=lambda p: p['delta'])
            for i in range(1, len(all_pts)):
                if all_pts[i]['chi'] < all_pts[i-1]['chi'] - 0.0001:
                    monotonicity_violations += 1
                    break

    lines.append("### Range Span Distribution")
    lines.append("")
    lines.append("| Boundaries Crossed | Count | % |")
    lines.append("|-------------------:|------:|--:|")
    total_spans = sum(span_counts.values())
    for span in sorted(span_counts.keys()):
        pct = 100 * span_counts[span] / total_spans if total_spans else 0
        lines.append(f"| {span} | {span_counts[span]} | {pct:.0f}% |")
    lines.append("")

    # Crossing δ distribution
    if crossing_deltas:
        near_std = sum(1 for d in crossing_deltas if abs(d) <= 0.10)
        extended_range_ct = sum(1 for d in crossing_deltas if abs(d) > 0.10)
        lines.append(f"Crossing δ distribution: {len(crossing_deltas)} total crossings")
        lines.append(f"  Near standard range (|δ| ≤ 0.10): {near_std}")
        lines.append(f"  Extended range (|δ| > 0.10): {extended_range_ct}")
        lines.append("")

    # Domain category coverage
    lines.append("### Domain Category Coverage")
    lines.append("")
    lines.append("| Domain | Count | % |")
    lines.append("|--------|------:|--:|")
    for dom, ct in domain_counts.most_common():
        pct = 100 * ct / len(triggered) if triggered else 0
        lines.append(f"| {dom} | {ct} | {pct:.0f}% |")
    lines.append("")

    # Hidden range
    lines.append("### Hidden Range (Baseline tangled_rope Reaching Other Zones)")
    lines.append("")
    lines.append(f"Tangled_rope at baseline → rope under extended negative δ: {hidden_to_rope}")
    lines.append(f"Tangled_rope at baseline → snare under extended positive δ: {hidden_to_snare}")
    lines.append("")

    # Monotonicity
    lines.append("### Monotonicity Validation")
    lines.append("")
    if monotonicity_violations == 0:
        lines.append("All computed ranges are monotonically increasing with δ. No violations.")
    else:
        lines.append(f"**WARNING**: {monotonicity_violations} monotonicity violation(s) detected.")
    lines.append("")

    return "\n".join(lines)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    base_dir = Path(__file__).parent.parent
    enriched_path = base_dir / 'outputs' / 'enriched_pipeline.json'
    report_dir = base_dir / 'outputs' / 'constraint_reports'

    dry_run = '--dry-run' in sys.argv
    audit_only = '--audit-only' in sys.argv

    if not enriched_path.exists():
        print(f"ERROR: {enriched_path} not found", file=sys.stderr)
        sys.exit(1)

    print(f"Running CCDP audit on {enriched_path}...")
    results, audit_meta = run_corpus_audit(str(enriched_path))

    # Summary
    triggered = [r for r in results if r['trigger']]
    h1_ge3 = [r for r in results if r['h1'] >= 3]
    deep_fracture = [r for r in h1_ge3 if not r['trigger']]
    curvature_alerted = [r for r in results if r.get('curvature_alert')]
    print(f"  Total constraints: {len(results)}")
    print(f"  H¹ ≥ 3: {len(h1_ge3)}")
    print(f"  CCDP triggered: {len(triggered)}")
    print(f"  Deep fracture: {len(deep_fracture)}")
    print(f"  Curvature alerts: {len(curvature_alerted)}")

    # Generate audit report
    audit_report = generate_audit_report(results, str(report_dir), meta=audit_meta)
    audit_path = base_dir / 'outputs' / 'ccdp_audit_report.md'

    if dry_run:
        print(f"\n[DRY RUN] Would write audit report to {audit_path}")
        print(f"[DRY RUN] Would annotate constraint reports in {report_dir}")
    else:
        with open(audit_path, 'w') as f:
            f.write(audit_report)
        print(f"  Audit report written to {audit_path}")

    if audit_only or dry_run:
        if not dry_run:
            print("  --audit-only: skipping report annotations")
        return

    # Annotate individual reports
    annotated = 0
    margin_annotated = 0
    margin_skipped = 0
    skipped_no_report = 0
    skipped_existing = 0
    skipped_no_marker = 0

    for r in results:
        cid = r['id']
        report_path = report_dir / f"{cid}_report.md"

        if not report_path.exists():
            skipped_no_report += 1
            continue

        h1 = r['h1']

        if h1 == 0:
            # Don't annotate H¹=0 reports
            continue

        if r['trigger']:
            annotation = generate_ccdp_annotation(
                constraint_name=cid,
                chi_values=r['chi_values'],
                h1=h1,
                threshold_distances={pos: (info['distance'], info['threshold'], info['direction'])
                                     for pos, info in r['threshold_distances'].items()},
                contamination=r.get('contamination', {}),
                orbit_signature=r.get('signature', ''),
                active_theorems=[],
                perspective_chi_detail=r.get('perspective_chi_detail', {}),
            )
        elif h1 >= 3:
            annotation = generate_deep_fracture_annotation(
                constraint_name=cid,
                h1=h1,
                chi_values=r['chi_values'],
                threshold_distances={pos: (info['distance'], info['threshold'], info['direction'])
                                     for pos, info in r['threshold_distances'].items()},
                perspective_chi_detail=r.get('perspective_chi_detail', {}),
            )
        else:
            continue

        success = append_to_report(str(report_path), annotation)
        if success:
            annotated += 1
        else:
            # Check why
            with open(report_path, 'r') as f:
                content = f.read()
            if "CULTURAL COGNITION" in content or "CCDP:" in content:
                skipped_existing += 1
            else:
                skipped_no_marker += 1

        # Margin Analysis: append for triggered constraints (whether CCDP was just added or already existed)
        if r['trigger']:
            margin = generate_margin_analysis(
                constraint_name=cid,
                chi_values=r['chi_values'],
                epsilon=r['epsilon'],
                perspective_chi_detail=r.get('perspective_chi_detail', {}),
                h1=h1,
                threshold_distances={pos: (info['distance'], info['threshold'], info['direction'])
                                     for pos, info in r['threshold_distances'].items()},
                contamination=r.get('contamination', {}),
                topic_domain=r.get('topic_domain', ''),
            )
            margin_ok = append_margin_to_report(str(report_path), margin)
            if margin_ok:
                margin_annotated += 1
            else:
                margin_skipped += 1

    print(f"  Reports annotated: {annotated}")
    print(f"  Margin analyses added: {margin_annotated}")
    print(f"  Margin skipped (already exists or no CCDP): {margin_skipped}")
    print(f"  Skipped (no report file): {skipped_no_report}")
    print(f"  Skipped (already annotated): {skipped_existing}")
    print(f"  Skipped (no insertion marker): {skipped_no_marker}")


if __name__ == '__main__':
    main()
