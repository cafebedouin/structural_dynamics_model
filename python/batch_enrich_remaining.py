#!/usr/bin/env python3
"""
Batch Enrichment Script for Remaining 26 Constraints

Handles:
- Missing analytical perspectives (classification blocks)
- Missing beneficiary/victim predicates
- Missing omega variables
- Missing claim/interval/enforcement flags
- ID mismatches between corpus ID and internal Prolog ID

Each enrichment block is appended to the end of the target file.
"""

import json
import re
from pathlib import Path


def detect_internal_id(filepath):
    """Detect the internal constraint ID used in a file's predicates."""
    content = filepath.read_text()
    # Look for base_extractiveness(ID, ...) as the canonical internal ID
    m = re.search(r'base_extractiveness\((\w+),', content)
    if m:
        return m.group(1)
    # Fallback: look for constraint_claim(ID, ...)
    m = re.search(r'constraint_claim\((\w+),', content)
    if m:
        return m.group(1)
    # Fallback: look for interval(ID, ...)
    m = re.search(r'interval\((\w+),', content)
    if m:
        return m.group(1)
    return None


def has_predicate(content, pattern):
    return bool(re.search(pattern, content))


def classify_from_chi(chi, eps, suppression, theater, claimed_type):
    """Classify using February 2026 gate thresholds."""
    # Priority order: Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton
    if eps <= 0.25 and suppression <= 0.05:
        return 'mountain'
    if chi >= 0.66 and eps >= 0.46 and suppression >= 0.60:
        return 'snare'
    if chi <= 0.30 and theater <= 0.70:
        return 'scaffold'
    if chi <= 0.35 and eps <= 0.45:
        return 'rope'
    if 0.40 <= chi <= 0.90 and eps >= 0.30 and suppression >= 0.40:
        return 'tangled_rope'
    if chi <= 0.25 and eps > 0.10 and theater >= 0.70:
        return 'piton'
    # Fallback: use claimed type or nearest
    if claimed_type:
        return claimed_type
    return 'scaffold'


# ============================================================================
# ENRICHMENT DATA — hand-curated from file narratives
# ============================================================================

ENRICHMENTS = {
    # --- Missing Analytical Perspectives (2) ---
    'broke_vs_poor_grocery_math': {
        'gaps': ['ANALYTICAL_PERSPECTIVE'],
        'analytical_class': None,  # will be computed
    },
    'portugal_government_stability_ad': {
        'gaps': ['ANALYTICAL_PERSPECTIVE'],
        'analytical_class': None,
    },

    # --- Missing Structural Flags (20) ---
    'choice_architecture_design': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'OMEGA'],
        'beneficiary': 'choosers_with_good_defaults',
        'victim': 'manipulated_consumers',
        'omega_id': 'omega_dark_pattern_drift',
        'omega_q': "Will the proliferation of 'Dark Patterns' cause regulatory frameworks to reclassify choice architecture from Rope to Snare?",
        'omega_res': "Longitudinal analysis of EU Digital Services Act enforcement and FTC dark pattern litigation outcomes.",
        'omega_impact': "If regulated: Remains a Rope with guardrails. If unchecked: Drifts toward a systemic Snare.",
        'omega_conf': 'medium',
    },
    'columbia_2026_elections': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'OMEGA'],
        'beneficiary': 'political_elites',
        'victim': 'marginalized_voters',
        'omega_id': 'omega_colombian_polarization',
        'omega_q': "Will the two-round system intensify polarization to the point where centrist alternatives are structurally eliminated?",
        'omega_res': "Post-election analysis of vote fragmentation and centrist party viability after the May 2026 results.",
        'omega_impact': "If centrists survive: The constraint remains a Rope. If eliminated: It hardens into a Mountain of binary choice.",
        'omega_conf': 'medium',
    },
    'emotional_cycles_of_change': {
        'gaps': ['CLAIM', 'ENFORCEMENT', 'INTERVAL', 'OMEGA'],
        'claim': 'tangled_rope',
        'omega_id': 'omega_valley_of_despair_depth',
        'omega_q': "Is the 'Valley of Despair' a fixed neurochemical depth (Mountain) or a variable influenced by coaching/support (Scaffold)?",
        'omega_res': "Comparative study of coached vs. uncoached adoption cycles measuring cortisol and completion rates.",
        'omega_impact': "If fixed: The cycle is a permanent Mountain of human limitation. If variable: Coaching transforms the Snare into a manageable Scaffold.",
        'omega_conf': 'medium',
    },
    'epstein_kgb_honeytrap': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'intelligence_operators',
        'victim': 'compromised_associates',
    },
    'fast_growing_hierarchy': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'OMEGA'],
        'beneficiary': 'proof_theorists',
        'victim': 'none',
        'omega_id': 'omega_fgh_church_turing',
        'omega_q': "Does the FGH hierarchy reflect an absolute limit of computation, or could non-standard models of arithmetic reveal computable functions beyond its reach?",
        'omega_res': "Advances in reverse mathematics and the relationship between proof-theoretic ordinals and computational complexity.",
        'omega_impact': "If absolute: The Mountain is permanent. If non-standard models apply: The hierarchy's scope narrows to a specific axiom system.",
        'omega_conf': 'high',
    },
    'france_cordon_sanitaire_2026': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'establishment_parties',
        'victim': 'excluded_populist_voters',
    },
    'material_tensile_strength': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'OMEGA'],
        'beneficiary': 'structural_engineers',
        'victim': 'none',
        'omega_id': 'omega_metamaterial_bypass',
        'omega_q': "Could engineered metamaterials or nano-scale structures effectively bypass the classical tensile strength limits of bulk materials?",
        'omega_res': "Laboratory testing of carbon nanotube composites and graphene structures at macro-scale loads.",
        'omega_impact': "If bypassed: The Mountain is conditional on material scale. If not: The physical law remains absolute.",
        'omega_conf': 'medium',
    },
    'mom_z14_galaxy_2026': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'ANALYTICAL_PERSPECTIVE'],
        'beneficiary': 'cosmological_research_programs',
        'victim': 'superseded_formation_models',
        'analytical_class': None,
    },
    'narcissistic_ego_maintenance': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'narcissists_self_image',
        'victim': 'emotional_labor_providers',
    },
    'new_civilizational_rope': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'citizen_auditors',
        'victim': 'legacy_rent_seeking_bureaucracies',
    },
    'open_source_commons': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'CLASSIFICATIONS', 'ANALYTICAL_PERSPECTIVE', 'OMEGA'],
        'beneficiary': 'open_source_contributors',
        'victim': 'none',
        'analytical_class': None,
        'extra_classifications': [
            {
                'type': 'rope',
                'power': 'institutional',
                'time': 'generational',
                'exit': 'mobile',
                'scope': 'global',
                'comment': 'PERSPECTIVE 2: THE PROJECT MAINTAINER (ROPE)\n% Institutional actors use the commons as coordination infrastructure.',
            },
        ],
        'omega_id': 'omega_oss_sustainability',
        'omega_q': "Can open-source commons survive the 'Maintainer Burnout' problem without introducing extractive funding mechanisms?",
        'omega_res': "Analysis of long-term sustainability models (sponsorships, foundations, cooperatives) vs. maintainer attrition rates.",
        'omega_impact': "If sustainable: Remains a pure Rope. If burnout prevails: Drifts toward a Scaffold requiring external support.",
        'omega_conf': 'medium',
    },
    'perseverance_rover_autonomy': {
        'gaps': ['VICTIM'],
        'victim': 'none',
    },
    'rare_earth_coop_2026': {
        'gaps': ['VICTIM', 'POWERLESS_PERSPECTIVE'],
        'victim': 'non_member_market_participants',
        'powerless_classification': {
            'type': 'rope',
            'power': 'powerless',
            'time': 'immediate',
            'exit': 'mobile',
            'scope': 'global',
            'comment': 'PERSPECTIVE 3: THE SMALL MANUFACTURER (ROPE)\n% Even non-members benefit from price stabilization; membership is voluntary.',
        },
    },
    'rogue_wave_control_2026': {
        'gaps': ['BENEFICIARY', 'VICTIM', 'ANALYTICAL_PERSPECTIVE'],
        'beneficiary': 'photonics_researchers',
        'victim': 'none',
        'analytical_class': None,
    },
    'riot_incentive_loop_2026': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'security_apparatus',
        'victim': 'civilian_protesters',
    },
    'spv_variations_us_cold': {
        'gaps': ['VICTIM'],
        'victim': 'affected_populations_during_cold_outbreaks',
    },
    'square_cube_law': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'engineering_disciplines',
        'victim': 'none',
    },
    'thai_article_112_mountain': {
        'gaps': ['BENEFICIARY', 'VICTIM'],
        'beneficiary': 'royalist_establishment',
        'victim': 'progressive_political_movements',
    },
    'winter_olympics_2026': {
        'gaps': ['VICTIM'],
        'victim': 'none',
    },

    # --- Missing Omega Only (4) ---
    'ergo_nipopows': {
        'gaps': ['OMEGA'],
        'omega_id': 'omega_nipopow_velvet_fork',
        'omega_q': "Can a velvet fork or soft fork break NiPoPoW assumptions by altering the superblock interlink structure?",
        'omega_res': "Formal verification of NiPoPoW security proofs against protocol upgrade scenarios on Ergo mainnet.",
        'omega_impact': "If vulnerable: The Rope weakens under governance disputes. If robust: Security guarantee holds across forks.",
        'omega_conf': 'medium',
    },
    'rfc9293_interoperability': {
        'gaps': ['OMEGA'],
        'omega_id': 'omega_tcp_ossification',
        'omega_q': "Will middlebox ossification permanently prevent TCP extension deployment, effectively converting the protocol from a Rope into a Mountain?",
        'omega_res': "Longitudinal measurement of TCP option negotiation success rates across diverse network paths.",
        'omega_impact': "If ossified: TCP becomes an immutable Mountain. If extensible: Remains a living Rope that can adapt.",
        'omega_conf': 'medium',
    },
    'shannon_entropy_limit': {
        'gaps': ['OMEGA'],
        'omega_id': 'omega_quantum_channel_capacity',
        'omega_q': "Does quantum information theory reveal channel capacities that fundamentally exceed Shannon's classical limits?",
        'omega_res': "Experimental verification of quantum channel coding theorems and practical quantum error correction demonstrations.",
        'omega_impact': "If exceeded: The classical Mountain has a quantum escape route. If not: Shannon's limits remain absolute.",
        'omega_conf': 'high',
    },
    'social_narrative_casting': {
        'gaps': ['OMEGA'],
        'omega_id': 'omega_criticism_objectivity',
        'omega_q': "Can criticism ever reflect an objective truth about the subject, or is it always a projection of the critic's internal narrative?",
        'omega_res': "Cross-cultural studies of criticism reception and longitudinal tracking of projected vs. observed behavioral patterns.",
        'omega_impact': "If objective: Criticism is a Rope for self-improvement. If always projection: It remains a Snare until consciously reframed.",
        'omega_conf': 'medium',
    },
}


# ============================================================================
# CORPUS DATA — needed for chi calculation on analytical perspectives
# ============================================================================

METRICS = {
    'broke_vs_poor_grocery_math': {'eps': 0.80, 'sup': 0.60, 'tr': 0.10, 'claim': 'mountain'},
    'portugal_government_stability_ad': {'eps': 0.35, 'sup': 0.45, 'tr': 0.30, 'claim': 'scaffold'},
    'mom_z14_galaxy_2026': {'eps': 0.05, 'sup': 0.45, 'tr': 0.02, 'claim': 'mountain'},
    'rogue_wave_control_2026': {'eps': 0.15, 'sup': 0.10, 'tr': 0.05, 'claim': 'mountain'},
    'open_source_commons': {'eps': 0.05, 'sup': 0.10, 'tr': 0.15, 'claim': 'rope'},
}

# Internal ID mapping: corpus_id -> file's internal prolog ID
ID_MAP = {
    'choice_architecture_design': 'rope_design',
    'columbia_2026_elections': 'colombia_2026_presidential_election',
    'emotional_cycles_of_change': 'emotional_cycles_2026',
    'epstein_kgb_honeytrap': 'epstein_honeytrap',
    'new_civilizational_rope': 'decentralized_infrastructure_rope',
    'mom_z14_galaxy_2026': 'mom_z14_2026',
    'winter_olympics_2026': 'milano_cortina_2026',
    'spv_variations_us_cold': 'spv_variations',
}


def generate_enrichment_block(corpus_id, enrichment, filepath):
    """Generate the enrichment block text for a given constraint."""
    content = filepath.read_text()
    internal_id = ID_MAP.get(corpus_id)
    if not internal_id:
        internal_id = detect_internal_id(filepath) or corpus_id

    lines = []
    lines.append('')
    lines.append('% ============================================================================')
    lines.append('% ENRICHMENT: Structural predicates for remaining gaps')
    lines.append('% Generated: 2026-02-08')
    lines.append('% Template: v5.2 namespace alignment')
    lines.append(f'% Source: Derived from narrative context in this file ({corpus_id})')
    lines.append('% ============================================================================')

    gaps = enrichment['gaps']

    # --- Beneficiary ---
    if 'BENEFICIARY' in gaps:
        ben = enrichment['beneficiary']
        if not has_predicate(content, r'constraint_beneficiary\('):
            lines.append(f'constraint_beneficiary({internal_id}, {ben}).')

    # --- Victim ---
    if 'VICTIM' in gaps:
        vic = enrichment['victim']
        if not has_predicate(content, r'constraint_victim\('):
            lines.append(f'constraint_victim({internal_id}, {vic}).')

    # --- Claim ---
    if 'CLAIM' in gaps:
        claim = enrichment['claim']
        if not has_predicate(content, r'constraint_claim\('):
            lines.append(f'narrative_ontology:constraint_claim({internal_id}, {claim}).')

    # --- Enforcement ---
    if 'ENFORCEMENT' in gaps:
        if not has_predicate(content, r'requires_active_enforcement\('):
            lines.append(f'domain_priors:requires_active_enforcement({internal_id}).')

    # --- Interval ---
    if 'INTERVAL' in gaps:
        if not has_predicate(content, r'interval\('):
            lines.append(f'narrative_ontology:interval({internal_id}, 0, 10).')

    # --- Analytical Perspective ---
    if 'ANALYTICAL_PERSPECTIVE' in gaps:
        m = METRICS.get(corpus_id)
        if m:
            eps = m['eps']
            sup = m['sup']
            tr = m['tr']
            claimed = m['claim']
            chi = eps * 1.15 * 1.2
            aclass = classify_from_chi(chi, eps, sup, tr, claimed)
            lines.append(f'')
            lines.append(f'% --- Analytical perspective classification ---')
            lines.append(f'% chi = {eps} * 1.15 (analytical) * 1.2 (global) = {chi:.3f}')
            lines.append(f'% Classification: {aclass}')
            lines.append(f'constraint_indexing:constraint_classification({internal_id}, {aclass},')
            lines.append(f'    context(agent_power(analytical),')
            lines.append(f'            time_horizon(civilizational),')
            lines.append(f'            exit_options(analytical),')
            lines.append(f'            spatial_scope(global))).')

    # --- Extra Classifications (e.g., open_source_commons needs more perspectives) ---
    if 'CLASSIFICATIONS' in gaps:
        for cls in enrichment.get('extra_classifications', []):
            lines.append(f'')
            lines.append(f'% {cls["comment"]}')
            lines.append(f'constraint_indexing:constraint_classification({internal_id}, {cls["type"]},')
            lines.append(f'    context(agent_power({cls["power"]}),')
            lines.append(f'            time_horizon({cls["time"]}),')
            lines.append(f'            exit_options({cls["exit"]}),')
            lines.append(f'            spatial_scope({cls["scope"]}))).')

    # --- Powerless Perspective ---
    if 'POWERLESS_PERSPECTIVE' in gaps:
        cls = enrichment.get('powerless_classification', {})
        if cls:
            lines.append(f'')
            lines.append(f'% {cls["comment"]}')
            lines.append(f'constraint_indexing:constraint_classification({internal_id}, {cls["type"]},')
            lines.append(f'    context(agent_power({cls["power"]}),')
            lines.append(f'            time_horizon({cls["time"]}),')
            lines.append(f'            exit_options({cls["exit"]}),')
            lines.append(f'            spatial_scope({cls["scope"]}))).')

    # --- Omega Variable ---
    if 'OMEGA' in gaps:
        oid = enrichment.get('omega_id', f'omega_{corpus_id}')
        oq = enrichment.get('omega_q', '')
        ores = enrichment.get('omega_res', '')
        oimpact = enrichment.get('omega_impact', '')
        oconf = enrichment.get('omega_conf', 'medium')
        lines.append(f'')
        lines.append(f'omega_variable(')
        lines.append(f'    {oid},')
        lines.append(f'    "{oq}",')
        lines.append(f'    "{ores}",')
        lines.append(f'    "{oimpact}",')
        lines.append(f'    confidence_without_resolution({oconf})')
        lines.append(f').')

    lines.append('')

    return '\n'.join(lines)


def main():
    base = Path(__file__).resolve().parent.parent
    testsets = base / 'prolog' / 'testsets'

    enriched = 0
    skipped = 0

    for corpus_id, enrichment in sorted(ENRICHMENTS.items()):
        filepath = testsets / f'{corpus_id}.pl'
        if not filepath.exists():
            print(f'  SKIP {corpus_id}: file not found')
            skipped += 1
            continue

        block = generate_enrichment_block(corpus_id, enrichment, filepath)

        # Only append if block has content beyond the header
        content_lines = [l for l in block.strip().split('\n')
                        if l and not l.startswith('%')]
        if not content_lines:
            print(f'  SKIP {corpus_id}: no new predicates to add')
            skipped += 1
            continue

        # Append to file
        with open(filepath, 'a') as f:
            f.write(block)

        enriched += 1
        gaps = enrichment['gaps']
        print(f'  OK   {corpus_id}: +{", ".join(gaps)}')

    print(f'\nDone: {enriched} enriched, {skipped} skipped')


if __name__ == '__main__':
    main()
