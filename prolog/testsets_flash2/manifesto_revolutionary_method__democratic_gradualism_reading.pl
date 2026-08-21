% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic Gradualism for Socialist Transformation
 *   domain: political_philosophy/revolutionary_theory/historical_materialism
 *
 * SUMMARY:
 *   This constraint represents the 'democratic gradualism' reading of the
 *   manifesto_revolutionary_method kernel. It posits that socialism is
 *   achievable through democratic electoral majorities and gradual
 *   institutional reform, with working-class power exercised through existing
 *   democratic structures. This reading emphasizes institutional continuity
 *   with liberal democracy, benefiting social democratic parties and trade
 *   unions, while suppressing revolutionary militants. The moderate
 *   extractiveness (0.40) reflects the inherent limitations and compromises
 *   of working within existing systems, which can dilute radical aims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.3).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic Gradualism for Socialist Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political_philosophy/revolutionary_theory/historical_materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '89c9e6e3-ea10-4bef-aa76-ad2607a5e71e').
narrative_ontology:cs_kernel_codification('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', formalized).
narrative_ontology:cs_authority_grounding('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', lineage).
narrative_ontology:cs_interpretation_layer_present('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e').
narrative_ontology:cs_reading_relation('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', foundational, parliamentary_path_to_socialism).
narrative_ontology:cs_axiom_status(parliamentary_path_to_socialism, holdable).
narrative_ontology:cs_axiom_grounding('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', parliamentary_path_to_socialism, conventional).
narrative_ontology:cs_axiom('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', foundational, democratic_legitimacy_of_reform).
narrative_ontology:cs_axiom_status(democratic_legitimacy_of_reform, holdable).
narrative_ontology:cs_axiom_grounding('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', democratic_legitimacy_of_reform, deontological).
narrative_ontology:cs_reference_frame('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', kautskyist_parliamentary_socialism).
narrative_ontology:cs_drift_state('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('89c9e6e3-ea10-4bef-aa76-ad2607a5e71e', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for socialist transformation through parliamentary means, electoral victories, and gradual policy reforms within existing liberal democratic frameworks. They benefit from the legitimacy and operational space provided by this reading, which positions them as the primary agents of change.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, constrained, national).

% Operate within the existing democratic system, using collective bargaining and political lobbying to advance working-class interests. They benefit from the stability and recognized channels of influence that democratic gradualism offers, avoiding the risks of revolutionary upheaval.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, trade_unions, beneficiary,
    organized, biographical, constrained, national).

% Participates in the democratic process, voting for parties that promise socialist reforms. They are the intended beneficiaries of gradual reforms and the source of electoral power, but their agency is mediated through representative institutions.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_working_class, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for immediate, extra-parliamentary action and revolutionary rupture. They are suppressed and marginalized by the democratic gradualist framework, often labeled as 'adventurist' or 'extremist,' losing legitimacy and resources within the mainstream political discourse.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    powerless, immediate, trapped, local).

% Seek more fundamental and rapid change than gradualism allows, but often find themselves operating within the constraints of the existing democratic system. They bear the cost of being outmaneuvered or co-opted by mainstream social democratic forces, and their alternatives are often delegitimized.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, radical_left_factions, payer,
    moderate, biographical, constrained, national).

% The existing institutional framework through which gradualist reforms are pursued. It provides the arena for political action and benefits from the stability and legitimacy conferred by this reading, which avoids revolutionary challenges to its authority.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, agenda_setter,
    institutional, generational, arbitrage, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the efforts of working-class movements and political parties towards a common goal of socialist transformation through established, non-violent, and legitimate democratic channels, preventing fragmentation and revolutionary adventurism.
% TRANSFER_FUNCTION: Transfers political legitimacy and resources from revolutionary or extra-parliamentary methods to electoral and institutional reform processes, channeling working-class power into existing state structures.
% ABSENT_VOICES: Those advocating for immediate, non-electoral, or violent revolutionary change are systematically excluded from the legitimate discourse, their methods deemed illegitimate or counterproductive by the dominant gradualist framework.
% DISAPPEARANCE_RATIONALE: If the belief in democratic gradualism vanished, social democratic parties would lose their core mandate, trade unions would face pressure for more radical action, and the political landscape would fragment into revolutionary and reactionary camps, leading to significant political instability and potentially violent conflict.
% FOUNDING_PROBLEM: The historical problem of achieving socialist goals without resorting to violent revolution or authoritarian vanguardism, seeking a path that respects individual liberties and democratic norms.
% FOUNDING_PROBLEM_CORROBORATION: Historians of socialist movements and political theorists from across the spectrum acknowledge the historical challenge of reconciling socialist aims with democratic means. Social democratic parties and many trade unions continue to attest that this problem remains live, requiring ongoing commitment to democratic processes.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because while it channels working-class energy into a specific, often slow, path, it also delivers tangible reforms and avoids the high costs of revolution. Suppression (0.30) is present in the delegitimization and marginalization of more radical alternatives, but it's not overtly coercive. Theater ratio (0.15) is low, as the commitment to democratic processes is largely genuine, though some performative aspects exist in electoral politics. The time series reflects a period of increasing institutionalization and some disillusionment, but overall stability in the core metrics.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of social democratic parties, this is a genuine 'rope' for coordinating broad-based political action. From the perspective of revolutionary militants, it functions more like a 'snare' that co-opts and neutralizes genuine revolutionary potential, channeling it into ineffective reformism. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Social democratic parties and trade unions are beneficiaries, as this reading legitimizes their methods and provides a framework for their operation. The electoral working class is also a beneficiary, as their power is recognized and channeled, even if indirectly. Revolutionary militants and radical left factions are victims, as their preferred methods are actively suppressed or delegitimized, forcing them into a constrained or trapped position relative to the dominant political discourse.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (achieving socialism democratically) is still live, though its effectiveness is contested. The classification as 'rope' (claimed) with moderate extractiveness acknowledges the coordination function while also capturing the costs borne by those whose alternatives are suppressed. It avoids mislabeling as a pure snare by recognizing the genuine benefits and agency within the democratic framework, but also avoids a naive 'mountain' classification by acknowledging the active enforcement and suppression of alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    efficacy_of_gradualism,
    'Is democratic gradualism genuinely capable of achieving fundamental socialist transformation, or does it inherently lead to co-optation and reformism that perpetuates capitalism?',
    'Long-term historical analysis of social democratic governments'' ability to fundamentally alter capitalist property relations and power structures, rather than merely ameliorating capitalism''s effects.',
    'If found to be inherently reformist, the extractiveness for the working class would be re-evaluated as higher, as their political energy is channeled into a system that cannot deliver its promised outcome, potentially reclassifying it closer to a Snare for the working class.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_gradualism, empirical, 'The actual transformative power of democratic gradualism versus its tendency towards reformism.').

omega_variable(
    legitimacy_of_revolutionary_alternatives,
    'Is the suppression of revolutionary alternatives by democratic gradualism a legitimate defense of democratic norms, or an extractive mechanism to maintain the power of established political actors?',
    'Comparative analysis of historical outcomes: do societies that suppress revolutionary alternatives achieve more equitable and democratic outcomes than those that allow for more radical political contestation?',
    'If the suppression is primarily extractive, the overall suppression metric would be higher, and the classification for revolutionary militants would shift further towards ''trapped'' or ''snare'', indicating a more coercive dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_revolutionary_alternatives, conceptual, 'The normative justification for suppressing revolutionary political methods.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mani_tr_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(mani_tr_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1930, 0.12).
narrative_ontology:measurement(mani_tr_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(mani_tr_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(mani_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(mani_be_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(mani_be_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1930, 0.35).
narrative_ontology:measurement(mani_be_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(mani_be_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1990, 0.42).
narrative_ontology:measurement(mani_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(mani_su_t1900, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1900, 0.2).
narrative_ontology:measurement(mani_su_t1930, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement(mani_su_t1960, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1960, 0.3).
narrative_ontology:measurement(mani_su_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1990, 0.28).
narrative_ontology:measurement(mani_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_party_discipline).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, trade_union_political_strategy).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'manifesto_revolutionary_method' kernel, focusing on democratic gradualism. It is linked to other readings (vanguard_rupture_reading, council_communist_reading) which represent alternative methods of socialist transformation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
