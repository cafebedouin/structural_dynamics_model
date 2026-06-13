% ============================================================================
% CONSTRAINT STORY: magna_carta_constraint_authority__feudal_obsolescence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_constraint_authority__feudal_obsolescence_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_constraint_authority__feudal_obsolescence_reading
 *   human_readable: Magna Carta's Feudal Obsolescence (Executive Discretion Reading)
 *   domain: constitutional_history/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal obsolescence' reading of Magna
 *   Carta's authority, arguing that the 1215 charter was a specific baronial
 *   compact with no enduring constitutional force over modern sovereign
 *   states. This reading serves to maximize executive discretion and
 *   parliamentary power by dismissing historical constraints. It is presented
 *   as a historical truth, but functions as a Snare by extracting power from
 *   other constitutional actors and citizens who might appeal to charter
 *   rights.
 *
 * KEY AGENTS:
 *   - executive_branch: Primary beneficiary (institutional/arbitrage) — gains discretion
 *   - parliamentary_majority: Secondary beneficiary (institutional/arbitrage) — gains legislative freedom
 *   - popular_constitutionalists: Primary victim (organized/constrained) — loses historical grounding for rights claims
 *   - juridical_restraint_advocates: Victim (organized/constrained) — loses precedent for judicial review
 *   - citizens_seeking_charter_rights: Victim (powerless/constrained) — loses appeal to historical rights
 *   - legal_historians: Observer (analytical/analytical) — analyzes historical context and contemporary interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.65).
domain_priors:suppression_score(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.7).
domain_priors:theater_ratio(magna_carta_constraint_authority__feudal_obsolescence_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(magna_carta_constraint_authority__feudal_obsolescence_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_constraint_authority__feudal_obsolescence_reading, snare).
narrative_ontology:human_readable(magna_carta_constraint_authority__feudal_obsolescence_reading, "Magna Carta's Feudal Obsolescence (Executive Discretion Reading)").
narrative_ontology:topic_domain(magna_carta_constraint_authority__feudal_obsolescence_reading, "constitutional_history/legal_philosophy/political_theory").

domain_priors:requires_active_enforcement(magna_carta_constraint_authority__feudal_obsolescence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_constraint_authority__feudal_obsolescence_reading, '1841eee0-967a-4fb5-82b3-8c0eab9e4bbc').
narrative_ontology:cs_kernel_codification('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', fixed_text).
narrative_ontology:cs_authority_grounding('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', extraction).
narrative_ontology:cs_interpretation_layer_present('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc').
narrative_ontology:cs_reading_relation('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', magna_carta_constraint_authority__living_constitutionalism_reading, forecloses).
narrative_ontology:cs_reading_relation('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', magna_carta_constraint_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', foundational, magna_carta_feudal_specific).
narrative_ontology:cs_axiom_status(magna_carta_feudal_specific, holdable).
narrative_ontology:cs_axiom_grounding('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', magna_carta_feudal_specific, empirically_contingent).
narrative_ontology:cs_axiom('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', foundational, modern_sovereignty_unbound_by_ancient_charters).
narrative_ontology:cs_axiom_status(modern_sovereignty_unbound_by_ancient_charters, holdable).
narrative_ontology:cs_axiom_grounding('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', modern_sovereignty_unbound_by_ancient_charters, conventional).
narrative_ontology:cs_reference_frame('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', id_13th_century_feudal_compact).
narrative_ontology:cs_drift_state('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', contemporary_constitutional_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1841eee0-967a-4fb5-82b3-8c0eab9e4bbc', '').
narrative_ontology:cs_kernel_id(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, executive_branch).
narrative_ontology:constraint_beneficiary(magna_carta_constraint_authority__feudal_obsolescence_reading, parliamentary_majority).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, popular_constitutionalists).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, juridical_restraint_advocates).
narrative_ontology:constraint_victim(magna_carta_constraint_authority__feudal_obsolescence_reading, citizens_seeking_charter_rights).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_constraint_authority__feudal_obsolescence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_constraint_authority__feudal_obsolescence_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_constraint_authority__feudal_obsolescence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_constraint_authority__feudal_obsolescence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-to-high, reflecting the significant power transfer to the executive and parliament. Suppression (0.70) is high because this reading actively dismisses and marginalizes alternative interpretations that assert Magna Carta's enduring relevance. The theater ratio (0.40) indicates that while some genuine historical scholarship supports the limited original scope, a substantial portion of its contemporary invocation is performative, aimed at justifying expanded state power rather than pure historical accuracy. The constraint is claimed as a Snare because its coordination story (historical accuracy) is cover for extraction (power consolidation).
 *
 * PERSPECTIVAL GAP:
 *   The executive and parliamentary beneficiaries experience this as a liberation from anachronistic constraints, enabling efficient governance. The victims, however, experience it as a loss of fundamental rights and checks on power, a historical narrative used to justify unchecked authority. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The executive branch and parliamentary majority are clear beneficiaries (d near 0.0) as this reading grants them greater freedom from historical and judicial oversight. Popular constitutionalists, juridical restraint advocates, and citizens seeking charter rights are victims (d near 1.0) as their claims are undermined by the assertion of obsolescence. Legal historians are analytical observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This reading itself is a form of mandatrophy resolution, asserting that Magna Carta's mandate is 'dead' in a modern context. However, the framework classifies it as a Snare because this 'resolution' is not benign; it enables extraction by dismissing historical checks on power. The classification prevents mislabeling this as a neutral historical observation when it actively facilitates power consolidation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine historical assessment of Magna Carta''s limited original scope, or a strategic reading to justify expanded executive power?',
    'Historical and legal scholarship on the intent and reception of Magna Carta in the 13th century, compared with contemporary political rhetoric invoking its obsolescence.',
    'If a genuine historical assessment, the constraint accurately reflects a historical truth. If a strategic reading, it functions as a Snare, using historical narrative to extract power from other branches/citizens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''feudal obsolescence'' reading of the ''magna_carta_constraint_authority'' kernel. Sibling readings (living_constitutionalism_reading, parliamentary_sovereignty_reading) would assert ongoing or transformed authority.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative readings structural (e.g., through judicial appointments) or internalized (e.g., through a dominant historical narrative in legal education)?',
    'Analysis of judicial appointment patterns and legal curriculum content over time. If suppression persists after structural barriers are removed, it suggests internalization.',
    'If internalized, the effective suppression of popular constitutionalism is higher than structural measures suggest, as advocates carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for alternative interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_constraint_authority__feudal_obsolescence_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(magn_tr_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(magn_tr_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(magn_tr_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(magn_be_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(magn_be_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(magn_be_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(magn_su_t10, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(magn_su_t20, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(magn_su_t30, magna_carta_constraint_authority__feudal_obsolescence_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_constraint_authority__feudal_obsolescence_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__living_constitutionalism_reading).
narrative_ontology:affects_constraint(magna_carta_constraint_authority__feudal_obsolescence_reading, magna_carta_constraint_authority__parliamentary_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'magna_carta_constraint_authority' kernel. Its assertion of obsolescence directly impacts the legitimacy and operational scope of sibling readings that claim ongoing or transformed authority for Magna Carta.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
