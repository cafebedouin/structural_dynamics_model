% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 (Feudal Prerogative Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   In 1215, English magnates extracted from King John a written confirmation
 *   of customary feudal rights: Magna Carta, with Clause 39 specifying that
 *   no free man shall be dispossessed except by lawful judgment or the law of
 *   the land. The feudal prerogative reading interprets this clause narrowly:
 *   it is a reassurance to great vassals and the Church that the king will
 *   observe customary feudal procedures (counsel, justification within feudal
 *   norms) when exercising his lawful prerogatives. The clause does NOT
 *   establish universal individual rights or challenge the king's authority;
 *   it encodes a bargain between the crown and the magnates within the
 *   existing hierarchy. The unfree population and commons are completely
 *   excluded and their arbitrary treatment is preserved. This reading
 *   emphasizes the constraint's function as legitimation of hierarchical
 *   authority rather than as a challenge to it.
 *
 * KEY AGENTS:
 *   - crown_authority — institutional agenda-setter; retains prerogative while gaining magnate consent through procedure
 *   - noble_peers — powerful beneficiaries; secure narrow procedural protections as privileged actors within the hierarchy
 *   - unfree_population — powerless, excluded; the crown's arbitrary power over them is completely preserved
 *   - church_authority — powerful beneficiary-observer; secures ecclesiastical standing within feudal order
 *   - royal_courts — institutional agenda-setter; administer procedures but remain subordinate to crown's will
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.28).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.42).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 (Feudal Prerogative Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'e00d7a08-29af-460f-8603-bff0847dbc70').
narrative_ontology:cs_kernel_codification('e00d7a08-29af-460f-8603-bff0847dbc70', fixed_text).
narrative_ontology:cs_authority_grounding('e00d7a08-29af-460f-8603-bff0847dbc70', lineage).
narrative_ontology:cs_interpretation_layer_present('e00d7a08-29af-460f-8603-bff0847dbc70').
narrative_ontology:cs_reading_relation('e00d7a08-29af-460f-8603-bff0847dbc70', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('e00d7a08-29af-460f-8603-bff0847dbc70', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('e00d7a08-29af-460f-8603-bff0847dbc70', foundational, feudal_hierarchy_preserving_principle).
narrative_ontology:cs_axiom_status(feudal_hierarchy_preserving_principle, holdable).
narrative_ontology:cs_axiom_grounding('e00d7a08-29af-460f-8603-bff0847dbc70', feudal_hierarchy_preserving_principle, deontological).
narrative_ontology:cs_axiom('e00d7a08-29af-460f-8603-bff0847dbc70', foundational, custom_over_universal_law).
narrative_ontology:cs_axiom_status(custom_over_universal_law, overridden).
narrative_ontology:cs_axiom_grounding('e00d7a08-29af-460f-8603-bff0847dbc70', custom_over_universal_law, conventional).
narrative_ontology:cs_reference_frame('e00d7a08-29af-460f-8603-bff0847dbc70', feudal_hierarchical_framework).
narrative_ontology:cs_drift_state('e00d7a08-29af-460f-8603-bff0847dbc70', early_modern_period_emergence_of_universal_rights, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e00d7a08-29af-460f-8603-bff0847dbc70', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_prerogative).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, noble_peers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.28, low by design: this reading emphasizes the crown's genuine need for magnate consent and the procedure as real (not pure theater). The constraint coordinates within the hierarchy rather than extracting from outside it. Suppression is 0.42, moderate but not high: the clause's enforcement depends on magnate acceptance and the crown's need for their support, not on coercive exclusion of alternatives. Theater ratio is low (0.15) because the procedural obligation is substantive within feudal custom, though it rises slightly in later centuries as the reading's feudal foundations weaken and the procedure becomes increasingly performative (by interval end, theater stays at 0.15, modest rise reflects gradual performativity but does not dominate). Accessibility collapse is moderate-high (0.65): magnates have few alternatives to accepting the crown's rule within feudal custom, but they are not trapped — they retain the option of revolt and renegotiation. Resistance is low (0.22): this reading depicts magnates as consenting participants, not resisting targets. The measurements show stability around 0.28 extractiveness (feudal procedure as genuine coordination) with modest rise in theater toward interval-end, reflecting the reading's own decay as the feudal framework erodes.
 *
 * PERSPECTIVAL GAP:
 *   The crown and magnates who benefit from this reading would perceive the constraint as rope (genuine coordination). The unfree population and commons, excluded from the clause, would perceive the crown's authority over them as unrestrained extraction — but their perspective is structurally absent from the constraint story because this reading defines the constraint narrowly, for magnates only. The engine would compute the crown's directionality near 0.5 (symmetric: the crown benefits from magnate consent but loses the unrestrained prerogative), while magnates compute near 0.25 (modest target: they are protected procedurally but retain their position within hierarchy). The excluded unfree see no protection and no voice — they experience the constraint as irrelevant to their situation, not as applicable to them.
 *
 * DIRECTIONALITY LOGIC:
 *   The crown benefits from magnate validation of its authority (d toward beneficiary, ~0.3). Magnates benefit from procedural protections but remain in their feudal subordination (d near 0.4, weak target status — they negotiate within the hierarchy, not against it). The unfree and commons experience the crown's prerogative as unrestrained (d would be ~0.95 if they were included, but this reading excludes them entirely). The ecclesiastical beneficiary computes near 0.25 (protected standing within hierarchy). The directionality pattern reflects the reading's core claim: magnate rights are INTRA-hierarchical, not ANTI-hierarchical.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids false mandatrophy by refusing to generalize: it does NOT claim the clause establishes universal due process (that would be mandated by a later, different reading). It claims instead that Clause 39 legitimizes feudal hierarchy by formalizing the magnates' procedural expectation. The mandate — that magnates shall be consulted — remains live throughout the feudal period and into the early-modern period, even as the feudal framework decays. However, the reading faces mandatrophy in the long run (beyond the interval): once the unfree are freed, once commons gain legal standing, and once universal rights discourse becomes hegemonic, the clause's feudal-proprietary reading becomes obsolete and is overridden by liberal and originalist readings. The measurement series shows modest theater-rise in the 60–80 range (projected), reflecting the constraint's gradual performativity as the feudal mandate weakens but before it is explicitly overridden.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feudal_vs_liberal_reading_contest,
    'Is Clause 39 fundamentally a feudal customary protection of magnate privilege, or does it instantiate a universal principle of due process that can be extended to all persons?',
    'Historical exegesis of contemporary 1215 intent, statements by magnates and chroniclers about their understanding of the clause. Contemporary political theory and jurisprudence examining whether feudal custom can be generalized to universal rights.',
    'If feudal reading is correct, the constraint protects only magnates and the crown''s prerogative is essentially preserved; if the universal reading is correct, the constraint''s logic demands extension to all subjects and challenges the hierarchical order itself. This is the central interpretive fork in Clause 39''s entire genealogy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_vs_liberal_reading_contest, conceptual, 'Whether Clause 39 is feudal privilege or universal rights principle.').

omega_variable(
    procedure_as_real_constraint_vs_theater,
    'How much does the ''procedure'' the clause invokes actually constrain the crown''s behavior, versus serving as ceremonial legitimation of decisions the crown has already made?',
    'Examination of crown behavior before and after 1215: did documented seizures of magnate property drop or become more justified-sounding? Did magnates prevail in disputes at court, or did the crown continue to take what it wanted while offering procedure as cover?',
    'If procedure is substantially constraining, the extractiveness is lower and the coordination function is real. If procedure is mostly theater, the extractiveness is higher (masked extraction) and the constraint is closer to a snare dressed as rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(procedure_as_real_constraint_vs_theater, empirical, 'Whether procedural obligation constrains the crown or functions as performative legitimacy.').

omega_variable(
    crown_prerogative_boundary,
    'What happens to the crown''s prerogative powers over unfree persons and commons as the feudal system decays? Is Clause 39 read forward as limiting those powers too, or does it remain confined to magnate relations?',
    'Tracing how later courts, parliaments, and legal theorists apply and extend Clause 39. Historical records of whether the unfree or commons ever gained standing through appeals to the clause''s language.',
    'If Clause 39 is applied universally, this reading (feudal prerogative) is overridden by later interpretation and becomes a historical artifact, not a live constraint. If it remains confined to magnates, the reading persists as a stable narrow interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(crown_prerogative_boundary, empirical, 'Whether Clause 39''s scope expands beyond magnates as the feudal system decays.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(magn_tr_t20, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(magn_tr_t40, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(magn_tr_t60, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(magn_tr_t80, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(magn_tr_t100, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(magn_be_t20, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(magn_be_t40, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement(magn_be_t60, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 60, 0.3).
narrative_ontology:measurement(magn_be_t80, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 80, 0.35).
narrative_ontology:measurement(magn_be_t100, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(magn_su_t20, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(magn_su_t40, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 40, 0.41).
narrative_ontology:measurement(magn_su_t60, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 60, 0.44).
narrative_ontology:measurement(magn_su_t80, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 80, 0.48).
narrative_ontology:measurement(magn_su_t100, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 100, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(magna_carta_clause_39__feudal_prerogative_reading, 0.18).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% Magna Carta Clause 39 is a contested kernel instantiated by three structurally distinct constraint stories: the feudal_prerogative_reading (narrow magnate procedure within hierarchy), the liberal_due_process_reading (universal individual rights against state), and the originalist_limitation_reading (specific documented abuses of 1215). Each reading has a distinct ε (extraction rates), victim set, and beneficiary structure. The three stories are linked as siblings via network.affects_constraints; the interpretive contest between them is documented in omega variables and cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_clause_39__feudal_prerogative_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
