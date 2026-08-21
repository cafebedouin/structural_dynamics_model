% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Protections Scaled by Conflict Type and Proportionality
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid proportionality reading' of the
 *   Geneva Conventions' protective scope. It posits that protections scale by
 *   conflict type (AP I for international, AP II/Common Article 3 for
 *   non-international) and that proportionality analysis determines their
 *   application. This reading acknowledges the need for differentiated legal
 *   regimes but introduces significant interpretive flexibility. The claimed
 *   type is 'tangled_rope' because it offers genuine coordination (protection
 *   in conflict) but also enables asymmetric extraction through the
 *   discretion afforded to powerful actors in classifying conflicts and
 *   applying proportionality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.55).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Protections Scaled by Conflict Type and Proportionality").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '81d61586-cf2b-4110-880e-1e0fdda66ef1').
narrative_ontology:cs_kernel_codification('81d61586-cf2b-4110-880e-1e0fdda66ef1', formalized).
narrative_ontology:cs_authority_grounding('81d61586-cf2b-4110-880e-1e0fdda66ef1', lineage).
narrative_ontology:cs_interpretation_layer_present('81d61586-cf2b-4110-880e-1e0fdda66ef1').
narrative_ontology:cs_reading_relation('81d61586-cf2b-4110-880e-1e0fdda66ef1', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('81d61586-cf2b-4110-880e-1e0fdda66ef1', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('81d61586-cf2b-4110-880e-1e0fdda66ef1', foundational, conflict_type_determines_protection_scope).
narrative_ontology:cs_axiom_status(conflict_type_determines_protection_scope, holdable).
narrative_ontology:cs_axiom_grounding('81d61586-cf2b-4110-880e-1e0fdda66ef1', conflict_type_determines_protection_scope, conventional).
narrative_ontology:cs_axiom('81d61586-cf2b-4110-880e-1e0fdda66ef1', foundational, proportionality_as_limiting_principle).
narrative_ontology:cs_axiom_status(proportionality_as_limiting_principle, holdable).
narrative_ontology:cs_axiom_grounding('81d61586-cf2b-4110-880e-1e0fdda66ef1', proportionality_as_limiting_principle, conventional).
narrative_ontology:cs_reference_frame('81d61586-cf2b-4110-880e-1e0fdda66ef1', differentiated_conflict_framework).
narrative_ontology:cs_drift_state('81d61586-cf2b-4110-880e-1e0fdda66ef1', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('81d61586-cf2b-4110-880e-1e0fdda66ef1', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret and apply the Geneva Conventions, benefiting from the flexibility to scale protections based on conflict classification and proportionality analysis, which can reduce constraints on their military operations.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, powerful_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Responsible for implementing IHL on the ground, they utilize proportionality analysis to justify military actions, often balancing military necessity against civilian harm within the framework's interpretive flexibility.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_commanders, agenda_setter,
    organized, biographical, constrained, national).

% Often operating in non-international armed conflicts, they face reduced protections under AP II/Common Article 3 compared to state armies, and are frequently targets of operations where proportionality assessments are made by opposing forces.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    powerless, immediate, trapped, local).

% Their protections are explicitly scaled down in non-international conflicts, making them more vulnerable to harm justified by proportionality analysis, and they often lack clear recourse or protected status.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_non_international_conflict, payer,
    powerless, immediate, trapped, local).

% They analyze, interpret, and critique the application of IHL, including the scaling of protections and the nuances of proportionality, often highlighting gaps or abuses in practice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_humanitarian_law_scholars, observer,
    analytical, generational, analytical, global).

% They argue for a universal floor of human rights protections in all conflicts, often finding their arguments marginalized or structurally excluded by the IHL framework's emphasis on conflict classification and proportionality.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework for regulating the conduct of hostilities and protecting specific categories of persons in both international and non-international armed conflicts, aiming to limit suffering while acknowledging different conflict realities.
% TRANSFER_FUNCTION: Transfers significant discretion in applying protections from universal principles to state actors and military commanders, in exchange for a framework that is ostensibly more 'realistic' for diverse conflict scenarios, particularly non-international ones.
% ABSENT_VOICES: Universal human rights advocates and victims of non-international armed conflicts who argue for a consistent, higher standard of protection regardless of conflict classification or proportionality calculus. Their perspectives are often outside the direct interpretive authority of IHL bodies.
% DISAPPEARANCE_RATIONALE: If this specific framework vanished, the legal basis for differentiating protections by conflict type and applying proportionality would collapse. This would force a reorganization of how armed conflicts are conducted and judged, likely leading to either a vacuum of regulation or a shift towards more universal (or more strictly state-centric) interpretations.
% FOUNDING_PROBLEM: The need to regulate the conduct of hostilities and protect specific categories of persons in both international and non-international armed conflicts, recognizing the different legal and practical realities of each while seeking to limit suffering.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, UN reports, and independent humanitarian organizations consistently attest to the ongoing need for IHL to regulate conflict and protect civilians, even while critiquing its application and specific interpretations. This corroboration comes from outside the direct beneficiaries of interpretive flexibility.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.65) due to the inherent ambiguity in conflict classification and proportionality assessments, which can be leveraged by powerful states to limit protections for weaker parties or in non-international conflicts. Suppression (0.55) arises from the legal framework itself, which limits the scope for alternative, more universal claims. The theater ratio (0.4) reflects that while proportionality is a genuine legal principle, its application can sometimes be performative, justifying actions that cause significant harm. Resistance (0.45) comes from human rights advocates and scholars who challenge the limitations and interpretations of this reading.
 *
 * PERSPECTIVAL GAP:
 *   Powerful states and military commanders (agenda-setters) perceive this framework as a necessary and pragmatic approach to regulating diverse conflicts, balancing military necessity with humanitarian concerns. However, non-state armed groups and civilians in non-international conflicts (payers/victims) experience it as a system that scales down their protections and allows for substantial harm under the guise of legal compliance. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Powerful states and military commanders are beneficiaries (low d) as they gain flexibility and discretion in operations. Non-state armed groups and civilians in non-international conflicts are targets (high d) as their protections are reduced and they bear the costs of scaled application. International law scholars are observers, and human rights advocates are excluded, as their preferred universalist framing is often outside the direct application of this specific IHL reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_objectivity_ambiguity,
    'To what extent is proportionality analysis an objective legal calculus versus a flexible justification for military action?',
    'Empirical analysis of proportionality assessments across multiple conflicts and actors, comparing stated justifications with observed outcomes and independent expert evaluations.',
    'If proportionality is found to be consistently subjective or biased, the constraint''s effective extractiveness would be higher, as it provides a cover for harm. If objective, extractiveness would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_objectivity_ambiguity, empirical, 'Ambiguity in the objectivity and gameability of proportionality assessments.').

omega_variable(
    conflict_classification_impact,
    'How consistently and objectively are conflicts classified as international vs. non-international, and what is the actual impact of this classification on protected populations?',
    'Comparative legal and sociological studies of conflict classification processes and their humanitarian consequences in different contexts.',
    'If classification is found to be arbitrary or politically motivated, the victim set''s vulnerability is structurally amplified, increasing effective extraction. If consistent, the scaling is a more legitimate coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_impact, empirical, 'The impact of conflict classification on the scope of protections.').

omega_variable(
    selective_interpretation_risk,
    'Does the interpretive flexibility inherent in this reading allow powerful actors to selectively apply protections, thereby undermining the humanitarian intent?',
    'Case studies of IHL application by powerful states, examining instances where interpretations of conflict type or proportionality appear to align with strategic interests rather than humanitarian imperatives.',
    'If selective interpretation is prevalent, the constraint functions more as a snare, with its coordination function serving as cover for extraction. If not, it functions closer to a rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_interpretation_risk, conceptual, 'Risk of powerful actors leveraging legal ambiguity for selective application of protections.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1949, 0.25).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.42).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1965, 0.55).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1949, 0.45).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1965, 0.5).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1980, 0.55).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
