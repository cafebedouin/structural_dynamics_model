% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__conditional_reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__conditional_reciprocity_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (Conditional Reciprocity Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.45).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (Conditional Reciprocity Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, 'a1404d2b-845c-4975-9572-8ea080a834b4').
narrative_ontology:cs_kernel_codification('a1404d2b-845c-4975-9572-8ea080a834b4', fixed_text).
narrative_ontology:cs_authority_grounding('a1404d2b-845c-4975-9572-8ea080a834b4', lineage).
narrative_ontology:cs_interpretation_layer_present('a1404d2b-845c-4975-9572-8ea080a834b4').
narrative_ontology:cs_reading_relation('a1404d2b-845c-4975-9572-8ea080a834b4', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('a1404d2b-845c-4975-9572-8ea080a834b4', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('a1404d2b-845c-4975-9572-8ea080a834b4', foundational, reciprocity_is_foundational_to_ihl).
narrative_ontology:cs_axiom_status(reciprocity_is_foundational_to_ihl, holdable).
narrative_ontology:cs_axiom_grounding('a1404d2b-845c-4975-9572-8ea080a834b4', reciprocity_is_foundational_to_ihl, conventional).
narrative_ontology:cs_axiom('a1404d2b-845c-4975-9572-8ea080a834b4', foundational, pow_status_requires_article_4_compliance).
narrative_ontology:cs_axiom_status(pow_status_requires_article_4_compliance, holdable).
narrative_ontology:cs_axiom_grounding('a1404d2b-845c-4975-9572-8ea080a834b4', pow_status_requires_article_4_compliance, conventional).
narrative_ontology:cs_reference_frame('a1404d2b-845c-4975-9572-8ea080a834b4', post_wwii_state_centric_ihl).
narrative_ontology:cs_drift_state('a1404d2b-845c-4975-9572-8ea080a834b4', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a1404d2b-845c-4975-9572-8ea080a834b4', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, detained_lawful_combatants).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adheres to the Conventions when adversaries comply, expecting reciprocal restraint. Benefits from the ability to degrade protections proportionally against non-compliant irregular forces, while maintaining a framework for their own lawful combatants.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_military_forces, agenda_setter,
    institutional, generational, constrained, global).

% Often unable or unwilling to meet Article 4 criteria (organized command, distinctive insignia, carrying arms openly), leading to their classification as unlawful combatants. Bears the cost of reduced protections, including denial of POW status upon capture.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces, payer,
    powerless, immediate, trapped, local).

% Receives protection, but this protection is conditional on proportionality calculations and the distinction between combatants and non-combatants. Bears the risk of 'collateral damage' or being caught in areas where irregular forces operate, leading to proportional degradation of protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, trapped, local).

% Receives full POW protections under the Conventions, provided they meet Article 4 criteria. Their status is secured by the reciprocal adherence of state parties.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, detained_lawful_combatants, beneficiary,
    moderate, biographical, constrained, global).

% Analyzes the application and interpretation of the Conventions, particularly in asymmetric conflicts. Documents instances of compliance and non-compliance, and debates the legal implications of conditional reciprocity.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal restraint in armed conflict, aiming to limit suffering by coordinating expectations of conduct between state parties, particularly regarding the treatment of combatants and civilians.
% TRANSFER_FUNCTION: Transfers the right to proportional degradation of protections from non-compliant irregular forces to state military forces, in exchange for maintaining protections for compliant forces and civilians (albeit with proportionality caveats).
% ABSENT_VOICES: Advocates for universal human rights and absolute humanitarian minimums would object, arguing that protections should not be conditional on adversary compliance or status. They are often excluded from the operational interpretation of IHL by military and legal advisors focused on state security.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state military forces would likely face increased pressure to adhere to absolute humanitarian standards, or conversely, might feel less constrained by any rules, leading to a more chaotic and less predictable conflict environment. The legal and ethical landscape of warfare would be fundamentally altered.
% FOUNDING_PROBLEM: The need to regulate the conduct of warfare and protect non-combatants and those no longer fighting, particularly after the atrocities of World War II, while acknowledging the practicalities of military operations.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, humanitarian organizations, and military legal advisors continue to grapple with the application of the Conventions in contemporary conflicts, especially concerning non-state actors. The ongoing debates and challenges attest to the problem's live status, corroborated by UN reports and ICRC statements.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).
:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'What constitutes ''proportional degradation'' in response to adversary non-compliance, and who adjudicates this proportionality?',
    'Establishment of independent, internationally recognized bodies with clear mandates and enforcement powers to review and rule on proportionality claims in real-time conflict scenarios.',
    'If proportionality is self-adjudicated by state actors, it risks becoming a cover for maximalist security objectives, increasing extraction. If externally adjudicated, it could re-center the constraint closer to a genuine rope by ensuring fair application.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in defining and adjudicating proportional degradation of protections.').

omega_variable(
    article_4_applicability_to_irregular_forces,
    'Are the Article 4 criteria for POW status (organized command, distinctive insignia, carrying arms openly) genuinely applicable or achievable for all irregular forces in modern asymmetric conflicts?',
    'Empirical study of irregular force structures and operational realities across diverse conflicts, combined with legal re-evaluation of Article 4''s intent versus its practical effect.',
    'If Article 4 is found to be systematically unachievable for many irregular forces, its application under this reading becomes a de facto denial of protections, increasing extraction. If achievable, it reinforces the reciprocal basis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_4_applicability_to_irregular_forces, empirical, 'Whether Article 4 criteria are fair and applicable to irregular forces.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''conditional reciprocity'' reading of the Geneva Conventions, or does it function as a ''security maximization'' reading under the guise of reciprocity?',
    'Analysis of state practice over time: if ''proportional degradation'' consistently leads to maximal security outcomes even when less severe responses are available, it suggests a security maximization reading. If it genuinely tracks adversary non-compliance, it supports reciprocity.',
    'If it''s a security maximization reading, the constraint''s effective extractiveness is higher, and its claimed coordination function is more theatrical. If it''s genuine reciprocity, the current metrics are accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing conditional reciprocity from security maximization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.3).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.43).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1970, 0.52).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
