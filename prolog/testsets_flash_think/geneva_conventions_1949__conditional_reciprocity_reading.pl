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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: geneva_conventions_1949__conditional_reciprocity_reading
 *   human_readable: Geneva Conventions (Conditional Reciprocity Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents a 'conditional reciprocity' reading of the
 *   1949 Geneva Conventions, where protections for combatants and civilians
 *   are understood to apply fully only when adversaries comply with
 *   established rules, particularly regarding combatant status (Article 4).
 *   Non-compliance by irregular forces is seen to permit a proportional
 *   degradation of protections. This reading emphasizes state sovereignty and
 *   military necessity, often leading to the classification of detained
 *   irregulars as 'unlawful combatants' without full Prisoner of War (POW)
 *   protections. The metrics reflect the substantial extraction from
 *   irregular forces and the active enforcement required to maintain this
 *   conditional application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.75).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (Conditional Reciprocity Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '86f74f16-6528-41f0-ac1f-c69f12211697').
narrative_ontology:cs_kernel_codification('86f74f16-6528-41f0-ac1f-c69f12211697', fixed_text).
narrative_ontology:cs_authority_grounding('86f74f16-6528-41f0-ac1f-c69f12211697', lineage).
narrative_ontology:cs_interpretation_layer_present('86f74f16-6528-41f0-ac1f-c69f12211697').
narrative_ontology:cs_reading_relation('86f74f16-6528-41f0-ac1f-c69f12211697', geneva_conventions_1949__humanitarian_ceiling_reading, forecloses).
narrative_ontology:cs_reading_relation('86f74f16-6528-41f0-ac1f-c69f12211697', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('86f74f16-6528-41f0-ac1f-c69f12211697', foundational, reciprocity_is_foundational).
narrative_ontology:cs_axiom_status(reciprocity_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('86f74f16-6528-41f0-ac1f-c69f12211697', reciprocity_is_foundational, conventional).
narrative_ontology:cs_axiom('86f74f16-6528-41f0-ac1f-c69f12211697', foundational, combatant_status_conditions_protection).
narrative_ontology:cs_axiom_status(combatant_status_conditions_protection, holdable).
narrative_ontology:cs_axiom_grounding('86f74f16-6528-41f0-ac1f-c69f12211697', combatant_status_conditions_protection, conventional).
narrative_ontology:cs_reference_frame('86f74f16-6528-41f0-ac1f-c69f12211697', state_centric_reciprocal_order).
narrative_ontology:cs_drift_state('86f74f16-6528-41f0-ac1f-c69f12211697', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86f74f16-6528-41f0-ac1f-c69f12211697', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_parties_regular_forces).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, international_courts_and_tribunals).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, state_sovereignty).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__conditional_reciprocity_reading, military_necessity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to the Conventions conditionally, expecting reciprocity from adversaries. They benefit from a framework that legitimizes their use of force against non-compliant adversaries while providing some protection for their own personnel if captured, provided they meet combatant criteria.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_parties_regular_forces, agenda_setter,
    institutional, generational, mobile, global).

% Often operate without clear command structures or distinctive insignia, making them vulnerable to classification as 'unlawful combatants' under this reading. They bear the full cost of degraded protections, including loss of POW status, and face severe suppression.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces, payer,
    powerless, immediate, trapped, local).

% While nominally protected, their immunity is narrowed by proportionality calculations in military operations, and they bear the risk of being caught in operations against irregulars. Their options are limited by the conflict itself.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_conflict_zones, payer,
    powerless, immediate, constrained, local).

% Monitor compliance and advocate for broader application of humanitarian protections, often challenging the conditional interpretations. They operate within the framework but seek to expand its scope and reduce extraction from vulnerable populations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, humanitarian_organizations, observer,
    moderate, biographical, constrained, global).

% Interpret and apply the Conventions, often navigating the tension between state security concerns and humanitarian principles. Their rulings shape the practical application of this conditional reciprocity, sometimes reinforcing, sometimes challenging, state interpretations.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal restraint in armed conflict, aiming to limit the brutality of war by offering protections to those who comply with rules of engagement and combatant status.
% TRANSFER_FUNCTION: Transfers the burden of full compliance and protection from states to irregular forces who fail to meet combatant criteria, while granting states the right to proportionally degrade protections in response to non-compliance.
% ABSENT_VOICES: Advocates for absolute humanitarian protections (e.g., the humanitarian_ceiling_reading) and those who argue for complete suspension of IHL in asymmetric conflict (e.g., the security_maximization_reading) are present in the broader discourse but are structurally excluded from the internal logic of this specific conditional reciprocity framework.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, states would lose a key legal justification for their actions against irregulars and a basis for demanding reciprocal treatment for their own forces. The legal and operational landscape of armed conflict would fundamentally shift, likely leading to even greater brutality and legal ambiguity.
% FOUNDING_PROBLEM: To regulate the conduct of warfare, protect combatants and civilians, and ensure humane treatment of prisoners, while acknowledging the practical realities of state sovereignty and military necessity.
% FOUNDING_PROBLEM_CORROBORATION: State military legal advisors and international legal scholars generally agree that the core problem of regulating warfare remains live, though the specific interpretation of how the Conventions apply to irregular forces is highly contested by human rights organizations and some legal academics.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_1949__conditional_reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because this reading allows states to significantly reduce protections for non-compliant adversaries, effectively extracting their rights. Suppression is also high (0.75) as it requires active enforcement of combatant status criteria and the suppression of irregular warfare tactics that challenge these criteria. The theater ratio (0.40) reflects that while states performatively adhere to IHL, a significant portion of the interpretive and enforcement effort is directed at justifying conditional application rather than universal protection. Accessibility collapse (0.70) is high for irregular forces, as their alternatives to being classified as unlawful combatants are severely limited. Resistance (0.60) is substantial from irregular forces and human rights advocates who challenge this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state parties, this reading is a necessary and pragmatic interpretation that balances humanitarian concerns with military necessity and the need for reciprocal compliance. From the perspective of irregular forces and human rights advocates, it is a highly extractive interpretation that undermines the universal principles of IHL and creates a two-tiered system of protection.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and their regular forces are primary beneficiaries (low d) as this reading provides a legal framework that legitimizes their actions and offers conditional protection for their personnel. International courts and tribunals, while mediating, also benefit from the clarity of a framework they can interpret and enforce. Irregular forces and civilians in conflict zones are the primary targets (high d), bearing the costs of degraded protections and increased vulnerability due to conditional application and proportionality calculations. Humanitarian organizations act as observers, advocating for those who bear the costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_ambiguity,
    'Is ''proportional degradation'' a clear and consistently applied standard, or does it serve as a flexible justification for expanded use of force against non-state actors?',
    'Detailed case studies of military operations and judicial reviews of proportionality assessments in asymmetric conflicts, comparing stated intent with actual outcomes and civilian harm.',
    'If found to be consistently vague or used as a pretext, the effective extractiveness of this reading is higher than currently estimated, as it enables greater harm under a veneer of legality. If consistently applied, it supports the claim of a balanced, albeit conditional, constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_ambiguity, empirical, 'Ambiguity in the application of ''proportional degradation'' in practice.').

omega_variable(
    combatant_status_application_fairness,
    'How consistently and fairly are the Article 4 criteria for combatant status applied to irregular forces across different conflicts and state actors?',
    'Comparative legal analysis of military manuals and judicial decisions, alongside empirical studies of detention practices and treatment of irregulars in various conflicts.',
    'Inconsistent or biased application would indicate higher effective suppression and extractiveness, as it would demonstrate a systemic denial of protections based on arbitrary or politically motivated classifications. Consistent application would lend more legitimacy to the conditional framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(combatant_status_application_fairness, empirical, 'Fairness and consistency of combatant status application to irregular forces.').

omega_variable(
    reciprocity_as_pretext,
    'Is the demand for reciprocity genuinely about mutual restraint and protection, or does it primarily function as a pretext for denying protections to adversaries who cannot or will not comply with state-centric norms?',
    'Analysis of state diplomatic and military communications, and historical patterns of IHL application, to discern underlying motivations for emphasizing reciprocity in asymmetric conflicts.',
    'If primarily a pretext, the coordination function of this reading is significantly weaker or non-existent, and its classification shifts closer to a pure Snare, as the coordination story serves mainly as cover for extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_as_pretext, conceptual, 'Whether reciprocity is a genuine coordination mechanism or a pretext for extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.45).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2001, 0.62).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2001, 0.7).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
