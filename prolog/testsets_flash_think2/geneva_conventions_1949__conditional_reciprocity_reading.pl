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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint story instantiates the 'conditional reciprocity' reading
 *   of the 1949 Geneva Conventions. Under this reading, the Conventions
 *   function as reciprocal restraints that apply fully only when adversaries
 *   comply with their provisions. Non-compliance by irregular forces, in
 *   particular, is seen to permit a proportional degradation of protections,
 *   especially regarding combatant status and civilian immunity in
 *   proportionality calculations. This reading emphasizes state security and
 *   the practical challenges of asymmetric warfare.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__conditional_reciprocity_reading, 0.6).
domain_priors:suppression_score(geneva_conventions_1949__conditional_reciprocity_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_1949__conditional_reciprocity_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(geneva_conventions_1949__conditional_reciprocity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__conditional_reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__conditional_reciprocity_reading, "Geneva Conventions (Conditional Reciprocity Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__conditional_reciprocity_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__conditional_reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__conditional_reciprocity_reading, '9dcddd66-fc99-4eef-b82a-f075b5428716').
narrative_ontology:cs_kernel_codification('9dcddd66-fc99-4eef-b82a-f075b5428716', fixed_text).
narrative_ontology:cs_authority_grounding('9dcddd66-fc99-4eef-b82a-f075b5428716', lineage).
narrative_ontology:cs_interpretation_layer_present('9dcddd66-fc99-4eef-b82a-f075b5428716').
narrative_ontology:cs_reading_relation('9dcddd66-fc99-4eef-b82a-f075b5428716', geneva_conventions_1949__humanitarian_ceiling_reading, coexists_with).
narrative_ontology:cs_reading_relation('9dcddd66-fc99-4eef-b82a-f075b5428716', geneva_conventions_1949__security_maximization_reading, coexists_with).
narrative_ontology:cs_axiom('9dcddd66-fc99-4eef-b82a-f075b5428716', foundational, reciprocity_is_foundational_to_ihl).
narrative_ontology:cs_axiom_status(reciprocity_is_foundational_to_ihl, holdable).
narrative_ontology:cs_axiom_grounding('9dcddd66-fc99-4eef-b82a-f075b5428716', reciprocity_is_foundational_to_ihl, conventional).
narrative_ontology:cs_axiom('9dcddd66-fc99-4eef-b82a-f075b5428716', foundational, combatant_status_is_conditional).
narrative_ontology:cs_axiom_status(combatant_status_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('9dcddd66-fc99-4eef-b82a-f075b5428716', combatant_status_is_conditional, conventional).
narrative_ontology:cs_reference_frame('9dcddd66-fc99-4eef-b82a-f075b5428716', post_wwii_state_centric_order).
narrative_ontology:cs_drift_state('9dcddd66-fc99-4eef-b82a-f075b5428716', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9dcddd66-fc99-4eef-b82a-f075b5428716', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, state_parties_to_conventions).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__conditional_reciprocity_reading, organized_armed_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces).
narrative_ontology:constraint_victim(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_proportionality_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret and apply the Conventions, benefiting from the reciprocal restraints that limit the scope of conflict and provide a framework for managing hostilities, provided adversaries also comply. They enforce the conditional application of protections.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, state_parties_to_conventions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, state_parties_to_conventions, beneficiary).

% Members of these forces benefit from Prisoner of War (POW) status and associated protections under Article 4 of GCIII, provided they meet criteria like organized command, distinctive insignia, and carrying arms openly. This status is conditional on their compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, organized_armed_forces, beneficiary,
    organized, biographical, constrained, global).

% These forces, often lacking clear command structures or distinctive insignia, are frequently denied full POW protections under this reading, classified as 'unlawful combatants.' They bear the cost of degraded status and increased vulnerability.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_1949__conditional_reciprocity_reading, irregular_forces, excluded).

% While civilian immunity is preserved, it is narrowed by proportionality calculations in military operations. These civilians bear the indirect costs of conflict when their presence is weighed against military advantage, leading to potential harm that is deemed 'proportional.'
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, civilians_in_proportionality_zones, payer,
    powerless, immediate, trapped, local).

% These scholars analyze the application and interpretation of IHL, often debating the implications of conditional reciprocity versus absolute humanitarian standards. They do not directly benefit or pay but influence discourse.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% These advocates argue for absolute humanitarian minimums regardless of reciprocity, often finding their arguments marginalized or dismissed by states prioritizing security and conditional application. They are excluded from the direct interpretation and enforcement mechanisms of this reading.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__conditional_reciprocity_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for reciprocal restraint in armed conflict, aiming to limit violence and protect certain categories of persons, conditional on compliance by all parties. It coordinates state behavior by offering protections in exchange for adherence to specific rules.
% TRANSFER_FUNCTION: Transfers the burden of restraint and protection from compliant state parties to non-compliant irregular forces (who lose protections) and, indirectly, to civilians caught in proportionality calculations (who bear the costs of 'acceptable' collateral damage).
% ABSENT_VOICES: Humanitarian advocates who argue for absolute protections, and irregular forces who are denied full legal status and protections, are largely excluded from shaping this reading. Their perspectives would challenge the conditional nature of protections.
% DISAPPEARANCE_RATIONALE: If this conditional reading of the Conventions vanished, states would either face no reciprocal restraint (potentially leading to total war with no rules) or be forced into an absolute humanitarianism (perceived as a security risk), fundamentally altering military doctrine, international relations, and the very nature of armed conflict.
% FOUNDING_PROBLEM: The need to regulate warfare and protect victims while acknowledging the practical realities of state security, the need for reciprocal obligations, and the challenges posed by non-state actors in armed conflict.
% FOUNDING_PROBLEM_CORROBORATION: Military legal advisors, some international relations theorists, and state defense ministries corroborate this framing, arguing it represents the only pragmatic way to maintain any restraint in contemporary asymmetric conflicts. This perspective is often contested by humanitarian organizations and some legal scholars.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__conditional_reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__conditional_reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__conditional_reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_1949__conditional_reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__conditional_reciprocity_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate-high (0.60) because this reading allows for the denial of full protections to certain combatants and permits civilian harm under proportionality, effectively extracting compliance or bearing costs from non-state actors and civilians. Suppression is high (0.70) as states actively enforce their interpretation, denying status and justifying actions based on adversary non-compliance. Theater ratio is moderate (0.40); while genuine efforts are made to adhere to IHL, a significant portion of the discourse and enforcement activity is dedicated to justifying conditional application and managing the narrative around 'proportional degradation.' The measurements show a trend of increasing extractiveness and suppression as asymmetric conflicts became more prevalent, challenging the original state-centric framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state parties, this reading is a pragmatic necessity for maintaining security and order in conflict. From the perspective of irregular forces and humanitarian advocates, it represents a degradation of universal protections and an asymmetric application of law. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties and their organized armed forces are beneficiaries, as they gain a framework for reciprocal restraint and the flexibility to degrade protections against non-compliant adversaries. Irregular forces and civilians in conflict zones are victims, bearing the costs of denied status and 'proportional' harm. International legal scholars observe, while human rights advocates are excluded from the dominant interpretive frame.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the Conventions as a pure 'Rope' (universal coordination) by acknowledging the inherent extraction and enforcement required for its conditional application. It also prevents mislabeling as a pure 'Snare' by recognizing the genuine coordination function of reciprocal restraint among compliant state parties. The 'tangled_rope' classification captures both the coordination and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportional_degradation_ambiguity,
    'What constitutes ''proportional degradation'' of protections in response to non-compliance, and is it applied symmetrically?',
    'Detailed empirical analysis of state practice in various conflicts, coupled with independent legal review of proportionality assessments and their outcomes for affected populations.',
    'If ''proportional degradation'' is found to be consistently asymmetric or to exceed reasonable bounds, the constraint''s effective extractiveness and suppression would be higher, pushing it closer to a Snare. If it is found to be genuinely reciprocal and limited, it would reinforce the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportional_degradation_ambiguity, empirical, 'The scope and symmetry of ''proportional degradation'' in IHL application.').

omega_variable(
    combatant_status_criteria_flexibility,
    'How flexible are the criteria for ''organized command'' and ''distinctive insignia'' in determining combatant status for irregular forces, and is this flexibility exploited?',
    'Comparative legal analysis of different states'' interpretations and judicial rulings on combatant status, alongside case studies of how these criteria are applied in practice during conflicts.',
    'If the criteria are found to be highly flexible and consistently interpreted to deny status to irregular forces, it would indicate higher suppression and extraction, strengthening the Snare-like aspects. If interpretations are consistent and genuinely applied, it would support the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(combatant_status_criteria_flexibility, conceptual, 'The interpretive latitude in applying combatant status criteria.').

omega_variable(
    reciprocity_vs_unilateralism,
    'To what extent is the ''reciprocity'' claimed by this reading a genuine driver of state behavior, versus a justification for unilateral actions?',
    'Analysis of state diplomatic and military communications, legal justifications for actions, and the actual impact of adversary compliance/non-compliance on state conduct over time.',
    'If reciprocity is primarily a post-hoc justification for unilateral actions, the constraint''s coordination function is weaker, and its extraction stronger, pushing it towards a Snare. If it genuinely shapes state behavior, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reciprocity_vs_unilateralism, empirical, 'The actual role of reciprocity in state IHL compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__conditional_reciprocity_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1969, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1969, 0.25).
narrative_ontology:measurement(gene_tr_t1989, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 1989, 0.3).
narrative_ontology:measurement(gene_tr_t2004, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2004, 0.35).
narrative_ontology:measurement(gene_tr_t2014, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__conditional_reciprocity_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1949, 0.45).
narrative_ontology:measurement(gene_be_t1969, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(gene_be_t1989, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 1989, 0.55).
narrative_ontology:measurement(gene_be_t2004, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2004, 0.6).
narrative_ontology:measurement(gene_be_t2014, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__conditional_reciprocity_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(gene_su_t1969, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1969, 0.6).
narrative_ontology:measurement(gene_su_t1989, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 1989, 0.65).
narrative_ontology:measurement(gene_su_t2004, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2004, 0.7).
narrative_ontology:measurement(gene_su_t2014, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2014, 0.72).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__conditional_reciprocity_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__conditional_reciprocity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, rules_of_engagement_doctrine).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__humanitarian_ceiling_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__conditional_reciprocity_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 1949 Geneva Conventions kernel, each with different structural properties and classifications. This reading emphasizes conditional reciprocity and state security.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
