% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Anti-Subordination)
 *   domain: constitutional_law/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which interprets the clause as forbidding the perpetuation of a
 *   caste system and permitting race-conscious measures to dismantle systemic
 *   subordination. It is one reading of the broader
 *   `equal_protection_commitment` kernel. Sibling readings include the
 *   'colorblind reading' (forbidding any racial classification) and the
 *   'diversity reading' (permitting race as one factor for educational
 *   diversity). The metrics reflect the active, enforced nature of this
 *   reading, which involves significant extraction from historically
 *   privileged groups to achieve its anti-subordination goals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.5).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.8).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.5).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading (Anti-Subordination)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a').
narrative_ontology:cs_kernel_codification('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', fixed_text).
narrative_ontology:cs_authority_grounding('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', lineage).
narrative_ontology:cs_interpretation_layer_present('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a').
narrative_ontology:cs_reading_relation('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', foundational, equality_requires_anti_subordination).
narrative_ontology:cs_axiom_status(equality_requires_anti_subordination, holdable).
narrative_ontology:cs_axiom_grounding('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', equality_requires_anti_subordination, deontological).
narrative_ontology:cs_axiom('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', secondary, race_conscious_remedies_permissible).
narrative_ontology:cs_axiom_status(race_conscious_remedies_permissible, holdable).
narrative_ontology:cs_axiom_grounding('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', race_conscious_remedies_permissible, instrumental).
narrative_ontology:cs_reference_frame('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', post_reconstruction_anti_caste_principle).
narrative_ontology:cs_drift_state('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', contemporary_legal_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7e87eec8-7ab2-4d7c-bfcb-5e40676e5a6a', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the intended beneficiaries of race-conscious remedial measures, designed to dismantle systemic subordination and address historical disadvantages. Their ability to exit the cycle of disadvantage is constrained by the persistence of structural inequalities.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    moderate, generational, constrained, national).

% Government agencies, educational institutions, and other public bodies tasked with designing and implementing race-conscious policies to achieve anti-subordination goals. They operate under legal mandates but face significant political and legal challenges.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, biographical, constrained, national).

% These groups bear the costs of remedial measures when they are denied preferential access or opportunities in favor of historically subordinated groups. They often resist these measures through legal challenges and political advocacy, viewing them as 'reverse discrimination'.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups, payer,
    organized, biographical, constrained, national).

% Advocates who believe that any state use of racial classification is unconstitutional, regardless of intent. They are excluded from the foundational premise of this remedial reading, which permits race-conscious measures, and are identity-locked into their colorblind interpretation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_advocates, excluded,
    organized, biographical, identity_locked, national).

% The ultimate arbiter of constitutional meaning, whose interpretations shape the scope and legitimacy of equal protection. Its decisions can either expand or contract the ability of state actors to implement remedial measures, creating significant drift in the constraint's operation.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Academics and legal experts who analyze, critique, and propose interpretations of equal protection. They provide the analytical framework for understanding the constraint's evolution and its impact on society.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state and societal efforts to dismantle systemic racial subordination and prevent the perpetuation of a caste system, ensuring that formal equality translates into substantive equality.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from historically privileged groups to historically subordinated groups, aiming to rectify past and ongoing harms and create a more equitable distribution of societal goods.
% ABSENT_VOICES: Advocates for a strictly colorblind interpretation of equal protection are structurally excluded from the foundational premise of this reading. They would argue that any race-conscious measure is inherently discriminatory and perpetuates racial division, rather than remedies it.
% DISAPPEARANCE_RATIONALE: If this remedial reading of equal protection vanished, state actors would lose the legal basis for race-conscious programs aimed at dismantling subordination. This would lead to a re-entrenchment of existing racial disparities, a failure to address systemic inequalities, and a significant reorganization of social policy and legal challenges.
% FOUNDING_PROBLEM: The historical and ongoing perpetuation of a racial caste system through state action and inaction, despite formal declarations of equality, leading to persistent systemic subordination and disadvantage for certain racial groups.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, social scientists, and international human rights bodies consistently attest to the ongoing existence of systemic racial disparities and the need for remedial action. Their reports, data, and advocacy provide corroboration from outside the immediate beneficiaries of remedial programs.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.5, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.50) because this reading actively reallocates opportunities and resources, which is experienced as a cost by those who previously benefited from the status quo. `Suppression` is very high (0.80) because the implementation of remedial measures requires overcoming significant legal, political, and social resistance, necessitating active enforcement to maintain. `Theater_ratio` is low (0.15) as the constraint is genuinely functional in its aim to dismantle subordination, though some performative aspects may emerge in compliance efforts. The temporal measurements show an initial rise in extractiveness and suppression as remedial programs gained traction, followed by a slight dip in extractiveness due to increasing legal challenges, while suppression requirements continued to rise to defend the reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and state actors, this reading is a necessary mechanism for justice and equality. From the perspective of historically privileged groups and colorblind advocates, it is an unjust form of 'reverse discrimination' that itself violates equal protection. The engine's per-seat classification will reflect these divergent experiences, with beneficiaries experiencing it as a Rope or Scaffold, and payers/targets experiencing it as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are clear beneficiaries, as the constraint aims to rectify their disadvantage. State actors implementing remedies are agenda-setters and beneficiaries, as they gain legitimacy and fulfill their mandate. Historically privileged groups are targets/payers, as they bear the costs of reallocation. Colorblind advocates are excluded, as their core premise is incompatible with this reading. The Supreme Court acts as an agenda-setter, shaping the constraint's application, and legal scholars are observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_equality,
    'Is the core disagreement over equal protection a conceptual one between ''substantive equality'' (requiring outcomes to be addressed) and ''formal equality'' (requiring only equal treatment under the law)?',
    'Conceptual analysis of legal and philosophical arguments; examination of judicial reasoning to identify underlying principles.',
    'If primarily conceptual, the readings may be irreconcilable within a single framework, leading to persistent legal and political contestation. If the difference is bridgeable, a more unified interpretation might emerge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formal_equality, conceptual, 'Ambiguity regarding the fundamental definition of ''equality'' underlying equal protection.').

omega_variable(
    efficacy_of_race_conscious_measures,
    'Are race-conscious measures empirically effective at dismantling systemic subordination and achieving their stated remedial goals without creating new forms of disadvantage?',
    'Longitudinal social science research, statistical analysis of outcomes in jurisdictions with and without such measures, and qualitative studies of lived experiences.',
    'Empirical evidence of ineffectiveness would weaken the instrumental grounding of this reading''s axioms, potentially shifting its status or leading to calls for alternative approaches. Strong evidence of efficacy would bolster its legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_race_conscious_measures, empirical, 'Empirical effectiveness of race-conscious remedial policies.').

omega_variable(
    colorblind_reading_structural_impact,
    'How would the structural adoption of the ''colorblind reading'' as the dominant interpretation of equal protection alter the beneficiary/victim structure and extractiveness of the legal system?',
    'Counterfactual analysis of legal precedent and policy outcomes under a hypothetical colorblind regime; comparative analysis with jurisdictions that have adopted strict colorblind policies.',
    'A shift to a colorblind reading would likely invert the beneficiary/victim structure for many policies, making historically privileged groups beneficiaries and historically subordinated groups victims, with a corresponding shift in the overall extractiveness experienced by each.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(colorblind_reading_structural_impact, conceptual, 'Impact of a colorblind interpretation on the constraint''s structural effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__remedial_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(equa_tr_t1970, equal_protection_commitment__remedial_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_commitment__remedial_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_commitment__remedial_reading, theater_ratio, 2010, 0.12).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__remedial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__remedial_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(equa_be_t1970, equal_protection_commitment__remedial_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(equa_be_t1990, equal_protection_commitment__remedial_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(equa_be_t2010, equal_protection_commitment__remedial_reading, base_extractiveness, 2010, 0.55).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__remedial_reading, base_extractiveness, 2024, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__remedial_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1970, equal_protection_commitment__remedial_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(equa_su_t1990, equal_protection_commitment__remedial_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(equa_su_t2010, equal_protection_commitment__remedial_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__remedial_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, affirmative_action_programs).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, voting_rights_enforcement).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, desegregation_mandates).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equal_protection_commitment' kernel, alongside the 'colorblind_reading' and 'diversity_reading'. Each reading instantiates a distinct constraint with its own structural properties and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
