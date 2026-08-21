% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__universal_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__universal_rights_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__universal_rights_reading
 *   human_readable: Geneva Conventions Protective Scope: Universal Rights Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint represents the 'universal rights' reading of the Geneva
 *   Conventions' protective scope, which asserts that protections extend to
 *   all persons affected by armed conflict, regardless of their combatant
 *   status, by integrating Common Article 3 and human rights law to create a
 *   universal floor. This reading expands the victim set to include all
 *   conflict participants and significantly restricts state military
 *   operational flexibility. It is a contested interpretation, actively
 *   resisted by some state actors who prefer a narrower, state-centric view
 *   of IHL.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__universal_rights_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__universal_rights_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__universal_rights_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__universal_rights_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__universal_rights_reading, "Geneva Conventions Protective Scope: Universal Rights Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__universal_rights_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__universal_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__universal_rights_reading, '87d8ce57-e1bf-49ec-864c-099c1387277a').
narrative_ontology:cs_kernel_codification('87d8ce57-e1bf-49ec-864c-099c1387277a', fixed_text).
narrative_ontology:cs_authority_grounding('87d8ce57-e1bf-49ec-864c-099c1387277a', lineage).
narrative_ontology:cs_interpretation_layer_present('87d8ce57-e1bf-49ec-864c-099c1387277a').
narrative_ontology:cs_reading_relation('87d8ce57-e1bf-49ec-864c-099c1387277a', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('87d8ce57-e1bf-49ec-864c-099c1387277a', geneva_conventions_protective_scope__hybrid_proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('87d8ce57-e1bf-49ec-864c-099c1387277a', foundational, human_dignity_universal_in_conflict).
narrative_ontology:cs_axiom_status(human_dignity_universal_in_conflict, holdable).
narrative_ontology:cs_axiom_grounding('87d8ce57-e1bf-49ec-864c-099c1387277a', human_dignity_universal_in_conflict, deontological).
narrative_ontology:cs_axiom('87d8ce57-e1bf-49ec-864c-099c1387277a', foundational, common_article_3_non_derogable_floor).
narrative_ontology:cs_axiom_status(common_article_3_non_derogable_floor, holdable).
narrative_ontology:cs_axiom_grounding('87d8ce57-e1bf-49ec-864c-099c1387277a', common_article_3_non_derogable_floor, conventional).
narrative_ontology:cs_reference_frame('87d8ce57-e1bf-49ec-864c-099c1387277a', post_world_war_ii_human_rights_expansion).
narrative_ontology:cs_drift_state('87d8ce57-e1bf-49ec-864c-099c1387277a', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('87d8ce57-e1bf-49ec-864c-099c1387277a', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__universal_rights_reading, detainees_and_prisoners_of_war).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, universal_human_rights_doctrine).
narrative_ontology:constraint_vindicates(geneva_conventions_protective_scope__universal_rights_reading, jus_cogens_norms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the costs of operational restrictions, expanded targeting constraints, and stricter detention/interrogation rules. They view this reading as an impediment to effective military action, increasing risk to their personnel and complicating strategic objectives. Exit means abandoning international legal legitimacy.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, state_military_forces, payer,
    institutional, biographical, constrained, global).

% Face significant constraints on interrogation techniques, intelligence gathering, and covert operations, particularly concerning non-state actors and civilians. They argue it hinders their ability to prevent threats. Exit means operating outside international law, with severe reputational and legal consequences.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% Benefit from expanded protections, even if they do not adhere to IHL themselves. This reading grants them a legal floor for treatment if captured, and restricts state actions against their members and supporting populations. Their 'exit' is to cease hostilities, which is not a relevant option for their operational goals.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, non_state_armed_groups, beneficiary,
    organized, immediate, mobile, regional).

% Are the primary beneficiaries of expanded protections, as their status is explicitly decoupled from combatant status. This reading aims to reduce harm, displacement, and suffering during conflict. They are often trapped in conflict zones with no viable exit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, civilian_populations, beneficiary,
    powerless, immediate, trapped, local).

% Receive a universal floor of humane treatment, regardless of their formal status as POWs or unprivileged belligerents. This reading aims to prevent torture, arbitrary detention, and summary execution. They are physically trapped by their captors.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, detainees_and_prisoners_of_war, beneficiary,
    powerless, immediate, trapped, local).

% Interpret and enforce this reading, holding states and individuals accountable for violations. They are the primary institutional actors driving the expansion of protective scope through jurisprudence. Their 'exit' would be to cease functioning, which is not a practical option.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, international_courts_and_tribunals, agenda_setter,
    institutional, generational, analytical, global).

% Actively promote and monitor adherence to this universal rights reading, documenting violations and advocating for stronger enforcement. They provide critical corroboration for the constraint's impact and resistance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__universal_rights_reading, human_rights_advocacy_organizations, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable floor of humane treatment for all persons affected by armed conflict, aiming to reduce suffering and provide a common standard for state and non-state actors.
% TRANSFER_FUNCTION: Transfers operational flexibility and targeting discretion from state military and intelligence agencies to non-state actors and civilian populations, in exchange for expanded protections and a reduction in human suffering.
% ABSENT_VOICES: Hardline military strategists and national security hawks who prioritize unconstrained operational freedom would object, arguing that this reading unduly ties the hands of states and grants legitimacy to non-state adversaries. They are often excluded from IHL interpretive forums.
% DISAPPEARANCE_RATIONALE: If this universal rights reading vanished, state military and intelligence operations would likely revert to more permissive targeting and detention practices, leading to increased civilian casualties, torture, and arbitrary detention. The legal landscape for armed conflict would fundamentally shift, with severe humanitarian consequences.
% FOUNDING_PROBLEM: The original Geneva Conventions left gaps in protection for non-international armed conflicts and for individuals not formally recognized as combatants, leading to widespread abuses in 'grey zone' conflicts.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations, UN bodies, and independent legal scholars consistently attest that the problem of protecting all persons in armed conflict remains live, citing ongoing conflicts where individuals fall through the cracks of narrower interpretations of IHL. This corroboration comes from outside the direct beneficiaries (non-state groups) and victims (state militaries).
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__universal_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__universal_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__universal_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(geneva_conventions_protective_scope__universal_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__universal_rights_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__universal_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__universal_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because this reading imposes significant costs on state military and intelligence operations, requiring them to adapt tactics, targeting, and detention practices. Suppression (0.70) is high due to the active resistance from states and the need for continuous advocacy and legal enforcement to uphold this expanded scope. Theater ratio (0.20) is relatively low, as the efforts to expand and enforce this reading are genuine, though often met with performative compliance or outright rejection by some states. Resistance (0.75) is high, reflecting the ongoing struggle to implement this reading against powerful state interests. Accessibility collapse (0.40) is moderate, as alternative (narrower) interpretations of IHL still exist and are actively pursued by some states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state military forces, this reading is highly extractive, imposing undue burdens and risks. From the perspective of human rights advocates and civilian populations, it is a necessary coordination mechanism to mitigate suffering. The engine's classification will reflect this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   State military forces and intelligence agencies are the primary payers/victims, as this reading directly restricts their operational freedom (high d). Non-state armed groups, civilian populations, and detainees are the beneficiaries, gaining expanded protections (low d). International courts and human rights organizations act as agenda-setters and observers, driving the interpretation and enforcement of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading actively combats mandatrophy by re-interpreting existing legal frameworks (Common Article 3, human rights law) to address evolving conflict dynamics and ensure protections remain relevant. It prevents the original mandate from atrophying in the face of new forms of warfare and non-state actors by expanding its application, rather than allowing it to become a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_acceptance_vs_resistance,
    'To what extent is this universal rights reading genuinely accepted and integrated into state military doctrine and practice, versus being resisted or circumvented?',
    'Empirical analysis of state military manuals, rules of engagement, and judicial outcomes in domestic and international courts, particularly in cases involving non-state actors or ''unprivileged belligerents''.',
    'If acceptance is widespread, the constraint''s effective suppression might be lower (less active enforcement needed), and its classification might shift closer to a Rope. If resistance is high, it remains a Tangled Rope or Snare, requiring continuous external enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_acceptance_vs_resistance, empirical, 'The actual degree of state compliance with the universal rights reading.').

omega_variable(
    human_rights_law_integration_legitimacy,
    'Is the integration of human rights law into IHL, as foundational to this reading, a legitimate and coherent legal development, or an overreach that distorts the original intent of the Geneva Conventions?',
    'Conceptual analysis by leading international legal scholars, focusing on the historical evolution of both IHL and IHRL, and the principles of treaty interpretation. This is a debate within legal theory.',
    'If deemed an overreach, the legitimacy of this reading''s expanded scope would be weakened, potentially reducing its effective suppression and increasing resistance. If deemed coherent, its normative force would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(human_rights_law_integration_legitimacy, conceptual, 'The conceptual coherence and legitimacy of integrating IHRL into IHL for universal protection.').

omega_variable(
    operational_impact_on_state_security,
    'Does the expanded protective scope of this reading genuinely compromise state security and military effectiveness, or are these claims exaggerated by states seeking to avoid accountability?',
    'Independent military and strategic analysis, comparing operational outcomes in conflicts where this reading is applied versus those where it is not, controlling for other variables. This is a complex empirical question.',
    'If state security is genuinely compromised, it would strengthen arguments for a more balanced approach, potentially leading to a re-evaluation of the extractiveness. If claims are exaggerated, it would reinforce the need for this reading''s strict application.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(operational_impact_on_state_security, empirical, 'The actual impact of universal rights reading on state military effectiveness and security.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__universal_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1965, 0.12).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 1995, 0.18).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__universal_rights_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1949, 0.4).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1965, 0.48).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__universal_rights_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1949, 0.5).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1965, 0.58).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__universal_rights_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__universal_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, geneva_conventions_protective_scope__hybrid_proportionality_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__universal_rights_reading, international_criminal_court_jurisdiction).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Geneva Conventions Protective Scope' kernel. This 'universal rights' reading expands protections to all persons, influencing (and often conflicting with) the 'state-centric' and 'hybrid proportionality' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
