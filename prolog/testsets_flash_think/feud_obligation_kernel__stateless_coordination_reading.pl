% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligations (Stateless Coordination Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'stateless coordination' reading
 *   of the blood-feud kernel. It describes blood-feud obligations as a
 *   functional, self-enforcing mechanism for justice and deterrence in
 *   societies lacking centralized authority. The system coordinates kin
 *   groups to respond to offenses, thereby creating a deterrent effect and a
 *   means of dispute resolution. While it involves costs and risks for
 *   participants, and sanctions for defectors, its primary function, from
 *   this reading, is to provide social order where none would otherwise
 *   exist. The metrics reflect a system that is largely functional, with
 *   moderate costs of participation and low suppression of alternatives like
 *   wergild.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.35).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.25).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligations (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '3e7402b7-4454-4d7b-850a-ae575b6ced60').
narrative_ontology:cs_kernel_codification('3e7402b7-4454-4d7b-850a-ae575b6ced60', implicit).
narrative_ontology:cs_authority_grounding('3e7402b7-4454-4d7b-850a-ae575b6ced60', practice).
narrative_ontology:cs_reading_relation('3e7402b7-4454-4d7b-850a-ae575b6ced60', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e7402b7-4454-4d7b-850a-ae575b6ced60', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('3e7402b7-4454-4d7b-850a-ae575b6ced60', foundational, retaliation_as_deterrence).
narrative_ontology:cs_axiom_status(retaliation_as_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('3e7402b7-4454-4d7b-850a-ae575b6ced60', retaliation_as_deterrence, instrumental).
narrative_ontology:cs_axiom('3e7402b7-4454-4d7b-850a-ae575b6ced60', foundational, kinship_solidarity_as_enforcement).
narrative_ontology:cs_axiom_status(kinship_solidarity_as_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('3e7402b7-4454-4d7b-850a-ae575b6ced60', kinship_solidarity_as_enforcement, conventional).
narrative_ontology:cs_reference_frame('3e7402b7-4454-4d7b-850a-ae575b6ced60', stateless_justice_equilibrium).
narrative_ontology:cs_drift_state('3e7402b7-4454-4d7b-850a-ae575b6ced60', early_medieval_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3e7402b7-4454-4d7b-850a-ae575b6ced60', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_participants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and their immediate kin who engage in the feud system. They receive a form of justice and deterrence against aggression, but also bear the direct costs and risks of participation, including potential injury, death, or property loss. Their participation is often driven by honor and kinship loyalty.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, feud_participants, payer).

% The primary social units responsible for enforcing feud obligations. They benefit from the system's ability to protect their members and maintain their collective honor and standing within the community. They set and administer the norms, and organize retaliatory actions or wergild negotiations. Exit means loss of social standing and protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, kin_groups, beneficiary).

% Individuals or kin groups who fail to uphold their feud obligations, either by not seeking vengeance when required or by refusing to pay wergild. They face severe social sanctions, including honor loss, ostracization, and potential expulsion from their kin network, leaving them vulnerable.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors, payer,
    powerless, immediate, trapped, local).

% Nascent state or religious institutions (e.g., early medieval church, emerging monarchies) that would seek to monopolize legitimate violence and replace feuding with centralized legal systems. They are structurally excluded from the operation of the feud system itself, but their influence grows over time.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, external_authorities, excluded,
    institutional, generational, analytical, regional).

% Scholars who study the historical and anthropological function of blood feuds, analyzing their role in social order and dispute resolution in stateless societies. They observe the system from an external, analytical perspective.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a self-enforcing framework for resolving grievances, deterring aggression, and maintaining social order in societies lacking centralized legal or enforcement capacity, by making violence costly for offenders and their kin.
% TRANSFER_FUNCTION: Transfers the burden of justice and enforcement from a non-existent centralized authority to kin groups, and transfers the cost of offense (retaliation, wergild) to the offending kin group, thereby restoring balance and deterring future transgressions.
% ABSENT_VOICES: Early state-builders and religious authorities would object, arguing for their own monopoly on legitimate violence and the moral superiority of their legal systems. Victims of excessive or misdirected feuds, or those caught in escalating cycles, might also be unheard, as the system prioritizes kin-group honor over individual suffering.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight in a stateless society, the primary mechanism for dispute resolution and deterrence would collapse. This would likely lead to unchecked aggression, pervasive insecurity, and potentially the rapid emergence of alternative, possibly more extractive or brutal, systems to fill the vacuum.
% FOUNDING_PROBLEM: The absence of a centralized state or legal system capable of enforcing justice, deterring aggression, and maintaining order, leading to pervasive insecurity and unpunished wrongs within a community.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of contemporary stateless societies, historical accounts of early medieval Europe, and comparative political science analyses of institutional development corroborate that the problem of stateless order was (and in some contexts, remains) live, and that such mechanisms served a functional role. This corroboration comes from outside the direct participants in the feud system.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) reflects the inherent costs and risks of participating in a feud, even when justified, and the sanctions for defectors. Suppression (0.25) is low because alternative dispute resolution mechanisms (like wergild or arbitration) often coexist and are sometimes preferred, indicating that participation is not entirely coerced but rather a choice within a constrained set. Theater ratio is low (0.10) because the system is understood as genuinely functional in its context. The temporal measurements reflect a period of relative stability and functionality for the system, with slight fluctuations in costs and enforcement as external pressures (like emerging states) begin to appear.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of kin groups and participants, the system is a necessary and functional means of survival and justice. From the perspective of defectors, it is a harsh and unforgiving system. External authorities (e.g., early states) would view it as barbaric and inefficient, seeking to replace it with their own, more centralized, forms of justice. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups and feud participants are beneficiaries, as they gain protection and a means of justice, though they also bear costs. Defectors are targets, facing severe social and physical consequences for non-compliance. External authorities are excluded, as their claims to legitimate violence are outside the system's operational logic. The system is 'self-enforcing' through internal social pressure and retaliatory action, rather than external coercion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_ambiguity,
    'Is the blood-feud system primarily a coordination mechanism providing order, or a destructive extraction cycle that depletes social capital?',
    'Comparative historical analysis of long-term societal outcomes in feud-based vs. non-feud-based stateless societies, focusing on demographic stability, economic productivity, and internal conflict rates.',
    'If primarily extractive, the constraint''s effective extractiveness would be higher, potentially reclassifying it as a Snare or Tangled Rope. If primarily coordination, the current Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_ambiguity, empirical, 'Ambiguity between the ''stateless coordination'' and ''extraction cycle'' readings.').

omega_variable(
    effectiveness_vs_cost_balance,
    'Does the deterrence and justice provided by the feud system genuinely outweigh the social and economic costs (e.g., lives lost, resources diverted, psychological toll) for the participating community?',
    'Detailed micro-historical studies and anthropological ethnographies that quantify the direct and indirect costs of feuding against its perceived benefits over extended periods.',
    'If costs consistently outweigh benefits, the system''s coordination function is undermined, increasing its effective extractiveness and potentially shifting classification towards Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_cost_balance, empirical, 'Balance between the functional benefits and the inherent costs of the feud system.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the low measured suppression due to genuine availability of alternatives (like wergild), or is it an artifact of the strong internalized social pressure and identity-lock that makes non-participation unthinkable for most kin-group members?',
    'Analysis of individual narratives and historical records detailing attempts to opt out of feud obligations and the social consequences. If opting out is rare and severely punished, internalized suppression is higher.',
    'If internalized suppression is higher, the constraint''s effective suppression is higher than the structural measure suggests, making it more coercive than it appears.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in a kin-based system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.13).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.32).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 60, 0.33).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 80, 0.3).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 60, 0.23).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, identity_coordination).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feud_obligation_kernel', which also includes 'extraction_cycle_reading' and 'christianized_pacification_reading'. Each reading offers a distinct structural interpretation of the same historical phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
