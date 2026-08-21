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
 *   This constraint represents the 'stateless coordination' reading of the
 *   blood-feud kernel. It describes blood-feud obligations as a functional,
 *   self-enforcing mechanism for justice and deterrence in societies lacking
 *   centralized state authority. While it involves significant costs
 *   (retaliation, social sanctions), this reading emphasizes the net benefit
 *   of maintaining social order and accountability where no other system
 *   exists. The claimed type is Tangled Rope, acknowledging both the
 *   coordination function and the asymmetric costs borne by those targeted or
 *   those who defect.
 *
 * KEY AGENTS:
 *   - Kin groups seeking justice: Primary beneficiaries (organized/constrained) — receive redress and honor restoration.
 *   - Community members seeking deterrence: Secondary beneficiaries (moderate/constrained) — benefit from general social order.
 *   - Defectors from obligation: Primary targets/victims (powerless/trapped) — bear honor loss and expulsion.
 *   - Kin groups targeted by feud: Secondary targets/victims (organized/constrained) — bear costs of retaliation.
 *   - Neutral arbiters or elders: Agenda-setters (powerful/mobile) — mediate and facilitate alternative resolutions.
 *   - Analytical historians: Observers (analytical/analytical) — study the system's function.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.55).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.2).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligations (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '887db768-ddb4-44d4-90d4-6f6bdebba57a').
narrative_ontology:cs_kernel_codification('887db768-ddb4-44d4-90d4-6f6bdebba57a', implicit).
narrative_ontology:cs_authority_grounding('887db768-ddb4-44d4-90d4-6f6bdebba57a', practice).
narrative_ontology:cs_reading_relation('887db768-ddb4-44d4-90d4-6f6bdebba57a', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('887db768-ddb4-44d4-90d4-6f6bdebba57a', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('887db768-ddb4-44d4-90d4-6f6bdebba57a', foundational, retribution_is_justice).
narrative_ontology:cs_axiom_status(retribution_is_justice, holdable).
narrative_ontology:cs_axiom_grounding('887db768-ddb4-44d4-90d4-6f6bdebba57a', retribution_is_justice, deontological).
narrative_ontology:cs_axiom('887db768-ddb4-44d4-90d4-6f6bdebba57a', foundational, kin_solidarity_is_obligation).
narrative_ontology:cs_axiom_status(kin_solidarity_is_obligation, holdable).
narrative_ontology:cs_axiom_grounding('887db768-ddb4-44d4-90d4-6f6bdebba57a', kin_solidarity_is_obligation, conventional).
narrative_ontology:cs_reference_frame('887db768-ddb4-44d4-90d4-6f6bdebba57a', pre_state_social_order).
narrative_ontology:cs_drift_state('887db768-ddb4-44d4-90d4-6f6bdebba57a', historical_period_of_operation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('887db768-ddb4-44d4-90d4-6f6bdebba57a', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, kin_groups_targeted_by_feud).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups receive a mechanism for redress and honor restoration when one of their members is wronged, ensuring accountability in a stateless context. They are bound by the obligation to participate in feuds when necessary.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice, beneficiary,
    organized, biographical, constrained, local).

% Individuals and families within the community benefit from the general deterrence against aggression and transgression that the threat of feud provides, contributing to a degree of social order. They are also subject to the obligations of kin solidarity.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence, beneficiary,
    moderate, biographical, constrained, local).

% Individuals or kin groups who fail to uphold their feud obligations face severe social sanctions, including honor loss, ostracism, and expulsion from their kin network, leaving them vulnerable and without protection.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation, payer,
    powerless, immediate, trapped, local).

% These groups bear the direct costs of retaliation, including loss of life, property, and social standing, when one of their members commits a transgression. They are compelled to either endure the costs or seek resolution through negotiation (e.g., wergild).
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_targeted_by_feud, payer,
    organized, biographical, constrained, local).

% Respected individuals or councils who mediate disputes, negotiate wergild payments, and help manage the escalation or de-escalation of feuds, acting as a check on unchecked violence and facilitating alternative resolutions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, neutral_arbiters_or_elders, agenda_setter,
    powerful, biographical, mobile, local).

% Scholars who study historical and anthropological records to understand the function and dynamics of blood feuds in stateless societies, analyzing their role in social order and conflict resolution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, analytical_historians, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a self-enforcing framework for resolving grievances, deterring aggression, and maintaining social order in societies lacking centralized state authority, by establishing clear obligations for kin groups to seek justice for wrongs.
% TRANSFER_FUNCTION: Transfers the burden of justice and deterrence from a non-existent state to kin groups, and transfers the cost of transgression (retaliation, honor loss, expulsion) to offending kin groups or individuals.
% ABSENT_VOICES: Those who advocate for absolute peace or a centralized, non-violent justice system would object, but such alternatives are often structurally unavailable or ideologically suppressed within the context of stateless societies where feuds operate.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight in a stateless society, the primary mechanism for justice, deterrence, and inter-group conflict resolution would disappear, likely leading to widespread anarchy, unchecked aggression, or the rapid emergence of alternative, potentially more brutal, forms of social control.
% FOUNDING_PROBLEM: The absence of a centralized state or legal authority capable of enforcing justice, deterring crime, and resolving inter-group conflicts, leading to a need for self-help justice mechanisms.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of contemporary stateless societies, historical accounts of early medieval Europe, and comparative political science analyses of state formation consistently corroborate the problem of statelessness and the emergence of such mechanisms. This corroboration comes from outside the direct participants in the feud system.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the inherent costs of feuding (violence, resource drain) even within a functional system, but it's not overwhelmingly high because the system provides a necessary service. Suppression (0.20) is low because alternative dispute mechanisms like wergild (blood money) often coexist and are sometimes preferred, indicating that participation in feuds is not strictly coerced but rather a strong social obligation. Theater ratio (0.10) is low as the system is genuinely functional, not performative. Accessibility collapse (0.40) is moderate, as alternatives exist but are not always sufficient or preferred. Resistance (0.20) is low because the system is largely self-enforcing and culturally embedded.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of kin groups upholding their obligations, the system provides essential justice and maintains honor. From the perspective of defectors or those targeted by feuds, it is a costly and potentially destructive mechanism. The engine's per-seat classification will highlight this divergence, with beneficiaries experiencing a Rope-like function and victims experiencing a Snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups seeking justice and community members seeking deterrence are beneficiaries (low d) as they gain social order and redress. Defectors and targeted kin groups are victims (high d) as they bear the direct costs and sanctions. Neutral arbiters act as agenda-setters, managing the system's operation. The low suppression and coexistence of alternatives mean that even for victims, the 'trapped' status is more about social obligation than absolute physical coercion, though the consequences of exit are severe.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the feud system as pure extraction by emphasizing its genuine coordination function in a stateless context. It acknowledges the costs but frames them as part of a necessary, albeit violent, social technology. The 'live' status of the founding problem (absence of state authority) further supports its ongoing function, preventing a Piton classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_balance,
    'Does the ''justice'' and ''deterrence'' provided by the feud system genuinely outweigh the ''extraction'' of violence, resource drain, and social disruption it causes?',
    'Comparative analysis with other stateless societies that developed alternative, less violent, coordination mechanisms, or detailed economic and social cost-benefit analysis of historical feud cycles.',
    'If extraction is found to consistently outweigh coordination, the constraint would reclassify closer to a Snare or Tangled Rope with higher extractiveness, even within this reading''s framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_balance, empirical, 'The true balance between the coordination benefits and extractive costs of the feud system.').

omega_variable(
    stateless_coordination_vs_extraction_cycle_framing,
    'Is this constraint primarily a ''stateless coordination mechanism'' (this reading) or a ''destructive extraction cycle'' (the sibling reading)?',
    'Analysis of long-term societal stability, population growth, and resource accumulation under the feud system versus its absence or alternative systems. If it consistently depletes productive capacity, the extraction cycle framing gains strength.',
    'Adopting the ''extraction_cycle_reading'' would significantly increase the measured extractiveness and likely reclassify the constraint as a Snare, emphasizing its destructive aspects.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stateless_coordination_vs_extraction_cycle_framing, conceptual, 'Ambiguity in framing the feud system''s primary function and impact.').

omega_variable(
    stateless_coordination_vs_christianized_pacification_framing,
    'Is the feud system a legitimate form of justice in its context (this reading) or a violation of higher moral/divine law (the ''christianized pacification'' reading)?',
    'This is a preference-based question, resolvable only by adopting a specific moral or theological framework. Empirical data cannot resolve the normative conflict.',
    'Adopting the ''christianized_pacification_reading'' would frame the feud as morally illegitimate, likely leading to a Snare classification due to its perceived violation of divine law and suppression of ''true'' justice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stateless_coordination_vs_christianized_pacification_framing, preference, 'Normative conflict regarding the moral legitimacy of the feud system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 25, 0.1).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(feud_tr_t75, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 75, 0.1).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement(feud_be_t75, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 75, 0.57).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 25, 0.2).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(feud_su_t75, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 75, 0.2).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feud_obligation_kernel'. This 'stateless_coordination_reading' emphasizes the functional aspects of feuds in maintaining order, contrasting with the 'extraction_cycle_reading' (which highlights destructive costs) and the 'christianized_pacification_reading' (which focuses on moral illegitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
