% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)
 *   domain: islamic_jurisprudence/political_theology
 *
 * SUMMARY:
 *   This constraint describes the 'revolutionary vanguard' reading of Jihad,
 *   which interprets it as an immediate individual obligation (fard 'ayn)
 *   against rulers deemed apostate and foreign occupiers. This reading
 *   bypasses traditional state authority through declarations of takfir
 *   (excommunication) and emergency jurisprudence, leading to decentralized,
 *   often indiscriminate, violence. It expands the victim set to include
 *   apostate Muslims and civilians in target areas, justified by collective
 *   guilt and the urgency of the perceived threat. This is one reading of the
 *   broader 'jihad_quranic_corpus' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.9).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.95).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Immediate Individual Obligation (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "islamic_jurisprudence/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '570541c9-cc54-4080-9d85-fb645f2319f9').
narrative_ontology:cs_kernel_codification('570541c9-cc54-4080-9d85-fb645f2319f9', fixed_text).
narrative_ontology:cs_authority_grounding('570541c9-cc54-4080-9d85-fb645f2319f9', extraction).
narrative_ontology:cs_interpretation_layer_present('570541c9-cc54-4080-9d85-fb645f2319f9').
narrative_ontology:cs_reading_relation('570541c9-cc54-4080-9d85-fb645f2319f9', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('570541c9-cc54-4080-9d85-fb645f2319f9', jihad_quranic_corpus__expansionist_legalist_reading, forecloses).
narrative_ontology:cs_axiom('570541c9-cc54-4080-9d85-fb645f2319f9', foundational, takfir_justifies_individual_jihad).
narrative_ontology:cs_axiom_status(takfir_justifies_individual_jihad, holdable).
narrative_ontology:cs_axiom_grounding('570541c9-cc54-4080-9d85-fb645f2319f9', takfir_justifies_individual_jihad, theological).
narrative_ontology:cs_axiom('570541c9-cc54-4080-9d85-fb645f2319f9', foundational, emergency_overrides_classical_jurisprudence).
narrative_ontology:cs_axiom_status(emergency_overrides_classical_jurisprudence, holdable).
narrative_ontology:cs_axiom_grounding('570541c9-cc54-4080-9d85-fb645f2319f9', emergency_overrides_classical_jurisprudence, theological).
narrative_ontology:cs_reference_frame('570541c9-cc54-4080-9d85-fb645f2319f9', early_islamic_revolutionary_purity).
narrative_ontology:cs_drift_state('570541c9-cc54-4080-9d85-fb645f2319f9', contemporary_global_jihad_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('570541c9-cc54-4080-9d85-fb645f2319f9', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_muslim_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret religious texts to declare rulers apostate and call for immediate, decentralized jihad. They gain authority and legitimacy by framing themselves as the true defenders of Islam against corruption and foreign influence. Their power is derived from ideological commitment and the mobilization of followers.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, revolutionary_vanguard_leaders, agenda_setter,
    institutional, generational, identity_locked, global).

% Are compelled by the doctrine to engage in violent acts, often sacrificing their lives. They perceive themselves as fulfilling a divine command and achieving spiritual salvation, gaining status within the vanguard. Their identity is fused with the cause, making exit unthinkable.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, payer,
    powerless, immediate, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, radicalized_individuals, beneficiary).

% Are declared legitimate targets for violence, facing direct attacks and delegitimization campaigns. They are trapped by the theological declaration, which bypasses their secular authority and mobilizes segments of the population against them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers, payer,
    powerful, biographical, trapped, national).

% Are targeted as invaders and infidels, facing asymmetric warfare. Their presence is used to justify the immediate obligation of jihad, making their withdrawal a primary goal of the vanguard.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_forces, payer,
    institutional, biographical, constrained, regional).

% Are caught in the violence, often suffering casualties, displacement, and disruption of daily life. They are deemed complicit or collateral damage by the vanguard's emergency jurisprudence, losing non-combatant immunity.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_target_areas, payer,
    powerless, immediate, trapped, local).

% Are delegitimized by the vanguard for not adhering to its interpretation of jihad. They would object to the violence, the takfir declarations, and the bypassing of established religious authority, but their voices are suppressed by the vanguard's ideological purity tests and threats.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_muslim_communities, excluded,
    organized, generational, constrained, global).

% Analyze and critique the vanguard's interpretations, arguing they deviate from mainstream Islamic jurisprudence regarding legitimate authority for jihad, conditions for takfir, and rules of engagement. They provide counter-narratives but lack direct enforcement power over the vanguard.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_islamic_scholars, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes individuals into a decentralized, ideologically coherent fighting force against perceived enemies, bypassing traditional state and religious hierarchies. It coordinates individual acts of violence towards a common revolutionary goal.
% TRANSFER_FUNCTION: Transfers the obligation of jihad from state authority to individuals, transferring the burden of warfare and its consequences (including martyrdom) to radicalized individuals, and transferring the costs of violence (lives, stability) to target rulers, occupiers, and civilians.
% ABSENT_VOICES: Mainstream Islamic scholars and communities, who would argue for a more constrained, state-sanctioned, and defensive understanding of jihad, are excluded by the vanguard's takfir declarations and its rejection of traditional authority. They are often targeted as 'hypocrites' or 'sellouts'.
% DISAPPEARANCE_RATIONALE: If this reading of jihad vanished, the primary ideological justification for many non-state armed groups would collapse. Decentralized violence against 'apostate' regimes and occupiers would significantly diminish, and the global landscape of political Islam would shift dramatically, likely empowering more traditional, state-centric interpretations of jihad.
% FOUNDING_PROBLEM: The perceived corruption and apostasy of Muslim rulers, coupled with foreign occupation of Muslim lands, leading to a state of humiliation and injustice for the global Muslim community.
% FOUNDING_PROBLEM_CORROBORATION: The perception of corrupt rulers and foreign occupation is widely attested by various political and social movements across the Muslim world, not just the vanguard. However, the vanguard's specific theological solution (takfir, individual obligation) is contested by mainstream religious authorities and international observers, who offer alternative solutions to the same underlying problems.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very high (0.9) because this reading demands ultimate sacrifice (life) from its adherents and imposes severe costs on its targets, including non-combatants. Suppression is also very high (0.95) due to the ideological purity, the severe consequences for dissent (being labeled an apostate or traitor), and the suppression of alternative interpretations. Theater ratio is low (0.1) as the violence is genuinely intended to achieve its stated (revolutionary) goals, not merely to perform. The rising extractiveness and suppression over time reflect the increasing radicalization and operationalization of this interpretation since the late 20th century.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the revolutionary vanguard leaders, this is a necessary, divinely mandated coordination to restore justice and purity. From the perspective of apostate rulers, occupiers, and civilians, it is pure, unconstrained extraction and violence. Mainstream scholars see it as a dangerous deviation from established jurisprudence. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard leaders are primary beneficiaries (d=0.0-0.1) as they gain immense authority and mobilize followers. Radicalized individuals are both payers (d=0.9-1.0, bearing the ultimate cost) and beneficiaries (d=0.0-0.1, gaining spiritual salvation and status within the group). Apostate rulers, occupying forces, and civilians in target areas are clear targets (d=0.9-1.0). Mainstream Muslim communities and classical scholars are excluded or targeted for their dissent, placing them at the high end of the directionality spectrum.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    takfir_legitimacy,
    'Is the declaration of takfir (excommunication) against Muslim rulers and populations legitimate according to mainstream Islamic jurisprudence, or is it an extremist innovation?',
    'Consensus of recognized, independent Islamic legal authorities and historical precedent analysis.',
    'If illegitimate, the entire theological basis for this reading''s bypass of state authority collapses, reclassifying it from a religiously-grounded snare to a purely ideological one, with significantly reduced legitimacy and increased resistance from mainstream Muslim communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(takfir_legitimacy, conceptual, 'Legitimacy of takfir declarations as a basis for jihad.').

omega_variable(
    emergency_jurisprudence_scope,
    'Is the application of emergency jurisprudence (darura) to justify indiscriminate violence and bypass classical rules of engagement (e.g., non-combatant immunity) a valid extension or an abuse of the concept?',
    'Analysis of classical Islamic legal texts on darura and their application in historical contexts, compared with the vanguard''s contemporary interpretations.',
    'If an abuse, the justification for targeting civilians and operating without state authority is removed, significantly reducing the claimed coordination function and increasing the perceived extractiveness and illegitimacy of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_jurisprudence_scope, conceptual, 'Scope and limits of emergency jurisprudence in justifying violence.').

omega_variable(
    identity_lock_durability,
    'How durable is the identity-lock mechanism for radicalized individuals? Does it persist after exposure to counter-narratives or alternative social structures?',
    'Longitudinal studies of individuals who have disengaged from vanguard groups, assessing the persistence of ideological commitment and the mechanisms of disengagement.',
    'If the identity-lock is less durable than assumed, the suppression metric for radicalized individuals is lower, and their exit options are more ''constrained'' than ''identity_locked'', suggesting a greater potential for intervention and deradicalization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_durability, empirical, 'Durability of identity fusion in radicalized individuals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 1979, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement(jiha_tr_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(jiha_tr_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(jiha_tr_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(jiha_tr_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jiha_be_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1979, 0.7).
narrative_ontology:measurement(jiha_be_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(jiha_be_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2001, 0.9).
narrative_ontology:measurement(jiha_be_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(jiha_be_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 2024, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t1979, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1979, 0.75).
narrative_ontology:measurement(jiha_su_t1990, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(jiha_su_t2001, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2001, 0.95).
narrative_ontology:measurement(jiha_su_t2010, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2010, 0.92).
narrative_ontology:measurement(jiha_su_t2024, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 2024, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.08).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jihad_quranic_corpus' kernel. Its high extractiveness and suppression, driven by takfir and emergency jurisprudence, stand in stark contrast to the defensive_spiritual_reading and significantly diverge from the expansionist_legalist_reading's more constrained approach to offensive jihad.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
