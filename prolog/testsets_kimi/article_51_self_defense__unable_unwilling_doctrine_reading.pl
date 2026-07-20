% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__unable_unwilling_doctrine_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: article_51_self_defense__unable_unwilling_doctrine_reading
 *   human_readable: Article 51 Self-Defense: Unable or Unwilling Doctrine Reading
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the Article 51 self-defense
 *   kernel: the unable or unwilling doctrine. Under this reading, a state may
 *   use force in self-defense against a non-state actor located in another
 *   state's territory when that host state is unwilling or unable to suppress
 *   the threat. The doctrine sits between a narrow reading
 *   (state-attributable armed attack only) and an expansive preventive
 *   reading (preemptive force against emerging threats). It functions as a
 *   hybrid legal mechanism: it coordinates international counterterrorism
 *   responses by lowering the consent barrier, while asymmetrically
 *   extracting sovereignty from host states who are bypassed in the
 *   determination. The kernel is contested because the UN Charter text does
 *   not explicitly address non-state actor attacks or the unwilling/unable
 *   formula, making this reading a constructive interpretation that
 *   reallocates authority from the Security Council and host states to
 *   intervening states.
 *
 * KEY AGENTS:
 *   - Intervening states with counterterrorism mandates: assert the doctrine, conduct operations, and gain strategic autonomy (institutional/mobile/global).
 *   - Host states bypassed: lose territorial integrity and bear the direct costs of foreign military operations on their soil (moderate/constrained/national).
 *   - International Court of Justice: evaluates the doctrine's legality but lacks enforcement power over major powers (institutional/analytical/global).
 *   - UN Security Council: sidelined gatekeeper whose Chapter VII authorization function is bypassed by the doctrine (institutional/constrained/global).
 *   - Non-state actor militants: the nominal trigger for the doctrine, excluded from the legal determination (organized/trapped/regional).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.55).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.62).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unable or Unwilling Doctrine Reading").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'e066d6ae-2f5e-4bc1-b2f2-c61ed2813610').
narrative_ontology:cs_kernel_codification('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', formalized).
narrative_ontology:cs_authority_grounding('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', lineage).
narrative_ontology:cs_interpretation_layer_present('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610').
narrative_ontology:cs_reading_relation('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', article_51_self_defense__expansive_preventive_reading, forecloses).
narrative_ontology:cs_axiom('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', foundational, host_state_sovereignty_conditional_on_effective_control).
narrative_ontology:cs_axiom_status(host_state_sovereignty_conditional_on_effective_control, holdable).
narrative_ontology:cs_axiom_grounding('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', host_state_sovereignty_conditional_on_effective_control, conventional).
narrative_ontology:cs_axiom('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', foundational, non_state_actor_attack_satisfies_armed_attack_requirement).
narrative_ontology:cs_axiom_status(non_state_actor_attack_satisfies_armed_attack_requirement, holdable).
narrative_ontology:cs_axiom_grounding('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', non_state_actor_attack_satisfies_armed_attack_requirement, conventional).
narrative_ontology:cs_reference_frame('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', conditional_sovereignty_security_framework).
narrative_ontology:cs_drift_state('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', contemporary_insecurity_environment, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('e066d6ae-2f5e-4bc1-b2f2-c61ed2813610', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_bypassed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert the unable or unwilling doctrine to justify cross-border military operations against non-state actors without host state consent or UN Security Council authorization. They gain strategic flexibility, unilateral security policy autonomy, and expanded operational theaters. They administer the legal standard and determine when a host state is unwilling or unable.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, beneficiary).

% Lose territorial integrity and sovereign control when foreign powers conduct military strikes on their soil under the doctrine. They bear the costs of civilian harm, infrastructure damage, governance erosion, and diplomatic humiliation. Their formal optionsâICJ litigation, UNSC petitions, diplomatic protestârarely prevent operations by powerful intervening states.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_bypassed, payer,
    moderate, generational, constrained, national).

% Evaluates the legality of uses of force under Article 51 through advisory opinions and contentious cases. Its rulings shape the doctrinal legitimacy of the unable or unwilling standard, but it lacks enforcement mechanisms against powerful states that reject its findings.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% Has primary responsibility under the UN Charter for maintaining international peace and security. The unable or unwilling doctrine allows states to bypass Chapter VII authorization, eroding the Council's gatekeeping role and concentrating use-of-force decisions in unilateral state assessments.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, un_security_council, observer,
    institutional, civilizational, constrained, global).

% Operate from host state territory and are the nominal target of self-defense claims. They are not parties to the legal determination of whether the host state is unwilling or unable, nor to the decision to use force, yet their presence triggers the entire constraint structure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_militants, excluded,
    organized, immediate, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international counterterrorism operations across borders when a host state genuinely cannot or will not suppress non-state actor threats, providing a legal pathway for security action that does not require UN Security Council authorization for every non-state threat.
% TRANSFER_FUNCTION: Moves the cost and risk of security enforcement from intervening states (who avoid the political and procedural burden of gaining consent or Council authorization) to host states (whose territorial sovereignty is overridden and whose populations bear the direct costs of military operations).
% ABSENT_VOICES: Civilian populations in host states suffer the direct effects of strikes but are not represented in the legal determination; weaker states lacking counterterrorism capacity face the doctrine asymmetrically and would be subjected to it rather than able to invoke it; non-state actors are excluded from the legal framework despite being its trigger.
% DISAPPEARANCE_RATIONALE: If the doctrine vanished, intervening states would lose their primary legal justification for unilateral cross-border counterterrorism operations. They would need to rely consistently on host state consent or UN Security Council authorization, shifting the institutional balance back toward sovereign equality and collective security gatekeeping. Host states would regain stronger territorial integrity claims, and the global security architecture would reorganize around formal multilateral authorization.
% FOUNDING_PROBLEM: Non-state actors operating from territories of weak, failed, or complicit states pose security threats that the UN collective security system is too slow or politically gridlocked to address through timely Chapter VII authorization.
% FOUNDING_PROBLEM_CORROBORATION: Independent international legal scholars, many Global South states, and humanitarian organizations attest that while non-state threats are real, the doctrine solves them at unacceptable sovereignty cost and often functions as cover for geostrategic objectives. The ICJ has not formally endorsed the unable or unwilling standard, and legal scholarship outside counterterrorism policy circles is deeply divided. Corroboration from outside the beneficiary set is split rather than unified.
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__unable_unwilling_doctrine_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is moderate but significant: the doctrine permits unilateral military action without host state consent or UNSC authorization, transferring the costs of security enforcement onto host states. Suppression (0.62) reflects that the constraint's persistence depends on powerful states actively maintaining the legal argument and military capacity to override sovereignty claims, while suppressing alternative legal frameworks (strict consent, Security Council primacy). Theater ratio (0.45) is moderate: a substantial portion of legal scholarship and diplomatic justification is performative, dressing power politics in legal form, though genuine security coordination also occurs. Accessibility collapse (0.50) is partial: alternatives (UNSC authorization, host-state consent) remain formally available but are practically foreclosed for weak states once the doctrine is accepted by powerful interveners. Resistance (0.55) captures sustained diplomatic and legal objections from target states and many Global South actors. The measurement series tracks the doctrine's evolution from post-9/11 emergence through the expansion of drone warfare and special operations, showing rising extraction and theater that have plateaued as legal contestation has intensified.
 *
 * PERSPECTIVAL GAP:
 *   The intervening state seat experiences the constraint as a necessary coordination tool that solves the collective-action problem of terrorist safe havens in failed states. The host state seat experiences the same structure as extraction: a unilateral override of territorial sovereignty that externalizes the costs of foreign security policy. The engine computes this divergence from the structural dataâbeneficiary declarations, victim declarations, and the stark asymmetry in exit options (mobile vs. constrained). The ICJ seat is analytical and computes yet another type, observing the gap between doctrinal assertion and institutional acceptance.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states are the structural beneficiaries: they collect operational flexibility, unilateral decision-making authority, and reduced legal friction (d near the beneficiary end). Host states are the structural victims: they bear sovereignty costs, territorial incursions, and instability (d near the target end). The UNSC is a sidelined observer whose authority is eroded by the doctrine's operation. Non-state actors are excluded from the legal framework entirely. Directionality is derived from these structural relationships without override.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the doctrine as pure coordination (Rope) by requiring explicit victim identification: host states bypassed are not merely inconvenienced but suffer a direct transfer of sovereignty costs. It prevents mislabeling as pure extraction (Snare) by acknowledging the genuine coordination functionâthe doctrine does address a real gap in the UN collective security system regarding non-state actors in ungoverned spaces. The Tangled Rope classification captures both faces. The founding problem status is contested, suggesting the doctrine may be drifting toward Piton if the coordination rationale atrophies further into routine unilateralism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'Does the unable or unwilling doctrine represent a discoverable interpretation of Article 51 consistent with the Charter''s textual lineage, or a constructive expansion that creates new state authority beyond the original kernel?',
    'Historical-legal analysis of the 1945 Charter negotiating history and subsequent state practice up to 2001, compared with the doctrine''s post-2001 emergence in scholarship and official justifications.',
    'If the doctrine lacks textual lineage, it is a constructed constraint benefiting intervening states under cover of legal continuity; if rooted in the text, the extraction is more plausibly the price of coordination within the Charter framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Ambiguity over whether this reading discovers or constructs the legal authority it claims.').

omega_variable(
    host_state_capacity_test,
    'Is the ''unable or unwilling'' standard an objective empirical assessment of host state capacity, or a rhetorical frame that permits subjective self-judgment by intervening states?',
    'Systematic case comparison of interventions invoking the doctrine, measuring host state capacity indicators against the claims made by intervening states.',
    'If applied subjectively without objective capacity assessment, the constraint functions more as a snare (cover for unilateralism) than a tangled rope (genuine coordination with asymmetric cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_state_capacity_test, empirical, 'Whether the unwilling or unable determination is objectively verifiable or a captured standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_51_uu_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(article_51_uu_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(article_51_uu_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.35).
narrative_ontology:measurement(article_51_uu_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.42).
narrative_ontology:measurement(article_51_uu_tr_t16, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 16, 0.48).
narrative_ontology:measurement(article_51_uu_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.46).
narrative_ontology:measurement(article_51_uu_tr_t23, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 23, 0.45).

% Extraction over time
narrative_ontology:measurement(article_51_uu_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(article_51_uu_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.4).
narrative_ontology:measurement(article_51_uu_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(article_51_uu_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(article_51_uu_be_t16, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(article_51_uu_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(article_51_uu_be_t23, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 23, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(article_51_uu_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(article_51_uu_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(article_51_uu_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(article_51_uu_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(article_51_uu_su_t16, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(article_51_uu_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(article_51_uu_su_t23, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 23, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 51 self-defense kernel. The natural-language label 'Article 51 self-defense' conflates three structurally distinct claims: the narrow armed attack reading (state-attribution required), the unable/unwilling doctrine reading (non-state actor attacks with host state failure), and the expansive preventive reading (preemptive force against emerging threats). Their epsilon values, beneficiary structures, and empirical statuses differ. They are modeled as separate linked stories, not as one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
