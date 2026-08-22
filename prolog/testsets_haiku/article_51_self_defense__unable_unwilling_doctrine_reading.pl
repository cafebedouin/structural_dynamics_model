% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__unable_unwilling_doctrine_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   human_readable: Article 51 Self-Defense: Unwilling/Unable Host State Doctrine
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   The unable/unwilling doctrine reads Article 51 of the UN Charter to
 *   permit self-defense against non-state actor attacks originating from host
 *   states that lack capacity or political will to suppress the threat. This
 *   reading creates a hybrid constraint: it preserves the state-centric
 *   international law framework while permitting unilateral intervention into
 *   other states' territory. Intervening states claim coordination benefit
 *   (addressing collective security gaps); host states experience it as
 *   sovereignty erosion. The constraint is claimed as tangled_rope (genuine
 *   coordination function + asymmetric extraction) because it does solve a
 *   real collective-action problem (non-state threats the host state cannot
 *   suppress) while permitting powerful states to bypass the Security Council
 *   and extract strategic advantage by defining 'unwilling' and 'unable'
 *   unilaterally.
 *
 * KEY AGENTS:
 *   - Intervening states with counterterrorism mandates (agenda-setters; define unwilling/unable; initiate interventions)
 *   - Host states harboring non-state actors (payers; suffer sovereignty erosion; cannot control intervention trigger)
 *   - Non-state actors in theater (trapped payers; targeted by interventions justified by the doctrine)
 *   - International Security Council (formally preserved beneficiary but structurally bypassed)
 *   - Weak states without military capacity (excluded; vulnerable to doctrine application; cannot contest terms)
 *   - Regional rivals using the doctrine as cover (excluded from the framing; their competitive motives cannot be voiced)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68).
domain_priors:suppression_score(article_51_self_defense__unable_unwilling_doctrine_reading, 0.71).
domain_priors:theater_ratio(article_51_self_defense__unable_unwilling_doctrine_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(article_51_self_defense__unable_unwilling_doctrine_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__unable_unwilling_doctrine_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__unable_unwilling_doctrine_reading, "Article 51 Self-Defense: Unwilling/Unable Host State Doctrine").
narrative_ontology:topic_domain(article_51_self_defense__unable_unwilling_doctrine_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__unable_unwilling_doctrine_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__unable_unwilling_doctrine_reading, 'cf8e7324-56f1-4af7-bfee-e75e457b3e9d').
narrative_ontology:cs_kernel_codification('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', fixed_text).
narrative_ontology:cs_authority_grounding('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', lineage).
narrative_ontology:cs_interpretation_layer_present('cf8e7324-56f1-4af7-bfee-e75e457b3e9d').
narrative_ontology:cs_reading_relation('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', article_51_self_defense__narrow_armed_attack_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', article_51_self_defense__expansive_preventive_reading, influences).
narrative_ontology:cs_axiom('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', foundational, non_state_actor_attacks_trigger_self_defense).
narrative_ontology:cs_axiom_status(non_state_actor_attacks_trigger_self_defense, holdable).
narrative_ontology:cs_axiom_grounding('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', non_state_actor_attacks_trigger_self_defense, deontological).
narrative_ontology:cs_axiom('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', foundational, host_state_capacity_conditions_sovereignty).
narrative_ontology:cs_axiom_status(host_state_capacity_conditions_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', host_state_capacity_conditions_sovereignty, empirically_contingent).
narrative_ontology:cs_axiom('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', secondary, unilateral_response_permitted_absent_state_action).
narrative_ontology:cs_axiom_status(unilateral_response_permitted_absent_state_action, holdable).
narrative_ontology:cs_axiom_grounding('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', unilateral_response_permitted_absent_state_action, instrumental).
narrative_ontology:cs_reference_frame('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', classical_state_centric_collective_security).
narrative_ontology:cs_drift_state('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', contemporary_counterterrorism_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf8e7324-56f1-4af7-bfee-e75e457b3e9d', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_harboring_non_state_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_51_self_defense__unable_unwilling_doctrine_reading, international_security_council).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_victims_in_theater).
narrative_ontology:constraint_victim(article_51_self_defense__unable_unwilling_doctrine_reading, weak_states_without_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defend the unable/unwilling doctrine as a legitimate self-defense framework against non-state actor threats that originate from territories outside their control. Set the interpretive standard: a host state's unwillingness or inability to suppress a harbored threat triggers the intervening state's right to unilateral response without prior Security Council authorization. Argue this doctrine coordinates collective security by filling the gap when the host state fails to act, while maintaining the fiction of host-state responsibility.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates, agenda_setter,
    institutional, generational, arbitrage, global).

% Bear the cost of sovereignty erosion: the doctrine permits unilateral military intervention into their territory by any state claiming the host is unwilling or unable to suppress a threat. Their borders become porous; their control over their own airspace and territory is conditional on demonstrable counterterrorism capacity. Exit options are limited—they cannot abandon the territory or unilaterally eject non-state actors that other states claim to be intervening against, and they face pressure to either suppress the threat preemptively or accept incursions.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, host_states_with_harboring_non_state_actors, payer,
    powerful, generational, constrained, regional).

% Subjected to intervention operations—drone strikes, raids, targeted killing—conducted under the doctrine's rationale. Their presence in the host territory is framed as justifying intervention into the host state itself. They cannot exit the theater without ceasing operations or relocating to states with greater suppression capacity.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, non_state_actor_victims_in_theater, payer,
    powerless, immediate, trapped, local).

% The doctrine benefits the permanent members (particularly those with global counterterrorism mandates) by creating a lawful pathway for military action outside the formal authorization mechanism. The Council is formally preserved as the adjudicator of international peace and security, but the doctrine permits major states to bypass Security Council authorization when claiming unable/unwilling circumstances. The Council's role is reduced to post-hoc acknowledgment or diplomatic negotiation.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_security_council, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, international_security_council, agenda_setter).

% Structurally vulnerable to the doctrine's application: limited military or counterterrorism capacity, harboring some non-state actors (willingly or unwillingly), and no ability to defend their airspace or territory against major-power intervention. The doctrine makes their weakness a trigger for external intervention justified as self-defense. They are excluded from the negotiation of what 'unwilling' and 'unable' mean—stronger states define the terms unilaterally.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, weak_states_without_capacity, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__unable_unwilling_doctrine_reading, weak_states_without_capacity, excluded).

% Could invoke the doctrine against regional competitors under the guise of counterterrorism. They are excluded from the doctrine's framing because acknowledging regional competition as the motive would expose the doctrine's cover story. Their exclusion is structural: admitting that the doctrine licenses competitive intervention would undermine the 'collective security' narrative.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, regional_rivals_and_competitors, excluded,
    institutional, generational, constrained, regional).

% Interprets the doctrine's application to specific cases. Acts as an external analytical seat that receives post-hoc justifications for interventions and evaluates their legal coherence. Lacks enforcement power over state action but provides interpretive authority that shapes subsequent doctrine development.
narrative_ontology:constraint_stakeholder(article_51_self_defense__unable_unwilling_doctrine_reading, international_court_of_justice, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__unable_unwilling_doctrine_reading, intervening_states_with_counterterrorism_mandates).
narrative_ontology:fixing_cost_class(article_51_self_defense__unable_unwilling_doctrine_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective security response to non-state actor threats that originate from territories where state control is absent or ineffective. By permitting intervention when host states are unable or unwilling to suppress threats, the doctrine aims to solve the coordination problem of transnational terrorism—no single state can interdict a threat before it reaches them if the host state will not act, and the Security Council cannot respond quickly enough. The doctrine substitutes unilateral determination for centralized authorization.
% TRANSFER_FUNCTION: Transfers the right to use force unilaterally from the Security Council (the formal arbiter of collective security under Article 42) to individual states claiming self-defense circumstances. Moves sovereignty constraints from intervening states (which are normally bound by territorial borders and state sovereignty) to host states (whose sovereign immunity becomes conditional on demonstrated suppression capacity). Moves the burden of proof: host states must demonstrate ability/willingness to suppress; intervening states need only claim the doctrine's conditions are met.
% ABSENT_VOICES: Weak states lacking counterterrorism capacity (who become targets for intervention under the doctrine's rationale) are excluded from negotiating what 'unwilling' and 'unable' mean. Non-state actors whose presence triggers the doctrine have no seat at the table. Regional competitors of intervening states cannot voice their concern that the doctrine licenses pretextual intervention without risking exposure of competitive motives. The doctrine's beneficiaries control the interpretive authority; the doctrine's victims have no standing to contest the framing.
% DISAPPEARANCE_RATIONALE: If the unable/unwilling doctrine were nullified overnight, intervening states would lose their legal cover for unilateral counterterrorism operations in foreign territory. They would be forced back to either seeking Security Council authorization (which P5 veto holders could block) or acting without legal authority. Host states would recover their classical sovereign immunity—borders would become genuinely inviolable absent Security Council action. The distribution of security burdens and military prerogatives would radically shift; states would have to negotiate collective responses rather than act unilaterally.
% FOUNDING_PROBLEM: The founding problem was the gap between transnational terrorism threats originating from weak or failed states and the inability of the Security Council to authorize collective action quickly, or the inability of weak states to suppress non-state actors harboring on their territory. Early counterterrorism operations after 2001 revealed that waiting for Security Council authorization paralyzed response to imminent threats, and that some host states lacked the capacity or will to suppress terrorist groups. The doctrine was developed to fill this gap.
% FOUNDING_PROBLEM_CORROBORATION: Intervening states and security scholars cite ongoing non-state actor threats and host-state incapacity as evidence the founding problem remains live. Host states and non-aligned states argue the founding problem was overstated and has been superseded by stronger state capacity, international coordination, and the normalization of multilateral counterterrorism. No corroborating source outside the intervening states' security establishments independently validates that the 'inability' rationale is still the dominant factor driving interventions (most documented interventions cite counterterrorism but are motivated by geopolitical competition, as regional analysts attest).
narrative_ontology:disappearance_verdict(article_51_self_defense__unable_unwilling_doctrine_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__unable_unwilling_doctrine_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__unable_unwilling_doctrine_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__unable_unwilling_doctrine_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.48 to 0.68 over the interval as the doctrine becomes normalized practice rather than exceptional invocation. Early applications were claimed as emergency self-defense (lower extraction rationale); later applications cite the doctrine routinely without detailed threat verification, indicating the enforcement object has shifted from responding to imminent threats to maintaining unilateral intervention authority. Theater ratio plateaus at 0.42 (higher than it was initially but stabilizing), indicating the doctrine's dual function: genuine counterterrorism operations coexist with geopolitical positioning that uses the doctrine as cover. Suppression requirement rises and plateaus at 0.71, showing that maintaining the doctrine's legal coherence requires active enforcement through interpretive authority—the International Court, state legal positions, diplomatic pressure—not just through military operations. The constraint is tangled_rope because (1) it coordinates collective security response by filling a genuine gap (weak host states cannot suppress transnational threats), (2) it extracts because intervening states unilaterally determine what counts as unwilling/unable, and (3) active enforcement through legal and diplomatic framing is required to keep the cover story intact against challenges from host states and non-aligned states.
 *
 * PERSPECTIVAL GAP:
 *   From the intervening states' seat, the doctrine is genuine coordination—a necessary framework to respond to non-state actor threats when host states fail. The agenda-setter seat computes the constraint as rope: coordination without significant extraction cost (other states benefit from security; no one is victimized except the non-state actors, who are the threat). From the host states' seat, the constraint is snare—the doctrine is pure cover for unilateral intervention into their territory, justified retroactively by claims about their 'unwillingness' or 'inability' that they have no standing to contest. The payer seat computes extraction because the beneficiary (intervening state) has defined the terms unilaterably and the host state cannot exit. From the weak states' seat (excluded), the constraint is structurally predatory—they are most vulnerable to being labeled 'unable' and most subject to intervention, with no say in the framing. The engine computes these divergent types from the structural data: beneficiary/victim declaration (asymmetry), active enforcement requirement (tangled_rope gate), and directionality derivation from power + exit options + spatial scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states (institutional power, global scope, arbitrage exit via ability to define doctrine terms) are full beneficiaries—d near 0.0. Host states (powerful formally, but regionally scoped and constrained exit—they cannot abandon their territory or unilaterally eject non-state actors) sit at moderate target position—d around 0.65. Weak states (powerless, national or local scope, trapped exit) are full targets—d near 1.0. The Security Council (institutional power, but bypassed in practice) sits near 0.4 (formally beneficiary of its advisory role, but structurally excluded from decision-making). The directionality reflects who controls the constraint's operation (intervening states set the terms) and who bears its costs (host states and weak states that cannot resist intervention claims).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was real—non-state actor threats originating from weak states that could not suppress them. That problem persists in the abstract, but the constraint's actual operation has drifted. Early doctrine invocations identified specific imminent threats and demonstrated host-state incapacity; current invocations often cite broad counterterrorism mandates without specific threat verification. The theater ratio rising from 0.28 to 0.42 reflects this drift: more enforcement activity is theatrical (maintaining the legal cover story) and less is directly threat-responsive. The constraint is NOT mandatrophy-resolved—the founding problem status is 'contested' precisely because intervening states continue to claim it is live while host states and non-aligned states argue the constraint persists by bureaucratic inertia and geopolitical advantage rather than solving an active problem. A genuine mandatrophy resolution would require the constraint to be abandoned or formally restricted, neither of which has occurred.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unable_vs_unwilling_ambiguity,
    'How do we distinguish between a host state that is genuinely unable to suppress a non-state actor threat and one that is unwilling but claiming inability?',
    'International verification regime examining military capacity (number and training of counterterrorism forces, surveillance capability, border control infrastructure), prior enforcement actions, and credible third-party assessment of willingness indicators (diplomatic communications, prior cooperation with intervening states on other issues). A state that cooperates on some threats but not others signals unwillingness rather than inability.',
    'If most interventions are justified by ''unwillingness'' rather than genuine inability, the doctrine becomes a pretext for bypassing state consent entirely. If both are conflated into ''unable/unwilling'' as a single standard, the doctrine licenses intervention against any host state that fails to meet intervening states'' counterterrorism standards, regardless of the host state''s actual capacity constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unable_vs_unwilling_ambiguity, empirical, 'Whether inability and unwillingness are separately verifiable or whether the doctrine conflates them to mask political choice as capability deficit.').

omega_variable(
    doctrine_vs_cover_story_boundary,
    'Does the unable/unwilling doctrine coordinate genuine collective security response to transnational terrorism, or does it license unilateral intervention that is motivated by geopolitical competition and justified retroactively by the doctrine?',
    'Pattern analysis of intervention justifications vs. outcomes: if interventions occur predominantly against states that are weak or geopolitical rivals (not against strong allies harboring non-state actors), and if the ''threat'' rationale is not deployed consistently across comparable cases, the doctrine is operating as cover story rather than coordination framework.',
    'If the doctrine is genuine coordination, interventions should be consistent across different dyads and proportional to threat severity. If it is cover story, interventions should cluster among weak states and geopolitical competitors of intervening states, with inconsistent threat framing. The classification would shift from tangled_rope (coordination + extraction) to snare (extraction disguised as coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_vs_cover_story_boundary, conceptual, 'Whether the doctrine''s stated coordination function is the actual structural reason it persists, or whether geopolitical advantage is the dominant driver and coordination is the narrative mask.').

omega_variable(
    sovereignty_conditionality_precedent,
    'Does accepting the unable/unwilling doctrine establish a precedent that state sovereignty is conditional on demonstrated capacity to suppress threats within one''s borders, and if so, what other capacity thresholds might be invoked as justification for intervention?',
    'Historical analysis of post-doctrine doctrine-derived interventions and the scope expansion of the ''threshold.'' If the doctrine expands from counterterrorism to humanitarian intervention, to suppressing internal armed groups that pose ''transnational risks,'' the precedent is being generalized. If weak states begin invoking ''our population is unable to suppress the refugee flow'' or ''unable to suppress smuggling networks'' as justification for cross-border intervention, the doctrine has been weaponized.',
    'If the conditionality precedent is established and generalized, state sovereignty becomes permanently provisional—dependent on meeting evolving capacity standards set by more powerful states. This would constitute a fundamental shift in the international legal order from Westphalian to conditional sovereignty.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_conditionality_precedent, conceptual, 'Whether the doctrine establishes a generalizable precedent that erodes classical state sovereignty as a categorical principle.').

omega_variable(
    reading_foreclosure_narrow_vs_unable_unwilling,
    'Does the unable/unwilling reading logically foreclose the narrow_armed_attack reading, or can both coexist within the same international legal framework?',
    'Textual and doctrinal analysis: the narrow reading asserts that Article 51 requires attribution to a state actor and imminent or actual armed attack by that state. The unable/unwilling reading permits response to non-state actor attacks from weak states. These readings can coexist if the narrow reading applies to state-on-state conflict and the unable/unwilling reading applies to non-state actor contexts. They foreclose each other only if one reading asserts it is the EXCLUSIVE interpretation of Article 51 for all cases.',
    'If the readings coexist, both constraints exist and apply to different factual scenarios—the narrow reading for conventional war, the unable/unwilling reading for counterterrorism. If one forecloses the other, the engine would reclassify the foreclosed reading as inert and mark the forecloser as the operative doctrine. The actual state of the law is coexistence with borderline cases being litigated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_narrow_vs_unable_unwilling, conceptual, 'Whether the unable/unwilling doctrine logically contradicts or coexists with the classical narrow reading of Article 51.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__unable_unwilling_doctrine_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t0, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t4, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 4, 0.32).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t8, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 8, 0.36).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t12, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 12, 0.39).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t16, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t20, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_tr_t24, article_51_self_defense__unable_unwilling_doctrine_reading, theater_ratio, 24, 0.42).

% Extraction over time
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t0, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t4, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t8, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 8, 0.6).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t12, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t16, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t20, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_be_t24, article_51_self_defense__unable_unwilling_doctrine_reading, base_extractiveness, 24, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t0, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t4, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 4, 0.62).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t8, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 8, 0.66).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t12, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t16, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t20, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(article_51_self_defense_unwilling_unable_su_t24, article_51_self_defense__unable_unwilling_doctrine_reading, suppression_requirement, 24, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__unable_unwilling_doctrine_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__unable_unwilling_doctrine_reading, 0.15).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__unable_unwilling_doctrine_reading, article_51_self_defense__expansive_preventive_reading).

% DUAL FORMULATION NOTE:
% The Article 51 self-defense kernel decomposes into three structurally distinct constraints corresponding to three contested readings: the narrow_armed_attack_reading (state-attributable armed attack only, ε ≈ 0.15, mountain-like); the unable_unwilling_doctrine_reading (non-state actor attacks with host-state incapacity trigger, ε ≈ 0.68, tangled_rope); and the expansive_preventive_reading (preemptive/preventive use of force, ε ≈ 0.80, snare). These are NOT three measurements of one constraint—they are three separate constraints instantiated by three readings of the same kernel. Each has different beneficiaries, victims, extractiveness profiles, and enforcement mechanisms. The narrow reading constrains (and benefits from constraining) the other two; the expansive reading maximizes state freedom of action and extracts by bypassing collective authorization; the unable/unwilling reading sits between, solving a genuine collective-action problem while permitting unilateral intervention that benefits powerful states disproportionately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__unable_unwilling_doctrine_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
