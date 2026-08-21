% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__extraction_cycle_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__extraction_cycle_reading, []).

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
 *   constraint_id: feud_obligation_kernel__extraction_cycle_reading
 *   human_readable: Blood-Feud Extraction Cycle
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story analyzes blood-feud obligations as a destructive
 *   extraction cycle, focusing on their negative impact on productive
 *   capacity and state formation. It is one reading of the
 *   'feud_obligation_kernel', emphasizing the costs borne by participants and
 *   the benefits accrued by emerging centralized authorities who offer an
 *   alternative. The high extractiveness and suppression reflect the coercive
 *   social norms that perpetuate the cycle and the lack of viable
 *   alternatives for those caught within it.
 *
 * KEY AGENTS:
 *   - feud_participants: Primary target (moderate/identity_locked) — bears direct costs, perpetuates cycle
 *   - kinship_groups: Agenda setter/Payer (organized/constrained) — enforce norms, bear collective costs
 *   - peasantry_farmers: Primary victim (powerless/trapped) — bears indirect costs, no exit
 *   - royal_authority: Primary beneficiary/Agenda setter (institutional/arbitrage) — benefits from feud's breakdown, offers alternative
 *   - ecclesiastical_authorities: Observer/Agenda setter (institutional/analytical) — advocate for peace, offer moral alternative
 *   - stateless_society_members: Excluded (organized/constrained) — their 'coordination' argument is excluded by this reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__extraction_cycle_reading, 0.78).
domain_priors:theater_ratio(feud_obligation_kernel__extraction_cycle_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__extraction_cycle_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__extraction_cycle_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__extraction_cycle_reading, "Blood-Feud Extraction Cycle").
narrative_ontology:topic_domain(feud_obligation_kernel__extraction_cycle_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__extraction_cycle_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__extraction_cycle_reading, 'c54bf3ee-8126-4b37-b958-b507383987e6').
narrative_ontology:cs_kernel_codification('c54bf3ee-8126-4b37-b958-b507383987e6', implicit).
narrative_ontology:cs_authority_grounding('c54bf3ee-8126-4b37-b958-b507383987e6', practice).
narrative_ontology:cs_interpretation_layer_present('c54bf3ee-8126-4b37-b958-b507383987e6').
narrative_ontology:cs_reading_relation('c54bf3ee-8126-4b37-b958-b507383987e6', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('c54bf3ee-8126-4b37-b958-b507383987e6', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('c54bf3ee-8126-4b37-b958-b507383987e6', foundational, feud_is_net_negative_sum).
narrative_ontology:cs_axiom_status(feud_is_net_negative_sum, holdable).
narrative_ontology:cs_axiom_grounding('c54bf3ee-8126-4b37-b958-b507383987e6', feud_is_net_negative_sum, empirically_contingent).
narrative_ontology:cs_axiom('c54bf3ee-8126-4b37-b958-b507383987e6', secondary, centralized_violence_monopoly_is_efficient).
narrative_ontology:cs_axiom_status(centralized_violence_monopoly_is_efficient, holdable).
narrative_ontology:cs_axiom_grounding('c54bf3ee-8126-4b37-b958-b507383987e6', centralized_violence_monopoly_is_efficient, empirically_contingent).
narrative_ontology:cs_reference_frame('c54bf3ee-8126-4b37-b958-b507383987e6', pre_state_violence_equilibrium).
narrative_ontology:cs_drift_state('c54bf3ee-8126-4b37-b958-b507383987e6', rise_of_centralized_states, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c54bf3ee-8126-4b37-b958-b507383987e6', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__extraction_cycle_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, peasantry_farmers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(feud_obligation_kernel__extraction_cycle_reading, kinship_groups).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and families directly involved in feuds, bound by honor and kinship to avenge wrongs. They bear the direct costs of violence (mortality, injury, property destruction) and resource depletion, but also perpetuate the cycle through their actions. Exit is difficult due to social pressure and identity.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, feud_participants, payer,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, feud_participants, agenda_setter).

% The collective units that enforce and perpetuate feud obligations through social norms, pressure, and collective action. They benefit from internal cohesion and perceived justice but also bear the collective costs of the feud cycle, including reduced productive capacity and vulnerability to external threats.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, kinship_groups, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, kinship_groups, payer).

% The broader population caught in the crossfire of feuds. They suffer indirect costs through disrupted agriculture, forced conscription, and general insecurity, with little power to resist or exit the affected territories. Their productive capacity is directly depleted.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, peasantry_farmers, payer,
    powerless, immediate, trapped, local).

% Emerging centralized power structures (kings, princes) that benefit from the breakdown of kinship-based justice. The destructive nature of feuds provides a strong justification for the state's monopoly on violence, allowing it to consolidate power, collect taxes, and impose its own legal system. They actively seek to suppress feuds.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, royal_authority, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, royal_authority, agenda_setter).

% Church institutions and clergy who observe the destructive impact of feuds and often advocate for peace, truces, and alternative forms of justice based on Christian doctrine. They seek to replace feud obligations with ecclesiastical law and moral injunctions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, observer,
    institutional, civilizational, analytical, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__extraction_cycle_reading, ecclesiastical_authorities, agenda_setter).

% Members of societies or factions who view blood-feud obligations as a legitimate and necessary form of self-enforcing justice and coordination in the absence of a state. From the perspective of this 'extraction cycle' reading, their arguments for the functional utility of feuds are dismissed or actively suppressed by emerging state powers.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__extraction_cycle_reading, stateless_society_members, excluded,
    organized, generational, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__extraction_cycle_reading, royal_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__extraction_cycle_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized authority, feuds provide a mechanism for perceived justice and deterrence against wrongs, ensuring that transgressions are met with a response, thereby maintaining a form of social order within kinship groups.
% TRANSFER_FUNCTION: Feud obligations transfer human lives, productive capacity, and material wealth from feuding kinship groups and the general populace to the cycle of vengeance itself, ultimately consolidating power and legitimacy for emerging royal authorities who offer an alternative.
% ABSENT_VOICES: Those who view feuds as a legitimate and self-enforcing coordination mechanism in stateless societies are excluded from the 'extraction cycle' framing, which emphasizes their destructive economic and political consequences. Their perspective would highlight the social functions of feuds that are lost with state pacification.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight, the social fabric of many historical and anthropological societies would fundamentally reorganize. Kinship-based justice systems would collapse, leading to either anarchy or a rapid acceleration of centralized state authority to fill the vacuum. Productive capacity would increase, and territorial consolidation would become easier.
% FOUNDING_PROBLEM: The absence of a centralized, legitimate authority to adjudicate disputes and enforce justice, leading to a need for kinship groups to self-regulate and deter transgressions.
% FOUNDING_PROBLEM_CORROBORATION: While kinship groups might still claim the problem is live, historical and anthropological analyses from outside these groups (e.g., historians studying state formation, comparative political scientists) corroborate that the problem of stateless justice has largely been superseded by the rise of centralized states, rendering the original 'solution' obsolete and destructive.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__extraction_cycle_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__extraction_cycle_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__extraction_cycle_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__extraction_cycle_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__extraction_cycle_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__extraction_cycle_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__extraction_cycle_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the feud cycle continuously depletes human and material resources without generating equivalent value, leading to a net loss for participants and society. Suppression (0.78) is strong due to deeply ingrained social norms, honor codes, and the lack of external enforcement mechanisms, which trap individuals and groups in the cycle. Theater ratio is low (0.10) because the obligations are genuinely enacted and have real, destructive consequences; there is little performative maintenance without functional impact. Accessibility collapse is high (0.70) as alternatives to feud (e.g., state courts, peaceful arbitration) are often unavailable or lack legitimacy in the absence of strong centralized power. Resistance (0.45) is moderate; while individuals may seek to avoid feuds, the social pressure to participate is significant.
 *
 * PERSPECTIVAL GAP:
 *   The 'extraction_cycle_reading' emphasizes the destructive economic and political consequences of feuds, positioning participants as victims and emerging states as beneficiaries. This contrasts sharply with the 'stateless_coordination_reading' which views feuds as a functional, self-enforcing mechanism for justice. The engine's per-seat classification will highlight how feud participants experience this as a snare, while royal authority views it as a problem to be 'solved' for its own benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants and peasantry farmers are clear targets (high d) as they bear the direct and indirect costs of violence and resource depletion. Kinship groups, while perpetuating the cycle, also bear significant costs, placing them closer to the target end. Royal authority is the primary beneficiary (low d) as the destructive nature of feuds legitimizes its claim to a monopoly on violence and enables state-building. Ecclesiastical authorities are observers, seeking to shift the system rather than directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a Snare prevents mislabeling the feud as a 'Rope' (pure coordination) or 'Piton' (inertial decay). The high extractiveness and active social enforcement demonstrate it is not a benign coordination mechanism, nor is it merely a vestigial practice. The 'dead' status of the founding problem (lack of centralized justice) combined with the 'world_rearranges' disappearance verdict signals a zombie-like persistence, where the original function is gone but the destructive cycle continues, benefiting new actors (royal authority) by providing a problem for them to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'This constraint is the ''extraction_cycle_reading'' of the ''feud_obligation_kernel''. What structural elements would change if a sibling reading were adopted?',
    'Analysis of alternative historical or anthropological framings of blood feuds.',
    'If the ''stateless_coordination_reading'' were adopted, feud participants would shift from victims to beneficiaries of a functional system, and extractiveness would be re-evaluated as coordination cost. If the ''christianized_pacification_reading'' were adopted, the primary axis of evaluation would shift from economic/political to moral/theological.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Identifies this constraint as one reading of a contested kernel and outlines structural deltas for sibling readings.').

omega_variable(
    feud_functional_utility_ambiguity,
    'To what extent did blood feuds genuinely provide a functional coordination mechanism for justice and deterrence in stateless societies, rather than being purely extractive?',
    'Comparative anthropological studies of societies with and without feuds, assessing long-term stability, dispute resolution efficacy, and resource allocation outcomes.',
    'If significant functional utility is established, the base extractiveness for feud participants would be lower, reflecting a genuine coordination cost rather than pure extraction, potentially shifting the classification closer to a Tangled Rope or even a Rope (from the perspective of stateless societies).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_functional_utility_ambiguity, empirical, 'Ambiguity regarding the functional utility of feuds in their original context.').

omega_variable(
    internalized_suppression_of_exit,
    'Is the high suppression primarily structural (lack of external alternatives) or internalized (identity-locked by honor codes and kinship obligations)?',
    'Sociological studies examining post-pacification communities: if feud-like behaviors or social ostracism persist after state enforcement, it indicates internalized suppression.',
    'If internalized, the effective suppression for feud participants is higher than the structural measure suggests, as the ''trap'' is carried within their identity and social fabric, making exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_suppression_of_exit, empirical, 'Structural vs. internalized suppression mechanism for feud participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__extraction_cycle_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 20, 0.13).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__extraction_cycle_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 40, 0.81).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 60, 0.83).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__extraction_cycle_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 80, 0.77).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__extraction_cycle_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__extraction_cycle_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, territorial_consolidation_constraint).
narrative_ontology:affects_constraint(feud_obligation_kernel__extraction_cycle_reading, state_monopoly_on_violence_legitimacy).

% DUAL FORMULATION NOTE:
% This constraint is the 'extraction_cycle_reading' of the 'feud_obligation_kernel'. It focuses on the destructive economic and political consequences of feuds, contrasting with the 'stateless_coordination_reading' (which emphasizes functional utility) and the 'christianized_pacification_reading' (which emphasizes moral/theological objections).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
