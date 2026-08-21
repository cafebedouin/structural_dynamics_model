% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UNSC Article 27 Veto Power (Coordination Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coordination reading' of the UN
 *   Security Council's Article 27 veto power. Under this reading, the veto is
 *   understood as a necessary institutional mechanism designed to prevent
 *   great-power military confrontation, particularly between nuclear states.
 *   It functions by ensuring that no Security Council resolution can compel a
 *   permanent member into military action it fundamentally rejects, thereby
 *   safeguarding the UN's foundational premise of collective security by
 *   avoiding its collapse due to internal conflict among its most powerful
 *   members. The low extractiveness and suppression reflect its framing as a
 *   beneficial, stabilizing force, rather than a tool of oppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.15).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.2).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UNSC Article 27 Veto Power (Coordination Reading)").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '55cf3fd8-5965-45c6-a58e-91d83ddcd72c').
narrative_ontology:cs_kernel_codification('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', fixed_text).
narrative_ontology:cs_authority_grounding('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', lineage).
narrative_ontology:cs_interpretation_layer_present('55cf3fd8-5965-45c6-a58e-91d83ddcd72c').
narrative_ontology:cs_reading_relation('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', foundational, great_power_unanimity_prevents_war).
narrative_ontology:cs_axiom_status(great_power_unanimity_prevents_war, holdable).
narrative_ontology:cs_axiom_grounding('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', great_power_unanimity_prevents_war, deontological).
narrative_ontology:cs_axiom('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', secondary, nuclear_deterrence_requires_veto_safeguard).
narrative_ontology:cs_axiom_status(nuclear_deterrence_requires_veto_safeguard, holdable).
narrative_ontology:cs_axiom_grounding('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', nuclear_deterrence_requires_veto_safeguard, empirically_contingent).
narrative_ontology:cs_reference_frame('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', post_wwii_collective_security_framework).
narrative_ontology:cs_drift_state('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55cf3fd8-5965-45c6-a58e-91d83ddcd72c', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, all_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__coordination_reading, non_p5_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As permanent members of the UN Security Council, they possess the veto power, which they view as a necessary safeguard for their national interests and a mechanism to prevent direct military confrontation between nuclear states. They benefit from the stability this mechanism provides, even if it means diplomatic deadlock.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_members, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the overall international system stability and the prevention of great-power wars, which the veto is argued to ensure. However, they also 'pay' by accepting the paralysis of the Security Council on issues where P5 interests diverge, leading to inaction on some crises.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_member_states, beneficiary,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, non_p5_member_states, payer).

% The abstract concept of global peace and security, which is the primary beneficiary of the veto power under this reading, as it prevents conflicts that could escalate to catastrophic levels.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Administers the UN system and observes the operation of the veto. While often frustrated by its use, they recognize its foundational role in maintaining the UN's structure and preventing its collapse due to great-power conflict.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, un_secretariat, observer,
    institutional, generational, constrained, global).

% Advocates for a more effective and democratic UN, often criticizing the veto as an anachronism that hinders action on humanitarian crises and human rights. They are excluded from the direct decision-making process of the Security Council but exert pressure through public opinion and advocacy.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, global_civil_society, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents direct military confrontation between nuclear-armed states by ensuring no Security Council resolution can compel a permanent member into military action it rejects, thereby maintaining a fragile great-power consensus necessary for the UN's existence.
% TRANSFER_FUNCTION: Transfers the risk of catastrophic great-power war into the cost of diplomatic deadlock and inaction on issues where P5 interests conflict, effectively trading decisive action for systemic stability.
% ABSENT_VOICES: Global civil society, smaller states, and proponents of a more democratic and effective UN Security Council would argue the veto is an anachronism that paralyzes action on humanitarian crises and undermines collective security. They are excluded from the P5 decision-making process.
% DISAPPEARANCE_RATIONALE: If the veto power vanished overnight, the UN Security Council could theoretically mandate military action against a P5 member, risking direct military confrontation between nuclear powers. This would fundamentally alter the global security architecture, likely leading to the collapse of the UN as a collective security body and a return to a more volatile, multipolar system.
% FOUNDING_PROBLEM: Preventing a third world war, particularly among nuclear-armed great powers, and ensuring that these powers had a vested interest in the UN's continued existence rather than opting out or undermining it, by guaranteeing their vital interests would not be overridden.
% FOUNDING_PROBLEM_CORROBORATION: Realist international relations scholars, historical analyses of Cold War dynamics, and statements from P5 members themselves consistently corroborate that the risk of great-power conflict remains a live concern, justifying the veto's continued function as a safeguard. Independent analyses often acknowledge the veto's role in preventing UN collapse, even while critiquing its other effects.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects the view that the 'cost' of the veto (UN inaction) is a necessary trade-off for the 'benefit' of preventing great-power war, making the net extraction minimal from a systemic stability perspective. Suppression (0.20) is low because while it suppresses certain UN actions, this is seen as preventing greater suppression (war) and is accepted as a structural feature. The theater ratio (0.05) is minimal, as the veto is a functional, actively used mechanism, not a performative relic. Accessibility collapse (0.70) is high because the option of UN-mandated military action against a P5 member is indeed largely foreclosed, which is considered a feature for stability. Resistance (0.10) is low because, despite frequent criticism, the veto's fundamental role in preventing great-power war is widely acknowledged within realist international relations theory and by the P5 themselves.
 *
 * PERSPECTIVAL GAP:
 *   This 'coordination reading' stands in contrast to other interpretations, such as the 'oligopoly reading' (which views the veto as entrenching P5 power and extracting rents) or the 'sovereignty reading' (which frames it as a manifestation of Westphalian sovereignty). The engine's per-seat classification would highlight how P5 members experience this as a pure Rope (high benefit, low cost), while other seats might experience the inaction as a cost, even if they acknowledge the systemic benefit. The divergence is in the interpretation of the 'cost' of inaction versus the 'benefit' of avoided war.
 *
 * DIRECTIONALITY LOGIC:
 *   From this coordination reading, all member states are beneficiaries of the veto's function in preventing catastrophic war, hence the low directionality for 'all_member_states'. P5 members are also direct beneficiaries as it protects their vital interests and ensures their continued participation in the UN. 'International system stability' is an abstract beneficiary. There are no direct 'victims' in this reading, as the 'cost' of inaction is framed as a necessary systemic trade-off for a greater good. Non-P5 member states are listed as secondary 'payers' to acknowledge the cost of inaction, but their primary role is still beneficiary of stability.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_oligopoly_function,
    'Is the P5 veto primarily a coordination mechanism for international system stability, or does it function more as a structural entrenchment of geopolitical oligopoly power?',
    'Empirical analysis of veto usage patterns: if vetoes consistently block actions that would redistribute power or challenge P5 interests, even when not directly threatening great-power war, it supports the oligopoly reading. If vetoes are primarily used to prevent direct military intervention against P5 interests, it supports the coordination reading.',
    'If resolved towards oligopoly, the constraint''s effective extractiveness and suppression would be significantly higher, reclassifying it towards a Tangled Rope or Snare. If resolved towards coordination, the current Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_oligopoly_function, conceptual, 'Ambiguity between the veto''s role in coordination vs. power entrenchment.').

omega_variable(
    necessity_vs_obsolescence,
    'Is the P5 veto still a necessary mechanism for preventing great-power war in the contemporary international system, or has it become an obsolete relic that paralyzes effective UN action?',
    'Counterfactual analysis and expert consensus: if a credible alternative mechanism for managing great-power conflict without the veto could be demonstrated, or if the veto''s use consistently leads to worse outcomes than its absence, it would support obsolescence. If the risk of great-power war remains high and the veto is seen as the only viable circuit-breaker, its necessity is affirmed.',
    'If resolved towards obsolescence, the constraint''s theater_ratio would increase, and its coordination function would be seen as atrophied, pushing it towards a Piton or even Snare if its persistence is purely extractive. If resolved towards necessity, the current Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_obsolescence, empirical, 'Whether the veto''s original function remains relevant and effective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__coordination_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__coordination_reading, theater_ratio, 1965, 0.05).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__coordination_reading, theater_ratio, 1985, 0.05).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__coordination_reading, theater_ratio, 2005, 0.05).
narrative_ontology:measurement(arti_tr_t2024, article_27_veto_power__coordination_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__coordination_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__coordination_reading, base_extractiveness, 1965, 0.15).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__coordination_reading, base_extractiveness, 1985, 0.15).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__coordination_reading, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(arti_be_t2024, article_27_veto_power__coordination_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__coordination_reading, suppression_requirement, 1945, 0.2).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__coordination_reading, suppression_requirement, 1965, 0.2).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__coordination_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__coordination_reading, suppression_requirement, 2005, 0.2).
narrative_ontology:measurement(arti_su_t2024, article_27_veto_power__coordination_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UN Security Council's Article 27 veto power. Other readings, such as the 'oligopoly_reading' and 'sovereignty_reading', offer alternative structural interpretations of the same mechanism, with different ε values and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
