% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__countervailing_thinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__countervailing_thinkable, []).

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
 *   constraint_id: war_winnability_post_1945__countervailing_thinkable
 *   human_readable: Countervailing Strategy: Limited Nuclear Victory Thinkable
 *   domain: strategic_studies/nuclear_deterrence_theory/international_relations
 *
 * SUMMARY:
 *   This constraint describes the strategic doctrine that, even after the
 *   advent of nuclear weapons, limited victory in a nuclear exchange remains
 *   achievable through precise counterforce targeting. This reading maintains
 *   the operational relevance of strategic nuclear forces and planning for
 *   'winnable' scenarios, despite the catastrophic risks. It is a reading of
 *   the broader 'war_winnability_post_1945' kernel, specifically the
 *   'countervailing_thinkable' interpretation, which contrasts with
 *   'deterrence_unthinkable' and 'rhetorical_contraction'.
 *
 * KEY AGENTS:
 *   - military_industrial_complex: Primary beneficiary (institutional/arbitrage)
 *   - strategic_planners: Agenda setter (institutional/constrained)
 *   - arms_control_regimes: Payer (organized/trapped)
 *   - global_stability_advocates: Payer (moderate/constrained)
 *   - political_leaders: Agenda setter (powerful/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, 0.65).
domain_priors:suppression_score(war_winnability_post_1945__countervailing_thinkable, 0.7).
domain_priors:theater_ratio(war_winnability_post_1945__countervailing_thinkable, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, extractiveness, 0.65).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_winnability_post_1945__countervailing_thinkable, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__countervailing_thinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__countervailing_thinkable, "Countervailing Strategy: Limited Nuclear Victory Thinkable").
narrative_ontology:topic_domain(war_winnability_post_1945__countervailing_thinkable, "strategic_studies/nuclear_deterrence_theory/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__countervailing_thinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__countervailing_thinkable, '080fa5bf-6515-403b-9f09-d9629c0b4634').
narrative_ontology:cs_kernel_codification('080fa5bf-6515-403b-9f09-d9629c0b4634', formalized).
narrative_ontology:cs_authority_grounding('080fa5bf-6515-403b-9f09-d9629c0b4634', lineage).
narrative_ontology:cs_interpretation_layer_present('080fa5bf-6515-403b-9f09-d9629c0b4634').
narrative_ontology:cs_reading_relation('080fa5bf-6515-403b-9f09-d9629c0b4634', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('080fa5bf-6515-403b-9f09-d9629c0b4634', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('080fa5bf-6515-403b-9f09-d9629c0b4634', foundational, limited_nuclear_war_is_controllable).
narrative_ontology:cs_axiom_status(limited_nuclear_war_is_controllable, holdable).
narrative_ontology:cs_axiom_grounding('080fa5bf-6515-403b-9f09-d9629c0b4634', limited_nuclear_war_is_controllable, empirically_contingent).
narrative_ontology:cs_axiom('080fa5bf-6515-403b-9f09-d9629c0b4634', foundational, counterforce_targeting_is_effective).
narrative_ontology:cs_axiom_status(counterforce_targeting_is_effective, holdable).
narrative_ontology:cs_axiom_grounding('080fa5bf-6515-403b-9f09-d9629c0b4634', counterforce_targeting_is_effective, empirically_contingent).
narrative_ontology:cs_reference_frame('080fa5bf-6515-403b-9f09-d9629c0b4634', flexible_response_doctrine).
narrative_ontology:cs_drift_state('080fa5bf-6515-403b-9f09-d9629c0b4634', contemporary_strategic_environment, gap(stable, minor, true)).
narrative_ontology:cs_created_at('080fa5bf-6515-403b-9f09-d9629c0b4634', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__countervailing_thinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__countervailing_thinkable, strategic_planners).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes).
narrative_ontology:constraint_victim(war_winnability_post_1945__countervailing_thinkable, global_stability_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the continued justification for developing and maintaining advanced nuclear weapons systems, as 'winnable' scenarios require continuous modernization and diversification of arsenals. Mission continuity and funding are directly tied to the 'thinkable' nature of limited nuclear war.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, military_industrial_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Responsible for developing and refining nuclear war plans, including counterforce targeting and escalation control. This reading provides a framework for their continued professional relevance and the intellectual justification for their work, even if the scenarios are highly theoretical.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, strategic_planners, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the cost of undermined legitimacy and reduced effectiveness. The 'winnable war' narrative creates incentives for arms races and makes verifiable disarmament more difficult, as states seek to maintain or gain advantages in a perceived 'thinkable' conflict.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, arms_control_regimes, payer,
    organized, generational, trapped, global).

% Experience increased existential risk and a more precarious international environment. Their efforts to promote de-escalation, non-proliferation, and peace are directly challenged by the persistence of 'winnable' nuclear war doctrines.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, global_stability_advocates, payer,
    moderate, generational, constrained, global).

% Must balance the perceived need for a credible deterrent (which this reading supports) with the immense risks of nuclear conflict. They authorize strategic doctrines and funding, often relying on the advice of strategic planners.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__countervailing_thinkable, political_leaders, agenda_setter,
    powerful, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the strategic planning and resource allocation for maintaining a credible nuclear deterrent, ensuring that military forces are prepared for a range of conflict scenarios, including limited nuclear exchanges.
% TRANSFER_FUNCTION: Transfers resources (funding, intellectual capital, political will) from arms control and disarmament efforts towards nuclear weapons development, maintenance, and strategic planning for 'winnable' scenarios.
% ABSENT_VOICES: Populations directly threatened by nuclear conflict, future generations, and those who believe nuclear war is categorically unwinnable are largely excluded from the strategic planning discourse, which is dominated by military and political elites. They would argue for a complete rejection of winnability narratives.
% DISAPPEARANCE_RATIONALE: If the belief in limited nuclear victory vanished overnight, strategic doctrines would undergo a radical shift, potentially leading to significant reductions in nuclear arsenals, a reorientation of military-industrial priorities, and a fundamental change in international security architecture. The world would reorganize around a more absolute understanding of nuclear deterrence.
% FOUNDING_PROBLEM: The need to maintain a credible deterrent against a peer adversary while avoiding mutually assured destruction (MAD) by developing options for limited nuclear use.
% FOUNDING_PROBLEM_CORROBORATION: Strategic planners and defense establishments continue to attest to the live status of this problem, citing ongoing geopolitical rivalries and the need for flexible response options. Critics, including some former officials and academics, contest the 'live' status, arguing that the problem is a self-perpetuating construct of the military-industrial complex.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__countervailing_thinkable, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__countervailing_thinkable, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__countervailing_thinkable, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__countervailing_thinkable, 'none', 1).
narrative_ontology:epsilon_provenance(war_winnability_post_1945__countervailing_thinkable, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__countervailing_thinkable_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__countervailing_thinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__countervailing_thinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the diversion of vast resources to maintain and modernize nuclear arsenals for these 'winnable' scenarios, often at the expense of other security priorities or social programs. Suppression (0.70) is high due to the institutional inertia and secrecy surrounding nuclear planning, which limits public and political debate on the feasibility and ethics of such doctrines. The theater ratio (0.20) is relatively low, as the planning and hardware are genuinely intended for potential use, even if the scenarios are highly theoretical. The claimed type is 'tangled_rope' because it coordinates strategic stability (deterrence) while extracting resources and legitimacy from arms control efforts through the same structure.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners and the military-industrial complex perceive this as a necessary coordination mechanism for national security, ensuring a credible deterrent and flexible response options. Arms control advocates and global stability advocates, however, experience it as a highly extractive and dangerous constraint that perpetuates an arms race and increases the risk of catastrophic conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   The military-industrial complex and strategic planners are clear beneficiaries, as the doctrine ensures their mission continuity and funding. Arms control regimes and global stability advocates are victims, as their efforts are undermined by the 'winnable war' narrative. Political leaders are agenda-setters who navigate these competing pressures.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring the coordination function of deterrence). It highlights that while it serves a genuine, albeit contested, coordination function (deterrence through flexible response), it simultaneously extracts significant resources and suppresses alternative approaches to security, such as disarmament. The persistence of 'winnability' planning, despite its high costs and risks, suggests a complex interplay of coordination and extraction, rather than simple inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_winnability_of_limited_nuclear_war,
    'Is a ''limited nuclear victory'' empirically achievable without escalating to full-scale mutual assured destruction?',
    'Extensive wargaming, advanced simulation, and declassified historical analysis of crisis scenarios, though definitive empirical resolution is impossible without actual conflict.',
    'If empirically shown to be impossible, the ''countervailing_thinkable'' reading would collapse, forcing a reclassification towards ''deterrence_unthinkable'' and significantly reducing extractiveness and suppression. If a credible pathway were demonstrated, it would reinforce the current classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(empirical_winnability_of_limited_nuclear_war, empirical, 'The empirical feasibility of limited nuclear victory.').

omega_variable(
    legitimacy_of_counterforce_targeting,
    'Is the concept of ''counterforce targeting'' a legitimate strategic option, or is it a dangerous illusion that increases the risk of nuclear war?',
    'A shift in international norms and legal frameworks regarding nuclear use, or a consensus among strategic theorists and political leaders that such targeting is inherently destabilizing.',
    'If deemed illegitimate, the justification for maintaining large, precise nuclear arsenals would erode, leading to reduced extractiveness and suppression. If reaffirmed, it would maintain the current resource allocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_counterforce_targeting, conceptual, 'The normative and strategic legitimacy of counterforce targeting.').

omega_variable(
    military_industrial_complex_influence,
    'To what extent does the military-industrial complex''s interest in mission continuity and funding drive the persistence of ''winnable war'' doctrines, independent of genuine strategic necessity?',
    'Independent audits of defense spending, analysis of lobbying efforts, and historical studies of doctrine evolution in relation to technological development and industrial interests.',
    'Strong evidence of undue influence would shift the classification closer to a Snare, highlighting the purely extractive nature of the constraint''s persistence, even if a coordination function is claimed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(military_industrial_complex_influence, empirical, 'Influence of the military-industrial complex on strategic doctrine.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__countervailing_thinkable, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(war__tr_t1975, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(war__tr_t1990, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(war__tr_t2005, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__countervailing_thinkable, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(war__be_t1975, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(war__be_t1990, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(war__be_t2005, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__countervailing_thinkable, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(war__su_t1975, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(war__su_t1990, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(war__su_t2005, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__countervailing_thinkable, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__countervailing_thinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, nuclear_arms_race_dynamics).
narrative_ontology:affects_constraint(war_winnability_post_1945__countervailing_thinkable, arms_control_treaty_negotiations).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'war_winnability_post_1945' kernel. Its siblings are 'deterrence_unthinkable' and 'rhetorical_contraction', each representing a distinct structural claim about nuclear war winnability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
