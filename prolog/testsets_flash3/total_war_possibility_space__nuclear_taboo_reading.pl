% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo on Total War
 *   domain: international_relations/strategic_studies/institutional_history
 *
 * SUMMARY:
 *   This constraint represents the 'nuclear taboo' reading of the total war
 *   possibility space, where total war remains materially possible but is
 *   normatively prohibited through a constructed taboo. This taboo is
 *   maintained by active enforcement of non-proliferation norms and
 *   diplomatic pressure, rather than purely by material deterrence. The
 *   constraint is claimed as a Rope, as it primarily serves a coordination
 *   function (preventing catastrophic war) with broad benefits, though it
 *   requires active maintenance and suppression of alternative strategic
 *   rationales. The metrics reflect a low but present extractiveness (costs
 *   of self-restraint, maintaining non-proliferation), significant
 *   suppression of alternative strategic options, and a moderate theater
 *   ratio (diplomatic performances reinforcing the norm).
 *
 * KEY AGENTS:
 *   - global_population: Primary beneficiary (powerless/trapped)
 *   - nuclear_weapons_states: Agenda-setter (institutional/constrained)
 *   - non_nuclear_states: Beneficiary (moderate/constrained)
 *   - non_proliferation_regime_institutions: Agenda-setter (organized/constrained)
 *   - norm_entrepreneurs: Agenda-setter (moderate/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.15).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.7).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo on Total War").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies/institutional_history").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '3f9230b8-fbd2-455c-87a5-a80207dc2a02').
narrative_ontology:cs_kernel_codification('3f9230b8-fbd2-455c-87a5-a80207dc2a02', distributed).
narrative_ontology:cs_authority_grounding('3f9230b8-fbd2-455c-87a5-a80207dc2a02', practice).
narrative_ontology:cs_interpretation_layer_present('3f9230b8-fbd2-455c-87a5-a80207dc2a02').
narrative_ontology:cs_reading_relation('3f9230b8-fbd2-455c-87a5-a80207dc2a02', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f9230b8-fbd2-455c-87a5-a80207dc2a02', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('3f9230b8-fbd2-455c-87a5-a80207dc2a02', foundational, nuclear_weapons_are_morally_unacceptable).
narrative_ontology:cs_axiom_status(nuclear_weapons_are_morally_unacceptable, holdable).
narrative_ontology:cs_axiom_grounding('3f9230b8-fbd2-455c-87a5-a80207dc2a02', nuclear_weapons_are_morally_unacceptable, deontological).
narrative_ontology:cs_axiom('3f9230b8-fbd2-455c-87a5-a80207dc2a02', foundational, total_war_is_normatively_prohibited).
narrative_ontology:cs_axiom_status(total_war_is_normatively_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('3f9230b8-fbd2-455c-87a5-a80207dc2a02', total_war_is_normatively_prohibited, conventional).
narrative_ontology:cs_reference_frame('3f9230b8-fbd2-455c-87a5-a80207dc2a02', post_hiroshima_normative_consensus).
narrative_ontology:cs_drift_state('3f9230b8-fbd2-455c-87a5-a80207dc2a02', contemporary_geopolitical_tensions, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('3f9230b8-fbd2-455c-87a5-a80207dc2a02', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, global_population).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the absence of total war, which would entail catastrophic loss of life and societal collapse. Has no direct agency in maintaining the taboo but is the ultimate beneficiary of its persistence.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, global_population, beneficiary,
    powerless, generational, trapped, global).

% Are the primary actors in upholding the taboo through policy declarations (e.g., no-first-use pledges), diplomatic pressure, and non-proliferation efforts. They bear the cost of self-restraint but gain legitimacy and stability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapons_states, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the reduced risk of total war, which would disproportionately affect them. They participate in non-proliferation efforts and reinforce the taboo through international norms and treaties.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_states, beneficiary,
    moderate, biographical, constrained, global).

% Are the formal and informal organizations (e.g., IAEA, NPT signatories) that codify, monitor, and enforce the norms against nuclear proliferation and the use of nuclear weapons. Their existence and influence are directly tied to the taboo's strength.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_regime_institutions, agenda_setter,
    organized, generational, constrained, global).

% Are individuals and groups (e.g., disarmament activists, academics) who actively promote and reinforce the nuclear taboo through advocacy, research, and public education. Their continued effort is crucial for the taboo's normative strength.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, agenda_setter,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global state behavior by establishing a shared normative prohibition against the use of nuclear weapons and the escalation to total war, preventing a race to the bottom in strategic thinking.
% TRANSFER_FUNCTION: Transfers a sense of security and stability to the global population by reducing the perceived likelihood of total war. It also transfers legitimacy and influence to states and institutions that uphold the taboo.
% ABSENT_VOICES: States or non-state actors who might consider nuclear weapons as a legitimate tool for national survival or power projection, but are marginalized or sanctioned by the international community. Their voices are suppressed by the normative consensus.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, the strategic landscape would fundamentally shift. The perceived utility of nuclear weapons would increase, leading to heightened proliferation risks, a breakdown of arms control, and a much higher probability of conventional conflicts escalating to total war. Global security architectures would collapse.
% FOUNDING_PROBLEM: The existential threat posed by nuclear weapons after their initial use in WWII, which demonstrated the catastrophic potential of total war in the nuclear age.
% FOUNDING_PROBLEM_CORROBORATION: Historians, international relations scholars, and peace activists widely corroborate the founding problem, citing the immediate post-WWII efforts to control nuclear technology and the subsequent development of arms control treaties. The ongoing threat of nuclear proliferation and the destructive power of modern arsenals attest to the problem's continued relevance.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).
:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because the primary 'cost' is self-restraint by nuclear powers, which is outweighed by the benefit of avoiding total war. Suppression is high because the taboo actively discourages and penalizes any rhetoric or action that normalizes nuclear use or total war. The theater ratio is moderate, reflecting the necessary diplomatic performances and symbolic gestures that reinforce the norm, alongside genuine efforts. Accessibility collapse is high because the taboo makes the option of total war almost unthinkable for most state actors, despite its material possibility. Resistance is low because the taboo is widely accepted as a necessary evil.
 *
 * PERSPECTIVAL GAP:
 *   Nuclear weapons states might perceive the constraint as a necessary but costly burden on their sovereignty, while non-nuclear states and the global population see it as a vital safeguard. Norm entrepreneurs view it as a fragile achievement requiring constant vigilance. The engine's per-seat classification will reflect these differences in directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   The global population and non-nuclear states are clear beneficiaries (d near 0.0) as they gain security without direct costs of enforcement. Nuclear weapons states and non-proliferation institutions are agenda-setters (d near 0.5, slightly beneficiary) as they bear the costs of maintaining the taboo but also gain legitimacy and stability. Norm entrepreneurs are also agenda-setters, with a slightly higher d due to the active effort required to maintain the norm.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the nuclear taboo as pure extraction (Snare) by recognizing its genuine coordination function in preventing global catastrophe. It also avoids mislabeling it as a pure Mountain, acknowledging that it is a constructed norm requiring active maintenance and enforcement, not a natural law. The 'live' status of the founding problem (existential threat of nuclear war) indicates that the mandate has not atrophied, though its persistence is not guaranteed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_strength_measurement,
    'How can the ''strength'' of the nuclear taboo be empirically measured, independent of observed non-use?',
    'Analysis of state rhetoric, military doctrine, public opinion surveys, and diplomatic responses to nuclear threats or tests. A weakening of these indicators would signal a decline in taboo strength.',
    'If the taboo is found to be weaker than assumed, the constraint''s effective suppression and accessibility collapse would be lower, potentially reclassifying it towards a Tangled Rope or even Snare if underlying extractive dynamics (e.g., security dilemma) become dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_strength_measurement, empirical, 'Empirical measurement of normative strength vs. mere non-use.').

omega_variable(
    taboo_vs_deterrence_causality,
    'What is the causal weight of the nuclear taboo versus material deterrence in preventing total war?',
    'Counterfactual historical analysis, comparative case studies of states with varying nuclear doctrines and normative commitments, and expert elicitation. If deterrence is found to be the dominant factor, this reading''s distinctiveness from the ''deterrence_equilibrium_reading'' would diminish.',
    'If deterrence is the primary driver, this constraint''s classification might shift towards a Mountain (if deterrence is seen as an unchangeable strategic reality) or a different type of Rope (if deterrence is a coordination problem). If the taboo is primary, the Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(taboo_vs_deterrence_causality, conceptual, 'Disentangling normative prohibition from material deterrence.').

omega_variable(
    norm_entrepreneur_exit_impact,
    'How would the exit or decline of norm entrepreneurs impact the persistence and strength of the nuclear taboo?',
    'Longitudinal studies of disarmament movements, analysis of shifts in international advocacy funding, and expert projections on the future of non-proliferation activism. A significant decline would test the taboo''s self-sustaining capacity.',
    'If the taboo''s persistence is highly dependent on active norm entrepreneurship, their decline would lead to a decrease in suppression and an increase in resistance to the taboo, potentially shifting the constraint towards a Piton (if it persists by inertia) or a Snare (if it becomes a tool for nuclear powers to maintain their advantage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(norm_entrepreneur_exit_impact, empirical, 'Impact of norm entrepreneur activity on taboo maintenance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(tota_tr_t1980, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 1980, 0.2).
narrative_ontology:measurement(tota_tr_t2000, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(tota_tr_t2024, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1945, 0.05).
narrative_ontology:measurement(tota_be_t1960, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement(tota_be_t1980, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 1980, 0.12).
narrative_ontology:measurement(tota_be_t2000, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2000, 0.15).
narrative_ontology:measurement(tota_be_t2024, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(tota_su_t1960, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(tota_su_t1980, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(tota_su_t2000, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(tota_su_t2024, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, identity_coordination).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_treaty_regime).
narrative_ontology:affects_constraint(total_war_possibility_space__nuclear_taboo_reading, arms_control_agreements).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'total_war_possibility_space' kernel. This 'nuclear_taboo_reading' emphasizes normative prohibition, distinct from the 'deterrence_equilibrium_reading' (material deterrence) and 'space_contraction_reading' (strategic impossibility).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
