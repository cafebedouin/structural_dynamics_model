% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nws_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT Treaty Text: NWS Reading (Non-Proliferation as Binding, Disarmament as Aspirational)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the Nuclear Weapon States' (NWS) reading of
 *   the Non-Proliferation Treaty (NPT) text. In this reading,
 *   non-proliferation is a binding, enforceable obligation for Non-Nuclear
 *   Weapon States (NNWS), while disarmament (Article VI) is interpreted as an
 *   aspirational, long-term goal without concrete enforcement mechanisms or
 *   timelines. This interpretation allows NWS to maintain their arsenals
 *   while benefiting from the non-proliferation regime. This is one reading
 *   of the 'npt_treaty_text' kernel; sibling readings include 'nnws_reading'
 *   (disarmament as binding) and 'withdrawal_threshold_reading' (Article X
 *   interpretation).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.78).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.85).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT Treaty Text: NWS Reading (Non-Proliferation as Binding, Disarmament as Aspirational)").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '7d181b90-e9dc-417a-be11-e412d4d217e9').
narrative_ontology:cs_kernel_codification('7d181b90-e9dc-417a-be11-e412d4d217e9', fixed_text).
narrative_ontology:cs_authority_grounding('7d181b90-e9dc-417a-be11-e412d4d217e9', extraction).
narrative_ontology:cs_interpretation_layer_present('7d181b90-e9dc-417a-be11-e412d4d217e9').
narrative_ontology:cs_reading_relation('7d181b90-e9dc-417a-be11-e412d4d217e9', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d181b90-e9dc-417a-be11-e412d4d217e9', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('7d181b90-e9dc-417a-be11-e412d4d217e9', foundational, non_proliferation_is_binding_law).
narrative_ontology:cs_axiom_status(non_proliferation_is_binding_law, holdable).
narrative_ontology:cs_axiom_grounding('7d181b90-e9dc-417a-be11-e412d4d217e9', non_proliferation_is_binding_law, conventional).
narrative_ontology:cs_axiom('7d181b90-e9dc-417a-be11-e412d4d217e9', foundational, disarmament_is_aspirational_goal).
narrative_ontology:cs_axiom_status(disarmament_is_aspirational_goal, holdable).
narrative_ontology:cs_axiom_grounding('7d181b90-e9dc-417a-be11-e412d4d217e9', disarmament_is_aspirational_goal, conventional).
narrative_ontology:cs_reference_frame('7d181b90-e9dc-417a-be11-e412d4d217e9', npt_original_intent_nws_perspective).
narrative_ontology:cs_drift_state('7d181b90-e9dc-417a-be11-e412d4d217e9', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7d181b90-e9dc-417a-be11-e412d4d217e9', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, iaea).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, nuclear_deterrence_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nws_reading, great_power_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain their nuclear arsenals, interpret Article VI's disarmament obligation as aspirational and long-term, and benefit from the non-proliferation regime that prevents other states from acquiring nuclear weapons. They control the enforcement mechanisms (UNSC, IAEA budget priorities) that focus on horizontal proliferation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Abide by non-proliferation commitments, forgo nuclear weapons, and submit to IAEA safeguards. They bear the cost of verification and the strategic disadvantage of not possessing nuclear weapons, while perceiving the NWS's disarmament commitments as unfulfilled.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, non_nuclear_weapon_states, payer,
    moderate, biographical, constrained, global).

% Receives its mandate and funding primarily for verifying non-proliferation in NNWS. Its budget and operational focus are heavily influenced by NWS priorities, leading to a concentration on horizontal proliferation safeguards rather than disarmament verification.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, iaea, agenda_setter).

% Advocates for nuclear disarmament and universal non-proliferation, but lacks direct institutional power within the NPT framework. Its voice is often marginalized in official NPT review conferences, and its calls for NWS disarmament are largely unheeded.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, global_civil_society, excluded,
    organized, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm against the spread of nuclear weapons, preventing a multi-polar nuclear arms race and providing a framework for international cooperation on peaceful nuclear energy.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons exclusively to the five NWS, while transferring the obligation to forgo nuclear weapons and accept safeguards to all other states. It also transfers strategic stability benefits to NWS and security assurances to NNWS.
% ABSENT_VOICES: Non-nuclear weapon states advocating for a more binding disarmament timeline, and global civil society movements pushing for universal disarmament, are largely excluded from setting the NPT's interpretive agenda. Their arguments for a stronger Article VI are consistently sidelined by NWS.
% DISAPPEARANCE_RATIONALE: If the NPT vanished overnight, the global non-proliferation regime would collapse. Many NNWS would likely pursue nuclear weapons programs, leading to a rapid and dangerous proliferation cascade, fundamentally altering global security dynamics.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the uncontrolled spread of nuclear weapons technology after WWII and the Cuban Missile Crisis.
% FOUNDING_PROBLEM_CORROBORATION: All states, including NWS and NNWS, acknowledge the founding problem of nuclear proliferation remains live. However, NNWS and civil society groups argue that the NWS's failure to disarm exacerbates this problem, while NWS emphasize the continued need for non-proliferation.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because NNWS bear the full burden of non-proliferation without reciprocal disarmament from NWS. Suppression is very high (0.85) due to the UN Security Council's enforcement powers, which are disproportionately applied to NNWS proliferation. Theater ratio is high (0.60) as NWS engage in performative disarmament talks and incremental reductions that do not challenge their fundamental nuclear posture, while the core obligation remains unfulfilled. Accessibility collapse is high (0.70) because the NPT regime, backed by NWS power, effectively closes off the nuclear option for most NNWS. Resistance is moderate (0.45) from NNWS and civil society, but largely ineffective against NWS power.
 *
 * PERSPECTIVAL GAP:
 *   NWS perceive the NPT as a successful coordination mechanism for global stability, with their disarmament efforts being 'good faith' but not strictly binding. NNWS, however, experience it as an extractive regime that perpetuates a nuclear apartheid, where their security is diminished by the NWS's continued possession of nuclear weapons. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear Weapon States are clear beneficiaries (d=0.0-0.1) as they retain their arsenals and strategic advantage. Non-Nuclear Weapon States are targets (d=0.9-1.0) as they bear the costs of non-proliferation and verification without the promised disarmament. The IAEA, while a global body, largely benefits (d=0.2-0.3) from the NWS reading as its mandate and funding are focused on horizontal proliferation verification, aligning with NWS priorities. Global civil society is excluded (d=0.9) as their advocacy for disarmament is not structurally incorporated into the NPT's operational interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The NWS reading prevents mislabeling the NPT as a pure Snare by acknowledging its genuine coordination function (preventing widespread proliferation). However, it highlights how the coordination function has been leveraged to create an extractive asymmetry, where the disarmament mandate has atrophied into theatrical performance, but the non-proliferation mandate remains robustly enforced. The 'contested' status of the founding problem reflects this tension: the problem of proliferation is live, but the solution (universal disarmament) is not being pursued by the primary beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_interpretation_ambiguity,
    'Is Article VI''s ''at an early date'' clause a binding legal obligation for NWS to disarm, or an aspirational political commitment?',
    'International Court of Justice advisory opinion or a new NPT protocol with concrete disarmament timelines and verification mechanisms.',
    'If binding, the NWS reading''s extractiveness would be reclassified as higher, and its claimed type would shift closer to Snare due to non-compliance with a core obligation. If purely aspirational, the NWS reading''s current classification as Tangled Rope is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_interpretation_ambiguity, conceptual, 'Ambiguity in the legal force of NPT Article VI''s disarmament clause.').

omega_variable(
    iaea_mandate_scope,
    'To what extent is the IAEA''s mandate and budget structurally constrained by NWS priorities, preventing it from actively verifying disarmament?',
    'Independent audit of IAEA funding sources and operational directives, or a UN General Assembly resolution mandating IAEA involvement in NWS disarmament verification.',
    'If heavily constrained, the IAEA''s role as a ''beneficiary'' of the NWS reading is reinforced, highlighting its co-optation. If less constrained, it suggests a missed opportunity for the IAEA to push for disarmament verification, shifting its classification towards a more ''constrained'' or ''payer'' role.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iaea_mandate_scope, empirical, 'Structural limits on IAEA''s ability to verify NWS disarmament.').

omega_variable(
    npt_kernel_framing_ambiguity,
    'Is the NPT fundamentally a non-proliferation treaty with a disarmament aspiration, or a disarmament treaty with a non-proliferation component?',
    'A global consensus shift among states, or a new international treaty that explicitly redefines the balance of obligations.',
    'If framed as primarily non-proliferation, the NWS reading is reinforced. If framed as primarily disarmament, the NWS reading''s extractiveness and suppression would be seen as much higher, and its classification would shift towards a Snare, as it would be failing its primary purpose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_framing_ambiguity, conceptual, 'Fundamental framing of the NPT''s core purpose.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nws_reading, theater_ratio, 1968, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nws_reading, theater_ratio, 1980, 0.4).
narrative_ontology:measurement(npt__tr_t1992, npt_treaty_text__nws_reading, theater_ratio, 1992, 0.5).
narrative_ontology:measurement(npt__tr_t2004, npt_treaty_text__nws_reading, theater_ratio, 2004, 0.55).
narrative_ontology:measurement(npt__tr_t2016, npt_treaty_text__nws_reading, theater_ratio, 2016, 0.58).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nws_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nws_reading, base_extractiveness, 1968, 0.6).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nws_reading, base_extractiveness, 1980, 0.68).
narrative_ontology:measurement(npt__be_t1992, npt_treaty_text__nws_reading, base_extractiveness, 1992, 0.72).
narrative_ontology:measurement(npt__be_t2004, npt_treaty_text__nws_reading, base_extractiveness, 2004, 0.75).
narrative_ontology:measurement(npt__be_t2016, npt_treaty_text__nws_reading, base_extractiveness, 2016, 0.77).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nws_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__nws_reading, suppression_requirement, 1968, 0.7).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nws_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(npt__su_t1992, npt_treaty_text__nws_reading, suppression_requirement, 1992, 0.8).
narrative_ontology:measurement(npt__su_t2004, npt_treaty_text__nws_reading, suppression_requirement, 2004, 0.82).
narrative_ontology:measurement(npt__su_t2016, npt_treaty_text__nws_reading, suppression_requirement, 2016, 0.84).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nws_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, ctbt_ratification_constraint).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, iran_nuclear_deal_constraint).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel. It is linked to sibling readings (NNWS reading, withdrawal threshold reading) and other related arms control constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
