% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI as Reciprocal Disarmament Bargain
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint models the NPT as a reciprocal disarmament bargain,
 *   emphasizing Article VI's binding obligation for Nuclear Weapon States
 *   (NWS) to disarm, in exchange for Non-Nuclear Weapon States (NNWS)
 *   foregoing nuclear weapons. This reading views horizontal and vertical
 *   nonproliferation as two sides of the same coin, with temporal urgency for
 *   NWS disarmament. The increasing extractiveness and suppression over time
 *   reflect the growing frustration of NNWS with the lack of NWS disarmament
 *   progress, and the increasing enforcement required to maintain the
 *   horizontal nonproliferation norm in the face of perceived NWS
 *   non-compliance.
 *
 * KEY AGENTS:
 *   - non_nuclear_weapon_states_coalition: Primary beneficiary (organized/constrained) – gains normative leverage, but security remains contingent.
 *   - nuclear_weapon_states: Primary payer (institutional/constrained) – bears the normative cost of delayed disarmament, strategic autonomy constrained.
 *   - non_nuclear_weapon_states_constrained: Secondary payer (moderate/identity_locked) – bears the cost of non-acquisition without reciprocal disarmament.
 *   - international_atomic_energy_agency: Agenda setter (institutional/constrained) – enforces horizontal nonproliferation, but lacks mandate for Article VI.
 *   - global_civil_society_disarmament_advocates: Excluded (organized/mobile) – advocates for full Article VI implementation, but without direct negotiation power.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.65).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Reciprocal Disarmament Bargain").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '2b0a4d7c-e123-43de-bcd0-b354d84b4d7b').
narrative_ontology:cs_kernel_codification('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', fixed_text).
narrative_ontology:cs_authority_grounding('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', lineage).
narrative_ontology:cs_interpretation_layer_present('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b').
narrative_ontology:cs_reading_relation('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', foundational, article_vi_binding_and_urgent).
narrative_ontology:cs_axiom_status(article_vi_binding_and_urgent, holdable).
narrative_ontology:cs_axiom_grounding('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', article_vi_binding_and_urgent, deontological).
narrative_ontology:cs_axiom('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', foundational, horizontal_and_vertical_nonproliferation_linked).
narrative_ontology:cs_axiom_status(horizontal_and_vertical_nonproliferation_linked, holdable).
narrative_ontology:cs_axiom_grounding('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', horizontal_and_vertical_nonproliferation_linked, conventional).
narrative_ontology:cs_reference_frame('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', grand_bargain_reciprocity).
narrative_ontology:cs_drift_state('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', contemporary_npt_review_cycle, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2b0a4d7c-e123-43de-bcd0-b354d84b4d7b', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_constrained).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the promise of eventual disarmament by NWS, which legitimizes their own non-acquisition. They gain normative leverage to press for Article VI compliance, but their security remains contingent on NWS actions. Their exit options are constrained by the security dilemma.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary,
    organized, generational, constrained, global).

% Are obligated by Article VI to pursue good-faith negotiations on disarmament. This reading places their strategic autonomy and modernization programs under normative pressure, making them 'payers' of the disarmament bargain. Their exit is constrained by the political costs of treaty withdrawal.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% Have foregone nuclear weapons under the NPT, expecting reciprocal disarmament from NWS. They bear the cost of non-acquisition without seeing the promised disarmament, leading to a sense of structural injustice. Their identity as responsible non-proliferators makes withdrawal difficult.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_constrained, payer,
    moderate, biographical, identity_locked, national).

% Administers safeguards for horizontal nonproliferation (Articles I-II) but lacks a mandate or verification mechanism for Article VI disarmament. This reading highlights the gap in its enforcement capacity regarding the reciprocal bargain.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Advocates for the full and urgent implementation of Article VI, viewing it as a moral and legal imperative. They are excluded from direct negotiation but exert pressure through public campaigns and international forums.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, global_civil_society_disarmament_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global security by establishing a reciprocal bargain: NNWS forego nuclear weapons in exchange for NWS pursuing disarmament, aiming to prevent both horizontal and vertical proliferation.
% TRANSFER_FUNCTION: Transfers the right to develop nuclear weapons from NNWS to NWS (who retain them temporarily), in exchange for a promise of future disarmament. It also transfers security assurances and peaceful nuclear technology to NNWS.
% ABSENT_VOICES: States that have withdrawn from the NPT or never joined, arguing the treaty is inherently discriminatory, would object to the continued retention of nuclear weapons by NWS. They are absent from the NPT review process.
% DISAPPEARANCE_RATIONALE: If the NPT's reciprocal disarmament obligation vanished, the global nonproliferation regime would collapse. Many NNWS would likely pursue nuclear weapons, leading to a rapid increase in horizontal proliferation and a drastically less stable international security environment.
% FOUNDING_PROBLEM: The problem of preventing the spread of nuclear weapons (horizontal proliferation) while also addressing the existential threat posed by existing nuclear arsenals (vertical proliferation), through a grand bargain.
% FOUNDING_PROBLEM_CORROBORATION: The non-nuclear-weapon states coalition consistently attests that the founding problem of vertical proliferation remains live and unaddressed, citing the continued modernization of NWS arsenals. Independent security analysts and UN reports corroborate the ongoing threat and the lack of progress on disarmament.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates horizontal nonproliferation (benefiting NNWS) but simultaneously extracts from NWS (by obligating disarmament) and from NNWS (who bear the cost of non-acquisition without full reciprocity). The extractiveness (0.65) is high due to the perceived imbalance in the bargain, with NWS retaining and modernizing arsenals while NNWS are strictly held to non-acquisition. Suppression (0.70) is significant, reflecting the active enforcement of horizontal nonproliferation and the diplomatic pressure on NWS to maintain the disarmament facade. Theater ratio (0.40) indicates that a substantial portion of NWS disarmament rhetoric and diplomatic activity is performative, masking a lack of genuine progress. The rising extractiveness and theater over time reflect the increasing gap between the promise of Article VI and its implementation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the NNWS coalition, the NPT is a Tangled Rope where they are coordinated into non-acquisition but pay through the lack of NWS disarmament. From the NWS perspective, it is a Rope that coordinates global security, with their disarmament obligation being a long-term aspiration rather than an immediate, binding cost. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The NNWS coalition is a beneficiary due to the security benefits of horizontal nonproliferation and the normative leverage for disarmament (d near 0.0). NWS are payers because Article VI places a binding obligation on their strategic autonomy (d near 1.0). Non-nuclear-weapon states constrained are also payers, bearing the costs of non-acquisition without the promised reciprocity (d near 1.0). The IAEA is an agenda-setter, enforcing parts of the regime. Global civil society advocates are excluded, as their calls for disarmament are not directly integrated into the treaty's enforcement mechanisms.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the NPT as a pure Rope by highlighting the unfulfilled reciprocal bargain. The increasing extractiveness and theater ratio over time suggest a drift towards a Snare if Article VI continues to be ignored, as the coordination function (horizontal nonproliferation) becomes cover for the extraction of strategic advantage by NWS. The 'live' status of the founding problem, coupled with 'world_rearranges' if it disappeared, indicates that the constraint's mandate is still relevant, but its implementation is deeply contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_verifiability,
    'Is Article VI''s disarmament obligation genuinely verifiable, or does the lack of a verification mechanism render it inherently aspirational?',
    'Development and adoption of a robust, intrusive, and universally accepted verification regime for nuclear disarmament, similar to IAEA safeguards for non-acquisition.',
    'If verifiable, the NWS''s non-compliance becomes a clear violation, increasing the constraint''s extractiveness and suppression. If inherently unverifiable, the ''reciprocal bargain'' aspect weakens, potentially reclassifying towards a more aspirational ''Rope'' for NWS.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_verifiability, conceptual, 'Ambiguity regarding the verifiability of NWS disarmament obligations.').

omega_variable(
    nws_disarmament_intent,
    'Do NWS genuinely intend to pursue nuclear disarmament in good faith, or is their Article VI commitment primarily a diplomatic facade to maintain the nonproliferation regime?',
    'Analysis of NWS nuclear doctrine, spending on modernization vs. disarmament, and participation in disarmament negotiations over a multi-decade period, assessed by independent experts.',
    'If intent is genuinely lacking, the theater_ratio would increase, and the constraint would lean more towards a ''Snare'' for NNWS. If genuine intent is demonstrated, extractiveness would decrease.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_intent, empirical, 'Uncertainty about the true intentions of Nuclear Weapon States regarding disarmament.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''reciprocal disarmament bargain'' reading of the NPT, or is it an over-interpretation of Article VI''s legal force?',
    'Comparative legal analysis of treaty interpretation principles, state practice, and travaux préparatoires (preparatory works) of the NPT, assessed by international legal scholars.',
    'If it is an over-interpretation, the constraint''s extractiveness from NWS would be lower, and its classification might shift towards a ''Rope'' or ''Tangled Rope'' with less emphasis on NWS as payers. If confirmed as a valid reading, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the NPT kernel (npt_treaty_1970). This ''reciprocal_disarmament_reading'' emphasizes Article VI as a binding, urgent obligation. Sibling readings include ''oligopoly_enforcement_reading'' (focus on horizontal nonproliferation) and ''withdrawal_sovereignty_reading'' (focus on Article X withdrawal rights). The disagreement is located in the legal weight and temporal urgency of Article VI versus other treaty provisions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.65).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty kernel. It emphasizes Article VI as a binding reciprocal disarmament bargain, contrasting with readings that prioritize horizontal nonproliferation or withdrawal rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
