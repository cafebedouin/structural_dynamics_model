% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint represents the Non-Nuclear Weapon States' (NNWS) reading
 *   of the Nuclear Non-Proliferation Treaty (NPT) Article VI, which posits
 *   disarmament as a binding obligation for Nuclear Weapon States (NWS) and
 *   views NNWS non-proliferation as conditional on NWS compliance. This
 *   reading emphasizes the reciprocal nature of the NPT bargain. It is one
 *   reading of the 'npt_treaty_text' kernel, distinct from the NWS reading
 *   which treats disarmament as aspirational, and from readings concerning
 *   withdrawal thresholds.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.45).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.3).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'cf9607d6-1a61-4b89-bc03-c44c8ec5982b').
narrative_ontology:cs_kernel_codification('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', fixed_text).
narrative_ontology:cs_authority_grounding('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', lineage).
narrative_ontology:cs_interpretation_layer_present('cf9607d6-1a61-4b89-bc03-c44c8ec5982b').
narrative_ontology:cs_reading_relation('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', npt_treaty_text__withdrawal_threshold_reading, coexists_with).
narrative_ontology:cs_axiom('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', foundational, disarmament_is_binding_obligation).
narrative_ontology:cs_axiom_status(disarmament_is_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', disarmament_is_binding_obligation, deontological).
narrative_ontology:cs_axiom('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', foundational, non_proliferation_is_conditional).
narrative_ontology:cs_axiom_status(non_proliferation_is_conditional, holdable).
narrative_ontology:cs_axiom_grounding('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', non_proliferation_is_conditional, conventional).
narrative_ontology:cs_reference_frame('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', npt_foundational_bargain_reciprocity).
narrative_ontology:cs_drift_state('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cf9607d6-1a61-4b89-bc03-c44c8ec5982b', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_security_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states forgo nuclear weapons in exchange for security assurances and the promise of disarmament by NWS. They actively press for NWS compliance with Article VI at Review Conferences and through initiatives like the TPNW, viewing non-proliferation as conditional on disarmament progress.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary,
    organized, generational, constrained, global).

% These states are obligated under Article VI to pursue nuclear disarmament. From the NNWS perspective, this is a binding legal obligation, not an aspiration. They face diplomatic pressure and reputational costs for perceived non-compliance, but retain their arsenals.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer,
    institutional, generational, constrained, global).

% The IAEA administers safeguards to verify NNWS non-proliferation commitments. While not directly enforcing Article VI disarmament, its reporting and verification activities contribute to the transparency and accountability framework that NNWS leverage to pressure NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, civilizational, analytical, global).

% NGOs, academics, and civil society groups that advocate for nuclear disarmament and universal non-proliferation. They benefit from the NPT's framework as a basis for their advocacy and pressure campaigns, particularly on NWS disarmament obligations.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, global_security_advocates, beneficiary,
    moderate, generational, mobile, global).

% States that have ratified the TPNW, which explicitly prohibits nuclear weapons. While outside the NPT framework for NWS, their existence and advocacy create an alternative normative pressure on NWS to disarm, reinforcing the NNWS reading of Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, treaty_on_the_prohibition_of_nuclear_weapons_states, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global norm against nuclear proliferation by linking NNWS non-acquisition to NWS disarmament commitments, providing a framework for collective security and arms control negotiations.
% TRANSFER_FUNCTION: Transfers the obligation to disarm from an aspirational goal to a binding legal commitment for NWS, in exchange for NNWS foregoing nuclear weapons. It also transfers diplomatic pressure and reputational costs to NWS.
% ABSENT_VOICES: States that have withdrawn from the NPT or never joined, particularly those that have pursued nuclear weapons outside the treaty, represent voices that reject the premise of conditional restraint. TPNW states, while active, are excluded from the NPT's internal NWS-NNWS dialogue.
% DISAPPEARANCE_RATIONALE: If the NPT's Article VI obligation vanished, the foundational bargain of the treaty would collapse. NNWS would lose their primary legal leverage for disarmament, potentially leading to increased proliferation pressures and a more unstable global security environment. The entire arms control architecture would need to be renegotiated or would unravel.
% FOUNDING_PROBLEM: The original problem was to prevent the spread of nuclear weapons while acknowledging the existing nuclear arsenals of a few states, with the understanding that these states would eventually disarm.
% FOUNDING_PROBLEM_CORROBORATION: NNWS and global security advocates attest that the disarmament problem is very much live, citing the lack of significant progress by NWS. NWS, however, often argue that the security environment prevents rapid disarmament, effectively contesting the 'live' status of the problem as originally conceived, or reinterpreting 'eventually' to mean 'indefinitely'. Independent analysts and UN reports corroborate the slow pace of disarmament and the ongoing tension between NWS and NNWS interpretations.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The NNWS reading frames Article VI as a 'rope' because it genuinely coordinates global non-proliferation efforts, but with a moderate level of extractiveness (0.45) on NWS due to the diplomatic and reputational costs of non-compliance. Suppression (0.3) is relatively low, as NWS are sovereign and cannot be directly coerced into disarmament, but they face significant political pressure. Theater ratio (0.4) reflects the gap between stated disarmament goals and actual progress, with NWS often engaging in performative gestures without substantive reductions. Resistance (0.6) is high, driven by NNWS and civil society pushing for greater NWS accountability.
 *
 * PERSPECTIVAL GAP:
 *   The NNWS reading fundamentally differs from the NWS reading. NNWS perceive Article VI as a binding legal commitment, while NWS often interpret it as an aspirational goal. This divergence leads to different experiences of the constraint: for NNWS, it's a tool for leverage; for NWS, it's a source of diplomatic friction. The engine will compute these as distinct classifications from the different structural declarations in each reading's story.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are beneficiaries (d near 0.0) as they gain security assurances and a legal basis to pressure NWS. NWS are targets (d near 1.0) as they bear the obligation to disarm and face diplomatic costs. The IAEA acts as an agenda-setter, facilitating the framework. Global security advocates are beneficiaries of the framework's existence, while TPNW states are excluded from the NPT's internal dialogue but exert external pressure.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_disarmament_commitment_sincerity,
    'To what extent are NWS genuinely committed to Article VI disarmament, versus using it as a diplomatic facade for indefinite retention?',
    'Analysis of NWS nuclear doctrine, investment in modernization vs. reduction, and willingness to engage in verifiable multilateral disarmament negotiations.',
    'If NWS commitment is primarily theatrical, the constraint''s effective extractiveness on NNWS (via unfulfilled promises) is higher, and its classification shifts closer to a Snare for NNWS, as the coordination function becomes cover for NWS retention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_disarmament_commitment_sincerity, empirical, 'Assessing the sincerity of NWS disarmament commitments under Article VI.').

omega_variable(
    nnws_leverage_effectiveness,
    'How effective is NNWS diplomatic pressure and the TPNW regime in compelling NWS disarmament, given NWS security concerns?',
    'Empirical study of NWS policy changes, arms control agreements, and public statements in response to NNWS and TPNW advocacy over time.',
    'If NNWS leverage is consistently ineffective, the constraint''s ''rope'' classification for NNWS weakens, as the promised benefit of disarmament remains elusive, potentially pushing it towards a ''piton'' (atrophied function) or even a ''snare'' (false promise).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_leverage_effectiveness, empirical, 'Evaluating the real-world impact of NNWS pressure on NWS disarmament.').

omega_variable(
    npt_bargain_integrity,
    'Is the NPT''s foundational bargain (NNWS non-proliferation for NWS disarmament) still considered valid by a majority of NNWS, or has it eroded significantly?',
    'Analysis of NNWS statements at NPT Review Conferences, voting patterns on disarmament resolutions, and adherence to the TPNW.',
    'If the bargain''s integrity is widely perceived as eroded, the constraint''s legitimacy and stability are compromised, increasing the risk of NPT withdrawal or proliferation by NNWS, and shifting the constraint''s classification towards a ''piton'' or even ''snare'' from the NNWS perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_bargain_integrity, conceptual, 'Assessing the continued validity of the NPT''s core bargain from the NNWS perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_text__nnws_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_text__nnws_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__nnws_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_text__nnws_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_text__nnws_reading, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.42).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__nnws_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_text__nnws_reading, suppression_requirement, 1980, 0.22).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_text__nnws_reading, suppression_requirement, 1990, 0.25).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.27).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.29).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__nnws_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the NPT treaty text kernel, focusing on the NNWS interpretation of Article VI disarmament obligations. It is linked to the NWS reading and the withdrawal threshold reading, which represent alternative interpretations of the same treaty.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
