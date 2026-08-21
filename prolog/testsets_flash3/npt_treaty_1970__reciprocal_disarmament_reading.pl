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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Treaty (1970): Reciprocal Disarmament Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint represents the 'reciprocal disarmament' reading of the
 *   Nuclear Non-Proliferation Treaty (NPT), emphasizing Article VI as a
 *   binding legal obligation for Nuclear Weapon States (NWS) to disarm, in
 *   exchange for Non-Nuclear Weapon States (NNWS) foregoing nuclear weapons.
 *   It views horizontal and vertical nonproliferation as a reciprocal
 *   bargain. The NWS's continued modernization of arsenals and lack of
 *   verifiable disarmament are seen as a violation of this bargain, leading
 *   to high perceived extraction and resistance from NNWS. This reading
 *   frames the NPT as a Tangled Rope, where the coordination of horizontal
 *   nonproliferation is tied to the extraction of strategic autonomy from
 *   NNWS due to unfulfilled disarmament promises.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.75).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Treaty (1970): Reciprocal Disarmament Reading").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '6a487e51-ff59-4c15-b6cb-175c935f7c02').
narrative_ontology:cs_kernel_codification('6a487e51-ff59-4c15-b6cb-175c935f7c02', fixed_text).
narrative_ontology:cs_authority_grounding('6a487e51-ff59-4c15-b6cb-175c935f7c02', lineage).
narrative_ontology:cs_interpretation_layer_present('6a487e51-ff59-4c15-b6cb-175c935f7c02').
narrative_ontology:cs_reading_relation('6a487e51-ff59-4c15-b6cb-175c935f7c02', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a487e51-ff59-4c15-b6cb-175c935f7c02', npt_treaty_1970__withdrawal_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('6a487e51-ff59-4c15-b6cb-175c935f7c02', foundational, article_vi_binding_temporal_urgency).
narrative_ontology:cs_axiom_status(article_vi_binding_temporal_urgency, holdable).
narrative_ontology:cs_axiom_grounding('6a487e51-ff59-4c15-b6cb-175c935f7c02', article_vi_binding_temporal_urgency, deontological).
narrative_ontology:cs_axiom('6a487e51-ff59-4c15-b6cb-175c935f7c02', foundational, horizontal_vertical_nonproliferation_reciprocal_bargain).
narrative_ontology:cs_axiom_status(horizontal_vertical_nonproliferation_reciprocal_bargain, holdable).
narrative_ontology:cs_axiom_grounding('6a487e51-ff59-4c15-b6cb-175c935f7c02', horizontal_vertical_nonproliferation_reciprocal_bargain, conventional).
narrative_ontology:cs_reference_frame('6a487e51-ff59-4c15-b6cb-175c935f7c02', original_grand_bargain_intent).
narrative_ontology:cs_drift_state('6a487e51-ff59-4c15-b6cb-175c935f7c02', contemporary_nws_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6a487e51-ff59-4c15-b6cb-175c935f7c02', '').
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

% Possess nuclear weapons and are obligated by Article VI to pursue disarmament in good faith. This reading views their continued modernization and lack of verifiable disarmament as a violation of the reciprocal bargain, making their strategic autonomy a cost borne by the regime. They benefit from horizontal nonproliferation but resist vertical disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Forego nuclear weapons in exchange for security assurances and the NWS commitment to disarm. This reading emphasizes their normative leverage and the expectation of reciprocal disarmament. They are beneficiaries of horizontal nonproliferation but victims of the NWS's failure to disarm.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, beneficiary,
    organized, generational, constrained, global).

% Are bound by IAEA safeguards and forgo nuclear weapons, bearing the costs of verification and the strategic disadvantage relative to NWS. Their identity as NPT signatories is tied to the reciprocal bargain, making exit difficult despite perceived NWS non-compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_constrained, payer,
    moderate, biographical, identity_locked, national).

% Administers safeguards to prevent horizontal proliferation (Articles I-II) but lacks a mandate or mechanism to verify NWS disarmament (Article VI). Its enforcement is asymmetric, focusing on NNWS compliance while NWS disarmament remains unverified.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Advocate for full implementation of Article VI and highlight the imbalance between horizontal and vertical nonproliferation. They provide critical analysis and pressure for NWS disarmament, often framing the NPT as a disarmament treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, civil_society_nonproliferation_advocates, observer,
    moderate, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates global efforts to prevent the spread of nuclear weapons (horizontal nonproliferation) by establishing a framework for states to forgo nuclear weapons in exchange for peaceful nuclear technology and a commitment from NWS to disarm.
% TRANSFER_FUNCTION: Transfers strategic advantage and security from non-nuclear weapon states to nuclear weapon states, in exchange for a promise of future disarmament and access to peaceful nuclear technology. It also transfers the burden of verifiable nonproliferation to NNWS.
% ABSENT_VOICES: States that have withdrawn from the NPT or never joined, citing the NWS's failure to disarm, are absent from the core dialogue but their actions serve as a constant challenge to the regime's legitimacy. They would argue for a more equitable and enforceable disarmament process.
% DISAPPEARANCE_RATIONALE: If the NPT vanished, the global nonproliferation architecture would collapse, leading to a rapid increase in nuclear weapon states, heightened regional tensions, and a breakdown of international security norms. The world would fundamentally reorganize around a more dangerous, multipolar nuclear landscape.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent the uncontrolled spread of nuclear weapons technology to more states, while also acknowledging the NWS's commitment to eventually disarm.
% FOUNDING_PROBLEM_CORROBORATION: NWS attest the problem of horizontal proliferation is live, justifying their continued nuclear arsenals. NNWS and civil society advocates attest the problem of vertical proliferation (NWS disarmament) is also live and unaddressed, making the founding bargain incomplete. Independent historical analysis and UN resolutions corroborate the dual nature of the founding problem.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.68) because NNWS bear the costs of non-proliferation (safeguards, strategic disadvantage) without the promised reciprocal disarmament from NWS. Suppression (0.75) is high because the regime actively enforces horizontal nonproliferation through IAEA safeguards and sanctions, while NWS disarmament remains largely unverified and unenforced. Theater ratio (0.45) reflects the performative aspects of NWS disarmament rhetoric versus actual modernization programs. Resistance (0.8) is high due to persistent calls from NNWS for NWS compliance with Article VI and the establishment of a nuclear-weapon-free world.
 *
 * PERSPECTIVAL GAP:
 *   NWS perceive the NPT primarily as a successful horizontal nonproliferation regime, with Article VI as an aspirational goal. NNWS, under this reading, perceive it as an unfulfilled reciprocal bargain, where their compliance is met with NWS non-compliance on disarmament. This leads to a significant divergence in perceived extractiveness and fairness, with NNWS experiencing the constraint as highly extractive and NWS as a beneficial coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The NNWS coalition is a beneficiary of horizontal nonproliferation (reduced risk of neighbors acquiring weapons) but a victim of the NWS's failure to disarm (perpetual strategic disadvantage). The NWS are agenda-setters, benefiting from horizontal nonproliferation while resisting the disarmament aspect of the bargain. The IAEA is an agenda-setter for horizontal nonproliferation but lacks authority over vertical disarmament. This reading places NWS strategic autonomy within the victim set, as it is constrained by the normative expectation of disarmament.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verifiability_of_nws_disarmament,
    'Is verifiable NWS disarmament technically and politically feasible, or is it an inherently unresolvable problem?',
    'Development and implementation of robust, intrusive verification mechanisms for NWS arsenals, or a consensus among NWS that such verification is impossible without compromising national security.',
    'If feasible, the lack of disarmament is a political choice, increasing the perceived extraction and suppression. If infeasible, the NPT''s reciprocal bargain is structurally flawed, shifting the constraint towards a more ''mountain-like'' (unsolvable) or ''snare-like'' (inherently extractive) classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verifiability_of_nws_disarmament, empirical, 'Feasibility of verifying NWS disarmament.').

omega_variable(
    npt_founding_intent_ambiguity,
    'Was the NPT primarily intended as a horizontal nonproliferation treaty with a secondary disarmament aspiration, or as a grand bargain for eventual nuclear disarmament?',
    'Historical analysis of diplomatic records, declassified documents, and statements from original negotiators, weighed against subsequent state practice and legal interpretations.',
    'If primarily horizontal, the ''oligopoly enforcement'' reading gains legitimacy, reducing the perceived extraction from NWS. If a grand bargain, this ''reciprocal disarmament'' reading is strengthened, increasing the perceived injustice of NWS non-compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_founding_intent_ambiguity, conceptual, 'Ambiguity in the NPT''s founding intent regarding disarmament.').

omega_variable(
    nws_strategic_autonomy_as_victim,
    'Is the NWS''s strategic autonomy (freedom to modernize nuclear arsenals) a legitimate national security imperative, or an extractive privilege that undermines the NPT''s reciprocal bargain?',
    'International legal adjudication or a shift in global security norms that redefines the legitimacy of nuclear deterrence in a non-proliferation context.',
    'If legitimate, NWS strategic autonomy is not a ''cost'' to the regime, reducing the perceived extraction. If an extractive privilege, it reinforces the ''tangled rope'' classification and the victim status of NWS strategic autonomy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nws_strategic_autonomy_as_victim, preference, 'Legitimacy of NWS strategic autonomy within the NPT framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1980, 0.58).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, ctbt_treaty_regime).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the NPT Treaty (1970) kernel. This 'reciprocal disarmament' reading emphasizes Article VI as a binding obligation for NWS, contrasting with the 'oligopoly enforcement' reading (focus on Articles I-II) and the 'withdrawal sovereignty' reading (focus on Article X).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
