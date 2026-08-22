% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
 *   constraint_id: npt_treaty_text__nws_reading
 *   human_readable: NPT NWS Reading: Binding Non-Proliferation, Aspirational Disarmament
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This constraint instantiates the Nuclear-Weapon-State (NWS) reading of
 *   the Nuclear Non-Proliferation Treaty (NPT) kernel: non-proliferation
 *   obligations under Articles II and III are treated as binding and
 *   enforceable upon Non-Nuclear-Weapon States (NNWS), while Article VI
 *   disarmament obligations are interpreted as aspirational, hortatory, and
 *   lacking enforcement machinery. The NWS (P5) exercise interpretive control
 *   over the 'at an early date' ambiguity to sustain asymmetric obligations.
 *   The IAEA safeguards budget and verification apparatus concentrate
 *   overwhelmingly on horizontal proliferation by NNWS, leaving NWS vertical
 *   arsenals and modernization outside comparable verification. This reading
 *   coexists with the NNWS reading (which treats Article VI as a binding
 *   reciprocal obligation) and the withdrawal-threshold reading (which
 *   contests Article X accessibility). The constraint is claimed as
 *   tangled_rope because it retains a genuine coordination
 *   functionâpreventing unchecked horizontal proliferationâwhile
 *   asymmetrically extracting compliance and sovereignty costs from NNWS to
 *   the benefit of NWS.
 *
 * KEY AGENTS:
 *   - nws_p5: Primary beneficiary and agenda-setter (powerful/arbitrage) â exercises interpretive control to maintain nuclear status while shifting non-proliferation burdens.
 *   - nnws_states: Primary payer (organized/constrained) â bound by safeguards and non-proliferation, forgo weapons option, receive non-binding disarmament promises.
 *   - iaea_secretariat: Enforcement administrator (institutional/constrained) â verifies NNWS compliance via safeguards; mandate and budget concentrate on horizontal proliferation.
 *   - disarmament_advocacy: Excluded voice (organized/constrained) â argues for binding Article VI interpretation but lacks access to interpretive control.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nws_reading, 0.72).
domain_priors:suppression_score(npt_treaty_text__nws_reading, 0.68).
domain_priors:theater_ratio(npt_treaty_text__nws_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_treaty_text__nws_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nws_reading, "NPT NWS Reading: Binding Non-Proliferation, Aspirational Disarmament").
narrative_ontology:topic_domain(npt_treaty_text__nws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nws_reading, '1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa').
narrative_ontology:cs_kernel_codification('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', fixed_text).
narrative_ontology:cs_authority_grounding('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', lineage).
narrative_ontology:cs_interpretation_layer_present('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa').
narrative_ontology:cs_reading_relation('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', npt_treaty_text__nnws_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', foundational, disarmament_aspirational_not_binding).
narrative_ontology:cs_axiom_status(disarmament_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', disarmament_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', foundational, non_proliferation_sole_binding_obligation).
narrative_ontology:cs_axiom_status(non_proliferation_sole_binding_obligation, holdable).
narrative_ontology:cs_axiom_grounding('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', non_proliferation_sole_binding_obligation, conventional).
narrative_ontology:cs_reference_frame('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', strategic_bargain_stability).
narrative_ontology:cs_drift_state('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', contemporary_nnws_pressure, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('1f606ab6-ac10-4bf0-b5d9-cfd2e6620dfa', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nws_reading, nws_p5).
narrative_ontology:constraint_victim(npt_treaty_text__nws_reading, nnws_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five nuclear-weapon states interpret the NPT to preserve their nuclear status indefinitely while enforcing non-proliferation on others. They control the treaty review process and resist binding disarmament timetables, treating Article VI as aspirational.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nws_p5, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nws_reading, nws_p5, agenda_setter).

% The non-nuclear-weapon states parties accept comprehensive IAEA safeguards, forgo nuclear weapons development, and rely on a disarmament promise that lacks enforcement. Their exit via Article X withdrawal is technically available but politically and economically prohibitive.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, nnws_states, payer,
    organized, generational, constrained, global).

% Administers safeguards agreements for NNWS under Articles II and III, verifying non-diversion of nuclear material. Its mandate, budget, and technical capacity concentrate on horizontal proliferation verification, with no comparable verification of NWS disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, iaea_secretariat, agenda_setter,
    institutional, generational, constrained, global).

% Civil society coalitions and some state delegations argue that Article VI creates a binding legal obligation to negotiate disarmament in good faith. They participate in Review Conferences but are structurally excluded from the interpretive control exercised by NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nws_reading, disarmament_advocacy, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nws_reading, nws_p5).
narrative_ontology:fixing_cost_class(npt_treaty_text__nws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unchecked horizontal nuclear proliferation by creating a verified, legally binding renunciation of nuclear weapons by non-nuclear-weapon states, coupled with access to peaceful nuclear technology.
% TRANSFER_FUNCTION: Moves compliance burdens, sovereignty costs, and verification intrusiveness from NNWS to the NWS-led order; NWS retain nuclear arsenals and interpretive control while NNWS forgo the weapons option.
% ABSENT_VOICES: NNWS advocates of binding disarmament timelines are present in forums but excluded from interpretive control; states that might exercise Article X withdrawal face systemic discouragement; rival nuclear orders outside the treaty framework are excluded entirely.
% DISAPPEARANCE_RATIONALE: If the NWS reading vanished and Article VI were enforced as binding, NWS would face disarmament timelines, NNWS would likely condition continued compliance on reciprocal verification, the IAEA would reorient toward vertical proliferation monitoring, and the global nuclear order would restructure around symmetrical obligation rather than asymmetric restraint.
% FOUNDING_PROBLEM: Rapid horizontal nuclear proliferation in the 1960s threatened superpower stability and increased the risk of nuclear war; a bargain was needed to freeze the number of nuclear-armed states while offering peaceful nuclear cooperation to others.
% FOUNDING_PROBLEM_CORROBORATION: NWS and the IAEA corroborate the ongoing live threat of horizontal proliferation. Independent security studies scholars, the International Court of Justice advisory opinion on Article VI, and the Non-Aligned Movement corroborate that the disarmament promise is unfulfilled and the bargain is asymmetric. No single external party attests both halves without conflict of interest.
narrative_ontology:disappearance_verdict(npt_treaty_text__nws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nws_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Base extractiveness (0.72) is high because the standing arrangement decouples NWS obligations from NNWS obligations: NWS retain nuclear weapons indefinitely while NNWS accept intrusive verification and forgo the weapons option. Suppression (0.68) reflects active enforcement (safeguards, export controls, sanctions for NNWS violations) combined with the effective unavailability of Article X withdrawal for most states. Theater ratio (0.45) captures the growing performative dimension of Review Conferences, where disarmament commitments are reiterated without binding timetables. Accessibility collapse (0.70) is high because alternativesâproliferation or withdrawalâare heavily sanctioned and normatively foreclosed. Resistance (0.50) is moderate: NNWS mount rhetorical and diplomatic resistance through the Non-Aligned Movement, but material non-compliance remains rare. The measurement series trace a monotonic increase in extraction and theater from 1970 to 2020, reflecting the widening gap between the disarmament promise and NWS arsenal modernization.
 *
 * PERSPECTIVAL GAP:
 *   The NWS seat perceives the constraint as a stabilizing coordination mechanism that prevents nuclear anarchy and preserves a manageable arsenal hierarchy; from this seat the disarmament language was always understood as aspirational. The NNWS seat perceives the same text as a broken bargain in which their permanent renunciation is enforced while the reciprocal promise is indefinitely deferred. The IAEA seat perceives a technical verification mandate that is structurally lopsided by design. The engine computes these divergent types from the same structural dataâbeneficiary status, exit options, and scopeâwithout requiring reconciliation.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS P5 are declared beneficiaries with arbitrage-grade exit (they control interpretation and face no enforcement for non-disarmament), producing very low directionality. NNWS states are declared victims/payers with constrained exit (safeguards are intrusive, withdrawal is politically and economically prohibitive), producing high directionality. The IAEA secretariat sits near symmetric: it administers enforcement but does not capture the extracted surplus; its directionality is structurally intermediate. The engine amplifies effective extraction for NNWS and damps it for NWS accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâunchecked horizontal proliferationâremains live, so the coordination function has not fully atrophied. However, the disarmament pillar's indefinite postponement constitutes partial mandatrophy: one stated purpose of the arrangement (eventual disarmament) is treated as obsolescent by the dominant reading while the enforcement machinery persists. The constraint is classified as tangled_rope rather than snare because the non-proliferation coordination is genuine and not merely cover; the extraction is layered onto that coordination through interpretive asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nws_reading_kernel_position,
    'Does the NWS reading of the NPT represent the only coherent interpretation of Articles II and VI, or does the NNWS reading have equal textual support?',
    'Comparative legal analysis of travaux prÃ©paratoires and state practice; ICJ advisory opinion on Article VI bindingness.',
    'If the NNWS reading is textually coherent, the NWS reading is a contested construction rather than a natural interpretation, raising epsilon and shifting classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_reading_kernel_position, conceptual, 'Whether the NWS reading is the sole viable interpretation of the NPT kernel.').

omega_variable(
    enforcement_asymmetry_verification,
    'Is the concentration of IAEA safeguards on NNWS horizontal proliferation a necessary structural feature of treaty verification, or an allocative choice benefiting NWS?',
    'Comparative budget analysis of IAEA resources devoted to NNWS safeguards vs NWS disarmament verification; assessment of technical feasibility of universal verification.',
    'If allocative choice, the enforcement asymmetry constitutes extractive overhead layered onto genuine coordination, confirming tangled_rope classification; if technically necessary, extraction is damped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_verification, empirical, 'Whether verification asymmetry is structural necessity or allocative bias.').

omega_variable(
    withdrawal_threshold_extraction,
    'Does the effective high threshold for Article X withdrawal (despite textual ambiguity) function as suppression of NNWS exit options?',
    'Case study of state withdrawal attempts and the political and economic sanctions incurred; textual analysis of ''extraordinary events'' threshold.',
    'If the threshold is effectively insurmountable for most NNWS, suppression is higher than formal treaty text suggests, amplifying effective extraction for NNWS seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_extraction, empirical, 'Whether Article X withdrawal is practically accessible or structurally suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nws_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__nws_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_text__nws_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_text__nws_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_text__nws_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__nws_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(npt__tr_t50, npt_treaty_text__nws_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__nws_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt__be_t10, npt_treaty_text__nws_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(npt__be_t20, npt_treaty_text__nws_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(npt__be_t30, npt_treaty_text__nws_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__nws_reading, base_extractiveness, 40, 0.7).
narrative_ontology:measurement(npt__be_t50, npt_treaty_text__nws_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__nws_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(npt__su_t10, npt_treaty_text__nws_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(npt__su_t20, npt_treaty_text__nws_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(npt__su_t30, npt_treaty_text__nws_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__nws_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(npt__su_t50, npt_treaty_text__nws_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the npt_treaty_text kernel. It is structurally paired with nnws_reading and withdrawal_threshold_reading as alternative instantiations of the same treaty text. Epsilon values differ because the referentâthe standing arrangement under contestâis indexed to the reading's own evaluative frame: this reading treats the non-proliferation obligation as the binding core and disarmament language as hortatory, while the nnws_reading treats disarmament as a binding reciprocal obligation. The constraint family should be analyzed as a decomposition per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
