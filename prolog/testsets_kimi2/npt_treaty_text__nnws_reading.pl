% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Treaty Text â NNWS Reading (Binding Disarmament)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the NNWS reading of the npt_treaty_text
 *   kernel, which holds that Article VI creates a binding, legally
 *   enforceable obligation on Nuclear Weapon States (NWS) to pursue nuclear
 *   disarmament, and that Non-Nuclear Weapon States' (NNWS) non-proliferation
 *   restraint is a conditional quid pro quo purchasing NWS compliance. The
 *   kernel conflates three distinct readings: the NWS reading
 *   (non-proliferation as binding, disarmament as aspirational), the NNWS
 *   reading (this file), and the withdrawal_threshold_reading (high vs. low
 *   barrier to exit under Article X). The NNWS reading is characterized by
 *   moderate extractiveness: the coordination function (non-proliferation) is
 *   real and beneficial, but the asymmetric failure of NWS disarmament means
 *   NNWS restraint is not fully reciprocated. The constraint relies on weak
 *   enforcement mechanismsâReview Conference consensus pressure and the
 *   TPNW's normative competitionâto impose costs on NWS, resulting in lower
 *   suppression and a tangled rather than purely extractive structure.
 *
 * KEY AGENTS:
 *   - Nuclear Weapon States (P5): Primary beneficiaries of NNWS non-proliferation; structurally dual-positioned as intended targets of disarmament obligation (institutional/constrained).
 *   - Non-Nuclear Weapon States: Primary payers of proliferation restraint; collectively organized but individually moderate power; constrained exit (biographical/generational).
 *   - IAEA: Agenda-setter for safeguards verification; institutional power but no disarmament enforcement mandate.
 *   - TPNW Regime: Excluded alternative pathway; exerts normative pressure from outside the NPT architecture.
 *   - International Court of Justice: Analytical observer supplying legal interpretation without enforcement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.45).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.55).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Treaty Text â NNWS Reading (Binding Disarmament)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '0c76681e-d786-479c-973c-146a6d167917').
narrative_ontology:cs_kernel_codification('0c76681e-d786-479c-973c-146a6d167917', formalized).
narrative_ontology:cs_authority_grounding('0c76681e-d786-479c-973c-146a6d167917', distributed).
narrative_ontology:cs_reading_relation('0c76681e-d786-479c-973c-146a6d167917', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c76681e-d786-479c-973c-146a6d167917', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('0c76681e-d786-479c-973c-146a6d167917', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('0c76681e-d786-479c-973c-146a6d167917', article_vi_binding_disarmament_obligation, conventional).
narrative_ontology:cs_axiom('0c76681e-d786-479c-973c-146a6d167917', foundational, nonproliferation_conditional_reciprocity).
narrative_ontology:cs_axiom_status(nonproliferation_conditional_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('0c76681e-d786-479c-973c-146a6d167917', nonproliferation_conditional_reciprocity, conventional).
narrative_ontology:cs_reference_frame('0c76681e-d786-479c-973c-146a6d167917', reciprocal_nuclear_bargain_1968).
narrative_ontology:cs_drift_state('0c76681e-d786-479c-973c-146a6d167917', tpnw_contemporary_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0c76681e-d786-479c-973c-146a6d167917', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, article_vi_binding_obligation).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, reciprocal_bargain_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive the primary security benefit of widespread NNWS non-proliferation and IAEA verification. Bear the Article VI obligation to pursue nuclear disarmament in good faith, which they have not substantively fulfilled. Control the NPT review process and resist formal disarmament timelines or verification.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, nuclear_weapon_states, payer).

% Forwent the nuclear weapon option and accepted IAEA safeguards in exchange for the Article VI disarmament promise and access to peaceful nuclear technology. The disarmament promise remains largely unfulfilled after five decades. They coordinate through the Non-Aligned Movement, the New Agenda Coalition, and the TPNW to pressure NWS, but lack enforcement mechanisms.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% Administers safeguards agreements that verify NNWS compliance with non-proliferation obligations. Has no equivalent verification or enforcement mandate over NWS nuclear arsenals or disarmament progress. Its authority is derived from the NPT and member-state mandates.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Established by NNWS as a competing legal framework that prohibits nuclear weapons outright. Structurally excluded from the NPT's institutional architecture and actively opposed by NWS, but exerts normative pressure that affects NPT review conference dynamics.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_regime, excluded,
    organized, generational, constrained, global).

% Provided the 1996 advisory opinion affirming the obligation to pursue nuclear disarmament. Lacks enforcement power but supplies legal interpretation cited by NNWS to corroborate their reading of Article VI.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the global non-proliferation of nuclear weapons by creating a bargain: NNWS forgo nuclear weapons and accept verification, while NWS pursue disarmament and provide access to peaceful nuclear technology.
% TRANSFER_FUNCTION: Moves non-proliferation compliance from NNWS to NWS as a security benefit; moves a legal disarmament obligation from NWS to the international community. In practice, the transfer of restraint from NNWS to NWS is completed, while the reciprocal disarmament transfer is stalled.
% ABSENT_VOICES: TPNW adherents and humanitarian-initiative states are partially inside the NPT but marginalized in consensus-based review conferences; nuclear-dependent allies of NWS (extended deterrence recipients) are inside the regime but systematically silent on disarmament obligations; future generations exposed to nuclear risk have no representation.
% DISAPPEARANCE_RATIONALE: Without the NPT, the global non-proliferation architecture collapses: IAEA safeguards lose their legal foundation, dozens of states face renewed proliferation incentives, and the normative barrier to nuclear acquisition dissolves. The disarmament obligation would also disappear, removing the primary legal lever NNWS use to pressure NWS.
% FOUNDING_PROBLEM: Unchecked horizontal nuclear proliferation in the 1960s threatened to create a multipolar nuclear-armed world with heightened war risk and Cold War instability.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of the Eighteen Nation Committee on Disarmament corroborate the proliferation threat. However, the claim that the disarmament pillar was equally foundational is contested by NWS and corroborated by NNWS diplomatic archives, the ICJ advisory opinion, and independent nuclear historians outside the NWS beneficiary set.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.45) is moderate because the non-proliferation coordination is genuine and valuable, but the decades-long failure of NWS disarmament converts NNWS restraint into one-sided transfer. Suppression (0.55) reflects the structural suppression of NNWS proliferation alternatives via IAEA safeguards and the diplomatic cost of NPT withdrawal, not violent coercion. Theater_ratio (0.25) captures the performative cycle of Review Conference outcomes that repeatedly reaffirm disarmament commitments without implementation. Accessibility_collapse (0.60) acknowledges that NNWS alternatives (proliferation, NPT withdrawal, or reliance on the TPNW alone) are costly and partially collapsed but not eliminated. Resistance (0.50) registers both NNWS diplomatic resistance (Humanitarian Initiative, TPNW) and NWS resistance to disarmament constraints.
 *
 * PERSPECTIVAL GAP:
 *   From the NNWS seat, the constraint is a broken reciprocal bargain: they have paid with permanent restraint and received only unfulfilled promises. From the NWS seat, the constraint is a stable non-proliferation regime that legitimately retains nuclear deterrence while managing (not eliminating) arsenals. The engine will compute a high directionality (near-target) for NNWS and a low directionality (near-beneficiary) for NWS, producing divergent seat classifications despite the single treaty text.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are declared beneficiaries because they capture the security dividend of widespread NNWS non-proliferation verified by IAEA safeguards. They are simultaneously assigned secondary_role payer because Article VI nominally extracts disarmament from them, but this extraction is unrealized in practice. NNWS are declared victims/payers because their sovereign option to acquire nuclear weapons is permanently suppressed and the promised disarmament quid pro quo has not materialized. The IAEA sits as agenda_setter with constrained exit because its mandate is structurally bound to the NPT's non-proliferation pillar. The TPNW regime is excluded because it operates outside the NPT's enforcement architecture but influences its normative environment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing unchecked horizontal proliferationâhas been largely solved by the non-proliferation pillar. However, the disarmament pillar, which NNWS regard as equally foundational, has atrophied. The constraint persists because NWS continue to benefit from non-proliferation and because NNWS have not collectively withdrawn. Without the NNWS reading's active resistance (TPNW, ICJ campaign, review conference pressure), the constraint would likely degrade toward a pitonâa non-proliferation shell maintained by inertia with disarmament as theatrical residue. The NNWS reading prevents this mislabeling by insisting that disarmament is not a voluntary extra but a binding obligation, maintaining the constraint's tangled-rope character against both NWS attempts to reduce it to a pure coordination device and cynical readings that would treat it as a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a binding legal obligation with specific, reviewable conduct requirements, or an aspirational standard whose content is determined by NWS discretion?',
    'Detailed analysis of travaux prÃ©paratoires, ICJ case law, and subsequent state practice to determine whether Article VI creates justiciable duties or merely programmatic goals.',
    'If binding with specific conduct, the constraint''s extraction is dampened by reciprocity; if aspirational, the NNWS reading attempts to impose coordination on a text that structurally permits extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'Legal character of Article VI obligation').

omega_variable(
    nws_good_faith_performance,
    'Have NWS fulfilled their Article VI obligation to pursue nuclear disarmament in good faith, or has their conduct been largely performative?',
    'Empirical inventory of warhead reductions, modernization expenditures, CTBT ratification status, and FMCT negotiation blockage measured against disarmament benchmarks.',
    'Good-faith performance would lower extractiveness and theater_ratio; performative maintenance without disarmament would raise both and strengthen the piton/snare hypothesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_good_faith_performance, empirical, 'NWS compliance with Article VI good faith standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_nnws_tr_t0, npt_treaty_text__nnws_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(npt_nnws_tr_t10, npt_treaty_text__nnws_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(npt_nnws_tr_t20, npt_treaty_text__nnws_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(npt_nnws_tr_t30, npt_treaty_text__nnws_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(npt_nnws_tr_t40, npt_treaty_text__nnws_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(npt_nnws_tr_t50, npt_treaty_text__nnws_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(npt_nnws_be_t0, npt_treaty_text__nnws_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(npt_nnws_be_t10, npt_treaty_text__nnws_reading, base_extractiveness, 10, 0.32).
narrative_ontology:measurement(npt_nnws_be_t20, npt_treaty_text__nnws_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(npt_nnws_be_t30, npt_treaty_text__nnws_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(npt_nnws_be_t40, npt_treaty_text__nnws_reading, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(npt_nnws_be_t50, npt_treaty_text__nnws_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(npt_nnws_su_t0, npt_treaty_text__nnws_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(npt_nnws_su_t10, npt_treaty_text__nnws_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(npt_nnws_su_t20, npt_treaty_text__nnws_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(npt_nnws_su_t30, npt_treaty_text__nnws_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(npt_nnws_su_t40, npt_treaty_text__nnws_reading, suppression_requirement, 40, 0.53).
narrative_ontology:measurement(npt_nnws_su_t50, npt_treaty_text__nnws_reading, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the npt_treaty_text kernel, which decomposes into three structurally distinct readings: the NNWS reading (disarmament as binding obligation), the NWS reading (disarmament as aspirational), and the withdrawal_threshold_reading (Article X interpretation). The kernel label 'NPT' conflates these; the framework disambiguates them into separate constraints with distinct epsilon values and stakeholder directionalities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
