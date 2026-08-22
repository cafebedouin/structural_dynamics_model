% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__grand_bargain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__grand_bargain, []).

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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: npt_article_iv_vi_pairing__grand_bargain
 *   human_readable: NPT Article IV-VI Grand Bargain Reciprocity
 *   domain: international_law/nuclear_governance/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint instantiates the grand_bargain reading of the NPT Article
 *   IV-VI kernel. Under this reading, the two articles form a reciprocal
 *   bargain: non-weapon states forgo nuclear weapons and accept safeguards
 *   (Article IV and III) in exchange for weapon states pursuing disarmament
 *   (Article VI). Over the interval, weapon states have retained and
 *   modernized arsenals while non-weapon states remain structurally disarmed,
 *   producing an asymmetric extraction of security and strategic optionality.
 *   The constraint is actively enforced through IAEA safeguards, export
 *   control regimes, and sanctions, but the disarmament side of the bargain
 *   has stagnated.
 *
 * KEY AGENTS:
 *   - npt_weapon_states: Primary agenda-setter and beneficiary (powerful/mobile/global) â retain nuclear arsenals and benefit from NNWS restraint
 *   - nnws_treaty_parties: Primary payer (organized/constrained/global) â forgo nuclear weapons and bear safeguards costs under a breached bargain
 *   - international_atomic_energy_agency: Enforcement administrator (institutional/constrained/global) â verifies non-proliferation but not disarmament
 *   - nuclear_threshold_states: Excluded outsiders (powerful/mobile/global) â nuclear-armed but outside the treaty
 *   - disarmament_advocacy_circles: Analytical observers (organized/mobile/global) â track compliance and push for disarmament
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, 0.65).
domain_priors:suppression_score(npt_article_iv_vi_pairing__grand_bargain, 0.6).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__grand_bargain, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__grand_bargain, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__grand_bargain, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__grand_bargain, "NPT Article IV-VI Grand Bargain Reciprocity").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__grand_bargain, "international_law/nuclear_governance/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__grand_bargain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__grand_bargain, 'cb2dcd4e-fcad-4466-9b7d-764a370039d6').
narrative_ontology:cs_kernel_codification('cb2dcd4e-fcad-4466-9b7d-764a370039d6', formalized).
narrative_ontology:cs_authority_grounding('cb2dcd4e-fcad-4466-9b7d-764a370039d6', distributed).
narrative_ontology:cs_reading_relation('cb2dcd4e-fcad-4466-9b7d-764a370039d6', npt_article_iv_vi_pairing__nonproliferation_primary, influences).
narrative_ontology:cs_reading_relation('cb2dcd4e-fcad-4466-9b7d-764a370039d6', npt_article_iv_vi_pairing__abolitionist, coexists_with).
narrative_ontology:cs_axiom('cb2dcd4e-fcad-4466-9b7d-764a370039d6', foundational, reciprocal_obligation_binding).
narrative_ontology:cs_axiom_status(reciprocal_obligation_binding, holdable).
narrative_ontology:cs_axiom_grounding('cb2dcd4e-fcad-4466-9b7d-764a370039d6', reciprocal_obligation_binding, conventional).
narrative_ontology:cs_axiom('cb2dcd4e-fcad-4466-9b7d-764a370039d6', foundational, article_iv_legitimacy_conditional).
narrative_ontology:cs_axiom_status(article_iv_legitimacy_conditional, holdable).
narrative_ontology:cs_axiom_grounding('cb2dcd4e-fcad-4466-9b7d-764a370039d6', article_iv_legitimacy_conditional, conventional).
narrative_ontology:cs_reference_frame('cb2dcd4e-fcad-4466-9b7d-764a370039d6', reciprocal_bargain_equilibrium).
narrative_ontology:cs_drift_state('cb2dcd4e-fcad-4466-9b7d-764a370039d6', contemporary_nuclear_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb2dcd4e-fcad-4466-9b7d-764a370039d6', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__grand_bargain, npt_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__grand_bargain, nnws_treaty_parties).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, npt_nonproliferation_norm).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__grand_bargain, peaceful_nuclear_use_right).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear arsenals, set the NPT's interpretive frame through review conference diplomacy and Security Council action, benefit from NNWS restraint that preserves their nuclear monopoly, and resist binding disarmament timelines while claiming Article VI compliance via incremental arms reduction.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, npt_weapon_states, agenda_setter,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_article_iv_vi_pairing__grand_bargain, npt_weapon_states, beneficiary).

% Forwent nuclear weapons programs in exchange for promised disarmament and peaceful technology access; accept IAEA safeguards and export control restrictions; bear the opportunity cost of permanent strategic inferiority while weapon states modernize arsenals.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nnws_treaty_parties, payer,
    organized, generational, constrained, global).

% Administers safeguards agreements under Article III, verifies NNWS compliance with non-proliferation obligations, promotes peaceful nuclear cooperation, but lacks mandate to enforce Article VI disarmament against weapon states.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, international_atomic_energy_agency, agenda_setter,
    institutional, generational, constrained, global).

% Hold nuclear weapons outside the NPT framework or withdrew from it; are not bound by Article IV restraint or Article VI promises; their exclusion highlights the bargain's incompleteness.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, nuclear_threshold_states, excluded,
    powerful, generational, mobile, global).

% Monitor compliance with disarmament obligations, publicize modernization programs, advocate for treaty-compliant disarmament timelines, and operate as analytical observers outside the state-beneficiary structure.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__grand_bargain, disarmament_advocacy_circles, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal proliferation of nuclear weapons by offering non-weapon states access to peaceful nuclear technology in exchange for forgoing weapons, while weapon states commit to disarmament.
% TRANSFER_FUNCTION: Moves nuclear restraint, safeguards compliance, and forbearance from non-weapon states to the international community and weapon states; moves disarmament promises (but not actual disarmament) from weapon states to non-weapon states.
% ABSENT_VOICES: Non-party nuclear-armed states are structurally excluded from the bargain; abolitionist advocates who reject Article IV legitimacy entirely are marginalized in NPT review conferences.
% DISAPPEARANCE_RATIONALE: The NPT underpins the global nuclear order; its disappearance would remove the legal and normative foundation for safeguards, trigger reconsideration of nuclear options by many states, and fragment the peaceful-use trade regime.
% FOUNDING_PROBLEM: Rapid horizontal proliferation of nuclear weapons in the 1960s threatened great-power stability and regional security; simultaneous desire by non-weapon states for peaceful nuclear technology access.
% FOUNDING_PROBLEM_CORROBORATION: Historians and international legal scholars outside the NWS beneficiary bloc attest the horizontal proliferation threat was genuine in the 1960s. Civil society and NNWS diplomats contest that the disarmament problem has been addressed; independent security studies from non-beneficiary seats corroborate that vertical proliferation and modernization continue.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__grand_bargain, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__grand_bargain, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__grand_bargain, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__grand_bargain, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__grand_bargain, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__grand_bargain, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__grand_bargain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) reflects the accumulated asymmetry: NNWS have permanently forgone the nuclear option while NWS modernize. Suppression (0.60) captures the active safeguards and sanctions architecture that enforces NNWS restraint. Theater ratio (0.50) registers the growing gap between disarmament rhetoric (START, NPT RevCon outcomes) and actual modernization. Accessibility collapse (0.70) is high because the peaceful-use and non-proliferation regimes have no viable institutional alternative for NNWS. Resistance (0.55) reflects the TPNW, NNWS bloc statements, and repeated review conference gridlock. The measurement series share a single time grid to prevent misaligned substitution.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS seat, the arrangement is a functioning non-proliferation regime that preserves stability; from the NNWS seat, the same arrangement is a breached bargain where restraint is enforced but reciprocity is denied. The engine computes this divergence from the structural asymmetry in exit options (mobile vs. constrained) and the beneficiary/victim split.
 *
 * DIRECTIONALITY LOGIC:
 *   NWS are structural beneficiaries of the constraint: they collect security from NNWS non-proliferation and retain nuclear superiority. Their power (powerful) and exit (mobile) drive d toward the beneficiary end. NNWS are structural targets: they pay through permanent restraint and opportunity cost, with constrained exit and organized (but less powerful) positioning, driving d toward the target end. The IAEA and disarmament observers occupy analytical or enforcement seats without collecting extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â horizontal proliferation â is still live, so the coordination function is not dead. However, the disarmament half of the bargain has atrophied, which would push toward piton if the coordination function were gone. Because the non-proliferation function remains genuinely valued (especially by NNWS facing regional rivals), the constraint is not mere performance. Classifying it as tangled_rope captures the coexistence of real coordination and asymmetric extraction, preventing mislabeling as either pure extraction (snare) or pure coordination (rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    grand_bargain_kernel_location,
    'This constraint is the grand_bargain reading of kernel npt_article_iv_vi_pairing. What would structurally change if the nonproliferation_primary reading were adopted instead?',
    'Comparative treaty-impact assessment: if Article VI is treated as non-justiciable, does the NNWS restraint mechanism shift from conditional bargain to unconditional obligation?',
    'Would reclassify the constraint from tangled_rope toward rope or snare depending on whether the coordination function survives without reciprocity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grand_bargain_kernel_location, conceptual, 'Sibling reading structural boundary for nonproliferation_primary').

omega_variable(
    article_vi_breach_licensing_power,
    'Does breach of Article VI by weapon states legally license NNWS withdrawal or Article IV expansion, or is this a political argument without treaty mechanism?',
    'ICJ advisory opinion or arbitral ruling on the interdependence of NPT articles, or state practice analysis of withdrawal justifications.',
    'If breach licenses withdrawal, the constraint''s suppression is lower than it appears; if not, NNWS are more trapped than the conditional framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_breach_licensing_power, conceptual, 'Legal status of reciprocity under treaty law').

omega_variable(
    npt_enforcement_asymmetry,
    'Is the NPT enforcement architecture applied symmetrically to weapon-state disarmament breaches and non-weapon-state proliferation breaches?',
    'Comparative analysis of IAEA Board findings, UNSC resolutions, and sanctions episodes targeting NNWS vs. NWS non-compliance.',
    'Asymmetric enforcement confirms the tangled_rope classification; symmetric enforcement would support a rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npt_enforcement_asymmetry, empirical, 'Enforcement symmetry between Article VI and Article III breaches').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__grand_bargain, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1980, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1980, 0.22).
narrative_ontology:measurement(npt__tr_t1990, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2010, 0.48).
narrative_ontology:measurement(npt__tr_t2020, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2020, 0.54).
narrative_ontology:measurement(npt__tr_t2025, npt_article_iv_vi_pairing__grand_bargain, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(npt__be_t1980, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1980, 0.36).
narrative_ontology:measurement(npt__be_t1990, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 1990, 0.44).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2000, 0.52).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(npt__be_t2020, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement(npt__be_t2025, npt_article_iv_vi_pairing__grand_bargain, base_extractiveness, 2025, 0.65).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_article_iv_vi_pairing__grand_bargain, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__grand_bargain, npt_article_iv_vi_pairing__abolitionist).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the NPT Article IV-VI kernel. The grand_bargain reading treats the articles as reciprocal obligations; the nonproliferation_primary reading treats Article VI as aspirational; the abolitionist reading treats Article IV as illegitimate. Each reading instantiates a distinct constraint with its own epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
