% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT as Enforced Nuclear Oligopoly (Articles I-II Binding, Article VI Aspirational)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the oligopoly-enforcement reading of the NPT
 *   kernel: Articles I-II function as the treaty's operative, verified,
 *   sanctionable core, while Article VI's disarmament language is treated (by
 *   this reading) as contingent, unscheduled, and practically unenforceable.
 *   Under this reading the treaty is a Tangled Rope — it solves a real
 *   coordination problem (reducing independent nuclear command authorities)
 *   but does so through a structure that concentrates verification burden
 *   entirely on NNWS while leaving the P5's own weapons status legally
 *   entrenched and practically unverifiable. Threshold states facing acute
 *   regional threats become an identifiable victim class: their deterrent
 *   option is foreclosed by the same instrument that leaves the states
 *   threatening them (P5 nuclear powers, or non-signatories) unconstrained.
 *   This is ONE reading of the NPT kernel; the reciprocal_disarmament_reading
 *   and withdrawal_sovereignty_reading are separate constraints with their
 *   own ε and stakeholder structures, linked via network.affects_constraints,
 *   not merged into this file.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.72).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT as Enforced Nuclear Oligopoly (Articles I-II Binding, Article VI Aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '209fc038-adf4-46a4-8251-16229e75f269').
narrative_ontology:cs_kernel_codification('209fc038-adf4-46a4-8251-16229e75f269', fixed_text).
narrative_ontology:cs_authority_grounding('209fc038-adf4-46a4-8251-16229e75f269', extraction).
narrative_ontology:cs_interpretation_layer_present('209fc038-adf4-46a4-8251-16229e75f269').
narrative_ontology:cs_reading_relation('209fc038-adf4-46a4-8251-16229e75f269', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('209fc038-adf4-46a4-8251-16229e75f269', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('209fc038-adf4-46a4-8251-16229e75f269', foundational, horizontal_nonproliferation_is_primary_treaty_object).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_is_primary_treaty_object, holdable).
narrative_ontology:cs_axiom_grounding('209fc038-adf4-46a4-8251-16229e75f269', horizontal_nonproliferation_is_primary_treaty_object, conventional).
narrative_ontology:cs_axiom('209fc038-adf4-46a4-8251-16229e75f269', foundational, article_vi_is_non_self_executing_aspiration).
narrative_ontology:cs_axiom_status(article_vi_is_non_self_executing_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('209fc038-adf4-46a4-8251-16229e75f269', article_vi_is_non_self_executing_aspiration, conventional).
narrative_ontology:cs_axiom('209fc038-adf4-46a4-8251-16229e75f269', secondary, nuclear_status_quo_reduces_systemic_risk).
narrative_ontology:cs_axiom_status(nuclear_status_quo_reduces_systemic_risk, holdable).
narrative_ontology:cs_axiom_grounding('209fc038-adf4-46a4-8251-16229e75f269', nuclear_status_quo_reduces_systemic_risk, instrumental).
narrative_ontology:cs_reference_frame('209fc038-adf4-46a4-8251-16229e75f269', id_1968_five_state_weapons_freeze_bargain).
narrative_ontology:cs_drift_state('209fc038-adf4-46a4-8251-16229e75f269', post_1995_indefinite_extension_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('209fc038-adf4-46a4-8251-16229e75f269', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_apparatus).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_industry_suppliers).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_state_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_denied_deterrent).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, civilian_nuclear_program_operators_under_intrusive_inspection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and modernize their arsenals while Article II binds every other party to permanent non-acquisition. They chair the review conferences, control Security Council enforcement referrals, and face no comparable inspection regime themselves. Article VI's disarmament language imposes no schedule, no verification body, and no penalty for its own indefinite deferral.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter).

% Accept comprehensive IAEA safeguards on their entire civilian nuclear sector, submit to intrusive inspection regimes, and forgo any weapons pathway permanently, in exchange for a disarmament promise from the P5 that carries no enforceable timeline. Leaving the treaty triggers diplomatic isolation and loss of access to civilian nuclear technology and fuel supply.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_state_parties, payer,
    moderate, generational, constrained, global).

% States facing acute regional security threats from nuclear-armed or nuclear-capable neighbors are structurally barred from acquiring their own deterrent while the neighbors that either stayed outside the treaty or belong to the P5 face no equivalent restraint. Their security calculus is frozen by a regime that treats their acquisition as the primary threat and the P5's retention as the stabilizing baseline.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_denied_deterrent, payer,
    moderate, generational, trapped, regional).

% Administers the verification regime that operationalizes Articles I-II, drawing budget, mandate, and institutional authority from the asymmetric inspection burden it imposes on NNWS. Has no comparable mandate or access to verify Article VI progress among the P5, and has institutionalized around the horizontal-only enforcement function.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_apparatus, beneficiary).

% Nuclear Suppliers Group members and established civilian nuclear technology exporters benefit from a regime that channels all civilian nuclear trade through NPT-compliant, safeguarded pathways, locking in their market position against new entrants who lack access to enrichment or reprocessing technology under the supplier cartel's export control terms.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_industry_suppliers, beneficiary,
    organized, generational, arbitrage, global).

% Domestic nuclear facility operators and national regulatory agencies in NNWS bear the operational cost and sovereignty intrusion of comprehensive safeguards agreements and additional protocols, with no reciprocal inspection access into P5 weapons production or disarmament facilities to verify Article VI performance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, civilian_nuclear_program_operators_under_intrusive_inspection, payer,
    powerless, biographical, trapped, national).

% Receives IAEA non-compliance referrals and can authorize sanctions or enforcement action against NNWS violators, but the P5 hold veto power over any Security Council action — including any conceivable enforcement action against themselves for Article VI non-performance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, un_security_council, agenda_setter).

% Have argued at every review conference since 1995 that the treaty's bargain is being read one-sidedly, that Article VI must carry binding force with verification, and that the indefinite extension of 1995 was conditioned on disarmament progress that never materialized. Their objections are recorded in final documents but carry no enforcement mechanism and are structurally unable to trigger consequences for the P5.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_aligned_movement_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single verifiable global standard preventing the spread of nuclear weapons to additional states, reducing the number of independent nuclear command authorities and the associated first-use, accident, and escalation risks — a genuine collective-action problem that a purely unilateral non-proliferation policy could not solve.
% TRANSFER_FUNCTION: Moves sovereignty over nuclear weapons acquisition, inspection access, and civilian nuclear sector autonomy from non-nuclear-weapon states to the IAEA verification apparatus and, indirectly, to the P5 states whose weapons status the treaty entrenches; no comparable transfer runs from the P5 to any external verifying authority.
% ABSENT_VOICES: Threshold states facing existential regional threats (whose deterrent aspirations are treated as the treaty's core problem) have no seat that can renegotiate the bargain; non-aligned states raise the enforcement-asymmetry objection at every review conference but have no mechanism to compel P5 compliance or to renegotiate Article VI's non-binding language.
% DISAPPEARANCE_RATIONALE: If Articles I-II enforcement vanished overnight, the safeguards apparatus, the nuclear suppliers cartel's export-control leverage, and the P5's unique legal status as the only lawful nuclear-weapon possessors would all lose their institutional basis simultaneously; several threshold states would very plausibly move toward overt weapons programs within a decade, and the entire nonproliferation-industrial complex (IAEA safeguards budgets, supplier-group licensing regimes) would need to reconstitute on a different footing or dissolve.
% FOUNDING_PROBLEM: In 1968, an expanding number of states were approaching nuclear weapons capability; the treaty was built to freeze the number of nuclear-weapon states at five and channel all other nuclear activity through a verifiable civilian-only pathway, with Article VI's disarmament language added to secure NNWS assent to a bargain that was otherwise entirely one-directional.
% FOUNDING_PROBLEM_CORROBORATION: The P5 and the IAEA secretariat attest the horizontal proliferation problem remains fully live and justifies the current enforcement asymmetry. Independent sources outside the beneficiary set — the 1996 ICJ Advisory Opinion on the Legality of Nuclear Weapons (finding an obligation to pursue negotiations in good faith to a conclusion), successive NAM and New Agenda Coalition review-conference statements, and independent arms-control research bodies (e.g. SIPRI) documenting continued P5 arsenal modernization — corroborate that the reciprocal half of the founding bargain (Article VI) has not been treated as live by the parties positioned to perform it.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68 by 2024) reflects the widening gap between an intensifying, technologically sophisticated safeguards/verification burden on NNWS (Additional Protocol, comprehensive safeguards agreements) and the complete absence of any comparable verification mechanism applied to P5 arsenal reduction. Suppression (0.72) is high because exit is costly: withdrawal under Article X triggers diplomatic isolation, loss of fuel-cycle access, and is itself treated as evidence of proliferation intent, which forecloses the exit route this reading's sibling (withdrawal_sovereignty_reading) treats as a live sovereign option. Theater ratio (0.40) captures the review-conference cycle: quinquennial conferences produce extensive Article VI language and 'action plans' (1995, 2000, 2010) that generate significant diplomatic activity without binding verification machinery — a growing proxy-goal substitution over time.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the treaty is functioning exactly as designed: a stable, legally entrenched status quo that has in fact reduced (compared to 1960s projections) the number of nuclear-weapon states. From the threshold-state and NNWS seats under this reading, the same structure is an enforced asymmetry that treats their security anxiety as the problem to be verified against while treating the P5's retained arsenals as the stabilizing baseline requiring no verification. The engine computes these as structurally different classifications from the same base data; this reading does not average them.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states derive d near the full-beneficiary end: they retain the constrained good (nuclear weapons status) permanently, chair enforcement mechanisms, and hold Security Council veto insulating them from any consequence for Article VI non-performance. NNWS and threshold states derive d near the full-target end: they bear the entire verification burden, forgo the constrained good permanently, and have constrained-to-trapped exit options (regional threshold states especially, since their non-acquisition is not matched by any change in the threat environment). The IAEA and nuclear suppliers occupy a beneficiary-adjacent position: they gain institutional mandate and market position from the horizontal-only enforcement architecture, independent of whether Article VI is ever performed.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope (rather than snare) preserves the genuine coordination function — reducing the number of independent nuclear weapons states is a real global public good this reading does not dispute. What prevents mislabeling it as pure coordination (rope) is the requirement that Article II obligations bind permanently and asymmetrically while Article VI carries no comparable binding force in THIS reading's own account — that asymmetry is the extraction the coordination function is riding on. The founding_problem mismatch check is central here: founding_problem_status is authored as contested rather than dead precisely because the P5 (an interested party) attest the horizontal problem remains fully live, while independent corroboration (ICJ 1996, NAM statements, SIPRI arsenal data) documents that the reciprocal half of the bargain was never treated as live by those positioned to perform it — a status=contested + verdict=world_rearranges profile that should register as a partial-capture flag rather than either full mandatrophy resolution or full legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_ambiguity,
    'Is Article VI a binding legal obligation with an implied performance timeline (as the ICJ''s 1996 Advisory Opinion and the reciprocal_disarmament_reading hold), or a contingent, aspirational, non-self-executing provision whose performance is left to P5 discretion (as this reading holds)?',
    'A binding ICJ contentious-case ruling (rather than advisory opinion) on Article VI performance, or a negotiated verification protocol for disarmament comparable in intrusiveness to IAEA comprehensive safeguards, would resolve which reading better describes the treaty''s operative legal structure.',
    'If Article VI is authoritatively established as binding with enforceable timeline, the coordination/extraction split this reading identifies collapses substantially — the treaty would read closer to a genuine reciprocal rope. If confirmed as aspirational, this reading''s tangled_rope classification is reinforced and the asymmetry becomes the treaty''s stable structural feature rather than a temporary imbalance awaiting correction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_status_ambiguity, conceptual, 'Whether Article VI carries binding force comparable to Articles I-II — the central interpretive fork between this reading and reciprocal_disarmament_reading.').

omega_variable(
    threshold_state_victim_status_ambiguity,
    'Are threshold states genuinely denied a legitimate security option by the treaty''s horizontal-only enforcement (this reading''s premise), or does the treaty''s stabilizing effect on the broader nuclear order reduce their actual security risk more than an unconstrained regional arms race would (the coordination-benefit counter-reading)?',
    'Comparative regional security analysis: do threshold states in regions where NPT enforcement is strong (e.g., Northeast Asia pre-DPRK-withdrawal, Middle East) exhibit measurably worse security outcomes than counterfactual unconstrained-acquisition scenarios? This requires case-by-case regional security modeling, which is contested and not fully resolvable empirically.',
    'If threshold states are net beneficiaries of the broader stability effect despite the individual deterrent-denial, their inclusion in the victim set overstates the constraint''s extractiveness. If they are net losers under plausible counterfactuals, their victim-set inclusion is well-founded and the extractiveness score is conservative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_state_victim_status_ambiguity, empirical, 'Whether threshold-state deterrent denial is a net cost or is offset by systemic stability benefits.').

omega_variable(
    p5_accountability_gap_permanence,
    'Is the absence of verification machinery for P5 arsenal reduction a permanent structural feature of the treaty''s design (as this reading treats it), or a temporary institutional gap that New START-style bilateral verification and future multilateral frameworks could eventually close?',
    'Track whether any multilateral (not merely bilateral US-Russia) verification framework for P5 disarmament is negotiated and implemented within a defined future window; absence after a further multi-decade period would support permanence.',
    'If closed, the enforcement asymmetry this reading centers becomes a historical phase rather than the treaty''s stable equilibrium, weakening the tangled_rope classification toward scaffold (temporary asymmetry pending institution-building) or rope. If it persists indefinitely, the tangled_rope reading is reinforced as the treaty''s steady state.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_accountability_gap_permanence, empirical, 'Whether the P5 verification gap is a transitional institutional deficit or a permanent structural feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1968, 0.15).
narrative_ontology:measurement(npt__tr_t1978, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1968, 0.42).
narrative_ontology:measurement(npt__be_t1978, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1978, 0.48).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1990, 0.53).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2005, 0.61).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1968, 0.5).
narrative_ontology:measurement(npt__su_t1978, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1978, 0.55).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.63).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2005, 0.67).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2015, 0.7).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposed from the single natural-language label 'the NPT' (npt_treaty_1970 kernel), per the ε-invariance principle: measuring the treaty by Article I-II enforcement outcomes yields one ε (this file, 0.68, tangled_rope); measuring it by Article VI performance outcomes yields a structurally distinct constraint (reciprocal_disarmament_reading, expected higher ε and a different victim set centered on NNWS as parties to an unperformed bargain rather than threshold states as denied-deterrent victims); measuring it by Article X withdrawal legitimacy yields a third distinct constraint (withdrawal_sovereignty_reading). These are not the same constraint viewed from three angles — they have different beneficiary/victim structures and would each fail the ε-invariance test if collapsed into one file. Linked here via affects_constraints; each carries its own cs_structure.reading_relations back to this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
