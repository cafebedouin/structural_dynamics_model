% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: NPT as Horizontal Nonproliferation Enforcement Regime (Articles I-II Primary, Article VI Aspirational)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the oligopoly-enforcement reading of the NPT
 *   kernel: Articles I-II (the horizontal nonproliferation obligations
 *   binding NNWS to renounce weapons and accept safeguards) are read as the
 *   treaty's primary, operative, enforced core, while Article VI (the P5
 *   pledge to pursue disarmament negotiations) is read as contingent,
 *   aspirational, and non-binding language with no verification or timetable
 *   attached. Under this reading the treaty is a hybrid structure: it
 *   genuinely coordinates against a real collective-action problem
 *   (uncontrolled weapons proliferation) while simultaneously operating an
 *   asymmetric extraction — NNWS bear escalating inspection and sovereignty
 *   costs, threshold states are locked out of the deterrent option available
 *   to the P5, and the P5 and supplier-state industries retain the benefits
 *   of the weapons monopoly and the technology-gatekeeping position with no
 *   comparable enforcement burden of their own. This is emphatically NOT the
 *   reciprocal_disarmament_reading (which holds Article VI as an equally
 *   binding, temporally urgent obligation forming one side of a genuine
 *   bargain) nor the withdrawal_sovereignty_reading (which centers Article X
 *   as the treaty's real safety valve). Each of those readings is a separate
 *   constraint, authored separately, with its own epsilon.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: institutional beneficiary and agenda-setter — retains arsenal, sets NSG/IAEA terms, faces no enforcement
 *   - non_nuclear_weapon_state_parties: moderate-power payer — accepts safeguards burden, gets aspirational promise in return
 *   - threshold_capable_states: trapped payer — denied deterrent parity with non-signatory nuclear states
 *   - iaea_safeguards_bureaucracy: institutional beneficiary — derives mandate from asymmetric inspection regime
 *   - established_nuclear_industry_suppliers: organized beneficiary — controls civilian technology access via NSG
 *   - disarmament_advocacy_states_and_ngos: excluded — raises Article VI claims with no enforcement mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.78).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT as Horizontal Nonproliferation Enforcement Regime (Articles I-II Primary, Article VI Aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'd55c6f8d-90d1-450e-abfb-1c0ac3fd24c3').
narrative_ontology:cs_kernel_codification('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', fixed_text).
narrative_ontology:cs_authority_grounding('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', extraction).
narrative_ontology:cs_interpretation_layer_present('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3').
narrative_ontology:cs_reading_relation('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', foundational, horizontal_proliferation_is_the_primary_treaty_object).
narrative_ontology:cs_axiom_status(horizontal_proliferation_is_the_primary_treaty_object, holdable).
narrative_ontology:cs_axiom_grounding('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', horizontal_proliferation_is_the_primary_treaty_object, conventional).
narrative_ontology:cs_axiom('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', foundational, article_vi_creates_no_enforceable_obligation_of_result).
narrative_ontology:cs_axiom_status(article_vi_creates_no_enforceable_obligation_of_result, holdable).
narrative_ontology:cs_axiom_grounding('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', article_vi_creates_no_enforceable_obligation_of_result, conventional).
narrative_ontology:cs_reference_frame('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', asymmetric_enforcement_baseline_1970).
narrative_ontology:cs_drift_state('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', post_2015_review_conference_breakdown, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d55c6f8d-90d1-450e-abfb-1c0ac3fd24c3', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_bureaucracy).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, established_nuclear_industry_suppliers).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_state_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, civilian_nuclear_energy_developing_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain and modernize nuclear arsenals while facing no binding verification, inspection, or timetable under Article VI, which is read as an aspirational pursuit-of-negotiations clause rather than an enforceable disarmament obligation. They chair the Security Council permanent seats, control IAEA Board dynamics through funding and diplomatic weight, and set the terms of export control regimes (NSG) that determine which states get civilian nuclear technology. Their exit from any obligation is structurally guaranteed by the treaty's own asymmetric design — there is no reciprocal inspection regime that could constrain them the way IAEA safeguards constrain non-nuclear parties.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter).

% Accept comprehensive IAEA safeguards, routine and special inspections, and permanent renunciation of nuclear weapons in exchange for a promise of eventual disarmament negotiations and nominal access to civilian nuclear cooperation. Fifty-plus years after signature, the inspection burden has only grown (Additional Protocol, more intrusive verification) while Article VI has produced no binding disarmament timetable. Exit means withdrawal under Article X, which triggers diplomatic isolation, sanctions risk, and loss of civilian technology cooperation — a cost most cannot absorb.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_state_parties, payer,
    moderate, generational, constrained, global).

% States with the technical capacity to develop weapons but formally committed as NNWS parties face a treaty structure that denies them the deterrent value the P5 retain, while regional rivals outside the treaty (or that withdrew) face no equivalent constraint. Under this reading, the treaty's function is understood as locking in a permanent capability gap rather than as a genuine step toward a weapons-free world, since the P5's Article VI obligation is not enforced. Their security calculus is structurally worsened relative to non-signatory nuclear-armed neighbors.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_states, payer,
    moderate, generational, trapped, national).

% Rely on Article IV's promise of assistance with peaceful nuclear technology but find civilian cooperation gated by Nuclear Suppliers Group export controls set by the same states that benefit from the arms monopoly. Full-scope safeguards and NSG conditions raise the practical cost and delay of any civilian program, while the promised 'inalienable right' to peaceful technology is administered as a discretionary grant by supplier states rather than a guaranteed entitlement.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, civilian_nuclear_energy_developing_states, payer,
    powerless, biographical, constrained, national).

% Administers and expands the verification apparatus that gives Articles I-II operational force, deriving budget, mandate, and institutional relevance from the ever-intensifying inspection regime applied to NNWS. Has no equivalent enforcement mandate over NWS arsenal reduction or verification, since Article VI created no comparable inspection body. Its institutional survival depends on the horizontal-proliferation-first framing remaining dominant.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_bureaucracy, agenda_setter).

% Firms and state agencies in NWS and allied supplier states control enrichment, reprocessing, and reactor export markets, using NSG guidelines (justified by nonproliferation logic) to maintain competitive advantage over would-be new entrants in the civilian nuclear sector. They benefit commercially from the same asymmetric technology-control structure that Article IV nominally promises to democratize.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, established_nuclear_industry_suppliers, beneficiary,
    organized, generational, arbitrage, global).

% States in coalitions like the New Agenda Coalition and NGOs pressing for binding disarmament timetables raise Article VI at every Review Conference but have no enforcement mechanism, no veto, and no seat with power comparable to the P5's. Their objections are recorded in final documents that are routinely not implemented, and this reading of the treaty treats their claim — that Article VI is co-equal and binding — as a misreading of the treaty's actual operative core.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, disarmament_advocacy_states_and_ngos, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely coordinates against the spread of nuclear weapons capability to new state actors, reducing the number of nuclear-armed states below what an uncontrolled proliferation cascade would likely have produced, and creates a verification infrastructure (IAEA safeguards) that gives other states assurance about NNWS civilian programs.
% TRANSFER_FUNCTION: Moves sovereignty over weapons-development choice, inspection access, and technological latitude from non-nuclear-weapon states to the P5 and the safeguards bureaucracy they oversee, in exchange for a non-binding aspiration toward eventual P5 disarmament and gated access to civilian nuclear cooperation.
% ABSENT_VOICES: Threshold states and disarmament-coalition states object loudest at Review Conferences that Article VI is being read as aspirational rather than obligatory, but they hold no mechanism to force reinterpretation; non-signatory nuclear states (never bound at all) are entirely outside this treaty's conversation while shaping the security environment NNWS parties must live in.
% DISAPPEARANCE_RATIONALE: If Articles I-II enforcement (safeguards, export controls, verification) vanished overnight, several threshold-capable states would plausibly move toward weapons programs within a decade, the IAEA's core institutional mandate would dissolve, and the NSG's export-control rationale would collapse — a substantial reorganization of the nuclear order, even though the underlying physics and weapons technology would be unchanged.
% FOUNDING_PROBLEM: In the late 1960s, a rapid multiplication of nuclear-weapon states appeared imminent and dangerous; the treaty was built to freeze the weapons club at five members while offering NNWS a bargain of security assurance, civilian technology access, and an eventual path to disarmament.
% FOUNDING_PROBLEM_CORROBORATION: P5 governments and IAEA officials attest the horizontal-proliferation problem remains the primary live threat, citing Iran, North Korea, and past covert programs (Iraq, Libya, Syria) as vindication. Independent arms-control scholars, the International Court of Justice's 1996 advisory opinion, and non-P5 Review Conference delegations attest that the reciprocal bargain's disarmament half has gone substantially unaddressed for over fifty years, which is corroboration from outside the P5 beneficiary set that this reading's Article VI framing, while textually arguable, has produced a persistent enforcement asymmetry rather than a completed bargain.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.71 at 2025) reflects that the inspection/verification burden on NNWS has intensified (Additional Protocol regime post-1997) while zero comparable verification exists for P5 arsenal reduction — the asymmetry has widened, not narrowed, over the interval. Suppression (0.78) is high because exit (Article X withdrawal) carries severe diplomatic and economic costs, and the treaty's own design gives the P5 no reciprocal exposure to comparable suppression. Theater ratio (0.32) is moderate: safeguards inspections are functionally real (they do detect undeclared programs), but a growing share of Review Conference activity — reaffirming Article VI 'commitment' with no implementation — is performative under this reading. All three temporal series share the single 1970-2025 grid.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states and the safeguards/supplier institutions they anchor sit near the full-beneficiary end: they collect the stability and monopoly benefits of the arrangement and bear none of its enforcement costs under this reading. NNWS and threshold states sit near the full-target end: mandatory safeguards, technology gatekeeping, and denial of deterrent parity are costs imposed with only aspirational compensation. Civilian-energy developing states are victims through a slower channel — gatekept technology access rather than direct security cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled 1960s-era proliferation cascade) remains partly live — this is why the constraint is tangled_rope rather than pure snare: there is a real, still-operative coordination function preventing weapons spread. But the R5 corroboration shows the OTHER half of the original bargain (Article VI disarmament) has gone substantially unaddressed for over fifty years by non-P5 attestation, meaning the mandate for treating horizontal nonproliferation as the sole enforceable core, while vertical disarmament stays aspirational, has outlived any temporary justification it may have had at signature. Classifying this as tangled_rope rather than mountain or pure rope prevents two errors: treating the asymmetry as natural/inevitable (mountain framing, which the oligopoly reading itself sometimes uses rhetorically), and treating the whole treaty as pure extraction (snare framing, which would erase the genuine proliferation-prevention coordination it still performs).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_status_ambiguity,
    'Is Article VI a binding legal obligation of result (as the ICJ''s 1996 advisory opinion characterized ''an obligation to pursue in good faith and bring to a conclusion negotiations'') or a hortatory best-efforts clause with no enforceable content, as this reading holds?',
    'Authoritative international judicial ruling with binding force (as opposed to advisory opinion), or a Review Conference consensus document that establishes concrete verification/timetable mechanisms for Article VI comparable to Article III safeguards.',
    'If Article VI is authoritatively established as co-equal and binding, the treaty''s classification shifts substantially toward the reciprocal_disarmament_reading''s structure — the P5 would become payers, not pure beneficiaries, and the tangled_rope''s extraction component would shrink or vanish into a genuinely reciprocal rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_status_ambiguity, conceptual, 'Whether Article VI carries binding legal force equal to Articles I-II, unresolved across five decades of Review Conferences.').

omega_variable(
    counterfactual_proliferation_baseline,
    'Absent the NPT''s horizontal enforcement, how many additional states would plausibly have acquired nuclear weapons by 2025, and does that counterfactual justify the enforcement asymmetry as a lesser evil?',
    'Comparative case analysis of near-nuclear states that did not sign or withdrew (India, Pakistan, Israel, North Korea) versus signatory states that abandoned programs (South Africa, Libya, several post-Soviet states), controlling for regional security dynamics.',
    'A high counterfactual proliferation baseline would strengthen the coordination-function half of the tangled_rope reading; a low baseline would suggest the enforcement asymmetry extracts sovereignty cost disproportionate to the proliferation risk actually averted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_proliferation_baseline, empirical, 'Whether the horizontal-enforcement regime averted proliferation that would otherwise have occurred, or extracted cost against a smaller counterfactual risk.').

omega_variable(
    reading_framing_underdetermination,
    'Is the NPT kernel more accurately read through its enforced operative text (favoring this reading) or through its negotiating history and preambular bargain structure (favoring the reciprocal_disarmament_reading)?',
    'This is a conceptual framing question, not empirically resolvable: the treaty''s text supports the primary-obligation reading of Articles I-II (which alone carry verification machinery), while its negotiating history and preamble support the reciprocal-bargain reading. Both are internally coherent given different interpretive priors about whether enforcement mechanisms or negotiating intent determine a treaty''s operative core.',
    'Choosing the text-and-enforcement framing (this story) yields tangled_rope with P5 as concentrated beneficiaries; choosing the negotiating-history framing yields a different constraint (reciprocal_disarmament_reading) with a different epsilon and beneficiary/victim structure entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Alternative framing note: text-enforcement primacy (this reading) versus negotiating-history bargain primacy (sibling reading) as two coherent but structurally distinct bases for classifying the same treaty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.24).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2015, 0.3).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.32).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.48).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1985, 0.55).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_suppliers_group_export_controls).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_additional_protocol_regime).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the npt_treaty_1970 kernel, each authored as an independent constraint with its own epsilon per the epsilon-invariance principle. oligopoly_enforcement_reading (this file) reads Articles I-II as primary/enforced and Article VI as aspirational, producing tangled_rope with concentrated P5/IAEA/supplier beneficiaries and NNWS/threshold-state victims. reciprocal_disarmament_reading treats Article VI as co-equal and binding, which would substantially lower measured extraction and reposition NWS as payers under a genuine bargain. withdrawal_sovereignty_reading centers Article X and treats the whole obligation structure as contingent on security conditions rather than fixed, which changes the suppression/exit analysis entirely. Do not average epsilon across these three files; each is a separate constraint linked here for contamination-propagation analysis only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
