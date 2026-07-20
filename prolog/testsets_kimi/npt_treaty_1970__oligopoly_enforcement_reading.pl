% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: NPT Oligopoly Enforcement Reading (Articles I-II binding; Article VI aspirational)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty of 1970 is a contested kernel whose
 *   text supports multiple structurally distinct readings. This constraint
 *   story instantiates the oligopoly enforcement reading: Articles I-II
 *   (horizontal nonproliferation) are treated as the treaty's primary,
 *   binding, and actively enforced obligations, while Article VI
 *   (disarmament) is rendered contingent and aspirational. Under this
 *   reading, the treaty functions not as a reciprocal bargain but as an
 *   enforcement mechanism that legitimates a P5 nuclear oligopoly.
 *   Non-nuclear weapon states bear intrusive inspections and technology
 *   denial; threshold states are denied deterrent status; and the P5 capture
 *   strategic status rents with no accountability timeline. The constraint
 *   coordinates genuine nonproliferation while asymmetrically extracting
 *   security status and sovereignty autonomy from the non-nuclear majority.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: Primary beneficiary/agenda setter (institutional/arbitrage) â captures oligopolistic security rents and controls enforcement priorities through the Security Council and Review Conference structure.
 *   - non_nuclear_weapon_states: Primary payer (organized/constrained) â bears intrusive safeguards burden and forgoes nuclear option without receiving binding disarmament reciprocity.
 *   - threshold_states: Secondary payer (moderate/constrained) â denied deterrent status despite advanced capability; subject to technology denial and acute inspection scrutiny.
 *   - iaea_verification_regime: Agenda setter (institutional/constrained) â administers asymmetric enforcement of Articles I-II; mandate does not extend to verifying P5 Article VI compliance.
 *   - nonproliferation_advocacy_complex: Observer (organized/analytical) â frames the treaty as successful coordination, often backgrounding the enforcement asymmetry.
 *   - excluded_security_guarantee_seekers: Excluded (moderate/trapped) â demands for negative security guarantees and equitable enforcement are structurally marginalized in treaty discourse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.75).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.78).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading (Articles I-II binding; Article VI aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'f888490a-da93-4b99-98f3-e9f3e9a50ada').
narrative_ontology:cs_kernel_codification('f888490a-da93-4b99-98f3-e9f3e9a50ada', formalized).
narrative_ontology:cs_authority_grounding('f888490a-da93-4b99-98f3-e9f3e9a50ada', extraction).
narrative_ontology:cs_interpretation_layer_present('f888490a-da93-4b99-98f3-e9f3e9a50ada').
narrative_ontology:cs_reading_relation('f888490a-da93-4b99-98f3-e9f3e9a50ada', npt_treaty_1970__reciprocal_disarmament_reading, forecloses).
narrative_ontology:cs_reading_relation('f888490a-da93-4b99-98f3-e9f3e9a50ada', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('f888490a-da93-4b99-98f3-e9f3e9a50ada', foundational, article_vi_as_aspirational_nonbinding).
narrative_ontology:cs_axiom_status(article_vi_as_aspirational_nonbinding, holdable).
narrative_ontology:cs_axiom_grounding('f888490a-da93-4b99-98f3-e9f3e9a50ada', article_vi_as_aspirational_nonbinding, conventional).
narrative_ontology:cs_axiom('f888490a-da93-4b99-98f3-e9f3e9a50ada', foundational, p5_oligopoly_as_stabilizing_imperative).
narrative_ontology:cs_axiom_status(p5_oligopoly_as_stabilizing_imperative, holdable).
narrative_ontology:cs_axiom_grounding('f888490a-da93-4b99-98f3-e9f3e9a50ada', p5_oligopoly_as_stabilizing_imperative, instrumental).
narrative_ontology:cs_reference_frame('f888490a-da93-4b99-98f3-e9f3e9a50ada', p5_strategic_oligopoly).
narrative_ontology:cs_drift_state('f888490a-da93-4b99-98f3-e9f3e9a50ada', contemporary_multipolar_challenges, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f888490a-da93-4b99-98f3-e9f3e9a50ada', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_oligopoly_legitimacy).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, indefinite_extension_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear weapons legally under the treaty and sit as permanent members of the Security Council. They set the nonproliferation enforcement agenda, resist binding disarmament timelines, and derive strategic status and oligopolistic security benefits from preventing horizontal proliferation. Their modernization programs proceed while Article VI is treated as aspirational.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% Comprise the vast majority of treaty parties. They accept comprehensive safeguards, intrusive inspections, and technology denial regimes. They bear sovereignty costs and forgo the nuclear deterrent option without receiving commensurate security guarantees or verified disarmament progress from the P5. Withdrawal under Article X is legally possible but diplomatically and economically catastrophic.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).

% Maintain advanced nuclear fuel-cycle capabilities and face acute security dilemmas. They are denied the deterrent status afforded to the P5 and subjected to intensified technology denial and inspection scrutiny. Their security concerns are cited as proliferation risks rather than legitimate defense needs, locking them into a subordinate tier.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, constrained, regional).

% Administers safeguards and monitoring under Articles I-II. Its authority, budget, and organizational survival depend on the treaty regime. It applies intrusive verification measures to NNWS and threshold states but possesses no mandate to verify P5 disarmament compliance under Article VI, replicating the treaty asymmetry in its operational posture.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_verification_regime, agenda_setter,
    institutional, generational, constrained, global).

% Includes think tanks, NGOs, and diplomatic professionals who frame the NPT as a successful global coordination regime. They observe the structural asymmetry but typically background it, emphasizing the treaty's success in limiting the number of nuclear-armed states rather than the enforcement costs borne by the non-nuclear majority.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nonproliferation_advocacy_complex, observer,
    organized, generational, analytical, global).

% States and movements demanding binding negative security guarantees, equitable enforcement, and a nuclear weapons convention. They participate in Review Conferences but are structurally marginalized from agenda-setting; their proposals are relegated to non-operational preambular language or rejected as incompatible with the P5 security architecture.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, excluded_security_guarantee_seekers, excluded,
    moderate, biographical, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the horizontal spread of nuclear weapons by establishing a technology-denial and verification architecture that reduces the risk of nuclear war, accidental use, and destabilizing regional arms races.
% TRANSFER_FUNCTION: Moves security status, strategic leverage, and sovereignty costs from non-nuclear weapon states and threshold states to the five recognized nuclear-weapon states, who retain exclusive deterrent capability while the non-nuclear parties absorb the enforcement burden and forgo the weapons option.
% ABSENT_VOICES: States demanding binding negative security guarantees, threshold states seeking recognition of their security dilemmas, and proponents of a nuclear weapons convention are present in discourse but structurally excluded from setting enforcement priorities; their agenda is confined to aspirational Article VI language and ignored in operational enforcement decisions.
% DISAPPEARANCE_RATIONALE: If the oligopoly enforcement structure vanished, the safeguards and technology-control architecture would collapse, threshold states would likely pursue deterrent capabilities, and the P5's exclusive strategic status would erode. The absence of Article VI enforcement would no longer be masked by the coercion of Articles I-II.
% FOUNDING_PROBLEM: The rapid horizontal proliferation of nuclear weapons in the 1960s threatened to distribute nuclear capability to dozens of states, raising the risk of nuclear war, accidental use, and destabilizing regional arms races.
% FOUNDING_PROBLEM_CORROBORATION: Independent security studies scholars and Global South diplomatic coalitions attest that horizontal proliferation was the founding crisis; however, these same outside observers contest that the current asymmetry is still justified by that original problem, noting that the oligopoly now persists beyond its security rationale. No purely neutral party corroborates the P5 claim that disarmament progress is sufficient.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.75, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.75 over the interval because the 1995 indefinite extension locked in the asymmetry without disarmament timelines, transforming a provisional bargain into a permanent status hierarchy. Suppression is high (0.78) because the regime's persistence depends on active enforcement: sanctions on deviators, technology denial, and diplomatic coercion against withdrawal. Theater ratio climbs to 0.48 because Review Conferences and disarmament rhetoric increasingly function as performative maintenance of a legitimacy claim that masks continued P5 modernization. Accessibility collapse (0.65) reflects that alternatives such as the TPNW exist but are stigmatized and lack enforcement architecture, while Article X withdrawal triggers pariah status. Resistance (0.55) captures sustained Global South and TPNW pushback without sufficient power to alter the core structure.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 seat, the constraint appears as a Rope or mild Tangled Rope: they provide a global public good of nonproliferation security and bear the responsibility of deterrence. From the NNWS and threshold-state seats, the same structure computes as heavily extractive, because the disarmament reciprocity promised in Article VI is structurally deferred while enforcement of Articles I-II intensifies. The engine computes this divergence from identical structural data by applying directionality: the same scope and suppression produce negative effective extraction for the P5 and amplified extraction for the constrained NNWS.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5 are structural beneficiaries: the constraint subsidizes their exclusive nuclear status by externalizing the costs of nonproliferation onto NNWS and threshold states (d near the beneficiary end). NNWS and threshold states are structural targets: they pay the verification burden and forgo the deterrent without receiving the promised disarmament reciprocity (d near the target end). The IAEA sits in the middle: it administers enforcement and derives institutional survival from the regime, but does not itself capture the strategic rents (d mid-range). The advocacy complex and excluded seekers occupy observer/excluded positions where directionality is not computed as extraction or subsidy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â rapid 1960s horizontal proliferation â was genuine and is partially solved. However, the 1995 indefinite extension locked in the oligopoly long after the original proliferation crisis stabilized, creating mandatrophy risk. The classification as Tangled Rope prevents mislabeling: it preserves the genuine coordination function (fewer nuclear-armed states) while registering the asymmetric extraction (victim set of NNWS and threshold states, absence of P5 accountability). A pure Snare reading would ignore the real nonproliferation benefit; a pure Rope reading would ignore the status hierarchy and deferred disarmament. The measurement series shows extraction accumulating over time, supporting the Tangled Rope diagnosis over the interval.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a binding obligation with a temporal dimension, or an aspirational goal without enforceable content?',
    'ICJ advisory opinion depth or consistent state practice analysis establishing whether P5 behavior constitutes breach or political discretion.',
    'If Article VI is binding, the oligopoly reading is legally incoherent and the constraint shifts toward snare (active suppression of a legal obligation); if aspirational, the reciprocal disarmament reading loses legal grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'Core kernel ambiguity over Article VI legal status.').

omega_variable(
    p5_status_rent_vs_security_provision,
    'Do the P5 extract unearned status rents, or do they provide a genuine security stabilization function that justifies asymmetric obligations?',
    'Comparative counterfactual analysis of nuclear instability in a multipolar deterrence world versus the observed oligopolistic regime.',
    'If pure rent, extraction metric is higher and coordination function is cover; if genuine stabilization, floor extraction is coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(p5_status_rent_vs_security_provision, empirical, 'Whether P5 oligopoly is rent extraction or necessary security provision.').

omega_variable(
    threshold_state_security_seeking,
    'Are threshold states genuinely seeking deterrent capacity for defensive security, or are they proliferation risks that justify enforcement asymmetry?',
    'Historical case studies of threshold state behavior under security dilemma conditions.',
    'If defensive security-seeking, the victim set is larger and the constraint more extractive; if proliferation-prone, the coordination function is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_security_seeking, empirical, 'Threshold state motivation ambiguity.').

omega_variable(
    oligopoly_reading_operational_accuracy,
    'Does the oligopoly enforcement reading capture the treaty''s operative structure, or does the reciprocal disarmament reading describe the regime as actually practiced?',
    'Analysis of Review Conference outcome documents, P5 voting records on disarmament resolutions, and IAEA resource allocation between safeguards and disarmament verification.',
    'If reciprocal disarmament dominates practice, this constraint''s epsilon is overestimated; if oligopoly enforcement dominates, the reading is structurally accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_reading_operational_accuracy, conceptual, 'Operational dominance of oligopoly versus reciprocal reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 0, 55).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligopoly_tr_t0, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(npt_oligopoly_tr_t8, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(npt_oligopoly_tr_t15, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement(npt_oligopoly_tr_t25, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(npt_oligopoly_tr_t35, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 35, 0.4).
narrative_ontology:measurement(npt_oligopoly_tr_t45, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(npt_oligopoly_tr_t55, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 55, 0.48).

% Extraction over time
narrative_ontology:measurement(npt_oligopoly_be_t0, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(npt_oligopoly_be_t8, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(npt_oligopoly_be_t15, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement(npt_oligopoly_be_t25, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(npt_oligopoly_be_t35, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement(npt_oligopoly_be_t45, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 45, 0.72).
narrative_ontology:measurement(npt_oligopoly_be_t55, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 55, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligopoly_su_t0, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(npt_oligopoly_su_t8, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(npt_oligopoly_su_t15, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(npt_oligopoly_su_t25, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(npt_oligopoly_su_t35, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(npt_oligopoly_su_t45, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 45, 0.75).
narrative_ontology:measurement(npt_oligopoly_su_t55, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 55, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
