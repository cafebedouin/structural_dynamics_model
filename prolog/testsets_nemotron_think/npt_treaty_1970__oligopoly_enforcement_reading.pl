% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Oligopoly Enforcement Reading (Articles I-II Primary, Article VI Aspirational)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The 1970 NPT establishes a grand bargain: NNWS forego nuclear weapons
 *   (Articles I-II) in exchange for P5 pursuit of disarmament (Article VI)
 *   and access to peaceful nuclear technology (Article IV). This reading
 *   treats Articles I-II as the primary, legally binding obligation enforced
 *   through IAEA safeguards and UNSC action, while Article VI is read as
 *   contingent and aspirational — a political commitment without temporal
 *   benchmarks or enforcement. The result is an enforcement oligopoly: P5
 *   retain recognized nuclear status and veto power over enforcement while
 *   bearing no verification burden; NNWS and threshold states bear full
 *   inspection costs and are denied deterrent capabilities. The constraint
 *   coordinates horizontal nonproliferation (genuine coordination function)
 *   while extracting status hierarchy benefits for P5 (asymmetric
 *   extraction).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.72).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.78).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.76).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading (Articles I-II Primary, Article VI Aspirational)").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'cb4174ad-df97-430a-8aa1-a0cba619317f').
narrative_ontology:cs_kernel_codification('cb4174ad-df97-430a-8aa1-a0cba619317f', formalized).
narrative_ontology:cs_authority_grounding('cb4174ad-df97-430a-8aa1-a0cba619317f', extraction).
narrative_ontology:cs_interpretation_layer_present('cb4174ad-df97-430a-8aa1-a0cba619317f').
narrative_ontology:cs_reading_relation('cb4174ad-df97-430a-8aa1-a0cba619317f', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb4174ad-df97-430a-8aa1-a0cba619317f', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('cb4174ad-df97-430a-8aa1-a0cba619317f', foundational, horizontal_nonproliferation_primacy).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_primacy, holdable).
narrative_ontology:cs_axiom_grounding('cb4174ad-df97-430a-8aa1-a0cba619317f', horizontal_nonproliferation_primacy, conventional).
narrative_ontology:cs_axiom('cb4174ad-df97-430a-8aa1-a0cba619317f', foundational, article_vi_aspirational_only).
narrative_ontology:cs_axiom_status(article_vi_aspirational_only, holdable).
narrative_ontology:cs_axiom_grounding('cb4174ad-df97-430a-8aa1-a0cba619317f', article_vi_aspirational_only, conventional).
narrative_ontology:cs_reference_frame('cb4174ad-df97-430a-8aa1-a0cba619317f', npt_1970_bargain_horizontal_primacy).
narrative_ontology:cs_drift_state('cb4174ad-df97-430a-8aa1-a0cba619317f', contemporary_revcon_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('cb4174ad-df97-430a-8aa1-a0cba619317f', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, horizontal_nonproliferation_norm).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_order_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain recognized nuclear status under Article IX.3 while Article VI disarmament obligation is treated as aspirational. Benefit from legal monopoly on nuclear deterrence, status hierarchy in UNSC, and nonproliferation enforcement that prevents competitors. Bear no inspection burden; accountability for disarmament is procedural only.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary,
    institutional, generational, arbitrage, global).

% Bound by Articles I-II to forego nuclear weapons and accept IAEA comprehensive safeguards. Bear full inspection costs, transparency obligations, and technology denial regimes. Receive security assurances (negative/positive) that are politically non-binding. Exit via Article X withdrawal is legally available but politically costly and triggers security dilemmas.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_parties, payer,
    organized, biographical, constrained, global).

% States with technical capacity to weaponize rapidly (e.g., Japan, Germany, Iran, Brazil, Argentina historically). Denied deterrent capability by treaty while facing regional security threats. Bear heightened inspection scrutiny and political pressure. Cannot access Article X withdrawal without severe consequences; trapped in NNWS category despite latent capacity.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, payer,
    moderate, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states, excluded).

% Administers safeguards agreements, conducts inspections, reports compliance to UNSC. Operationalizes the enforcement asymmetry: NNWS receive comprehensive safeguards; P5 receive voluntary offer agreements only. Dependent on state funding and political cooperation; authority derives from treaty mandate but enforcement power rests with UNSC/P5.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_verification_regime, agenda_setter,
    institutional, generational, analytical, global).

% NGOs, advocacy networks, and expert communities monitoring treaty implementation. Push for Article VI compliance, challenge enforcement asymmetry, document humanitarian consequences. No formal seat at NPT RevCons but influence discourse and state positions through documentation and norm entrepreneurship.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, civil_society_antinuke, observer,
    organized, generational, analytical, global).

% States actively pursuing or considering nuclear weapons outside NPT (e.g., DPRK post-withdrawal, Iran pre-JCPOA, potential future aspirants). Would object to the bargain if present: denied the P5 pathway to recognized status, face enforcement without representation. Their exclusion is structural — the treaty defines them as proliferators, not parties.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, excluded_nuclear_aspirants, excluded,
    powerless, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation through binding non-acquisition commitments (Articles I-II) verified by a universal inspection regime (IAEA safeguards), creating a predictable legal framework that reduces incentives for nuclear hedging and arms racing among NNWS.
% TRANSFER_FUNCTION: Moves inspection burden, transparency costs, and technology denial from P5 to NNWS and threshold states; moves legal recognition of nuclear status, UNSC veto power, and freedom from verification to P5. Article VI disarmament obligation transfers aspirational legitimacy to P5 without material transfer.
% ABSENT_VOICES: Nuclear aspirant states (excluded from party status), threshold states denied deterrent capability, future generations bearing proliferation and disarmament failure risks, and non-state actors affected by nuclear testing/use. They would challenge the enforcement asymmetry and the aspirational reading of Article VI if they had standing.
% DISAPPEARANCE_RATIONALE: If the treaty vanished overnight, the legal basis for IAEA inspections and UNSC enforcement actions against proliferators would collapse. P5 would lose the primary legal instrument constraining horizontal proliferation. NNWS would face immediate pressure to hedge or withdraw. A proliferation cascade would be probable within 5-10 years, fundamentally rearranging the nuclear order.
% FOUNDING_PROBLEM: Prevent nuclear war by limiting nuclear weapons to the five states that possessed them in 1968 (horizontal nonproliferation) while committing those five to pursue nuclear disarmament in good faith (vertical nonproliferation), establishing a grand bargain of non-acquisition in exchange for disarmament.
% FOUNDING_PROBLEM_CORROBORATION: NNWS and Non-Aligned Movement statements at every NPT Review Conference since 1995 attest the disarmament bargain is broken — P5 modernization programs contradict Article VI. P5 official statements maintain the bargain is live, citing arsenal reductions from Cold War peaks. Civil society analyses (ICAN, SIPRI, BASIC) corroborate NNWS position: reductions are bilateral US-Russia, not multilateral; modernization undermines disarmament. No independent body corroborates P5 claim that Article VI is being fulfilled.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) reflects the structural transfer: P5 collect status/legitimacy benefits and freedom from verification while NNWS/threshold states pay inspection costs and accept technology denial. Suppression (0.78) is high because the constraint actively suppresses alternatives — threshold states cannot legally acquire deterrents, withdrawal triggers severe consequences, and the verification regime is enforced by UNSC where P5 hold veto. Theater ratio (0.42) captures the performative disarmament rhetoric (NPT RevCon statements, unilateral reductions) that masks the absence of binding Article VI enforcement. Accessibility collapse (0.76) is high because the deterrent alternative is legally and politically foreclosed for parties. Resistance (0.48) is moderate — NNWS resist through RevCon statements and demand for action plans, but structural exit is constrained.
 *
 * PERSPECTIVAL GAP:
 *   From P5 seat: the treaty is a Rope — genuine coordination preventing proliferation cascade, with Article VI as aspirational goal. From NNWS/threshold seats: the treaty is a Snare/Tangled Rope — coordination function exists but extraction is asymmetric and enforcement is oligopolistic. The engine computes this divergence from the structural data: same constraint, different directionality, different effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 are structural beneficiaries (d ≈ 0.15): they collect status rents, control enforcement, face no verification. NNWS are targets (d ≈ 0.85): they pay inspection costs, accept technology denial, receive non-binding assurances. Threshold states are trapped targets (d ≈ 0.95): denied deterrent, heightened scrutiny, no voice. IAEA is agenda_setter with analytical exit (d ≈ 0.5 symmetric). Civil society is observer (d ≈ 0.0 beneficiary). Excluded aspirants are structurally excluded — their exclusion is the enforcement object.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent nuclear war via nonproliferation + disarmament) is contested. Horizontal nonproliferation remains live (proliferation risk persists). Vertical disarmament is dead in practice (P5 modernizing, no multilateral process) but live in rhetoric. The arrangement persists because P5 benefit from the status hierarchy and NNWS lack coordinated exit. Mandatrophy is unresolved: the disarmament mandate has atrophied but the nonproliferation mandate has not; the treaty is neither pure coordination nor pure extraction but a tangled rope where the coordination function is real but the extraction asymmetry is structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_bindingness,
    'Is Article VI a legally binding obligation of result (disarmament) or a procedural obligation of conduct (pursue negotiations in good faith)?',
    'ICJ advisory opinion on Nuclear Weapons (1996) para 105: ''obligation to pursue in good faith and bring to a conclusion negotiations leading to nuclear disarmament.'' But state practice and P5 statements treat it as aspirational. Resolution requires authoritative interpretation accepted by all parties — currently absent.',
    'If binding obligation of result, P5 are in material breach, changing classification toward snare. If procedural/aspirational, the enforcement asymmetry is treaty-design, not violation, supporting tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_bindingness, conceptual, 'Legal character of Article VI obligation — determines whether P5 non-compliance is breach or design.').

omega_variable(
    enforcement_asymmetry_structurality,
    'Is the enforcement asymmetry (NNWS comprehensive safeguards vs P5 voluntary offers) a necessary feature of the bargain or a contingent outcome of P5 leverage?',
    'Historical analysis of 1968 negotiations: did NNWS accept asymmetry as quid pro quo for Article VI, or was it imposed? Documentary record shows NNWS sought stronger Article VI language; asymmetry reflects P5 negotiating power.',
    'If necessary feature, asymmetry is coordination cost (lower extractiveness). If contingent P5 leverage, asymmetry is extraction (higher extractiveness).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_asymmetry_structurality, empirical, 'Whether the verification double-standard is structural necessity or power imposition.').

omega_variable(
    deterrent_denial_stability,
    'Does denying deterrent capability to threshold states increase or decrease strategic stability?',
    'Compare proliferation outcomes: states that acquired deterrents outside NPT (India, Pakistan, Israel, DPRK) vs threshold states that remained NNWS (Japan, Germany, Brazil, Argentina). Stability outcomes are mixed and context-dependent.',
    'If denial increases stability, coordination function is stronger (lower effective extraction). If denial decreases stability by creating unresolved security dilemmas, extraction is higher — the constraint creates the insecurity it claims to prevent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deterrent_denial_stability, preference, 'Strategic effect of deterrent denial on threshold states — depends on security environment valuation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_oligopoly_tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt_oligopoly_tr_t1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement(npt_oligopoly_tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt_oligopoly_tr_t2005, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(npt_oligopoly_tr_t2015, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(npt_oligopoly_tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(npt_oligopoly_be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt_oligopoly_be_t1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(npt_oligopoly_be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt_oligopoly_be_t2005, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(npt_oligopoly_be_t2015, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(npt_oligopoly_be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(npt_oligopoly_su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt_oligopoly_su_t1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(npt_oligopoly_su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(npt_oligopoly_su_t2005, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(npt_oligopoly_su_t2015, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2015, 0.76).
narrative_ontology:measurement(npt_oligopoly_su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This reading and reciprocal_disarmament_reading share the same kernel (NPT 1970) but diverge on Article VI bindingness. This reading treats horizontal nonproliferation as primary and Article VI as aspirational; the sibling treats them as reciprocal. They coexist as live positions held by different party coalitions (P5/Western allies vs NNWS/NAM). Withdrawal_sovereignty_reading is influenced by this reading's enforcement rigor — the stronger the oligopoly enforcement, the more withdrawal appears as the only exit for dissatisfied states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
