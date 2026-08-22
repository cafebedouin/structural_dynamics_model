% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-23
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
 *   human_readable: NPT Oligopoly Enforcement Reading: Horizontal Nonproliferation as Binding, Vertical Disarmament as Aspirational
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   The Nuclear Non-Proliferation Treaty (NPT) entered into force in 1970 as
 *   a three-pillar bargain: non-proliferation (Articles I-II), disarmament
 *   (Article VI), and peaceful use (Article IV). The oligopoly enforcement
 *   reading instantiates the constraint where Articles I-II operate as the
 *   primary binding obligation — horizontal proliferation prevention enforced
 *   through IAEA safeguards on non-nuclear-weapon states (NNWS) — while
 *   Article VI (the NWS disarmament obligation) functions as contingent and
 *   aspirational, invoked rhetorically but not operationally binding on the
 *   five recognized nuclear-weapon states (P5). This reading produces a
 *   structurally asymmetric constraint: NNWS bear intrusive verification
 *   burdens (comprehensive safeguards, Additional Protocols, complementary
 *   access) while NWS face no comparable verification of disarmament
 *   commitments. Threshold states with latent nuclear capacity (Japan,
 *   Brazil, Germany, Iran, etc.) are denied independent deterrent capability
 *   without receiving credible security guarantees, placing them in the
 *   victim set. The P5 and their nuclear umbrella allies are the
 *   beneficiaries, maintaining a status hierarchy that the treaty's
 *   enforcement machinery protects.
 *
 * KEY AGENTS:
 *   - P5 nuclear-weapon states (US, Russia, UK, France, China): agenda_setter / beneficiary — set the enforcement agenda, collect status hierarchy rents, avoid reciprocal verification
 *   - NNWS (185+ states): payer — bear inspection burden, forgo deterrent option, receive Article IV cooperation as coordination benefit
 *   - Threshold states (Japan, Brazil, Iran, Germany, etc.): payer / victim — denied deterrent, bear enhanced scrutiny, security guarantees substitutive not substitutable
 *   - IAEA inspection apparatus: beneficiary / agenda_setter — institutional mandate expands with each compliance crisis, budget and authority grow
 *   - Nuclear umbrella allies (NATO, US allies in Asia): beneficiary — receive extended deterrence without proliferation cost, support enforcement regime
 *   - Global South development aspirants: victim / excluded — Article IV promise of peaceful use technology transfer remains unrealized, bear nonproliferation costs without disarmament dividends
 *   - Analytical observers (arms control community, ICJ, UNSC): observer — assess compliance, produce normative discourse, no direct extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.72).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Oligopoly Enforcement Reading: Horizontal Nonproliferation as Binding, Vertical Disarmament as Aspirational").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, '64f25e71-86b7-4552-bb65-b9b98b4a7302').
narrative_ontology:cs_kernel_codification('64f25e71-86b7-4552-bb65-b9b98b4a7302', formalized).
narrative_ontology:cs_authority_grounding('64f25e71-86b7-4552-bb65-b9b98b4a7302', lineage).
narrative_ontology:cs_interpretation_layer_present('64f25e71-86b7-4552-bb65-b9b98b4a7302').
narrative_ontology:cs_reading_relation('64f25e71-86b7-4552-bb65-b9b98b4a7302', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('64f25e71-86b7-4552-bb65-b9b98b4a7302', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('64f25e71-86b7-4552-bb65-b9b98b4a7302', foundational, article_vi_aspirational_not_binding).
narrative_ontology:cs_axiom_status(article_vi_aspirational_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('64f25e71-86b7-4552-bb65-b9b98b4a7302', article_vi_aspirational_not_binding, conventional).
narrative_ontology:cs_axiom('64f25e71-86b7-4552-bb65-b9b98b4a7302', foundational, horizontal_nonproliferation_as_primary_obligation).
narrative_ontology:cs_axiom_status(horizontal_nonproliferation_as_primary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('64f25e71-86b7-4552-bb65-b9b98b4a7302', horizontal_nonproliferation_as_primary_obligation, conventional).
narrative_ontology:cs_axiom('64f25e71-86b7-4552-bb65-b9b98b4a7302', secondary, p5_status_hierarchy_as_regime_foundation).
narrative_ontology:cs_axiom_status(p5_status_hierarchy_as_regime_foundation, holdable).
narrative_ontology:cs_axiom_grounding('64f25e71-86b7-4552-bb65-b9b98b4a7302', p5_status_hierarchy_as_regime_foundation, instrumental).
narrative_ontology:cs_reference_frame('64f25e71-86b7-4552-bb65-b9b98b4a7302', three_pillar_bargain_1968).
narrative_ontology:cs_drift_state('64f25e71-86b7-4552-bb65-b9b98b4a7302', post_2010_review_conference, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('64f25e71-86b7-4552-bb65-b9b98b4a7302', '2026-08-23T14:22:00Z').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea_inspection_apparatus).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_umbrella_allies).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_denied_deterrent).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, global_south_development_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five recognized NWS (US, Russia, UK, France, China) set the enforcement agenda through UNSC veto power, control the IAEA Board of Governors, and define the interpretation of Article VI. They collect status hierarchy rents: permanent UNSC seats, nuclear monopoly legitimacy, security architecture built on extended deterrence. They bear no verification burden for disarmament — no intrusive inspections, no complementary access, no timeline enforcement. Their exit option is arbitrage: they can reinterpret obligations, modernize arsenals, and adjust posture without regime penalty.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary).

% The IAEA Secretariat and safeguards inspection corps administer the verification regime. Their mandate, budget, and authority expand with each compliance crisis (Iraq 1991, DPRK 1993, Iran 2002, Libya 2003, Syria 2007). They benefit from the oligopoly enforcement reading because it makes their verification function the operational center of the regime while the reciprocal disarmament verification function remains absent. They face no exit pressure — the regime depends on them.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_inspection_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_inspection_apparatus, beneficiary).

% NATO allies and US allies in Asia (Japan, South Korea, Australia) receive extended deterrence without bearing proliferation costs. They support the enforcement regime diplomatically and financially (IAEA funding, political cover for P5). Their exit is mobile: they could theoretically pursue independent deterrents (Japan, South Korea have latent capacity) but alliance credibility and security guarantees make the current arrangement beneficial. They are not victims — they opt into the umbrella.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_umbrella_allies, beneficiary,
    organized, generational, mobile, regional).

% 185+ NNWS parties bear comprehensive safeguards (INFCIRC/153) and, for many, Additional Protocol obligations. They accept intrusive inspections, reporting requirements, and technology restrictions (NSG guidelines, dual-use controls) in exchange for Article IV peaceful use cooperation that is often delayed, conditional, or denied. Their exit is constrained: withdrawal (Article X) triggers security guarantee loss, sanctions risk, and IAEA scrutiny intensification. They lack individual leverage but coordinate through NAM at Review Conferences.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_nuclear_weapon_states, payer,
    moderate, biographical, constrained, global).

% States with latent nuclear capacity (Japan, Brazil, Germany, Iran, South Korea, Taiwan historically) are structurally denied independent deterrent capability. The oligopoly enforcement regime treats their latency as a proliferation risk requiring enhanced scrutiny (Iran: JCPOA/Additional Protocol; Brazil: Resende access disputes; Japan: plutonium stockpile monitoring). Security guarantees (US umbrella) are substitutive — offered instead of independent deterrent — but not substitutable (credibility gaps, alliance politics, extended deterrence coupling). Their nuclear latency is constitutive of their security identity and great power aspiration; weaponization or withdrawal would be existentially costly (identity_locked exit).
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_denied_deterrent, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, threshold_states_denied_deterrent, payer).

% Non-aligned and developing states bear the nonproliferation costs (technology denial, NSG restrictions, IAEA safeguards costs) without receiving the Article IV promised benefits (nuclear technology transfer, energy cooperation, development assistance). They are excluded from the regime's decision-making (UNSC P5 veto, IAEA Board concentration, NSG consensus rules). Their exit is constrained: withdrawal isolates them from nuclear cooperation entirely; compliance yields diminishing returns. They coordinate through NAM and G77 but lack structural leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, global_south_development_aspirants, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, global_south_development_aspirants, excluded).

% Think tanks, academia, former diplomats, ICJ, UNODA, and civil society produce the normative discourse around the NPT. They assess compliance, propose reforms, and witness Review Conferences. They neither collect extraction nor bear its costs directly. Their analytical seat sees the full structural asymmetry but has no enforcement lever. They are the engine's observational frame.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, arms_control_analytical_community, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents horizontal nuclear proliferation cascades by establishing a verified non-proliferation norm with institutional verification (IAEA safeguards), reducing the number of states that must be monitored for weaponization intent from potentially dozens to a manageable set of threshold cases.
% TRANSFER_FUNCTION: Moves inspection burden, technology access, and deterrent forbearance from NNWS and threshold states to the P5 and IAEA apparatus. The P5 receive status hierarchy maintenance and monopoly legitimacy; the IAEA receives expanding mandate and resources. NNWS transfer sovereignty over nuclear activities and forego the deterrent option. Threshold states transfer independent security agency for alliance-substitutive guarantees.
% ABSENT_VOICES: Threshold states' security establishments (who would argue for deterrent optionality), Global South nuclear energy agencies (who would demand Article IV fulfillment), and future generations (who inherit the disarmament deficit). The threshold states' security bureaucracies are structurally excluded — their latency is treated as proliferation risk, not security reasoning. The TPNW proponents (civil society, non-nuclear states) are excluded from NPT Review Conference consensus blocks.
% DISAPPEARANCE_RATIONALE: If the NPT oligopoly enforcement constraint vanished overnight: (1) P5 would lose the legal/institutional framework legitimizing their monopoly and constraining horizontal proliferation; (2) NNWS would face immediate proliferation cascade pressure (security dilemma, hedge decisions); (3) IAEA safeguards regime would lose its treaty mandate; (4) Nuclear umbrella allies would face immediate credibility crises; (5) Threshold states would face unrestrained weaponization decisions. The world would rearrange into a multipolar nuclear order with higher proliferation density and no verification baseline — not because the constraint is coordination, but because its extraction has become structural to the international order.
% FOUNDING_PROBLEM: The 1960s fear of '20-30 nuclear weapon states by 1980' — a proliferation cascade that would make nuclear war probable through accident, miscalculation, or cascade instability. The NPT was built to cap the nuclear club at five, buying time for disarmament (Article VI) and sharing peaceful benefits (Article IV).
% FOUNDING_PROBLEM_CORROBORATION: P5 and nuclear umbrella allies attest the proliferation cascade risk remains live (citing DPRK, Iran, potential future proliferators) — the problem persists. NAM states and TPNW proponents attest the reciprocal bargain (disarmament for nonproliferation) is dead — the P5 have not disarmed, the cascade was prevented but the bargain was broken. Independent arms control analysts (SIPRI, IISS, UNIDIR) corroborate: the horizontal nonproliferation function works (club capped at 9, not 20+); the vertical disarmament function has failed (arsenals reduced but modernization continues, no Article VI timeline). The founding problem is half-live, half-dead — hence contested.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   The extractiveness (0.68) reflects the structural transfer: NNWS and threshold states bear the costs of verification, forgone deterrent options, and constrained technological development, while the P5 maintain their monopoly without verifiable disarmament progress. Suppression (0.72) is high because the constraint actively prevents exit: Article X withdrawal is legally available but politically catastrophic (security guarantees withdrawn, sanctions triggered, IAEA scrutiny intensified), and threshold states face preventive pressure (Iran, Syria, Libya cases). Theater ratio (0.45) captures the growing gap between the treaty's rhetorical reciprocity (three pillars) and operational reality (one pillar enforced). Accessibility collapse (0.55) is moderate: alternatives exist (withdrawal, hedging, latency) but are structurally suppressed. Resistance (0.48) reflects NNWS pushback at Review Conferences (NAM statements, 2010 Action Plan, TPNW emergence) but without structural leverage to change the enforcement asymmetry.
 *
 * PERSPECTIVAL GAP:
 *   The P5 and nuclear umbrella allies experience the constraint as coordination (rope-like): a stable order preventing proliferation cascades, with Article VI as a long-term aspirational horizon. NNWS and threshold states experience it as extraction (snare-like): a one-way bargain where their compliance is verified but the reciprocal obligation is not. The IAEA apparatus experiences it as institutional mandate (self-reinforcing): each compliance crisis expands its authority. The engine computes this divergence from the structural data — the declared roles (agenda_setter/beneficiary vs. payer/victim) and exit options (arbitrage/mobile vs. trapped/identity_locked) drive the per-seat classification.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 and nuclear umbrella allies are structural beneficiaries (d ≈ 0.15-0.25): they collect status rents, security architecture benefits, and institutional control without bearing verification costs. NNWS are payers (d ≈ 0.75-0.85): they bear inspection costs, technology restrictions, and sovereignty intrusions. Threshold states are payers/victims with identity_locked exit (d ≈ 0.8-0.9): their nuclear latency is constitutive of their security identity, making exit (weaponization or withdrawal) existentially costly. The IAEA apparatus is a beneficiary with institutional power (d ≈ 0.2): its mandate and resources expand with enforcement. Global South aspirants are excluded (d ≈ 0.7): they bear developmental costs without voice in the regime.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing nuclear war through non-proliferation) remains live but the reciprocal bargain (disarmament for non-proliferation) has degraded. The arrangement persists because the P5 benefit from the status hierarchy and the IAEA benefits from institutional expansion, while NNWS lack coalition power to force reciprocity. The mandatrophy is unresolved: the constraint's mandate (three pillars) has outlived its reciprocal function, but the enforcement machinery (Articles I-II + IAEA) has become self-sustaining. The theater ratio rise (0.25→0.45) documents this drift: increasing performance of reciprocity (Review Conferences, Action Plans) masking static extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_character,
    'Is Article VI a binding legal obligation of result, a binding obligation of conduct, or a hortatory aspiration?',
    'ICJ advisory opinions (1996 Nuclear Weapons advisory opinion para. 105), state practice of NWS disarmament steps, NPT Review Conference consensus documents, and travaux préparatoires analysis.',
    'If Article VI is binding obligation of result, the oligopoly enforcement reading''s core premise (Article VI as contingent) is legally false — the reading forecloses on its own terms. If obligation of conduct, the reading''s asymmetry is a compliance failure not a structural feature. If aspirational, the reading''s structural claim holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_legal_character, conceptual, 'The legal character of Article VI — the hinge on which the oligopoly vs. reciprocal readings turn.').

omega_variable(
    inspection_burden_asymmetry_causality,
    'Does the NNWS inspection burden reflect genuine coordination necessity (verification of non-diversion) or does it structurally encode the oligopoly''s enforcement asymmetry?',
    'Comparative analysis of IAEA safeguards evolution: Comprehensive Safeguards Agreement (INFCIRC/153) vs. Model Additional Protocol (INFCIRC/540); inspection intensity correlation with NWS disarmament progress; resource allocation between verification and disarmament verification.',
    'If coordination necessity, the burden is the price of the rope function. If oligopoly encoding, the burden is extractive — the same inspections that verify non-diversion also legitimize the NWS monopoly by making NNWS compliance visible while NWS disarmament remains unverifiable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_burden_asymmetry_causality, empirical, 'Whether the inspection regime''s asymmetry is functionally necessary or structurally extractive.').

omega_variable(
    threshold_state_victim_status,
    'Are threshold states (Japan, Brazil, Iran, etc.) properly classified as victims of the oligopoly enforcement reading, or are they voluntary non-participants in the deterrent bargain?',
    'Historical analysis of threshold state nuclear decision-making: security environment assessments, alliance credibility calculations, domestic political constraints, and NPT Article X withdrawal calculations.',
    'If voluntary non-participants, they are not victims — they chose the umbrella over independent deterrent. If structurally denied deterrent by the oligopoly''s enforcement architecture (security guarantees as substitute, Article X withdrawal costs, IAEA scrutiny of threshold capabilities), they are victims of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_state_victim_status, conceptual, 'Whether threshold states bear extractive costs from being denied independent deterrent capability under the NPT regime.').

omega_variable(
    reading_kernel_relationship,
    'Is the oligopoly enforcement reading a defensible interpretation of the NPT kernel, or a cynical description of state practice that misreads the treaty''s structural logic?',
    'Treaty interpretation methodology (VCLT Articles 31-33): text, context, object and purpose, subsequent practice, and supplementary means. Compare the three declared readings against the treaty''s own architecture.',
    'If the reading is a defensible interpretation, it represents a genuine structural perspective on the constraint. If it misreads the treaty''s logic, it is an external critique masquerading as a reading — the kernel''s structural logic would be better captured by the reciprocal_disarmament_reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship, conceptual, 'Committee-frame omega: whether this reading is a genuine instantiation of the NPT kernel or an external critique.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t1985, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.38).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t2000, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2000, 0.41).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t2010, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t2020, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.45).

% Extraction over time
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t1985, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1985, 0.58).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t2000, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t2010, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t2020, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t1985, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1985, 0.62).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t2000, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t2010, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2010, 0.71).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t2020, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(npt_treaty_1970__oligopoly_enforcement_reading_su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_umbrella_arrangements).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, tpnw_treaty_2017).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_review_conference_process).

% DUAL FORMULATION NOTE:
% NPT kernel decomposes into three constraint stories: (1) oligopoly_enforcement_reading — Articles I-II binding, Article VI aspirational (this story, ε=0.68, tangled_rope); (2) reciprocal_disarmament_reading — Article VI binding reciprocal bargain (ε≈0.35, rope/tangled_rope boundary); (3) withdrawal_sovereignty_reading — Article X as sovereignty valve (ε≈0.25, rope/scaffold). The ε values differ because each reading structures the beneficiary/victim sets differently: this reading names NWS as beneficiaries and threshold states as victims; the reciprocal reading names all parties as coordinated beneficiaries; the withdrawal reading names sovereign exit as the coordination function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, institutional, 0.2).
constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, organized, 0.3).
constraint_indexing:directionality_override(npt_treaty_1970__oligopoly_enforcement_reading, moderate, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
