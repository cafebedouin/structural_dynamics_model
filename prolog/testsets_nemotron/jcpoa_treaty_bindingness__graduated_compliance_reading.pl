% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Mechanism
 *   domain: international_law/nuclear_nonproliferation/treaty_compliance
 *
 * SUMMARY:
 *   The Joint Comprehensive Plan of Action (JCPOA) established a reciprocal
 *   commitment framework where Iran's nuclear constraints were exchanged for
 *   phased sanctions relief. This reading interprets the treaty as a scaled
 *   commitment mechanism: compliance is assessed proportionally, enforcement
 *   responds to violation severity, and dispute resolution (Joint Commission,
 *   Advisory Board, UNSC referral) prioritizes de-escalation over automatic
 *   snapback. The constraint operates as a tangled rope — genuine
 *   coordination on non-proliferation verification coupled with asymmetric
 *   extraction where Iran bears disproportionate verification burden and the
 *   P5+1 retain snapback authority. The 2018 U.S. withdrawal and subsequent
 *   Iranian countermeasures (2019-2021) tested the graduated mechanism; the
 *   2021-2023 negotiation rounds revealed its structural limits.
 *
 * KEY AGENTS:
 *   - iranian_state: Primary target (institutional/trapped) — bears verification burden, enrichment limits, snapback exposure
 *   - p5_plus_1: Agenda setter (institutional/arbitrage) — controls relief flow, defines compliance thresholds, holds snapback trigger
 *   - pragmatic_diplomacy_advocates: Beneficiary (organized/mobile) — gains diplomatic off-ramps, crisis management tools
 *   - economic_actors_seeking_partial_engagement: Beneficiary (powerful/mobile) — accesses Iranian market conditional on compliance tiers
 *   - iaea_verification_mission: Beneficiary (institutional/analytical) — gains unprecedented monitoring access
 *   - hardline_iranian_factions: Payer (powerful/constrained) — loses nuclear leverage, faces domestic political cost
 *   - sanctions_regime_purists: Payer (organized/trapped) — sees graduated relief as legitimizing Iranian program
 *   - israeli_and_gulf_states: Excluded (powerful/trapped) — bear security risk without veto in Joint Commission
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.58).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.63).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Mechanism").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_nonproliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2').
narrative_ontology:cs_kernel_codification('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', formalized).
narrative_ontology:cs_authority_grounding('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', lineage).
narrative_ontology:cs_interpretation_layer_present('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2').
narrative_ontology:cs_reading_relation('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', jcpoa_treaty_bindingness__binding_multilateral_reading, influences).
narrative_ontology:cs_reading_relation('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', foundational, proportional_compliance_deserves_proportional_relief).
narrative_ontology:cs_axiom_status(proportional_compliance_deserves_proportional_relief, holdable).
narrative_ontology:cs_axiom_grounding('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', proportional_compliance_deserves_proportional_relief, conventional).
narrative_ontology:cs_axiom('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', foundational, dispute_resolution_precedes_snapback).
narrative_ontology:cs_axiom_status(dispute_resolution_precedes_snapback, holdable).
narrative_ontology:cs_axiom_grounding('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', dispute_resolution_precedes_snapback, conventional).
narrative_ontology:cs_reference_frame('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', jcpoa_2015_consensus_implementation).
narrative_ontology:cs_drift_state('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', post_2018_us_withdrawal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('2e861bd7-99b7-49a2-90e3-8ee8d23d9ed2', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_partial_engagement).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_mission).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_nuclear_program).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_iranian_factions).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_regime_purists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, proportionality_in_arms_control_enforcement).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, dispute_resolution_before_snapback).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__graduated_compliance_reading, partial_compliance_merits_partial_relief).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepts the most intrusive nuclear verification regime in history (Additional Protocol + JCPOA-specific measures), caps enrichment at 3.67%, reduces centrifuge count by 2/3, ships out 97% of LEU stockpile. In return receives phased sanctions relief that can be snapped back by any P5+1 member triggering UNSCR 2231 dispute resolution. Exit (Art. 36 withdrawal) triggers immediate snapback and international isolation. Domestic political cost of compliance is borne by Rouhani-era pragmatists; hardliners frame compliance as surrender.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state, payer,
    institutional, generational, trapped, global).

% Collectively defines compliance thresholds through the Joint Commission; controls the pace and scope of sanctions relief implementation; holds the snapback trigger (UNSCR 2231) which any single member can invoke. The U.S. (2015-2018) drove the relief architecture; post-2018 the E3 (France, Germany, UK) plus Russia/China attempted to maintain the relief channel via INSTEX. Their coordination cost is diplomatic capital; their extraction is non-proliferation assurance at minimal direct cost.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, p5_plus_1, agenda_setter,
    institutional, generational, arbitrage, global).

% Gain a structured crisis management mechanism: the Joint Commission's 35-day dispute resolution timeline, the Advisory Board's technical arbitration, and the graduated snapback process provide off-ramps that pure confrontation lacks. They include EU foreign policy institutions, UN diplomats, arms control NGOs, and crisis management specialists. Their benefit is institutional — the constraint gives them procedural tools — but they bear reputational risk when the framework degrades.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Multinational corporations (Airbus, Total, Peugeot, Siemens, Chinese state firms) and financial institutions accessed the Iranian market during 2016-2018 relief. Their engagement was tiered: full relief required full compliance; partial relief (e.g., civil aviation, humanitarian trade) persisted even during disputes. Post-2018, secondary sanctions forced exit; the constraint's graduated structure meant their exposure varied by sector and timing. They lobby for predictable rules but accept the compliance conditionality.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_actors_seeking_partial_engagement, beneficiary,
    powerful, biographical, mobile, global).

% Gained unprecedented verification authority: continuous online enrichment monitoring, managed access to centrifuge workshops, uranium ore concentrate tracking, and the right to investigate undeclared sites via the Joint Commission. This is the most intrusive verification regime ever negotiated — a genuine coordination achievement. The mission bears operational burden (inspectors, cameras, seals, data analysis) but its institutional mandate is strengthened by the constraint's technical specificity.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_mission, beneficiary,
    institutional, generational, analytical, global).

% IRGC, conservative parliamentarians, and Supreme Leader's hardline allies opposed the JCPOA as surrendering nuclear leverage for unreliable relief. They bear the domestic political cost: the constraint validates the 'resistance economy' narrative when relief fails, and empowers them to argue for nuclear threshold crossing. Their exit option is constrained — they control veto points (parliament ratification, Supreme Leader approval) but breaking the constraint risks war and total isolation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_iranian_factions, payer,
    powerful, biographical, constrained, national).

% U.S. Congressional hawks, Israeli government, Gulf state security establishments view any sanctions relief as legitimizing Iran's nuclear program. They bear the cost of the constraint's existence: the graduated mechanism forces them to accept partial Iranian compliance in exchange for partial relief, rather than maintaining maximum pressure. Their exit is trapped — the constraint is embedded in UNSCR 2231 and P5+1 consensus; unilateral rejection (U.S. 2018) fractured the coalition but didn't dissolve the legal architecture.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_regime_purists, payer,
    organized, biographical, trapped, global).

% Israel, Saudi Arabia, UAE bear direct security externalities from Iran's nuclear program and regional proxies. They had no seat at the Joint Commission, no veto in dispute resolution, and no formal role in compliance assessment. Their 'exit' is military action (which the constraint was designed to make unnecessary) or diplomatic normalization outside the framework (Abraham Accords). They are structurally excluded from the coordination mechanism while bearing its failure costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, israeli_and_gulf_states, excluded,
    powerful, biographical, trapped, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the mutual distrust deadlock: Iran gets phased sanctions relief for verified nuclear constraints; P5+1 get unprecedented verification of Iran's program; both get a structured dispute resolution mechanism that replaces ad-hoc crisis escalation with calibrated, time-bound procedures.
% TRANSFER_FUNCTION: Moves sanctions relief (oil revenue access, banking normalization, trade licenses, asset unfreezing) from P5+1 to Iran in exchange for nuclear constraints (enrichment caps, centrifuge limits, stockpile reduction, verification access) from Iran to P5+1/IAEA. The transfer is proportional: each compliance tier unlocks a corresponding relief tier; violations trigger proportional relief suspension.
% ABSENT_VOICES: Iranian civil society (would demand broader rights and economic justice beyond nuclear deal), regional non-state actors (Hezbollah, Houthis, PMF — bear security consequences without representation), U.S. Congress (treaty ratification bypassed via executive agreement, creating democratic legitimacy deficit), future generations (nuclear waste, proliferation precedent, environmental costs of enrichment).
% DISAPPEARANCE_RATIONALE: If the graduated compliance mechanism vanished overnight, the 2015-2025 diplomatic architecture collapses: IAEA loses its most intrusive verification mandate; Iran's enrichment would likely accelerate to 60%+ without calibrated off-ramps; P5+1 unity fractures into unilateral pressure vs. engagement camps; regional states face binary choice of acquiescence or military action. The world rearranges because the constraint is the only structured channel managing the Iran nuclear crisis.
% FOUNDING_PROBLEM: Iran's nuclear breakout capacity (2-3 months to weapons-grade HEU in 2015) created a crisis where military action, unlimited enrichment, or a negotiated constraint were the only options. The JCPOA was built to extend breakout to 12+ months via verified constraints, buying time for confidence-building and potential normalization.
% FOUNDING_PROBLEM_CORROBORATION: IAEA Director General reports (2016-2023) confirm breakout extension during full compliance and compression after 2019 countermeasures. U.S. intelligence community assessments (2015, 2019, 2021) corroborate the technical breakout timeline shifts. Iranian Atomic Energy Organization statements acknowledge the constraint's technical effect while disputing its political legitimacy. P5+1 foreign ministry archives document the founding consensus. No single party's narrative is unchallenged.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron+rescue1', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects the asymmetric verification burden: Iran accepts IAEA Additional Protocol + JCPOA-specific measures (cameras, seals, centrifuge monitoring) while P5+1 obligations are primarily negative (refraining from sanctions). Suppression (0.58) is moderate — the constraint doesn't eliminate alternatives (Iran could withdraw per Art. 36) but makes withdrawal costly via snapback and international isolation. Theater ratio (0.31) captures the gap between the 'proportional compliance' framing and the reality that snapback is a binary nuclear option held by one side. Accessibility collapse (0.55) and resistance (0.63) reflect that alternatives exist but are politically costly; the constraint is neither a natural law nor a pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the P5+1 seat, the constraint is a rope — they built the verification architecture and control the relief valve. From Iran's seat, it is a snare — they surrendered 97% of enriched stockpile and accepted unprecedented intrusion for relief that proved reversible. From the IAEA seat, it is a mountain of verification methodology — the monitoring regime is technically unprecedented and structurally embedded. From excluded regional states, it is a piton — a decaying constraint maintained theatrically while the strategic reality shifts. The engine computes these seat divergences from the structural data; this reading's claim (tangled_rope) captures the aggregate structural truth.
 *
 * DIRECTIONALITY LOGIC:
 *   Iran (institutional/trapped) sits at d ≈ 0.85 — full target of verification, enrichment caps, and snapback exposure. P5+1 (institutional/arbitrage) sit at d ≈ 0.15 — they collect non-proliferation assurance and retain enforcement discretion. Pragmatic diplomats and economic actors (organized/mobile, powerful/mobile) sit at d ≈ 0.3-0.4 — they benefit from partial engagement but bear reputational risk if the framework collapses. IAEA (institutional/analytical) at d ≈ 0.2 — gains verification authority but bears implementation burden. Hardliners and purists (powerful/constrained, organized/trapped) at d ≈ 0.7-0.8 — they pay political costs for a constraint they oppose. Excluded states (powerful/trapped) at d ≈ 0.9 — they bear security externalities with no institutional voice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Iran's nuclear breakout capacity) remains live but transformed: the 2015 breakout timeline (2-3 months) was extended to 12+ months under JCPOA constraints; post-2018 Iranian advances have compressed it again. The constraint's mandate has not atrophied — the coordination problem persists — but the extraction asymmetry has become more visible as relief eroded while verification remained. The graduated mechanism prevents mislabeling: it is not pure extraction (Iran gets calibrated relief for calibrated compliance) nor pure coordination (snapback authority is unilateral). The tangled_rope classification holds because both functions are structurally present and neither has displaced the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Where does the graduated compliance reading draw the line between proportional response and material breach that triggers full snapback?',
    'Joint Commission precedent and state practice in the 2019-2021 compliance disputes; P5+1 and Iran positions on what constitutes ''significant non-performance'' under JCPOA Art. 36-37.',
    'If the threshold is vague, the constraint operates as a snare with moving goalposts; if legally crystallized, it functions as a genuine tangled rope with determinate boundaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Boundary between graduated enforcement and material breach in the JCPOA framework').

omega_variable(
    extraction_distribution_ambiguity,
    'Does the economic benefit of partial sanctions relief flow primarily to the Iranian state apparatus or to the broader Iranian population and foreign commercial actors?',
    'Sectoral trade data 2016-2018 vs. 2019-2021; revenue tracking of Iranian oil exports, automotive, aviation, and banking sectors; household welfare indicators.',
    'If benefits concentrate in state-controlled sectors, the reading''s ''pragmatic engagement'' beneficiary claim is overstated; if diffuse, the coordination function is genuinely broadly distributed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_distribution_ambiguity, empirical, 'Distribution of economic gains from partial sanctions relief under graduated compliance').

omega_variable(
    committer_structure_jcpoa_kernel,
    'How does the graduated_compliance_reading structurally relate to the binding_multilateral_reading and transactional_provisional_reading of the JCPOA treaty_bindingness kernel?',
    'Comparative analysis of state practice, legal opinions, and diplomatic records across the three readings; tracking which reading governs Joint Commission decisions and P5+1-Iran diplomatic exchanges.',
    'If readings foreclose each other, the kernel is fractured into incompatible frameworks; if they coexist, the constraint operates in a permanent interpretive contest that affects enforcement credibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_jcpoa_kernel, conceptual, 'Structural relationship of this reading to sibling readings of the JCPOA treaty_bindingness kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_gcr_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t0, observed).
narrative_ontology:measurement(jcpoa_gcr_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t2, observed).
narrative_ontology:measurement(jcpoa_gcr_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.24).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t4, observed).
narrative_ontology:measurement(jcpoa_gcr_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.31).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t6, observed).
narrative_ontology:measurement(jcpoa_gcr_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.38).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t8, observed).
narrative_ontology:measurement(jcpoa_gcr_tr_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(jcpoa_gcr_tr_t10, observed).

% Extraction over time
narrative_ontology:measurement(jcpoa_gcr_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t0, observed).
narrative_ontology:measurement(jcpoa_gcr_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t2, observed).
narrative_ontology:measurement(jcpoa_gcr_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t4, observed).
narrative_ontology:measurement(jcpoa_gcr_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t6, observed).
narrative_ontology:measurement(jcpoa_gcr_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t8, observed).
narrative_ontology:measurement(jcpoa_gcr_be_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(jcpoa_gcr_be_t10, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_gcr_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t0, observed).
narrative_ontology:measurement(jcpoa_gcr_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t2, observed).
narrative_ontology:measurement(jcpoa_gcr_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t4, observed).
narrative_ontology:measurement(jcpoa_gcr_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t6, observed).
narrative_ontology:measurement(jcpoa_gcr_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t8, observed).
narrative_ontology:measurement(jcpoa_gcr_su_t10, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(jcpoa_gcr_su_t10, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.12).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_additional_protocol_universalization).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, unscr_2231_snapback_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_nuclear_breakout_timeline).

% DUAL FORMULATION NOTE:
% JCPOA treaty_bindingness kernel decomposes into three readings with distinct ε values: binding_multilateral (ε≈0.25, low extraction, high coordination), graduated_compliance (ε≈0.42, moderate extraction, calibrated enforcement), transactional_provisional (ε≈0.65, high extraction, unilateral voidability). The graduated reading structurally depends on the binding reading's legal architecture while creating pressure toward the transactional reading when compliance disputes escalate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, institutional, 0.85).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, organized, 0.35).
constraint_indexing:directionality_override(jcpoa_treaty_bindingness__graduated_compliance_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
