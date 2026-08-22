% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, []).

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
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Interpretive Authority over TRIPS
 *   domain: international_law/trade/health
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Understanding (DSU) grants panels binding
 *   interpretive authority over the TRIPS Agreement text, with enforcement
 *   through authorized trade retaliation. This interpretive layer operates as
 *   a meta-constraint on the TRIPS kernel: panel rulings lock in one reading
 *   (strong exclusivity, public health flexibility, or dispute settlement
 *   authority) through precedent. Since the Appellate Body's paralysis in
 *   2019, bilateral power dynamics increasingly substitute for multilateral
 *   adjudication — powerful members settle disputes on favorable terms while
 *   weaker members face panel rulings without appeal. The constraint
 *   coordinates genuine dispute resolution (rope function) but extracts
 *   asymmetrically through precedent that narrows flexibilities, benefits
 *   pharmaceutical innovators and high-income members, and suppresses generic
 *   competition and public health access.
 *
 * KEY AGENTS:
 *   - wto_dispute_settlement_body: Agenda setter (institutional/analytical) — administers interpretive authority, writes precedent
 *   - pharmaceutical_innovator_firms: Primary beneficiary (powerful/organized) — gain extended exclusivity through narrow panel readings
 *   - high_income_member_states: Secondary beneficiary (institutional/powerful) — shape panel composition, use retaliation threats
 *   - low_middle_income_country_governments: Primary payer/victim (moderate/organized) — face retaliation for using flexibilities, lack appeal recourse
 *   - generic_pharmaceutical_producers: Victim (moderate/organized) — blocked by panel precedent narrowing compulsory licensing
 *   - public_health_access_populations: Victim (powerless/trapped) — bear health costs of delayed/denied generic access
 *   - wto_member_states_general: Observer/analytical (various) — systemic interest in rule of law vs. power dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.75).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.68).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_law/trade/health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '06b5c26a-2c7b-49df-9166-5300cb1ae253').
narrative_ontology:cs_kernel_codification('06b5c26a-2c7b-49df-9166-5300cb1ae253', formalized).
narrative_ontology:cs_authority_grounding('06b5c26a-2c7b-49df-9166-5300cb1ae253', lineage).
narrative_ontology:cs_interpretation_layer_present('06b5c26a-2c7b-49df-9166-5300cb1ae253').
narrative_ontology:cs_reading_relation('06b5c26a-2c7b-49df-9166-5300cb1ae253', trips_agreement_interpretive_kernel__strong_exclusivity_reading, influences).
narrative_ontology:cs_reading_relation('06b5c26a-2c7b-49df-9166-5300cb1ae253', trips_agreement_interpretive_kernel__public_health_flexibility_reading, forecloses).
narrative_ontology:cs_axiom('06b5c26a-2c7b-49df-9166-5300cb1ae253', foundational, panel_precedent_binds_flexibility_scope).
narrative_ontology:cs_axiom_status(panel_precedent_binds_flexibility_scope, holdable).
narrative_ontology:cs_axiom_grounding('06b5c26a-2c7b-49df-9166-5300cb1ae253', panel_precedent_binds_flexibility_scope, conventional).
narrative_ontology:cs_axiom('06b5c26a-2c7b-49df-9166-5300cb1ae253', foundational, retaliation_enforces_interpretive_compliance).
narrative_ontology:cs_axiom_status(retaliation_enforces_interpretive_compliance, holdable).
narrative_ontology:cs_axiom_grounding('06b5c26a-2c7b-49df-9166-5300cb1ae253', retaliation_enforces_interpretive_compliance, conventional).
narrative_ontology:cs_axiom('06b5c26a-2c7b-49df-9166-5300cb1ae253', secondary, appellate_body_paralysis_shifts_authority_to_panels).
narrative_ontology:cs_axiom_status(appellate_body_paralysis_shifts_authority_to_panels, holdable).
narrative_ontology:cs_axiom_grounding('06b5c26a-2c7b-49df-9166-5300cb1ae253', appellate_body_paralysis_shifts_authority_to_panels, empirically_contingent).
narrative_ontology:cs_reference_frame('06b5c26a-2c7b-49df-9166-5300cb1ae253', dsu_multilateral_adjudication_framework).
narrative_ontology:cs_drift_state('06b5c26a-2c7b-49df-9166-5300cb1ae253', post_appellate_body_collapse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('06b5c26a-2c7b-49df-9166-5300cb1ae253', '2026-08-05T14:30:00Z').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_innovator_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_member_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_middle_income_country_governments).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_access_populations).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_interpretive_supremacy).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_minimum_standards_enforcement).
narrative_ontology:constraint_vindicates(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, patent_protection_uniformity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Panels and (formerly) Appellate Body issue binding rulings on TRIPS interpretation. Their precedent narrows flexibilities (compulsory licensing conditions, parallel import rules, patentability standards). They collect institutional authority and legitimacy; their enforcement arm is the DSB's retaliation authorization. Post-2019, they operate without appellate review, increasing their de facto interpretive power.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_body, agenda_setter,
    institutional, generational, analytical, global).

% Originator pharmaceutical companies (primarily US/EU/JP-based) gain extended market exclusivity when panels interpret TRIPS flexibilities narrowly. They lobby home governments to bring disputes, fund legal support, and use panel precedent to pressure governments against using compulsory licenses. Their exit is arbitrage-grade: they operate globally and can shift R&D, pricing, and enforcement strategies across jurisdictions.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_innovator_firms, beneficiary,
    powerful, biographical, arbitrage, global).

% Countries with strong pharmaceutical sectors (US, EU, Japan, Switzerland, UK) initiate most TRIPS disputes, shape panel composition through DSU procedures, and wield credible retaliation threats. They benefit from the constraint's interpretive drift toward stronger protection. Their exit is arbitrage: they can bypass the system via bilateral/regional agreements (TRIPS-plus FTAs) while using the multilateral system to pressure others.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_member_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, high_income_member_states, agenda_setter).

% Governments seeking to use TRIPS flexibilities (compulsory licenses, parallel imports, patent opposition) face dispute risk. Panel rulings against them authorize trade retaliation that can devastate export sectors. Post-Appellate Body collapse, they cannot appeal adverse panels; they must comply or face retaliation. Exit is constrained: leaving WTO is economically catastrophic; regional blocs offer limited shelter.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, low_middle_income_country_governments, payer,
    moderate, biographical, constrained, national).

% Generic manufacturers (India, China, Brazil, regional producers) face panel precedent that narrows Art. 31 compulsory licensing (e.g., Canada – Patent Protection, EC – Trademarks/Geographical Indications). They bear compliance costs, lost markets, and legal uncertainty. Exit is constrained: they depend on global supply chains and regulatory harmonization that the WTO system governs.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_pharmaceutical_producers, payer,
    moderate, biographical, constrained, global).

% Patients and health systems in low/middle-income countries bear the health costs when panel precedent delays or blocks generic entry (e.g., HIV/AIDS, COVID-19, cancer drugs). They have no voice in disputes, no exit from the patent system, and no retaliation capacity. Their situation is structural: the constraint operates through their governments' compelled compliance, but they bear the ultimate cost in morbidity/mortality.
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_access_populations, payer,
    powerless, immediate, trapped, local).

% The broader membership has systemic interest in a functioning dispute settlement system (rule of law, predictability) but divergent interests on TRIPS interpretation. They observe panel precedent, participate in DSB meetings, and may join disputes as third parties. Their analytical seat sees the full structure: coordination function (dispute resolution) + extraction function (precedent drift) + enforcement shift (bilateral substitution post-2019).
narrative_ontology:constraint_stakeholder(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_member_states_general, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpretation of TRIPS obligations across 164 members, resolving disputes that would otherwise require constant renegotiation or unilateral action. The DSU's binding panel rulings (with former Appellate Body review) create legal certainty for trade in intellectual property.
% TRANSFER_FUNCTION: Moves interpretive authority from member states (who negotiated TRIPS flexibilities as self-judging) to dispute panels (who narrow those flexibilities through precedent). The economic transfer: extended patent rents flow from generic producers and public health systems in developing countries to originator firms and high-income states. The political transfer: policy space for public health shifts from national governments to Geneva panels.
% ABSENT_VOICES: Patients and civil society in affected countries are structurally excluded from dispute proceedings — only governments can be parties. Indigenous knowledge holders and traditional medicine practitioners are excluded from patentability interpretations. Future generations (who bear innovation incentive costs/benefits) are excluded. These voices would likely support the public_health_flexibility_reading but have no standing in the DSU.
% DISAPPEARANCE_RATIONALE: If panel interpretive authority vanished overnight, TRIPS interpretation would revert to member-state practice (each government interpreting flexibilities for itself). Generic competition would expand rapidly in countries currently constrained by panel precedent. High-income states would accelerate bilateral TRIPS-plus agreements. The WTO's dispute settlement function would collapse or transform. The global pharmaceutical market would reorganize around national/regional patent regimes rather than a single interpretive hierarchy.
% FOUNDING_PROBLEM: After TRIPS negotiation (1994), members needed a mechanism to ensure consistent interpretation of the Agreement's minimum standards without constant renegotiation. The DSU provided binding panel rulings with appellate review, creating a rule-of-law framework for IP trade disputes.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and DSB annual reports attest the dispute settlement system remains active and necessary (200+ disputes since 1995). However, developing country members (India, Brazil, South Africa, African Group) and public health NGOs (MSF, Oxfam, Knowledge Ecology International) corroborate that the founding problem (consistent interpretation) has been overtaken by a new problem: interpretive capture narrowing flexibilities beyond the negotiated text. The 2001 Doha Declaration on TRIPS and Public Health — negotiated by all members — explicitly reaffirmed flexibilities that panels have subsequently narrowed, corroborating the drift from coordination to extraction.
narrative_ontology:disappearance_verdict(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, world_rearranges).
narrative_ontology:founding_problem_status(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 'none', 1).
narrative_ontology:epsilon_provenance(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the interpretive layer's net transfer: panels narrow flexibilities beyond textual minima, extending patent rents to innovators and high-income states. Suppression (0.75) is high because the enforcement mechanism (retaliation) is actively used and the Appellate Body collapse removes the correction channel. Theater ratio (0.22) is low-moderate: dispute resolution is real coordination, but a growing share of panel activity serves precedent-building for narrow readings. Accessibility collapse (0.62) reflects that once a panel ruling establishes precedent, alternative readings are foreclosed for all members. Resistance (0.58) is substantial: developing country coalitions, public health advocates, and some members contest narrow readings, but the institutional machinery continues.
 *
 * PERSPECTIVAL GAP:
 *   From the DSB/institutional seat: this is a rope — genuine coordination solving the 'who interprets TRIPS' problem with rule of law. From the pharmaceutical innovator/high-income seat: this is a beneficial coordination mechanism protecting innovation incentives. From the developing country/generic producer/access population seats: this is a snare — the coordination story (dispute resolution) covers extraction (precedent narrowing flexibilities) enforced by retaliation. The engine computes this seat divergence from the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO DSB (agenda_setter) sits at d ≈ 0.15 (beneficiary of institutional authority). Pharmaceutical innovators and high-income states (beneficiaries) sit at d ≈ 0.2–0.3 (subsidized by the constraint). Low/middle-income governments, generic producers, and access populations (victims) sit at d ≈ 0.7–0.9 (targets of extraction, trapped by retaliation threat and no appeal). The Appellate Body collapse increased d for weaker members by removing the only check on panel overreach — bilateral settlements now substitute, where power asymmetry dictates terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1995): 'How to ensure consistent interpretation of TRIPS across 164 members without endless renegotiation?' That problem remains live (disputes still arise). But the arrangement has accumulated extractive precedent that serves narrow interests beyond the coordination function. The mandate hasn't atrophied — it's been captured. The constraint is tangled_rope, not piton, because the coordination function is still actively used and the extraction is actively maintained, not merely inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the dispute settlement interpretive authority reading a distinct constraint from the TRIPS agreement itself, or merely an enforcement mechanism of the same constraint?',
    'Compare extraction profiles: if dispute panels extract beyond the TRIPS text''s own requirements (e.g., through precedent that narrows flexibilities), the interpretive authority is a separate constraint layer with its own ε.',
    'If separate, this constraint''s claimed_type applies only to the interpretive layer; the TRIPS substantive text would need its own story. If unified, the high extraction reflects the entire regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the interpretive authority constitutes a separable constraint from the substantive TRIPS obligations.').

omega_variable(
    post_appellate_body_drift,
    'How has the Appellate Body''s paralysis since 2019 changed the constraint''s effective extraction and suppression?',
    'Track panel compliance rates, appeal-into-void outcomes, and bilateral settlement patterns post-2019. Compare effective extraction for identical disputes pre- and post-Appellate Body collapse.',
    'If bilateral power substitution amplifies extraction against weaker members, the constraint shifts toward snare. If panel authority degrades uniformly, theater_ratio rises and constraint drifts toward piton.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_appellate_body_drift, empirical, 'Structural drift from multilateral adjudication to bilateral power dynamics.').

omega_variable(
    public_health_flexibility_capture,
    'Have dispute panel rulings systematically narrowed TRIPS flexibilities (Art. 31, Doha Declaration) beyond what the text requires?',
    'Code panel reports 1995–present for interpretation of compulsory licensing, parallel imports, and public health exceptions. Measure narrowing trend against textual baseline.',
    'Systematic narrowing establishes the interpretive layer as extractive overlay (tangled_rope → snare drift). Textual fidelity would support coordination framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_health_flexibility_capture, empirical, 'Whether interpretive precedent has contracted flexibilities beyond textual mandate.').

omega_variable(
    committer_frame_location,
    'This constraint instantiates the dispute_settlement_interpretive_authority reading of the trips_agreement_interpretive_kernel. What structural elements do the sibling readings (strong_exclusivity_reading, public_health_flexibility_reading) change?',
    'Map each sibling''s beneficiary/victim structure, claimed_type, and ε against this reading. The disagreement is located in: (1) who holds interpretive authority (panels vs. members vs. text), (2) whether flexibilities are self-judging or panel-adjudicated, (3) whether enforcement is multilateral or bilateral.',
    'Sibling readings produce different constraint classifications: strong_exclusivity_reading likely snare (high extraction, narrow beneficiaries); public_health_flexibility_reading likely rope or scaffold (coordination with sunset via Doha). This reading (dispute_settlement_interpretive_authority) is tangled_rope — genuine coordination (dispute resolution) with asymmetric extraction (precedent locks in narrow readings).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_location, conceptual, 'Commiter frame: kernel_id=trips_agreement_interpretive_kernel, reading_id=dispute_settlement_interpretive_authority, sibling readings differ on interpretive authority locus, flexibility scope, and enforcement modality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_dsp_tr_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(trips_dsp_tr_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2001, 0.15).
narrative_ontology:measurement(trips_dsp_tr_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(trips_dsp_tr_t2011, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2011, 0.2).
narrative_ontology:measurement(trips_dsp_tr_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2019, 0.21).
narrative_ontology:measurement(trips_dsp_tr_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(trips_dsp_be_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(trips_dsp_be_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2001, 0.51).
narrative_ontology:measurement(trips_dsp_be_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(trips_dsp_be_t2011, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2011, 0.63).
narrative_ontology:measurement(trips_dsp_be_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2019, 0.66).
narrative_ontology:measurement(trips_dsp_be_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(trips_dsp_su_t1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement(trips_dsp_su_t2001, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(trips_dsp_su_t2005, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2005, 0.68).
narrative_ontology:measurement(trips_dsp_su_t2011, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2011, 0.71).
narrative_ontology:measurement(trips_dsp_su_t2019, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2019, 0.73).
narrative_ontology:measurement(trips_dsp_su_t2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.12).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel__public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_substantive_patent_standards).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_dispute_settlement_understanding).

% DUAL FORMULATION NOTE:
% Part of the TRIPS Agreement Interpretive Kernel family. This story models the meta-constraint: DSU grant of binding interpretive authority to panels with retaliation enforcement. The sibling stories model the substantive readings that panels choose between. The kernel itself (trips_agreement_interpretive_kernel) is the stabilized commitment — the TRIPS text + Doha Declaration — that all three readings claim to interpret. This reading's ε (0.68) is higher than the public_health_flexibility_reading's expected ε (~0.25, rope/scaffold) but lower than strong_exclusivity_reading's expected ε (~0.75, snare), because this reading includes genuine dispute resolution coordination alongside extractive precedent-building.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.15).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, powerful, 0.25).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, moderate, 0.75).
constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
