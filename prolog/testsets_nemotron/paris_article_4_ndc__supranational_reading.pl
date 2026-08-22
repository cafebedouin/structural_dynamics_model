% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_paris_article_4_ndc__supranational_reading, []).

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
 *   constraint_id: paris_article_4_ndc__supranational_reading
 *   human_readable: Paris Agreement Article 4 NDCs — Supranational Binding Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint story models the supranational reading of Paris Agreement
 *   Article 4: Nationally Determined Contributions (NDCs) are legally binding
 *   commitments on a ratcheting trajectory toward net-zero, enforceable
 *   through international accountability mechanisms including transparency
 *   frameworks, global stocktakes, and potential state responsibility for
 *   non-compliance. The reading treats the Paris Agreement as creating a
 *   supranational climate legal order where sovereignty over energy systems
 *   is progressively constrained by treaty obligations. This is one of three
 *   contested readings of the same kernel (paris_article_4_ndc) — the others
 *   being the sovereigntist reading (NDCs as voluntary pledges preserving
 *   energy sovereignty) and the equity reading (NDCs requiring structural
 *   CBDR-RC differentiation). The supranational reading's epsilon is high
 *   because it instantiates a constraint system where states face
 *   reputational, financial, and potentially legal sanctions for trajectory
 *   deviation; carbon-intensive industries face regulatory phase-out; and
 *   wealth transfers from North to South are institutionalized through
 *   climate finance and carbon market mechanisms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.82).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDCs — Supranational Binding Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '89a8e787-3982-414b-a0ee-0616ebf0008c').
narrative_ontology:cs_kernel_codification('89a8e787-3982-414b-a0ee-0616ebf0008c', formalized).
narrative_ontology:cs_authority_grounding('89a8e787-3982-414b-a0ee-0616ebf0008c', lineage).
narrative_ontology:cs_interpretation_layer_present('89a8e787-3982-414b-a0ee-0616ebf0008c').
narrative_ontology:cs_reading_relation('89a8e787-3982-414b-a0ee-0616ebf0008c', paris_article_4_ndc__sovereigntist_reading, coexists_with).
narrative_ontology:cs_reading_relation('89a8e787-3982-414b-a0ee-0616ebf0008c', paris_article_4_ndc__equity_reading, influences).
narrative_ontology:cs_axiom('89a8e787-3982-414b-a0ee-0616ebf0008c', foundational, ndcs_are_legally_binding_outcome_obligations).
narrative_ontology:cs_axiom_status(ndcs_are_legally_binding_outcome_obligations, holdable).
narrative_ontology:cs_axiom_grounding('89a8e787-3982-414b-a0ee-0616ebf0008c', ndcs_are_legally_binding_outcome_obligations, conventional).
narrative_ontology:cs_axiom('89a8e787-3982-414b-a0ee-0616ebf0008c', foundational, ratchet_mechanism_creates_progressive_obligation_escalation).
narrative_ontology:cs_axiom_status(ratchet_mechanism_creates_progressive_obligation_escalation, holdable).
narrative_ontology:cs_axiom_grounding('89a8e787-3982-414b-a0ee-0616ebf0008c', ratchet_mechanism_creates_progressive_obligation_escalation, conventional).
narrative_ontology:cs_axiom('89a8e787-3982-414b-a0ee-0616ebf0008c', secondary, international_accountability_includes_state_responsibility_for_non_compliance).
narrative_ontology:cs_axiom_status(international_accountability_includes_state_responsibility_for_non_compliance, holdable).
narrative_ontology:cs_axiom_grounding('89a8e787-3982-414b-a0ee-0616ebf0008c', international_accountability_includes_state_responsibility_for_non_compliance, conventional).
narrative_ontology:cs_reference_frame('89a8e787-3982-414b-a0ee-0616ebf0008c', paris_agreement_2015_adoption).
narrative_ontology:cs_drift_state('89a8e787-3982-414b-a0ee-0616ebf0008c', post_gst1_2023, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89a8e787-3982-414b-a0ee-0616ebf0008c', '2026-08-04T14:30:00Z').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, climate_vulnerable_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, green_technology_sectors).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_financial_institutions).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, carbon_market_intermediaries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developing_states_without_transition_capital).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, energy_sovereignty_advocates).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, international_legally_binding_climate_obligations).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, common_but_differentiated_responsibilities_progressive_interpretation).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, net_zero_by_midcentury_as_legal_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Small island states and least developed countries facing existential climate risk. They gain legal standing to demand action, access to climate finance, and moral authority in negotiations. They cannot exit the climate threat itself, and exiting the treaty would forfeit their primary leverage. Their survival depends on the constraint's bindingness.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, climate_vulnerable_states, beneficiary,
    organized, generational, trapped, global).

% Major oil/gas/coal exporters (OPEC+, Russia, Australia, Canada, US). They bear stranded asset risk, revenue collapse, and geopolitical decline. Exit from the constraint means exiting the global energy market or facing carbon border adjustments. Their power lets them resist implementation but not the constraint's existence.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies, payer,
    powerful, biographical, constrained, global).

% Steel, cement, chemicals, aviation, shipping, automotive. They face regulatory phase-out, carbon pricing, technology forcing, and capital reallocation. Exit options are limited: green transition (costly), relocation (carbon border adjustments follow), or political capture of regulators (increasingly difficult under transparency framework).
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    organized, biographical, constrained, global).

% Renewables, batteries, EVs, hydrogen, CCS, efficiency. They capture massive policy-driven demand, subsidies, and carbon market revenues. They have high exit mobility — they can sell to any jurisdiction implementing the constraint. Their interest aligns with constraint tightening.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, green_technology_sectors, beneficiary,
    organized, biographical, mobile, global).

% World Bank, IMF, regional development banks, Green Climate Fund. They design and administer climate finance architecture, set conditionalities, and intermediate carbon finance. They collect fees and expand mandate. They can arbitrage across climate and development portfolios. Their agenda-setting power shapes the constraint's operational form.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_financial_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, international_financial_institutions, beneficiary).

% Middle-income developing countries (India, Indonesia, South Africa, Vietnam, etc.) with growing emissions, limited fiscal space, and technology gaps. They face pressure to peak emissions early without guaranteed finance/technology. Exit is constrained: they need market access and finance, but the constraint's terms may exceed their capacity. They are the pivotal swing seat.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_states_without_transition_capital, payer,
    moderate, biographical, constrained, regional).

% Domestic political movements, industry associations, and sovereignist governments arguing energy policy is exclusive domestic jurisdiction. They are structurally excluded from the supranational reading's framework — their objection is treated as non-compliance rather than legitimate disagreement. They would object to bindingness and ratchet if their voice were counted.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, energy_sovereignty_advocates, excluded,
    moderate, biographical, trapped, national).

% Verra, Gold Standard, brokers, exchanges, corporate buyers. They capture transaction rents from Article 6 markets, corporate net-zero commitments, and compliance demand. They have high mobility across jurisdictions and registries. Their interest is in market expansion and rule standardization, not necessarily environmental integrity.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_market_intermediaries, beneficiary,
    moderate, biographical, mobile, global).

% Technical expert review teams under the Enhanced Transparency Framework, IPCC authors, Climate Action Tracker, UNFCCC secretariat. They assess NDCs, review reports, run stocktakes. They neither collect nor pay — they produce the information that makes enforcement possible. Their analytical seat sees the full structure.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, independent_expert_review_teams, observer,
    analytical, immediate, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective action problem of climate change by establishing a universal, legally binding framework where all states commit to progressive decarbonization trajectories, with transparency and review mechanisms that enable trust and ratcheting ambition over time.
% TRANSFER_FUNCTION: Moves regulatory authority over energy systems from national sovereignty to supranational treaty regime; moves financial capital from North to South (climate finance, carbon markets); moves regulatory costs onto carbon-intensive sectors; moves legal liability onto non-compliant states.
% ABSENT_VOICES: Energy sovereignty advocates (domestic sovereignist movements, fossil fuel worker communities, national oil company workforces) are structurally excluded — their framing of energy policy as exclusive domestic jurisdiction is treated as non-compliance rather than a legitimate interpretive position. Indigenous peoples' right to free prior informed consent on transition projects is often proceduralized rather than substantive. Future generations (the ultimate beneficiaries of net-zero) have no voice in current NDC ambition-setting.
% DISAPPEARANCE_RATIONALE: If the supranational reading vanished overnight, the Paris Agreement would revert to a purely voluntary pledge-and-review system (the sovereigntist reading). Major emitters would weaken NDCs, climate finance flows would collapse without legal obligation, carbon markets would lose compliance demand, and the 1.5°C/2°C trajectory would become unattainable. The global climate governance architecture would reorganize around national discretion.
% FOUNDING_PROBLEM: The Kyoto Protocol's top-down, differentiated binding targets failed because major emitters (US, China, Canada, Japan, Russia) refused or withdrew. The Paris Agreement's founding problem was designing a universal framework that could secure participation of ALL major emitters while still delivering sufficient ambition — achieved through nationally determined contributions with a ratchet mechanism.
% FOUNDING_PROBLEM_CORROBORATION: The UNFCCC secretariat and COP Presidencies attest the founding problem (universal participation) is solved — Paris has near-universal ratification. Climate-vulnerable states and independent analysts (Climate Action Tracker, IPCC) attest the problem is NOT solved — participation without sufficient ambition is a false summit. The supranational reading's proponents (EU, AOSIS, LDCs) argue the founding problem requires bindingness; the sovereigntist reading's proponents (US historical position, China, India, OPEC) argue bindingness was never agreed and would destroy participation. No neutral arbiter has resolved this.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.82, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(paris_article_4_ndc__supranational_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(paris_article_4_ndc__supranational_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint demands transformative economic restructuring — regulatory extinction of carbon-intensive sectors, capital reallocation at trillion-dollar scale, and sovereignty concessions on energy policy — with compliance costs concentrated on specific states and sectors. Suppression (0.78) is high because the constraint's persistence depends on active enforcement: enhanced transparency framework reviews, global stocktake political pressure, Article 6 market exclusion, trade carbon border adjustments, and potential ICJ advisory proceedings. Theater ratio (0.45) is moderate: the coordination function (global temperature goal, technology cooperation, finance mobilization) is real but increasingly overshadowed by performative NDC submissions that mask implementation gaps. Accessibility collapse (0.65) reflects that once a state ratifies Paris, exit is legally possible (Article 28) but politically and economically near-impossible for major economies. Resistance (0.72) captures sustained pushback from fossil fuel exporters, energy sovereignty advocates, and developing states demanding differentiated treatment.
 *
 * PERSPECTIVAL GAP:
 *   From the climate-vulnerable state seat, the constraint appears as rope/scaffold — genuine coordination enabling survival. From the fossil fuel exporter seat, it appears as snare — extraction without consent. From the developing state without capital seat, it appears as tangled rope — coordination demanded but extraction imposed without means. The engine's per-seat computation should reflect this divergence. The supranational reading's claim (tangled rope) reflects the structural reality that both coordination AND asymmetric extraction are present and inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Climate-vulnerable states (AOSIS, LDCs) are primary beneficiaries — they gain legal leverage, finance claims, and survival pathway. Green technology sectors and carbon market intermediaries are secondary beneficiaries — they capture rent from mandated transition. Fossil fuel dependent economies and carbon-intensive industries are primary victims — they bear stranded asset risk, regulatory phase-out, and capital flight. Developing states without transition capital are caught in a double bind: the constraint demands decarbonization but the promised finance/technology transfer is underdelivered. International financial institutions sit ambiguously: they administer climate finance (beneficiary-adjacent) but also enforce conditionality (agenda-setter). The engine computes directionality from these structural positions and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordinating global decarbonization to avoid catastrophic warming) remains live and intensifying — no mandatrophy in the classical sense. However, the constraint's form (NDCs + ratchet) may be mismatched to the problem's scale: the coordination mechanism (voluntary pledges reviewed internationally) was designed for a softer regime; the supranational reading retrofits bindingness onto a structure built for flexibility. This mismatch generates the high theater ratio — the constraint performs bindingness while the enforcement machinery lags. The mandatrophy risk is not that the problem is solved, but that the instrument is structurally inadequate to the reading's own claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_supranational_vs_sovereigntist,
    'Does the Paris Agreement''s text and subsequent practice legally bind parties to achieve their NDCs, or only to pursue domestic measures with the aim of achieving them?',
    'Authoritative interpretation by the CMA (Conference of Parties serving as Meeting of Parties to the Paris Agreement), ICJ advisory opinion, or consensus COP decision on the legal character of Article 4.2.',
    'If legally binding to achieve, non-compliance triggers state responsibility and countermeasures (high extraction on laggards). If only binding to pursue, the constraint is largely performative (low extraction, high theater).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_supranational_vs_sovereigntist, conceptual, 'Legal character of NDCs: binding outcome vs. binding conduct obligation').

omega_variable(
    kernel_reading_supranational_vs_equity,
    'Can a supranational accountability regime operate without structural differentiation between developed and developing states, or does CBDR-RC require permanently distinct obligation tiers?',
    'Negotiated outcome on differentiation in the Global Stocktake and enhanced transparency framework; or judicial determination of CBDR-RC''s operational content.',
    'If differentiation is permanent, the supranational reading''s universal accountability claim fractures — extraction falls asymmetrically on developing states without transition capital. If differentiation is dynamic/progressive, the reading''s coherence improves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_supranational_vs_equity, conceptual, 'Whether supranational accountability is compatible with CBDR-RC''s structural differentiation').

omega_variable(
    extraction_distribution_north_south,
    'Does the supranational reading''s enforcement architecture concentrate extraction costs on carbon-intensive developing economies while benefit capture flows to Northern financial intermediaries?',
    'Empirical tracking of Article 6 carbon market flows, climate finance delivery vs. $100bn pledge, technology transfer metrics, and stranded asset distribution by country income group.',
    'If extraction is regressive (developing states bear transition costs while Northern intermediaries capture rents), the constraint functions as a snare masked as coordination. If distribution is progressive, it approaches genuine tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_distribution_north_south, empirical, 'Distributional incidence of supranational NDC enforcement across North-South divide').

omega_variable(
    ratchet_mechanism_credibility,
    'Is the five-year ratchet cycle (NDC submission → Global Stocktake → enhanced NDC) structurally capable of delivering net-zero trajectories, or does it produce performative ambition without implementation?',
    'Longitudinal analysis of NDC ambition cycles (2015, 2020, 2025, 2030) against emissions trajectories and implementation gaps; credibility assessment of domestic enabling legislation.',
    'If ratchet is performative, theater_ratio approaches 1.0 and the constraint is piton/snare. If ratchet drives real decarbonization, theater_ratio falls and coordination function strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_mechanism_credibility, empirical, 'Whether the ratchet mechanism produces real trajectory change or performative cycling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_ndc_supra_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(paris_ndc_supra_tr_t0, observed).
narrative_ontology:measurement(paris_ndc_supra_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement_basis(paris_ndc_supra_tr_t5, observed).
narrative_ontology:measurement(paris_ndc_supra_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(paris_ndc_supra_tr_t10, observed).
narrative_ontology:measurement(paris_ndc_supra_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(paris_ndc_supra_tr_t15, projected).
narrative_ontology:measurement(paris_ndc_supra_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(paris_ndc_supra_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(paris_ndc_supra_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(paris_ndc_supra_be_t0, observed).
narrative_ontology:measurement(paris_ndc_supra_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(paris_ndc_supra_be_t5, observed).
narrative_ontology:measurement(paris_ndc_supra_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(paris_ndc_supra_be_t10, observed).
narrative_ontology:measurement(paris_ndc_supra_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.73).
narrative_ontology:measurement_basis(paris_ndc_supra_be_t15, projected).
narrative_ontology:measurement(paris_ndc_supra_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement_basis(paris_ndc_supra_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(paris_ndc_supra_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(paris_ndc_supra_su_t0, observed).
narrative_ontology:measurement(paris_ndc_supra_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement_basis(paris_ndc_supra_su_t5, observed).
narrative_ontology:measurement(paris_ndc_supra_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement_basis(paris_ndc_supra_su_t10, observed).
narrative_ontology:measurement(paris_ndc_supra_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(paris_ndc_supra_su_t15, projected).
narrative_ontology:measurement(paris_ndc_supra_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement_basis(paris_ndc_supra_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(paris_article_4_ndc__supranational_reading, 0.12).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_6_carbon_markets).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_9_climate_finance).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_13_transparency_framework).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, eu_carbon_border_adjustment_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, international_court_of_justice_climate_advisory_opinion).

% DUAL FORMULATION NOTE:
% This constraint is one of three in the paris_article_4_ndc constraint family. The supranational_reading claims high extraction bindingness; the sovereigntist_reading claims low extraction voluntarism; the equity_reading claims differentiated extraction. They share the same kernel (Article 4 NDCs) but instantiate different constraints with different epsilon values, beneficiary/victim structures, and types. The supranational reading structurally influences the carbon markets, finance, and transparency constraints because its bindingness claim raises the stakes for those mechanisms' operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, institutional, 0.15).
constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, organized, 0.35).
constraint_indexing:directionality_override(paris_article_4_ndc__supranational_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
