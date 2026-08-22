% ============================================================================
% CONSTRAINT STORY: climate_response_action__adaptation_priority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_response_action__adaptation_priority, []).

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
 *   constraint_id: climate_response_action__adaptation_priority
 *   human_readable: Adaptation-Priority Climate Response
 *   domain: climate_policy/political_economy/intergenerational_ethics
 *
 * SUMMARY:
 *   The adaptation-priority reading of climate response emerged after the
 *   2009 Copenhagen failure as the pragmatic track: accept that temperature
 *   rise is inevitable, mobilize finance for protection, prioritize the
 *   vulnerable. But the structural delta reveals a tangled rope — the
 *   coordination function (mobilizing $540B/year for universal protection) is
 *   real and necessary, yet it operates through a finance architecture that
 *   extracts from the Global South via co-financing requirements,
 *   debt-financed adaptation, and the $350B annual gap. The reading's moral
 *   claim ('protect the vulnerable') is the very mechanism that legitimizes
 *   the extraction: vulnerability becomes the collateral for finance that
 *   does not arrive, while the mitigation deferral embedded in 'accepting
 *   temperature rise' transfers compounding costs to future generations in
 *   the Global South. The engine computes this from the structural data — the
 *   claimed type (tangled_rope) and metrics are authored independently.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_response_action__adaptation_priority, 0.68).
domain_priors:suppression_score(climate_response_action__adaptation_priority, 0.42).
domain_priors:theater_ratio(climate_response_action__adaptation_priority, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, extractiveness, 0.68).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(climate_response_action__adaptation_priority, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_response_action__adaptation_priority, tangled_rope).
narrative_ontology:human_readable(climate_response_action__adaptation_priority, "Adaptation-Priority Climate Response").
narrative_ontology:topic_domain(climate_response_action__adaptation_priority, "climate_policy/political_economy/intergenerational_ethics").

domain_priors:requires_active_enforcement(climate_response_action__adaptation_priority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(climate_response_action__adaptation_priority, '51e8c6dc-e96f-4911-8f17-7afcfd2d0754').
narrative_ontology:cs_kernel_codification('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', formalized).
narrative_ontology:cs_authority_grounding('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', lineage).
narrative_ontology:cs_interpretation_layer_present('51e8c6dc-e96f-4911-8f17-7afcfd2d0754').
narrative_ontology:cs_reading_relation('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', climate_response_action__mitigation_priority, coexists_with).
narrative_ontology:cs_reading_relation('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', climate_response_action__degrowth_transformation, influences).
narrative_ontology:cs_axiom('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', foundational, temperature_rise_acceptance_is_realist).
narrative_ontology:cs_axiom_status(temperature_rise_acceptance_is_realist, holdable).
narrative_ontology:cs_axiom_grounding('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', temperature_rise_acceptance_is_realist, instrumental).
narrative_ontology:cs_axiom('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', foundational, protection_of_vulnerable_is_moral_imperative).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', protection_of_vulnerable_is_moral_imperative, deontological).
narrative_ontology:cs_axiom('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', secondary, adaptation_finance_architecture_is_sufficient_mechanism).
narrative_ontology:cs_axiom_status(adaptation_finance_architecture_is_sufficient_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', adaptation_finance_architecture_is_sufficient_mechanism, conventional).
narrative_ontology:cs_reference_frame('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', post_copenhagen_pragmatic_adaptation_track).
narrative_ontology:cs_drift_state('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', contemporary_gst_loss_and_damage_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('51e8c6dc-e96f-4911-8f17-7afcfd2d0754', '').
narrative_ontology:cs_kernel_id(climate_response_action__adaptation_priority, climate_response_action).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, multilateral_development_banks).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, climate_finance_institutions).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, large_engineering_firms).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, developed_nation_governments).
narrative_ontology:constraint_beneficiary(climate_response_action__adaptation_priority, insurance_reinsurance_sector).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, developing_nation_governments).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, least_developed_countries).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, small_island_developing_states).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_response_action__adaptation_priority, future_generations_global_south).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, climate_justice_requires_protection_now).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, adaptation_is_moral_imperative).
narrative_ontology:constraint_vindicates(climate_response_action__adaptation_priority, temperature_rise_acceptance_is_realist).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer adaptation finance facilities (Green Climate Fund, Adaptation Fund, World Bank climate windows). Set eligibility criteria, co-financing requirements, and project pipelines. Collect management fees and institutional prestige from administering the adaptation architecture. Can redirect portfolios to mitigation or other windows if adaptation proves politically difficult.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, multilateral_development_banks, agenda_setter,
    institutional, generational, arbitrage, global).

% Operationalize adaptation funding through blended finance, guarantees, and concessional lending. Earn fees on structuring deals, gain mandate expansion from the adaptation priority narrative. Their business model scales with the adaptation finance pipeline; they face no direct climate risk from project failures.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_finance_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, climate_finance_institutions, beneficiary).

% Win contracts for sea walls, resilient infrastructure, early warning systems, and urban adaptation megaprojects. The adaptation priority reading creates a $540B/year addressable market. They can pivot to other infrastructure sectors if adaptation funding stalls; their exit is mobile at the firm level though individual projects are sunk.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, large_engineering_firms, beneficiary,
    powerful, biographical, mobile, global).

% Provide the bulk of public adaptation finance (pledged $100B/year, largely unmet). Adaptation priority lets them meet climate obligations through capital exports rather than domestic emissions cuts. They control the purse strings and set conditionality; their fiscal capacity insulates them from the protection gaps they help create. Can shift narrative to mitigation or loss-and-damage if adaptation becomes inconvenient.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developed_nation_governments, beneficiary,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, developed_nation_governments, agenda_setter).

% Adaptation investments directly reduce their catastrophe loss ratios and protect asset values in exposed regions. They advise on risk modeling for adaptation projects and benefit from public funding of protective infrastructure that privatizes resilience gains. Can withdraw coverage from unadapted zones — their exit is mobile and they price the adaptation gap into premiums.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, insurance_reinsurance_sector, beneficiary,
    powerful, biographical, mobile, global).

% Face the $350B North-South financing gap directly. Must co-finance adaptation projects (often 20-40% of costs) while servicing existing debt and meeting development needs. Limited fiscal space forces tradeoffs between adaptation, health, education, and mitigation. Borrowing for adaptation increases debt vulnerability; grant finance is insufficient. Exit is constrained — they cannot leave the climate system, and leaving the finance architecture means losing even inadequate support.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, developing_nation_governments, payer,
    moderate, biographical, constrained, national).

% Contribute least to historical emissions but face highest adaptation needs relative to GDP. Depend almost entirely on grant finance which is chronically short. Co-financing requirements are often waived in principle but enforced in practice through project design. No fiscal capacity to self-fund; trapped in a finance architecture they did not design and cannot influence. Their voice in agenda-setting is minimal.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, least_developed_countries, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, least_developed_countries, excluded).

% Existential adaptation needs (relocation, freshwater, coastal protection) exceed any conceivable finance flow. Adaptation priority reading accepts temperature rise that may render their territories uninhabitable — the protection promised is structurally insufficient for their survival. Trapped geographically and financially; exit from the climate system is literal impossibility. Their moral claim is loud but structurally powerless.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, small_island_developing_states, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(climate_response_action__adaptation_priority, small_island_developing_states, excluded).

% Informal settlement dwellers, smallholder farmers, outdoor workers, Indigenous communities in climate-exposed regions. Bear the protection disparities directly — adaptation projects prioritize urban centers, economic assets, and measurable outcomes over marginalized communities. No voice in project selection; no exit from exposure. The reading's 'prioritizing vulnerable populations' is the very mechanism that renders their vulnerability legible for extraction.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, climate_vulnerable_populations, payer,
    powerless, biographical, trapped, local).

% Inherit the compound costs of accepted temperature rise: higher warming locked in by mitigation deferral, degraded ecosystems, reduced development options, and adaptation debt. Not represented in current negotiations; their interests are asserted by proxies with conflicting mandates. The adaptation priority reading structurally transfers costs to them by accepting higher future warming as the price of current protection for some.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, future_generations_global_south, excluded,
    powerless, generational, trapped, global).

% Track the adaptation finance gap, document protection disparities, challenge the narrative that adaptation substitutes for mitigation. Provide the evidentiary basis for the structural delta this reading creates. No formal power but shape discourse and legal challenges. Exit is analytical — they observe the constraint from outside its operational logic.
narrative_ontology:constraint_stakeholder(climate_response_action__adaptation_priority, global_civil_society_climate_justice, observer,
    organized, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mobilizes global capital for immediate protective infrastructure, creating a shared framework for vulnerability assessment, project prioritization, and finance allocation that did not exist at this scale before Paris Agreement.
% TRANSFER_FUNCTION: Moves $540B/year (target) from public and private capital in developed nations through multilateral channels to adaptation projects in developing nations, with a $350B/year shortfall borne as foregone protection, increased debt, and lost development by the Global South.
% ABSENT_VOICES: Future generations (especially Global South) who inherit the mitigation debt; communities displaced by maladaptation projects; Indigenous peoples whose territories host adaptation infrastructure without consent; fossil fuel workers in developing nations whose transition is unfunded while adaptation is prioritized.
% DISAPPEARANCE_RATIONALE: If adaptation-priority framing vanished, the $540B/year finance architecture would lose its organizing logic; developed nations would face pressure to close the mitigation gap instead; developing nations would lose even inadequate adaptation finance but gain moral leverage for mitigation and loss-and-damage; the North-South financing gap would be reframed rather than managed.
% FOUNDING_PROBLEM: After Copenhagen 2009 and the failure of binding mitigation targets, the climate regime needed a functional track that could deliver visible results without requiring consensus on emissions cuts — adaptation filled that vacuum by offering tangible projects, measurable finance flows, and a narrative of 'protecting the vulnerable' that all parties could endorse.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the UNFCCC secretariat's own history (adaptation as the 'action agenda' track post-Copenhagen), by developing country negotiators who accepted adaptation finance as the achievable half of a broken bargain, and by climate finance scholars (e.g., Roberts & Weikmans, 2021; Bhattacharya et al., 2022) who document the regime's structural pivot from mitigation enforcement to adaptation mobilization. No party outside the beneficiary set claims the founding problem is 'live' in its original form — mitigation failure is now acknowledged, but adaptation priority has become self-justifying.
narrative_ontology:disappearance_verdict(climate_response_action__adaptation_priority, world_rearranges).
narrative_ontology:founding_problem_status(climate_response_action__adaptation_priority, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(climate_response_action__adaptation_priority, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(climate_response_action__adaptation_priority, 'none', 1).
narrative_ontology:epsilon_provenance(climate_response_action__adaptation_priority, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_response_action__adaptation_priority_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_response_action__adaptation_priority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(climate_response_action__adaptation_priority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects the structural transfer: the finance gap is not accidental but built into the architecture (co-financing, loan-heavy instruments, conditionality). Suppression (0.42) is moderate — alternatives (mitigation-priority, degrowth) are not banned but are structurally disadvantaged by the finance architecture's momentum and the sunk costs of adaptation institutions. Theater ratio (0.38) captures the growing gap between 'protecting the vulnerable' rhetoric and the protection disparities in project allocation (urban over rural, measurable over transformative, economic assets over lives). Accessibility collapse (0.55) reflects that once the adaptation-priority frame is accepted, mitigation-first and degrowth alternatives become cognitively and institutionally harder to pursue — but not impossible. Resistance (0.61) is high and rising: climate justice movements, LDC negotiators, and loss-and-damage advocates actively contest the frame.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (MDBs, developed nations), this is a rope — genuine coordination mobilizing unprecedented finance for protection. From the payer seat (LDCs, SIDS, vulnerable populations), it is a snare — the coordination story is cover for a finance architecture that extracts via debt, conditionality, and the mitigation deferral that locks in their future harm. The engine computes this seat divergence from power/exit asymmetries and beneficiary/victim declarations; the claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The agenda-setters (MDBs, climate finance institutions, developed nations) sit at the beneficiary end of directionality — they control the architecture, collect fees/prestige/mandate, and face mobile exit. The payers (developing nations, LDCs, SIDS, vulnerable populations) sit at the target end — they bear the financing gap, co-financing burdens, protection disparities, and intergenerational costs with trapped or constrained exit. Insurance sector and engineering firms are beneficiaries with mobile exit. Future generations and civil society are excluded observers. The North-South financing gap is the structural manifestation of this directional asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-Copenhagen need for a functional track) is contested — mitigation failure is real, but the adaptation architecture has become self-perpetuating. The mandate has not resolved: the protection gap widens, the finance gap persists, and the mitigation deferral compounds. This is not pure extraction (coordination function is real) nor pure coordination (extraction is structural) — hence tangled_rope. The mandatrophy is unresolved because the constraint still solves a real problem (immediate protection needs) while its extraction mechanism (finance architecture) has outgrown its justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    adaptation_mitigation_substitutability,
    'Does the adaptation-priority reading structurally substitute for mitigation (crowding out emissions cuts by absorbing finance and political attention), or does it genuinely complement mitigation in a portfolio approach?',
    'Counterfactual analysis: in scenarios where adaptation finance reaches $540B/year, does mitigation ambition increase, decrease, or stay constant? Track NDC revisions correlated with adaptation finance flows.',
    'If substitutive, the reading''s acceptance of temperature rise is self-fulfilling — the constraint creates the warming it claims to adapt to. This would shift classification toward snare. If complementary, the tangled_rope classification holds: real coordination with real extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(adaptation_mitigation_substitutability, empirical, 'Whether adaptation finance crowds out or enables mitigation ambition').

omega_variable(
    protection_disparity_mechanism,
    'Are the protection disparities (urban over rural, measurable over transformative, economic assets over lives) an inevitable feature of project-based adaptation finance, or a contingent outcome of current institutional design?',
    'Compare adaptation project portfolios across different finance channels (GCF vs. bilateral vs. private). If disparities persist across all channels, they are structural to project-based adaptation. If some channels achieve equitable protection, disparities are design choices.',
    'If structural, the reading''s ''prioritizing vulnerable populations'' claim is internally contradicted by its own operational logic — a false summit signal. If contingent, the extraction could be reduced without losing the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_disparity_mechanism, conceptual, 'Whether protection disparities are structural or design-contingent').

omega_variable(
    intergenerational_cost_transfer,
    'How much of the future warming cost (accepted by this reading) falls on Global South future generations vs. Global North future generations, and is this distribution acknowledged in the reading''s moral framework?',
    'Integrated assessment modeling with regional disaggregation: compare warming damages under adaptation-priority pathways (higher warming, lower mitigation) vs. mitigation-priority pathways, distributed by region and generation.',
    'If the intergenerational transfer is heavily North-to-South and unacknowledged, the reading''s justice claim is structurally undermined — this would be a conceptual omega supporting snare reclassification. If acknowledged and compensated (e.g., through loss-and-damage finance), the tangled_rope holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_cost_transfer, empirical, 'Distribution of accepted future warming costs across generations and regions').

omega_variable(
    kernel_reading_relations,
    'What are the structural relationships between this adaptation_priority reading and its sibling readings (mitigation_priority, degrowth_transformation) within the climate_response_action kernel?',
    'Analyze whether any single policy framework can coherently hold adaptation_priority and mitigation_priority (coexists_with), or whether accepting temperature rise as inevitable (adaptation_priority) logically forecloses limiting warming to 1.5°C (mitigation_priority). Assess whether adaptation_priority''s finance architecture influences degrowth_transformation''s political viability.',
    'Determines cs_structure.reading_relations: forecloses, coexists_with, or influences. If adaptation_priority forecloses mitigation_priority, the kernel has a genuine logical fracture. If they coexist_with, the kernel hosts a stable multi-reading dispute. If adaptation_priority influences degrowth_transformation by absorbing finance, that is a structural pressure without foreclosure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relations between sibling readings of the climate_response_action kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_response_action__adaptation_priority, 2009, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_adaptation_tr_t2009, climate_response_action__adaptation_priority, theater_ratio, 2009, 0.15).
narrative_ontology:measurement(climate_adaptation_tr_t2013, climate_response_action__adaptation_priority, theater_ratio, 2013, 0.22).
narrative_ontology:measurement(climate_adaptation_tr_t2015, climate_response_action__adaptation_priority, theater_ratio, 2015, 0.28).
narrative_ontology:measurement(climate_adaptation_tr_t2018, climate_response_action__adaptation_priority, theater_ratio, 2018, 0.33).
narrative_ontology:measurement(climate_adaptation_tr_t2021, climate_response_action__adaptation_priority, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(climate_adaptation_tr_t2024, climate_response_action__adaptation_priority, theater_ratio, 2024, 0.38).

% Extraction over time
narrative_ontology:measurement(climate_adaptation_be_t2009, climate_response_action__adaptation_priority, base_extractiveness, 2009, 0.35).
narrative_ontology:measurement(climate_adaptation_be_t2013, climate_response_action__adaptation_priority, base_extractiveness, 2013, 0.42).
narrative_ontology:measurement(climate_adaptation_be_t2015, climate_response_action__adaptation_priority, base_extractiveness, 2015, 0.48).
narrative_ontology:measurement(climate_adaptation_be_t2018, climate_response_action__adaptation_priority, base_extractiveness, 2018, 0.55).
narrative_ontology:measurement(climate_adaptation_be_t2021, climate_response_action__adaptation_priority, base_extractiveness, 2021, 0.62).
narrative_ontology:measurement(climate_adaptation_be_t2024, climate_response_action__adaptation_priority, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(climate_adaptation_su_t2009, climate_response_action__adaptation_priority, suppression_requirement, 2009, 0.25).
narrative_ontology:measurement(climate_adaptation_su_t2013, climate_response_action__adaptation_priority, suppression_requirement, 2013, 0.3).
narrative_ontology:measurement(climate_adaptation_su_t2015, climate_response_action__adaptation_priority, suppression_requirement, 2015, 0.35).
narrative_ontology:measurement(climate_adaptation_su_t2018, climate_response_action__adaptation_priority, suppression_requirement, 2018, 0.38).
narrative_ontology:measurement(climate_adaptation_su_t2021, climate_response_action__adaptation_priority, suppression_requirement, 2021, 0.4).
narrative_ontology:measurement(climate_adaptation_su_t2024, climate_response_action__adaptation_priority, suppression_requirement, 2024, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_response_action__adaptation_priority, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_response_action__adaptation_priority, 0.15).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__mitigation_priority).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, climate_response_action__degrowth_transformation).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, international_climate_finance_architecture).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, loss_and_damage_finance).
narrative_ontology:affects_constraint(climate_response_action__adaptation_priority, sovereign_debt_climate_vulnerability).

% DUAL FORMULATION NOTE:
% Part of the climate_response_action constraint family (kernel_id: climate_response_action). This adaptation_priority reading accepts temperature rise as inevitable and centers protective investment; mitigation_priority centers emissions cuts; degrowth_transformation centers structural economic transformation. The three readings share the kernel (the need for climate response) but instantiate structurally distinct constraints with different ε, beneficiaries, victims, and type classifications. Linked via affects_constraints for contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_response_action__adaptation_priority, institutional, 0.15).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, powerful, 0.25).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, moderate, 0.75).
constraint_indexing:directionality_override(climate_response_action__adaptation_priority, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
