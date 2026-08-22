% ============================================================================
% CONSTRAINT STORY: paris_article_4_ndc__supranational_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Paris Agreement Article 4 NDC â Supranational Binding Reading
 *   domain: international_climate_governance/treaty_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the supranational reading of Paris Agreement
 *   Article 4 NDCs: a binding, ratcheting commitment architecture with
 *   international accountability mechanisms (global stocktake, enhanced
 *   transparency framework, and anticipated financial or reputational
 *   sanctions). It treats NDCs not as voluntary pledges but as enforceable
 *   trajectories toward net-zero, institutionalizing wealth transfers from
 *   developed to developing states and imposing regulatory extinction on
 *   carbon-intensive industries. The kernel is contested: the sovereigntist
 *   reading holds NDCs as voluntary and sovereignty-preserving, while the
 *   equity reading emphasizes Common But Differentiated Responsibilities as
 *   the primary interpretive lens. This reading foregrounds supranational
 *   accountability and bindingness.
 *
 * KEY AGENTS:
 *   - developed_states: Primary payer (institutional/constrained) â bears compliance costs, finance obligations, and sovereignty erosion
 *   - developing_states: Primary beneficiary (institutional/constrained) â receives transfers and differential treatment within the ratchet
 *   - carbon_intensive_industries: Secondary payer (powerful/trapped) â faces regulatory extinction and asset stranding
 *   - international_climate_bureaucracy: Agenda setter with secondary beneficiary status (institutional/analytical) â administers accountability mechanisms and derives institutional authority
 *   - clean_energy_sector: Secondary beneficiary (organized/mobile) â captures redirected capital and policy support
 *   - fossil_fuel_dependent_economies: Tertiary payer (moderate/constrained) â fiscal and export revenue under threat
 *   - sovereigntist_states: Excluded voice (institutional/constrained) â rejects bindingness but remains structurally marginalized
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(paris_article_4_ndc__supranational_reading, 0.8).
domain_priors:suppression_score(paris_article_4_ndc__supranational_reading, 0.78).
domain_priors:theater_ratio(paris_article_4_ndc__supranational_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(paris_article_4_ndc__supranational_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(paris_article_4_ndc__supranational_reading, tangled_rope).
narrative_ontology:human_readable(paris_article_4_ndc__supranational_reading, "Paris Agreement Article 4 NDC â Supranational Binding Reading").
narrative_ontology:topic_domain(paris_article_4_ndc__supranational_reading, "international_climate_governance/treaty_law/political_economy").

domain_priors:requires_active_enforcement(paris_article_4_ndc__supranational_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(paris_article_4_ndc__supranational_reading, '49672b7e-92b7-460a-a199-962b7a63f25b').
narrative_ontology:cs_kernel_codification('49672b7e-92b7-460a-a199-962b7a63f25b', formalized).
narrative_ontology:cs_authority_grounding('49672b7e-92b7-460a-a199-962b7a63f25b', lineage).
narrative_ontology:cs_interpretation_layer_present('49672b7e-92b7-460a-a199-962b7a63f25b').
narrative_ontology:cs_reading_relation('49672b7e-92b7-460a-a199-962b7a63f25b', paris_article_4_ndc__sovereigntist_reading, forecloses).
narrative_ontology:cs_reading_relation('49672b7e-92b7-460a-a199-962b7a63f25b', paris_article_4_ndc__equity_reading, coexists_with).
narrative_ontology:cs_axiom('49672b7e-92b7-460a-a199-962b7a63f25b', foundational, supranational_accountability_overrides_sovereignty).
narrative_ontology:cs_axiom_status(supranational_accountability_overrides_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('49672b7e-92b7-460a-a199-962b7a63f25b', supranational_accountability_overrides_sovereignty, deontological).
narrative_ontology:cs_axiom('49672b7e-92b7-460a-a199-962b7a63f25b', foundational, ratchet_mechanism_prevents_free_riding).
narrative_ontology:cs_axiom_status(ratchet_mechanism_prevents_free_riding, holdable).
narrative_ontology:cs_axiom_grounding('49672b7e-92b7-460a-a199-962b7a63f25b', ratchet_mechanism_prevents_free_riding, instrumental).
narrative_ontology:cs_reference_frame('49672b7e-92b7-460a-a199-962b7a63f25b', paris_treaty_binding_trajectory).
narrative_ontology:cs_drift_state('49672b7e-92b7-460a-a199-962b7a63f25b', post_global_stocktake_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('49672b7e-92b7-460a-a199-962b7a63f25b', '').
narrative_ontology:cs_kernel_id(paris_article_4_ndc__supranational_reading, paris_article_4_ndc).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, developing_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, clean_energy_sector).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, small_island_states).
narrative_ontology:constraint_beneficiary(paris_article_4_ndc__supranational_reading, international_climate_bureaucracy).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, developed_states).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, carbon_intensive_industries).
narrative_ontology:constraint_victim(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, international_accountability_doctrine).
narrative_ontology:constraint_vindicates(paris_article_4_ndc__supranational_reading, ratchet_mechanism_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the primary burden of stringent emission reduction targets, climate finance obligations, and technology transfer mandates under the accountability framework. Face reputational and financial sanctions for non-compliance. Wealth transfers to developing states are institutionalized as binding obligations rather than voluntary aid.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developed_states, payer,
    institutional, generational, constrained, global).

% Receive institutionalized wealth transfers, technology support, and differential treatment under the ratcheting framework. Subject to less stringent immediate mitigation obligations but locked into the long-term net-zero trajectory. Benefit from the enforcement pressure directed at developed nations.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, developing_states, beneficiary,
    institutional, generational, constrained, global).

% Face regulatory extinction and asset stranding as the binding net-zero trajectory tightens. Extraction occurs through forced compliance costs, exclusion from finance, and direct regulatory phase-out. Exit is limited by the global scope of the regime and the collapse of alternative markets.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, carbon_intensive_industries, payer,
    powerful, biographical, trapped, global).

% States whose fiscal stability and export revenues depend on fossil fuel extraction face existential threat from the binding trajectory. They resist the supranational constraint but are subject to its transparency and accountability mechanisms, with limited exit without sovereign default or isolation.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, fossil_fuel_dependent_economies, payer,
    moderate, generational, constrained, national).

% Administers the NDC registry, enhanced transparency framework, and global stocktake. Interprets compliance gaps and sets the agenda for ambition ratcheting. Derives institutional budget, staffing, and treaty-political authority from the persistence of the supranational reading.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, international_climate_bureaucracy, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(paris_article_4_ndc__supranational_reading, international_climate_bureaucracy, beneficiary).

% Captures redirected public and private capital as carbon-intensive alternatives are phased out by the binding trajectory. Benefits from regulatory certainty, subsidy flows, and policy preferences embedded in the NDC implementation architecture.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, clean_energy_sector, beneficiary,
    organized, biographical, mobile, global).

% Low-lying island nations whose territorial survival depends on global emissions constraints. Act as normative entrepreneurs within the regime, leveraging existential vulnerability to demand ambition. Benefit from the supranational enforcement structure as their primary hedge against sea-level rise.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, small_island_states, beneficiary,
    organized, civilizational, constrained, global).

% States and political blocs that reject the binding supranational authority of the Paris framework over national energy policy. Their interpretive claims are formally heard in COP processes but structurally marginalized by the transparency and accountability architecture, which treats sovereignty as subordinate to the net-zero trajectory.
narrative_ontology:constraint_stakeholder(paris_article_4_ndc__supranational_reading, sovereigntist_states, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the global collective action problem of atmospheric greenhouse gas emissions by binding states to a ratcheting trajectory of nationally determined contributions that increase in ambition over time, preventing free-riding and ensuring mutual accountability toward net-zero.
% TRANSFER_FUNCTION: Moves capital, technology, and compliance costs from developed states and carbon-intensive industries to developing states and clean energy sectors, enforced through reputational exposure, financial sanctions, and regulatory phase-out.
% ABSENT_VOICES: Sovereigntist states and fossil-fuel dependent economies are formally included in negotiations but their rejection of bindingness is structurally marginalized by the accountability framework; carbon-intensive industries are excluded from NDC formulation but are the primary regulatory target.
% DISAPPEARANCE_RATIONALE: If the supranational bindingness and accountability mechanisms vanished, the ratcheting trajectory would stall, climate finance flows would collapse, carbon-intensive industries would see immediate regulatory reprieve, and global emissions governance would fragment into unenforceable voluntary pledges.
% FOUNDING_PROBLEM: The tragedy of the commons in the global atmospheric sink: no individual state has sufficient incentive to unilaterally reduce emissions, leading to free-riding and collective catastrophic warming.
% FOUNDING_PROBLEM_CORROBORATION: Climate scientists and small island states attest the problem remains live and acute. Sovereigntist states and fossil fuel exporters contest that the supranational enforcement mechanism is a proportional or legitimate response, arguing the founding problem does not justify the specific wealth-transfer and sovereignty-cost architecture; independent atmospheric monitoring data corroborates the emissions gap, while independent legal scholarship disputes the bindingness interpretation.
narrative_ontology:disappearance_verdict(paris_article_4_ndc__supranational_reading, world_rearranges).
narrative_ontology:founding_problem_status(paris_article_4_ndc__supranational_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(paris_article_4_ndc__supranational_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(paris_article_4_ndc__supranational_reading, 'none', 1).
narrative_ontology:epsilon_provenance(paris_article_4_ndc__supranational_reading, 0.8, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.80 at interval end) because the regime structurally moves wealth and compliance costs from developed states and carbon-intensive industries to developing states and clean energy sectors, backed by sanctions. Suppression is high (0.78) because persistence depends on actively marginalizing sovereigntist alternatives and enforcing compliance through reputational and financial mechanisms. Theater ratio is moderate (0.30) and declining: early NDC cycles were heavy on pledging and light on delivery, but the supranational reading's accountability mechanisms are designed to reduce theater over time by narrowing the say-do gap. Accessibility collapse is substantial (0.65): the Paris architecture has effectively crowded out alternative global climate governance frameworks since 2015. Resistance is high (0.70) from fossil-fuel dependent actors and sovereigntist blocs. The measurement series share a single time grid (2015â2050) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The developed state seat experiences the constraint as sovereignty-costly extraction and regulatory burden; the developing state seat experiences it as legitimate burden-sharing and institutionalized support. The carbon-intensive industry seat experiences regulatory extinction, while the clean energy sector experiences subsidized expansion. The international bureaucracy sees necessary coordination; sovereigntist dissenters see illegitimate supranational overreach. These divergences are structural â the engine computes them from the beneficiary/victim asymmetries and differentiated exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Developed states, carbon-intensive industries, and fossil-fuel dependent economies are directional targets (d near 1.0): they pay the transfers, bear the sanctions, and face stranded assets with constrained or trapped exit. Developing states, clean energy sectors, and small island states are directional beneficiaries (d near 0.0): they receive finance, policy space, and survival rents. The international bureaucracy sits near the agenda-setter middle but leans beneficiary through institutional budget and authority capture. Sovereigntist states are excluded from the directional calculus but their suppression is the enforcement precondition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem â global emissions free-riding â remains live, preventing a simple piton classification. However, the supranational reading carries a mandatrophy risk: if the ratcheting mechanism becomes decoupled from actual emissions reductions and devolves into a wealth-transfer and reporting ritual without atmospheric impact, it could degrade toward a snare or piton. The authored metrics show rising extraction and stable enforcement, suggesting the coordination function is still active but increasingly coupled to asymmetric extraction â the Tangled Rope signature. The declining theater ratio indicates the constraint is moving from pledge-theater toward enforcement, consistent with a live but extractive coordination structure rather than an atrophied one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bindingness_vs_voluntarism,
    'Is the supranational bindingness of NDCs a genuine enforceable legal constraint, or a reputational coordination mechanism dressed in treaty language?',
    'Systematic analysis of ICJ advisory potential, treaty non-compliance procedures, and actual sanctioning history under the Paris transparency framework.',
    'If bindingness is purely reputational, suppression and extractiveness are lower than measured; if genuinely enforceable, the classification as high-extraction enforcement mechanism is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_vs_voluntarism, conceptual, 'Ambiguity between legal bindingness and reputational coordination in the supranational reading.').

omega_variable(
    transfer_efficiency,
    'Do institutionalized North-South wealth transfers under the NDC regime produce measurable decarbonization outcomes, or do they constitute extraction with coordination cover?',
    'Independent audit of climate finance flows against recipient-country emissions trajectories and project implementation rates.',
    'If transfers fail to reduce emissions, the wealth-transfer component reclassifies from necessary coordination cost toward pure extraction, tightening the snare signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transfer_efficiency, empirical, 'Whether climate finance transfers achieve their stated coordination goal or serve as extraction.').

omega_variable(
    sovereignty_cost_ambiguity,
    'Is the sovereignty cost borne by developed states a necessary price of global coordination, or asymmetric extraction without proportional return?',
    'Comparative cost-benefit analysis across Annex I and non-Annex I parties measuring compliance costs against avoided climate damages attributable to the regime.',
    'If costs are grossly disproportionate to attributable benefits for payers, the constraint shifts toward snare; if proportional, it remains a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_cost_ambiguity, conceptual, 'Whether sovereignty erosion is a coordination cost or asymmetric extraction.').

omega_variable(
    sibling_reading_interaction,
    'Does the supranational reading''s enforcement architecture foreclose the sovereigntist reading, or do they coexist in different institutional registers?',
    'Analysis of treaty reservations, Article 28 withdrawal clauses, and domestic constitutional challenges to NDC enforcement.',
    'If foreclosed, the kernel is structurally unstable; if coexisting, the supranational reading is one of several live institutional interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_interaction, conceptual, 'Structural relationship between the supranational and sovereigntist readings of the NDC kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(paris_article_4_ndc__supranational_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(paris_ndc_supra_tr_t0, paris_article_4_ndc__supranational_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(paris_ndc_supra_tr_t5, paris_article_4_ndc__supranational_reading, theater_ratio, 5, 0.5).
narrative_ontology:measurement(paris_ndc_supra_tr_t10, paris_article_4_ndc__supranational_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(paris_ndc_supra_tr_t15, paris_article_4_ndc__supranational_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(paris_ndc_supra_tr_t20, paris_article_4_ndc__supranational_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(paris_ndc_supra_tr_t25, paris_article_4_ndc__supranational_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement(paris_ndc_supra_tr_t30, paris_article_4_ndc__supranational_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement(paris_ndc_supra_tr_t35, paris_article_4_ndc__supranational_reading, theater_ratio, 35, 0.3).

% Extraction over time
narrative_ontology:measurement(paris_ndc_supra_be_t0, paris_article_4_ndc__supranational_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(paris_ndc_supra_be_t5, paris_article_4_ndc__supranational_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(paris_ndc_supra_be_t10, paris_article_4_ndc__supranational_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(paris_ndc_supra_be_t15, paris_article_4_ndc__supranational_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(paris_ndc_supra_be_t20, paris_article_4_ndc__supranational_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(paris_ndc_supra_be_t25, paris_article_4_ndc__supranational_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement(paris_ndc_supra_be_t30, paris_article_4_ndc__supranational_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(paris_ndc_supra_be_t35, paris_article_4_ndc__supranational_reading, base_extractiveness, 35, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(paris_ndc_supra_su_t0, paris_article_4_ndc__supranational_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(paris_ndc_supra_su_t5, paris_article_4_ndc__supranational_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(paris_ndc_supra_su_t10, paris_article_4_ndc__supranational_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(paris_ndc_supra_su_t15, paris_article_4_ndc__supranational_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(paris_ndc_supra_su_t20, paris_article_4_ndc__supranational_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(paris_ndc_supra_su_t25, paris_article_4_ndc__supranational_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(paris_ndc_supra_su_t30, paris_article_4_ndc__supranational_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(paris_ndc_supra_su_t35, paris_article_4_ndc__supranational_reading, suppression_requirement, 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(paris_article_4_ndc__supranational_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__sovereigntist_reading).
narrative_ontology:affects_constraint(paris_article_4_ndc__supranational_reading, paris_article_4_ndc__equity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the supranational reading of the Paris Article 4 NDC kernel, which decomposes into three structurally distinct constraints: the sovereigntist reading (voluntary pledges), the equity reading (CBDR-differentiated responsibilities), and this supranational reading (binding trajectory with international accountability). Each has a distinct epsilon, beneficiary/victim structure, and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
