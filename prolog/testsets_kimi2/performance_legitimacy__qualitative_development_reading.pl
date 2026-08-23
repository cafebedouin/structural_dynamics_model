% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__qualitative_development_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: performance_legitimacy__qualitative_development_reading
 *   human_readable: High-Quality Development Performance Legitimacy
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   In the Chinese party-state, legitimacy has historically been tied to
 *   economic performance. Since roughly 2015, a 'high-quality development'
 *   paradigm has displaced raw GDP growth as the primary metric of political
 *   success. This constraint story models that paradigm as an institutional
 *   arrangement: central authorities reallocate credit, regulatory
 *   forbearance, and political status toward innovation-driven and
 *   sustainable sectors, while deliberately tolerating lower headline growth
 *   rates. Traditional manufacturing and property-dependent local governments
 *   bear the costs through credit starvation, fiscal stress, and
 *   deprioritized political standing. The constraint is actively enforced
 *   through cadre evaluation, industrial policy, and financial regulation.
 *   This is one reading of the contested performance_legitimacy kernel;
 *   siblings include quantitative_growth_reading, techno_nationalist_reading,
 *   and livelihood_security_reading.
 *
 * KEY AGENTS:
 *   - Central state authority (agenda_setter/institutional): Sets and enforces the high-quality development paradigm through planning, cadre evaluation, and credit guidance.
 *   - High-tech sectors (beneficiary/powerful): Receive preferential policy, subsidy, and market access under the innovation-first regime.
 *   - State innovation ecosystem (beneficiary/institutional): State-backed funds and vehicles whose survival is tethered to the paradigm.
 *   - Venture capital infrastructure (beneficiary/powerful): Captures policy-tailored exit and funding opportunities.
 *   - Traditional manufacturing (payer/organized): Bears credit denial and regulatory costs of the sectoral shift.
 *   - Property-dependent local governments (payer/institutional): Suffer fiscal collapse from the end of land-sales growth model.
 *   - Development economists (observer/analytical): External analytical seat assessing efficiency claims.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__qualitative_development_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__qualitative_development_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(performance_legitimacy__qualitative_development_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__qualitative_development_reading, "High-Quality Development Performance Legitimacy").
narrative_ontology:topic_domain(performance_legitimacy__qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__qualitative_development_reading, 'c7295e6f-66a6-4987-b480-08c5f24a88ef').
narrative_ontology:cs_kernel_codification('c7295e6f-66a6-4987-b480-08c5f24a88ef', formalized).
narrative_ontology:cs_authority_grounding('c7295e6f-66a6-4987-b480-08c5f24a88ef', lineage).
narrative_ontology:cs_interpretation_layer_present('c7295e6f-66a6-4987-b480-08c5f24a88ef').
narrative_ontology:cs_reading_relation('c7295e6f-66a6-4987-b480-08c5f24a88ef', performance_legitimacy__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('c7295e6f-66a6-4987-b480-08c5f24a88ef', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('c7295e6f-66a6-4987-b480-08c5f24a88ef', performance_legitimacy__livelihood_security_reading, influences).
narrative_ontology:cs_axiom('c7295e6f-66a6-4987-b480-08c5f24a88ef', foundational, innovation_supersedes_growth_as_primary_legitimacy_metric).
narrative_ontology:cs_axiom_status(innovation_supersedes_growth_as_primary_legitimacy_metric, holdable).
narrative_ontology:cs_axiom_grounding('c7295e6f-66a6-4987-b480-08c5f24a88ef', innovation_supersedes_growth_as_primary_legitimacy_metric, conventional).
narrative_ontology:cs_axiom('c7295e6f-66a6-4987-b480-08c5f24a88ef', foundational, sustainability_as_non_negotiable_structural_constraint).
narrative_ontology:cs_axiom_status(sustainability_as_non_negotiable_structural_constraint, holdable).
narrative_ontology:cs_axiom_grounding('c7295e6f-66a6-4987-b480-08c5f24a88ef', sustainability_as_non_negotiable_structural_constraint, conventional).
narrative_ontology:cs_reference_frame('c7295e6f-66a6-4987-b480-08c5f24a88ef', high_quality_development_paradigm).
narrative_ontology:cs_drift_state('c7295e6f-66a6-4987-b480-08c5f24a88ef', contemporary_implementation_phase, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c7295e6f-66a6-4987-b480-08c5f24a88ef', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, high_tech_sectors).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(performance_legitimacy__qualitative_development_reading, venture_capital_infrastructure).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, traditional_manufacturing).
narrative_ontology:constraint_victim(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets national development priorities through Party congresses, five-year plans, and cadre evaluation criteria. Reorients credit policy, regulatory approval, and political discipline away from GDP growth toward innovation and sustainability metrics. Administers the constraint through state-backed funds and industrial policy vehicles.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, central_state_authority, agenda_setter,
    institutional, generational, constrained, national).

% Receives preferential credit, subsidy, and IPO regulatory fast-tracking under the high-quality development paradigm. Benefits from the strategic priority shift but remains dependent on state policy continuity for market access and capital.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, high_tech_sectors, beneficiary,
    powerful, biographical, constrained, national).

% State-backed funds, research institutes, and industrial policy vehicles that channel capital into targeted sectors. Their mandate, budget, and institutional survival derive directly from the high-quality development paradigm.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, state_innovation_ecosystem, beneficiary,
    institutional, biographical, constrained, national).

% Private and hybrid capital pools prioritized by policy to fund industrial upgrading. Operate with regulatory tailwinds and exit opportunities created by the constraint, though they retain cross-border mobility.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, venture_capital_infrastructure, beneficiary,
    powerful, biographical, mobile, national).

% Faces credit starvation, rising environmental compliance costs, and closure pressure under the new policy regime. Formerly the primary growth engine, now structurally deprioritized and unable to access the policy support flowing to high-tech sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, traditional_manufacturing, payer,
    organized, biographical, constrained, national).

% Local governments whose fiscal health and political advancement historically depended on land sales and real estate development. Now face revenue collapse, debt stress, and cadre evaluation that no longer rewards property-driven GDP growth, while their expenditure obligations remain fixed.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, constrained, regional).

% Analyze whether the structural transformation represents genuine efficiency gains and sustainability improvements, or a reallocation of rents to politically favored sectors under the cover of developmental necessity.
narrative_ontology:constraint_stakeholder(performance_legitimacy__qualitative_development_reading, development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates national resource allocation away from saturated property and heavy-industry sectors toward innovation-driven, sustainable industries by redefining state priorities, credit channels, and cadre incentives.
% TRANSFER_FUNCTION: Moves fiscal revenue, credit access, regulatory forbearance, and political status from traditional manufacturing and property-dependent local governments to high-tech sectors, state-backed innovation vehicles, and venture capital infrastructure.
% ABSENT_VOICES: Workers displaced from traditional manufacturing without retraining pathways; local officials who would prefer GDP-first growth but are excluded from central policy discourse; environmental groups concerned that 'green' industrial policy reproduces extractive patterns; independent economists skeptical of state-directed innovation efficiency.
% DISAPPEARANCE_RATIONALE: If the high-quality development mandate vanished overnight, credit would revert to property and heavy industry, local governments would restart land-sales cycles, innovation subsidies would collapse, and the stock market's STAR board and VC ecosystem would lose their policy rationale. The entire political economy of the past decade would unwind.
% FOUNDING_PROBLEM: The post-2008 growth model based on real estate and infrastructure investment was generating unsustainable debt, environmental degradation, and technological dependence; the economy required structural transformation to escape the middle-income trap.
% FOUNDING_PROBLEM_CORROBORATION: Central state planners and high-tech beneficiaries attest the problem remains live and the solution is working. Traditional manufacturers, distressed local governments, and some international economists attest the problem has been replaced by new distortions; independent TFP studies and local debt analyses from outside the benefiting coalition corroborate the extraction reading.
narrative_ontology:disappearance_verdict(performance_legitimacy__qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__qualitative_development_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__qualitative_development_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__qualitative_development_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__qualitative_development_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint systematically moves fiscal and credit resources from one set of actors to another through state power, not market choice. Suppression (0.62) is substantial but not total: local governments and traditional firms resist through shadow banking and off-balance-sheet activity, but the central state progressively hardens enforcement. Theater_ratio (0.45) reflects growing performative complianceâinnovation projects that game metrics without genuine technological breakthrough. Accessibility_collapse (0.58) captures that the old growth model is increasingly inaccessible as a legitimate alternative, though workarounds persist. Resistance (0.52) is moderate: victims are organized and vocal but politically subordinate. The claim/metric independence is maintained: the constraint is claimed as tangled_rope because it has a genuine coordination function alongside asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   The central state and beneficiary seats experience the constraint as necessary structural transformation and legitimate industrial policy; the traditional manufacturing and local government seats experience it as expropriation of their former policy privileges and fiscal base. The engine computes this divergence from directionalities: agenda-setter and beneficiaries sit near d=0.0, while payers with constrained exit sit near d=1.0. The analytical observer sits near d=0.5 with analytical-grade exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (high_tech_sectors, state_innovation_ecosystem, venture_capital_infrastructure) receive policy rents and low derived directionality. The agenda_setter (central_state_authority) captures legitimacy and administrative control, sitting near the beneficiary end despite its enforcement costs. Payers (traditional_manufacturing, property_dependent_local_governments) bear fiscal and operational costs with constrained institutional exit, yielding high directionality and amplified effective extraction. No override is needed because beneficiary/victim declarations plus exit options structurally derive the correct asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids misclassification as a pure snare because there is a demonstrable coordination problemâstructural transformation away from an unsustainable property-and-debt growth model is a genuine collective-action challenge requiring centralized reallocation. It avoids misclassification as a pure rope because the extraction is asymmetric: identifiable victims bear concentrated costs while beneficiaries capture concentrated gains. The active enforcement requirement (cadre evaluation, credit guidance) confirms the hybrid tangled_rope classification rather than spontaneous coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    growth_quality_tradeoff_empirical,
    'Does the reallocation from traditional manufacturing and property sectors to high-tech innovation produce net efficiency and sustainability gains, or does it primarily transfer resources to politically favored sectors without proportional productivity improvement?',
    'Independent total factor productivity analysis by sector, environmental outcome metrics, and comparison with counterfactual growth paths.',
    'If productivity gains are illusory, the constraint operates closer to pure extraction; if real, the coordination function is structurally genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_quality_tradeoff_empirical, empirical, 'Whether innovation reallocation produces real efficiency gains or rent transfers.').

omega_variable(
    local_government_fiscal_viability,
    'Can local governments dependent on land sales and traditional industry taxes transition to viable revenue models under this constraint, or is their fiscal distress structural rather than transitional?',
    'Longitudinal fiscal data from local governments, debt restructuring outcomes, and cadre evaluation adaptation over a full political cycle.',
    'If distress is structural, the victim set is permanently trapped; if transitional, the constraint carries scaffold-like properties.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_government_fiscal_viability, empirical, 'Whether local fiscal collapse is transitional or permanent.').

omega_variable(
    legitimacy_kernel_stability,
    'Is the qualitative development reading structurally stable against the quantitative growth and livelihood security siblings, or is it a transitional coalition that will revert under growth or social pressure?',
    'Policy continuity across leadership transitions, crisis response patterns, and factional power shifts within the party-state.',
    'If unstable, the constraint''s long-term extraction profile is lower than current measures suggest; if stable, it represents a durable reallocation regime.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_kernel_stability, conceptual, 'Stability of the qualitative development legitimacy claim within the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__qualitative_development_reading, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perfleg_qualdev_tr_t0, performance_legitimacy__qualitative_development_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perfleg_qualdev_tr_t2, performance_legitimacy__qualitative_development_reading, theater_ratio, 2, 0.2).
narrative_ontology:measurement(perfleg_qualdev_tr_t4, performance_legitimacy__qualitative_development_reading, theater_ratio, 4, 0.3).
narrative_ontology:measurement(perfleg_qualdev_tr_t6, performance_legitimacy__qualitative_development_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(perfleg_qualdev_tr_t8, performance_legitimacy__qualitative_development_reading, theater_ratio, 8, 0.42).
narrative_ontology:measurement(perfleg_qualdev_tr_t9, performance_legitimacy__qualitative_development_reading, theater_ratio, 9, 0.45).

% Extraction over time
narrative_ontology:measurement(perfleg_qualdev_be_t0, performance_legitimacy__qualitative_development_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(perfleg_qualdev_be_t2, performance_legitimacy__qualitative_development_reading, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(perfleg_qualdev_be_t4, performance_legitimacy__qualitative_development_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(perfleg_qualdev_be_t6, performance_legitimacy__qualitative_development_reading, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(perfleg_qualdev_be_t8, performance_legitimacy__qualitative_development_reading, base_extractiveness, 8, 0.65).
narrative_ontology:measurement(perfleg_qualdev_be_t9, performance_legitimacy__qualitative_development_reading, base_extractiveness, 9, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perfleg_qualdev_su_t0, performance_legitimacy__qualitative_development_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(perfleg_qualdev_su_t2, performance_legitimacy__qualitative_development_reading, suppression_requirement, 2, 0.45).
narrative_ontology:measurement(perfleg_qualdev_su_t4, performance_legitimacy__qualitative_development_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(perfleg_qualdev_su_t6, performance_legitimacy__qualitative_development_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(perfleg_qualdev_su_t8, performance_legitimacy__qualitative_development_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(perfleg_qualdev_su_t9, performance_legitimacy__qualitative_development_reading, suppression_requirement, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel, which decomposes into four structurally distinct legitimacy claims. The qualitative_development_reading differs from its siblings in prioritizing innovation, sustainability, and efficiency over raw growth rates, with distinct beneficiary and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
