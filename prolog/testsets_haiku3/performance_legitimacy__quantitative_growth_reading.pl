% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: GDP Growth Rate Legitimacy (Quantitative Reading)
 *   domain: political_economy/state_capitalism/development_planning
 *
 * SUMMARY:
 *   A state legitimizes its continued rule by maintaining and publicizing
 *   high GDP growth rates as proof of regime efficacy and progress. This
 *   reading instantiates ONE way of grounding legitimacy in performance—the
 *   quantitative-growth reading—which treats growth as the primary metric by
 *   which all development priorities are evaluated and public officials are
 *   measured. This reading is contested by three sibling readings: the
 *   livelihood-security reading (which grounds legitimacy in tangible
 *   improvements citizens experience directly), the qualitative-development
 *   reading (which emphasizes innovation and sustainability over raw
 *   expansion), and the techno-nationalist reading (which grounds legitimacy
 *   in technological self-sufficiency). The constraint described here is the
 *   growth reading's operative structure: it creates a coherent institutional
 *   logic in which industrial-export sectors and local officials measured on
 *   growth targets form a coalition, while small producers, rural workers,
 *   and environmental commons are systematically deprioritized because they
 *   do not register as growth contributions. The extraction escalates over
 *   the interval as growth becomes harder to achieve through productivity
 *   gains and requires increasing capital intensity and resource mobilization
 *   (debt, land expropriation, environmental depletion) to sustain headline
 *   numbers.
 *
 * KEY AGENTS:
 *   - state_central_authority: Sets and enforces growth targets; agenda-setter; trapped in the growth narrative as primary legitimacy source
 *   - local_government_officials: Measured on GDP contribution; identity fused with growth metrics; beneficiary when meeting targets but payer when targets drive extraction from constituents
 *   - industrial_export_complex: Primary beneficiary; receives preferential credit, land, investment; benefits from state capacity committed to their expansion
 *   - state_financial_system: Payer; required to fund growth through directed lending and carries stranded assets as overcapacity persists
 *   - small_domestic_producers and rural_agricultural_workforce: Payers; resources diverted to export sectors; livelihoods compressed; trapped in regions where growth targets drive extraction
 *   - environmental_commons and future_fiscal_sustainability: Non-agent payers; costs externalized to commons and future generations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.72).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.64).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "GDP Growth Rate Legitimacy (Quantitative Reading)").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political_economy/state_capitalism/development_planning").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, '0d61dc51-f770-47d3-8f3d-e572cafce443').
narrative_ontology:cs_kernel_codification('0d61dc51-f770-47d3-8f3d-e572cafce443', formalized).
narrative_ontology:cs_authority_grounding('0d61dc51-f770-47d3-8f3d-e572cafce443', extraction).
narrative_ontology:cs_interpretation_layer_present('0d61dc51-f770-47d3-8f3d-e572cafce443').
narrative_ontology:cs_reading_relation('0d61dc51-f770-47d3-8f3d-e572cafce443', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d61dc51-f770-47d3-8f3d-e572cafce443', performance_legitimacy__qualitative_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d61dc51-f770-47d3-8f3d-e572cafce443', performance_legitimacy__techno_nationalist_reading, influences).
narrative_ontology:cs_axiom('0d61dc51-f770-47d3-8f3d-e572cafce443', foundational, quantitative_expansion_proof_of_efficacy).
narrative_ontology:cs_axiom_status(quantitative_expansion_proof_of_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('0d61dc51-f770-47d3-8f3d-e572cafce443', quantitative_expansion_proof_of_efficacy, empirically_contingent).
narrative_ontology:cs_axiom('0d61dc51-f770-47d3-8f3d-e572cafce443', secondary, gdp_rate_separable_from_composition).
narrative_ontology:cs_axiom_status(gdp_rate_separable_from_composition, overridden).
narrative_ontology:cs_axiom_grounding('0d61dc51-f770-47d3-8f3d-e572cafce443', gdp_rate_separable_from_composition, empirically_contingent).
narrative_ontology:cs_reference_frame('0d61dc51-f770-47d3-8f3d-e572cafce443', growth_model_legitimacy_state).
narrative_ontology:cs_drift_state('0d61dc51-f770-47d3-8f3d-e572cafce443', contemporary_high_debt_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d61dc51-f770-47d3-8f3d-e572cafce443', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, small_domestic_producers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, rural_agricultural_workforce).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, environmental_commons).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_fiscal_sustainability).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, international_credit_markets).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, state_financial_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Stakes its legitimacy claim on the GDP growth metric as the primary proof of regime efficacy. Sets growth targets annually, allocates credit and state investment to pursue them, and evaluates all subordinate officials by their contribution to meeting targets. The metric becomes the governance lens through which all other policy is filtered. Cannot exit without surrendering its primary legitimacy narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_central_authority, agenda_setter,
    institutional, generational, trapped, national).

% Career advancement, resource allocation, and political survival depend on provincial/municipal GDP growth performance metrics. They benefit from the system insofar as meeting growth targets brings career mobility and central budget transfers. They also bear costs when pursuing growth requires extracting resources from their constituents or masking local livelihood decay. Identity as 'effective administrator' becomes fused with GDP-target achievement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, payer).

% Benefits from preferential access to subsidized credit, land acquisition, energy allocation, and export promotion. Growth targets drive investment flows directly into their sectors (automotive, electronics, petrochemicals, heavy machinery). Sustains competitive advantage through state capacity committed to their expansion, even when overcapacity and export subsidy require structural transfers from other regions or sectors.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    institutional, generational, arbitrage, global).

% Channeled to fund growth targets through directed lending mandates to priority sectors, below-market credit rates, and contingent liabilities for infrastructure buildout. Carries accumulating bad loans and stranded assets as overcapacity persists. Cannot refuse lending without violating the legitimacy mandate; cannot price risk accurately without signaling doubt about the growth narrative.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_financial_system, payer,
    institutional, generational, constrained, national).

% Access to credit and resources compressed by prioritization of export-oriented industrial sectors and state-supported enterprises. Face predatory pricing from subsidized competitors. Land and environmental commons are diverted to industrial-export infrastructure. Cannot exit because local livelihoods depend on the same regional economy where growth targets drive extraction.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, small_domestic_producers, payer,
    powerless, biographical, trapped, local).

% Faces land expropriation for industrial zones and infrastructure serving export corridors. Agricultural pricing compressed by export-sector competition for resources and labor. Rural youth migration accelerates not from rural development pull but from rural resource depletion. Identity tied to agrarian livelihood is eroded as policy treats agriculture as a cost to minimize in the growth calculation.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, rural_agricultural_workforce, payer,
    powerless, biographical, identity_locked, regional).

% Water, air, soil, and biodiversity treated as externalities or free inputs into the growth calculation. Industrial-export model drives high-intensity resource extraction and manufacturing, with environmental costs borne by commons rather than production cost. No collective seat to negotiate; damage compounds across the interval.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, environmental_commons, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, environmental_commons).

% Debt accumulation, contingent liabilities in the financial system, and unrecovered public investment create fiscal commitments that crowd out future social spending. The growth model requires continuous credit expansion to sustain the appearance of expansion; slowing that growth exposes accumulated losses. Future generations bear the deferred fiscal adjustment.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_fiscal_sustainability, payer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(performance_legitimacy__quantitative_growth_reading, future_fiscal_sustainability).

% Advocates for livelihood-first development, environmental sustainability, or technological self-sufficiency find their policy proposals systematically deprioritized because they do not contribute to headline GDP growth or are difficult to quantify in growth terms. Their presence in policy spaces is ceremonial; decision-making power concentrates in industrial-export and finance constituencies whose interests align with the growth reading.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, alternative_development_constituencies, excluded,
    organized, generational, constrained, national).

% Sustained access to state credit demand and refinancing needs created by growth-driven investment and infrastructure buildout. Growth-narrative legitimacy reduces perceived risk and lowers borrowing costs. Benefits from the financial depth created by continuous credit expansion to support growth targets, even as stranded assets and overcapacity accumulate.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, international_credit_markets, beneficiary,
    institutional, biographical, arbitrage, global).

% Examines the constraint's structural logic: whether GDP growth rates actually deliver the legitimacy outcomes (poverty reduction, employment, state capacity) they are claimed to deliver, and whether the measurement framework itself (GDP aggregation, sector-weighting) is neutral or embeds the preferences of growth-benefiting constituencies.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the state's need for a quantifiable, aggregable metric of regime efficacy that permits decentralized targeting (local officials can pursue their own growth paths provided they hit targets) and integrates dispersed investment decisions (industrial, infrastructure, financial) into a single evaluative framework. GDP growth rate aggregates diverse activities into a single comparable number; it coordinates expectation-setting and resource allocation across sectors and regions.
% TRANSFER_FUNCTION: Redirects credit, land, energy, and state investment toward industrial-export sectors and away from domestic consumption and rural/small-producer sectors. Moves environmental costs and future fiscal obligations from visible calculation into commons or deferred liability. Transfers decision-making authority from stakeholders experiencing livelihood tradeoffs (rural workers, small producers, local communities) to technocratic planners optimizing the growth metric.
% ABSENT_VOICES: Rural producers and small domestic enterprises are present in the constituent population but excluded from the governing coalition that sets growth priorities. Alternative development frameworks (livelihood-first, environmental sustainability, endogenous innovation) are voiced in policy settings but systematically deprioritized because they do not operationalize as headline GDP contributions. Future generations and environmental commons have no seated advocate.
% DISAPPEARANCE_RATIONALE: If the GDP growth legitimacy framework disappeared overnight, investment priorities would shift away from industrial-export sectors toward sectors that deliver direct livelihood improvements (healthcare, education, housing, local agriculture support). Local governance evaluation would shift to livelihood and environmental metrics rather than growth metrics. Stranded assets and accumulated debt would surface as fiscal crises rather than being masked by continuous growth narratives. The financial system's lending patterns and risk pricing would recalibrate to actual productive capacity rather than growth assumptions.
% FOUNDING_PROBLEM: Post-1979 state sought a measurable, objective metric to demonstrate regime efficacy and economic progress after decades of stagnation and poverty; growth rate provided a quantifiable proof distinct from ideological claims and verifiable through trade data, industrial output, and investment flows.
% FOUNDING_PROBLEM_CORROBORATION: The state's own officials and economic planners attest that the founding problem (proving progress through measurable expansion) remains live and that the growth metric has delivered poverty reduction and infrastructure modernization. International development economists and the state's own retrospective analyses document that early growth phases (1980s–2000s) did correlate with employment creation and living-standard gains for large populations. However, subsequent analyses by development critics, environmental economists, and fiscal auditors from outside the benefiting constituencies attest that the founding problem has shifted: current growth requires debt accumulation and resource extraction at rates disconnected from underlying productivity gains, suggesting the model's original problem-solving power has degraded while the metric persists.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.45 to 0.72 over the interval because growth targets become progressively harder to meet through productivity gains alone and require intensifying capital mobilization, land expropriation, and credit expansion. Theater rises from 0.25 to 0.58 as the constraint's machinery becomes increasingly devoted to sustaining the growth narrative (statistical smoothing, sector-selection that boosts headline GDP, infrastructure projects with low productive return but high GDP-accounting impact) rather than delivering underlying improvements. Suppression rises from 0.48 to 0.68 because the state must increasingly suppress alternative policy narratives (livelihood-first development, environmental sustainability, technological self-sufficiency) and constrain local officials' ability to deviate from growth targets to pursue constituent priorities. The accessibility_collapse (0.64) reflects that alternatives to growth-target-driven governance exist in theory but are structurally excluded from decision-making power. The resistance (0.71) reflects growing pushback from rural constituencies, environmental movements, and fiscal-sustainability advocates, even as the state's enforcement machinery intensifies.
 *
 * PERSPECTIVAL GAP:
 *   From the state central authority's seat, the growth metric is a genuine coordination innovation—it operationalizes development as a measurable target, permits decentralized decision-making by local officials, and has historically correlated with poverty reduction. From the industrial-export complex's seat, the constraint is coordination that benefits them: it aligns state capacity with their expansion. From the small-producer and rural-workforce seats, the same constraint is pure extraction: growth targets drive resource diversion, land expropriation, and livelihood compression. From the environmental-commons seat (non-agent), the structure is extractive parasitism: costs are invisible in the GDP accounting. The engine computes these divergences from the stakeholder power/exit/role data—the analytical gap is the signature of how the growth reading distributes winners and losers unevenly.
 *
 * DIRECTIONALITY LOGIC:
 *   State central authority: d ≈ 0.15 (beneficiary—the constraint sustains its legitimacy claim, but it also traps it; cannot exit without surrendering the narrative). Industrial-export complex: d ≈ 0.1 (beneficiary—direct resource flows, preferential status). Local government officials: d ≈ 0.5–0.65 (symmetric to slightly-target; benefits from advancement when meeting targets, but increasingly bears costs as growth requires extraction from constituents). Small domestic producers: d ≈ 0.8 (target—compressed credit, resource diversion, predatory competition). Rural agricultural workforce: d ≈ 0.85 (target—land expropriation, livelihood erosion, identity loss). Environmental commons and future fiscal sustainability: d ≈ 0.9 (near-total target—costs externalized, no recovery mechanism). International credit markets: d ≈ 0.15 (beneficiary—sustained access to state credit demand).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—proving regime progress through quantifiable growth—was live and structurally sound in the 1980s–2000s interval: growth correlated with employment creation, infrastructure modernization, and poverty reduction from a very low base. By the current interval (years 0–40 in the measurement series), the founding problem has substantially died: most poverty that could be easily addressed through growth has been, and current growth requires increasing capital intensity, debt accumulation, and resource extraction at rates disconnected from productivity or livelihood gains. Yet the constraint persists not because the founding problem remains live but because the state has become identity-locked to the growth narrative as its primary legitimacy source, and the industrial-export complex and local government officials have become institutional dependents on growth targets for resource allocation and career advancement. The constraint is now a mandatroph: the institutional machinery persists, the measurement framework persists, the extraction persists, but the legitimate problem the machinery was built to solve has largely been solved or transformed. The theater rises because increasing share of enforcement activity defends the narrative against contradictory evidence (environmental damage, debt accumulation, rural livelihood erosion) rather than delivering underlying improvements.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_vs_capital_intensity_drift,
    'As the interval progresses, is rising extractiveness driven by genuine productivity gains (the founding problem of growth delivering prosperity) or by capital intensity and resource mobilization masking stagnating productivity?',
    'Decompose growth into contribution from productivity gains vs. contribution from capital accumulation and resource inputs. If the latter dominates and is accelerating, the extraction is increasingly parasitic on future fiscal adjustment rather than present productivity.',
    'If capital-intensity dominates, the constraint is drifting from tangled_rope (coordination + extraction) toward snare (extraction with coordination narrative as cover). The mandatrophy would be more severe: the legitimacy claim (growth = progress) would be substantially false while the extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_vs_capital_intensity_drift, empirical, 'Whether growth reflects productivity gains or capital mobilization masking productivity stagnation.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Are the four sibling readings (quantitative_growth, livelihood_security, qualitative_development, techno_nationalist) genuinely coexisting as live political positions, or is the quantitative_growth reading foreclosing others through institutional capture?',
    'Examine policy space: Can a regional government pursue livelihood-security metrics without sanctions? Can a ministry advance qualitative-development research without career penalty? If yes, the readings coexist; if no, coexistence is ceremonial and the growth reading forecloses others in practice.',
    'If coexistence is ceremonial (alternatives exist in policy settings but have no decision power), the constraint should be reclassified as a snare with a coordination narrative: the apparent pluralism is theater masking institutional capture by the growth coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, empirical, 'Whether alternative development readings are live policy options or ceremonially present but foreclosed by institutional structure.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of alternative development narratives structural (institutional barriers, exclusion from decision-making) or internalized (local officials and planners believe the growth metric is genuinely the right objective)?',
    'Post-constraint counterfactual: If growth targets were removed but state legitimacy remained at stake, would local officials and planners continue to pursue growth or shift to livelihood/sustainability metrics? If they continue growth out of fear of performance evaluation, suppression is structural; if they continue because they believe it is right, suppression is internalized.',
    'If suppression is substantially internalized, the constraint''s hold is tighter than the structural enforcement alone would suggest—officials would self-enforce the growth priority even without external evaluation. The effective extraction would be higher than the suppression metric indicates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression of alternatives is structural enforcement or internalized belief in growth as objective.').

omega_variable(
    livelihood_security_reading_empirical_grounding,
    'The livelihood_security_reading grounds legitimacy in tangible improvements citizens experience directly. Is this reading empirically refutable (can it be falsified by evidence of stagnating/declining livelihoods) or is it a deontological claim about what legitimacy should mean regardless of outcomes?',
    'Test the livelihood-security reading''s own internal logic: Does it predict that legitimacy should erode if healthcare/education/employment outcomes stagnate despite growth? Or does it hold that legitimacy should rest on livelihood improvements regardless of growth? The answer determines the grounding type.',
    'If livelihood_security is empirically_contingent (refutable by livelihood data), it competes with quantitative_growth on falsifiable grounds and might foreclose it if livelihood evidence becomes undeniable. If it is deontological (normative regardless of outcomes), it coexists with quantitative_growth as an irresolvable moral disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(livelihood_security_reading_empirical_grounding, conceptual, 'Whether the livelihood-security reading is empirically contestable or normatively grounded beyond empirical refutation.').

omega_variable(
    state_institutional_path_dependence,
    'To what extent is the state central authority''s commitment to the growth reading path-dependent (irreversibly locked by prior institutional commitments) versus strategically chosen (could shift to an alternative reading if evidence supported it)?',
    'Examine historical moments when state authorities reconsidered growth targets: Did policy shift based on evidence of declining returns, or did state increase enforcement and narrative defense instead? If the former, the state retains strategic flexibility; if the latter, path-dependence dominates.',
    'If path-dependent, the state''s role shifts from beneficiary choosing the growth reading to trapped agenda-setter locked in its own narrative—directionality would be higher (d nearer 0.5) and the constraint would more closely resemble a snare in which all parties including the rule-setter are partially trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_institutional_path_dependence, empirical, 'Whether state commitment to growth reading is strategically reversible or institutionally path-dependent.').

omega_variable(
    measurement_framework_neutrality,
    'Is the GDP aggregation framework (what activities count as ''growth,'' how sectors are weighted, what is excluded) technically neutral or does it embed the preferences and interests of growth-benefiting constituencies?',
    'Compare GDP-derived growth rankings with alternative development indices (HDI, sustainable development goals, livelihood security metrics): Do states that rank high on GDP also rank high on these alternatives? If divergence is systematic, GDP framework is non-neutral.',
    'If GDP framework is non-neutral (embeds industrial-export and financial-sector preferences), the entire constraint is a structural mechanism for legitimizing extraction through measurement design. The extraction would be understood as built into the framework itself, not as a feature of particular policy choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_framework_neutrality, empirical, 'Whether GDP-based legitimacy framework is technically neutral or embeds the preferences of growth-benefiting sectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.42).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.51).
narrative_ontology:measurement(perf_tr_t32, performance_legitimacy__quantitative_growth_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement(perf_be_t32, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 32, 0.71).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(perf_su_t32, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 32, 0.67).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(performance_legitimacy__quantitative_growth_reading, 0.18).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, state_debt_accumulation__growth_model).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, rural_resource_extraction__infrastructure_corridors).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, industrial_export_subsidy_cascade).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the performance_legitimacy kernel. The quantitative_growth_reading treats GDP growth rate as the primary performance metric through which state legitimacy is grounded. The sibling readings (livelihood_security, qualitative_development, techno_nationalist) instantiate alternative performance metrics and resulting stakeholder coalitions. All four constraints share the same kernel (state legitimacy through performance) but differ in what counts as performance evidence and which constituencies benefit. The network edges indicate downstream constraints affected by this reading's institutional logic: the growth-model drives state debt accumulation (to finance capital-intensive growth), rural resource extraction (to supply industrial corridors), and industrial-export subsidy mechanisms (to sustain headline growth rates). Each of these downstream constraints would have different structure under alternative readings of the performance_legitimacy kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, organized, 0.62).
constraint_indexing:directionality_override(performance_legitimacy__quantitative_growth_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
