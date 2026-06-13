% ============================================================================
% CONSTRAINT STORY: qualitative_development_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualitative_development_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qualitative_development_reading
 *   human_readable: High-Quality Development Legitimacy Framework
 *   domain: political_economy/development_planning/state_capitalism
 *
 * SUMMARY:
 *   This constraint is one reading of the performance_legitimacy kernel — the
 *   contested claim that a state's authority derives from delivering
 *   developmental outcomes. The qualitative_development_reading instantiates
 *   a specific version: legitimacy comes from structural transformation
 *   toward innovation, sustainability, and efficiency, not from aggregate
 *   growth rates. This reading emerged as environmental limits and debt
 *   accumulation made the previous quantitative_growth_reading unsustainable.
 *   The constraint coordinates real investment toward productivity-enhancing
 *   sectors while extracting from legacy industries that no longer serve the
 *   legitimacy narrative. The claim/metric independence is preserved: the
 *   constraint is claimed as tangled_rope (genuine coordination with
 *   asymmetric costs) and the metrics describe substantial extraction and
 *   rising enforcement requirements as the framework matures.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualitative_development_reading, 0.68).
domain_priors:suppression_score(qualitative_development_reading, 0.71).
domain_priors:theater_ratio(qualitative_development_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualitative_development_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(qualitative_development_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(qualitative_development_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualitative_development_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(qualitative_development_reading, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualitative_development_reading, tangled_rope).
narrative_ontology:human_readable(qualitative_development_reading, "High-Quality Development Legitimacy Framework").
narrative_ontology:topic_domain(qualitative_development_reading, "political_economy/development_planning/state_capitalism").

domain_priors:requires_active_enforcement(qualitative_development_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualitative_development_reading, 'b1a2e23b-4e6d-4699-9105-9cee3c7843f3').
narrative_ontology:cs_kernel_codification('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', formalized).
narrative_ontology:cs_authority_grounding('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', practice).
narrative_ontology:cs_interpretation_layer_present('b1a2e23b-4e6d-4699-9105-9cee3c7843f3').
narrative_ontology:cs_reading_relation('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', qualitative_development_reading__quantitative_growth_reading, influences).
narrative_ontology:cs_reading_relation('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', qualitative_development_reading__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', qualitative_development_reading__livelihood_security_reading, coexists_with).
narrative_ontology:cs_axiom('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', foundational, innovation_primacy_over_volume).
narrative_ontology:cs_axiom_status(innovation_primacy_over_volume, holdable).
narrative_ontology:cs_axiom_grounding('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', innovation_primacy_over_volume, instrumental).
narrative_ontology:cs_axiom('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', foundational, sustainability_as_legitimacy_marker).
narrative_ontology:cs_axiom_status(sustainability_as_legitimacy_marker, holdable).
narrative_ontology:cs_axiom_grounding('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', sustainability_as_legitimacy_marker, conventional).
narrative_ontology:cs_reference_frame('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', quantitative_growth_paradigm).
narrative_ontology:cs_drift_state('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', post_environmental_constraint_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b1a2e23b-4e6d-4699-9105-9cee3c7843f3', '').
narrative_ontology:cs_kernel_id(qualitative_development_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualitative_development_reading, state_backed_innovation_ecosystem).
narrative_ontology:constraint_beneficiary(qualitative_development_reading, high_tech_manufacturing_sectors).
narrative_ontology:constraint_beneficiary(qualitative_development_reading, venture_capital_networks).
narrative_ontology:constraint_beneficiary(qualitative_development_reading, strategic_technology_firms).
narrative_ontology:constraint_victim(qualitative_development_reading, traditional_manufacturing_sectors).
narrative_ontology:constraint_victim(qualitative_development_reading, property_dependent_local_governments).
narrative_ontology:constraint_victim(qualitative_development_reading, labor_intensive_exporters).
narrative_ontology:constraint_victim(qualitative_development_reading, commodity_producers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(qualitative_development_reading, urban_professional_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets industrial policy priorities, allocates state capital to innovation sectors, and defines what counts as 'high-quality' development. Justifies tolerance for slower aggregate growth by emphasizing technological sovereignty, environmental sustainability, and productivity gains. Controls credit allocation through state banks and directs resources toward strategic sectors.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, central_planning_authority, agenda_setter,
    institutional, generational, analytical, national).

% Receives preferential credit access, subsidies for R&D, procurement guarantees, and regulatory protection. Includes state-owned enterprises in semiconductors, electric vehicles, renewable energy, and AI. Benefits from the legitimacy framework that prioritizes their sectors as markers of developmental success, insulating them from market discipline.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, state_backed_innovation_ecosystem, beneficiary,
    institutional, generational, mobile, national).

% Private firms in advanced manufacturing that align with state priorities receive subsidized financing, tax incentives, and protection from foreign competition. Their growth metrics become the visible evidence of the development model's success, giving them structural leverage to extract continued support.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, high_tech_manufacturing_sectors, beneficiary,
    powerful, biographical, constrained, national).

% State-guided funds and government-backed venture capital channels flow toward innovation sectors, creating deal flow and exit opportunities. They benefit from the policy framework that treats innovation financing as a public good, while retaining private upside from successful exits.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, venture_capital_networks, beneficiary,
    powerful, biographical, mobile, global).

% Face tightening credit access as state banks redirect capital toward strategic sectors. Environmental compliance costs rise as sustainability becomes a legitimacy marker. Their contribution to employment and exports is discounted in the new framework, which treats them as legacy sectors to be upgraded or phased out.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, traditional_manufacturing_sectors, payer,
    organized, biographical, constrained, regional).

% Lose revenue as land sales decline under the new model's emphasis on sustainable urbanization. Central directives prioritize innovation zones over property development, but local governments still carry debt from infrastructure built under the previous growth model. They bear the fiscal cost of the transition without the tools to fund it.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, property_dependent_local_governments, payer,
    institutional, biographical, trapped, local).

% Employ millions but generate low value-added output. The legitimacy framework treats their employment contribution as less important than innovation metrics, making them vulnerable to credit rationing and regulatory pressure. Their exit options are limited by sunk capital and workforce skills mismatched to high-tech sectors.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, labor_intensive_exporters, payer,
    organized, biographical, constrained, regional).

% Face environmental restrictions and reduced state support as the framework prioritizes efficiency over volume. Their output is necessary for the industrial base but no longer valorized as a development achievement, leaving them structurally subordinated to innovation sectors in resource allocation.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, commodity_producers, payer,
    moderate, biographical, constrained, regional).

% Benefits from employment in innovation sectors, improved environmental quality, and the cultural prestige of working in 'high-quality' industries. Observes the distributional consequences for displaced workers and fiscally strained regions but experiences the framework as delivering on its promises.
narrative_ontology:constraint_stakeholder(qualitative_development_reading, urban_professional_class, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(qualitative_development_reading, urban_professional_class, observer).

% Analyze whether the framework represents genuine structural transformation or rebranded industrial policy that concentrates rents in state-favored sectors. They document the coordination function (channeling investment toward productivity-enhancing sectors) and the extraction function (subsidizing incumbents while imposing costs on legacy sectors).
narrative_ontology:constraint_stakeholder(qualitative_development_reading, international_development_economists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels state and private capital toward sectors with higher productivity potential, coordinates environmental standards to reduce externalities, and aligns industrial policy with long-term technological competitiveness rather than short-term output maximization.
% TRANSFER_FUNCTION: Moves credit access, subsidies, regulatory forbearance, and policy attention from traditional manufacturing and property-driven growth toward innovation sectors, state-backed technology firms, and sustainability-aligned industries. Local governments and labor-intensive sectors bear the fiscal and employment costs of the transition.
% ABSENT_VOICES: Workers displaced from traditional manufacturing, rural populations dependent on commodity production, and local officials whose fiscal capacity depends on the old growth model are structurally excluded from the policy formation process. They would argue for employment protection and fiscal transfers to manage the transition, but the framework treats their concerns as legacy problems rather than ongoing obligations.
% DISAPPEARANCE_RATIONALE: If this legitimacy framework disappeared overnight, state banks would revert to lending to property developers and traditional exporters, environmental enforcement would relax, innovation subsidies would dry up, and local governments would restart land-sale-driven infrastructure cycles. The economy would reorganize around the previous growth model's incentives.
% FOUNDING_PROBLEM: The previous quantitative growth model generated environmental degradation, industrial overcapacity, mounting local government debt, and technological dependence on foreign suppliers. Legitimacy required a new framework that could justify slower aggregate growth while claiming progress on sustainability and innovation.
% FOUNDING_PROBLEM_CORROBORATION: International development institutions, domestic environmental researchers, and technology policy analysts outside the state-backed innovation ecosystem corroborate that the founding problems (environmental limits, debt accumulation, technological dependence) remain unresolved and require structural adjustment. The framework's claim to address them is contested, but the problems themselves are widely acknowledged.
narrative_ontology:disappearance_verdict(qualitative_development_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualitative_development_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualitative_development_reading, '046e0a40c34cddf4fff29b8c15f632dbdef31b7a',
    'c6d6880c39ec6bdfedde2a1d41cc00211f451559', '2026-06-11',
    'performance_legitimacy_kernel', 'agent/example_platform_commission.json',
    'claude-sonnet-4-20250514', 'temperature=1.0').
narrative_ontology:story_seed(qualitative_development_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualitative_development_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualitative_development_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualitative_development_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.68) because the framework channels state resources toward politically favored innovation sectors while imposing transition costs on traditional manufacturing and local governments without compensating transfers. Suppression is higher (0.71) because the constraint's persistence depends on actively preventing local governments from reverting to property-driven growth and on rationing credit away from legacy sectors. Theater ratio is moderate (0.42): the innovation and sustainability functions are real, but a growing share of policy activity is performative demonstration that the new model is working, even as fiscal strain and employment displacement accumulate. Accessibility collapse is moderate (0.58) because alternative development models remain conceptually available, though institutionally foreclosed. Resistance is substantial (0.54) from displaced workers, fiscally strained local governments, and traditional sectors losing access to credit.
 *
 * PERSPECTIVAL GAP:
 *   From the central planning authority's seat, the constraint is a necessary structural transformation coordinating investment toward long-term competitiveness. From the property-dependent local governments' seat, the same structure operates as fiscal extraction — they lose revenue tools without gaining new ones and are trapped by legacy debt. From the traditional manufacturing sectors' seat, it is enforced obsolescence — their contribution to employment is discounted and credit access is rationed to fund competitors. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   The central planning authority is the agenda-setter with analytical exit options — it defines what counts as development success. State-backed innovation ecosystem and high-tech sectors are structural beneficiaries (d near 0.2) — they collect subsidies, preferential credit, and regulatory protection. Traditional manufacturing, local governments, and labor-intensive exporters are targets (d near 0.8) — they bear credit rationing, environmental compliance costs, and fiscal strain. Venture capital networks are beneficiaries with mobile exit (d near 0.3) — they profit from state-guided deal flow but can exit to other markets. Urban professionals are beneficiaries with constrained exit (d near 0.4) — they gain employment and environmental quality but are locked into the domestic labor market.
 *
 * MANDATROPHY ANALYSIS:
 *   The framework risks mandatrophy if the founding problems (environmental limits, debt, technological dependence) are substantially resolved but the subsidy and credit allocation machinery persists. The measurement series shows rising theater ratio and extraction, consistent with the coordination function being layered over by rent distribution. The omega variables document the unresolved empirical questions: whether innovation subsidies are generating genuine productivity gains or capturing rents, and whether the framework's sustainability claims are structurally separable from its industrial policy function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_subsidy_productivity_gap,
    'Are state subsidies to innovation sectors generating genuine productivity gains and technological spillovers, or are they primarily capturing rents while concentrating resources in politically connected firms?',
    'Longitudinal total factor productivity analysis comparing subsidized innovation sectors to unsubsidized comparators, controlling for selection effects. Patent citation networks to measure knowledge spillovers. Firm-level profitability data to distinguish productivity gains from subsidy dependence.',
    'If subsidies are generating spillovers and productivity gains, the framework''s coordination function is real and extraction is the price of structural transformation. If subsidies are primarily rent capture, the framework is a snare using innovation rhetoric as cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_subsidy_productivity_gap, empirical, 'Whether innovation subsidies generate productivity or capture rents.').

omega_variable(
    sustainability_industrial_policy_separability,
    'Is the framework''s environmental sustainability function structurally separable from its industrial policy function, or is sustainability enforcement selectively applied to disadvantage non-favored sectors?',
    'Cross-sectoral comparison of environmental enforcement intensity controlling for pollution levels. Analysis of whether state-backed innovation firms face equivalent environmental compliance costs as traditional manufacturers. Examination of whether sustainability standards are tightened when they advantage strategic sectors.',
    'If sustainability enforcement is uniform, the environmental coordination function is genuine. If enforcement is selective, sustainability is a tool for industrial reallocation rather than a separable public good, and the framework''s extraction is higher than the base metric suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sustainability_industrial_policy_separability, conceptual, 'Whether sustainability is a genuine coordination function or a selective industrial policy tool.').

omega_variable(
    sibling_reading_coexistence,
    'Can this reading coexist with the livelihood_security_reading within a single policy framework, or does prioritizing innovation necessarily subordinate employment stability?',
    'Analysis of policy trade-offs in jurisdictions attempting to pursue both innovation-led growth and employment protection. Examination of whether fiscal capacity exists to fund both transition support for displaced workers and innovation subsidies simultaneously.',
    'If the readings can coexist, the framework''s victim set is smaller than modeled (transition costs are compensated). If they are structurally incompatible, the qualitative_development_reading necessarily extracts from labor-intensive sectors and the victim set is accurately specified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, conceptual, 'Whether innovation prioritization and employment security are compatible within one framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualitative_development_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qual_tr_t0, qualitative_development_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(qual_tr_t5, qualitative_development_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(qual_tr_t10, qualitative_development_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(qual_tr_t15, qualitative_development_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(qual_tr_t20, qualitative_development_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(qual_tr_t25, qualitative_development_reading, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(qual_be_t0, qualitative_development_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(qual_be_t5, qualitative_development_reading, base_extractiveness, 5, 0.57).
narrative_ontology:measurement(qual_be_t10, qualitative_development_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(qual_be_t15, qualitative_development_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(qual_be_t20, qualitative_development_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement(qual_be_t25, qualitative_development_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(qual_su_t0, qualitative_development_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(qual_su_t5, qualitative_development_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(qual_su_t10, qualitative_development_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(qual_su_t15, qualitative_development_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(qual_su_t20, qualitative_development_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(qual_su_t25, qualitative_development_reading, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualitative_development_reading, resource_allocation).
narrative_ontology:affects_constraint(qualitative_development_reading, quantitative_growth_reading).
narrative_ontology:affects_constraint(qualitative_development_reading, techno_nationalist_reading).
narrative_ontology:affects_constraint(qualitative_development_reading, livelihood_security_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the performance_legitimacy kernel. Each reading instantiates a different claim about what developmental outcomes ground state authority. The readings share a kernel (performance legitimacy) but differ in their beneficiary structures, victim sets, and extractiveness profiles. They are linked via network.affects_constraints because shifts in which reading dominates policy discourse reallocate state resources and redefine which sectors are valorized or subordinated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
