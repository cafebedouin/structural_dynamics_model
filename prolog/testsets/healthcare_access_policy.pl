% ============================================================================
% CONSTRAINT STORY: healthcare_access_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_healthcare_access_policy, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: healthcare_access_policy
 *   human_readable: Healthcare Access Policy as Mixed Coordination-Extraction
 *   domain: healthcare/political_economy
 *
 * SUMMARY:
 *   Healthcare access policy in the United States exhibits structural
 *   hybridity: genuine coordination functions (insurance risk pooling,
 *   disease surveillance, emergency preparedness) coexist with asymmetric
 *   extraction (medical debt accumulation, network restrictions,
 *   profit-driven care denial). The constraint operates through a multi-payer
 *   system with employer linkage, regulatory complexity, and information
 *   asymmetry that benefits institutional actors (insurers, pharmaceutical
 *   manufacturers, hospital networks) while extracting from victims
 *   (uninsured and underinsured populations, public health infrastructure).
 *   The policy demonstrates the Tangled Rope archetype across the modal
 *   observation site: it genuinely coordinates complex resource allocation
 *   while systematically extracting wealth and health capacity from powerless
 *   agents. Theater ratio (0.55) reflects moderate performativity: insurance
 *   choice architecture (annual enrollment periods, plan comparisons) creates
 *   appearance of consumer agency within constrained options; prior
 *   authorization and utilization review generate administrative ritual with
 *   limited clinical justification. Extractiveness has increased over the
 *   20-year interval (0.35 → 0.58) as administrative burden and out-of-pocket
 *   costs have grown while public capacity has declined. The constraint is
 *   downstream of political-economic choices (tax incentives for employer
 *   coverage, pharmaceutical patent regimes, regulatory capture of insurance
 *   commissions) rather than natural law, making it subject to reform, but
 *   suppression mechanisms (institutional inertia, incumbent lobbying,
 *   cultural normalization) create durable barriers to change.
 *
 * KEY AGENTS:
 *   - Uninsured Low-Income Populations: Primary victim (powerless/trapped) — face medical bankruptcy, debt collection, care deferral; no arbitrage options; maximum suppression through cost barriers and information asymmetry
 *   - Underinsured Middle-Income Populations: Secondary victim (moderate/constrained) — experience mixed coordination (preventive access) and extraction (high deductibles, narrow networks); constrained by family coverage dependencies
 *   - Insurance Companies: Primary beneficiary (institutional/arbitrage) — profit from risk pooling margins, network control, and service restrictions; high arbitrage options for pricing and coverage design
 *   - Pharmaceutical Manufacturers: Secondary beneficiary (institutional/arbitrage) — benefit from patent protection, pricing power, and insurance coverage leverage; global arbitrage options
 *   - Hospital Networks: Tertiary beneficiary (institutional/constrained) — coordinate service delivery but also extract rents through market consolidation and network narrowing
 *   - Public Health Infrastructure: Tertiary victim (institutional/constrained) — loses resources to insurance subsidization and medicalization of public health functions; constrained by mandate creep
 *   - Healthcare Reform Coalition: Organized agent (organized/constrained) — patient advocates, labor unions, progressive policymakers seeing sunset path toward universal coverage
 *   - Employer-Based Insurance System: Institutional carrier (institutional/arbitrage) — maintains path-dependent coordination architecture that has lost original function but persists through regulatory support
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(healthcare_access_policy, 0.58).
domain_priors:suppression_score(healthcare_access_policy, 0.65).
domain_priors:theater_ratio(healthcare_access_policy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(healthcare_access_policy, extractiveness, 0.58).
narrative_ontology:constraint_metric(healthcare_access_policy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(healthcare_access_policy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(healthcare_access_policy, tangled_rope).
narrative_ontology:human_readable(healthcare_access_policy, "Healthcare Access Policy as Mixed Coordination-Extraction").
narrative_ontology:topic_domain(healthcare_access_policy, "healthcare/political_economy").

domain_priors:requires_active_enforcement(healthcare_access_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(healthcare_access_policy, insurance_companies).
narrative_ontology:constraint_beneficiary(healthcare_access_policy, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(healthcare_access_policy, hospital_networks).
narrative_ontology:constraint_victim(healthcare_access_policy, uninsured_low_income_populations).
narrative_ontology:constraint_victim(healthcare_access_policy, underinsured_middle_income_populations).
narrative_ontology:constraint_victim(healthcare_access_policy, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED LOW-INCOME PATIENT (SNARE) — Faces maximal extraction with minimal exit options. Medical bankruptcy, debt traps, and deferral of care are structural features. No arbitrage available; constrained by geography, employment status, and catastrophic cost barriers. High suppression through information asymmetry about billing, prior authorization requirements, and debt collection mechanisms.
constraint_indexing:constraint_classification(healthcare_access_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERINSURED MIDDLE-INCOME WORKER (TANGLED ROPE) — Experiences genuine coordination benefit (preventive care access through employer plans) but also asymmetric extraction through high deductibles, narrow networks, and rising premiums. Has some exit options (employer switching, marketplace plans) but constrained by family coverage dependencies and pre-existing condition locks. Mixed experience of both benefit and burden.
constraint_indexing:constraint_classification(healthcare_access_policy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURANCE COMPANY LEADERSHIP (ROPE) — Experiences healthcare policy as coordination mechanism: risk pooling, pricing mechanisms, network agreements, and regulatory compliance frameworks all coordinate complex resource allocation. Net beneficiary with high arbitrage options (pricing adjustments, service exclusions, market positioning). Low experienced extraction because exit costs are minimal and benefits flow consistently to institutional actors.
constraint_indexing:constraint_classification(healthcare_access_policy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC HEALTH INFRASTRUCTURE (TANGLED ROPE) — Experiences policy as mixed coordination (communicable disease surveillance, emergency preparedness) and extraction (privatization pressure, resource diversion to subsidizing insurance pools, medicalization of public health functions). Constrained by mandate creep and funding volatility. Must coordinate across jurisdictions but bears disproportionate burden for uninsured populations.
constraint_indexing:constraint_classification(healthcare_access_policy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTHCARE REFORM COALITION (SCAFFOLD) — Organized agents (patient advocates, progressive policymakers, labor unions) view healthcare access policy as a temporary arrangement with sunset trajectory toward universal coverage models. See extraction mechanism as contingent on market-based policy choice, not structural necessity. High suppression of alternative models through incumbent lobbying, but organized agents perceive agency and exit path through democratic reform. Theater ratio lower from this perspective because alternative models (single-payer, public option, cooperative models) are documented and achievable.
constraint_indexing:constraint_classification(healthcare_access_policy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EMPLOYER-BASED INSURANCE SYSTEM (PITON) — From civilizational perspective, the employer-health insurance link is a historical accident (wage controls during WWII) that has become inertial institutional fact. The system persists through path dependency despite widespread recognition of its dysfunction. Theater is performative: annual coverage discussions, HR benefits consultants, plan comparisons create the appearance of choice within a constrained architecture. The system has lost its original coordination function (during wartime labor scarcity) but remains enforced through tax policy and institutional habit.
constraint_indexing:constraint_classification(healthcare_access_policy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit: some perspectives frame healthcare access inequality as natural or inevitable ('markets efficiently allocate resources,' 'administrative complexity requires gatekeeping'). However, comparative international evidence undermines mountain classification — many democratic nations maintain universal coverage through public systems with lower administrative overhead and better population health outcomes. The natural law framing naturalizes what is actually contingent policy choice.
constraint_indexing:constraint_classification(healthcare_access_policy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(healthcare_access_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(healthcare_access_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(healthcare_access_policy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(healthcare_access_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(healthcare_access_policy, TR),
    TR >= 0.70.

:- end_tests(healthcare_access_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Insurance companies, pharmaceutical manufacturers, and consolidating hospital networks extract rents through pricing power, coverage restrictions, and administrative complexity. However, extractiveness is not maximal (≥0.66 for snare) because genuine coordination occurs: disease risk pooling is functional, emergency care capacity is maintained, and preventive services are provided. The mixed nature justifies Tangled Rope classification. Suppression (0.65): Moderate-high. Structural barriers include medical debt leverage, information asymmetry about billing and coverage, geographic access constraints, and employment linkage. Suppression is not maximum (0.60 for snare) because some alternatives exist: public options in some states, medicaid expansions, charity care programs. But suppression is substantial and has institutional reinforcement through regulatory capture of insurance commissions. Theater ratio (0.55): Moderate. Prior authorization and utilization review generate administrative ritual with modest clinical justification. Insurance plan choice creates appearance of consumer agency within constrained architecture. However, theater is not dominant (≥0.70 for piton) because underlying coordination functions are real: risk pooling, network arrangements, and claims processing serve coordination purposes, not purely performative ones. The measurement trajectory shows both extractiveness and theater increasing over 20 years: administrative burden has grown from complexity layering (prior auth, step therapy, network verification), and theater has increased from 0.40 to 0.55, indicating Goodhart drift where process metrics replace health outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between institutional beneficiaries and powerless victims. Insurance company leaders see healthcare policy as coordination mechanism (risk pooling, resource allocation) and experience it as Rope. Uninsured patients see the same policy as extraction mechanism (cost barriers, care denial) and experience it as Snare. The gap reveals asymmetric experience of the same structural arrangement: one agent's coordination is another agent's suppression. Underinsured middle-income workers occupy the middle: they benefit from preventive care coordination but bear extraction through high deductibles and premium inflation. The reform coalition introduces temporal gap: while incumbent institutions experience the policy as stable coordination (or inertial piton), organized agents perceive a sunset clause — reform is possible if political will mobilizes. The analytical observer risks naturalizing contingent policy as immutable law — framing healthcare inequality as inherent to markets rather than policy choice. International comparison data undermines this naturalization: peer democracies maintain universal coverage with lower costs and better outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the structural beneficiary/victim declarations and exit options for each agent. Uninsured populations (d → 0.95): identified as primary victims with trapped exit options; the sigmoid f(d) amplifies their experience of the ε=0.58 base extraction. Insurance companies (d → 0.05): identified as beneficiaries with arbitrage exit options; f(d) becomes negative, reducing experienced extraction below base. Underinsured workers (d → 0.50): mixture of beneficiary status (coordination access) and victim status (extraction through deductibles); d stays near 0.50 producing moderate f(d). Public health (d → 0.65): victim status with institutional but constrained options; f(d) stays in rope range. Reform coalition (d → 0.40): organized agent with perceived exit path; lower d reduces experienced extraction relative to base. The directionality chain maps structural position to experience: same ε produces different χ depending on whether the agent benefits or bears costs from the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Healthcare access policy resolves mandatrophy through recognition that institutional beneficiaries (insurers, pharma) legitimately perceive coordination (Rope/Scaffold), while powerless victims perceive extraction (Snare), and the analytical observer must recognize both perceptions as structurally accurate from their positions. No single type is 'correct' — the constraint is Tangled Rope at the modal site because it genuinely coordinates while systematically extracting. The mandatrophy is resolved by refusing false universality: it is not 'really' a Snare (pure extraction would ignore real coordination functions) nor 'really' a Rope (pure coordination would ignore real asymmetric extraction), but both simultaneously from different structural positions. The false summit danger appears when natural law frames (Mountain perspective: 'healthcare market outcomes are natural') naturalize what is actually policy choice. Comparative international analysis (universal coverage systems with lower costs and better outcomes) directly contradicts the mountain classification and reveals it as false natural law. The constraint's contingency — it results from tax policy choices, regulatory capture, and political decisions about intellectual property — makes reform possible and mandatrophy resolvable through institutional restructuring rather than accepting the constraint as immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_versus_coordination_boundary,
    'What portion of healthcare administrative complexity and cost is inherent to coordination versus artificially imposed to enable extraction?',
    'International comparative analysis: administrative costs per capita and complexity metrics in universal coverage systems versus multi-payer systems; randomized policy experiments comparing prior authorization requirements versus provider trust models',
    'If minimal inherent complexity (< 5% of cost): administrative burden is primarily extractive theater. If substantial inherent complexity (> 15% of cost): some theater is genuine coordination cost. Classification shifts from Snare toward Tangled Rope for victim perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_versus_coordination_boundary, empirical, 'Inherent versus extractive portions of administrative burden').

omega_variable(
    suppression_mechanism_internalization,
    'Is health outcome inequality primarily structurally suppressed (material barriers: cost, geography, information) or internalized (patients believe unequal access is inevitable or deserved)?',
    'Post-policy change analysis: if suppression is structural, removal of barriers increases access; if internalized, access may remain low even after barrier removal due to persistent health-seeking behavior changes; qualitative research on patient framing of healthcare justice',
    'If primarily structural: suppression score accurate at 0.65. If partially internalized: effective suppression is higher — internalized acceptance persists after structural barrier removal, requiring cultural intervention alongside policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanisms').

omega_variable(
    coalition_stability_timeline,
    'What is the realistic timeline and institutional stability for healthcare reform coalition''s sunset clause (transition to universal coverage or public option)?',
    'Political feasibility analysis: legislative pathways, incumbent resistance modeling, public opinion trends, international transition timelines from multi-payer to single-payer systems',
    'If sunset realistic within 20 years: Scaffold classification accurate. If sunset blocked indefinitely: Scaffold collapses to Tangled Rope or Snare — constraint becomes permanent rather than transitional.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_stability_timeline, preference, 'Feasibility of healthcare reform transition timeline').

omega_variable(
    international_path_dependency,
    'Does the U.S. employer-based system represent path-locked institutional dependency or politically contingent choice that could be reformed without wholesale system replacement?',
    'Comparative institutional analysis: countries that transitioned from employment-based to universal coverage without revolution (Germany, Japan); structural analysis of veto points in U.S. political economy that preserve employer linkage',
    'If path-locked: Piton is structural degradation of an inescapable form (mountain-like properties). If contingent: Piton is purely theatrical maintenance of a choice that could be unmade — enables more aggressive reform modeling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_path_dependency, empirical, 'Whether employer-insurance link is path-dependent or contingent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(healthcare_access_policy, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcap_tr_t0, healthcare_access_policy, theater_ratio, 0, 0.4).
narrative_ontology:measurement(hcap_tr_t10, healthcare_access_policy, theater_ratio, 10, 0.48).
narrative_ontology:measurement(hcap_tr_t20, healthcare_access_policy, theater_ratio, 20, 0.55).
narrative_ontology:measurement(hcap_tr_t5, healthcare_access_policy, theater_ratio, 5, 0.44).
narrative_ontology:measurement(hcap_tr_t15, healthcare_access_policy, theater_ratio, 15, 0.51).

% Extraction over time
narrative_ontology:measurement(hcap_be_t0, healthcare_access_policy, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hcap_be_t10, healthcare_access_policy, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hcap_be_t20, healthcare_access_policy, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(hcap_be_t5, healthcare_access_policy, base_extractiveness, 5, 0.41).
narrative_ontology:measurement(hcap_be_t15, healthcare_access_policy, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(healthcare_access_policy, resource_allocation).
narrative_ontology:affects_constraint(healthcare_access_policy, pharmaceutical_patent_regime).
narrative_ontology:affects_constraint(healthcare_access_policy, employer_tax_incentives).
narrative_ontology:affects_constraint(healthcare_access_policy, public_health_capacity_decline).

% DUAL FORMULATION NOTE:
% Healthcare access policy is upstream of specific extraction mechanisms (medical debt, prior authorization gatekeeping) and downstream of structural policy choices (tax incentives for employer coverage, patent regimes enabling price extraction, regulatory capture of insurance commissions). The Tangled Rope classification at ε=0.58 reflects that access policy contains genuine coordination functions alongside asymmetric extraction; decomposition into separate coordination-only and extraction-only stories is not appropriate because the mechanisms are inextricably linked.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(healthcare_access_policy, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
