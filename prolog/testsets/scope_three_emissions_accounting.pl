% ============================================================================
% CONSTRAINT STORY: scope_three_emissions_accounting
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_scope_three_emissions_accounting, []).

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
 *   constraint_id: scope_three_emissions_accounting
 *   human_readable: Scope Three Emissions Accounting in Corporate Climate Disclosure
 *   domain: environmental_policy/corporate_accountability
 *
 * SUMMARY:
 *   Scope Three emissions accounting in corporate climate disclosure creates
 *   a structural tension between the desire for comprehensive supply-chain
 *   transparency and the practical impossibility of drawing clear causal
 *   boundaries across global production networks. The constraint operates at
 *   the intersection of environmental accountability, capital market access,
 *   and corporate strategic incentives. Large multinationals report Scope
 *   Three emissions using frameworks (GRI, TCFD, ISSB) that permit
 *   'reasonable estimates' and 'materiality assessments,' creating
 *   methodological discretion that permits systematic under-reporting while
 *   maintaining the appearance of comprehensive disclosure. The constraint
 *   exhibits tangled_rope structure: genuine coordination function (enabling
 *   supply-chain visibility and carbon-risk pricing) overlaid with asymmetric
 *   extraction (permitting powerful actors to exclude high-emission
 *   categories while weak actors lack auditability). The theater_ratio (0.68)
 *   reflects that Scope Three protocols involve extensive documentation,
 *   third-party assurance, and verification rituals, yet the gap between
 *   disclosed and actual supply-chain emissions remains systemic (30-60% for
 *   consumer-facing sectors). This constraint is critical for understanding
 *   how environmental accountability infrastructure can be captured: the
 *   machinery of disclosure becomes a mechanism for legitimizing incomplete
 *   climate action.
 *
 * KEY AGENTS:
 *   - Compliant Reporting Corporations: Primary beneficiary (institutional/arbitrage) — capture ESG capital premium, regulatory arbitrage advantages, stakeholder trust despite emission-accounting gaps
 *   - Upstream Supply Chain Firms: Secondary beneficiary (institutional/arbitrage) — benefit from standardized protocols enabling contractual carbon-risk pricing and supply-chain coordination
 *   - Accounting Standards Bodies: Institutional maintainers (institutional/arbitrage) — TCFD, GRI, ISSB generate legitimacy and institutional authority; see own frameworks as degraded but maintain them through inertia
 *   - End Consumers and Waste Processors: Primary victims (moderate/constrained) — bears responsibility for use-phase and end-of-life emissions but lacks agency in reporting or boundary definition
 *   - Climate Accountability Baseline: Structural victim (powerless/trapped) — the atmospheric carbon budget cannot exit the reporting framework; actual emissions exceed disclosed figures through systematized methodological gaps
 *   - Environmental Advocates: Organized victims (organized/constrained) — NGOs and climate researchers dependent on corporate partnerships for data access; face suppression through methodological complexity and greenwashing legitimization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing boundary ambiguity (use-phase vs. manufacture vs. disposal) as immutable problem rather than contingent institutional allocation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(scope_three_emissions_accounting, 0.58).
domain_priors:suppression_score(scope_three_emissions_accounting, 0.62).
domain_priors:theater_ratio(scope_three_emissions_accounting, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(scope_three_emissions_accounting, extractiveness, 0.58).
narrative_ontology:constraint_metric(scope_three_emissions_accounting, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(scope_three_emissions_accounting, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(scope_three_emissions_accounting, tangled_rope).
narrative_ontology:human_readable(scope_three_emissions_accounting, "Scope Three Emissions Accounting in Corporate Climate Disclosure").
narrative_ontology:topic_domain(scope_three_emissions_accounting, "environmental_policy/corporate_accountability").

domain_priors:requires_active_enforcement(scope_three_emissions_accounting).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(scope_three_emissions_accounting, upstream_supply_chain_firms).
narrative_ontology:constraint_beneficiary(scope_three_emissions_accounting, reporting_corporations_compliant).
narrative_ontology:constraint_beneficiary(scope_three_emissions_accounting, accounting_standard_bodies).
narrative_ontology:constraint_victim(scope_three_emissions_accounting, unaccounted_end_consumers).
narrative_ontology:constraint_victim(scope_three_emissions_accounting, downstream_waste_processors).
narrative_ontology:constraint_victim(scope_three_emissions_accounting, climate_accountability_baseline).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE ACCOUNTABILITY BASELINE (SNARE) — The collective atmospheric carbon budget cannot exit the reporting framework. Scope Three accounting extracts value from climate credibility by permitting corporations to exclude high-emission categories (use phase, end-of-life, consumer behavior) while claiming comprehensive disclosure. The baseline bears the full cost: actual emissions exceed reported scope through systematized gaps.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: END CONSUMERS AND WASTE PROCESSORS (TANGLED ROPE) — Constrained by limited information and responsibility deflection norms, but also benefit from supply-chain transparency gains. Scope Three accounting theoretically includes use-phase emissions (consumer behavior) and end-of-life (waste processing), but methodologies permit suppliers to avoid responsibility assignments. These agents experience both coordination benefit (visibility into upstream supply) and asymmetric extraction (their emissions are counted but accountability is displaced).
constraint_indexing:constraint_classification(scope_three_emissions_accounting, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMPLIANT REPORTING CORPORATIONS (ROPE) — Large multinational corporations with resources to implement Scope Three accounting benefit from stakeholder trust, access to ESG capital markets, and regulatory arbitrage (complying with stringent EU standards while avoiding US federal mandates). Scope Three coordination solves a real problem: enabling supply-chain visibility. Net beneficiary — extraction flows toward these institutional actors through capital market advantages.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UPSTREAM SUPPLY CHAIN FIRMS (ROPE) — Material suppliers and component manufacturers benefit from standardized emissions protocols that enable contractual relationships and risk hedging. Scope Three accounting creates coordination infrastructure: if Supplier A's emissions are quantified, Buyer B can price carbon risk into contracts. The constraint solves collective action problems in multi-tier supply chains.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL ACCOUNTABILITY ADVOCATES (SNARE) — Organized NGOs and climate researchers face suppression through: (a) methodological complexity (Scope Three permits 'reasonable estimate' proxies that are impossible to audit), (b) capital flight incentives (corporations choosing non-mandatory frameworks), and (c) greenwashing legitimization (Scope Three disclosure provides appearance of comprehensive accounting while actual emissions exceed reported figures by 30-60% for consumer-facing sectors). Exit: constrained by dependence on corporate partnership and regulatory policy — the constraint's existence generates the advocacy ecosystem.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ACCOUNTING STANDARDS BODIES (PITON) — GRI, TCFD, ISSB, and ISO frameworks maintain Scope Three protocols through institutional inertia. Theater ratio is high (0.68): the methodologies involve extensive documentation and verification rituals, but the gap between disclosed and actual supply-chain emissions remains systemic. The standard-setting bodies see their own frameworks as degraded — they acknowledge that 'use phase emissions depend on consumer behavior beyond corporate control' and 'end-of-life responsibility is ambiguous' — yet continue publishing harmonization updates rather than solving boundary problems. Piton classification derives from the theater gate: the ritual persists because the alternatives (implicit carbon accounting, no supply-chain disclosure, manufacturer monopoly on emissions attribution) are worse, not because the current framework works.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / BOUNDARY PROBLEM (MOUNTAIN) — From a civilizational/universal perspective, causal responsibility for emissions across global supply chains is fundamentally ambiguous: does a smartphone manufacturer own the electricity emissions from use-phase charging (consumer behavior variable), the mining emissions from rare earth extraction (supplier nation's energy grid), the transport emissions from international logistics (fuel efficiency beyond manufacturer control), or the e-waste processing emissions (dumping country's informal recycling sector)? This perspective sees Scope Three accounting as addressing an intrinsically hard problem — ambiguous boundaries — rather than a contingent policy choice. However, the structural data (beneficiaries capturing value, victims bearing costs, suppression mechanisms) contradicts the mountain classification. This represents a false summit: naturalizing a contingent institutional allocation of responsibility as a law of nature.
constraint_indexing:constraint_classification(scope_three_emissions_accounting, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scope_three_emissions_accounting_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(scope_three_emissions_accounting, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scope_three_emissions_accounting, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(scope_three_emissions_accounting, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(scope_three_emissions_accounting, TR),
    TR >= 0.70.

:- end_tests(scope_three_emissions_accounting_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint permits large corporations to exclude 2-4 high-emission scopes (use phase, end-of-life, consumer behavior) while claiming comprehensive disclosure, extracting capital market trust and regulatory credibility. The extraction is substantial but not maximum because: (a) Scope Three accounting genuinely enables supply-chain coordination and carbon-risk pricing, (b) some frameworks are tightening methodological boundaries, and (c) some corporations are voluntarily including use-phase. Suppression (0.62): High. Barriers to accurate attribution include: methodological complexity (reasonable estimates are impossible to audit), capital flight incentives (corporations can choose non-mandatory frameworks), jurisdictional fragmentation (different regions accept different methodologies), and data asymmetry (corporations control supply-chain information). Theater ratio (0.68): High. Scope Three accounting involves extensive documentation, third-party assurance, stakeholder engagement processes, and standards-body harmonization meetings — yet the core boundary problems (use-phase attribution, end-of-life responsibility, consumer behavior) remain unresolved. The theater has increased over the 10-year interval as corporations have professionalized their disclosure without solving underlying attribution ambiguities. The extractiveness has increased (0.35 → 0.58) as methodological flexibility has been weaponized by sophisticated actors; the theater has also increased (0.42 → 0.68) as the legitimacy apparatus has matured.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between the institutional beneficiaries' experience (coordination, transparency benefits) and the climate baseline's experience (systematic exclusion, greenwashing legitimization). Large corporations report that Scope Three accounting improves supply-chain visibility and enables carbon-risk pricing — genuine coordination benefits. The climate accounting baseline, by contrast, experiences Scope Three as a mechanism for permitting legitimate non-accounting: corporations can exclude use-phase emissions (the largest category for consumer products), end-of-life processing, and consumer behavior, while the disclosure apparatus provides the appearance of comprehensiveness. This gap is the classification divergence: rope from beneficiary perspective, snare from baseline perspective, tangled_rope from moderate victim perspective. The perspectival gap is structural, not observational — it derives from real differences in exit options and benefit flows.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position: Compliant corporations (institutional/arbitrage) derive low d (~0.10): beneficiaries with exit options experience negative effective extraction. Upstream suppliers (institutional/arbitrage) derive low d (~0.15): beneficiaries with contractual coordination benefits. End consumers/waste processors (moderate/constrained) derive moderate d (~0.55): victims with limited but real agency; benefit from supply-chain transparency but lose control over responsibility attribution. Climate accountability baseline (powerless/trapped) derives maximum d (~0.95): cannot exit the framework, bears full cost of methodological gaps. Environmental advocates (organized/constrained) derive moderate-high d (~0.60): organized agents with some agency but constrained by dependence on corporate partnerships. The analytical observer (analytical/analytical) derives canonical d (~0.73) but faces the false-summit risk: the perspective sees boundary ambiguity as immutable, which would warrant mountain classification, but the structural data (beneficiaries, victims, extraction mechanisms) contradicts this naturalization.
 *
 * MANDATROPHY ANALYSIS:
 *   Scope Three accounting exhibits the full mandatrophy structure: the constraint could be classified as rope (pure coordination for supply-chain visibility), tangled_rope (coordination + extraction asymmetry), or snare (pure extraction of climate credibility through methodological discretion) depending on which boundary definitions are adopted and which agent perspective is prioritized. The mandatrophy is not resolved but rather instantiated: the accounting standards bodies acknowledge boundary ambiguities yet maintain existing methodologies, corporations use methodological discretion for strategic advantage, and the climate baseline experiences systematic under-reporting. The constraint is genuinely mixed — it solves real coordination problems (supply-chain carbon pricing, transparency infrastructure) while enabling real extraction (permitting comprehensive-seeming disclosure with 30-60% emissions exclusion gaps). The analytical observer risks false summit by naturalizing boundary ambiguity ('supply chains are intrinsically complex, responsibility is inherently ambiguous') when the structural data reveals that boundary choices are strategic: corporations choose boundaries that exclude high-emission scopes, advocacy groups push for tighter boundaries, and the standards bodies maintain flexibility to permit both. The mandatrophy persists because no single frame captures the full structure — the constraint IS both coordination and extraction, and the classification depends on which dimension is weighted as primary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_attribution_responsibility,
    'Should use-phase emissions (consumer behavior) be attributed to the manufacturer, consumer, or neither?',
    'Life-cycle thinking ethics: comparative analysis of causal control (who can modify the outcome?) vs. causal contribution (who enabled the outcome?) vs. moral responsibility (who benefits from the outcome?) — establish dominance criteria or reveal incommensurable frames',
    'If manufacturer responsible: Scope Three must include use-phase, vastly increasing reported emissions. If consumer responsible: Scope Three excludes use-phase, rendering corporate climate claims incomplete. If neither: supply-chain emissions become economically unattributable, destroying contracting infrastructure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(boundary_attribution_responsibility, conceptual, 'Attribution of use-phase emissions responsibility across supply chain').

omega_variable(
    end_of_life_decomposition_ambiguity,
    'Does responsibility for end-of-life emissions belong to the original manufacturer, the waste processor, the nation where processing occurs, or the consumer who chose disposal method?',
    'Empirical tracing of informal e-waste supply chains; jurisdictional analysis of extended producer responsibility (EPR) laws across regions; economic incentive analysis of who benefits from decomposition vs. who bears the environmental cost',
    'If manufacturer responsible: Scope Three must include downstream decomposition (complex, uncontrollable). If processor responsible: accountability diffuses into informal sectors with poor data. If shared: multi-party attribution impossible without contractual specification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_of_life_decomposition_ambiguity, empirical, 'Attribution of end-of-life processing emissions in supply chain').

omega_variable(
    disclosure_completeness_extraction,
    'Do Scope Three methodologies (which permit ''reasonable estimates'' and ''materiality assessments'') systematically under-report supply-chain emissions relative to measured actual emissions?',
    'Comparative analysis: reported Scope Three vs. bottom-up measured emissions across 50+ multinational corporations; correlation between permissiveness of methodology and gap magnitude; temporal trend of gap widening/narrowing',
    'If systematic under-reporting: Scope Three is extraction mechanism (high chi from climate perspective). If randomly distributed: Scope Three is genuine coordination with measurement noise. If positive correlation with firm size/power: tangled_rope classification confirmed (benefits large firms, masks actual emissions).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disclosure_completeness_extraction, empirical, 'Whether Scope Three methodologies systematically under-report supply-chain emissions').

omega_variable(
    greenwashing_legitimization,
    'Does Scope Three disclosure, by providing numerical completeness aesthetics, increase investor and consumer trust in corporate climate action despite the actual emissions-reduction gap?',
    'Behavioral study: investor capital allocation patterns pre/post Scope Three disclosure for firms with high reported vs. actual gaps; consumer brand perception studies; correlation between disclosure completeness and ESG capital premium',
    'If disclosure increases trust despite gaps: Scope Three extraction mechanism is legitimization (permitting corporate credibility capture). If disclosure increases skepticism: Scope Three coordination function is transparency (enables critical evaluation). If effect varies by investor sophistication: reveals power differential (institutional investors see through, retail investors trust disclosure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(greenwashing_legitimization, empirical, 'Whether Scope Three disclosure legitimizes incomplete climate action').

omega_variable(
    consumer_behavior_controllability,
    'Can manufacturers meaningfully influence use-phase emissions (through product design, durability, efficiency standards) or is use-phase emissions reduction purely a consumer behavior problem outside manufacturer control?',
    'Engineering analysis: margin of improvement from manufacturer design variables vs. consumer behavior variables (e.g., smartphone charging frequency, driving patterns, heating setpoint). Elasticity estimates: how much reduction in use-phase emissions follows from product efficiency improvement?',
    'If high manufacturer controllability: excluding use-phase from Scope Three is extraction mechanism (avoidable responsibility). If low controllability: including use-phase is unfair attribution (punishment for consumer choices). If variable by product category: requires granular boundary rules, increasing theater and reducing auditability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_behavior_controllability, empirical, 'Manufacturer controllability of use-phase emissions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(scope_three_emissions_accounting, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(scope3_tr_t0, scope_three_emissions_accounting, theater_ratio, 0, 0.42).
narrative_ontology:measurement(scope3_tr_t5, scope_three_emissions_accounting, theater_ratio, 5, 0.58).
narrative_ontology:measurement(scope3_tr_t10, scope_three_emissions_accounting, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(scope3_be_t0, scope_three_emissions_accounting, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(scope3_be_t5, scope_three_emissions_accounting, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(scope3_be_t10, scope_three_emissions_accounting, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(scope_three_emissions_accounting, resource_allocation).
narrative_ontology:affects_constraint(scope_three_emissions_accounting, carbon_credit_arbitrage).
narrative_ontology:affects_constraint(scope_three_emissions_accounting, greenwashing_legitimization).
narrative_ontology:affects_constraint(scope_three_emissions_accounting, supply_chain_transparency).

% DUAL FORMULATION NOTE:
% Scope Three emissions accounting should be decomposed into three structurally distinct constraints with different ε values: (1) use_phase_emissions_attribution (ε=0.65, snare) — manufacturer responsibility for consumer behavior; (2) end_of_life_responsibility_assignment (ε=0.55, tangled_rope) — ownership of waste processing emissions; (3) supply_chain_coordination_infrastructure (ε=0.25, rope) — standardization for carbon-risk pricing. The unified 'Scope Three' label conflates these three constraints into one story, which is the source of boundary ambiguity. Decomposition reveals that the coordination benefits are real but localized (supply-chain pricing), while the extraction mechanisms are real and distributed (use-phase and end-of-life exclusions). The family should be linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(scope_three_emissions_accounting, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
