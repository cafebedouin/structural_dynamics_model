% ============================================================================
% CONSTRAINT STORY: dma_interoperability_mandates
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dma_interoperability_mandates, []).

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
 *   constraint_id: dma_interoperability_mandates
 *   human_readable: DMA Interoperability Mandates and Platform Gating
 *   domain: digital_markets/regulation/platform_economics
 *
 * SUMMARY:
 *   The Digital Markets Act (DMA) interoperability mandates represent a
 *   foundational attempt by EU regulators to break platform lock-in through
 *   forced technical interoperability. The constraint exhibits characteristic
 *   Tangled Rope structure: a genuine coordination problem (users locked into
 *   single platforms, alternatives unable to compete) exists alongside
 *   asymmetric extraction (gatekeepers retain control through technical
 *   standards, API design, and integration timing). The extractiveness has
 *   increased from 0.35 to 0.58 over the initial implementation period as
 *   platforms discovered that interoperability compliance can be engineered
 *   to preserve gating functions while appearing to comply with regulatory
 *   intent. Theater ratio has risen from 0.28 to 0.48 as compliance
 *   documentation and regulatory interpretation have become increasingly
 *   performative — platforms demonstrate technical interoperability while
 *   retaining functional control. The constraint operates across six distinct
 *   structural perspectives simultaneously: powerless developers experience
 *   it as a new extraction mechanism (Snare), competing platforms as mixed
 *   coordination and constraint (Tangled Rope), regulators as pure
 *   coordination (Rope), gatekeepers as degraded but manageable theater
 *   (Piton), open-standards advocates as a temporary support with a sunset
 *   (Scaffold), and theoretical observers risk naturalizing platform lock-in
 *   as inherent to digital markets (Mountain).
 *
 * KEY AGENTS:
 *   - EU Regulatory Authority: Primary beneficiary (institutional/arbitrage) — establishes jurisdiction over global platforms, expands regulatory authority, legitimacy for future market intervention
 *   - Gatekeeper Platforms: Constrained victims (institutional/constrained) — face compliance costs and reduced gating capacity, but retain technical control through standards specification
 *   - Competing Platforms: Secondary victim and beneficiary (moderate/constrained) — gain access to gatekeeper user bases but absorb high integration costs; face reactive adaptation to platform changes
 *   - Small Application Developers: Powerless victims (powerless/trapped) — must implement costly interoperability to remain viable; trapped between market access (requiring DMA compliance) and development burden
 *   - Open Standards Coalition: Organized agents (organized/constrained) — building alternative decentralized platforms; see mandate as temporary scaffold enabling transition to open protocols
 *   - Users: Diffuse beneficiaries (powerless/mobile) — nominally benefit from reduced lock-in but may experience reduced feature quality or degraded user experience from interoperability overhead
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dma_interoperability_mandates, 0.58).
domain_priors:suppression_score(dma_interoperability_mandates, 0.52).
domain_priors:theater_ratio(dma_interoperability_mandates, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dma_interoperability_mandates, extractiveness, 0.58).
narrative_ontology:constraint_metric(dma_interoperability_mandates, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(dma_interoperability_mandates, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dma_interoperability_mandates, tangled_rope).
narrative_ontology:human_readable(dma_interoperability_mandates, "DMA Interoperability Mandates and Platform Gating").
narrative_ontology:topic_domain(dma_interoperability_mandates, "digital_markets/regulation/platform_economics").

domain_priors:requires_active_enforcement(dma_interoperability_mandates).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dma_interoperability_mandates, competing_platforms).
narrative_ontology:constraint_beneficiary(dma_interoperability_mandates, eu_regulatory_authority).
narrative_ontology:constraint_beneficiary(dma_interoperability_mandates, small_application_developers).
narrative_ontology:constraint_victim(dma_interoperability_mandates, gatekeeper_platforms).
narrative_ontology:constraint_victim(dma_interoperability_mandates, integration_complexity_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED DEVELOPER (SNARE) — Small developers in the EU face compulsory interoperability requirements that appear liberating but create new extraction mechanisms. They must implement costly technical integration with gatekeeper platforms to remain viable, effectively transferring development burden to them while gatekeepers capture network effects. No exit: participate in the DMA ecosystem or exit the market entirely.
constraint_indexing:constraint_classification(dma_interoperability_mandates, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: COMPETING PLATFORM (TANGLED ROPE) — Medium-sized platforms benefit from mandatory interoperability (access to gatekeeper user bases) but face high compliance costs and coordinated enforcement overhead. Real coordination function: interoperability enables multi-platform markets. Real extraction: gatekeepers retain control over technical standards, data formats, and integration timing, forcing competitors into constant reactive adaptation.
constraint_indexing:constraint_classification(dma_interoperability_mandates, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: EU REGULATORY AUTHORITY (ROPE) — Sees the mandate as pure coordination: solving the market failure of platform lock-in through rule specification. Extraction runs toward regulatory authority (legitimacy, jurisdiction expansion), not away. No victim declared from this perspective — regulatory function appears costless because enforcement costs are externalized to platforms.
constraint_indexing:constraint_classification(dma_interoperability_mandates, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: GATEKEEPER PLATFORM (PITON) — Acknowledges that interoperability mandates constrain their business model but observes that the mandate's enforcement has become primarily theatrical: compliance demonstrations, legal interpretations, and regulatory navigation consume resources without shifting underlying network effects. The platform can exit the EU market (mobile exit) but chooses to comply because the cost of non-compliance (regulatory retaliation) exceeds compliance cost. Theater ratio driven by performative compliance documentation rather than functional market change.
constraint_indexing:constraint_classification(dma_interoperability_mandates, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN STANDARDS COALITION (SCAFFOLD) — Organized developer communities, interop advocates, and smaller platforms see the DMA mandate as a temporary support structure with a sunset: open standards maturation and technical decoupling will eventually obviate forced interoperability requirements. Sunset logic: as industry adopts open protocols (ActivityPub-style federation, open identity standards), the regulatory mandate becomes redundant. Constrained exit (building alternatives takes time and coordination), but sunset is genuine — technologies for true decoupling exist.
constraint_indexing:constraint_classification(dma_interoperability_mandates, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some degree of platform network lock-in is inherent to digital markets: users prefer consolidated ecosystems with rich feature integration, and decentralization creates coordination costs that users actively avoid. The mandate fights a natural law of platform economics rather than solving a contingent regulatory failure. However, this naturalization risks obscuring that lock-in magnitude is policy-dependent: different regulatory regimes and technical standards produce measurably different levels of switching cost.
constraint_indexing:constraint_classification(dma_interoperability_mandates, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dma_interoperability_mandates_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dma_interoperability_mandates, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dma_interoperability_mandates, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dma_interoperability_mandates, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dma_interoperability_mandates, TR),
    TR >= 0.70.

:- end_tests(dma_interoperability_mandates_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mandate demonstrates genuine market coordination (breaking lock-in, enabling multi-platform competition) alongside asymmetric extraction (gatekeepers retain control of integration timing, data formats, API quality). The extractiveness reflects that gatekeepers can comply with letter while subverting spirit — they control the technical standards through which interoperability occurs. Initial extractiveness (0.35) was lower because the mandate's effects were uncertain; realized extractiveness increased as platforms demonstrated that technical interoperability compliance preserves market power. Suppression (0.52): Moderate. Barriers to exit include EU regulatory jurisdiction (platforms cannot ignore DMA without losing market access), technical switching costs (users cannot easily migrate to interoperable alternatives), and developer dependency (smaller platforms must implement integrations even under unfavorable terms). Suppression is not total — platforms have exit options (exit EU market entirely, lobby for regulatory change, build proprietary alternatives outside jurisdiction). Theater ratio (0.48): Moderate-low. Interoperability requirements have genuine technical content and create real operational changes. However, 48% of compliance effort is performative: regulatory documentation, technical interpretation disputes, and staged compliance demonstrations consume resources without shifting underlying competitive dynamics. Theater has risen from 0.28 to 0.48 as platforms learned to engineer compliant-appearing solutions that preserve substantive gating.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival variance. The same regulation appears as: (A) pure coordination solving lock-in (regulator view), (B) constrained victim status with managed theater (gatekeeper view), (C) mixed coordination and extraction burden (competing platform view), (D) new extraction mechanism replacing old (powerless developer view), (E) temporary support scaffolding (open standards coalition view), (F) naturalized platform economics (analytical observer view). The gap is diagnostic: if all perspectives produced the same type, the classification would be trivial; the fact that the same structural phenomenon produces six different classifications indicates that the constraint's form varies across structural positions — it IS genuinely different constraints depending on where you stand. This is not subjectivity; it is legitimate structural variance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural position relative to extraction. Gatekeepers appear as beneficiaries (arbitrage exit, institutional power) but are constrained victims of the mandate — their d is overridden upward from the canonical 0.00 beneficiary value (low d) to 0.40-0.50 (constrained victim) because they bear suppression costs. EU regulators genuinely benefit from the mandate (regulatory expansion, jurisdiction, legitimacy) with arbitrage exit (can revise mandate if ineffective) — their d remains low (0.10-0.15), producing negative χ (net coordination benefit to regulator). Competing platforms have mixed directionality: they benefit from access (d lowered) but bear compliance costs (d raised). Small developers are trapped victims with no arbitrage — d approaches 1.0, producing high χ (high experienced extraction). The beneficiary and victim declarations capture that the mandate's extraction flow runs from powerless developers and (partly) from gatekeepers toward competing platforms and regulators.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The mandate demonstrates genuine coordination function (breaking lock-in, enabling multi-platform markets) alongside asymmetric extraction (gatekeepers retain control through technical standards). The mandatrophy resolves when analyzing: (1) Beneficiaries are real: competing platforms gain actual market access, small developers gain regulatory protection, regulators gain jurisdiction expansion. (2) Victims are real: gatekeepers absorb compliance costs and regulatory constraint, small developers absorb integration burden, users may experience feature degradation. (3) Enforcement is active: ongoing regulatory interpretation, compliance audits, and technical specification updates. The classification is neither pure coordination (Rope) nor pure extraction (Snare) because both functions operate simultaneously. The Tangled Rope classification prevents the false natural law (Mountain) of 'platform consolidation is inevitable' and prevents the false coordination (Rope) of 'mandates solve lock-in without cost.' The actual structure is: regulatory coordination enables market entry that gatekeepers resist through technical sophistication, producing a constraint that is genuinely both mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interop_technical_feasibility,
    'How technically feasible is genuine interoperability without degrading user experience or platform functionality?',
    'Comparative analysis of interoperability implementations (message federation, data portability, API standardization); measurement of feature parity loss and latency introduction in interoperable vs native workflows',
    'If highly feasible: mandate is pure coordination with low inherent suppression (reduces chi). If fundamentally constrained: mandate forces suboptimal technical choices onto platforms, and suppression derives from technical necessity rather than regulatory choice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interop_technical_feasibility, empirical, 'Technical feasibility of interoperability without functionality loss').

omega_variable(
    gatekeeper_control_persistence,
    'Do interoperability mandates genuinely reduce gatekeeper control over data flow and user switching costs, or do gatekeepers retain effective control through technical standards, API design, and timing?',
    'Longitudinal measurement of switching costs pre- and post-DMA for cross-platform user migration; analysis of gatekeeper control over API specifications, rate limits, data formats; tracking of developer burden for integration vs native development',
    'If mandates reduce switching costs: classification shifts toward Rope. If gatekeepers retain control: classification shifts toward Snare (victims) and Piton (theater-driven compliance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_control_persistence, empirical, 'Whether mandates reduce gatekeeper control or enable control through standards').

omega_variable(
    open_standards_maturation_timeline,
    'How long will it realistically take for open standards and decentralized protocols to mature sufficiently to make regulatory interoperability mandates redundant?',
    'Technical roadmap analysis of federation protocols (ActivityPub, Matrix, DID standards); measurement of adoption rates among platforms; assessment of feature parity between open and proprietary solutions',
    'If < 5 years: scaffold sunset is credible and near-term exit pathway exists. If > 15 years: scaffold classification is aspirational; the mandate may become permanent structural feature.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_standards_maturation_timeline, empirical, 'Timeline for open standards maturation to obsolete mandates').

omega_variable(
    regulatory_cost_incidence,
    'Who ultimately bears the compliance costs: gatekeepers (reducing their rents), smaller platforms (increasing their costs), or users (through reduced feature quality or increased prices)?',
    'Cost accounting: platform spending on compliance infrastructure, developer hiring for integration work; measurement of feature velocity pre- and post-DMA; pricing analysis for premium features and data access',
    'If gatekeepers absorb costs: mandate achieves extraction goals and classification shifts toward rope. If smaller platforms or users bear costs: mandate becomes a subsidy to gatekeepers and classification shifts toward snare for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_cost_incidence, empirical, 'Incidence of DMA compliance costs across stakeholders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dma_interoperability_mandates, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dma_tr_t0, dma_interoperability_mandates, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dma_tr_t2, dma_interoperability_mandates, theater_ratio, 2, 0.38).
narrative_ontology:measurement(dma_tr_t4, dma_interoperability_mandates, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(dma_be_t0, dma_interoperability_mandates, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dma_be_t2, dma_interoperability_mandates, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(dma_be_t4, dma_interoperability_mandates, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dma_interoperability_mandates, enforcement_mechanism).
narrative_ontology:affects_constraint(dma_interoperability_mandates, platform_data_portability_mandates).
narrative_ontology:affects_constraint(dma_interoperability_mandates, app_store_sideloading_requirements).
narrative_ontology:affects_constraint(dma_interoperability_mandates, algorithmic_recommendation_transparency).

% DUAL FORMULATION NOTE:
% DMA interoperability mandates share a regulatory domain with data portability, sideloading, and algorithmic transparency requirements. Each constraint has distinct ε values reflecting different extractiveness mechanisms. Interoperability mandates extract primarily through integration burden and compliance costs (ε=0.58); data portability extracts through information asymmetry navigation (ε=0.42); sideloading extracts through security/liability frameworks (ε=0.35). These constraints form a regulatory family upstream of which gatekeeper platforms coordinate response strategies. Network decomposition links them as affects_constraints rather than merging them, because each has distinct resolution mechanisms and temporal dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dma_interoperability_mandates, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
