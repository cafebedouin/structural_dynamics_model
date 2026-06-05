% ============================================================================
% CONSTRAINT STORY: asymmetric_burden_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_burden_distribution, []).

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
 *   constraint_id: asymmetric_burden_distribution
 *   human_readable: The Externalization Lever
 *   domain: economic/social/logistical
 *
 * SUMMARY:
 *   The externalization lever describes a structural mechanism in modern
 *   logistics and consumer convenience systems where systemic benefits
 *   (low-cost goods delivery, urban density, e-commerce speed) are captured
 *   by central beneficiaries while operational burdens (pollution, noise,
 *   occupational stress, traffic risk) are systematically imposed on
 *   peripheral populations with minimal exit options. This constraint
 *   exhibits multiple DR types depending on observer position: powerless
 *   warehouse workers and port-adjacent residents experience it as a snare
 *   with no exit; municipal governments face tangled extraction-coordination
 *   (economic benefits offset by infrastructure costs and resident welfare
 *   losses); logistics platforms experience pure coordination (building
 *   efficient networks); organized environmental justice coalitions see a
 *   temporary coordination failure with emerging remediation mechanisms
 *   (scaffold); regulatory agencies maintain degraded compliance theater
 *   (piton); thermodynamic arguments risk naturalizing what is contingent
 *   institutional choice (false mountain). The constraint's theater ratio
 *   (0.64) reflects that formal regulations and compliance documentation
 *   exist but are weakly enforced; companies can meet rules on paper while
 *   externalities continue through loopholes and regulatory arbitrage. Base
 *   extractiveness has risen from 0.32 to 0.58 over the 20-year interval as
 *   e-commerce scale has concentrated benefits and as regulatory capture has
 *   deepened, allowing companies to avoid internalization of costs despite
 *   growing environmental awareness.
 *
 * KEY AGENTS:
 *   - Urban Convenience Consumers: Primary beneficiary (institutional/arbitrage) — capture low-cost goods, fast delivery, environmental anonymity. Can exit burden entirely through residential choice.
 *   - Logistics Platform Operators (Amazon, DHL, UPS, etc.): Primary beneficiary (institutional/arbitrage) — capture profit from volume scaling, regulatory arbitrage, operational flexibility. Maximum exit options; can relocate hubs or outsource operations.
 *   - Goods Distribution Networks: Beneficiary (institutional/arbitrage) — enable global supply chains; capture efficiency gains and network effects.
 *   - Warehouse Workers: Primary victim (powerless/trapped) — concentrated in logistics hubs; bear occupational stress, injury risk, scheduling pressure. Limited exit due to wage dependence and skill specificity.
 *   - Port-Adjacent Residential Communities: Primary victim (powerless/trapped) — geographic immobility (housing ownership, rental stigma, economic constraints); continuous exposure to noise, pollution, traffic hazard. Suppression through real estate lock-in.
 *   - Traffic-Affected Neighborhoods: Secondary victim (powerless/constrained) — bear collision risk and air quality degradation; limited exit without major relocation.
 *   - Environmental Receptor Populations: Victim (powerless/trapped) — peripheral regions absorbing manufacturing and extraction operations; abstract collective good with no organized voice.
 *   - Municipal Governments: Secondary actor (moderate/constrained) — benefit from tax base and employment claims; bear infrastructure maintenance costs and political pressure. Constrained exit through jurisdictional boundaries and tax competition.
 *   - Environmental Justice Coalition: Organized actor (organized/constrained) — growing political force pushing cost internalization; constrained by resource limits and regulatory capture.
 *   - Regulatory Agencies: Institutional actor (institutional/constrained) — maintain formal standards but weak enforcement; captured by logistics lobby. Constrained by budget, political pressure, and jurisdictional limits.
 *   - Extended Producer Responsibility Movement: Organized actor (organized/constrained) — building international norms for cost internalization; constrained by coordination barriers and corporate resistance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_burden_distribution, 0.58).
domain_priors:suppression_score(asymmetric_burden_distribution, 0.68).
domain_priors:theater_ratio(asymmetric_burden_distribution, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_burden_distribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_burden_distribution, tangled_rope).
narrative_ontology:human_readable(asymmetric_burden_distribution, "The Externalization Lever").
narrative_ontology:topic_domain(asymmetric_burden_distribution, "economic/social/logistical").

domain_priors:requires_active_enforcement(asymmetric_burden_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, urban_convenience_consumers).
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, logistics_platform_operators).
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, goods_distribution_networks).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, peripheral_warehouse_workers).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, port_communities).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, traffic_affected_neighborhoods).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, environmental_receptor_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAREHOUSE WORKER (SNARE) — Trapped in proximity to sorting facilities, delivery hubs, and logistics nodes. Bears full operational burden: noise pollution, air quality degradation, vehicular hazard, occupational stress, and compressed scheduling. No realistic exit without loss of livelihood. Maximum experienced extraction. The constraint's coercive structure leaves no meaningful alternatives.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PORT-ADJACENT RESIDENTIAL COMMUNITY (SNARE) — Trapped by residential ownership or rental immobility. Bears continuous exposure to container truck noise, diesel exhaust, light pollution from 24-hour operations, and chronic stress. Housing market discrimination makes relocation economically inaccessible. Suppression operates through real estate lock-in: exit costs are prohibitive.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: AFFECTED MUNICIPAL GOVERNMENT (TANGLED ROPE) — Constrained exit. Municipality benefits from logistics tax base and employment claims, but bears infrastructure maintenance costs (road degradation, emergency response for accidents) and political pressure from residents. Enforcement of noise ordinances is unevenly applied; companies have resources to litigate or lobby for exemptions. Mixed coordination-extraction: the system provides regional economic benefit (genuine coordination) while extracting local governance capacity and resident welfare.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: E-COMMERCE PLATFORM AND LOGISTICS OPERATOR (ROPE) — Full arbitrage. Can relocate operations to jurisdictions with lower enforcement, outsource externality costs, or exploit regulatory arbitrage between local and national standards. Experiences the constraint as pure coordination: creating logistics networks that move goods efficiently. The platform has maximum flexibility and exit options; constraint functions as enabling infrastructure for their business model.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL JUSTICE COALITION (TANGLED ROPE) — Organized actors with constrained exit. Coalition benefits from growing awareness of externalization and policy momentum toward burden-shifting accountability (emissions standards, community benefits agreements, worker protections). But enforcement remains uneven and companies can shift operations across jurisdictions. Active enforcement required: litigation, regulation, worker organizing. Mixed benefit (raises awareness, creates legal precedent) and constraint (limited resources, political capture by logistics lobby).
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY FRAMEWORK - NATIONAL LEVEL (PITON) — Formal environmental and labor regulations exist (Clean Air Act provisions, occupational safety standards, noise ordinances), but enforcement is degraded and theatrical. Regulatory capture by logistics industry has created exemptions, delayed implementation timelines, and measurement methodologies that underestimate exposure. Theater ratio is high: compliance is documented but externalization continues through loopholes. The regulatory apparatus persists through institutional inertia despite low functional constraint on the extraction mechanism.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: EXTENDED PRODUCER RESPONSIBILITY MOVEMENT (SCAFFOLD) — Organized agents (EU waste directives, corporate sustainability commitments, shareholder pressure) are building temporary mechanisms to internalize externality costs. Extended producer responsibility (EPR) frameworks create penalties for burden-shifting. Sunset logic applies: as EPR norms mature and international agreements tighten, companies will face genuine cost pressure to relocate externalities back into their operational budgets rather than peripheral communities. Theater is moderate; enforcement gaps exist but trajectory is toward actual cost internalization. Estimated sunset: 15-25 years for full EPR implementation across supply chains.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal/civilizational perspective, some externalization appears irreducible: entropy production in logistics systems must be dissipated somewhere. The core claim: externalization is inherent to complex supply chains — you cannot have centralized benefit without peripheral burden dissipation. However, this perspective risks naturalizing what is actually a contingent institutional arrangement. The engine's false summit detector will reveal that thermodynamic necessity is not the driver; organizational choice and regulatory arbitrage are.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_burden_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(asymmetric_burden_distribution, TR),
    TR >= 0.70.

:- end_tests(asymmetric_burden_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting significant asymmetry in burden distribution. The central beneficiaries capture the full value of logistics efficiency while peripheral populations absorb operational costs without proportional compensation. The extractiveness is not maximal (< 0.66 Snare threshold) because some operational burden is inherent to supply chain physics, not pure extraction — legitimate coordination value exists alongside extraction. The 20-year trajectory (0.32 → 0.58) shows increasing extraction as e-commerce scale amplifies volume and as regulatory capture deepens, allowing companies to avoid cost internalization. Suppression (0.68): High. Barrier mechanisms include: (1) residential/occupational immobility enforced through housing markets and wage structure; (2) information asymmetry — consumers don't perceive burden being imposed elsewhere; (3) regulatory capture — weak enforcement of environmental and labor standards; (4) geographic distance — burden bearers are not political constituencies for benefiting platforms; (5) collective action barriers — dispersed peripheral populations lack organizing power. Theater ratio (0.64): Moderate-high. Formal compliance exists: environmental impact statements, noise ordinances, labor regulations, corporate sustainability commitments. But enforcement is weak and measurement methodologies systematically underestimate exposure. Compliance documentation creates appearance of constraint without functional impact on extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare and rope perspectives is not measurement ambiguity but genuine structural difference in position. The worker bears material costs (noise exposure, occupational stress, health impacts) that are invariant across observational methods. The platform captures material benefits (profit, operational efficiency, market dominance) that are equally invariant. The disagreement is not empirical — it's structural: the same constraint feels like pure extraction to one agent and pure coordination to another because they occupy opposite ends of a benefit flow. The tangled rope perspectives (municipal government, environmental coalition) occupy intermediate positions where genuine coordination function coexists with asymmetric extraction. The piton and mountain perspectives reflect institutional inertia and naturalization respectively — they are not alternative measurements of the same thing, but incorrect framings that the engine's classifier should flag.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim status plus exit options through the sigmoid function f(d). Warehouse workers (victim + trapped) → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extractiveness. Port-adjacent residents (victim + trapped) → same high d and f(d). Municipal governments (mixed: benefit from tax base but victim to infrastructure costs + constrained exit) → d ≈ 0.65 → f(d) ≈ 1.00 → moderate experienced extraction. E-commerce platforms (beneficiary + arbitrage exit) → d ≈ 0.10 → f(d) ≈ -0.01 → negative experienced extractiveness (they experience coordination, not constraint). Environmental justice coalition (beneficiary of policy momentum + constrained exit) → d ≈ 0.45 → f(d) ≈ 0.45 → low-moderate extraction. Regulatory agencies (institutional actor with formal authority but captured + constrained by budget and politics) → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction. The chi formula χ = ε × f(d) × σ(S) scales effective extraction by scope: local warehouses (σ=0.8) show lower χ than global supply chains (σ=1.2), but base extraction ε stays constant at 0.58 — what changes is observability and verification difficulty at scale.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint exhibits mandatrophy because it combines genuine coordination function (efficient logistics networks do solve real distribution problems) with severe asymmetric extraction (burden is systematically imposed on powerless populations). The classification as Tangled Rope (not Snare) is justified by: (1) Beneficiaries clearly identified (consumers, platforms, distribution networks) — genuine coordination function exists; (2) Victims clearly identified (workers, communities, environmental receptors) — asymmetric extraction is structural, not incidental; (3) Active enforcement required (EPR, regulations, worker organizing) — the system persists only through institutional maintenance, not market equilibrium; (4) χ = 0.58 × f(d_beneficiary)×σ(global) ≈ 0.58 × (-0.01) × 1.2 ≈ -0.01 for beneficiaries, but χ = 0.58 × f(d_victim)×σ(local) ≈ 0.58 × 1.42 × 0.8 ≈ 0.66 for victims — the perspectival gap is real and reflects actual structural difference, not measurement error. The false mountain perspective (thermodynamic inevitability) is correctly identified as naturalization: supply chain externalities are not laws of physics but contingent outcomes of how we've chosen to organize logistics and pricing. Cost internalization is technically feasible; regulatory and institutional barriers are the constraint, not entropy. The scaffold perspective (EPR sunset) is the key to mandatrophy resolution: if producer responsibility norms mature and international enforcement strengthens, the externalization lever will gradually transition from Snare/Tangled Rope (for peripheral populations) toward Rope (coordinated burden distribution) or toward Scaffold completion (if norms fully internalize costs). The high extractiveness (0.58) and theater ratio (0.64) justify the mandatory mandatrophy flag: this is a high-extraction system where the coordination function (logistics efficiency) is real but does not justify the burden distribution, and where institutional inertia (regulatory capture, diffuse victim populations) masks the extraction mechanism behind compliance theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    total_burden_quantification,
    'What is the total health and environmental burden imposed on peripheral populations, and how does it compare to the central benefit captured by logistics operators?',
    'Health impact assessment: chronic disease prevalence, respiratory morbidity, noise-induced sleep disruption, occupational injury rates in logistics hubs vs control populations. Environmental sampling: air quality, particulate matter, soil/water contamination. Cost-benefit analysis comparing capitalized health burden to logistics operator profit and consumer surplus.',
    'If total burden >> total benefit: extraction mechanism is severe (Snare classification strengthened, mandatrophy triggered). If benefits roughly equal burden distributed fairly: reclassification toward Rope or Scaffold feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(total_burden_quantification, empirical, 'Quantified health and environmental burden versus central benefit').

omega_variable(
    exit_option_availability,
    'What proportion of burden-bearing populations have realistic economic exit options? Is residential relocation or job switching genuinely available?',
    'Labor market analysis: wage differentials between warehouse/port jobs and accessible alternatives; job mobility rates. Housing market analysis: price gradients, ownership barriers, tenant protection laws, discriminatory practices in peripheral vs central areas. Longitudinal tracking of residential and occupational mobility.',
    'If < 20% have realistic exit: trapped classification confirmed across multiple perspectives. If > 60% have exit: constrained reclassification may be appropriate; this would lower derived directionality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Realistic exit options for burden-bearing populations').

omega_variable(
    regulatory_arbitrage_scope,
    'How much of the externalization mechanism relies on regulatory arbitrage versus inherent operational necessity? Can operators internalize costs if penalties are sufficiently high?',
    'Comparative analysis: jurisdiction-by-jurisdiction enforcement patterns, cost pass-through analysis when regulations tighten. Case studies of companies relocating operations in response to environmental regulations (spatial externalization) versus companies absorbing costs through operational redesign (functional internalization).',
    'If arbitrage is primary driver: Snare/Tangled Rope confirmed; regulatory capture (piton perspective) is accurate. If operational necessity is primary: Mountain perspective gains credibility; thermodynamic argument for irreducibility strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_scope, empirical, 'Role of regulatory arbitrage versus operational necessity').

omega_variable(
    epr_implementation_feasibility,
    'Can extended producer responsibility mechanisms actually internalize externality costs at scale, or will companies find new jurisdictions and loopholes?',
    'Longitudinal study of EPR policy implementation: monitoring whether burden-shifting migrates to non-compliant jurisdictions or whether genuine internalization occurs. Analysis of policy escape routes: outsourcing to developing economies, subsidiary structure exploitation, regulatory forum shopping.',
    'If EPR successfully internalizes costs: Scaffold sunset is real; constraint will transition to Rope as burden distribution becomes coordinated function rather than extraction. If companies evade EPR: Scaffold is aspirational; constraint may intensify (Snare reclassification) as burden concentrates further in EPR-compliant jurisdictions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epr_implementation_feasibility, empirical, 'Extended producer responsibility implementation effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_burden_distribution, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abd_tr_t0, asymmetric_burden_distribution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(abd_tr_t10, asymmetric_burden_distribution, theater_ratio, 10, 0.53).
narrative_ontology:measurement(abd_tr_t20, asymmetric_burden_distribution, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(abd_be_t0, asymmetric_burden_distribution, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(abd_be_t10, asymmetric_burden_distribution, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(abd_be_t20, asymmetric_burden_distribution, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_burden_distribution, resource_allocation).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, regulatory_capture_logistics_sector).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, environmental_justice_infrastructure_disparity).
narrative_ontology:affects_constraint(asymmetric_burden_distribution, occupational_injury_externalization).

% DUAL FORMULATION NOTE:
% The externalization lever decomposes into three downstream constraints: (1) regulatory_capture_logistics_sector (ε ≈ 0.45) — the institutional mechanism that prevents enforcement of standards; (2) environmental_justice_infrastructure_disparity (ε ≈ 0.52) — the housing/location asymmetry that creates residential trapping; (3) occupational_injury_externalization (ε ≈ 0.48) — the labor market mechanism that makes warehouse work the default option for burden-bearing populations. These three constraints are upstream dependencies for the parent externalization lever — removing any one would reduce ε significantly. The parent constraint can be modeled either as their intersection (most severe manifestation) or as a summary constraint capturing the coordinated effect of all three mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asymmetric_burden_distribution, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
