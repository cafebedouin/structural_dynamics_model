% ============================================================================
% CONSTRAINT STORY: brazil_hiv_treatment_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_brazil_hiv_treatment_access, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: brazil_hiv_treatment_access
 *   human_readable: Brazil HIV Treatment Access Constraint
 *   domain: healthcare/pharmaceutical_policy
 *
 * SUMMARY:
 *   Brazil's HIV treatment access constraint represents a tangled
 *   intersection of pharmaceutical patent regimes, public health
 *   decentralization policy, geographic disparity, and community
 *   organization. Brazil has a constitutional commitment to free universal
 *   healthcare (Art. 196) and pioneered generic antiretroviral manufacturing
 *   in the 1990s, yet treatment gaps persist, particularly for low-income and
 *   marginalized communities. The constraint exhibits all six DR types
 *   depending on perspective: powerless patients experience it as a snare
 *   (trapped between formal free provision and practical barriers); community
 *   health workers experience tangled rope (genuine coordination function
 *   alongside exploitation); the public health system experiences rope
 *   (coordination of distribution); activists experience organized snare
 *   (capable of leverage but unable to exit crisis response); the
 *   international patent regime and pharmaceutical manufacturers experience
 *   rope or profitable tangled rope (depending on directionality); generic
 *   manufacturers see a scaffold (sunset logic as patents expire);
 *   decentralization policy exhibits piton (performative integration without
 *   functional delivery). The theater ratio has risen from 0.42 to 0.58 as
 *   formal policies have expanded while functional delivery has stagnated,
 *   indicating increasing performative content. Extractiveness has risen from
 *   0.38 to 0.52 as pharmaceutical costs have accumulated and geographic
 *   disparity has widened despite nominal free provision.
 *
 * KEY AGENTS:
 *   - Low-income HIV patients: Primary victims (powerless/trapped) — face cost barriers, geographic access gaps, stigma; cannot exit without treatment initiation
 *   - Community health workers: Secondary actor (moderate/constrained) — provide genuine access coordination but are underfunded and emotionally exploited
 *   - Public health system (SUS): Institutional coordinator (institutional/arbitrage) — distributes antiretrovirals and manages supply chains; extracts minimal surplus
 *   - HIV activist coalition: Organized opposition (organized/constrained) — fights access barriers but consumes organizational resources in perpetual crisis response
 *   - International patent regime: Global beneficiary (institutional/arbitrage) — incentivizes drug development; extracts through monopoly pricing
 *   - Pharmaceutical manufacturers (branded): Global extractor (powerful/arbitrage) — benefits from patent protections; threatened by generic competition
 *   - Generic manufacturers: Alternative pathway (institutional/constrained) — produces lower-cost antiretrovirals; faces patent disputes and scaling constraints
 *   - Healthcare decentralization policy: Performative institution (institutional/analytical) — exists on paper; functional delivery is degraded and uneven
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(brazil_hiv_treatment_access, 0.52).
domain_priors:suppression_score(brazil_hiv_treatment_access, 0.65).
domain_priors:theater_ratio(brazil_hiv_treatment_access, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(brazil_hiv_treatment_access, extractiveness, 0.52).
narrative_ontology:constraint_metric(brazil_hiv_treatment_access, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(brazil_hiv_treatment_access, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(brazil_hiv_treatment_access, tangled_rope).
narrative_ontology:human_readable(brazil_hiv_treatment_access, "Brazil HIV Treatment Access Constraint").
narrative_ontology:topic_domain(brazil_hiv_treatment_access, "healthcare/pharmaceutical_policy").

domain_priors:requires_active_enforcement(brazil_hiv_treatment_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(brazil_hiv_treatment_access, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(brazil_hiv_treatment_access, international_patent_holders).
narrative_ontology:constraint_beneficiary(brazil_hiv_treatment_access, healthcare_bureaucracy).
narrative_ontology:constraint_victim(brazil_hiv_treatment_access, low_income_hiv_patients).
narrative_ontology:constraint_victim(brazil_hiv_treatment_access, marginalized_communities).
narrative_ontology:constraint_victim(brazil_hiv_treatment_access, treatment_initiation_delays).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME HIV PATIENT (SNARE) — Faces interconnected barriers: geographic distance to treatment centers, cost of antiretrovirals despite nominal free provision, employment disruption from clinic visits, and stigma preventing access to informal care networks. Cannot exit the constraint without treatment initiation, yet treatment requires navigating the constraint. Maximum extraction experienced through delayed treatment and health deterioration.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY HEALTH WORKER NETWORK (TANGLED ROPE) — Provides genuine coordination function (connecting dispersed patients to treatment, reducing stigma through trusted local advocates) while bearing asymmetric extraction: underfunded, high burnout, limited career progression, emotional labor without adequate support infrastructure. The network both enables access and is exploited by the system.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH SYSTEM (ROPE) — Experiences the constraint as coordination: distributing antiretrovirals, managing supply chains, integrating HIV care into primary health centers. The system benefits from international technical support and disease-focused funding that prioritizes HIV. Extracts minimal surplus — primarily manages cost distribution.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HIV ACTIVIST COALITION (SNARE, ORGANIZED) — Organized groups (ABIA, Grupo Pela Vidda) fight for access but face resource constraints, legal barriers to advocacy, and the energy cost of perpetual crisis response. Their organization enables some leverage, but they cannot exit the constraint without dismantling the structural barriers that created the activist need.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL PATENT REGIME (ROPE) — Provides coordination function (incentivizing drug development, standardizing treatment protocols internationally) while benefiting patent holders. For Brazil, the regime is extractive at the national level but the international observers see it as pure coordination—cost distribution across globally-distributed development.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: GENERIC ANTIRETROVIRAL MANUFACTURERS (SCAFFOLD) — Brazil's capacity to produce generic ARVs (through FARMANGUINHOS and licensed manufacturers) creates an alternative pathway with sunset logic: as generic patents expire and manufacturing capacity matures, dependence on expensive branded pharmaceuticals declines. High suppression currently (patent disputes, manufacturing bottlenecks) but declining trajectory as sunset approaches.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: HEALTHCARE DECENTRALIZATION POLICY (PITON) — The formal policy of integrating HIV care into primary health centers was progressive in design but has degraded into performative compliance: clinics are formally designated to provide HIV services but lack resources, training, and supply chain integration. Theater ratio elevated because policy exists on paper but functional delivery is uneven.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 8: BRANDED PHARMACEUTICAL MANUFACTURER (TANGLED ROPE, POWERFUL) — Both coordinates global drug distribution and extracts monopoly rents through patent protections. From the manufacturer's perspective, Brazil's generics policy threatens their arbitrage exit (can shift production elsewhere). High directionality asymmetry: powerful with multiple exits, so effective extraction is dampened despite baseline ε being high.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: CIVILIZATIONAL ANALYTICAL VIEW (MOUNTAIN, FALSE SUMMIT RISK) — At the scale of civilizational time and universal scope, pharmaceutical supply constraints might appear as natural laws of biology and economics. But the structural data (beneficiary/victim declarations, measurable suppression, organized opposition) reveals this as naturalization of contingent policy choices. The mountain classification is a false summit that the engine should flag.
constraint_indexing:constraint_classification(brazil_hiv_treatment_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(brazil_hiv_treatment_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(brazil_hiv_treatment_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(brazil_hiv_treatment_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(brazil_hiv_treatment_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(brazil_hiv_treatment_access, TR),
    TR >= 0.70.

:- end_tests(brazil_hiv_treatment_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint extracts from patients through delayed treatment initiation, forced geographic mobility, and disrupted employment. The extraction flows to patent holders and pharmaceutical manufacturers through monopoly pricing, and to healthcare bureaucracy through administrative overhead. Suppression (0.65): High. Barriers include: geographic distance to treatment centers (Brazil's vast territory and sparse rural infrastructure), informal cost barriers despite nominal free provision (transportation, lost wages, informal fees), stigma preventing care-seeking, knowledge gaps about treatment availability, and limited clinic hours. Theater ratio (0.58): Moderate-high. Formal policy (decentralized HIV care, generic manufacturing, free provision) performs universally but functional delivery is inconsistent. Clinics are designated to provide HIV services but lack resources, training, and reliable supply chains. The gap between policy rhetoric ('universal healthcare') and patient experience (treatment delays, geographic barriers) creates performative theater.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap separates the manufacturer's arbitrage perspective (rope or tangled rope with profitable extraction, multiple exit options) from the patient's trapped perspective (snare, no exit). The manufacturer sees the constraint as coordinating global pharmaceutical supply; the patient sees it as preventing access to life-saving treatment. Both are accurate descriptions of the same structural mechanism—but experienced from positions with radically different extraction flows and exit capacities. The analytical observer risks naturalizing the constraint as a function of scarcity or geography (mountain) rather than recognizing it as a policy choice (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) derives from: (1) structural position in the constraint (beneficiary vs victim); (2) exit options available (arbitrage vs trapped); (3) power level relative to the constraint's enforcement. Patients benefit from formal free provision but pay through geographic mobility and employment disruption—they are net victims (d ≈ 0.95). The public health system distributes costs across the population—it is roughly balanced (d ≈ 0.50) or slightly beneficiary if it captures prestige from universal coverage. Manufacturers with arbitrage options experience low d despite high baseline extraction (ε = 0.52) because they can exit: they derive d from their powerful/arbitrage position (d ≈ 0.10), producing negative or very low effective extraction χ from their perspective. The generic manufacturers derive medium d (≈0.50) because they face patent disputes and scaling barriers but can eventually scale. The formula χ = ε × f(d) × σ(S) translates these structural positions into experienced extractiveness: low-income patients experience high χ (high d, high f(d), large scope σ increases national-level d to 1.0); manufacturers experience low χ (low d, negative f(d)). The gap between these experienced values is the perspectival divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating institutional framing from structural reality. The false mountain classification (decentralization as natural evolution of healthcare) obscures the tangled rope reality: the system coordinates treatment access AND asymmetrically extracts from the most vulnerable agents. The mandate to provide universal free HIV treatment (constitutional, international commitments) conflicts with the extractive effect of geographic disparity and cost barriers. Resolution: classify as tangled rope (genuine coordination of drug distribution alongside asymmetric extraction of mobility and time costs from low-income patients), not as mountain (inevitable scarcity) or rope (pure coordination). The scaffold perspective (generic manufacturing exit path) provides a partial resolution mechanism—as patents expire and manufacturing scales, the pharmaceutical extraction component declines, leaving primarily geographic/administrative coordination costs. The piton perspective (performative decentralization) flags the degradation of the formal policy, indicating that sustained investment in functional delivery is required to prevent the policy from further hollowing into theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_vs_access_causality,
    'Is treatment delay primarily caused by direct cost to patients or by supply chain / geographic access barriers?',
    'Cohort analysis comparing outcomes in regions with subsidized transport vs high drug cost regions; patient interviews on primary barriers to treatment initiation',
    'If cost-driven: generic manufacturing expansion is high-impact solution. If supply-chain-driven: geographic decentralization and logistics investment are required. Affects which perspective (pharma snare vs logistics tangled-rope) is most accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_vs_access_causality, empirical, 'Primary cause of treatment access delay: cost vs supply chain').

omega_variable(
    generic_manufacturing_ceiling,
    'Can Brazil''s generic manufacturing capacity scale to meet 100% of national demand while maintaining quality and supply chain resilience?',
    'Capacity audit of FARMANGUINHOS and licensed manufacturers; comparison with India''s generic scale; supply chain vulnerability analysis',
    'If yes: scaffold sunset is real, pharmaceutical dependence terminates within 10 years. If no: Brazil remains partially dependent on patent-protected imports, tangled-rope extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_manufacturing_ceiling, empirical, 'Feasibility of full domestic generic manufacturing scale').

omega_variable(
    activist_leverage_sustainability,
    'Can organized HIV activism sustain collective action against suppression without additional structural support?',
    'Historical analysis of activist organization persistence; funding source stability; comparison with activist movements that have degraded or splintered',
    'If sustainable: organized perspective is accurate, coalition can extract concessions. If unsustainable: organized status is temporary, coalition will fragment into powerless agents.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(activist_leverage_sustainability, empirical, 'Sustainability of organized activist capacity without systemic support').

omega_variable(
    geographic_access_mechanism,
    'Is geographic access disparity a function of infrastructure investment (fixable) or inherent to Brazil''s geography and population distribution (quasi-natural)?',
    'Cost-benefit analysis of mobile clinics and telemedicine in remote regions; comparison with access outcomes in regions with similar geography but different investment',
    'If fixable: geographic barrier is policy choice, not constraint. If quasi-natural: geographic suppression is partially structural, extraction is less avoidable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_access_mechanism, empirical, 'Whether geographic access disparity is policy-fixable or quasi-natural').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(brazil_hiv_treatment_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bhta_tr_t0, brazil_hiv_treatment_access, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bhta_tr_t5, brazil_hiv_treatment_access, theater_ratio, 5, 0.52).
narrative_ontology:measurement(bhta_tr_t10, brazil_hiv_treatment_access, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(bhta_be_t0, brazil_hiv_treatment_access, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(bhta_be_t5, brazil_hiv_treatment_access, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(bhta_be_t10, brazil_hiv_treatment_access, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(brazil_hiv_treatment_access, resource_allocation).
narrative_ontology:boltzmann_floor_override(brazil_hiv_treatment_access, 0.18).
narrative_ontology:affects_constraint(brazil_hiv_treatment_access, pharmaceutical_patent_enforcement).
narrative_ontology:affects_constraint(brazil_hiv_treatment_access, healthcare_geographic_disparity).
narrative_ontology:affects_constraint(brazil_hiv_treatment_access, activist_coalition_sustainability).

% DUAL FORMULATION NOTE:
% Brazil's HIV treatment access is decomposed into three related constraints: (1) pharmaceutical_patent_enforcement (ε≈0.70, pure extraction at global level) dominates pricing; (2) healthcare_geographic_disparity (ε≈0.45, independent infrastructure constraint) creates access barriers; (3) activist_coalition_sustainability (ε≈0.50, organizational extraction) determines organized opposition capacity. The present story integrates all three but treating them separately reveals that generic manufacturing sunset (scaffold perspective) primarily addresses constraint 1, while mobile clinics and telemedicine address constraint 2, and sustained activist funding addresses constraint 3. Full resolution requires simultaneous intervention on all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(brazil_hiv_treatment_access, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
