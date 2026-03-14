% ============================================================================
% CONSTRAINT STORY: longevity_research_prioritization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_longevity_research_prioritization, []).

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
 *   constraint_id: longevity_research_prioritization
 *   human_readable: Longevity Research Prioritization and Resource Extraction
 *   domain: biomedical_research/life_sciences_funding
 *
 * SUMMARY:
 *   The prioritization of longevity research over disease-burden research in
 *   global biomedical science represents a structural constraint that
 *   extracts from researchers studying infectious disease, maternal
 *   mortality, and malnutrition while benefiting wealthy-world aging
 *   populations, pharmaceutical companies, and elite research institutions.
 *   The constraint operates through funding concentration (NIH, EU Horizon,
 *   Gates Foundation prioritize aging), publication incentives (high-impact
 *   journals favor aging research), career gatekeeping (prestigious
 *   professorships go to gerontologists), and knowledge infrastructure (aging
 *   cohorts, biomarkers, methodologies are standardized while tropical
 *   medicine infrastructure decays). This is simultaneously a coordination
 *   mechanism (aging research genuinely requires standardized protocols,
 *   shared cohorts, and interdisciplinary knowledge) and an extraction
 *   mechanism (the same coordination benefits are denied to disease-burden
 *   researchers). The theater ratio reflects that longevity research is
 *   increasingly justified through abstract concepts like 'healthspan' and
 *   'biological aging' rather than concrete benefits, while disease-burden
 *   research must constantly prove applied relevance. The constraint has
 *   intensified over 20 years as aging populations in high-income countries
 *   have grown, concentrated wealth has increasingly driven research agendas,
 *   and biotechnology companies have aligned with longevity research
 *   investment.
 *
 * KEY AGENTS:
 *   - Global Disease-Burden Researchers: Primary victims (powerless/trapped) — cannot exit disease-burden research without identity loss; face structural barriers to funding, publication, and career advancement
 *   - Early-Career Scientists: Secondary victims (moderate/constrained) — can pursue alternative agendas but at high career cost; benefit from some infrastructure but gatekept by longevity-research establishment
 *   - Wealthy Aging Populations: Primary beneficiaries (institutional/arbitrage) — research agendas are designed around their diseases; market incentives and demographic power align with longevity prioritization
 *   - Pharmaceutical Companies: Secondary beneficiaries (institutional/arbitrage) — capture market value from longevity research; can invest in any domain but choose aging because research coordination reduces drug development costs
 *   - Public Funding Agencies: Institutional actors (institutional/constrained) — coordinate research infrastructure while simultaneously responding to aging-population political pressure; cannot easily reverse prioritization
 *   - Elite Research Institutions: Institutional hierarchies (institutional/arbitrage) — have internalized aging research as core identity; increasingly caught between global health advocacy pressure and endowment-protected aging research centers
 *   - Global Health Advocacy Coalition: Organized actors (organized/constrained) — perceive sunset pathway through health equity movement, non-communicable disease burden in middle-income countries, and pressure for research sovereignty
 *   - Analytical Observer: Universal perspective (analytical/analytical) — risks naturalizing contingent political economy as immutable demographic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(longevity_research_prioritization, 0.58).
domain_priors:suppression_score(longevity_research_prioritization, 0.62).
domain_priors:theater_ratio(longevity_research_prioritization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(longevity_research_prioritization, extractiveness, 0.58).
narrative_ontology:constraint_metric(longevity_research_prioritization, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(longevity_research_prioritization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(longevity_research_prioritization, tangled_rope).
narrative_ontology:human_readable(longevity_research_prioritization, "Longevity Research Prioritization and Resource Extraction").
narrative_ontology:topic_domain(longevity_research_prioritization, "biomedical_research/life_sciences_funding").

domain_priors:requires_active_enforcement(longevity_research_prioritization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(longevity_research_prioritization, wealthy_aging_populations).
narrative_ontology:constraint_beneficiary(longevity_research_prioritization, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(longevity_research_prioritization, elite_research_institutions).
narrative_ontology:constraint_victim(longevity_research_prioritization, global_disease_burden_researchers).
narrative_ontology:constraint_victim(longevity_research_prioritization, early_career_scientists).
narrative_ontology:constraint_victim(longevity_research_prioritization, developing_world_health_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL DISEASE BURDEN RESEARCHERS (SNARE) — Powerless researchers studying infectious disease, maternal mortality, and malnutrition in low-income regions face structural trapping: funding is concentrated in longevity research serving wealthy populations; publishing venues prioritize aging science; career advancement requires alignment with high-income country research agendas. No exit option exists — they cannot walk away from disease burden research and still maintain identity as public health researchers. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(longevity_research_prioritization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER SCIENTISTS (TANGLED ROPE) — Face constrained exit: can pursue alternative research agendas but at cost of reduced funding, delayed tenure, publication disadvantage. They also benefit from the longevity research ecosystem through established mentorship networks, infrastructure access, and validated research methodologies. Genuine coordination function (knowledge infrastructure) coupled with asymmetric extraction (career gatekeeping by senior researchers in aging science).
constraint_indexing:constraint_classification(longevity_research_prioritization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL COMPANIES (ROPE) — Primary beneficiaries with arbitrage options; can redirect investment to any research domain but choose longevity because market incentives align with wealthy aging populations. Experience the constraint as coordination: longevity research prioritization solves the collective action problem of standardizing aging biomarkers, sharing aging cohorts, and developing gerontology methodologies. No extraction experienced — benefits flow toward these agents.
constraint_indexing:constraint_classification(longevity_research_prioritization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC FUNDING AGENCIES (TANGLED ROPE) — Constrained by political pressures, aging electorates in high-income countries, and biotechnology lobbying. They coordinate genuine research infrastructure (databases, cohorts, protocols) while simultaneously extracting from disease-burden research by redirecting funds. Institutional actors with real constraints — politically difficult to reverse longevity prioritization once embedded in budgets and constituencies.
constraint_indexing:constraint_classification(longevity_research_prioritization, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: GLOBAL HEALTH ADVOCACY COALITION (SCAFFOLD) — Organized actors (WHO, NGOs, disease-specific foundations) see the prioritization imbalance as a temporary policy failure with a sunset: increasing pressure for health equity, growing prevalence of non-communicable diseases in middle-income countries, and emerging research on aging in non-Western populations create exit pathways. Their constraint is constrained exit (policy change is difficult but possible over 15-20 years), and their classification reflects low effective extraction because organized actors have agency and perceive an exit path.
constraint_indexing:constraint_classification(longevity_research_prioritization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ELITE RESEARCH INSTITUTION HIERARCHIES (PITON) — Universities and research centers have largely internalized longevity prioritization as their institutional identity (aging research centers, gerontology departments) despite growing awareness that this concentrates resources on wealthy population problems. The hierarchy persists through inertia: prestigious journals feature aging research, grant committees are dominated by gerontologists, career pathways are built around aging science. Theater ratio is high because the institutional prestige system performs legitimacy (citations, impact factors, rankings) for aging research while disease-burden research is dismissed as 'applied' or 'development work.' The institutions see their own prioritization as increasingly contested but lack agency to redirect because reputation and endowment interests are fused with aging science dominance.
constraint_indexing:constraint_classification(longevity_research_prioritization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, wealthier societies naturally prioritize research addressing their own population burdens; this is an inevitable structural feature of how funding follows demographic and economic power. This perspective naturalizes what appears as a mountain: aging research prioritization is inherent to global political economy. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit. The prioritization is contingent on funding mechanisms, publication incentives, and institutional gatekeeping, not on immutable natural laws.
constraint_indexing:constraint_classification(longevity_research_prioritization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(longevity_research_prioritization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(longevity_research_prioritization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(longevity_research_prioritization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(longevity_research_prioritization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(longevity_research_prioritization, TR),
    TR >= 0.70.

:- end_tests(longevity_research_prioritization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from disease-burden researchers through funding concentration, publication bias, and career gatekeeping, but the extraction is not total because alternative funding sources exist (some Gates Foundation work, WHO initiatives, national disease-control programs). The value reflects that the extraction is significant and growing but not absolute. Suppression (0.62): Moderate-high. Barriers to exit include epistemic gatekeeping (disease-burden research is dismissed as 'applied' rather than 'basic science'), publication venue control (high-impact journals favor aging research), funding concentration (NIH and EU spend >25% of life sciences budget on aging-related research despite lower burden-of-disease ranking), and career path dependency (early-career scientists must build reputation in dominant fields). However, some alternative pathways exist: disease-specific foundations, global health initiatives, and emerging recognition of antimicrobial resistance and pandemic risk as civilizational-scale problems. Theater ratio (0.65): Moderate-high. Longevity research is increasingly justified through performance metrics (publication counts, impact factors, biotechnology deal flow) rather than concrete health outcomes. The aging research establishment performs legitimacy through citation networks and prestigious journal placement while disease-burden research must constantly prove applied relevance. The theater has increased over the interval as aging research has become more specialized and removed from clinical application, while funding justifications have shifted to increasingly abstract concepts (healthspan, biological aging, geroscience) that are difficult to operationalize. The extractiveness has grown from 0.35 to 0.58 over 20 years as wealth concentration has accelerated, aging populations have grown in political power, and biotechnology companies have consolidated funding influence. The theater ratio has grown from 0.45 to 0.65 as aging research has become more specialized and infrastructure-dependent, making it harder for outside critics to assess whether the research is solving real health problems or performing scientific legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival gap across power levels and exit options. Disease-burden researchers see a snare — pure extraction with minimal coordination benefit, no exit, structural trapping. Early-career scientists see tangled rope — mixed coordination (infrastructure access, mentorship) and extraction (gatekeeping, publication bias). Pharmaceutical companies see rope — coordination mechanism that solves the collective action problem of standardizing aging biomarkers and sharing research costs. Public funding agencies see tangled rope — they coordinate research infrastructure while simultaneously responding to political pressure for aging research. Elite institutions see piton — their longevity research identity persists through inertia and endowment protection despite awareness that the prioritization is increasingly contested. The global health coalition sees scaffold — they perceive an exit pathway through institutional innovation, health equity advocacy, and emerging recognition of infectious disease and pandemic risk as global priorities. The analytical observer risks seeing mountain — naturalizing what is actually a contingent outcome of political economy as an immutable feature of demographic structure. This perspectival diversity reflects the genuine structural ambiguity: longevity research IS a valid research agenda (coordination benefits are real, aging is increasingly important to global health), but it ALSO IS an extractive mechanism (disease-burden research is systematically underfunded, career pathways are gatekept, global health priorities are subordinated to wealthy-world problems).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim declarations and exit options. Disease-burden researchers are victims with trapped exit, deriving high d (close to 1.0, maximum target). Wealthy aging populations are beneficiaries with arbitrage exit, deriving low d (close to 0.0, full beneficiary). Pharmaceutical companies are beneficiaries with arbitrage exit, deriving low d. Early-career scientists are mixed: they are partly victims (gatekept) and partly beneficiaries (infrastructure access), with constrained exit, deriving moderate d (0.5-0.6). Public funding agencies are institutional beneficiaries (respond to aging population demand) with constrained exit, deriving moderate d. Elite institutions are beneficiaries (aging centers drive prestige and funding) with arbitrage-level options (could reallocate but don't), deriving low-moderate d (0.3-0.4). The global health coalition are victims (their priorities are subordinated) but organized with constrained exit, deriving moderate d (0.55-0.60). These derivations map to the perspectival classifications: high d agents see snare (disease-burden researchers), low d agents see rope (pharmaceutical companies), moderate d agents see tangled rope (early-career scientists, funding agencies).
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves the mandatrophy by showing the structural difference between genuine coordination and asymmetric extraction. Longevity research prioritization is NOT purely extractive (pure snare) because the coordination function is real: aging biomarkers are genuinely standardized, methodologies are shared, research infrastructure enables knowledge creation that benefits multiple agents. It is also NOT purely coordination (pure rope) because the asymmetry is real: disease-burden researchers cannot access the same infrastructure, career pathways are gatekept by the longevity-research establishment, and the prioritization reflects political economy (aging voters in high-income countries) rather than burden-of-disease rankings. The tangled rope classification captures both: the constraint coordinates aging research (genuine function) while simultaneously extracting from disease-burden research (asymmetric cost distribution). The false mountain perspective (analytical/natural law) reveals the risk: the constraint naturalizes what is actually a contingent outcome of funding mechanisms and political power as an immutable feature of demographic structure. This is precisely where mandatrophy resolution matters: the insight that 'aging societies naturally prioritize aging research' is a cover story that obscures the institutional choices (funding allocation, publication venue control, career gatekeeping) that could be changed. The scaffold perspective (organized/generational) provides the exit path: global health equity advocacy, emerging recognition of infectious disease and pandemic risk, and demographic transition in middle-income countries create conditions for institutional innovation that could rebalance research priorities without eliminating longevity research.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    burden_of_disease_measurement_contestation,
    'Do standardized burden-of-disease metrics (DALYs, QALYs) accurately capture the research value and moral urgency of disease-burden research, or do they systematically underweight global health problems relative to aging research?',
    'Comparative analysis of burden-of-disease rankings vs actual funding allocation; empirical assessment of whether metrics-based funding would shift resources from longevity to infectious disease research',
    'If metrics are unbiased: prioritization reflects genuine burden distribution, constraint is weaker (Rope from more perspectives). If metrics systematically underweight global disease burden: prioritization is extraction masquerading as optimization (Snare confirmed for affected researchers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_of_disease_measurement_contestation, empirical, 'Whether burden-of-disease metrics accurately represent research priorities').

omega_variable(
    knowledge_transfer_asymmetry,
    'Does longevity research generate transferable methodologies and biological insights that meaningfully accelerate disease-burden research, or does the specialization and focus on aging-specific biomarkers create cognitive silos that isolate aging science from infectious disease and tropical medicine?',
    'Citation analysis of methods transfer between aging and disease-burden research domains; interview data on whether aging biology training produces researchers who apply insights to global health problems',
    'If transfer is significant: tangled rope classification is correct — genuine coordination benefits offset some extraction. If silos dominate: extraction is larger and purer (Snare for disease-burden researchers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_transfer_asymmetry, empirical, 'Whether aging research creates transferable knowledge for disease-burden research').

omega_variable(
    demographic_transition_future_burden,
    'As low-income countries age over the next 40 years, will aging research conducted now become relevant to their aging populations, or will the research designs and biomarkers be too specific to wealthy-country pathways to transfer?',
    'Longitudinal comparison of whether current longevity research benefits aging populations in countries with different health systems, genetic backgrounds, and disease ecologies; forecasting analysis of demographic transition timing and aging research applicability',
    'If research transfers effectively: prioritization is an early-mover coordination benefit with future global relevance (Rope from longer time horizon). If specificity prevents transfer: current extraction is unjustified by future benefits (Snare classification holds even at generational time scale).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(demographic_transition_future_burden, empirical, 'Whether aging research serves future aging populations in low-income countries').

omega_variable(
    political_economy_of_aging_demographics,
    'Is the concentration of funding in longevity research driven by the aging demographics and voting power of wealthy countries, or by genuine scientific opportunity and return on investment?',
    'Comparative analysis of funding allocation in countries with different age structures; correlation between electoral power of elderly voters and research funding distribution; counterfactual modeling of funding absent demographic pressure',
    'If driven primarily by political economy: prioritization is extractive mechanism (Snare/Tangled Rope from disease-burden perspective). If driven by scientific opportunity: prioritization reflects legitimate research value (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_aging_demographics, conceptual, 'Whether aging research prioritization reflects demographics or scientific opportunity').

omega_variable(
    alternative_funding_mechanism_feasibility,
    'Could global health priorities be served by alternative funding mechanisms (e.g., advance market commitments for vaccine development, disease eradication bonds, international health security funds) that would reduce extraction from disease-burden researchers without requiring redistribution from longevity research?',
    'Policy analysis of alternative funding mechanisms; modeling of resource expansion vs reallocation; empirical assessment of whether advocacy coalitions could successfully implement global health funding infrastructure',
    'If alternatives are feasible: scaffold perspective confirmed — sunset is possible through institutional innovation rather than zero-sum reallocation. If alternatives require reallocation: constraint is genuinely extractive and zero-sum (Snare/Tangled Rope with no exit path).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_mechanism_feasibility, preference, 'Whether alternative funding can address global health without redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(longevity_research_prioritization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(longrp_tr_t0, longevity_research_prioritization, theater_ratio, 0, 0.45).
narrative_ontology:measurement(longrp_tr_t10, longevity_research_prioritization, theater_ratio, 10, 0.62).
narrative_ontology:measurement(longrp_tr_t20, longevity_research_prioritization, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(longrp_be_t0, longevity_research_prioritization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(longrp_be_t10, longevity_research_prioritization, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(longrp_be_t20, longevity_research_prioritization, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(longevity_research_prioritization, resource_allocation).
narrative_ontology:boltzmann_floor_override(longevity_research_prioritization, 0.18).
narrative_ontology:affects_constraint(longevity_research_prioritization, antimicrobial_resistance_research_marginalization).
narrative_ontology:affects_constraint(longevity_research_prioritization, tropical_medicine_infrastructure_decay).
narrative_ontology:affects_constraint(longevity_research_prioritization, early_career_scientist_career_bottleneck).

% DUAL FORMULATION NOTE:
% Longevity research prioritization is upstream of several downstream constraints: antimicrobial resistance research is marginalized partly because funding is concentrated in aging research; tropical medicine infrastructure decays as institutional investment follows longevity priorities; early-career scientists face bottlenecks partly because career pathways are gatekept by aging-research establishment. Each downstream constraint has its own ε value reflecting how the upstream prioritization mechanism manifests in domain-specific ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(longevity_research_prioritization, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
