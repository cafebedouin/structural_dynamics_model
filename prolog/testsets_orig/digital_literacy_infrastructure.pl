% ============================================================================
% CONSTRAINT STORY: digital_literacy_infrastructure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_literacy_infrastructure, []).

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
 *   constraint_id: digital_literacy_infrastructure
 *   human_readable: Digital Literacy Infrastructure: Coordination and Asymmetric Extraction
 *   domain: education/technology/socioeconomic
 *
 * SUMMARY:
 *   Digital literacy infrastructure presents as a coordination mechanism for
 *   enabling universal access to essential economic and social skills in an
 *   increasingly digital world. Yet structural analysis reveals significant
 *   asymmetric extraction layered beneath the coordination function. The
 *   infrastructure is framed as solving a collective action problem (how to
 *   teach digital skills at scale) while simultaneously creating and
 *   maintaining the problem it claims to solve (defining what counts as
 *   'literacy' in ways that exclude, isolating excluded populations through
 *   cost barriers, and concentrating market power and data extraction in
 *   technology platforms). The constraint exhibits genuine coordination —
 *   shared standards, distributed access mechanisms, open-source tools, and
 *   peer learning — alongside robust extraction mechanisms: platform lock-in,
 *   credential-based gatekeeping, data harvesting, and perpetual reskilling
 *   demands that trap populations in debt cycles. Theater ratio has increased
 *   over the measurement interval as certification systems have proliferated
 *   without corresponding evidence of job placement improvement, suggesting
 *   that performative credentialing is substituting for functional skill
 *   development. The suppression level (0.58) reflects both structural
 *   barriers (cost of devices, internet access, time availability) and
 *   internalized barriers (stigma around 'digital natives' vs 'digital
 *   immigrants,' identity lock-in around educational credentials, geographic
 *   isolation from support networks).
 *
 * KEY AGENTS:
 *   - Technology Platforms (Google, Microsoft, Apple): Primary beneficiaries (institutional/arbitrage) — extract data, attention, and market lock-in; set literacy standards that advantage their tools
 *   - EdTech Companies: Secondary beneficiaries (institutional/arbitrage) — sell access, devices, and credentials at premium margins; benefit from infrastructure demands they help create
 *   - Economically Excluded Populations: Primary victims (powerless/trapped) — lack resources for device purchase and high-speed internet; trapped in perpetual skill-deficit framing
 *   - Rural and Underserved Communities: Secondary victims (moderate/constrained) — geographic barriers compound cost barriers; constrained by limited local instruction and infrastructure investment
 *   - Underfunded Schools: Victims with limited agency (moderate/constrained) — absorb costs of providing digital access while having little say in platform selection or curriculum design
 *   - Public Libraries: Organized mediators (organized/constrained) — attempt to provide free access but bear service burden with inadequate funding; constrained by platform policies and digital divide perpetuation
 *   - Credential Systems: Institutional actors (institutional/arbitrage) — maintain formal certification despite evidence of low correlation with job outcomes; benefit from perpetual reskilling demand
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination function and systematic extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_literacy_infrastructure, 0.52).
domain_priors:suppression_score(digital_literacy_infrastructure, 0.58).
domain_priors:theater_ratio(digital_literacy_infrastructure, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_literacy_infrastructure, extractiveness, 0.52).
narrative_ontology:constraint_metric(digital_literacy_infrastructure, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(digital_literacy_infrastructure, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_literacy_infrastructure, tangled_rope).
narrative_ontology:human_readable(digital_literacy_infrastructure, "Digital Literacy Infrastructure: Coordination and Asymmetric Extraction").
narrative_ontology:topic_domain(digital_literacy_infrastructure, "education/technology/socioeconomic").

domain_priors:requires_active_enforcement(digital_literacy_infrastructure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_literacy_infrastructure, technology_platforms).
narrative_ontology:constraint_beneficiary(digital_literacy_infrastructure, institutional_educators).
narrative_ontology:constraint_beneficiary(digital_literacy_infrastructure, credential_holders).
narrative_ontology:constraint_victim(digital_literacy_infrastructure, economically_excluded_populations).
narrative_ontology:constraint_victim(digital_literacy_infrastructure, rural_underserved_communities).
narrative_ontology:constraint_victim(digital_literacy_infrastructure, underfunded_schools).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ECONOMICALLY EXCLUDED POPULATIONS (SNARE) — Trapped without meaningful alternative. Digital literacy is framed as necessity for economic participation, yet infrastructure and devices cost money they don't have. No viable exit path; constraint forces unpaid or predatory labor to gain access. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNDERFUNDED SCHOOLS AND RURAL COMMUNITIES (TANGLED ROPE) — Structurally benefit from digital literacy coordination (internet, resources, skill-sharing) while simultaneously bearing disproportionate costs (infrastructure burden, digital divide maintenance). Constrained by limited budgets and geographic isolation; some coordination function exists (shared curricula, online resources) but benefits flow asymmetrically toward connected institutions.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECHNOLOGY PLATFORMS AND EDTECH COMPANIES (ROPE) — Primary beneficiaries with maximum exit flexibility. Experience the constraint as a coordination mechanism: platforms standardize digital literacy definitions, educators adopt their tools, users develop skills on their systems. Net positive extraction (concentration of data, attention, and market power) but the coordination function is genuine — platforms do enable education at scale.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PUBLIC LIBRARY SYSTEMS AND COMMUNITY ORGANIZATIONS (TANGLED ROPE) — Organized agents with some coordination function (free access points, beginner instruction) but heavily constrained by budget limitations and policy dependence. Benefit from digital literacy frameworks while bearing outsized burden of serving excluded populations. Active enforcement required to maintain free access against commercial pressure.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDENTIAL AND CERTIFICATION SYSTEMS (PITON) — Digital literacy credentials and certifications persist through institutional inertia despite questionable efficacy. Theater ratio high: credential testing often disconnects from actual job-relevant skills; formal certification rituals maintain legitimacy of increasingly arbitrary skill definitions. The certification infrastructure has become largely performative — employers value demonstrated experience over formal credentials, yet systems persist in demanding expensive certifications.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, digital literacy infrastructure exhibits both genuine coordination (shared standards, distributed access, skill transmission) and significant extraction (market concentration, exclusion mechanisms, data asymmetries). The constraint is neither natural law nor pure coordination — it is a hybrid where coordination enables the extraction mechanism to function at scale.
constraint_indexing:constraint_classification(digital_literacy_infrastructure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_literacy_infrastructure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_literacy_infrastructure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_literacy_infrastructure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_literacy_infrastructure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_literacy_infrastructure, TR),
    TR >= 0.70.

:- end_tests(digital_literacy_infrastructure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The infrastructure coordinates skill transmission (genuine value) while extracting through multiple mechanisms: platform lock-in (users train on proprietary tools), data harvesting (behavioral data monetized), credential gatekeeping (formal certification required despite weak signal), and perpetual reskilling (literacy baselines shift faster than populations can adapt). The measurement trajectory shows increasing extractiveness from 0.38 to 0.52 over 15 years, suggesting that coordination function has been partially captured by rent-seeking behaviors. Suppression (0.58): High. Multiple reinforcing barriers prevent exit: cost of devices and internet (economic), geographic isolation (structural), lack of informal alternative skill-building infrastructure (institutional), and internalized beliefs about 'native' digital competence (cognitive). Rural and excluded populations face all four simultaneously. Theater ratio (0.64, rising to 0.68): High and increasing. Credential systems proliferate despite weak job-placement signals; formal testing diverges from actual workplace skill requirements; certification rituals persist through institutional inertia (piton mechanism). The rising trajectory reflects increasing substitution of performative credentialing for demonstrated skill development. Claimed type: Tangled Rope. The constraint exhibits both genuine coordination (shared standards reduce learning costs, distributed infrastructure reduces duplication, open tools accelerate skill adoption) and significant extraction (beneficiaries extract data and market position; victims bear disproportionate costs). Active enforcement is required — platforms actively maintain extractive mechanisms (lock-in, data policies); schools actively demand formal credentials; governments actively subsidize access to proprietary tools.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Technology platforms experience coordination (Rope) — they solve genuine problems of standardization and scale, and they benefit from the solutions they create. Schools and libraries experience hybrid coordination-extraction (Tangled Rope) — they genuinely coordinate literacy development but bear costs disproportionate to benefits received. Excluded populations experience extraction (Snare) — perpetual debt cycles for access, credential inflation that outruns job relevance, and skill obsolescence that forces repeated reskilling investments. The credential system experiences its own degradation (Piton) — formal certifications persist despite weak predictive validity, maintained through institutional inertia rather than functional necessity. The analytical observer sees the full hybrid (Tangled Rope) — genuine coordination mechanisms that have been progressively captured and repurposed for extraction. The perspectival gap reveals that 'digital literacy' is not a unified good but multiple structurally distinct constraints: access provision (lower extraction if genuinely public; higher if privatized), skill transmission (lower extraction if peer/informal; higher if credentialized), and data extraction (systematic, hidden in most framing). The gap between platform/credential perspective and excluded-population perspective is the constraint's core diagnostic signal.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's d value is computed from their structural relationship to extraction flow: beneficiary status, victim status, and exit cost. Technology platforms gain market lock-in and data harvesting benefits with minimal cost — low d (0.05-0.10), producing negative chi (they benefit more than they pay). Economically excluded populations face genuine material barriers (device cost, internet cost, opportunity cost of learning time) with no meaningful alternative — trapped exit, high d (0.90-0.95), producing high chi (maximum experienced extraction). Schools occupy intermediate position: they benefit from standardized curricula and widely-available tools (coordination) but bear disproportionate cost of providing access to students who can't afford devices (extraction asymmetry) — constrained exit, moderate d (0.55-0.65), producing moderate chi. Credential systems appear to be institutional beneficiaries but analysis reveals they extract from populations through perpetual reskilling demand — the beneficiary status is primarily toward platforms that set literacy standards, not toward the credential systems themselves; this suggests d override is needed for credential system perspective. The directionality logic shows that coordination exists (genuine problem-solving) but is subordinated to extraction mechanisms that concentrate benefits toward platforms while distributing costs toward excluded populations.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing genuine coordination from extraction-enabling coordination. Digital literacy infrastructure DOES coordinate skill transmission — this is not a false claim. But the coordinating mechanism has been captured to serve extraction: platforms standardize literacy around their proprietary tools, credential systems enforce formal testing that excludes informal learners, reskilling demands perpetually relocate the literacy baseline to require repeated investments. The constraint resolves as Tangled Rope because BOTH coordination and extraction are structurally present and necessary. Remove the coordination (eliminate shared standards, distributed access), and the constraint collapses — platforms cannot extract from populations that lack access. The coordination function is prerequisite for the extraction to function at scale. The mandatrophy check: Is the constraint better classified as pure coordination (Rope) or pure extraction (Snare)? Neither works. Rope requires low suppression (current suppression is 0.58, high); Snare requires minimal coordination benefit (current coordination benefit is significant). Tangled Rope is correct because both functions are load-bearing and because one (coordination) serves the other (extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    access_vs_capability_distinction,
    'Does providing device and internet access constitute ''digital literacy infrastructure,'' or does literacy require cognitive-skill development that infrastructure alone cannot deliver?',
    'Longitudinal outcomes tracking: compare populations with access-only vs access-plus-instruction; measure job-placement and income effects 5-10 years post-intervention',
    'If access alone sufficient: constraint is primarily coordination (lower ε). If instruction/cognition required: constraint includes behavioral/psychological extraction (higher ε). Current ambiguity allows platforms to claim literacy success while excluding learning-disabled or neurodivergent populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_vs_capability_distinction, empirical, 'Whether digital literacy requires instruction beyond infrastructure access').

omega_variable(
    skill_obsolescence_acceleration,
    'How rapidly do digital literacy skill baselines shift? Is the infrastructure teaching stable skills or chasing ever-moving targets?',
    'Curriculum content analysis over 5-year periods; skill-relevance surveys from employers; job posting analysis for required digital competencies',
    'If baselines stable: infrastructure enables durable capability (coordination function). If obsolescence rapid: infrastructure is extractive churn (populations perpetually behind, paying for reskilling). May shift type from Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_obsolescence_acceleration, empirical, 'Rate of digital literacy skill obsolescence and curriculum churn').

omega_variable(
    platform_lock_in_irreversibility,
    'Once a population is trained on a specific platform''s tools (Microsoft, Google, Apple ecosystems), what is the actual cost of switching to alternatives?',
    'Market analysis of cross-platform skill transferability; survey of retraining costs when individuals shift between ecosystems; analysis of vendor lock-in contractual terms in educational institutions',
    'If switching costs low: platform extraction is bounded (competitive pressure restrains rent-seeking). If switching costs high: platforms function as hidden landlords extracting through path dependency. May support higher suppression and extractiveness values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lock_in_irreversibility, empirical, 'Switching costs and lock-in irreversibility across digital literacy platforms').

omega_variable(
    informal_vs_formal_literacy_parity,
    'Is formally credentialed digital literacy functionally superior to skills developed through informal practice and peer learning?',
    'Job-placement outcome comparison: formally credentialed vs self-taught for equivalent roles; employer satisfaction surveys; task completion speed and error rates for standardized tasks',
    'If parity high: formal credential system is extractive theater (Piton justified). If formal superior: credential system provides genuine filtering (lower theater, more Rope-like). Currently unclear — suggests high theater but actual parity may be higher than displayed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_vs_formal_literacy_parity, empirical, 'Functional equivalence of formal vs informal digital literacy development').

omega_variable(
    data_asymmetry_monetization_scope,
    'What is the actual monetary value of user data and attention extracted through digital literacy platforms, and how does it relate to the stated educational service value?',
    'Platform financial disclosures; analysis of ad-targeting data derived from user behavior; comparative study of free vs paid (ad-free) literacy platform outcomes for equivalent skill development',
    'If data value exceeds stated service value: platforms are primarily extractive engines disguised as education (moves toward Snare). If service value exceeds data value: extraction is real but secondary to coordination (supports Tangled Rope). Current data opacity prevents definitive answer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_asymmetry_monetization_scope, empirical, 'Monetary value of user data extracted vs educational service value provided').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_literacy_infrastructure, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dilit_tr_t0, digital_literacy_infrastructure, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dilit_tr_t5, digital_literacy_infrastructure, theater_ratio, 5, 0.58).
narrative_ontology:measurement(dilit_tr_t10, digital_literacy_infrastructure, theater_ratio, 10, 0.64).
narrative_ontology:measurement(dilit_tr_t15, digital_literacy_infrastructure, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(dilit_be_t0, digital_literacy_infrastructure, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dilit_be_t5, digital_literacy_infrastructure, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(dilit_be_t10, digital_literacy_infrastructure, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dilit_be_t15, digital_literacy_infrastructure, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_literacy_infrastructure, resource_allocation).
narrative_ontology:affects_constraint(digital_literacy_infrastructure, credential_inflation).
narrative_ontology:affects_constraint(digital_literacy_infrastructure, platform_lock_in_labor_markets).
narrative_ontology:affects_constraint(digital_literacy_infrastructure, data_extraction_educational_systems).
narrative_ontology:affects_constraint(digital_literacy_infrastructure, geographic_digital_divide).

% DUAL FORMULATION NOTE:
% Digital literacy infrastructure is a constraint family decomposed into multiple structurally distinct constraints: device/internet access (infrastructure_access, ε≈0.45), skill transmission (skill_transmission_coordination, ε≈0.35), credential gatekeeping (credential_inflation, ε≈0.62), and data extraction (platform_data_harvesting, ε≈0.68). The present story models the aggregate constraint; individual stories model specific mechanisms and their decomposition enables precision measurement of which extraction mechanism is dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_literacy_infrastructure, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
