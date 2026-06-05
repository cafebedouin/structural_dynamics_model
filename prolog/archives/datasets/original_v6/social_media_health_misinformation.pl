% ============================================================================
% CONSTRAINT STORY: social_media_health_misinformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_media_health_misinformation, []).

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
 *   constraint_id: social_media_health_misinformation
 *   human_readable: Social Media Health Misinformation Trap
 *   domain: public_health/digital_communication/information_ecosystems
 *
 * SUMMARY:
 *   The social media health misinformation constraint represents a
 *   multi-layered extraction system targeting vulnerable populations with
 *   health information gaps. The constraint operates across three
 *   interlocking institutional actors (social media platforms optimized for
 *   engagement, alternative health vendors seeking monetization, and
 *   traditional medical gatekeepers maintaining institutional authority) and
 *   exploits structural information asymmetries in health knowledge.
 *   Vulnerable health seekers and low-literacy populations face trapped
 *   status with no practical exit from the information environment, while
 *   alternative vendors experience the constraint as a coordination mechanism
 *   enabling customer acquisition. The constraint's extractiveness has
 *   increased over the measured interval (0.35 to 0.68) as algorithmic
 *   amplification has intensified, while theater ratio has remained moderate
 *   (0.42 to 0.58) — the misinformation ecosystem exhibits genuine functional
 *   extraction (vendor sales, platform engagement metrics) rather than purely
 *   performative activity, though platform fact-checking represents theater
 *   (visible effort without solving underlying algorithmic amplification).
 *
 * KEY AGENTS:
 *   - Vulnerable Health Seekers: Primary victims (powerless/trapped) — chronic illness or urgent health needs create inescapable information demand; health literacy gaps prevent claim evaluation; bear full cost of misinformation through delayed treatment and fraudulent product purchases
 *   - Low Health Literacy Populations: Primary victims (powerless/trapped, generational) — education and healthcare inequities reproduce vulnerability across generations; structurally excluded from legitimate health information access
 *   - Alternative Health Vendors: Primary beneficiaries (institutional/arbitrage) — monetize misinformation directly through supplement sales, telehealth consultations, and wellness products; high arbitrage capacity to reposition across regulatory changes
 *   - Social Media Platforms: Secondary beneficiary/mixed actor (institutional/constrained) — benefit from health misinformation engagement metrics but bear regulatory and reputational costs; caught between algorithm optimization and public health harm
 *   - Public Health Information Coalition: Organized responder (organized/constrained) — fact-checking networks, health literacy initiatives, media literacy programs building alternative pathways; perceive sunset logic where improved information infrastructure can reduce misinformation penetration
 *   - Traditional Medical Gatekeepers: Institutional defenders (institutional/arbitrage) — maintain authority through licensing and credentialing but have lost functional credibility in some domains due to historical exclusions; gatekeeping mechanism operates through theater as much as competence
 *   - Analytical Observer: Universalizing perspective (analytical/analytical) — risks naturalizing as inherent information asymmetry what is actually contingent on platform incentives and information architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_media_health_misinformation, 0.68).
domain_priors:suppression_score(social_media_health_misinformation, 0.72).
domain_priors:theater_ratio(social_media_health_misinformation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_media_health_misinformation, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_media_health_misinformation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_media_health_misinformation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_media_health_misinformation, snare).
narrative_ontology:human_readable(social_media_health_misinformation, "Social Media Health Misinformation Trap").
narrative_ontology:topic_domain(social_media_health_misinformation, "public_health/digital_communication/information_ecosystems").

domain_priors:requires_active_enforcement(social_media_health_misinformation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_media_health_misinformation, alternative_health_vendors).
narrative_ontology:constraint_beneficiary(social_media_health_misinformation, sensationalist_media_outlets).
narrative_ontology:constraint_beneficiary(social_media_health_misinformation, engagement_optimized_platforms).
narrative_ontology:constraint_victim(social_media_health_misinformation, vulnerable_health_seekers).
narrative_ontology:constraint_victim(social_media_health_misinformation, chronically_ill_patients).
narrative_ontology:constraint_victim(social_media_health_misinformation, low_health_literacy_populations).
narrative_ontology:constraint_victim(social_media_health_misinformation, public_health_infrastructure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VULNERABLE HEALTH SEEKER (SNARE) — Patient with chronic illness or urgent health need lacks medical expertise to evaluate claims, faces information asymmetry, and bears full cost of misinformation (delayed treatment, worsened condition, financial loss to fraudulent products). No structural exit: health needs are inescapable, and false health information saturates accessible channels. Maximum experienced extraction.
constraint_indexing:constraint_classification(social_media_health_misinformation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LOW HEALTH LITERACY POPULATIONS (SNARE) — Structural inability to evaluate medical claims due to education barriers, language barriers, or limited access to legitimate medical resources. Trapped across generations as educational and healthcare inequities reproduce. Misinformation fills information void created by absent health infrastructure. Extraction is intergenerational and compounding.
constraint_indexing:constraint_classification(social_media_health_misinformation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ALTERNATIVE HEALTH VENDOR (ROPE) — Benefits from misinformation ecosystem through direct monetization (supplement sales, telehealth consultations, wellness packages). Experiences the constraint as coordination of information flow: sharing health claims enables audience engagement and customer acquisition. Net beneficiary with significant arbitrage capacity — can reposition, rebrand, or shift product lines as regulation changes. Low experienced extraction.
constraint_indexing:constraint_classification(social_media_health_misinformation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOCIAL MEDIA PLATFORM (TANGLED ROPE) — Benefits from engagement metrics driven by health misinformation (high emotional arousal, sharing rates, algorithmic amplification). Also bears cost of regulatory pressure, advertiser flight, and reputational damage from health harms. Active enforcement of content policies required but creates theater: platforms remove obviously false claims while algorithmic recommendations surface subtle misinformation. Moderate extraction with mixed costs and benefits.
constraint_indexing:constraint_classification(social_media_health_misinformation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLIC HEALTH INFORMATION COALITION (SCAFFOLD) — Organized response (fact-checking networks, health literacy initiatives, media literacy programs) building alternative pathways to reliable health information. See the misinformation constraint as temporary and solvable through education, fact-checking infrastructure, and platform policy reform. Low effective extraction because coalition has agency and structural exits (media literacy investment, fact-check systems) with sunset logic. Expects constraint to weaken as information infrastructure matures.
constraint_indexing:constraint_classification(social_media_health_misinformation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL MEDICAL GATEKEEPERS (PITON) — Medical licensing, peer review, and clinical standards enforce orthodoxy that misinformation challenge. The gatekeeping mechanism persists through institutional inertia despite demonstrating theater: patients distrust institutions that denied certain conditions historically (chronic Lyme disease, long COVID early recognition), fueling misinformation adoption. The constraint maintains traditional authority through performative credentialing, not functional superiority in actual patient outcomes for complex conditions.
constraint_indexing:constraint_classification(social_media_health_misinformation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universalizing analytical perspective, information asymmetry in health is inherent: complex medical knowledge is inaccessible to lay people, creating inevitable gap between claim and verifiability. Misinformation proliferation is 'natural' outcome of this gap across digital media. However, structural data reveals this as false summit: the misinformation extraction relies on specific institutional arrangements (algorithmic amplification, engagement metrics, advertiser incentives, platform business models) rather than information asymmetry alone. Alternative information architectures (decentralized verification, transparent recommendation systems, health cooperative models) demonstrate the constraint is contingent, not natural.
constraint_indexing:constraint_classification(social_media_health_misinformation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_media_health_misinformation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_media_health_misinformation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_media_health_misinformation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_media_health_misinformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(social_media_health_misinformation, TR),
    TR >= 0.70.

:- end_tests(social_media_health_misinformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The constraint exhibits strong extraction mechanisms: alternative vendors capture direct revenue from vulnerable patients; social media platforms capture engagement value and attention; traditional gatekeepers maintain institutional authority. The metric increased from 0.35 to 0.68 over the interval as algorithmic amplification intensified health misinformation penetration. This is not inherent to health information — it reflects specific platform business model incentives (engagement optimization) and vendor incentives (monetization of uncertainty). Suppression (0.72): High. Vulnerable populations face multiple barriers to exit: health needs are inescapable (biological constraint), health literacy is asymmetrically distributed (educational constraint), and algorithm-driven information channels concentrate misinformation (architectural constraint). Low-literacy populations have compounding barriers through language access and healthcare system exclusion. Suppression is not total (some patients can access reliable information through existing institutions, some platforms implement fact-checking), but the barriers are severe enough to trap significant populations. Theater ratio (0.58): Moderate-high. Platform content moderation and fact-checking represent performative activity: visible effort to remove false claims that does not address underlying algorithmic amplification mechanisms. Traditional medical gatekeeping has increased theater as institutional credibility declined in specific domains (chronic Lyme disease, long COVID early in pandemic, vaccine adverse events) — patients distrust institutions, fueling misinformation adoption despite performative credentialing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival differentiation. Vulnerable health seekers see pure extraction (snare) with no coordination benefits and no exit. Vendors see pure coordination (rope) enabling their business model. Platforms see mixed costs and benefits (tangled rope) requiring active enforcement that itself becomes theater. Public health actors see a solvable problem with sunset logic (scaffold) — infrastructure investment creates pathway to reduced misinformation. Medical institutions see their authority degraded but persisting (piton) — credentialing continues but without functional foundation in patient trust. The analytical observer risks seeing natural law (mountain) in what is actually contingent on specific institutional incentives (platform algorithms, vendor monetization, gatekeeper exclusions). The core gap: the beneficiaries (vendors, platforms) experience the constraint as enabling their coordination while the victims (vulnerable seekers) experience it as pure extraction with no exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from structural position: vulnerable seekers are trapped with no alternatives (d→1.0, maximum f(d)) experiencing full extraction; vendors are beneficiaries with arbitrage options (d→0.0, minimum f(d)) experiencing no extraction; platforms are caught between beneficiary position (engagement metrics) and victim position (regulatory harm), producing intermediate d (~0.5-0.65); public health coalition has constrained exit (requires infrastructure investment) but organized agency (d~0.4); medical gatekeepers maintain institutional arbitrage with decreasing functional authority (d~0.1-0.2). The chi formula χ = ε × f(d) × σ(S) applies: global scope (σ=1.2) amplifies platform and vendor extractiveness, while vulnerable populations at global scope means maximum experienced extraction for powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE-DOMINANT WITH INSTITUTIONAL HYBRIDS: The base classification (snare) is accurate from the primary victim perspective — vulnerable populations experience high extraction with no practical exit. However, the constraint exhibits secondary snare, tangled rope, and piton characteristics depending on institutional position. The mandatrophy is resolved by recognizing that the snare classification dominates victim-centered analysis while the tangled rope classification (platforms) and rope classification (vendors) are legitimate from beneficiary perspectives. The constraint maintains itself through institutional hybridity: platforms need vendors to generate engaging content, vendors need platforms to reach vulnerable patients, gatekeepers maintain authority through credentialing theater. No single perspective reveals the full constraint — the presheaf over the observation site (victim, platform, vendor, coalition, gatekeeper, analytical) IS the constraint. The false mountain peak (natural law of information asymmetry) is detected by the structural data: alternative information architectures (decentralized health networks, cooperative information systems, transparent algorithms) demonstrate the constraint is contingent, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    health_literacy_supply_problem,
    'Is health misinformation''s power primarily extractive (vendors gaming patients) or primarily an information supply problem (insufficient health literacy infrastructure)?',
    'Compare misinformation adoption rates in regions with vs without mandatory health literacy programs; correlate patient outcomes against education access holding vendor density constant',
    'If primarily extractive: snare classification stands, require direct vendor accountability. If primarily supply problem: scaffold classification dominates, public health infrastructure investment is the primary intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(health_literacy_supply_problem, empirical, 'Extractive vs information supply mechanism distinction').

omega_variable(
    algorithmic_amplification_necessity,
    'Would health misinformation persist at current volume without algorithmic amplification by social media platforms, or is it inherent to distributed information networks?',
    'Historical comparison with pre-algorithmic bulletin boards and health forums; analysis of information flow patterns in platforms with chronological vs engagement-based feeds',
    'If algorithmic amplification is necessary: platform business models are direct extractors (snare component is contingent on platform incentives). If misinformation persists without amplification: extraction is vendor-driven, not platform-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_necessity, empirical, 'Whether algorithmic amplification is necessary for misinformation persistence').

omega_variable(
    trust_deficit_causality,
    'Does medical institution distrust drive misinformation adoption, or does exposure to misinformation drive distrust?',
    'Longitudinal studies tracking trust trajectories and misinformation exposure in cohorts; temporal ordering analysis of trust decline vs misinformation engagement',
    'If distrust drives adoption: gatekeeping piton classification is accurate (loss of functional authority). If misinformation drives distrust: constraint is primarily platform extraction (snare amplified by algorithmic engagement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trust_deficit_causality, empirical, 'Causality between trust deficit and misinformation adoption').

omega_variable(
    fact_check_effectiveness_threshold,
    'At what fact-check infrastructure density and distribution do vulnerable populations experience misinformation as escapable vs inescapable?',
    'Measure misinformation adoption rates and patient harm in jurisdictions with varying fact-check coverage; correlation with health literacy program investment',
    'If threshold is achievable: scaffold classification is structurally real, sunset is possible. If threshold is unachievable: trapped classification is fundamental, structural changes to information architecture are required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fact_check_effectiveness_threshold, empirical, 'Achievability threshold for fact-check saturation').

omega_variable(
    platform_business_model_dependency,
    'Can platforms maintain user engagement and revenue without engagement-optimized algorithms that amplify health misinformation?',
    'Natural experiment analysis of platform engagement metrics under chronological feed, algorithmic feed, and hybrid models; revenue impact modeling',
    'If platforms cannot sustain without amplification: platform business model is directly extractive (tangled rope with high enforcement cost). If alternative models are viable: platform perspective shifts from beneficiary to constrained institutional actor.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_business_model_dependency, empirical, 'Platform business model viability under different algorithmic regimes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_media_health_misinformation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smhm_tr_t0, social_media_health_misinformation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(smhm_tr_t5, social_media_health_misinformation, theater_ratio, 5, 0.5).
narrative_ontology:measurement(smhm_tr_t10, social_media_health_misinformation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(smhm_tr_t7, social_media_health_misinformation, theater_ratio, 7, 0.54).

% Extraction over time
narrative_ontology:measurement(smhm_be_t0, social_media_health_misinformation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(smhm_be_t5, social_media_health_misinformation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(smhm_be_t10, social_media_health_misinformation, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(smhm_be_t7, social_media_health_misinformation, base_extractiveness, 7, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_media_health_misinformation, resource_allocation).
narrative_ontology:boltzmann_floor_override(social_media_health_misinformation, 0.12).
narrative_ontology:affects_constraint(social_media_health_misinformation, vaccine_hesitancy_reinforcement).
narrative_ontology:affects_constraint(social_media_health_misinformation, medical_trust_erosion).
narrative_ontology:affects_constraint(social_media_health_misinformation, chronic_disease_self_management_failure).
narrative_ontology:affects_constraint(social_media_health_misinformation, health_literacy_structural_inequality).

% DUAL FORMULATION NOTE:
% Social media health misinformation is upstream of multiple domain-specific health claim constraints. Vaccine hesitancy, medical distrust erosion, and chronic disease self-management constraints all receive amplification through this platform-mediated extraction system. The network linkage documents how general misinformation infrastructure affects specific health domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_media_health_misinformation, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
