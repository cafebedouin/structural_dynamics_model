% ============================================================================
% CONSTRAINT STORY: social_justice_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_social_justice_distribution, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: social_justice_distribution
 *   human_readable: Genetic Stratification Through GGM Access Inequality
 *   domain: bioethics/reproductive_medicine/genetic_engineering
 *
 * SUMMARY:
 *   The social justice distribution constraint in germline genetic
 *   modification (GGM) describes the wealth-based stratification of access to
 *   genetic enhancement and disease prevention technologies. As of 2025, GGM
 *   procedures cost $50,000-$200,000 per embryo in wealthy nations, placing
 *   them far beyond the reach of median-income populations. Insurance
 *   coverage is limited to narrow therapeutic indications, excluding
 *   enhancement and many disease-prevention applications. This creates a
 *   structural mechanism for genetic stratification: wealthy populations can
 *   afford interventions that provide their children with health advantages,
 *   cognitive enhancements, and competitive benefits in education and labor
 *   markets, while low-income populations are excluded. The constraint
 *   operates at multiple scales: within wealthy nations (income-based
 *   stratification), between nations (developing countries lack GGM
 *   infrastructure entirely), and across generations (enhanced cohorts
 *   compound their advantages over time). The coordination story — GGM
 *   enables disease prevention and advances human health — is real but
 *   secondary to the extraction mechanism. The primary function is
 *   wealth-based genetic stratification, creating what critics term
 *   'genobility': a hereditary genetic aristocracy based on parental wealth
 *   rather than merit or need.
 *
 * KEY AGENTS:
 *   - Low-Income Populations: Primary victim (powerless/trapped) — structurally excluded by cost barriers; face multigenerational competitive disadvantage as enhanced cohorts enter labor markets
 *   - Uninsured Populations: Primary victim (powerless/trapped) — excluded even in wealthy nations by insurance coverage gaps; medical necessity determinations are performative gatekeeping
 *   - Middle-Income Families: Secondary victim (moderate/constrained) — can access GGM through severe financial sacrifice; experience both coordination (disease prevention) and extraction (wealth depletion, debt)
 *   - High-Income Populations: Primary beneficiary (powerful/arbitrage) — face no financial barriers; children gain competitive advantages; extraction runs toward this group
 *   - Private Fertility Clinics: Primary beneficiary (institutional/arbitrage) — revenue model depends on wealth-based stratification; high-margin procedures for affluent clients
 *   - Public Health Systems: Mixed position (institutional/constrained) — face genuine resource allocation problem but also extraction (impossible mandates, legitimacy erosion)
 *   - Developing Nation Citizens: Primary victim (powerless/trapped) — excluded at national scale; GGM infrastructure concentrated in wealthy nations; civilizational-scale stratification
 *   - Universal Access Advocacy Coalition: Organized agents (organized/mobile) — building alternative funding models and regulatory frameworks; see sunset through cost decline and policy intervention
 *   - Market Naturalization View: Analytical observer (analytical/analytical) — risks naturalizing wealth-based access as immutable economic law; false summit candidate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(social_justice_distribution, 0.68).
domain_priors:suppression_score(social_justice_distribution, 0.72).
domain_priors:theater_ratio(social_justice_distribution, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(social_justice_distribution, extractiveness, 0.68).
narrative_ontology:constraint_metric(social_justice_distribution, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(social_justice_distribution, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(social_justice_distribution, snare).
narrative_ontology:human_readable(social_justice_distribution, "Genetic Stratification Through GGM Access Inequality").
narrative_ontology:topic_domain(social_justice_distribution, "bioethics/reproductive_medicine/genetic_engineering").

domain_priors:requires_active_enforcement(social_justice_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(social_justice_distribution, high_income_populations).
narrative_ontology:constraint_beneficiary(social_justice_distribution, private_fertility_clinics).
narrative_ontology:constraint_beneficiary(social_justice_distribution, medical_tourism_industry).
narrative_ontology:constraint_beneficiary(social_justice_distribution, genetic_testing_companies).
narrative_ontology:constraint_victim(social_justice_distribution, low_income_populations).
narrative_ontology:constraint_victim(social_justice_distribution, uninsured_populations).
narrative_ontology:constraint_victim(social_justice_distribution, developing_nation_citizens).
narrative_ontology:constraint_victim(social_justice_distribution, future_unenhanced_cohorts).
narrative_ontology:constraint_vindicates(social_justice_distribution, market_efficiency_in_healthcare).
narrative_ontology:constraint_vindicates(social_justice_distribution, genetic_meritocracy_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME POPULATIONS (SNARE) — Structurally excluded from GGM access by cost barriers ($50K-$200K per procedure in 2025). No exit: cannot afford treatment, cannot migrate to subsidized jurisdictions, face compounding disadvantage as enhanced cohorts enter labor markets. Extraction is maximal and multigenerational: their children compete against genetically enhanced peers in education and employment without access to the same interventions. The coordination story (GGM enables disease prevention) is cover — the primary function is wealth-based genetic stratification.
constraint_indexing:constraint_classification(social_justice_distribution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNINSURED POPULATIONS (SNARE) — Even in wealthy nations, lack of insurance coverage for GGM creates absolute barriers. US insurance typically excludes enhancement and covers only narrow therapeutic indications. Cannot exit: medical tourism requires capital; domestic access requires insurance or wealth. Theater ratio reflects that 'medical necessity' determinations are performative — the treatment/enhancement boundary is enforced to justify exclusion, not to track genuine medical need.
constraint_indexing:constraint_classification(social_justice_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MIDDLE-INCOME FAMILIES (TANGLED ROPE) — Can access GGM through significant financial sacrifice (second mortgage, retirement savings depletion, medical tourism to lower-cost jurisdictions). Experience both coordination (genuine disease prevention for heritable conditions) and extraction (financial burden, debt, opportunity cost). Constrained exit: can technically access the technology but at severe cost. The constraint coordinates legitimate medical needs while extracting wealth and creating financial precarity.
constraint_indexing:constraint_classification(social_justice_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HIGH-INCOME POPULATIONS (ROPE) — Experience GGM access as pure coordination: can afford procedures domestically or internationally, can select optimal providers, face no financial barriers. Arbitrage exit: can choose jurisdictions with favorable regulatory environments or lower costs. Net beneficiaries: their children gain competitive advantages in education and labor markets. Extraction runs toward this group (they capture the benefits) rather than away from them.
constraint_indexing:constraint_classification(social_justice_distribution, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVATE FERTILITY CLINICS (ROPE) — Primary beneficiaries of the access inequality. Revenue model depends on wealth-based stratification: high-margin procedures for affluent clients, no obligation to serve low-income populations. Arbitrage exit: can relocate to favorable regulatory jurisdictions, can price discriminate across markets. The constraint coordinates their business model — the lack of universal access is not a bug but the revenue structure.
constraint_indexing:constraint_classification(social_justice_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC HEALTH SYSTEMS (TANGLED ROPE) — Face genuine coordination problem (how to allocate scarce resources for expensive interventions) but also experience extraction (political pressure to expand access without funding, liability for health disparities, erosion of universal care principles). Constrained exit: cannot refuse to address GGM policy but lack resources to provide universal access. The constraint coordinates resource allocation while extracting legitimacy and creating impossible mandates.
constraint_indexing:constraint_classification(social_justice_distribution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DEVELOPING NATION CITIZENS (SNARE) — Structurally excluded at national scale. GGM infrastructure concentrated in wealthy nations; costs exceed annual per-capita GDP in many countries. No exit: cannot migrate for treatment, cannot access medical tourism, face compounding disadvantage as enhanced populations in wealthy nations gain cognitive and health advantages. Extraction is civilizational: genetic stratification between nations, not just within them. The coordination story (GGM advances human health) naturalizes what is actually a mechanism for entrenching global inequality.
constraint_indexing:constraint_classification(social_justice_distribution, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: UNIVERSAL ACCESS ADVOCACY COALITION (SCAFFOLD) — Organized groups (bioethics organizations, public health advocates, international health equity coalitions) see the access inequality as a temporary coordination failure with a sunset: subsidized access programs, international technology transfer, and regulatory frameworks mandating coverage are being built. Mobile exit: can shift advocacy resources to more receptive jurisdictions, can build alternative funding models. Sunset logic: as GGM costs decline (projected 10-20 year trajectory to <$10K per procedure) and as political pressure for equity mounts, universal access becomes economically and politically feasible. The constraint's extractive phase is transitional, not permanent.
constraint_indexing:constraint_classification(social_justice_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: MARKET NATURALIZATION VIEW (MOUNTAIN) — From a market-fundamentalist analytical position, wealth-based access to medical technology is an immutable feature of resource allocation: those who can pay receive services; those who cannot do not. This perspective treats the access inequality as a natural law of economics rather than a policy choice. However, the structural data contradicts the mountain classification — the constraint requires active enforcement (insurance exclusions, regulatory barriers to international access, patent protections maintaining high costs), has identifiable beneficiaries (clinics, wealthy populations), and identifiable victims (low-income populations, developing nations). The engine's false summit detector will identify this as naturalization of a constructed constraint.
constraint_indexing:constraint_classification(social_justice_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_justice_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(social_justice_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_justice_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(social_justice_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(social_justice_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts from low-income populations through exclusion from genetic advantages that compound over generations. Wealthy populations capture benefits (enhanced children, competitive advantages) while bearing minimal costs. The extraction has increased over the interval (0.45 → 0.68) as GGM applications have expanded from narrow therapeutic uses to broader enhancement, widening the gap between those who can and cannot access the technology. Suppression (0.72): High. Multiple mechanisms suppress alternatives: cost barriers ($50K-$200K per procedure), insurance exclusions, patent protections maintaining high prices, regulatory barriers to international access, lack of public subsidy programs, and geographic concentration of expertise in wealthy nations. Suppression has increased over the interval (0.55 → 0.72) as the technology has matured and the access gap has widened — early-stage experimental access has transitioned to established clinical practice without corresponding expansion of access. Theater ratio (0.45): Moderate. The treatment/enhancement boundary is substantially performative: insurance 'medical necessity' determinations exclude interventions based on categorical distinctions (therapy vs enhancement) rather than genuine medical need or benefit magnitude. Public health rhetoric emphasizes disease prevention (coordination function) while policy structures maintain wealth-based access (extraction function). However, theater is not as high as in some constraints because the coordination function (disease prevention for heritable conditions) is real for those who can access it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — wealth-based access to GGM — appears as pure extraction (snare) from the perspective of excluded populations, mixed coordination and extraction (tangled rope) from middle-income families and public health systems, pure coordination (rope) from wealthy populations and fertility clinics, temporary coordination failure with a sunset (scaffold) from organized advocacy coalitions, and naturalized economic law (mountain) from market-fundamentalist analytical positions. The gap is not a measurement error but the core phenomenon: extraction is perspectival. Low-income populations experience maximum extraction because they are trapped, powerless, and excluded from benefits. Wealthy populations experience coordination because they are beneficiaries with arbitrage exit. The analytical observer's mountain is a false summit — the constraint naturalizes what is actually a policy choice (insurance coverage, subsidy programs, technology transfer, patent policy) as an immutable feature of resource allocation. The scaffold perspective from advocacy coalitions is structurally real: cost decline and policy intervention can eliminate the access gap, giving the constraint a genuine sunset. The perspectival gap reveals that 'genetic stratification' is not a single phenomenon but a presheaf over observation sites: snare from below, rope from above, scaffold from organized opposition, false summit from naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Low-income populations are full targets (d → 1.0): they bear the cost of exclusion and face compounding disadvantage as enhanced cohorts enter labor markets. The engine derives high d from victim status + trapped exit + powerless position, producing maximum experienced extraction. High-income populations are full beneficiaries (d → 0.0): they capture genetic advantages for their children at minimal relative cost. The engine derives low d from beneficiary status + arbitrage exit + powerful position, producing negative experienced extraction (subsidy). Middle-income families occupy an intermediate position (d ≈ 0.4-0.6): they can access GGM through severe financial sacrifice, experiencing both coordination (disease prevention) and extraction (wealth depletion). The engine derives moderate d from mixed beneficiary/victim status + constrained exit. Private fertility clinics are beneficiaries (d → 0.0): their revenue model depends on the access inequality. Public health systems occupy a complex position (d ≈ 0.3-0.5): they coordinate legitimate resource allocation but also experience extraction through impossible mandates and legitimacy erosion. The universal access advocacy coalition has mobile exit and organized power, producing low experienced extraction despite their structural opposition to the constraint — they can shift resources to more receptive jurisdictions and build alternative models. The analytical observer's mountain classification is a false summit: the constraint requires active enforcement (insurance exclusions, patent protections, regulatory barriers) and has identifiable beneficiaries and victims, contradicting the natural-law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that the classification depends on the observer's structural position. The snare classification from low-income populations is their genuine experience: they are trapped, excluded, and face compounding disadvantage. The rope classification from wealthy populations is their genuine experience: they coordinate access to beneficial technology with minimal cost. The scaffold classification from advocacy coalitions is structurally real: the constraint has a sunset through cost decline and policy intervention. The mountain classification from market-fundamentalist positions is a false summit: the constraint requires active enforcement and has identifiable beneficiaries and victims. No single type is 'the' answer — the presheaf over observation sites IS the answer. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' The constraint's mandate (enable disease prevention through genetic medicine) has not outlived its function, but the extraction mechanism (wealth-based stratification) operates alongside the coordination function (disease prevention), producing tangled rope from intermediate positions and snare from excluded positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cost_trajectory_uncertainty,
    'Will GGM costs decline to levels accessible to median-income populations within a generation, or will they remain concentrated among high-income groups?',
    'Longitudinal cost tracking; comparison to other medical technology diffusion curves (IVF, genetic testing); analysis of patent expiration timelines and generic competition entry',
    'If costs decline rapidly (<$10K by 2040): scaffold perspective confirmed, access inequality is temporary. If costs remain high (>$50K by 2040): snare perspective confirmed, stratification is structural and persistent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_trajectory_uncertainty, empirical, 'Whether GGM cost trajectory enables broad access or maintains stratification').

omega_variable(
    insurance_coverage_expansion,
    'Will insurance systems expand coverage to include GGM for broader indications, or will coverage remain limited to narrow therapeutic uses?',
    'Policy analysis of insurance coverage decisions; tracking of legislative mandates for genetic medicine coverage; international comparison of public health system GGM policies',
    'If coverage expands: tangled rope from more perspectives (coordination function strengthens). If coverage remains narrow: snare from more perspectives (extraction mechanism persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_coverage_expansion, preference, 'Whether insurance coverage expands beyond narrow therapeutic indications').

omega_variable(
    enhancement_normalization,
    'Will genetic enhancement become socially normalized and expected (creating compounding disadvantage for non-enhanced populations), or will social norms resist enhancement and maintain genetic diversity as valued?',
    'Sociological tracking of enhancement attitudes; labor market analysis of enhanced vs non-enhanced cohort outcomes; educational system response to genetic stratification',
    'If enhancement normalizes: extraction severity increases (non-enhanced face compounding disadvantage). If norms resist: extraction severity moderates (diversity remains valued).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enhancement_normalization, preference, 'Whether genetic enhancement becomes socially normalized and expected').

omega_variable(
    international_technology_transfer,
    'Will GGM technology and expertise transfer to developing nations at rates comparable to other medical technologies, or will it remain concentrated in wealthy nations?',
    'Analysis of GGM clinic distribution by national income; tracking of international training programs and technology licensing; comparison to IVF and genetic testing diffusion patterns',
    'If technology transfers: global stratification moderates. If technology remains concentrated: civilizational-scale extraction persists between nations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_technology_transfer, empirical, 'Whether GGM technology transfers to developing nations or remains concentrated').

omega_variable(
    false_summit_market_naturalization,
    'Is wealth-based access to GGM an immutable feature of resource allocation (genuine mountain), or a policy choice that naturalizes constructed inequality (false summit)?',
    'Cross-national comparison of GGM access policies; analysis of alternative funding models (public subsidy, insurance mandates, international aid); historical comparison to other medical technologies that transitioned from luxury to universal access',
    'If genuine mountain: no policy intervention can change access patterns. If false summit: policy choices (subsidies, mandates, technology transfer) can eliminate stratification. The structural data (active enforcement, identifiable beneficiaries and victims) suggests false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_market_naturalization, conceptual, 'Whether market-based access inequality is natural law or constructed constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(social_justice_distribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sjd_theater_2015, social_justice_distribution, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sjd_theater_2018, social_justice_distribution, theater_ratio, 3, 0.38).
narrative_ontology:measurement(sjd_theater_2021, social_justice_distribution, theater_ratio, 6, 0.42).
narrative_ontology:measurement(sjd_theater_2025, social_justice_distribution, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(sjd_extract_2015, social_justice_distribution, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(sjd_extract_2018, social_justice_distribution, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(sjd_extract_2021, social_justice_distribution, base_extractiveness, 6, 0.6).
narrative_ontology:measurement(sjd_extract_2025, social_justice_distribution, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sjd_suppress_2015, social_justice_distribution, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sjd_suppress_2018, social_justice_distribution, suppression_requirement, 3, 0.62).
narrative_ontology:measurement(sjd_suppress_2021, social_justice_distribution, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(sjd_suppress_2025, social_justice_distribution, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(social_justice_distribution, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of treatment_enhancement_boundary. The upstream constraint determines which GGM applications are classified as legitimate therapy vs enhancement, which in turn determines insurance coverage and public subsidy eligibility. The social_justice_distribution constraint describes the wealth-based stratification that results from those coverage decisions. The two constraints have distinct extractiveness values: treatment_enhancement_boundary reflects the epistemic and political contestation over the therapy/enhancement distinction; social_justice_distribution reflects the access inequality that results from applying that distinction in a market-based healthcare system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(social_justice_distribution, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
