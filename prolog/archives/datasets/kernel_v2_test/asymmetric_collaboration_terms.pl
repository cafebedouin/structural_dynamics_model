% ============================================================================
% CONSTRAINT STORY: asymmetric_collaboration_terms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_asymmetric_collaboration_terms, []).

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
 *   constraint_id: asymmetric_collaboration_terms
 *   human_readable: Asymmetric Collaboration Terms in Academic-Industry Mathematics Partnerships
 *   domain: science_policy/professional_ethics/technology_governance
 *
 * SUMMARY:
 *   Asymmetric collaboration terms in academic-industry mathematics
 *   partnerships have intensified over the past 12 years as academic funding
 *   stagnated while industry demand for mathematical expertise grew. The
 *   constraint exhibits classic Tangled Rope structure from the analytical
 *   perspective: genuine coordination function (academics gain access to
 *   computational resources, real-world problems, and supplemental income;
 *   industry gains specialized expertise) coexists with systematic extraction
 *   (IP restrictions, publication delays, salary arbitrage, erosion of
 *   academic freedom). The structural asymmetry derives from academic labor
 *   market conditions — a large pool of highly trained mathematicians
 *   competing for scarce tenure-track positions — rather than from any
 *   inherent property of academic-industry collaboration. Well-resourced
 *   institutions and senior mathematicians negotiate favorable terms or
 *   decline unfavorable partnerships, experiencing the constraint as pure
 *   coordination (Rope). Early-career mathematicians and underfunded
 *   departments face a choice between accepting extractive terms or forgoing
 *   access to computational infrastructure and income supplementation
 *   entirely, experiencing the constraint as a trap (Snare). The mathematical
 *   commons — the body of openly accessible mathematical knowledge — bears
 *   extraction through delayed publication and proprietary methods with no
 *   compensation mechanism. Theater ratio (0.35) reflects moderate
 *   performative content: collaboration agreements include nominal provisions
 *   for academic freedom and publication rights that are weakly enforced in
 *   practice, and institutional review processes for industry partnerships
 *   are often pro forma.
 *
 * KEY AGENTS:
 *   - Early-Career Mathematicians: Primary victims (powerless/trapped) — face job scarcity, student debt, and lack of computational resources; accept asymmetric terms due to absence of alternatives
 *   - Industry Research Labs: Primary beneficiaries (institutional/arbitrage) — access specialized mathematical expertise at below-market rates while controlling IP and publication timing
 *   - Senior Tenured Mathematicians: Secondary beneficiaries (powerful/arbitrage) — negotiate favorable terms or decline collaborations; benefit from resources without career risk
 *   - Underfunded Academic Departments: Secondary victims (moderate/constrained) — lack resources to provide competitive computational infrastructure; depend on industry partnerships for faculty retention
 *   - Well-Resourced Academic Departments: Secondary beneficiaries (institutional/arbitrage) — can provide competitive resources and negotiate from strength
 *   - Professional Mathematics Societies: Organized actors (organized/constrained) — attempt to establish ethical guidelines but lack enforcement power
 *   - Mathematical Commons: Abstract victim (powerless/trapped) — epistemic commons depleted by IP restrictions and publication delays with no advocate or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(asymmetric_collaboration_terms, 0.58).
domain_priors:suppression_score(asymmetric_collaboration_terms, 0.62).
domain_priors:theater_ratio(asymmetric_collaboration_terms, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(asymmetric_collaboration_terms, extractiveness, 0.58).
narrative_ontology:constraint_metric(asymmetric_collaboration_terms, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(asymmetric_collaboration_terms, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(asymmetric_collaboration_terms, tangled_rope).
narrative_ontology:human_readable(asymmetric_collaboration_terms, "Asymmetric Collaboration Terms in Academic-Industry Mathematics Partnerships").
narrative_ontology:topic_domain(asymmetric_collaboration_terms, "science_policy/professional_ethics/technology_governance").

domain_priors:requires_active_enforcement(asymmetric_collaboration_terms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(asymmetric_collaboration_terms, industry_research_labs).
narrative_ontology:constraint_beneficiary(asymmetric_collaboration_terms, well_resourced_academic_departments).
narrative_ontology:constraint_beneficiary(asymmetric_collaboration_terms, senior_tenured_mathematicians).
narrative_ontology:constraint_victim(asymmetric_collaboration_terms, early_career_mathematicians).
narrative_ontology:constraint_victim(asymmetric_collaboration_terms, underfunded_academic_departments).
narrative_ontology:constraint_victim(asymmetric_collaboration_terms, mathematical_commons).
narrative_ontology:constraint_vindicates(asymmetric_collaboration_terms, market_efficiency_in_knowledge_production).
narrative_ontology:constraint_vindicates(asymmetric_collaboration_terms, private_sector_innovation_superiority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER MATHEMATICIAN (SNARE) — Trapped by academic job scarcity and student debt. Industry collaboration offers computational resources and income but extracts IP rights and publication freedom. Cannot refuse terms due to lack of alternatives. Maximum experienced extraction — structural immobility with no coordination benefit.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-CAREER ACADEMIC (TANGLED ROPE) — Constrained by departmental resource limitations and tenure pressure but has some negotiating power. Benefits from access to industry computational infrastructure and datasets while bearing costs of IP restrictions and delayed publication. Mixed coordination (resource access) and extraction (asymmetric terms).
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRY RESEARCH LAB (ROPE) — Benefits from academic expertise at below-market rates while controlling IP and publication timing. Experiences the constraint as pure coordination: solving the problem of accessing specialized mathematical talent. Net beneficiary with full exit options — can hire in-house if terms become unfavorable.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SENIOR TENURED MATHEMATICIAN (ROPE) — Has bargaining power to negotiate favorable terms or decline unfavorable collaborations. Benefits from industry resources without career risk. Experiences constraint as coordination mechanism enabling productive partnerships. Low effective extraction due to arbitrage-level exit options.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROFESSIONAL SOCIETIES (TANGLED ROPE) — Organized agents (AMS, SIAM, IMU) attempting to establish ethical guidelines for industry collaboration. See both coordination function (facilitating knowledge transfer) and extraction (erosion of academic freedom and commons). Constrained by lack of enforcement power and member heterogeneity.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: MATHEMATICAL COMMONS (SNARE) — Abstract collective good with no advocate. Bears full cost of delayed publication, proprietary methods, and knowledge enclosure. Cannot exit or organize. Maximum extraction — IP restrictions and publication delays extract from the epistemic commons with no compensation mechanism.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, sees genuine coordination function (resource pooling, applied problem access) coexisting with asymmetric extraction (IP capture, salary arbitrage, publication restrictions). The constraint solves real coordination problems while systematically extracting from less powerful participants. Structural asymmetry is contingent on academic funding models and labor market conditions, not inherent to collaboration itself.
constraint_indexing:constraint_classification(asymmetric_collaboration_terms, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_collaboration_terms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(asymmetric_collaboration_terms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_collaboration_terms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(asymmetric_collaboration_terms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(asymmetric_collaboration_terms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Industry captures significant value through below-market access to mathematical expertise, IP control, and publication restrictions. Academics gain computational resources and supplemental income but at terms that would not prevail in a balanced labor market. The value has risen from 0.35 to 0.58 over the 12-year interval as academic job scarcity intensified and industry demand grew, increasing bargaining asymmetry. The extraction is not maximal (not 0.8+) because genuine coordination benefits exist — access to applied problems, real-world validation, and computational infrastructure that many academic departments cannot provide. Suppression (0.62): Moderate-high. Significant barriers to exit include academic job scarcity (tenure-track positions declining relative to PhD production), student debt burden (median $50k-$100k for mathematics PhDs in US), lack of alternative computational resources at underfunded institutions, and career risk of declining industry partnerships (signal of being 'difficult' or 'uncommercial'). Suppression has increased from 0.45 to 0.62 as the academic job market deteriorated and industry partnerships became normalized as expected career activity. Suppression is not total — some mathematicians can and do decline unfavorable terms, and some institutions provide competitive resources — but the structural pressure is substantial and growing. Theater ratio (0.35): Moderate-low. Collaboration agreements typically include nominal provisions for academic freedom, publication rights, and IP sharing that are weakly enforced in practice. Institutional review processes for industry partnerships are often pro forma — approval is presumed unless terms are egregiously extractive. The theater has increased modestly from 0.25 to 0.35 as universities adopted formal 'conflict of interest' review processes that create compliance paperwork without substantively altering power dynamics. The theater ratio is not high because much of the extraction is openly acknowledged — IP restrictions and publication delays are explicit contract terms, not hidden mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how structural position determines classification. Industry research labs and senior tenured mathematicians experience pure coordination (Rope) — they solve legitimate problems (accessing expertise, accessing resources) on favorable terms with full exit options. Early-career mathematicians and the mathematical commons experience pure extraction (Snare) — they bear costs (IP restrictions, publication delays, salary arbitrage) with no exit and minimal coordination benefit. Mid-career academics and professional societies experience mixed coordination and extraction (Tangled Rope) — genuine resource access coexists with asymmetric terms. The analytical observer sees the full structure: a coordination mechanism (resource pooling, applied problem access) that has been captured by bargaining asymmetry to extract systematically from less powerful participants. The gap between the industry Rope perspective and the early-career Snare perspective is not a difference of opinion about the same experience — it is a difference of structural position producing genuinely different experienced constraints. The senior mathematician who can decline unfavorable terms is not experiencing the same constraint as the early-career mathematician who cannot.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Industry research labs are declared beneficiaries with arbitrage exit — they experience low or negative effective extraction (the constraint subsidizes them through below-market access to expertise). Senior tenured mathematicians are declared beneficiaries with arbitrage exit — they experience low effective extraction (favorable terms, full exit options). Early-career mathematicians are declared victims with trapped exit — they experience maximum effective extraction (asymmetric terms, no alternatives). The mathematical commons is declared victim with trapped exit — it experiences maximum extraction (knowledge enclosure with no compensation). Mid-career academics are declared victims with constrained exit — they experience substantial but not maximal extraction (some negotiating power, high but surmountable exit costs). Professional societies are neither pure beneficiaries nor pure victims but are attempting to coordinate — they experience moderate extraction modulated by organized power and constrained exit. The directionality derivation captures the structural reality: extraction flows from trapped and constrained agents toward beneficiaries with arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that Tangled Rope classification requires BOTH genuine coordination function AND asymmetric extraction, with both properties present and measurable. The coordination function is real: academics gain access to computational resources (GPU clusters, cloud computing credits, proprietary datasets) that many underfunded departments cannot provide, and industry gains access to specialized mathematical expertise (optimization theory, numerical analysis, statistical methods) that is scarce in the labor market. The asymmetric extraction is also real: IP restrictions prevent follow-on research, publication delays fragment the citation network, salary arbitrage captures value below market rates, and the terms are enforced through academic job scarcity rather than through genuine mutual benefit. The constraint is not a Rope (pure coordination) because identifiable victims exist and bear systematic costs. It is not a Snare (pure extraction) because the coordination function is genuine — computational resources and applied problem access have real value that academics could not easily obtain otherwise. The Tangled Rope classification captures the structural reality: a coordination mechanism that has been bent toward extraction by power asymmetry, but where the coordination function remains load-bearing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_funding_counterfactual,
    'Would adequate public funding for academic mathematics eliminate the coordination function of industry collaboration, or would complementary benefits (applied problem access, real-world validation) persist?',
    'Comparative analysis of collaboration patterns in well-funded vs underfunded academic systems; survey data on mathematician motivations for industry partnership beyond resource access',
    'If resource access is the only driver: constraint is pure extraction masked as coordination (Snare from more perspectives). If applied problem access and validation remain valuable: genuine coordination function persists even with adequate funding (Rope/Tangled Rope distinction depends on term asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_counterfactual, empirical, 'Whether adequate public funding would eliminate coordination function').

omega_variable(
    ip_restriction_necessity,
    'Are IP restrictions and publication delays necessary for industry to justify collaboration investment, or are they extractive terms enabled by academic labor market weakness?',
    'Analysis of collaboration terms variation across different industry sectors and academic bargaining positions; comparison with open-science industry partnerships (e.g., pharmaceutical precompetitive consortia)',
    'If necessary for investment: IP terms are coordination overhead (supports Rope classification from industry perspective). If enabled by bargaining asymmetry: IP terms are pure extraction (supports Snare classification from academic perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ip_restriction_necessity, empirical, 'Whether IP restrictions are necessary coordination cost or extractive rent').

omega_variable(
    career_trajectory_divergence,
    'Do early-career mathematicians who accept asymmetric industry terms experience long-term career benefits (skill development, network access) that compensate for immediate extraction, or does the asymmetry compound over biographical time?',
    'Longitudinal career tracking of mathematicians with early industry collaboration vs those without; analysis of publication records, citation impact, and career advancement conditional on collaboration terms',
    'If compensatory: constraint is temporary extraction with biographical-scale coordination payoff (Scaffold-like dynamics). If compounding: constraint is persistent extraction that locks early-career mathematicians into subordinate positions (Snare confirmation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_trajectory_divergence, empirical, 'Whether early asymmetric terms compound or resolve over career trajectory').

omega_variable(
    commons_depletion_threshold,
    'At what proportion of mathematics research under IP restriction does the mathematical commons experience irreversible depletion vs sustainable extraction?',
    'Historical analysis of fields with high vs low proprietary enclosure; identification of tipping points where cumulative restrictions prevent follow-on research; measurement of citation network fragmentation',
    'If threshold is high (>60% proprietary): current extraction levels are sustainable coordination overhead. If threshold is low (<30% proprietary): current levels represent commons depletion crisis requiring intervention.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_depletion_threshold, empirical, 'Threshold at which IP restrictions deplete mathematical commons').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(asymmetric_collaboration_terms, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(asym_collab_theater_t0, asymmetric_collaboration_terms, theater_ratio, 0, 0.25).
narrative_ontology:measurement(asym_collab_theater_t3, asymmetric_collaboration_terms, theater_ratio, 3, 0.28).
narrative_ontology:measurement(asym_collab_theater_t6, asymmetric_collaboration_terms, theater_ratio, 6, 0.3).
narrative_ontology:measurement(asym_collab_theater_t9, asymmetric_collaboration_terms, theater_ratio, 9, 0.33).
narrative_ontology:measurement(asym_collab_theater_t12, asymmetric_collaboration_terms, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(asym_collab_extract_t0, asymmetric_collaboration_terms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(asym_collab_extract_t3, asymmetric_collaboration_terms, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(asym_collab_extract_t6, asymmetric_collaboration_terms, base_extractiveness, 6, 0.5).
narrative_ontology:measurement(asym_collab_extract_t9, asymmetric_collaboration_terms, base_extractiveness, 9, 0.55).
narrative_ontology:measurement(asym_collab_extract_t12, asymmetric_collaboration_terms, base_extractiveness, 12, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(asym_collab_suppress_t0, asymmetric_collaboration_terms, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(asym_collab_suppress_t3, asymmetric_collaboration_terms, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(asym_collab_suppress_t6, asymmetric_collaboration_terms, suppression_requirement, 6, 0.56).
narrative_ontology:measurement(asym_collab_suppress_t9, asymmetric_collaboration_terms, suppression_requirement, 9, 0.6).
narrative_ontology:measurement(asym_collab_suppress_t12, asymmetric_collaboration_terms, suppression_requirement, 12, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(asymmetric_collaboration_terms, resource_allocation).
narrative_ontology:affects_constraint(asymmetric_collaboration_terms, academic_publishing_oligopoly).
narrative_ontology:affects_constraint(asymmetric_collaboration_terms, adjunct_labor_precarity).
narrative_ontology:affects_constraint(asymmetric_collaboration_terms, computational_resource_concentration).

% DUAL FORMULATION NOTE:
% Asymmetric collaboration terms are downstream of academic funding austerity and labor market precarity but represent a distinct structural constraint. The upstream constraints (funding cuts, adjunctification) create the bargaining asymmetry; this constraint describes the specific extraction mechanism that operates within industry-academic partnerships given that asymmetry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(asymmetric_collaboration_terms, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
