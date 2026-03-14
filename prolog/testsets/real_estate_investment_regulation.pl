% ============================================================================
% CONSTRAINT STORY: real_estate_investment_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_real_estate_investment_regulation, []).

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
 *   constraint_id: real_estate_investment_regulation
 *   human_readable: Real Estate Investment Regulation and Affordable Housing Coordination
 *   domain: economic/housing_policy
 *
 * SUMMARY:
 *   Real estate investment regulation exists to coordinate housing provision
 *   and protect tenants from extraction, yet simultaneously enables the
 *   extraction it purports to constrain. This constraint exhibits a hybrid
 *   structure across six perspectives that diagnoses the central tension:
 *   regulation creates predictable rules that attract investment capital, but
 *   that capital then organizes around regulatory loopholes, captures
 *   enforcement agencies, and uses legal complexity to suppress tenant
 *   organizing. The framework shows steady degradation over the 45-year
 *   interval measured here (extractiveness rising from 0.38 to 0.62, theater
 *   rising from 0.42 to 0.65), indicating that the regulatory apparatus is
 *   increasingly performative rather than functionally protective. This
 *   pattern is classic mandatrophy resolution: the regulation was designed to
 *   solve a coordination problem (how to provide housing to low-income
 *   populations), but the mechanism it created (investor-friendly property
 *   rules, tax incentives, market-rate development with mandatory set-asides)
 *   has become an extraction apparatus that uses coordination language
 *   ('market efficiency', 'housing supply', 'economic development') to
 *   legitimize asymmetric capital flows. The measured degradation suggests
 *   that the regulation's primary function has shifted from housing provision
 *   to investor rent-seeking, while the coordination language persists as
 *   theater.
 *
 * KEY AGENTS:
 *   - Low-Income Renters: Primary victims (powerless/trapped) — bear extraction through above-wage-growth rents, eviction threat, substandard conditions, no organizing power
 *   - Institutional Investors: Primary beneficiaries (institutional/arbitrage) — capture appreciation, tax benefits, regulatory certainty, can exit or reallocate freely
 *   - Tenant Organizing Coalition: Organized secondary victims (organized/constrained) — can sometimes block or slow extraction but lack structural power to fundamentally reshape the constraint
 *   - Housing Trust Fund Coalition: Organized alternative-model builders (organized/constrained) — see extraction as temporary and are building parallel systems with sunset logic
 *   - Municipal Governments: Institutional actors with dual role — benefit from development fees and tax base expansion while nominally responsible for housing provision; experience constrained arbitrage
 *   - Zoning and Land Use System: Institutional structure (institutional/arbitrage) — persists through inertia; theater-heavy procedure blocks most affordable housing proposals while legitimizing developer-friendly projects
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent market-based housing allocation as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(real_estate_investment_regulation, 0.58).
domain_priors:suppression_score(real_estate_investment_regulation, 0.65).
domain_priors:theater_ratio(real_estate_investment_regulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(real_estate_investment_regulation, extractiveness, 0.58).
narrative_ontology:constraint_metric(real_estate_investment_regulation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(real_estate_investment_regulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(real_estate_investment_regulation, tangled_rope).
narrative_ontology:human_readable(real_estate_investment_regulation, "Real Estate Investment Regulation and Affordable Housing Coordination").
narrative_ontology:topic_domain(real_estate_investment_regulation, "economic/housing_policy").

domain_priors:requires_active_enforcement(real_estate_investment_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(real_estate_investment_regulation, institutional_investors).
narrative_ontology:constraint_beneficiary(real_estate_investment_regulation, real_estate_development_firms).
narrative_ontology:constraint_beneficiary(real_estate_investment_regulation, municipal_governments).
narrative_ontology:constraint_victim(real_estate_investment_regulation, low_income_renters).
narrative_ontology:constraint_victim(real_estate_investment_regulation, first_time_homebuyers).
narrative_ontology:constraint_victim(real_estate_investment_regulation, housing_affordability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RENTER (SNARE) — Trapped in housing markets where investment capital has concentrated ownership and driven rents beyond wage growth. No viable exit: relocation carries massive switching costs (transportation, childcare, school district disruption, informal social networks). Cannot organize collective bargaining due to high tenant turnover and atomization. Experiences the regulation as pure extraction: compliance costs are passed to renters through higher rents, while tenant protections are minimal and unenforced. Maximum structural extraction with near-total suppression of alternatives.
constraint_indexing:constraint_classification(real_estate_investment_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TENANT ORGANIZING COALITION (TANGLED ROPE) — Organized agents (tenant unions, housing justice groups) experience the regulation as hybrid coordination-extraction. The regulation creates genuine coordination: rent control, habitability standards, eviction protections, and anti-discrimination rules do solve collective action problems around housing stability. But the constraint simultaneously enables extraction: landlords/investors evade regulations through legal loopholes (corporate ownership structures, management company fragmentation), speculation during regulatory gaps, and systematic underinvestment in properties where rent control caps profit. The coalition has significant agency but faces substantial external suppression: legal barriers to enforcement, resource asymmetries in litigation, political capture of regulatory bodies. Genuine mixed structure with both coordination function and asymmetric extraction.
constraint_indexing:constraint_classification(real_estate_investment_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL INVESTOR (ROPE) — Large real estate investment firms, REITs, and institutional capital experience the regulation as coordination. The framework creates predictable rules: tax treatment of rental income, depreciation schedules, regulatory certainty around property rights, and enforcement against tenant activism. These actors have high exit optionality—capital is mobile, they can reallocate to other markets, other asset classes, or other countries. For them, regulation is a coordination mechanism that enables efficient capital deployment. They benefit from the framework's legitimacy (properties held under rules that investors trust) and from suppression of tenant organizing that would drive up labor costs. Net beneficiaries with substantial agency and escape routes.
constraint_indexing:constraint_classification(real_estate_investment_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOUSING TRUST FUND COALITION (SCAFFOLD) — Organized public and nonprofit actors (municipal housing authorities, community land trusts, housing trust funds) see the regulation as temporary scaffolding with a sunset clause. These actors are building alternative ownership and financing models (social housing, community control, equitable development frameworks) that will eventually replace the investor-driven model. The scaffolding coordinates housing delivery during the transition period via inclusionary zoning mandates, affordable housing set-asides, and funding mechanisms. Low effective extraction because the coalition perceives an exit path: as alternative models mature (15-30 year horizon), traditional investor-driven rental markets become optional rather than necessary. Theater ratio is moderately high—many policies are symbolic (targets that are missed) rather than functional, yet the real work of alternative model building is ongoing.
constraint_indexing:constraint_classification(real_estate_investment_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ZONING AND LAND USE SYSTEM (PITON) — The underlying land use and zoning framework that constrains investment regulation is substantially inertial and performative. Zoning codes were designed mid-20th century for single-family suburban development. Modern density, mixed-use, and affordable housing requirements exist on paper but are functionally blocked by:inherited zoning restrictions, NIMBY politics embedded in local governance, permitting processes with high theater and low approval rates. The zoning system persists through institutional inertia despite minimal functional verification that it achieves stated housing goals. Theater ratio is high—extensive public comment processes, environmental reviews, and planning procedures exist but outcome rates show that only 5-15% of proposed affordable units are actually built. The system maintains itself through procedural complexity and constituency capture, not because zoning regulation works.
constraint_indexing:constraint_classification(real_estate_investment_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some tension between housing as commodity and housing as social right may be immutable: any system allocating scarce land must choose between market efficiency and social provision, and no choice fully escapes extraction. This perspective risks naturalizing what is actually a contingent institutional choice: the assumption that private investment capital should be the primary housing supplier is a policy decision, not a law of nature. The engine will flag this as a false summit—the structural data shows the tension is resolvable through alternative models (social housing, community control), not immutable.
constraint_indexing:constraint_classification(real_estate_investment_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(real_estate_investment_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(real_estate_investment_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(real_estate_investment_regulation, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(real_estate_investment_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(real_estate_investment_regulation, TR),
    TR >= 0.70.

:- end_tests(real_estate_investment_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting genuine asymmetric capital flows to investors while acknowledging that some housing does get built through the regulated system. The value has risen over the interval from 0.38 to 0.62, indicating increasing extraction as regulatory loopholes have been discovered and capital has learned to work around protections. Suppression (0.65): High. Low-income renters face substantial barriers to collective action (atomization, legal liability for organizing activity, resource asymmetries in disputes with landlords, threat of eviction used to silence complaints), and tenant organizing faces structural opposition from investor-aligned local governments. Theater ratio (0.58): Moderate-high, indicating that roughly 58% of regulatory activity is performative rather than functionally protective. Public comment periods, environmental reviews, affordability targets that are missed, enforcement actions that rarely occur—these create the appearance of regulation without proportional housing security outcomes. The theater has increased over the interval as regulatory complexity has expanded without commensurate enforcement or outcome improvement. Claimed type (tangled_rope): Justified by the presence of genuine coordination function (the regulation does create some predictable rules that enable housing markets to function, does prevent the worst abuse) alongside genuine asymmetric extraction (capital systematically extracts more value than it provides, and the regulatory framework enables rather than prevents this). The active enforcement requirement is met—municipal governments actively enforce property rights, eviction rules, and tax frameworks that benefit investors.
 *
 * PERSPECTIVAL GAP:
 *   This constraint reveals fundamental disagreement about what the regulation does. The institutional investor sees Rope—pure coordination mechanism enabling efficient capital deployment. The low-income renter sees Snare—pure extraction with no functional protection. The tenant coalition sees Tangled Rope—mixed structure with real protections embedded alongside real extraction. The alternative-model builders see Scaffold—temporary structure being replaced by superior alternatives. The zoning system itself is Piton—the procedural machinery persists through inertia despite failing to produce stated outcomes. The civilizational observer risks seeing Mountain—immutable tension between efficiency and provision—but the structural data reveals this as a false summit: alternative models (social housing, community control, cooperative ownership) are resolvable at the institutional level, not natural law constraints. The perspectival gap is not disagreement about measurements—all perspectives use the same extractiveness and suppression values—but disagreement about what these facts mean: is the regulation succeeding (investor view) or failing (renter view) at its stated purpose? The gap reveals that 'success' and 'failure' are indexed to different agents' actual outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's structural position in the extraction flow. Low-income renters (powerless/trapped) have maximum d ≈ 0.95, producing maximum experienced extraction via f(d) ≈ 1.42. They bear full cost and receive minimal benefit. Institutional investors (institutional/arbitrage) have minimum d ≈ 0.00-0.10, producing negative or minimal experienced extraction—they benefit from the framework and can exit. The tenant coalition (organized/constrained) has intermediate d ≈ 0.45-0.60 because they have some agency but face real barriers; they bear substantial cost but can sometimes extract concessions. Municipal governments occupy a complex position: they are nominally agents of housing provision but structurally benefit from development fees and tax base expansion, placing them closer to beneficiary status despite their governance role. This institutional split is captured by including them as beneficiaries while noting in directionality_logic that their position is constrained arbitrage rather than pure institutional benefit—they have less freedom to exit than private investors but more benefit than tenant advocates. The housing trust fund coalition has d ≈ 0.40-0.50—they bear some cost of building alternatives (slow capital accumulation, political opposition) but will benefit as alternative models mature. The zoning system is treated as institutional/arbitrage with minimal directionality complexity because it is not an agent but a structure; its classification is based on theater_ratio rather than extraction flow.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The classification resolves the contradiction between regulation's stated purpose (provide housing and protect tenants) and actual outcome (enable investor extraction while using coordination language to legitimize it). The regulation was designed to solve a coordination problem: how to ensure housing provision to low-income populations without relying purely on charity or government construction. The mechanism chosen was investor-friendly property rules with mandatory public benefits (affordable housing set-asides, rent control, tenant protections). This mechanism created unintended extraction pathways: (1) investor capital learns to work around affordability mandates through corporate structures, loan securitization, and development timing; (2) the regulatory framework legitimizes private ownership and market allocation, making it politically impossible to adopt non-market alternatives; (3) enforcement capacity lags violations, so regulations exist on paper but not in practice. The mandatrophy is resolved by recognizing that the regulation has shifted functions: it was designed for housing provision but has become an apparatus for legitimizing investor extraction. The six perspectives capture this: the perspectives that see Rope or Tangled Rope (investor, coalition, municipal government) are experiencing the regulation's legitimization function—it creates rules that make investment safe and profitable. The perspectives that see Snare or higher extraction (renters, field of housing provision) are experiencing the extraction function—the rules enable accumulation and displacement. The Scaffold perspective (alternative-model builders) is the diagnostic perspective: they see that the current regulation solves a real problem (coordinating housing provision) but is not the only solution; the problem is solvable through different institutions. If the Scaffold perspective is correct, then the regulation is not solving an immutable coordination problem—it is solving a contingent one, and the extraction is not necessary overhead but surplus value captured by investors. The theater_ratio rising from 0.42 to 0.65 over the interval indicates that the coordination function is real but declining: actual housing provision is slowing, actual tenant protection is declining, while the procedural apparatus (theater) is expanding. This is the signature of a constraint whose primary function has shifted from solving coordination problems to enabling extraction while maintaining the fiction that coordination is occurring.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_depth,
    'To what extent is housing regulation captured by investor interests versus genuinely protective?',
    'Analysis of regulatory outcomes: rent increase trajectories in regulated vs unregulated markets, displacement rates, enforcement action frequency, penalties relative to violations, and the composition and funding sources of regulatory agencies',
    'If captured: snare classification dominates, extraction coefficient rises. If genuinely protective: tangled rope classification holds, mixed coordination-extraction structure is real. If purely protective: rope classification for all perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture by real estate interests').

omega_variable(
    alternative_model_viability,
    'Can community-owned, municipal, and trust-based housing models actually scale to meet housing demand, or are they constrained by capital availability and political will?',
    'Longitudinal comparison of social housing scaling in jurisdictions with genuine commitment (Vienna, Singapore, parts of Scandinavia) versus those with symbolic commitment; analysis of capital financing models and political sustainability',
    'If viable at scale: scaffold perspective is structural (true sunset), and alternative models will reduce investor extraction over generational timescale. If not viable: scaffold is aspirational, and investor-driven extraction persists indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_viability, empirical, 'Scalability of community-based housing alternatives').

omega_variable(
    suppression_mechanism_source,
    'Is suppression of tenant organizing primarily structural (material barriers: atomization, legal liability, capital asymmetry) or internalized (cultural beliefs about property rights, individualism, privatization)?',
    'Comparative analysis of tenant organizing success rates across jurisdictions with different legal frameworks, property philosophies, and cultural norms; case studies of organizing outcomes post-legal reform',
    'If structural: suppression will decline as legal barriers are removed. If internalized: suppression persists even after legal barriers fall, constraining organizing capacity. If both: post-reform suppression trajectory reveals the proportion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_source, empirical, 'Source of tenant organizing suppression').

omega_variable(
    affordability_definition_circularity,
    'Does ''affordable housing'' regulation define affordability relative to prevailing market rents (which investors set), creating a circularity where investor extraction determines what counts as ''affordable''?',
    'Analysis of how affordability thresholds are set; comparison of affordability targets versus actual household wage distributions and housing cost burden outcomes; tracking of how affordability definitions shift as markets change',
    'If circular: regulation cannot constrain extraction because extraction itself determines the baseline for ''affordability''. The regulation becomes definitional cover for continued extraction. If non-circular: regulation has genuine constraint power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(affordability_definition_circularity, conceptual, 'Circularity in affordability definition relative to market-set rents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(real_estate_investment_regulation, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reir_tr_t0, real_estate_investment_regulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(reir_tr_t15, real_estate_investment_regulation, theater_ratio, 15, 0.52).
narrative_ontology:measurement(reir_tr_t30, real_estate_investment_regulation, theater_ratio, 30, 0.58).
narrative_ontology:measurement(reir_tr_t45, real_estate_investment_regulation, theater_ratio, 45, 0.65).

% Extraction over time
narrative_ontology:measurement(reir_be_t0, real_estate_investment_regulation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(reir_be_t15, real_estate_investment_regulation, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(reir_be_t30, real_estate_investment_regulation, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(reir_be_t45, real_estate_investment_regulation, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(real_estate_investment_regulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(real_estate_investment_regulation, 0.18).
narrative_ontology:affects_constraint(real_estate_investment_regulation, homelessness_prevention_systems).
narrative_ontology:affects_constraint(real_estate_investment_regulation, gentrification_displacement_cycles).
narrative_ontology:affects_constraint(real_estate_investment_regulation, municipal_fiscal_dependency).

% DUAL FORMULATION NOTE:
% Real estate investment regulation decomposes along observable lines. Measuring as 'housing provision' (ε ≈ 0.30, Rope) emphasizes coordination function. Measuring as 'tenant extraction' (ε ≈ 0.70, Snare) emphasizes asymmetric capital flows. These are not alternative frames of the same constraint—they are different constraints reflecting different causal chains. The housing provision story focuses on regulatory frameworks enabling diverse ownership and financing models. The tenant extraction story focuses on capital concentration and organizing suppression. They are linked by network dependency: housing provision mechanisms enable the extraction structures that extract from tenants.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(real_estate_investment_regulation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
