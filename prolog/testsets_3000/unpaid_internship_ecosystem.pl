% ============================================================================
% CONSTRAINT STORY: unpaid_internship_ecosystem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unpaid_internship_ecosystem, []).

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
 *   constraint_id: unpaid_internship_ecosystem
 *   human_readable: Unpaid Internship Ecosystem
 *   domain: labor/economic/educational
 *
 * SUMMARY:
 *   The unpaid internship ecosystem functions as a credential signaling and
 *   labor acquisition mechanism that disproportionately benefits hiring
 *   organizations and wealthy families while extracting value from aspiring
 *   workers with limited financial resources. The constraint exhibits
 *   structural characteristics of Tangled Rope: it contains a genuine
 *   coordination function (skill development, mentorship, professional
 *   network access) alongside asymmetric extraction (unpaid labor,
 *   opportunity cost concentration on low-income workers). The extractiveness
 *   score (0.58) reflects moderate-to-high value capture by organizations;
 *   the suppression score (0.72) reflects substantial barriers to exit
 *   (credential dependency, labor market structure, family financial
 *   constraints). The theater ratio (0.65) reflects that internship
 *   positioning emphasizes development and mentorship value while downplaying
 *   pure labor extraction. Over the 20-year interval, both extractiveness and
 *   theater ratio have increased, indicating accumulation of rent-seeking
 *   behavior on top of coordination function and increasing reliance on
 *   narrative framing ('experience') to justify unpaid work.
 *
 * KEY AGENTS:
 *   - Low-Income Interns: Primary victim (powerless/trapped) — structurally dependent on unpaid experience to enter labor market; cannot exit without sacrificing career entry
 *   - Middle-Class Interns: Secondary victim (moderate/constrained) — face high cost of exit (debt, delayed career) but have family financial support as fallback; experience genuine skill development alongside extraction
 *   - Hiring Organizations: Primary beneficiary (institutional/arbitrage) — capture unpaid labor value, filter for committed workers, maintain cost advantage; can switch to paid labor if supply dries up
 *   - Wealthy Families: Primary beneficiary (powerful/arbitrage) — subsidize children's unpaid work; enable access to elite networks and prestigious positions
 *   - Labor Rights Coalition: Organized agent (organized/constrained) — arXiv equivalent organizations pushing regulation toward sunset; have agency and see regulatory pathways
 *   - University Career Services: Institutional actor (institutional/arbitrage) — maintain performative internship ecosystem through inertia; generate placement metrics while advancing equity rhetoric
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing recent contingent institutional arrangement as inevitable feature of labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unpaid_internship_ecosystem, 0.58).
domain_priors:suppression_score(unpaid_internship_ecosystem, 0.72).
domain_priors:theater_ratio(unpaid_internship_ecosystem, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unpaid_internship_ecosystem, extractiveness, 0.58).
narrative_ontology:constraint_metric(unpaid_internship_ecosystem, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unpaid_internship_ecosystem, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unpaid_internship_ecosystem, tangled_rope).
narrative_ontology:human_readable(unpaid_internship_ecosystem, "Unpaid Internship Ecosystem").
narrative_ontology:topic_domain(unpaid_internship_ecosystem, "labor/economic/educational").

domain_priors:requires_active_enforcement(unpaid_internship_ecosystem).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unpaid_internship_ecosystem, hiring_organizations).
narrative_ontology:constraint_beneficiary(unpaid_internship_ecosystem, wealthy_families).
narrative_ontology:constraint_victim(unpaid_internship_ecosystem, interns_from_low_income_backgrounds).
narrative_ontology:constraint_victim(unpaid_internship_ecosystem, labor_market_pricing).
narrative_ontology:constraint_victim(unpaid_internship_ecosystem, aspiring_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME INTERN (SNARE) — Structurally trapped by need for experience credentials, lack of financial cushion to survive unpaid work, and dependency on family income. Cannot exit without sacrificing career entry. Experiences maximum extraction: labor value extracted with zero compensation while bearing full opportunity cost.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-CLASS INTERN (TANGLED ROPE) — Constrained but not trapped: family financial support creates exit option at high cost (delaying career, accumulating debt, lifestyle compression). Benefits from genuine skill development and networking access while bearing extraction of labor value. Mixed experience: significant coercion but some coordination value.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIRING ORGANIZATION (ROPE) — Experiences the constraint as beneficial coordination: access to motivated labor at zero compensation, filter for identifying committed workers, cost reduction mechanism. Extraction runs toward this agent. Arbitrage option available: can hire paid employees if unpaid interns become unavailable; maintains flexibility.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WEALTHY FAMILY (ROPE) — Net beneficiary through child's ability to access prestigious unpaid positions; subsidizes child's labor contribution through living expenses. Experiences the constraint as enabling access to elite networks and experience credentials. No exit pressure; can maintain status quo indefinitely.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR RIGHTS COALITION (SCAFFOLD) — Organized agents (unions, advocacy groups, state labor boards) perceive unpaid internships as a temporary institutional anomaly with a sunset trajectory. Legal pathways (Fair Labor Standards Act enforcement, minimum wage extension, internship wage requirements) are building toward elimination. See the constraint as a coordination failure being corrected through regulation. Theater ratio is high (internship rhetoric about 'experience' and 'mentorship' masks pure extraction), but the coalition has agency and sees an exit mechanism.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: UNIVERSITY CAREER SERVICES (PITON) — Institutional maintenance of internship program framework that has lost functional justification. Universities simultaneously promote unpaid internships (career development theater) while declaring commitment to equity. The institutional ritual persists through inertia: internship placement is a measurable metric, funding often comes from employer partnerships, and alternatives (paid apprenticeships, cooperative education) require structural change. Theater ratio high; functional value degraded.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: NATURAL LAW FRAMING (MOUNTAIN) — A false summit perspective that naturalizes unpaid internships as inevitable: 'Entry-level workers have always had to prove themselves,' 'Experience has always been unpaid,' 'This is how professional socialization works.' From a civilizational/universal lens, this framing treats a contingent institutional arrangement as an immutable feature of labor markets. The analytical engine will flag this as a false summit, revealing that the 'naturalness' is performative — historical examination shows unpaid internships are a recent, concentrated phenomenon driven by specific policy choices and labor market dynamics.
constraint_indexing:constraint_classification(unpaid_internship_ecosystem, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unpaid_internship_ecosystem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unpaid_internship_ecosystem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unpaid_internship_ecosystem, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unpaid_internship_ecosystem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unpaid_internship_ecosystem, TR),
    TR >= 0.70.

:- end_tests(unpaid_internship_ecosystem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The hiring organization captures full labor value with zero monetary compensation. This is high but not maximum (0.66+) because: (1) genuine skill development and mentorship occur, reducing pure exploitation profile, and (2) some credentialing value flows to interns, creating partial benefit alignment. The trajectory from 0.35 to 0.58 over 20 years reflects increasing formalization of internships as labor strategies rather than developmental programs — organizations have optimized the extraction function. Suppression (0.72): High barriers to exit include: credential dependency (internship experience required for entry-level jobs), financial barriers (inability to live without compensation), labor market concentration (internship supply concentrated in prestigious organizations), and opportunity cost (competing life activities — other jobs, education, family obligations). Not maximal (0.85+) because some exit paths exist (paid positions, alternative credentials, family support). Theater ratio (0.65): Internship framing emphasizes development ('hands-on experience,' 'mentorship,' 'professional network') and downplays extraction ('supporting the next generation'). This narrative maintenance is necessary because pure extraction is not socially legitimate — the theater sustains organizational consent. Rising to 0.65 reflects increasing gap between rhetoric and function as organizations have systematized unpaid internship programs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a 6-type perspectival gap. Low-income interns see a snare: they are trapped in the constraint with no exit and no benefit compensation. Middle-class interns see tangled rope: genuine skill/network benefit exists alongside substantial extraction. Hiring organizations see rope: they experience pure coordination value (access to motivated labor, skill development of workforce). Wealthy families see rope: they benefit from children's network access and credential acquisition. Labor rights coalition sees scaffold: regulations (wage extension, apprenticeship requirements) are building an exit pathway with a sunset. University career services see piton: the institutional ritual of promoting internships has become performative while universities simultaneously advance equity goals. The analytical observer risks seeing mountain: naturalizing unpaid internships as inevitable feature of labor markets. The perspectival gap reveals that the same institutional arrangement is experienced as entrapment, extraction, coordination, beneficiary advantage, regulable problem, degraded ritual, and natural law — depending on structural position. No single classification captures all positions; the presheaf of perspectives IS the analytical output.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by structural position. Low-income interns (powerless/trapped) experience maximum extraction: d ≈ 0.95, f(d) ≈ 1.42, high experienced extractiveness. They bear full cost; organization captures full benefit. Middle-class interns (moderate/constrained) experience moderate extraction: d ≈ 0.65, f(d) ≈ 1.00, moderate experienced extractiveness. Hiring organizations (institutional/arbitrage) experience extraction running TOWARD them: d ≈ 0.05, f(d) ≈ -0.12, negative effective extraction (they are beneficiaries). Wealthy families (powerful/arbitrage) similarly experience negative extraction: d ≈ 0.10, f(d) ≈ 0.02, near-zero extraction. The pipeline computes d from beneficiary/victim status plus exit options: beneficiaries with exit options get low d; victims with no exit get high d. The gap between low-income and middle-class intern experiences reflects the differential cost of exit, not difference in labor extraction — both contribute unpaid work, but the financial impact stratifies their structural relationship to the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The unpaid internship ecosystem demonstrates how Tangled Rope resolves the mandatrophy (coordination ≠ extraction) by unifying multiple perspectives under a single type. The presence of genuine coordination (skill development, mentorship, professional network access) alongside asymmetric extraction (unpaid labor, opportunity cost concentration) is the defining structural signature of Tangled Rope. The mandatory condition χ ≥ 0.40 ∧ ≤ 0.90 is satisfied at χ ≈ 0.58. The presence of beneficiaries (hiring organizations, wealthy families) establishes coordination function. The presence of victims (low-income interns, aspiring workers) establishes asymmetric extraction. The requirement for active enforcement is satisfied: the ecosystem requires continuous organizational enforcement (classification as 'intern' vs 'employee,' wage-law avoidance, cultural narrative maintenance of development rhetoric) and cultural enforcement (prestige signaling, network gatekeeping). Without active enforcement, interns would demand wages or alternative pathways would emerge. The falsehood would be classifying this as pure Rope (pure coordination) or pure Snare (pure extraction). It is genuinely both — the mandatrophy is resolved by recognizing the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intra_class_heterogeneity,
    'Does unpaid internship access vary significantly within the ''middle-class'' category based on ethnic, gender, or geographic factors beyond raw household income?',
    'Demographic analysis of unpaid intern cohorts; controlling for household income, track access disparity by race, gender, geography, family occupation',
    'If significant disparity exists: the ''constrained'' exit for middle-class interns is actually stratified — some have functional arbitrage options while others face near-trapped conditions. The Tangled Rope classification for moderate agents would be too generous; many would map to trapped. Recompute perspectival classification with finer-grained stratification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intra_class_heterogeneity, empirical, 'Intra-class variation in unpaid internship access beyond income').

omega_variable(
    selection_effect_vs_extraction,
    'How much of the career advantage from unpaid internships reflects genuine skill development and networking value versus pure selection effect (already-privileged candidates sorting into high-status internships)?',
    'Longitudinal tracking of intern cohorts with matched controls; separate wage and career outcomes by (1) unpaid internship completers, (2) paid internship completers, (3) non-internship entrants, controlling for entry credentials and family background',
    'If selection effect dominates (80%+ of advantage): unpaid internships are primarily a filtering/sorting mechanism masquerading as development, increasing snare classification confidence. If genuine skill transfer significant (30%+ of advantage): tangled rope classification is correct — real coordination value exists alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selection_effect_vs_extraction, empirical, 'Selection effect versus genuine skill development in internships').

omega_variable(
    legal_enforcement_feasibility,
    'Can Fair Labor Standards Act minimum wage requirements be practically enforced against unpaid internship structures, or do organizational workarounds (classification as ''volunteer,'' ''participant,'' ''student researcher'') successfully evade wage law?',
    'Audit of state labor board enforcement: complaint rates, settlement outcomes, organizational compliance post-violation; tracking of classification strategy shifts following wage litigation',
    'If enforcement feasible: scaffold sunset trajectory is real; wage extension is a viable regulatory path. If workarounds are effective: sunset is aspirational rather than structural; the constraint will persist through reclassification rather than elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_enforcement_feasibility, empirical, 'Enforceability of wage law against unpaid internship structures').

omega_variable(
    alternative_credential_signaling,
    'Do alternative pathways to labor market entry (apprenticeships, bootcamps, direct hire-and-train, portfolio-based hiring) provide equivalent signaling value compared to prestigious internships?',
    'Comparative analysis of job placement rates, wage outcomes, and employer perception across credential pathways; employer hiring decision process analysis',
    'If alternatives are equivalent: unpaid internship necessity is artificial; the ecosystem persists due to organizational convenience rather than functional necessity. Snare classification strengthened. If alternatives are significantly weaker: unpaid internships may provide genuine valuable signal; tangled rope classification more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_credential_signaling, empirical, 'Equivalence of alternative credential pathways').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unpaid_internship_ecosystem, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unpaid_intern_tr_t0, unpaid_internship_ecosystem, theater_ratio, 0, 0.45).
narrative_ontology:measurement(unpaid_intern_tr_t10, unpaid_internship_ecosystem, theater_ratio, 10, 0.58).
narrative_ontology:measurement(unpaid_intern_tr_t20, unpaid_internship_ecosystem, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(unpaid_intern_be_t0, unpaid_internship_ecosystem, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(unpaid_intern_be_t10, unpaid_internship_ecosystem, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(unpaid_intern_be_t20, unpaid_internship_ecosystem, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unpaid_internship_ecosystem, resource_allocation).
narrative_ontology:affects_constraint(unpaid_internship_ecosystem, labor_market_credentialism).
narrative_ontology:affects_constraint(unpaid_internship_ecosystem, wealth_based_network_access).
narrative_ontology:affects_constraint(unpaid_internship_ecosystem, class_reproduction_in_professional_entry).

% DUAL FORMULATION NOTE:
% Unpaid internship ecosystem is downstream of class stratification in educational access and upstream of labor market outcome inequality. Separate constraint stories exist for (1) educational credentialing gatekeeping (high school → college → internship pipeline), (2) professional network asymmetry (wealthy families' access to industry contacts), and (3) early-career wage compression (internship → entry-level job wage penalty). Each has distinct ε values and perspectives; the family is linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unpaid_internship_ecosystem, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
