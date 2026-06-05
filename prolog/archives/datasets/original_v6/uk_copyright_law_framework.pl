% ============================================================================
% CONSTRAINT STORY: uk_copyright_law_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uk_copyright_law_framework, []).

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
 *   constraint_id: uk_copyright_law_framework
 *   human_readable: UK Copyright Law Framework
 *   domain: intellectual_property/cultural_production
 *
 * SUMMARY:
 *   The UK copyright law framework exhibits the structural characteristics of
 *   a tangled rope—a coordination mechanism with genuine function
 *   (incentivizing creative production, protecting creator investment) that
 *   has accumulated extractive overhead through term extensions, scope
 *   expansion, and enforcement intensification. The framework emerged as a
 *   coordination solution in the print era (protecting the capital-intensive
 *   book industry) but has been stretched across digital media, where
 *   distribution costs are negligible and the original justification weakens.
 *   The constraint's theater ratio (0.65) reflects that modern copyright
 *   enforcement increasingly performs symbolic enforcement (copyright
 *   notices, licensing language, DRM access controls) rather than preventing
 *   actual infringement at scale. The extractiveness value (0.52) captures
 *   the asymmetry between beneficiaries (incumbent copyright holders, major
 *   publishers) who capture monopoly rents and victims (derivative creators,
 *   educational institutions, the knowledge commons) who bear access
 *   restrictions and licensing costs. The framework is enforced through legal
 *   machinery (courts, statutory damages), contractual licensing (collecting
 *   societies), and technological DRM, creating multiple suppression
 *   pathways.
 *
 * KEY AGENTS:
 *   - Derivative Creators: Trapped victims (powerless/trapped) — face legal paralysis through transformative use uncertainty; bears litigation risk and creative suppression
 *   - Major Publishers: Beneficiaries (institutional/arbitrage) — capture licensing rents, pricing power, and international arbitrage opportunities; can negotiate exemptions and alternatives
 *   - Educational Institutions: Constrained victims (moderate/constrained) — benefit from copyright-incentivized textbooks but face licensing costs; high-cost but possible to circumvent through open-access alternatives
 *   - Knowledge Commons: Trapped victim (powerless/trapped, generational) — abstract collective harmed by term extensions locking works into private monopoly for centuries
 *   - Open Culture Coalition: Organized alternatives (organized/constrained) — Creative Commons, open repositories, remix communities building parallel systems with lower extraction
 *   - Copyright Enforcement Machinery: Institutional enforcer (institutional/arbitrage) — courts, collecting societies, DRM systems that maintain suppression infrastructure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uk_copyright_law_framework, 0.52).
domain_priors:suppression_score(uk_copyright_law_framework, 0.48).
domain_priors:theater_ratio(uk_copyright_law_framework, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uk_copyright_law_framework, extractiveness, 0.52).
narrative_ontology:constraint_metric(uk_copyright_law_framework, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(uk_copyright_law_framework, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uk_copyright_law_framework, tangled_rope).
narrative_ontology:human_readable(uk_copyright_law_framework, "UK Copyright Law Framework").
narrative_ontology:topic_domain(uk_copyright_law_framework, "intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(uk_copyright_law_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uk_copyright_law_framework, incumbent_copyright_holders).
narrative_ontology:constraint_beneficiary(uk_copyright_law_framework, major_publishers).
narrative_ontology:constraint_beneficiary(uk_copyright_law_framework, content_monopolies).
narrative_ontology:constraint_victim(uk_copyright_law_framework, derivative_creators).
narrative_ontology:constraint_victim(uk_copyright_law_framework, knowledge_commons).
narrative_ontology:constraint_victim(uk_copyright_law_framework, educational_institutions).
narrative_ontology:constraint_victim(uk_copyright_law_framework, consumer_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DERIVATIVE CREATORS (SNARE) — Fan artists, remix creators, transformative works producers face legal paralysis. Copyright law traps them in perpetual uncertainty: transformative use doctrine exists but is costly to defend. No exit without legal risk or abandoning creative practice. Maximum suppression through threat of litigation.
constraint_indexing:constraint_classification(uk_copyright_law_framework, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: KNOWLEDGE COMMONS (SNARE, GENERATIONAL) — Copyright term extensions (currently life+70 years in UK) lock creative works out of public domain for generations. Trapped in extended monopoly with no exit mechanism. Works that should be freely available remain under extraction for centuries. Pure extraction from collective cultural heritage.
constraint_indexing:constraint_classification(uk_copyright_law_framework, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTIONS (TANGLED ROPE) — Schools and universities benefit from copyright's incentive to create educational materials, but face high licensing costs and fair dealing restrictions. Genuine coordination function (incentivizes textbook creation) exists alongside asymmetric extraction (licensing fees, access restrictions). Constrained by budget barriers; high cost to circumvent but possible.
constraint_indexing:constraint_classification(uk_copyright_law_framework, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR PUBLISHERS (ROPE) — Experience copyright as coordination mechanism: protects their investment, incentivizes content production, enables licensing arbitrage across markets. Low extraction relative to their power because they benefit from the framework and can negotiate exemptions. Exit options abundant (licensing, adaptation, international arbitrage).
constraint_indexing:constraint_classification(uk_copyright_law_framework, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL-AGE CREATOR (PITON) — Copyright law originally incentivized book printing and physical distribution. In the digital era, incentives persist performatively while original function (protecting capital investment in physical reproduction) has atrophied. Creators now primarily monetize through direct audience relationships, Patreon, subscriptions—copyright's protection is theatrical rather than functional for many. Theater ratio driven by outdated statutory frameworks persisting through institutional inertia.
constraint_indexing:constraint_classification(uk_copyright_law_framework, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN CULTURE COALITION (TANGLED ROPE) — Creative Commons, open-source advocates, and remix culture communities see copyright as partially constraining genuine coordination (attribution, provenance tracking) while enabling strategic extraction (term monopolies). Organized enough to build alternatives (CC licenses, open repositories) but constrained by legal precedent and institutional lock-in. Moderate active enforcement required; building parallel systems with lower extraction.
constraint_indexing:constraint_classification(uk_copyright_law_framework, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some copyright protection might appear immutable: property rights in ideas are sometimes theorized as natural law extensions of labor (Lockean framework). However, structural data contradicts mountain classification—copyright duration, scope, and enforcement mechanisms are entirely statutory and culturally contingent. This perspective exemplifies false naturalization of a designed institutional arrangement.
constraint_indexing:constraint_classification(uk_copyright_law_framework, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uk_copyright_law_framework_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uk_copyright_law_framework, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uk_copyright_law_framework, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uk_copyright_law_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uk_copyright_law_framework, TR),
    TR >= 0.70.

:- end_tests(uk_copyright_law_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through multiple mechanisms: (1) licensing fees charged by copyright holders to derivative creators and educational institutions; (2) monopoly pricing by publishers unchallenged by public domain alternatives (terms are life+70 years, far longer than incentive justification requires); (3) exclusion of transformative works from remix culture. However, extraction is not maximal (0.66+ for snares) because the framework does coordinate genuine incentive creation—books are produced, music is funded, creators do invest in production because copyright protection exists. This is the defining feature of tangled rope: both coordination and extraction coexist in the same structure. Suppression (0.48): Moderate. Legal barriers to derivative creation are real (fair dealing doctrine is narrow, licensing is expensive) but not absolute—gray market, international arbitrage, and technological circumvention exist. Fair dealing exceptions provide partial relief. Educational exceptions exist (though narrow). Theater ratio (0.65): Moderately high and increasing. The ratio has climbed from 0.42 (when copyright primarily protected physical printing infrastructure) to 0.65 (in the digital era where enforcement is largely theatrical—copying costs near zero, but legal enforcement persists). Modern copyright enforcement is increasingly symbolic: copyright notices on websites, Digital Rights Management signals, licensing language in terms-of-service—these perform enforcement more than they prevent actual unauthorized copying at scale. The theater increase reflects that the original rationale (protecting capital-intensive physical reproduction) has atrophied while the enforcement machinery persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same legal framework produces radically different experienced extractiveness depending on the agent's structural position. The major publisher (institutional/arbitrage) experiences the framework as low-extraction coordination—copyright protects their investment and enables profitable licensing across markets. They can exit the framework by licensing internationally or adapting works. The derivative creator (powerless/trapped) experiences the same framework as maximal extraction—they cannot create transformative works without legal risk. They cannot exit without abandoning their creative practice. The knowledge commons (powerless/trapped at generational timescale) experiences extraction stretching across centuries as copyright terms lock creative works from public domain. The educational institution (moderate/constrained) experiences mixed coordination and extraction—the framework incentivizes textbook creation (coordination), but licensing costs extract from educational budgets. The open culture coalition (organized/constrained) is building parallel systems (Creative Commons, open repositories) with lower extraction, suggesting the constraint might sunset as alternatives mature. The piton perspective reveals that the framework's original function (protecting print-era capital investment) has atrophied, but enforcement persists through institutional inertia. The false-summit mountain perspective risks naturalizing copyright as inherent to creativity, when it is actually a designed institutional arrangement from the Statute of Anne (1710).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural relationship to the extraction flow. Beneficiaries (major publishers) have low directionality (d ≈ 0.15, arbitrage exit)—extraction flows toward them, so f(d) produces negative/low chi. Victims with no exit (derivative creators, knowledge commons) have high directionality (d ≈ 0.90-0.95, trapped exit)—extraction flows from them, so f(d) produces high chi. Constrained victims (educational institutions) have moderate directionality (d ≈ 0.65, constrained exit)—high costs to exit but not impossible, so f(d) produces moderate chi. Organized alternatives (open culture coalition) have lower directionality (d ≈ 0.50, constrained exit with coalition capacity)—they are building exits, so chi is lower than isolated constrained agents. The analytical observer at civilizational scope (analytical/analytical) has high directionality (d ≈ 0.72)—the observer's position is distal from the constraint's enforcement machinery, so experienced extraction is higher than it appears to embedded agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that copyright law is a genuine tangled rope, not a misclassified snare or rope. The coordination function is real: copyright does incentivize creative production (the empirical effect of copyright on publishing output is positive, though small). The extraction is equally real: copyright enables monopoly pricing, blocks derivative works, and extends terms far beyond what incentive justification requires (term life+70 years is unjustifiable on incentive grounds; terms of 14-28 years produce equivalent incentive). The ambiguity is not 'which type is it?' but 'at what ratio does coordination balance extraction?' The 0.52 extractiveness and required_active_enforcement=true confirm tangled rope: high enough extraction to require active legal enforcement, but genuine coordination exists alongside. The piton perspective (originalist function atrophied, enforcement theatrical) and the open culture coalition perspective (building scaffold alternatives) together suggest the constraint is moving toward lower coordination-to-extraction ratio over time—measurement drift from 0.38→0.52 extractiveness reflects accumulating extraction (term extensions, scope expansion) with constant or declining coordination value. The false-summit mountain perspective is flagged as naturalization: copyright is statutory, culturally contingent, and reformed in multiple jurisdictions—not a law of nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fair_dealing_boundary_ambiguity,
    'Where is the boundary between fair dealing (permitted use) and copyright infringement in digital transformation contexts?',
    'Case law evolution and statutory clarification; empirical analysis of litigation costs for small-scale transformative uses',
    'If boundary clarifies toward broader fair dealing: effective suppression drops (victim exit options improve from trapped to constrained). If boundary narrows: snare classification strengthens for derivative creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_dealing_boundary_ambiguity, empirical, 'Boundary definition for fair dealing vs infringement in digital contexts').

omega_variable(
    term_extension_justification,
    'Does copyright term extension (life+70 years) serve any incentive function, or is it pure extraction from the knowledge commons?',
    'Empirical study of creator behavior relative to term length; comparison with alternative incentive models (direct grants, prizes, contracts); measurement of knowledge commons economic value forgone',
    'If extensions serve no incentive function: victims reclassify from constrained to trapped (higher extraction). If marginal incentive exists: remains moderate extraction (tangled rope holds). Affects generational snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(term_extension_justification, empirical, 'Whether copyright term extensions provide genuine incentive function').

omega_variable(
    technological_enforcement_versus_legal_framework,
    'Is suppression primarily driven by legal copyright restrictions or by technological DRM (Digital Rights Management) enforcement?',
    'Correlation analysis of legal enforcement actions vs DRM-enabled access control; comparison of suppression in jurisdictions with weak copyright enforcement vs strong DRM norms',
    'If DRM dominates: suppression is largely internalized by technology, not law—affects whether constraint is truly ''legal framework'' or hybrid technical-legal. If legal framework dominates: suppression is institutional and more contestable through reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_enforcement_versus_legal_framework, empirical, 'Whether suppression is legal or technological in origin').

omega_variable(
    open_culture_exit_pathway_viability,
    'Can open-culture alternatives (Creative Commons, open-source communities, public domain strategies) become functionally equivalent to copyright-based incentive systems?',
    'Longitudinal comparison of creator earnings and output volume under CC licensing vs traditional copyright; measurement of ecosystem robustness in open-culture domains; network analysis of knowledge commons growth',
    'If viable: scaffold perspective gains credibility—copyright framework is temporary coordination failure being solved. If not viable: organized coalition remains constrained rather than building exit pathway. Affects long-term classification trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_culture_exit_pathway_viability, empirical, 'Viability of open-culture alternatives as functional substitutes').

omega_variable(
    international_harmonization_lock_in,
    'Does UK alignment with international copyright treaties (TRIPS, WIPO, Berne Convention) create irreversible lock-in that prevents UK policy reform?',
    'Legal analysis of treaty exit costs and withdrawal mechanisms; historical comparison with other jurisdictions attempting copyright reform; measurement of trade pressure against reform advocates',
    'If lock-in is real: suppression includes international treaty enforcement—victims'' exit options contract from constrained to trapped. If lock-in is soft: reform pathway exists—opens scaffold or organizational coalition perspective. Affects whether constraint is national or transnational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_harmonization_lock_in, empirical, 'Whether international treaties prevent UK copyright reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uk_copyright_law_framework, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uk_c_tr_t0, uk_copyright_law_framework, theater_ratio, 0, 0.42).
narrative_ontology:measurement(uk_c_tr_t10, uk_copyright_law_framework, theater_ratio, 10, 0.52).
narrative_ontology:measurement(uk_c_tr_t20, uk_copyright_law_framework, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(uk_c_be_t0, uk_copyright_law_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uk_c_be_t10, uk_copyright_law_framework, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(uk_c_be_t20, uk_copyright_law_framework, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uk_copyright_law_framework, information_standard).
narrative_ontology:affects_constraint(uk_copyright_law_framework, digital_rights_management_lock).
narrative_ontology:affects_constraint(uk_copyright_law_framework, knowledge_commons_enclosure).
narrative_ontology:affects_constraint(uk_copyright_law_framework, creative_industry_rent_seeking).

% DUAL FORMULATION NOTE:
% UK copyright law is upstream of specific DRM enforcement regimes and knowledge commons enclosure. The framework enables downstream extraction mechanisms through legal infrastructure; changes to copyright framework directly affect feasibility of downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uk_copyright_law_framework, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
