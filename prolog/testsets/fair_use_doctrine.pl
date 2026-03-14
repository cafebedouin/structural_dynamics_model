% ============================================================================
% CONSTRAINT STORY: fair_use_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_doctrine, []).

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
 *   constraint_id: fair_use_doctrine
 *   human_readable: Fair Use Doctrine in Copyright Law
 *   domain: intellectual_property/legal
 *
 * SUMMARY:
 *   The fair use doctrine in copyright law represents a foundational attempt
 *   to balance exclusive author rights against public access and creative
 *   transformation. Codified in Section 107 of the U.S. Copyright Act, fair
 *   use permits limited use of copyrighted material without permission or
 *   payment for purposes including criticism, comment, news reporting,
 *   teaching, scholarship, and parody. However, the doctrine exhibits
 *   structural extraction alongside genuine coordination functions. The
 *   four-factor test (purpose/nature of use, amount used, effect on market,
 *   transformativeness) is vague by design, creating a predictability gap
 *   that suppresses legitimate claims through litigation risk and licensing
 *   threats. Large copyright holders maintain institutional power through
 *   licensing monopolies and DMCA anti-circumvention provisions that
 *   functionally override fair use. Marginal users and transformative
 *   creators face asymmetric enforcement: cease-and-desist letters, takedown
 *   demands, and the chilling effect of potential litigation. The doctrine
 *   persists as a legal form despite degradation of its protective function —
 *   maintained through institutional inertia rather than actual capacity to
 *   protect legitimate uses. Theater has increased over three decades (from
 *   0.45 to 0.68) as digital technologies, platform dynamics, and licensing
 *   complexity have outpaced the doctrine's capacity to provide clear
 *   guidance.
 *
 * KEY AGENTS:
 *   - Original Copyright Holders: Institutional beneficiaries (institutional/arbitrage) — control licensing markets and enforce boundaries; benefit from doctrine's framing as protection against 'theft' while using licensing and DMCA to suppress fair use claims
 *   - Marginal Use Claimants: Primary victims (powerless/trapped) — cannot afford litigation to assert fair use defenses; trapped by cease-and-desist letters and licensing demands despite meritorious claims
 *   - Educational Institutions: Organized agents (organized/constrained) — benefit from educational fair use exception but face licensing costs and compliance audits; constrained by enforcement threats
 *   - Transformative Creators: Secondary victims (moderate/constrained) — benefit from transformative use doctrine in theory but face litigation risk and licensing suppression in practice; deterred by unpredictability of four-factor test
 *   - Cultural Commons: Diffuse victims (powerless/trapped) — abstract collective good (archival access, cultural preservation, remix traditions) that cannot organize; bears full cost of suppressed fair use
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent doctrine design as inherent tension in copyright
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_doctrine, 0.58).
domain_priors:suppression_score(fair_use_doctrine, 0.62).
domain_priors:theater_ratio(fair_use_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_doctrine, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_doctrine, tangled_rope).
narrative_ontology:human_readable(fair_use_doctrine, "Fair Use Doctrine in Copyright Law").
narrative_ontology:topic_domain(fair_use_doctrine, "intellectual_property/legal").

domain_priors:requires_active_enforcement(fair_use_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_doctrine, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_doctrine, cultural_commons).
narrative_ontology:constraint_beneficiary(fair_use_doctrine, transformative_creators).
narrative_ontology:constraint_victim(fair_use_doctrine, original_copyright_holders).
narrative_ontology:constraint_victim(fair_use_doctrine, marginal_use_claimants).
narrative_ontology:constraint_victim(fair_use_doctrine, independent_creators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL USE CLAIMANT (SNARE) — A small creator facing infringement claims has no realistic exit: litigation costs exceed available resources; fair use defense requires judicial adjudication they cannot afford; cease-and-desist letters force settlement despite meritorious fair use claims. Trapped by asymmetric legal burden and resource barriers.
constraint_indexing:constraint_classification(fair_use_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGINAL COPYRIGHT HOLDER / ENFORCEMENT PERSPECTIVE (SNARE) — Large copyright holders face genuine enforcement costs and erosion of control through fair use exceptions. However, suppression is high — DMCA anti-circumvention provisions, takedown demand scaling, and licensing monopolies constrain fair use claimants' ability to assert legitimate defenses. Extraction is bidirectional but asymmetric: the doctrine itself suppresses assertion of competing rights.
constraint_indexing:constraint_classification(fair_use_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTION (TANGLED ROPE) — Schools and universities benefit from fair use for teaching, research, and archival functions (coordination benefit); simultaneously face licensing costs and litigation threats for uses on the boundary (extraction). Active enforcement of licensing compliance audits and takedown requests; genuine coordination through educational exception. Mixed experience: some genuine coordination function alongside asymmetric extraction.
constraint_indexing:constraint_classification(fair_use_doctrine, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR COPYRIGHT HOLDER / LICENSING BENEFICIARY (ROPE) — Large studios and publishers experience fair use doctrine primarily as a coordination mechanism: it clarifies the boundaries of permissible use, stabilizing licensing markets and reducing unpredictable litigation. Fair use licenses out uses that are not profitable anyway (criticism, parody, educational use). Net beneficiary — the doctrine serves their interests.
constraint_indexing:constraint_classification(fair_use_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSFORMATIVE CREATOR (TANGLED ROPE) — Artists, remixers, and transformative users benefit from fair use doctrine's protection (coordination of creative freedom) while simultaneously facing deterrent effects from litigation risk and suppression through licensing threats. Theater is high — courts apply a vague four-factor test with unpredictable outcomes. Significant extraction through licensing demands and litigation costs despite meritorious claims.
constraint_indexing:constraint_classification(fair_use_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIR USE DOCTRINE AS INSTITUTIONAL FORM (PITON) — The four-factor fair use test (purpose, nature, amount, market effect) is maintained as a legal standard despite substantial degradation of its protective function. Theater is high — courts and lawyers perform fair use analysis that rarely delivers predictable protection. The doctrine persists through institutional inertia (it's embedded in statute and case law) despite losing its capacity to actually protect legitimate uses. Licensing demands bypass fair use entirely; DMCA circumvention bans suppress fair use claims; litigation costs deter assertion.
constraint_indexing:constraint_classification(fair_use_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some tension between author control and public use is inherent to copyright: any system balancing incentives for creation against access to culture must have permeable boundaries. This perspective risks naturalizing the fair use doctrine's vagueness and enforcement asymmetries as inevitable features of intellectual property law. However, the structural data contradicts this — the suppression (0.62), theater (0.68), and asymmetric enforcement reveal contingent institutional choices, not natural law.
constraint_indexing:constraint_classification(fair_use_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_doctrine, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_doctrine, TR),
    TR >= 0.70.

:- end_tests(fair_use_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The doctrine creates a licensing monopoly for major copyright holders while suppressing assertion of legitimate fair use claims through litigation cost barriers and licensing demands. The extraction is not total (educational institutions and some transformative uses are protected) but substantial. Measurement trajectory (0.38→0.48→0.58) reflects increasing extraction over 30 years as digital technologies, platform economics, and licensing complexity have expanded opportunities for licensing enforcement and reduced marginal users' ability to assert fair use. Suppression (0.62): High. Multiple suppression mechanisms: (1) litigation cost barriers deter assertion; (2) DMCA anti-circumvention provisions suppress technical access regardless of fair use; (3) licensing demands and takedown requests force settlement; (4) four-factor test unpredictability creates chilling effect; (5) institutional licensing monopolies create false choice between licensing and infringement. Theater (0.68): High-moderate. The four-factor test is substantially performative — it provides a vocabulary for discussing fair use but has degraded predictive power as courts apply inconsistent weightings. Lawyers and judges perform fair use analysis that rarely delivers actual protection despite doctrinal coverage. Theater has increased as doctrine has remained static while digital platforms, content licensing, and enforcement mechanisms have evolved.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence across power levels and exit options. Major copyright holders (institutional/arbitrage) see fair use as rope — a coordination mechanism that clarifies boundaries and protects their licensing markets. Educational institutions (organized/constrained) see tangled rope — genuine coordination through educational exception mixed with licensing extraction through compliance audits. Transformative creators (moderate/constrained) see tangled rope — protection for transformative use in principle, suppression through litigation risk in practice. Marginal claimants (powerless/trapped) see snare — no realistic exit from infringement claims despite potentially meritorious fair use defense. The copyright holder enforcement perspective (powerless/trapped in resource burden) also sees snare — genuine cost of monitoring and enforcement. The institutional doctrine itself (institutional/arbitrage) appears as piton — maintained through inertia, performative in function. The analytical observer risks false summit of mountain — naturalizing the tension between author control and public access as inherent rather than as a contingent institutional choice about how to balance competing interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect structural position in the licensing extraction flow. Copyright holders with licensing arbitrage options (d≈0.15) experience low or negative effective extraction — the doctrine serves their interests. Marginal claimants with trapped exit (d≈0.92) bear maximum extraction through litigation risk and licensing suppression. Educational institutions (d≈0.55) occupy intermediate position: benefit from educational exception (low d component) but face licensing demands (high d component). Transformative creators (d≈0.65) face high extraction through litigation deterrence despite theoretical protection. The doctrine's suppression mechanism operates across all d values — it suppresses assertion of competing claims through institutional and technical barriers, not through explicit prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   Fair use doctrine resolves mandatrophy by demonstrating how a single legal form (the four-factor test) serves fundamentally different structural functions for different agents: genuine coordination for large copyright holders and educational institutions (rope from their perspective), pure extraction for marginal claimants (snare), and mixed extraction-coordination for transformative creators (tangled rope). The doctrine's design — intentionally vague four-factor test with judicial discretion — enables this polyvalence. The vagueness is not a bug but a feature for institutions maintaining licensing power: it allows case-by-case suppression of marginal claims while legitimating the system as 'balanced' and 'flexible.' The mandatrophy is resolved not by choosing a single type but by recognizing that fair use is a presheaf: different indexical positions (power, exit, time horizon) genuinely perceive different constraint types because they experience different structural realities. Marginal claimants cannot exit; major holders can. The doctrine coordinates for those with resources; it extracts from those without. This is not a classification error — it is the structure the doctrine maintains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    four_factor_test_collapse,
    'Does the four-factor fair use test reliably predict litigation outcomes, or has it degraded into post-hoc rationalization of judicial preferences?',
    'Empirical analysis of fair use outcomes: correlation between factor scores and litigation success; comparison of factors cited in winning vs losing cases; consistency of weighting across similar cases',
    'If reliable: fair use is functioning as intended (rope from marginal claimant perspective). If degraded: fair use is purely theatrical, providing no actual protection despite doctrinal coverage (piton/snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(four_factor_test_collapse, empirical, 'Whether the four-factor fair use test predicts litigation outcomes').

omega_variable(
    licensing_market_suppression,
    'To what degree do licensing demands (via DMCA, copyright notices, takedown requests) suppress assertion of legitimate fair use claims that would otherwise prevail?',
    'Longitudinal study of fair use claims made vs not made; survey of creators on licensing demand impact; analysis of takedown request volume and reversal rates',
    'If suppression is high (>50% of meritorious claims abandoned before trial): fair use is snare from marginal claimant perspective. If low (<20%): fair use doctrine functions as rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_suppression, empirical, 'Degree to which licensing demands suppress fair use assertion').

omega_variable(
    transformative_use_threshold,
    'Is ''transformative use'' (the dominant modern fair use framework) sufficiently clear to guide conduct, or does it function as an unpredictable expansion/contraction of protection?',
    'Analysis of transformative use language in case law; survey of creators on predictability; comparison of transformative outcomes across similar factual scenarios',
    'If clear: fair use provides meaningful guidance (rope). If unpredictable: creators cannot plan use (snare/tangled rope with high theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_threshold, empirical, 'Clarity and predictability of transformative use standard').

omega_variable(
    dmca_anti_circumvention_override,
    'Does DMCA anti-circumvention enforcement functionally override fair use protection by making circumvention to access copyrighted content illegal regardless of use legality?',
    'Documented cases where fair use claims failed because circumvention of technical protection measures was illegal; frequency of anti-circumvention barriers to legitimate access',
    'If DMCA overrides fair use: fair use is a snare (protection exists on paper but not in practice). If circumvention is permitted for fair use: fair use remains viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dmca_anti_circumvention_override, empirical, 'Whether DMCA anti-circumvention provisions override fair use protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_doctrine, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fairuse_tr_t0, fair_use_doctrine, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fairuse_tr_t15, fair_use_doctrine, theater_ratio, 15, 0.58).
narrative_ontology:measurement(fairuse_tr_t30, fair_use_doctrine, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(fairuse_be_t0, fair_use_doctrine, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fairuse_be_t15, fair_use_doctrine, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fairuse_be_t30, fair_use_doctrine, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_doctrine, resource_allocation).
narrative_ontology:affects_constraint(fair_use_doctrine, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_doctrine, dmca_anticircumvention).
narrative_ontology:affects_constraint(fair_use_doctrine, licensing_monopoly_digital).

% DUAL FORMULATION NOTE:
% Fair use doctrine is upstream of specific digital licensing constraints. It provides the legal framework that enables licensing monopolies and DMCA enforcement; specific platform licensing systems (YouTube Content ID, music licensing, ebook DRM) are downstream implementations that operationalize fair use suppression.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_doctrine, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
