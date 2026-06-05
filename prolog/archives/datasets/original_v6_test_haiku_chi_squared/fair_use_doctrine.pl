% ============================================================================
% CONSTRAINT STORY: fair_use_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Fair Use (The Expression Safety Valve)
 *   domain: social/legal/technological
 *
 * SUMMARY:
 *   Fair use is a doctrine in copyright law that permits unlicensed use of
 *   copyrighted works for purposes including criticism, commentary, news
 *   reporting, teaching, scholarship, and transformative creation. Enacted as
 *   Section 107 of the Copyright Act (1976) and interpreted through the
 *   four-factor test (purpose, nature, amount, market effect), fair use is
 *   nominally a safety valve enabling expression freedom within a
 *   copyright-protected system. However, the constraint exhibits all six DR
 *   types from different perspectives, revealing a system under structural
 *   stress. Large copyright holders have weaponized fair use uncertainty
 *   through litigation strategy, self-censorship is widespread among
 *   independent creators, the four-factor test has degraded into inconsistent
 *   application across circuits, and open-access alternatives are emerging
 *   that may render the doctrine obsolete. The theater_ratio (0.68) reflects
 *   that the legal system now uses fair use as a suppression tool through
 *   litigation risk asymmetry rather than as a genuine enabling doctrine. The
 *   doctrine simultaneously functions as coordination (enabling education and
 *   commentary), extraction (enabling litigation-based suppression),
 *   temporary scaffolding (toward open-access alternatives), and inertial
 *   theater (continued reliance on a doctrine that no longer performs its
 *   stated function).
 *
 * KEY AGENTS:
 *   - Large Copyright Conglomerates: Primary extractors (institutional/mobile) — use fair use litigation to suppress derivative markets and enforce licensing control
 *   - Small Copyright Holders: Primary victims (powerless/trapped) — cannot defend fair use claims due to litigation cost asymmetry; bear full cost of legal uncertainty
 *   - Independent Creators & Critics: Mixed victim/beneficiary (moderate/constrained) — depend on fair use for legitimate criticism and transformation but face self-censorship pressure from litigation risk
 *   - Educational Institutions: Beneficiaries (institutional/arbitrage) — have litigation capacity and benefit from fair use as coordination mechanism
 *   - Open Access & Digital Rights Communities: Organized agents (organized/constrained) — view fair use as temporary scaffolding toward licensing alternatives and open-access publishing
 *   - Copyright Legal System: Institutional actor (institutional/arbitrage) — maintains performative four-factor test; has degraded into inconsistent application and litigation risk assessment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing fair use as inherent to copyright rather than as contingent US doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_doctrine, 0.52).
domain_priors:suppression_score(fair_use_doctrine, 0.58).
domain_priors:theater_ratio(fair_use_doctrine, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_doctrine, extractiveness, 0.52).
narrative_ontology:constraint_metric(fair_use_doctrine, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fair_use_doctrine, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_doctrine, tangled_rope).
narrative_ontology:human_readable(fair_use_doctrine, "Fair Use (The Expression Safety Valve)").
narrative_ontology:topic_domain(fair_use_doctrine, "social/legal/technological").

domain_priors:requires_active_enforcement(fair_use_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_doctrine, educators_and_scholars).
narrative_ontology:constraint_beneficiary(fair_use_doctrine, critics_and_commentators).
narrative_ontology:constraint_beneficiary(fair_use_doctrine, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_doctrine, public_discourse_ecosystem).
narrative_ontology:constraint_victim(fair_use_doctrine, copyright_holders).
narrative_ontology:constraint_victim(fair_use_doctrine, small_creators_without_litigation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL COPYRIGHT HOLDER (SNARE) — Individual creators and small publishers face asymmetric litigation risk. Fair use doctrine is nominally symmetric, but the cost of defending against an infringement claim (depositions, discovery, expert witnesses: $100k–$1M+) exceeds the value of most small-scale copyright works. Trapped: cannot afford to defend fair use claims in court even when meritorious. Victim + trapped → d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(fair_use_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTIONS & LIBRARIES (ROPE) — Schools, universities, and public libraries benefit from fair use as a coordination mechanism enabling classroom use, research citation, and preservation. They have institutional capacity to assert fair use and negotiate licenses. Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01, σ=1.0 → χ≈-0.01. Net benefit from coordination; minimal extraction.
constraint_indexing:constraint_classification(fair_use_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: INDEPENDENT CRITICS & CREATORS (TANGLED ROPE) — YouTubers, bloggers, and film critics depend on fair use to critique and comment on works. Fair use enables their productive activity (coordination benefit), but they also bear litigation risk. They are victims of copyright expansion (deterred from legitimate criticism) and beneficiaries of fair use protection (when it holds). Constrained exit: can use other sources for critique, but fair use is central to their model. Victim + beneficiary + constrained → d≈0.65, f(d)≈1.05, σ=1.0 → χ≈0.55. Mixed extraction and coordination.
constraint_indexing:constraint_classification(fair_use_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE COPYRIGHT CONGLOMERATES (SNARE) — Disney, Warner Bros., and major publishing houses face significant loss of licensing revenue to fair use, particularly for transformative works (fan art, criticism, remix). They have moved against fair use through litigation strategy (Sony v. Betamax, Harper & Row v. Nation, Google v. Oracle) and legislative capture (DMCA, term extension). From their perspective, fair use is a snare: they bear the cost of lost licensing fees and reduced control over derivative markets. However, their power is organizational + mobile: they can afford litigation and lobby for legislation. Victim + mobile + powerful → d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39. Extraction without full snare dynamics due to power and mobility.
constraint_indexing:constraint_classification(fair_use_doctrine, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN ACCESS & DIGITAL RIGHTS (SCAFFOLD) — EFF, Creative Commons, and academic open-access movements view fair use as temporary scaffolding toward a permanent solution: direct licensing (Creative Commons), open access publishing, and expanded public domain. They see fair use as a crutch (theater_ratio=0.68) — the four-factor test is ambiguous and litigation-dependent — that will be superseded by systemic licensing and commons-based alternatives. Organized + constrained → d≈0.35, f(d)≈0.30, σ=1.0 → χ≈0.16. Low extraction because the coalition has agency and a sunset vision.
constraint_indexing:constraint_classification(fair_use_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: COPYRIGHT LEGAL SYSTEM (PITON) — Fair use doctrine is a legal principle that has degraded into a theater of litigation risk assessment rather than a genuine safety valve. The four-factor test (purpose, nature, amount, market effect) is applied inconsistently by courts; different circuits reach opposite conclusions on identical facts. Copyright holders use fair use uncertainty as a suppression tool: creators self-censor rather than incur litigation risk even when meritorious fair use defense exists. Theater_ratio=0.68: the legal system claims to protect fair use but has converted it into a gambling game where the outcome depends on litigation capacity and judge preference, not on the four-factor test's stated logic. Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Theater gate satisfied; piton classification.
constraint_indexing:constraint_classification(fair_use_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some mechanism for allowing transformative and non-market-substituting uses of copyrighted works is inevitable in any system that protects expression. The tension between copyright incentives (promote creation through control) and expression freedom (enable criticism, education, transformation) is structural to copyright law itself — not contingent on doctrine, but inherent to the logical space. This perspective risks naturalizing what is actually a contingent US legal doctrine. However, the base properties (ε=0.52, suppression=0.58, theater=0.68) contradict mountain classification — the engine will compute this as a false summit.
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
    constraint_indexing:constraint_classification(fair_use_doctrine, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. Fair use doctrine was designed as a safety valve enabling expression, but copyright expansion (term extension, DMCA, work-made-for-hire rules) has structurally reduced its scope. Large copyright holders use fair use uncertainty as a suppression tool: the cost of litigation ($100k–$1M+) exceeds the value of most fair use works, creating asymmetric deterrence. However, extractiveness is not at snare levels (≥0.66) because fair use does function for well-resourced actors (large publishers, educational institutions, tech companies). Suppression (0.58): Moderate-high. Multiple suppression mechanisms: litigation cost asymmetry deters small creators from asserting valid fair use; DMCA anti-circumvention rules block transformative uses; four-factor test ambiguity creates legal chilling effect; copyright term extension reduces public domain availability. But suppression is not total — some uses clearly qualify as fair (criticism, news reporting), and some actors (educators) can assert fair use effectively. Theater ratio (0.68): High and increasing. The four-factor test is inconsistently applied across circuits; outcomes depend substantially on litigation capacity and judge preference, not on the stated factors. The legal system claims to apply a coherent doctrine but delivers inconsistent outcomes. Theater has increased over the interval (from 0.35 to 0.68) because copyright expansion has made the four-factor test's boundaries more contested.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Large copyright holders see fair use as extraction (Snare from their perspective) — they bear the cost of lost licensing revenue. Small creators see it as inadequate protection (Snare from their perspective) — litigation costs trap them. Educational institutions see it as genuine coordination (Rope) — they benefit without extractive costs. Independent critics see mixed benefit and harm (Tangled Rope) — fair use enables their work but litigation risk constrains it. The open-access movement sees a temporary solution with a sunset (Scaffold) — Creative Commons and open-access publishing will make fair use obsolete. The legal system sees its own doctrine as degraded (Piton) — inconsistent application maintained through institutional inertia. The civilizational observer risks naturalizing fair use as inherent to copyright (Mountain) — but the extractiveness data reveals it as contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Large copyright conglomerates: Victim + mobile + powerful → d≈0.55, f(d)≈0.75. They experience fair use as extraction (lost licensing fees) but have mobile exit options (litigation, lobbying, technology barriers). Small copyright holders: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction — cannot afford litigation defense. Independent creators: Mixed victim + beneficiary + constrained → d≈0.65, f(d)≈1.05. Fair use benefits their work but litigation risk constrains it. Educational institutions: Beneficiary + arbitrage → d≈0.15, f(d)≈-0.01. Net beneficiary with institutional capacity. Open-access community: Organized + constrained → d≈0.35, f(d)≈0.30. Low extraction; coalition has agency and sees exit path. Legal system: Institutional + arbitrage → d≈0.05, f(d)≈-0.12. Theater gate applies; piton classification.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL DISTINCTION: Fair use resolves the mandatrophy by revealing that copyright expansion (term extension, DMCA, work-made-for-hire) has converted a nominal coordination mechanism (fair use) into a power-asymmetric extraction tool. The doctrine is NOT a failed attempt at classification; it is a successful clarification that fair use's function has DEGRADED over the interval. The theater_ratio increase (0.35→0.68) and extractiveness increase (0.28→0.52) show that the doctrine itself has changed character, not that we misunderstood it. The large copyright holders are not misclassified as snares; they genuinely experience fair use as extraction (lost licensing revenue). Small creators are not misclassified as powerless; they genuinely are trapped by litigation cost asymmetry. The mandatrophy resolution is: fair use is NOW a tangled rope (mixed coordination + extraction) and increasingly a snare for small actors, whereas it was nominally a rope (pure coordination) in 1976. The doctrine's text has not changed significantly, but its effective function has degraded due to environmental pressure (copyright expansion) and structural capture (large holder litigation strategy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    litigation_cost_asymmetry_threshold,
    'At what ratio of litigation cost to copyright work value does fair use cease to function as a practical safety valve?',
    'Empirical study of actual litigation costs by case type vs. median copyright work value; analysis of self-censorship rates among independent creators as a function of expected litigation risk',
    'If ratio is 10:1 or higher: fair use is primarily a power asymmetry (Snare from small creator perspective). If ratio < 5:1: fair use can function as coordination for moderate actors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigation_cost_asymmetry_threshold, empirical, 'Litigation cost to copyright value ratio threshold').

omega_variable(
    four_factor_test_predictability,
    'Can the four-factor fair use test predict case outcomes better than random chance across circuit boundaries?',
    'Supervised learning analysis on published fair use decisions (1990–present) across all circuits; hold-out test on recent cases to evaluate prediction accuracy; cross-circuit consistency metrics',
    'If predictability < 70%: four-factor test is theater (Piton). If predictability > 85%: test functions as genuine doctrine (Rope or Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(four_factor_test_predictability, empirical, 'Predictive accuracy of the four-factor test across circuits').

omega_variable(
    transformative_use_definition_drift,
    'Has the courts'' definition of ''transformative use'' expanded or contracted over time, and does this reflect genuine doctrine evolution or substitution of different criteria?',
    'Longitudinal text analysis of fair use opinions (1990–present) comparing language used to justify transformative-use findings; coding of actual factors applied vs. stated four-factor test; correlation with copyright holder litigation success rate over time',
    'If drift reflects genuine doctrine expansion: fair use safety valve is strengthening (Tangled Rope trending toward Rope). If drift reflects criterion substitution: doctrine is degrading (Piton dynamics intensifying).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_definition_drift, conceptual, 'Evolution of transformative use definition in case law').

omega_variable(
    copyright_expansion_vs_fair_use_scope,
    'Is fair use a genuine balance mechanism, or has copyright expansion (term length, work-made-for-hire, DMCA) structurally reduced fair use''s effective scope?',
    'Historical analysis: compare pre-1976 fair use doctrine vs. post-DMCA fair use scope; identify categories of use that were unambiguously fair in 1976 but are now contested or blocked; estimate percentage of original content now locked behind technical (DMCA) or legal (term extension) barriers',
    'If effective fair use scope has shrunk: fair use is extraction mechanism (copyright holders gain asymmetric control). If scope has remained stable: fair use may be genuine balance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_expansion_vs_fair_use_scope, empirical, 'Whether copyright expansion has reduced fair use''s effective scope').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_doctrine, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_use_tr_t0, fair_use_doctrine, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fair_use_tr_t25, fair_use_doctrine, theater_ratio, 25, 0.54).
narrative_ontology:measurement(fair_use_tr_t50, fair_use_doctrine, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(fair_use_be_t0, fair_use_doctrine, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fair_use_be_t25, fair_use_doctrine, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(fair_use_be_t50, fair_use_doctrine, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_doctrine, information_standard).
narrative_ontology:affects_constraint(fair_use_doctrine, copyright_term_extension).
narrative_ontology:affects_constraint(fair_use_doctrine, dmca_anti_circumvention).
narrative_ontology:affects_constraint(fair_use_doctrine, work_made_for_hire_expansion).

% DUAL FORMULATION NOTE:
% Fair use is downstream of copyright expansion mechanisms (DMCA, Sonny Bono Act, work-made-for-hire rules). Each of those constraints has its own ε value reflecting its specific legal and technical structure. Fair use's ε=0.52 reflects the cumulative impact of all three upstream constraints on the doctrine's effective scope and function.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_doctrine, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
