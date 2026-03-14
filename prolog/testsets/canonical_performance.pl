% ============================================================================
% CONSTRAINT STORY: canonical_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_canonical_performance, []).

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
 *   constraint_id: canonical_performance
 *   human_readable: Canonical Performance Constraint
 *   domain: institutional/organizational
 *
 * SUMMARY:
 *   The canonical performance constraint creates a structural tension between
 *   the need for standardized coordination across distributed agents and the
 *   cost of enforcing canonical rigidity on agents whose local conditions
 *   diverge from the canonical model. This constraint emerges across
 *   organizational domains: medical protocols enforcing standardized
 *   treatment despite patient variance; academic curricula imposing canonical
 *   pedagogy despite student heterogeneity; corporate policy applying uniform
 *   rules to diverse operating contexts; professional standards requiring
 *   canonical certification despite context-specific expertise. The
 *   constraint exhibits all six DR types from different structural positions.
 *   The canonical authority benefits from legitimacy concentration; deviant
 *   performers bear conformity costs; adaptive organizations experience mixed
 *   coordination and extraction; reform coalitions see a temporary constraint
 *   with sunset potential; vestigial canons persist through performative
 *   enforcement; and the analytical observer risks naturalizing contingent
 *   standards as inherent organizational law. The theater_ratio (0.68)
 *   reflects that canonical enforcement increasingly relies on symbolic
 *   legitimacy — the threat of exclusion — rather than functional necessity.
 *   Organizations invoke 'standards,' 'best practices,' and 'certification
 *   requirements' that often provide more legitimacy theater than actual
 *   coordination benefit.
 *
 * KEY AGENTS:
 *   - Canonical Authority: Primary beneficiary (institutional/arbitrage) — captures legitimacy, gatekeeping power, and authority over what counts as acceptable performance. Can shift the canonical standard itself.
 *   - Deviant Performers: Primary victim (powerless/trapped) — forced to conform to canonical model or face systematic exclusion. No exit option without leaving the organizational context entirely.
 *   - Adaptive Organizations: Secondary victim (moderate/constrained) — need to maintain canonical legitimacy for market access while adapting to local conditions. Experience mixed coordination (standard enables compatibility) and extraction (canonical rigidity impedes necessary innovation).
 *   - Reform Coalition: Organized agents (organized/constrained) — professional societies, dissenting factions, standard-setting bodies pushing for canonical redefinition. Have agency and see exit paths through canonical evolution.
 *   - Vestigial Canon Enforcers: Institutional actor (institutional/arbitrage) — maintain canonicity through inertia, tradition invocation, and performative legitimacy claims. See their own enforcement as degraded ritual.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the need for standardization into the immutability of this specific canonical form.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(canonical_performance, 0.58).
domain_priors:suppression_score(canonical_performance, 0.65).
domain_priors:theater_ratio(canonical_performance, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(canonical_performance, extractiveness, 0.58).
narrative_ontology:constraint_metric(canonical_performance, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(canonical_performance, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(canonical_performance, tangled_rope).
narrative_ontology:human_readable(canonical_performance, "Canonical Performance Constraint").
narrative_ontology:topic_domain(canonical_performance, "institutional/organizational").

domain_priors:requires_active_enforcement(canonical_performance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(canonical_performance, canonical_authority).
narrative_ontology:constraint_victim(canonical_performance, deviation_agents).
narrative_ontology:constraint_victim(canonical_performance, organizational_adaptation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE DEVIANT PERFORMER (SNARE) — Trapped agents forced to mimic canonical performance or face systematic exclusion. No exit option without abandoning the organizational context. Bears full cost of conformity requirement while canonical authority captures legitimacy benefit.
constraint_indexing:constraint_classification(canonical_performance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ADAPTIVE ORGANIZATION (TANGLED ROPE) — Constrained by need to maintain canonical legitimacy while adapting to local conditions. Genuine coordination function (canonical standard enables cross-organizational compatibility) coexists with asymmetric extraction (canonical rigidity impedes necessary innovation).
constraint_indexing:constraint_classification(canonical_performance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE CANONICAL AUTHORITY (ROPE) — Net beneficiary with arbitrage options (can shift the canonical standard itself). Experiences constraint as coordination mechanism: the canonical form solves the standardization problem. Extraction runs toward this agent through legitimacy and compliance capture.
constraint_indexing:constraint_classification(canonical_performance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE REFORM COALITION (SCAFFOLD) — Organized agents (professional societies, standard-setting bodies, dissenting factions) pushing for canonicity redefinition with sunset logic. The constraint is temporary: as local adaptation becomes undeniable, the canonical standard itself shifts. Low effective extraction because coalition has agency and sees an exit path through canonical evolution.
constraint_indexing:constraint_classification(canonical_performance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: THE VESTIGIAL CANON (PITON) — When observed at civilizational timescale, some canonical standards persist long after their functional justification has eroded. The canon is maintained through performative enforcement — invoking tradition, legitimacy theater, and institutional inertia — rather than because it solves real problems. Theater ratio 0.68 reflects this degradation.
constraint_indexing:constraint_classification(canonical_performance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, some standardization is inherent to any coordination problem: organizations cannot function without shared reference points. Canonicity itself appears immutable. However, structural data reveals this as a false summit: the *specific* canonical form is contingent; what is immutable is only the need for *some* standard, not this particular one.
constraint_indexing:constraint_classification(canonical_performance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(canonical_performance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(canonical_performance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(canonical_performance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(canonical_performance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(canonical_performance, TR),
    TR >= 0.70.

:- end_tests(canonical_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The canonical authority captures legitimacy and gatekeeping benefits during the interval, and deviant performers bear meaningful conformity costs. However, the extraction is not maximal because genuine coordination benefit exists — the canonical standard does solve real standardization problems, even if imperfectly. The measurement trajectory (0.42 → 0.58 over interval) shows extraction accumulating as canonical rigidity increases and adaptive pressure grows. Suppression (0.65): High. Significant barriers to canonical deviation include: institutional gatekeeping (exclusion from markets, licenses, professional standing), reputational penalties (deviation signals incompetence or illegitimacy), and internalization of canonical legitimacy (identity fusion among some performers). However, suppression is not total — organizations do adapt informally, and reform coalitions push explicitly against canonical constraints. Theater ratio (0.68): Moderate-high. Canonical enforcement increasingly relies on legitimacy theater — symbolic gatekeeping and prestige signaling — rather than verification of functional performance. The canon invokes tradition, authority, and certification rituals that often provide little measurable improvement in actual outcomes. The trajectory (0.52 → 0.68) shows theater increasing as functional justification erodes.
 *
 * PERSPECTIVAL GAP:
 *   The canonical authority experiences the constraint as pure coordination (Rope) — the standard solves real standardization problems and enables cross-organizational compatibility. Their arbitrage options mean they can shift the canon itself, experiencing extraction as flowing toward them. Adaptive organizations experience tangled hybridity (Tangled Rope) — genuine coordination benefit coexists with extraction cost as canonical rigidity impedes necessary adaptation. Deviant performers experience maximum extraction (Snare) — they are trapped by canonical enforcement with no exit and no coordination benefit. The reform coalition sees this as temporary (Scaffold) — canonical standards evolve as local adaptation becomes undeniable; they have agency and see an exit through canonical redefinition. The vestigial canon perspective reveals that at civilizational timescale, the specific canonical form is degraded (Piton) — maintained by inertia and theater, not function. The analytical observer risks naturalizing contingency (false summit Mountain) — standardization is necessary; this specific canon is not. The perspectival gaps reveal that the constraint's classification depends entirely on the agent's structural position and power to shift what counts as canonical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) range from 0.0 (full beneficiary) to 1.0 (full target) based on structural position and exit capacity. The canonical authority is a beneficiary with arbitrage options: d ≈ 0.05 (low), producing negative or minimal effective extraction from their perspective. Adaptive organizations are mixed beneficiaries-victims with constrained exit: d ≈ 0.50 (symmetric), producing moderate experienced extraction. Deviant performers are pure victims with trapped exit: d ≈ 0.95 (near maximal), producing high experienced extraction. The reform coalition is organized with constrained exit but genuine agency: d ≈ 0.45, producing moderate extraction but with pathway to reduction. The vestigial canon perspective uses institutional power with arbitrage exit: d ≈ 0.05, mirroring the canonical authority's beneficiary position. The analytical observer uses analytical power and analytical exit: d ≈ 0.72 (canonical). The directionality derivation shows that exit capacity is the primary differentiator — those who can redefine the canon (arbitrage) experience it as coordination; those who must conform (trapped) experience it as extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canon_vs_coordination_boundary,
    'Where does necessary standardization end and extractive canonical rigidity begin?',
    'Comparative analysis across organizations: measure innovation rate, adaptation success, and member satisfaction under different canonical strictness regimes. Identify threshold where standardization benefit plateaus and rigidity cost accelerates.',
    'If threshold is high (much deviation tolerated): constraint is primarily Rope (coordination). If threshold is low (little deviation tolerated): constraint is primarily Snare (extraction masked as standardization). This determines whether the tangled rope classification reflects genuine hybridity or misclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(canon_vs_coordination_boundary, empirical, 'Boundary between necessary standardization and extractive rigidity').

omega_variable(
    identity_fusion_in_canonical_adherence,
    'Do performers internalize the canonical standard as identity (identity_locked) or do they comply due to external pressure (constrained)?',
    'Post-exit behavioral tracking: if performers maintain canonical performance after leaving the organization, suppression is partially internalized. If performance changes immediately, suppression is purely external/structural.',
    'If identity_locked: constraint is more resistant to reform (performers carry the lock post-exit). If constrained: coalition reform strategies targeting the organizational barrier rather than identity reconstitution will succeed more rapidly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_fusion_in_canonical_adherence, empirical, 'Whether canonical adherence is internalized identity or external constraint').

omega_variable(
    canonical_substitution_gaming,
    'Does the canonical standard itself become subject to extractive capture, with the canon weaponized by whoever controls canonical redefinition?',
    'Historical analysis of canonical shifts: do successive canonical reforms distribute adaptation benefits equally, or do they systematically advantage the institutional actor that controls the redefinition process? Measure agency concentration in standards bodies.',
    'If gaming is systematic: canonicity constraint morphs from Tangled Rope into a higher-order Snare where the extraction mechanism is precisely the authority to redefine the canon. The scaffold sunset becomes illusory — reform cycles are themselves extraction mechanisms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(canonical_substitution_gaming, empirical, 'Whether canonical redefinition becomes extractive gaming mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(canonical_performance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(canperf_tr_t0, canonical_performance, theater_ratio, 0, 0.52).
narrative_ontology:measurement(canperf_tr_t3, canonical_performance, theater_ratio, 3, 0.61).
narrative_ontology:measurement(canperf_tr_t6, canonical_performance, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(canperf_be_t0, canonical_performance, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(canperf_be_t3, canonical_performance, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(canperf_be_t6, canonical_performance, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(canonical_performance, information_standard).
narrative_ontology:affects_constraint(canonical_performance, standardization_lock_in).
narrative_ontology:affects_constraint(canonical_performance, legitimacy_capture).
narrative_ontology:affects_constraint(canonical_performance, certification_gatekeeping).

% DUAL FORMULATION NOTE:
% Canonical performance constraints decompose along the ε-invariance principle. The same organizational phenomenon generates distinct constraints: (1) information_standard function (low ε ≈ 0.15) representing genuine standardization benefit; (2) identity_coordination capture (moderate-high ε ≈ 0.58) representing professional/organizational identity fusion with canonical forms; (3) enforcement_mechanism extraction (high ε ≈ 0.75) representing the purely extractive gatekeeping function. This story focuses on the tangled hybrid (2), linking to both upstream (1) and downstream (3) constraints via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(canonical_performance, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
