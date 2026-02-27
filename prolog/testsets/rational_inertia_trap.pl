% ============================================================================
% CONSTRAINT STORY: rational_inertia_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_inertia_trap, []).

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
 *   constraint_id: rational_inertia_trap
 *   human_readable: Legacy Protocol Lock-in: Rational Individual Inertia in Face of Superior Alternatives
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The legacy protocol lock-in is a canonical coordination failure that
 *   appears rational at the individual level but collectively irrational. No
 *   single user can profitably switch to a superior protocol if network
 *   effects and switching costs keep everyone on the inferior one — yet if
 *   all users switched together, all would be better off. The constraint's
 *   structural nature is ambiguous: it can be read as a natural feature of
 *   network topology (mountain perspective), a justified coordination
 *   equilibrium (rope perspective), a temporary problem with a migration
 *   solution (scaffold perspective), an institutionally maintained degraded
 *   system (piton perspective), or a hybrid with asymmetric extraction
 *   (tangled rope perspective). The empirical progression from low
 *   extractiveness (0.28) to moderate (0.52) reflects the typical lifecycle
 *   of such constraints: they begin as genuine network coordination problems,
 *   gradually accumulate extractive behavior by incumbent maintainers
 *   (proprietary data formats, deliberate incompatibilities, licensing
 *   restrictions), and eventually demand organized migration efforts
 *   (regulatory intervention, industry consortia, open standards). The
 *   theater ratio remains low (0.35) because the constraint's mechanism is
 *   transparent — everyone understands why inertia persists — but the theater
 *   increases over time as maintainers resort to performative justifications
 *   for why the inferior protocol should persist.
 *
 * KEY AGENTS:
 *   - Locked-In Users: Primary victims (powerless/trapped) — rationally trapped by individual cost-benefit calculus despite collective suboptimality
 *   - Incumbent Protocol Maintainers: Primary beneficiaries (institutional/arbitrage) — extract value from critical mass coordination on legacy system; could switch but don't
 *   - Early Adopters of Superior Protocol: Secondary victims (moderate/constrained) — invest in alternative; face coordination problems of minority adoption; benefit when critical mass flips
 *   - Migration Authorities: Hybrid actors (organized/constrained) — regulatory bodies, industry consortia tasked with enabling transition; can extract rents by controlling timeline
 *   - Legacy System Administrators: Institutional roles (institutional/arbitrage) — maintain degraded system through organizational inertia; see own role as increasingly performative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as immutable features of network dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_inertia_trap, 0.52).
domain_priors:suppression_score(rational_inertia_trap, 0.48).
domain_priors:theater_ratio(rational_inertia_trap, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_inertia_trap, extractiveness, 0.52).
narrative_ontology:constraint_metric(rational_inertia_trap, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rational_inertia_trap, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_inertia_trap, tangled_rope).
narrative_ontology:human_readable(rational_inertia_trap, "Legacy Protocol Lock-in: Rational Individual Inertia in Face of Superior Alternatives").
narrative_ontology:topic_domain(rational_inertia_trap, "technological/economic").

domain_priors:requires_active_enforcement(rational_inertia_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_inertia_trap, incumbent_protocol_maintainers).
narrative_ontology:constraint_beneficiary(rational_inertia_trap, switching_cost_extractors).
narrative_ontology:constraint_victim(rational_inertia_trap, early_adopters_of_superior_protocol).
narrative_ontology:constraint_victim(rational_inertia_trap, collective_efficiency_loss).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — Individual agent rationally trapped. Switching cost (retraining, data migration, network reconfiguration) exceeds the biographical-horizon benefit. Even though the superior protocol would save hours daily over a career, the upfront cost is borne today and the benefit accrues slowly. The agent is trapped not by coercion but by the rational calculus of discounted payoffs. Zero degrees of freedom given the individual decision boundary.
constraint_indexing:constraint_classification(rational_inertia_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INCUMBENT PROTOCOL MAINTAINER (ROPE) — Institutional actor benefits from coordination on the legacy system. Network effects lock users in together; the maintainer's revenue model depends on the critical mass using the inferior protocol. The constraint appears as pure coordination: 'keep everyone on the same system' solves the collective action problem of fragmentation, even though that 'same system' is sub-optimal. The maintainer has arbitrage exit (could switch to supporting the superior protocol and extract transition fees).
constraint_indexing:constraint_classification(rational_inertia_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INTERMEDIATE MIGRATION AUTHORITY (TANGLED ROPE) — Authority tasked with enabling migration to superior protocol (government, regulatory body, industry consortium) sees both coordination function AND asymmetric extraction. Coordination: manages transition to reduce friction. Extraction: controls migration timeline, can extract rents by offering preferred pathways, can enforce sequencing. The authority has constrained exit — it cannot simply abandon the legacy system without catastrophic disruption, but it has more agency than individual users.
constraint_indexing:constraint_classification(rational_inertia_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EARLY ADOPTER COALITION (SCAFFOLD) — Organized actors who invest in the superior protocol see the constraint as temporary with a sunset. The coalition's value comes from building the alternative critical mass. As adoption accelerates, the legacy system's network effect reverses — staying becomes the costly choice. The constraint degrades as adoption crosses inflection points. Low effective extraction because the coalition has agency and sees a clear exit path through minority-to-majority transition.
constraint_indexing:constraint_classification(rational_inertia_trap, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SYSTEM ADMINISTRATOR (PITON) — Institutional role maintains the inferior protocol through organizational inertia. The administrator sees the protocol as degraded — everyone acknowledges the superior alternative exists — yet the maintenance continues because the switching solution hasn't fully replaced it. Theater ratio high: maintenance becomes ritual compliance rather than functional necessity. The role exists because institutions move slower than optimal, not because the protocol is optimal.
constraint_indexing:constraint_classification(rational_inertia_trap, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COORDINATION PROBLEM VIEW (MOUNTAIN) — From a civilizational perspective, coordination problems over network topology are inherent to distributed systems. Some degree of lock-in to early adopted protocols is an unavoidable property of how networks crystallize. This perspective risks naturalizing a contingent institutional choice as a law of network dynamics. The engine will flag this as a false summit: the lock-in appears inherent only if you ignore the intentional choices by incumbent maintainers to extract switching costs.
constraint_indexing:constraint_classification(rational_inertia_trap, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_inertia_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_inertia_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_inertia_trap, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_inertia_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_inertia_trap, TR),
    TR >= 0.70.

:- end_tests(rational_inertia_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint begins as a genuine coordination problem (users rationally stay on inferior protocol because others do) with minimal extractive content. As time progresses, incumbent maintainers intentionally increase switching costs — proprietary extensions, incompatibilities, licensing changes — transforming the coordination problem into an extraction mechanism. By period 10, the constraint has accumulated substantial extractive behavior (restricted data formats, planned obsolescence, vendor lock-in practices). Suppression (0.48): Moderate. Users are suppressed not by overt coercion but by rational individual incentives misaligned with collective good. The suppression is real and high (alternatives are available but individually inaccessible), but it is not absolute — organized coalitions can achieve migration and switching does eventually occur. Theater ratio (0.35) and rising: Low to moderate theater. The mechanism is transparent — everyone understands why inertia persists. Theater increases over time as maintainers employ performative justifications ('stability,' 'backward compatibility,' 'customer choice') for perpetuating inferior systems. True functional content is decreasing as superior alternatives mature.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits stark perspectival divergence. Users perceive a snare (rational trap with no exit), incumbents perceive a rope (justified stability), early adopters perceive a scaffold (temporary problem with a migration sunset), administrators perceive a piton (degraded system maintained by inertia), authorities perceive a tangled rope (coordination with asymmetric extraction opportunity), and analytical observers risk perceiving a mountain (inherent network property). The gap is not measurement ambiguity but structural position: each agent's power level and exit options determine whether they experience the constraint as coordination or extraction. The locked-in user has no alternatives; the incumbent has many. The classification divergence directly reflects this asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Locked-in users (powerless/trapped) experience maximum extraction: they cannot exit without bearing full switching cost, and they are trapped in an individually rational equilibrium that harms them collectively. Incumbent maintainers (institutional/arbitrage) experience negative extraction (benefit): they extract rents from critical mass coordination and can always exit to the superior protocol if it becomes profitable. Early adopters (moderate/constrained) experience mixed extraction: they benefit from pioneering investment but are constrained by slow adoption and face coordination problems of being in the minority. Migration authorities (organized/constrained) experience moderate extraction: they have agency and can shape the migration process, but they cannot unilaterally force transition without massive disruption — their constraint is real but not total. The piton perspective (institutional/arbitrage) observes that legacy system maintenance has become performative — the role persists through inertia, not function. The mountain perspective (analytical/analytical) risks claiming that lock-in to early-adopted protocols is inherent to networks; the engine flags this as a false summit because empirical analysis shows incumbent active extraction (deliberate cost increase) rather than pure topology-driven inertia.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy (false 'pure extraction' vs false 'pure coordination' classification) by showing that the lock-in is genuinely a hybrid. The early stages (periods 0-3) are dominated by coordination problems — users staying on inferior protocol because others do, with minimal active extraction. Later stages (periods 7-10) show substantial extraction — incumbents deliberately increasing switching costs to prolong lock-in. The tangled rope classification at the analytical level correctly captures both aspects: there is a genuine coordination function (network stability has value), but it is increasingly overlaid with asymmetric extraction (incumbent cost increases targeted at users). Neither pure rope nor pure snare fits the full lifecycle. The scaffold perspective on early adopters is crucial: it shows that migration is structurally possible via organized alternatives, not structurally impossible. The lock-in is real but not immutable — it has a sunset condition (when superior protocol adoption exceeds inflection point).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_empirical_threshold,
    'At what switching cost threshold does individual rational inertia flip from snare (behavioral trap) to rope (justified coordination stability)?',
    'Longitudinal adoption data: compare switching costs for historical protocol transitions (IPv4→IPv6, HTTP→HTTPS, legacy email protocols) against actual adoption rates and timing. Identify the threshold where marginal benefit exceeds cost.',
    'If threshold < 10% of annual benefit: most apparent lock-ins are pure extraction (snare from user perspective). If threshold > 50% of annual benefit: many lock-ins reflect genuinely efficient coordination (rope from both perspectives). Classification shifts between snare and rope depending on empirical cost structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_empirical_threshold, empirical, 'Switching cost threshold distinguishing behavioral trap from justified stability').

omega_variable(
    network_effect_asymmetry,
    'Does the incumbent protocol maintainer actively increase switching costs (extractive behavior) or do switching costs arise purely from network topology (coordination cost)?',
    'Historical analysis of switching-cost increases over time: proprietary data formats vs open standards, deliberate incompatibilities, licensing restrictions, planned obsolescence. Compare against technologically necessary costs.',
    'If active extraction: snare from user perspective is structural (extraction prevents exit). If network topology alone: snare is behavioral (users could exit collectively, but individually cannot). Impact on remedies: active extraction requires regulatory intervention; topology-based trap requires coordination technology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_asymmetry, empirical, 'Whether incumbent actively increases switching costs or costs arise from network topology').

omega_variable(
    critical_mass_inflection_point,
    'At what adoption percentage for the superior protocol does the lock-in spontaneously reverse (staying becomes costly)?',
    'Historical adoption curves for protocol transitions: identify inflection points where late adopters'' rational choice flips from staying to switching. Compare IPv4 remaining users (currently <5%), legacy mobile networks, outdated payment systems.',
    'If inflection < 20% adoption: lock-in is brittle — small organized pushes can trigger cascade. If inflection > 60% adoption: lock-in is structural — requires either forced transition or long co-existence. Affects scaffold sunset feasibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_inflection_point, empirical, 'Adoption percentage threshold for spontaneous lock-in reversal').

omega_variable(
    externality_visibility,
    'Do individual users perceive the collective efficiency loss from their personal inertia, and does that perception change their rational calculus?',
    'Survey-based willingness-to-pay for migration: if users internalize collective loss as a personal disutility. Compare stated reasons for staying with revealed preference data. Test whether making collective cost salient increases switching rates.',
    'If externalities invisible: users are rationally trapped from individual perspective only. If salient but disregarded: mixed preference/rationality failure. If salient and internalized: lock-in is primarily coordination problem, not extraction. Affects classification (snare vs rope vs scaffold).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(externality_visibility, conceptual, 'Whether users perceive and internalize collective efficiency loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_inertia_trap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rit_tr_t0, rational_inertia_trap, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rit_tr_t5, rational_inertia_trap, theater_ratio, 5, 0.25).
narrative_ontology:measurement(rit_tr_t10, rational_inertia_trap, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(rit_be_t0, rational_inertia_trap, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(rit_be_t5, rational_inertia_trap, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(rit_be_t10, rational_inertia_trap, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_inertia_trap, information_standard).
narrative_ontology:affects_constraint(rational_inertia_trap, network_effect_critical_mass).
narrative_ontology:affects_constraint(rational_inertia_trap, switching_cost_economies).
narrative_ontology:affects_constraint(rational_inertia_trap, standard_convergence_equilibria).

% DUAL FORMULATION NOTE:
% The legacy protocol lock-in decomposes into multiple structural claims: (1) Network effect lock-in (pure coordination, ε≈0.15, Mountain), (2) Switching cost accumulation by incumbents (extractive behavior, ε≈0.60, Snare), (3) Migration technology and standards (enabling exit, χ≤0.30, Scaffold). Each has distinct ε and classification. This story models the hybrid with rising extractiveness, showing how coordination problems become extraction mechanisms when incumbents have agency to increase costs. Upstream constraints (network_effect_critical_mass, switching_cost_economies) are structural foundations. This constraint sits at their intersection.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rational_inertia_trap, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
