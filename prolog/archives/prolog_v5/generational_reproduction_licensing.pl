% ============================================================================
% CONSTRAINT STORY: generational_reproduction_licensing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_generational_reproduction_licensing, []).

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
 *   constraint_id: generational_reproduction_licensing
 *   human_readable: Generational Reproduction Licensing Systems
 *   domain: biopolitics/governance/reproductive_justice
 *
 * SUMMARY:
 *   Generational reproduction licensing represents a biopolitical constraint
 *   system where state apparatus controls access to parenthood through
 *   mandatory approval mechanisms applied to reproductive individuals. The
 *   constraint exhibits extreme perspectival variation: those without
 *   licenses experience it as pure snare (maximal suppression, no exit),
 *   those with licenses experience mixed coordination and extraction (tangled
 *   rope), state administrators experience it as legitimate coordination
 *   (rope), resistance movements see a sunset horizon (scaffold), and the
 *   apparatus itself may be degrading into theater (piton). The core
 *   mechanism involves centralizing reproductive decision-making authority,
 *   suppressing unlicensed reproduction through legal, economic, or social
 *   penalties, and allocating reproductive capacity according to
 *   state-defined criteria (child welfare, eugenic, demographic, or mixed
 *   rationales). Extractiveness has risen over the interval (0.45 → 0.68) as
 *   criteria have become more restrictive and enforcement more intensive.
 *   Theater ratio has risen (0.35 → 0.65) as legitimacy narratives have
 *   accumulated without functional improvement in child welfare outcomes,
 *   indicating institutional degradation. This constraint is a canonical
 *   snare because it exhibits zero degrees of freedom for powerless
 *   reproductive individuals: parenthood is the only legal pathway, licensing
 *   is mandatory, denial is permanent or conditional on meeting impossible
 *   criteria, and suppression mechanisms (legal penalties, social stigma,
 *   economic barriers) prevent exit through unlicensed reproduction.
 *
 * KEY AGENTS:
 *   - Reproductive Individuals: Primary victims (powerless/trapped) — face total suppression of reproductive autonomy; cannot exit licensing system without forgoing parenthood
 *   - Disfavored Population Groups: Secondary victims (moderate/constrained) — systematically denied licensing based on genetic, economic, health, or demographic criteria; experience eugenically-motivated extraction
 *   - Licensed Parent Coalition: Organized beneficiaries (organized/constrained) — obtain licenses and access to subsidies/infrastructure; also subject to mandatory standards and compliance surveillance
 *   - Reproductive Licensing Administration: Institutional beneficiary (institutional/arbitrage) — gains legitimacy, funding, and structural continuity from administering licensing system
 *   - Reproductive Rights Movement: Organized resistance (organized/mobile) — views licensing as temporary aberration with sunset; builds counter-institutions and norm shifts toward abolition
 *   - Historical Biopolitical Apparatus: Institutional observer (institutional/arbitrage) — maintains licensing theater through inertia despite functional degradation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political choice as inevitable biological necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(generational_reproduction_licensing, 0.68).
domain_priors:suppression_score(generational_reproduction_licensing, 0.72).
domain_priors:theater_ratio(generational_reproduction_licensing, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(generational_reproduction_licensing, extractiveness, 0.68).
narrative_ontology:constraint_metric(generational_reproduction_licensing, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(generational_reproduction_licensing, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(generational_reproduction_licensing, snare).
narrative_ontology:human_readable(generational_reproduction_licensing, "Generational Reproduction Licensing Systems").
narrative_ontology:topic_domain(generational_reproduction_licensing, "biopolitics/governance/reproductive_justice").

domain_priors:requires_active_enforcement(generational_reproduction_licensing).

% --- Structural relationships ---
narrative_ontology:constraint_victim(generational_reproduction_licensing, reproductive_individuals).
narrative_ontology:constraint_victim(generational_reproduction_licensing, disfavored_populations).
narrative_ontology:constraint_victim(generational_reproduction_licensing, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REPRODUCTIVE INDIVIDUAL (SNARE) — Faces total suppression of reproductive autonomy. No legal pathway to parenthood without state approval. Cannot exit the licensing system without forgoing biological parenthood entirely. Extraction is maximal: the state captures full control over a fundamental biological and existential choice. High suppression derives from the absence of alternatives (parenthood is constrained to licensed pathway only) and the inescapability of the choice horizon itself.
constraint_indexing:constraint_classification(generational_reproduction_licensing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DISFAVORED POPULATIONS (SNARE) — Systematically denied licensing based on genetic, economic, health, or demographic criteria. Face asymmetric extraction: their reproductive capacity is instrumentalized for state eugenic or demographic goals while their own reproductive desires are suppressed. Exit options exist (migrate, bear children outside licensing, civil disobedience) but at high cost. The constraint is experienced as pure extraction because the licensing criteria explicitly target disfavored groups for suppression.
constraint_indexing:constraint_classification(generational_reproduction_licensing, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LICENSED PARENT COALITION (TANGLED ROPE) — Organized groups of parents who have obtained licenses experience a mixed constraint. The licensing system coordinates parent selection and enables child-rearing infrastructure (subsidies, housing, support services) that benefits licensed parents. But the same system extracts from this group through mandatory child-rearing standards, surveillance, and penalty mechanisms for non-compliance. Genuine coordination function exists (parent eligibility screening reduces child welfare costs) alongside asymmetric extraction (licensing fees, mandatory interventions, revocation threat).
constraint_indexing:constraint_classification(generational_reproduction_licensing, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LICENSING ADMINISTRATION (ROPE) — Institutional apparatus that implements licensing sees the system as pure coordination: screening parents improves child welfare outcomes, allocates reproductive resources to capable households, and maintains population stability. Experiences low extraction because the system benefits the administering institution through legitimacy, funding, and structural continuity. From this perspective, the constraint solves genuine coordination problems (matching parents to children, preventing unfit reproduction). The apparatus has arbitrage options (implement different criteria, expand licensing, shift enforcement intensity) and exercises them as policy.
constraint_indexing:constraint_classification(generational_reproduction_licensing, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: REPRODUCTIVE RIGHTS MOVEMENT (SCAFFOLD) — Organized resistance sees licensing as a temporary aberration with a sunset clause: generational turnover, international pressure, and evolving ethical norms around bodily autonomy are eroding political support for state-mandated reproductive licensing. The movement experiences the constraint as suppressive but explicitly time-bounded — viewing opposition as structural work toward abolition rather than adaptation. Theater ratio is moderate (legitimacy narratives around child welfare persist) but declining as violation becomes undeniable.
constraint_indexing:constraint_classification(generational_reproduction_licensing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: HISTORICAL BIOPOLITICAL APPARATUS (PITON) — From a civilizational scale, reproductive licensing is a degraded institutional form: it persists through performative legitimacy (child welfare narratives, eugenic science claims) long after the functional justification has collapsed. The apparatus has become theater — enforcement rituals, licensing ceremonies, compliance theater — maintained by institutional inertia despite minimal actual coordination of child welfare. The underlying eugenic or demographic goals are often abandoned or contradicted by policy, yet the licensing machinery persists.
constraint_indexing:constraint_classification(generational_reproduction_licensing, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a universal/civilizational analytical standpoint, one might attempt to classify reproductive licensing as a natural law: humans always have some mechanism for evaluating reproductive fitness, and state licensing is merely an explicit version of implicit cultural screening. This naturalizing frame treats the constraint as inevitable. However, the structural data contradicts the mountain classification — licensing requires active enforcement, exhibits high suppression, and is backed by coercive apparatus rather than emerging naturally. The engine will classify this as a false summit, revealing that naturalizing language obscures a contingent political choice.
constraint_indexing:constraint_classification(generational_reproduction_licensing, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(generational_reproduction_licensing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(generational_reproduction_licensing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(generational_reproduction_licensing, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(generational_reproduction_licensing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(generational_reproduction_licensing, TR),
    TR >= 0.70.

:- end_tests(generational_reproduction_licensing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. The constraint captures full control over fundamental biological and existential choice (reproduction). Initial extractiveness (0.45) reflected partial enforcement and ambiguous criteria; current value (0.68) reflects intensified criteria application and expanded surveillance. The rising trajectory indicates rent-seeking layered onto the original coordination rationale — bureaucratic expansion, fee structures, and penalty mechanisms have accumulated. Suppression (0.72): Very high. Reproductive individuals have no legal alternative pathway to parenthood; unlicensed reproduction carries severe penalties (child removal, criminal prosecution, economic penalties). For disfavored populations, suppression is systematic and eugenic in function. Suppression is structural (enforced through law and social/economic barriers) rather than primarily internalized, though legitimacy narratives create internalization vector. Theater ratio (0.65): High and rising. Original licensing rationales (child welfare) have become decoupled from actual criteria application (genetic screening, demographic targeting, eugenics). Compliance theater (licensing ceremonies, criteria review boards, evidence requirements) persists despite minimal functional relationship to child welfare outcomes. Rising theater indicates institutional degradation — the apparatus maintains legitimacy through narrative rather than demonstrated effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Reproductive individuals (powerless/trapped) perceive pure snare — no agency, no exit, total suppression. Disfavored populations perceive targeted extraction — eugenic suppression. Licensed parents perceive tangled rope — genuine coordination (parent evaluation) alongside extraction (compliance surveillance). Licensing administrators perceive rope — legitimate coordination solving child welfare problems. Resistance movements perceive scaffold — temporary problem with visible sunset. The apparatus perceives piton — theater maintained by inertia. The analytical observer risks perceiving mountain — reproductive screening as inevitable law. The perspectival gap reveals that the constraint's 'objectivity' depends entirely on structural position. What appears as legitimate coordination to administrators appears as extermination to disfavored populations. This gap is diagnostic: when the same constraint classifies as snare (powerless), tangled rope (licensed), rope (institutional), and mountain (analytical), the variation signals that legitimating narratives are obscuring extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values flow from structural position relative to the licensing apparatus. Reproductive individuals without licenses: d ≈ 0.95 (full victims, trapped, maximum d → high f(d) → maximum χ). Licensed parents: d ≈ 0.55 (mixed: beneficiary of subsidies/infrastructure but victim of compliance surveillance). Disfavored population groups: d ≈ 0.90 (nearly full victims, systematically denied, targeted extraction). Licensing apparatus: d ≈ 0.05 (institutional beneficiary, arbitrage exit, negative f(d) → negative χ, experiences as coordination). The derived d values feed the sigmoid function f(d), producing the experienced extractiveness chi for each perspective. Licensed parents with d ≈ 0.55 experience moderate extraction despite mid-range power level; trapped reproductive individuals with d ≈ 0.95 experience maximum extraction despite low power level.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness 0.68 > 0.46 threshold): The constraint resolves mandatrophy by demonstrating that reproductive licensing is unambiguously extractive rather than misclassified coordination. The snare classification is stable across perspectives: powerless reproductive individuals, disfavored populations, and (partially) licensed parents all experience extraction. The rope perspective (from licensing administrators) reflects their structural position as beneficiaries, not the constraint's true function. The piton perspective reveals institutional degradation — theater has accumulated (legitimacy narratives) while function has declined (child welfare outcomes do not improve with licensing intensity). The scaffold perspective correctly identifies a sunset horizon: generational turnover and norm evolution are eroding political support. The false mountain perspective (naturalizing as biopolitical law) is detected as a false summit: licensing requires active enforcement and exhibits no NL signature properties (accessibility_collapse and resistance values would be low, but are actually high, failing the mountain gate). The mandatrophy is resolved: reproductive licensing is a snare that has accumulated theater and is degrading toward piton, not a legitimate coordination mechanism (rope) that has been misclassified. No reframing produces rope-type coordination because the beneficiary class (licensing administrators) is not the primary agent experiencing the constraint — reproductive individuals are, and they experience pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    licensing_criteria_legitimacy,
    'On what basis is reproductive licensing legitimized — child welfare, eugenics, demographic control, or mixed rationales that contradict each other?',
    'Analysis of official licensing criteria, enforcement patterns, and policy evolution. Comparison between stated rationales and actual criteria application.',
    'If child welfare is primary: extraction may be lower than 0.68 (genuine coordination component). If eugenics/demographic control is primary: extraction may be higher, with snare classification more robust. Mixed/contradictory rationales indicate institutional degradation (Piton) rather than functional snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(licensing_criteria_legitimacy, empirical, 'Legitimacy basis for reproductive licensing criteria').

omega_variable(
    suppression_mechanism_internalization,
    'Is suppression (0.72) primarily structural (legal/economic barriers to unlicensed reproduction) or partially internalized (reproductive individuals accept licensing as legitimate)?',
    'Attitudinal surveys, resistance rates, compliance patterns. Comparison of suppression intensity before/after legitimacy erosion in populations with emerging anti-licensing norms.',
    'If primarily structural: suppression remains high post-abolition. If partially internalized: suppression may drop rapidly with norm change, revealing false consensus under coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    extraction_beneficiary_identification,
    'Who materially benefits from reproductive licensing — the state apparatus, licensed parent privileged class, child welfare bureaucracy, or some other actor?',
    'Budget analysis, resource flow mapping, career advancement tracking for licensing administrators. Identification of who captures rents from licensing fees, subsidies to licensed parents, or elimination of unlicensed competition.',
    'If state apparatus benefits: snare classification is robust (no beneficiary group emerges to provide rope-type coordination). If licensed parent class captures benefits: constraint becomes tangled rope for that group. If no clear beneficiary: constraint may degrade to piton (theater without extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, empirical, 'Identification of material beneficiaries from licensing system').

omega_variable(
    coalition_threshold_for_organized_resistance,
    'At what scale does reproductive licensing resistance become organized enough to shift from powerless (trapped) to organized (constrained) agent status?',
    'Tracking of resistance movement formation, coalition size, institutional backing, and capacity to impose costs on licensing apparatus.',
    'If resistance coalition exceeds critical mass: powerless perspective may upgrade to organized, lowering their experienced extraction and creating coalition coordination potential. This could shift the dominant classification from snare toward tangled rope or scaffold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_threshold_for_organized_resistance, empirical, 'Coalition formation threshold for organized resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(generational_reproduction_licensing, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genrepl_tr_t0, generational_reproduction_licensing, theater_ratio, 0, 0.35).
narrative_ontology:measurement(genrepl_tr_t10, generational_reproduction_licensing, theater_ratio, 10, 0.5).
narrative_ontology:measurement(genrepl_tr_t20, generational_reproduction_licensing, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(genrepl_be_t0, generational_reproduction_licensing, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(genrepl_be_t10, generational_reproduction_licensing, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(genrepl_be_t20, generational_reproduction_licensing, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(generational_reproduction_licensing, identity_coordination).
narrative_ontology:boltzmann_floor_override(generational_reproduction_licensing, 0.05).
narrative_ontology:affects_constraint(generational_reproduction_licensing, eugenic_criteria_enforcement).
narrative_ontology:affects_constraint(generational_reproduction_licensing, reproductive_biopower_apparatus).
narrative_ontology:affects_constraint(generational_reproduction_licensing, demographic_state_control).

% DUAL FORMULATION NOTE:
% Reproductive licensing is upstream of specific enforcement mechanisms (eugenic criteria, biopower apparatus, demographic control). This story captures the licensing system as a unified constraint; decomposition may separate the legitimizing narrative (child welfare coordination) from the extractive mechanism (demographic/eugenic control) if evidence shows these have distinct ε values and structural signatures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(generational_reproduction_licensing, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
