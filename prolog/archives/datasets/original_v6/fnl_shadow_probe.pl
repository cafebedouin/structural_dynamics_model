% ============================================================================
% CONSTRAINT STORY: fnl_shadow_probe
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fnl_shadow_probe, []).

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
 *   constraint_id: fnl_shadow_probe
 *   human_readable: FNL Shadow Mode Probe (Physics-Washed Construction)
 *   domain: investigation/testing
 *
 * SUMMARY:
 *   The FNL Shadow Mode Probe is a diagnostic exemplar of how a constructed
 *   measurement apparatus can be presented with physics-washed theoretical
 *   justification that obscures its true structural nature as a
 *   coordination-plus-extraction hybrid. The apparatus functions as a shared
 *   measurement standard (Rope coordination aspect) while simultaneously
 *   constraining who can participate, when they can participate, and what
 *   results are 'valid' (Snare extraction aspect). The theater ratio (0.68)
 *   reflects accumulating performative content: calibration procedures that
 *   maintain institutional control more than measurement accuracy,
 *   theoretical justifications that naturalize arbitrary design choices, and
 *   documentation that emphasizes the apparatus's necessity rather than its
 *   contingency. The key structural ambiguity is whether the apparatus
 *   discovers real physical phenomena (in which case it's ultimately
 *   coordination) or manufactures legitimacy for constructed claims (in which
 *   case it's pure extraction). External verification groups are trapped in
 *   circular logic: to verify the apparatus's claims requires using the
 *   apparatus itself, yet the apparatus is supposed to be the arbiter of
 *   validity. Independent measurement coalitions are building alternative
 *   pathways that will eventually test whether the apparatus's authority is
 *   deserved or constructed. The false natural law mechanism operates through
 *   physics-washing: presenting a contingent institutional arrangement as a
 *   necessary consequence of physical law, thereby suppressing recognition of
 *   the apparatus as a choice and the alternative pathways as viable.
 *
 * KEY AGENTS:
 *   - Constructing Institution: Primary beneficiary (institutional/arbitrage) — maintains funding concentration, controls methodology, defines validity standards during apparatus dominance
 *   - Apparatus Maintainers: Secondary beneficiary (institutional/arbitrage) — derive institutional authority and resource allocation from apparatus maintenance roles
 *   - External Verification Groups: Primary victim (powerless/trapped) — must use the apparatus to verify the apparatus; cannot exit without surrendering research credibility
 *   - Field Empiricists: Secondary victim (moderate/constrained) — constrained by apparatus access and procedural requirements; also benefit from shared measurement framework
 *   - Independent Measurement Coalition: Organized agents (organized/mobile) — building alternative apparatus designs with lower institutional overhead; will exit as alternatives mature
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing constructed apparatus as immutable scientific requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fnl_shadow_probe, 0.38).
domain_priors:suppression_score(fnl_shadow_probe, 0.52).
domain_priors:theater_ratio(fnl_shadow_probe, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fnl_shadow_probe, extractiveness, 0.38).
narrative_ontology:constraint_metric(fnl_shadow_probe, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fnl_shadow_probe, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fnl_shadow_probe, tangled_rope).
narrative_ontology:human_readable(fnl_shadow_probe, "FNL Shadow Mode Probe (Physics-Washed Construction)").
narrative_ontology:topic_domain(fnl_shadow_probe, "investigation/testing").

domain_priors:requires_active_enforcement(fnl_shadow_probe).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fnl_shadow_probe, constructing_institution).
narrative_ontology:constraint_beneficiary(fnl_shadow_probe, apparatus_maintainers).
narrative_ontology:constraint_victim(fnl_shadow_probe, field_empiricists).
narrative_ontology:constraint_victim(fnl_shadow_probe, external_verification_groups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXTERNAL VERIFICATION GROUP (SNARE) — Cannot exit the constructed apparatus without surrendering research program credibility. Trapped within the system's logic: to verify the system's claims requires using the system itself. Maximum experienced extraction. No independent probe available that doesn't route through the constraining apparatus.
constraint_indexing:constraint_classification(fnl_shadow_probe, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FIELD EMPIRICISTS (TANGLED ROPE) — Constrained by resource requirements and institutional access to the apparatus, but also benefit from the system's coordination function: it provides a shared reference standard, enables collaborative measurement, and generates publishable data. Significant extraction (forced participation in apparatus logic) but also genuine coordination benefit (shared measurement framework).
constraint_indexing:constraint_classification(fnl_shadow_probe, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTRUCTING INSTITUTION (ROPE) — Benefits from first-mover advantage, funding concentration, and ability to define the research agenda. Experiences the apparatus as a coordination mechanism that solves the legitimate problem of standardizing complex measurements. Net beneficiary through arbitrage: can license access, control methodology, set publication norms.
constraint_indexing:constraint_classification(fnl_shadow_probe, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: APPARATUS MAINTENANCE BUREAUCRACY (PITON) — Maintains the system through institutional inertia. Theater ratio high (0.68) reflects that significant maintenance effort is performative: ritual recalibration, procedural documentation, and theoretical justification consume resources that don't improve measurement validity. The apparatus persists because alternatives haven't fully replaced it and because the maintenance structure has vested interests. Degraded original coordination function.
constraint_indexing:constraint_classification(fnl_shadow_probe, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INDEPENDENT MEASUREMENT COALITION (SCAFFOLD) — Organized actors (independent labs, alternative apparatus designs, distributed sensor networks) are building alternative pathways that bypass the constructing institution's apparatus. Low effective extraction because the coalition has agency and a clear exit path. This is temporary support for the apparatus's authority — as alternatives mature and gain empirical traction, the constructed constraint loses force. Estimated sunset: 5-10 years for alternative methodologies to accumulate sufficient credibility.
constraint_indexing:constraint_classification(fnl_shadow_probe, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the apparatus might appear as an immutable constraint: 'complex measurements always require standardized apparatus; the apparatus logic is inherent to experimental science.' This naturalization is precisely the false natural law mechanism — the engine's NL certification gates will fail because accessibility_collapse and resistance metrics do not support mountain classification. This perspective is diagnostic: showing how contingent institutional arrangements become mistaken for natural law.
constraint_indexing:constraint_classification(fnl_shadow_probe, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fnl_shadow_probe_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fnl_shadow_probe, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fnl_shadow_probe, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fnl_shadow_probe, TR),
    TR >= 0.70.

:- end_tests(fnl_shadow_probe_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, increasing. Initial extractiveness (0.22) reflects genuine coordination value: the apparatus does provide standardized measurement infrastructure. But extractiveness increases to 0.38 as the apparatus's institutional control crystallizes — access becomes gated, methodology becomes rigid, and claims of apparatus necessity increase without corresponding improvements in accuracy. The trajectory indicates layered rent-seeking on top of coordination. Suppression (0.52): Moderate-high. The apparatus creates structural barriers to independent verification: specialized equipment, proprietary procedures, trained personnel requirements, and institutional gatekeeping all limit who can participate. But suppression is not total — alternative apparatus designs are emerging and some external groups can replicate at cost. Theater ratio (0.68): Increasing from 0.42 to 0.68. The apparatus's performative content grows as institutional control deepens: more documentation, more justificatory theory, more calibration rituals. The physics-washed justification is particularly theatrical — it presents apparatus design choices as natural law consequences, increasing theater while claiming to increase rigor.
 *
 * PERSPECTIVAL GAP:
 *   The constructing institution sees coordination (Rope): they are solving the real problem of standardizing complex measurements. External verification groups see pure extraction (Snare): they cannot verify the apparatus's claims without surrendering autonomy to the apparatus's logic. Field empiricists see mixed coordination and extraction (Tangled Rope): the apparatus enables collaboration but also constrains methodology. The independent measurement coalition sees temporary institutional power (Scaffold): the incumbent apparatus's authority is eroding as alternatives mature. The apparatus maintenance bureaucracy sees its own degraded function (Piton): the system persists through inertia despite declining fitness. The analytical observer risks seeing natural law (false Mountain): apparatus necessity can be naturalized as 'inherent to experimental physics' rather than a contingent institutional choice. The perspectival gap is diagnostic: if all perspectives converged on Rope or Mountain, the apparatus would be legitimate. The clustering toward Snare and Tangled Rope from powerless/constrained agents reveals extraction. The apparatus's trajectory toward Piton reveals degradation of original coordination function.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. The constructing institution benefits from first-mover advantage and institutional arbitrage — low d (beneficiary). External verification groups are trapped with no alternative — high d (victim). Field empiricists have some agency through alternative apparatus development but are constrained by incumbent apparatus dominance — moderate d. The independent measurement coalition has clear exit paths as alternatives mature — low-to-moderate d despite organized status, because exit is becoming available. The key insight: directionality shifts over time as alternatives emerge. The trap quality of external verifiers (d ≈ 0.95) is highest early in the apparatus's dominance; as alternatives accumulate, even trapped agents gain option value (d decreases). This trajectory is captured in the measurements: extractiveness increases (beneficiary securing gains) while theater increases (performative justification intensifies). The analytical observer's mountain perspective is false — emerges_naturally is false, and the apparatus's accessibility_collapse and resistance metrics would fail NL gates if computed.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PROBE: This constraint tests whether physics-washing can disguise pure extraction as coordination. The apparatus claims to be a natural law enforcement mechanism (measuring inherent properties of physical systems). But the structural analysis reveals it as a constructed institutional arrangement: (1) beneficiaries are clearly identified (constructing institution, maintainers); (2) victims are clearly identified (external verifiers, constrained empiricists); (3) active enforcement is required (access control, methodology gatekeeping); (4) alternatives are emerging (independent measurement coalition with scaffold properties). The apparatus would only classify as Mountain if accessibility_collapse ≥ 0.85 and resistance ≤ 0.15 — i.e., if exiting the apparatus were essentially impossible and the constraint were truly irreducible. But accessibility_collapse would be much lower (resistance methods exist, alternatives are emerging) and resistance would be higher (the apparatus is socially resisted). The physics-wash consists of claiming that the apparatus's constraints are natural law constraints. The mandatrophy is resolved by showing that the structural data (beneficiaries, victims, enforcement) classify the apparatus as Tangled Rope at best, Snare at worst — not Mountain. The false natural law detection mechanism activates: a claim to be Mountain fails the NL gates, revealing the naturalization as constructed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apparatus_independence_test,
    'Can the phenomenon claimed by the apparatus be independently verified using structurally distinct measurement methods?',
    'Experimental replications using non-apparatus-derived probes; cross-correlation analysis of apparatus outputs with independent sensor networks; theoretical predictions testable without apparatus infrastructure',
    'If independent verification succeeds: apparatus is coordination mechanism (Rope from most perspectives). If independent verification fails: apparatus is constructed constraint with no external validity (Snare remains dominant classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apparatus_independence_test, empirical, 'Whether claimed phenomena survive verification outside apparatus infrastructure').

omega_variable(
    theory_physics_wash_sufficiency,
    'Does the apparatus''s physics-washed theoretical justification accurately model the apparatus''s actual measurement processes, or does it naturalize constructed arbitrary choices?',
    'Systematic comparison of theoretical predictions from the justification with actual apparatus behavior; identification of unexplained calibration parameters; reverse-engineering the apparatus''s real logic vs the published physics model',
    'If theory matches reality: apparatus has legitimate physics foundations. If theory diverges: apparatus is pure construction with physics-washed legitimacy (extractiveness increases, snare classification becomes dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theory_physics_wash_sufficiency, empirical, 'Whether physics justification matches actual apparatus architecture').

omega_variable(
    alternative_apparatus_viability,
    'How long until alternative apparatus designs achieve cost-parity and methodological credibility with the incumbent apparatus?',
    'Tracking alternative apparatus development timelines; cost analysis and reliability comparisons; journal acceptance rates for alternative apparatus papers vs incumbent apparatus papers over time',
    'If timeline < 3 years: scaffold sunset is imminent, extractiveness declining. If timeline > 15 years: incumbent apparatus maintains extraction power for a generation, piton classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_apparatus_viability, empirical, 'Timeline for alternative apparatus viability and cost-parity').

omega_variable(
    physics_wash_perception_gap,
    'Do field empiricists and external observers actually believe the apparatus has natural law foundations, or do they recognize it as constructed?',
    'Survey and interview data from empiricists assessing apparatus legitimacy; textual analysis of published work using apparatus language; observation of how researchers discuss apparatus necessity vs apparatus choice',
    'If perception is naturalized: FNL mechanism is active, suppression high (0.52 justified or higher). If perception is skeptical: suppression is lower, exit options clearer, snare classification weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(physics_wash_perception_gap, conceptual, 'Whether field perceives apparatus as natural or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fnl_shadow_probe, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fnl_tr_t0, fnl_shadow_probe, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fnl_tr_t3, fnl_shadow_probe, theater_ratio, 3, 0.55).
narrative_ontology:measurement(fnl_tr_t6, fnl_shadow_probe, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(fnl_be_t0, fnl_shadow_probe, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(fnl_be_t3, fnl_shadow_probe, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(fnl_be_t6, fnl_shadow_probe, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fnl_shadow_probe, information_standard).
narrative_ontology:affects_constraint(fnl_shadow_probe, verification_bottleneck).
narrative_ontology:affects_constraint(fnl_shadow_probe, epistemic_commons_externality).

% DUAL FORMULATION NOTE:
% FNL Shadow Mode Probe is a sub-constraint of the broader verification bottleneck in experimental physics. It models a specific failure mode: how institutional control over measurement apparatus can be naturalized as physical necessity. The upstream constraint (verification_bottleneck) is ε=0.40 (mixed coordination/extraction); this downstream constraint (fnl_shadow_probe) is ε=0.38 (apparatus-specific institutional version of the same dynamic). They share the same family of alternatives: independent apparatus, distributed scrutiny, open methodologies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fnl_shadow_probe, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
