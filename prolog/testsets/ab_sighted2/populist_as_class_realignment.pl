% ============================================================================
% CONSTRAINT STORY: populist_as_class_realignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_populist_as_class_realignment, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: populist_as_class_realignment
 *   human_readable: Populist Realignment as Education-Based Class Restructuring
 *   domain: political_economy/comparative_politics/democratic_theory
 *
 * SUMMARY:
 *   The populist realignment represents a fundamental restructuring of class
 *   politics in post-industrial democracies, driven by education-based
 *   stratification that has fractured traditional working-class solidarity.
 *   Beginning in the 1980s and accelerating through the 2000s, working-class
 *   voters without college degrees have shifted from left-of-center parties
 *   (which historically mobilized them around economic redistribution) to
 *   right-wing populist parties (which mobilize them around cultural
 *   protection and anti-elite framing). This constraint exhibits tangled rope
 *   characteristics: it coordinates some voters (cultural protection
 *   coalitions) while extracting from others (traditional labor unions and
 *   redistributive policy frameworks). The realignment is not a natural law
 *   but a contingent outcome of post-industrial spatial extraction, media
 *   fragmentation, declining union density, and elite strategic choices. The
 *   theater_ratio (0.52) reflects that much populist mobilization is
 *   performative — cultural grievance rhetoric that does not translate into
 *   material policy gains for working-class voters. The constraint's
 *   extractiveness has increased over the 30-year interval as the education
 *   premium has widened and traditional labor institutions have weakened,
 *   making exit from the cultural framing increasingly difficult for
 *   identity-locked voters.
 *
 * KEY AGENTS:
 *   - Displaced Industrial Worker: Primary victim (powerless/identity_locked) — identity reconstituted through cultural grievance; cannot exit the realignment from within the identity frame
 *   - Traditional Union Member: Secondary victim (moderate/constrained) — declining collective bargaining power; political voice redirected from economic to cultural issues
 *   - Right-Wing Populist Party: Primary beneficiary (institutional/arbitrage) — captures working-class votes by reframing class conflict as cultural conflict
 *   - Social Democratic Party Coalition: Organized victim (organized/mobile) — lost traditional base; faces electoral collapse if exits to centrist coalitions
 *   - Progressive Coalition Builders: Organized agents (organized/mobile) — building cross-class coalitions around universal programs; see sunset path through policy reframing
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination (rational response to post-industrial change) and asymmetric extraction (cultural framing suppresses economic redistribution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(populist_as_class_realignment, 0.38).
domain_priors:suppression_score(populist_as_class_realignment, 0.48).
domain_priors:theater_ratio(populist_as_class_realignment, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(populist_as_class_realignment, extractiveness, 0.38).
narrative_ontology:constraint_metric(populist_as_class_realignment, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(populist_as_class_realignment, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(populist_as_class_realignment, tangled_rope).
narrative_ontology:human_readable(populist_as_class_realignment, "Populist Realignment as Education-Based Class Restructuring").
narrative_ontology:topic_domain(populist_as_class_realignment, "political_economy/comparative_politics/democratic_theory").

domain_priors:requires_active_enforcement(populist_as_class_realignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, right_wing_populist_parties).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, cultural_protection_coalitions).
narrative_ontology:constraint_beneficiary(populist_as_class_realignment, anti_establishment_media).
narrative_ontology:constraint_victim(populist_as_class_realignment, social_democratic_welfare_coalitions).
narrative_ontology:constraint_victim(populist_as_class_realignment, traditional_labor_unions).
narrative_ontology:constraint_victim(populist_as_class_realignment, redistributive_policy_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED INDUSTRIAL WORKER (SNARE) — Identity-locked into cultural protection framing after economic redistribution failed to materialize. Cannot exit the realignment because their working-class identity has been reconstituted through cultural grievance rather than economic solidarity. The shift from economic to cultural framing is experienced as natural rather than constructed, making the extraction mechanism invisible from within the identity frame.
constraint_indexing:constraint_classification(populist_as_class_realignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL UNION MEMBER (TANGLED ROPE) — Constrained by declining union density and weakened collective bargaining power, but still benefits from residual labor protections and social insurance. Experiences both coordination (unions still provide some workplace representation) and extraction (political voice redirected from economic redistribution to cultural issues). Can see the trade-off but faces high costs to exit either the union structure or the cultural realignment.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RIGHT-WING POPULIST PARTY (ROPE) — Primary beneficiary. Captures working-class votes by reframing class conflict as cultural conflict (native vs immigrant, cosmopolitan vs rooted). Experiences the realignment as pure coordination: solving the collective action problem of mobilizing voters who feel abandoned by traditional left parties. Can arbitrage between cultural and economic appeals as electoral strategy requires.
constraint_indexing:constraint_classification(populist_as_class_realignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: SOCIAL DEMOCRATIC PARTY COALITION (TANGLED ROPE) — Organized but mobile agents (party leadership, policy intellectuals) see the realignment as both coordination failure (lost working-class base) and extraction (cultural issues suppress economic redistribution). Can exit to centrist or green coalitions but face electoral collapse if they do. The constraint coordinates some voters (educated urban progressives) while extracting from the traditional base.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: PROGRESSIVE COALITION BUILDERS (SCAFFOLD) — Organized agents (new labor movements, climate justice coalitions, intersectional organizing) see the education-based realignment as a temporary fracture with a sunset: building cross-class coalitions around universal programs (healthcare, climate, housing) can reunite working-class voters across education levels. Low effective extraction because they have agency and see an exit path through policy reframing.
constraint_indexing:constraint_classification(populist_as_class_realignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the realignment exhibits both genuine coordination (working-class voters are responding rationally to post-industrial spatial extraction and cultural dislocation) and asymmetric extraction (cultural framing suppresses economic redistribution that would benefit the working class materially). The constraint is not a natural law but a contingent institutional arrangement shaped by education-based stratification, media fragmentation, and the collapse of traditional labor institutions.
constraint_indexing:constraint_classification(populist_as_class_realignment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(populist_as_class_realignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(populist_as_class_realignment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(populist_as_class_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-low. The realignment extracts from social democratic welfare coalitions and traditional labor unions by redirecting working-class political energy from economic redistribution to cultural protection. However, the extraction is not as severe as a pure snare because some working-class voters do experience genuine coordination through cultural protection coalitions, and the realignment is partly a rational response to post-industrial spatial extraction (the upstream constraint). The value reflects that the career and policy asymmetry is real but not total. Suppression (0.48): Moderate. Significant barriers to exit include identity fusion with cultural protection framing, media fragmentation that reinforces cultural grievance, declining union density that weakens alternative mobilization structures, and the education premium that makes cross-class solidarity harder to sustain. But suppression is not total — some voters do switch back, and progressive coalition builders are creating alternative pathways. Theater ratio (0.52): Moderate. Much populist mobilization is performative: cultural grievance rhetoric, anti-elite posturing, and symbolic policy (border walls, immigration restrictions) that do not translate into material gains for working-class voters (wage growth, healthcare access, housing affordability). The theater has increased over the interval as populist parties have consolidated power but delivered limited economic redistribution.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — education-based class restructuring — appears as snare (from the identity-locked displaced worker), tangled rope (from the constrained union member and the organized social democratic coalition), rope (from the beneficiary populist party), scaffold (from the progressive coalition builders who see a sunset), and tangled rope (from the analytical observer who sees both coordination and extraction). The displaced worker cannot see the extraction from within the cultural identity frame. The populist party sees only coordination (mobilizing abandoned voters). The social democratic coalition sees both coordination failure (lost base) and extraction (cultural issues suppress redistribution). The progressive coalition builders see a temporary fracture with a policy-driven sunset. The analytical observer sees a contingent institutional arrangement shaped by post-industrial spatial extraction, not a natural law of democratic politics.
 *
 * DIRECTIONALITY LOGIC:
 *   The displaced industrial worker is identity_locked rather than trapped because the binding mechanism is cognitive (identity reconstituted through cultural framing) rather than material (though material barriers also exist). The worker could structurally exit (vote for a different party, join a different coalition) but cannot do so from within the identity frame that constitutes their self-concept as a culturally rooted, anti-elite, native worker. This produces high d (victim + identity_locked) but not maximum d (victim + trapped). The right-wing populist party is the primary beneficiary with arbitrage exit options, producing low d and negative effective extraction. The social democratic coalition is organized but experiences the constraint as extraction (lost base) despite having mobile exit options, producing moderate d. The progressive coalition builders are organized with mobile exit and see a sunset path, producing low-moderate d. The analytical observer derives d from the canonical analytical fallback (0.73) because no explicit beneficiary/victim override applies at the analytical context.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by demonstrating both genuine coordination (working-class voters are responding rationally to post-industrial dislocation and cultural change) and asymmetric extraction (cultural framing suppresses economic redistribution that would materially benefit the working class). The coordination function is real: right-wing populist parties are solving a collective action problem for voters who feel abandoned by traditional left parties. The extraction is also real: the shift from economic to cultural framing redirects political energy away from policies (universal healthcare, wage growth, housing affordability) that would benefit working-class voters across education levels. The constraint is not mislabeled coordination (it genuinely coordinates cultural protection coalitions) and not mislabeled extraction (it genuinely extracts from redistributive policy frameworks). It is both, which is the structural signature of tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    education_stratification_threshold,
    'At what level of education-based income divergence does class identity fracture from economic to cultural axes?',
    'Cross-national panel analysis of education premium, union density, and populist vote share; identification of threshold effects in party realignment timing',
    'If threshold is low (education premium < 1.5x): realignment is driven by elite framing rather than material conditions. If threshold is high (> 2.5x): realignment is structural response to genuine class divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(education_stratification_threshold, empirical, 'Education premium threshold for class identity fracture').

omega_variable(
    cultural_framing_reversibility,
    'Can working-class voters who have shifted to cultural protection framing be re-mobilized around economic redistribution, or is the identity lock permanent?',
    'Longitudinal voter panel data tracking issue salience and party switching; natural experiments from policy shocks (universal programs, economic crises)',
    'If reversible: scaffold perspective confirmed — realignment is temporary coordination failure. If permanent: identity_locked perspective confirmed — cultural framing has reconstituted class identity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_framing_reversibility, empirical, 'Whether cultural identity lock is reversible').

omega_variable(
    elite_coordination_intentionality,
    'Is the education-based realignment an emergent outcome of post-industrial change, or a coordinated elite strategy to suppress economic redistribution?',
    'Historical analysis of party strategy documents, media ownership concentration, and policy platform evolution; identification of coordinated vs independent shifts',
    'If emergent: lower extractiveness (0.25-0.35 range), closer to rope from more perspectives. If coordinated: higher extractiveness (0.45-0.55 range), closer to snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_coordination_intentionality, conceptual, 'Whether realignment is emergent or coordinated').

omega_variable(
    universal_program_coalition_viability,
    'Can universal programs (Medicare for All, Green New Deal, universal childcare) actually reunite working-class voters across education levels, or do cultural divisions override material interests?',
    'Survey experiments on policy support conditional on framing; electoral outcomes in jurisdictions that implemented universal programs; cross-national comparison of welfare state generosity and populist vote share',
    'If viable: scaffold sunset is real — policy can reverse realignment. If not viable: cultural divisions are structural, and the realignment is stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universal_program_coalition_viability, empirical, 'Whether universal programs can bridge education-based class divide').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(populist_as_class_realignment, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pop_realign_theater_1980, populist_as_class_realignment, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pop_realign_theater_1995, populist_as_class_realignment, theater_ratio, 15, 0.44).
narrative_ontology:measurement(pop_realign_theater_2010, populist_as_class_realignment, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(pop_realign_extract_1980, populist_as_class_realignment, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(pop_realign_extract_1995, populist_as_class_realignment, base_extractiveness, 15, 0.3).
narrative_ontology:measurement(pop_realign_extract_2010, populist_as_class_realignment, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(pop_realign_suppress_1980, populist_as_class_realignment, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(pop_realign_suppress_1995, populist_as_class_realignment, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(pop_realign_suppress_2010, populist_as_class_realignment, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(populist_as_class_realignment, identity_coordination).

% DUAL FORMULATION NOTE:
% The populist realignment is downstream of post_industrial_spatial_extraction (the upstream mountain constraint that describes geographic concentration of economic opportunity in educated urban centers). The spatial extraction creates the material conditions (declining industrial towns, widening education premium) that make cultural protection framing appealing to working-class voters. The realignment is a distinct constraint with its own extractiveness reflecting the political-institutional dynamics, not merely a restatement of the spatial extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
