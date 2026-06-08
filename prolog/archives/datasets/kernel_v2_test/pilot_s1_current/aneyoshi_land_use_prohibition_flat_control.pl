% ============================================================================
% CONSTRAINT STORY: aneyoshi_land_use_prohibition_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_aneyoshi_land_use_prohibition_flat_control, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_land_use_prohibition_flat_control
 *   human_readable: Aneyoshi Tsunami Stone Land-Use Prohibition (1933)
 *   domain: disaster_anthropology/environmental_governance/temporal_institutional_analysis
 *
 * SUMMARY:
 *   In 1933, the village of Aneyoshi on the Sanriku coast inscribed a
 *   prohibition on stone: 'do not build below this stone line.' The
 *   constraint encodes knowledge of tsunami risk accumulated through
 *   centuries of experience and tragedy. The line marks the approximate
 *   extent of historical tsunami inundation, based on the 1611 and 1896
 *   events. The inscription represents a deliberate institutional choice to
 *   externalize hazard knowledge into a durable material form — a commitment
 *   system anchored in stone rather than in transient institutional memory or
 *   regulatory text. The constraint operates across multiple timescales:
 *   immediately as a land-use rule enforced by local custom and collective
 *   surveillance, and generationally/civilizationally as a transmission
 *   mechanism for knowledge that would otherwise be forgotten between major
 *   tsunami events (separated by 100-150 years). The 2011 Tōhoku tsunami
 *   provided a empirical test: Aneyoshi village, situated above the stone
 *   line, suffered no fatalities from the tsunami despite being on the coast.
 *   Nearby settlements that had abandoned the prohibition suffered casualties
 *   and property loss. This historical validation is the constraint's
 *   strongest evidence for Rope classification: the constraint solves the
 *   real coordination problem of preserving long-timescale hazard knowledge,
 *   and the empirical track record demonstrates its effectiveness.
 *
 * KEY AGENTS:
 *   - Aneyoshi Village Residents (powerless/constrained): Primary beneficiaries and primary agents — bear the coordination benefit of hazard avoidance; face the constraint's land-use restrictions as a cost to mobility and economic settlement patterns.
 *   - Village Council / Collective Authority (institutional/arbitrage): Enforcer and custodian of the constraint — benefits from its legitimacy and its demonstrated effectiveness; responsible for transmitting it across generations.
 *   - Developers / Land Owners (moderate/constrained): Secondary agents bearing extraction — land below the line is restricted from development; face economic loss from constrained access to valuable coastal land.
 *   - Displaced / Landless Persons (powerless/trapped): Tertiary victims — the prohibition blocks settlement on the only economically accessible land; no corresponding benefit accrues to them.
 *   - External Government / Modernization Pressure (institutional/arbitrage): Structural antagonist — post-1950s Japanese economic development and modernization incentivize settlement and development in coastal zones, creating pressure to relax or abandon the prohibition.
 *   - Analytical Observer (analytical/analytical): Seat from which the natural law vs constructed arrangement ambiguity is visible.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_land_use_prohibition_flat_control, 0.15).
domain_priors:suppression_score(aneyoshi_land_use_prohibition_flat_control, 0.08).
domain_priors:theater_ratio(aneyoshi_land_use_prohibition_flat_control, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition_flat_control, extractiveness, 0.15).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_land_use_prohibition_flat_control, theater_ratio, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_land_use_prohibition_flat_control, rope).
narrative_ontology:human_readable(aneyoshi_land_use_prohibition_flat_control, "Aneyoshi Tsunami Stone Land-Use Prohibition (1933)").
narrative_ontology:topic_domain(aneyoshi_land_use_prohibition_flat_control, "disaster_anthropology/environmental_governance/temporal_institutional_analysis").

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(aneyoshi_land_use_prohibition_flat_control, aneyoshi_land_use_prohibition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition_flat_control, village_residents_present_and_future).
narrative_ontology:constraint_beneficiary(aneyoshi_land_use_prohibition_flat_control, aneyoshi_collective_survival).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VILLAGE RESIDENT (ROPE) — The stone constraint solves a genuine coordination problem: how to preserve knowledge of tsunami risk across a 100+ year cycle (longer than individual lifespans). The prohibition coordinates behavior without coercion — residents understand the risk, benefit from collective knowledge, and can exit by relocating (but with real cost). The constraint enables survival.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: VILLAGE COLLECTIVE (ROPE, CIVILIZATIONAL) — From the perspective of intergenerational survival across centuries, the stone inscription is pure coordination: it solves the problem of transmitting hazard knowledge across the memory gap between tsunamis. The constraint persists because it works, not because it extracts. Villages below the line have historically suffered catastrophic loss; villages above have not. The empirical track record is the enforcement mechanism.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, rope,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: DEVELOPER/LAND OWNER (TANGLED ROPE) — The prohibition coordinates safety and settlement, but it also extracts economic value from the constrained agent. Land below the line is cheaper, more accessible for settlement and commerce, but restricted. The developer bears the cost (constrained access to valuable land); the village bears the benefit (collective survival). The constraint has a mixed structure: genuine coordination (hazard avoidance) and asymmetric extraction (land-value asymmetry enforced by the prohibition).
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: VILLAGE COUNCIL (ROPE) — The institution that enforces the prohibition (or did so traditionally) sees it as a coordination mechanism with high perceived benefit. The council's authority is grounded in the constraint's effectiveness: the village that follows the stone's guidance survives; the village that ignores it does not. The council benefits from the constraint's legitimacy but also bears the burden of enforcement and collective defense of the prohibition against outside pressure.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: DISPLACED/LANDLESS PERSON (SNARE) — From the immediate perspective of an agent with no land holdings and the prohibition blocking settlement on cheaper ground, the constraint appears as pure restriction with no corresponding benefit. The agent cannot exit (trapped by poverty; relocation is not feasible). The prohibition prevents settlement on the only available land. No coordination benefit accrues to this agent; only extraction (exclusion from livable space).
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN, NATURAL LAW) — From a sufficiently abstract perspective, the prohibition appears as a recognition of an immutable physical constraint: tsunami risk is a fact of geography; the stone line marks the historical limit of wave penetration. Respecting this line is not a social convention but deference to physical law. The analytical observer at civilizational scale risks naturalizing what is actually a contingent institutional arrangement — the stone line is ONE way of encoding the knowledge; it could be archived in databases, infrastructure codes, or cultural memory. But the immutability claim is tempting.
constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(aneyoshi_land_use_prohibition_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(aneyoshi_land_use_prohibition_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(aneyoshi_land_use_prohibition_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The primary function of the constraint is coordination — preserving hazard knowledge across generational timescales. The extraction that does occur (land-value asymmetry favoring land-holders above the line) is a secondary effect of the coordination mechanism, not its primary purpose. The 1933 measurement reflects the constraint's origins as a deliberate safety coordination mechanism with minimal rent-seeking. By 1993, extractiveness rose to 0.18 as Japanese economic development intensified pressure on coastal land — developers became aware of the restriction as an asymmetric cost. Post-2011, extractiveness remained moderate (0.15) because the tsunami validated the prohibition, reinforcing its legitimacy and reducing developer pressure. Suppression (0.08, rising to 0.12 by 1993): Low overall. The prohibition is enforced through cultural norms and collective surveillance rather than coercive state power. Residents understand the risk and generally comply voluntarily. Suppression rises during the 1950-1995 period as modernization pressure and external incentives to violate the prohibition increase, but the suppression remains moderate because exit (relocation to inland towns) is technically available at the cost of economic opportunity. Theater ratio (0.10→0.28→0.15): Low initially, rising during the calm period (60-90 years post-1933, when tsunami memory was weak and the prohibition was maintained largely through ritual respect rather than lived fear), then falling sharply post-2011 (the tsunami revalidated the constraint's functional purpose, reducing the performative component).
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap appears between the village resident/collective (Rope: the constraint solves a genuine coordination problem) and the landless/displaced person (Snare: the constraint blocks access to livable space with no corresponding benefit). Both perspectives are structurally accurate — they describe different directionalities in the same constraint. The developer (Tangled Rope) occupies an intermediate position: the constraint coordinates safety (benefit to all) but extracts land-value asymmetry (cost to the developer specifically). The analytical observer's Mountain perspective risks naturalizing the constraint as an immutable law of physics (tsunami risk is real, the stone line marks it) without recognizing that the constraint's persistence depends on cultural transmission, institutional commitment, and the periodic revalidation provided by actual tsunami events. This perspectival gap between natural law and constructed arrangement is the most theoretically significant — the constraint is both (it recognizes a real hazard) and neither (its persistence depends on human choices about inscription, transmission, and enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from the agent's structural relationship to the extraction flow. Beneficiaries (village residents who benefit from coordination) experience low d (subsidy — the constraint protects them). Victims (displaced persons blocked from building land) experience high d (target — extraction flows toward the constraint-enforcer from their exclusion). Intermediate agents (developers, land-owners) experience moderate d. The village collective experiences arbitrage-level exit (can theoretically abandon the prohibition, but only at unacceptable cost — loss of collective safety knowledge and historical legitimacy). The analytical observer's d is derived from their ability to perceive the full structure without being embedded in any particular extractive relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy does NOT resolve as a problem in this constraint. The constraint's mandate (preserve tsunami hazard knowledge across generations) remains live and functionally intact. The 2011 empirical validation confirmed that the prohibition works: Aneyoshi survived when other coastal villages did not. Unlike many institutional constraints that outlive their original purpose and persist through theater and inertia (Piton), this constraint's function is demonstrated regularly and catastrophically. The risk of mandatrophy erosion exists (the 60-90 year calm period elevated theater ratio as cultural memory faded), but the constraint has been revalidated rather than rendered obsolete. The analytical observer's temptation to classify as Mountain (natural law) is the only serious misclassification risk, and it is not mandatrophy — it is a perspectival failure to recognize constructed institutional arrangements as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intergenerational_knowledge_persistence,
    'Does the physical inscription (''stone line'') alone preserve hazard knowledge across generations, or does it require accompanying cultural transmission and institutional reinforcement?',
    'Historical case analysis: compare constraint effectiveness in villages that maintained active cultural transmission + stone marker vs villages that lost cultural context but retained the stone. Measure rates of violation and post-tsunami mortality in each cohort.',
    'If stone alone sufficient: constraint is a robust technological coordination mechanism (Rope classification stable across time horizons). If cultural transmission required: constraint is contingent on institutional fragility (Rope at generational, but degrading to Snare or Piton at longer horizons if cultural memory decays). Classification shifts with discovery.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_knowledge_persistence, empirical, 'Whether stone inscription alone preserves knowledge or requires cultural transmission').

omega_variable(
    tsunami_cycle_phase_ambiguity,
    'Is the prohibition''s extractiveness constant across the tsunami cycle, or does it oscillate between coordination (post-disaster, when memory is fresh) and extraction (in the 50+ years of calm before the next event)?',
    'Temporal measurement of settlement patterns, enforcement intensity, and local narrative about the stone across documented phases of the tsunami cycle. Aneyoshi recorded ~140-year intervals between major tsunamis (1611, 1751, 1896). Track constraint interpretation in years 0-20 post-tsunami (memory hot) vs years 100-120 (memory cool).',
    'If oscillating: measurements at time-point selection become critical (constraint appears as Rope or Snare depending on phase). Theater ratio will be low post-event (stone reinforces urgent memory) and high pre-event (performative respect without urgency). Piton interpretation may emerge in the pre-event phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tsunami_cycle_phase_ambiguity, empirical, 'Oscillation of constraint extractiveness across tsunami cycle phases').

omega_variable(
    external_pressure_and_norms_erosion,
    'To what degree does the constraint''s integrity depend on cultural consensus about tsunami risk, and to what degree does external pressure (development incentives, population migration, economic modernization) erode the normative force of the prohibition?',
    'Comparison of historical violation rates before and after Japanese economic modernization (1950s onward). Analysis of settlement patterns in Aneyoshi vs comparable coastal villages with different institutional or cultural constraints on development. Post-2011 tsunami behavior as a test: did Aneyoshi residents'' survival track match the stone line prediction?',
    'If external pressure dominant: constraint effectively becomes a Snare for developers and a weak Rope for residents who lack alternative livable space. Suppression metric may increase over time. If cultural consensus robust: constraint remains Rope despite external pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_pressure_and_norms_erosion, empirical, 'Role of external pressure vs cultural consensus in constraint persistence').

omega_variable(
    false_summit_natural_law_candidate,
    'Is the prohibition a recognition of a genuine natural law (immutable tsunami risk marked by stone), or is it a constructed institutional arrangement that benefits the village collective at the expense of landless and displaced populations?',
    'Beneficiary analysis: Does the prohibition primarily prevent tragedy (collective benefit) or does it also enable land scarcity and price asymmetries that benefit land-holders at the expense of displaced persons? Case studies of intra-village distribution of land above and below the line; demographic analysis of who settled where and under what constraints.',
    'If pure natural law: Mountain classification confirmed; no beneficiary/victim structure. If constructed institutional arrangement with concentrated benefits: FSM detection triggers; reclassifies to Tangled Rope or Snare depending on suppression mechanics. The dual nature of the prohibition (safety rule + land-use control) generates ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_natural_law_candidate, conceptual, 'Whether prohibition is natural law deference or constructed institutional arrangement with concentrated benefits').

omega_variable(
    post_tsunami_knowledge_accuracy,
    'Does the stone line''s historical placement match the actual maximum tsunami extent documented in the 2011 Tōhoku tsunami and other recent events, or does it represent a conservative (overly cautious) or loose (insufficiently cautious) estimate?',
    'Comparative measurement: overlay the 1933 stone line against GIS data of 2011 tsunami inundation limits in Aneyoshi and nearby communities. Cross-reference with historical records of 1896, 1751, and 1611 tsunami extents where available. Measure discrepancy magnitude.',
    'If stone line conservative (below actual max extent): constraint is robust, extractiveness is justified safety cost, Rope classification holds. If stone line loose (above actual max extent): constraint may be over-protective, extractiveness becomes potentially unjustified economic cost, Snare classification becomes more likely. If accuracy varies by event: the stone line encodes knowledge of one or two specific tsunami events, not a general principle, complicating generalization claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(post_tsunami_knowledge_accuracy, empirical, 'Historical accuracy of stone line placement vs documented tsunami extents').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_land_use_prohibition_flat_control, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aneyoshi_theater_1933, aneyoshi_land_use_prohibition_flat_control, theater_ratio, 0, 0.1).
narrative_ontology:measurement(aneyoshi_theater_1963, aneyoshi_land_use_prohibition_flat_control, theater_ratio, 30, 0.18).
narrative_ontology:measurement(aneyoshi_theater_1993, aneyoshi_land_use_prohibition_flat_control, theater_ratio, 60, 0.28).
narrative_ontology:measurement(aneyoshi_theater_2011_post, aneyoshi_land_use_prohibition_flat_control, theater_ratio, 78, 0.15).
narrative_ontology:measurement(aneyoshi_theater_2023, aneyoshi_land_use_prohibition_flat_control, theater_ratio, 90, 0.22).

% Extraction over time
narrative_ontology:measurement(aneyoshi_extractiveness_1933, aneyoshi_land_use_prohibition_flat_control, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(aneyoshi_extractiveness_1963, aneyoshi_land_use_prohibition_flat_control, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(aneyoshi_extractiveness_1993, aneyoshi_land_use_prohibition_flat_control, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(aneyoshi_extractiveness_2011_post, aneyoshi_land_use_prohibition_flat_control, base_extractiveness, 78, 0.15).
narrative_ontology:measurement(aneyoshi_extractiveness_2023, aneyoshi_land_use_prohibition_flat_control, base_extractiveness, 90, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(aneyoshi_suppression_1933, aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(aneyoshi_suppression_1963, aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 30, 0.08).
narrative_ontology:measurement(aneyoshi_suppression_1993, aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 60, 0.12).
narrative_ontology:measurement(aneyoshi_suppression_2011_post, aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 78, 0.08).
narrative_ontology:measurement(aneyoshi_suppression_2023, aneyoshi_land_use_prohibition_flat_control, suppression_requirement, 90, 0.09).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_land_use_prohibition_flat_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(aneyoshi_land_use_prohibition_flat_control, 0.12).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition_flat_control, japanese_coastal_zoning_development_pressure).
narrative_ontology:affects_constraint(aneyoshi_land_use_prohibition_flat_control, intergenerational_hazard_memory_persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
