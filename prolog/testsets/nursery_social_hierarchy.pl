% ============================================================================
% CONSTRAINT STORY: nursery_social_hierarchy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nursery_social_hierarchy, []).

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
 *   constraint_id: nursery_social_hierarchy
 *   human_readable: Social Hierarchy of Playthings in the Nursery
 *   domain: social/economic/developmental
 *
 * SUMMARY:
 *   The nursery social hierarchy is a status framework in which mechanical
 *   complexity is equated with developmental advancement, educational value,
 *   and modern sophistication. This constraint systematically extracts
 *   dignity and play authority from simple toys (wood, cloth, natural
 *   materials, open-ended forms) in order to fuel the cultural and economic
 *   dominance of complex, predetermined-function, technology-intensive
 *   playthings. The mechanism is simultaneously a coordination system
 *   (manufacturers solving the problem of communicating product value to
 *   aspirant parents) and an extraction system (simple-toy makers and
 *   imaginative-play advocates losing cultural standing and market share).
 *   The constraint exhibits all six DR types from different structural
 *   positions, making it a rich exemplar of how a single institutional
 *   arrangement can appear simultaneously as coordination, temporary
 *   scaffolding, degraded ritual, pure extraction, and hybrid
 *   extraction-coordination, depending on the observer's power, exit options,
 *   and temporal horizon. Theater ratio (0.68) reflects that much of the
 *   status-assignment work is performative: 'educational value' claims are
 *   largely unverified, developmental psychology literature often contradicts
 *   the marketing discourse, and the complex toy's perceived superiority
 *   persists despite research showing simple toys and parental engagement
 *   predict better outcomes.
 *
 * KEY AGENTS:
 *   - Manufacturers of Complex Toys: Primary beneficiary (institutional/arbitrage) — captures market share, premium pricing, cultural authority through 'educational' framing
 *   - Simple Toys and Their Makers: Primary victim (powerless/trapped) — lose market share, cultural status, and play authority; cannot exit hierarchy
 *   - Parents Aspiring Upward: Secondary beneficiary (powerful/mobile) — use toy purchases as status signaling and developmental insurance; have exit options but constrained by peer culture
 *   - Children: Secondary victim (moderate/constrained) — experience both genuine play enablement from complex toys and extraction pressure to prefer complexity over imagination
 *   - Imaginative Capacity: Victim (powerless/trapped) — abstract cultural good that cannot organize or defend itself; bears cost of suppressed open-ended play
 *   - Toy Preservation Movement: Organized agent (organized/constrained) — building alternative hierarchies and exit pathways; sees scaffold sunset
 *   - Developmental Psychology Establishment: Institutional degraded actor (institutional/arbitrage) — maintains complexity-as-value discourse through inertia despite contradictory research; piton classification
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing recent consumer-culture arrangement as inherent human cognition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nursery_social_hierarchy, 0.38).
domain_priors:suppression_score(nursery_social_hierarchy, 0.52).
domain_priors:theater_ratio(nursery_social_hierarchy, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nursery_social_hierarchy, extractiveness, 0.38).
narrative_ontology:constraint_metric(nursery_social_hierarchy, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(nursery_social_hierarchy, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nursery_social_hierarchy, tangled_rope).
narrative_ontology:human_readable(nursery_social_hierarchy, "Social Hierarchy of Playthings in the Nursery").
narrative_ontology:topic_domain(nursery_social_hierarchy, "social/economic/developmental").

domain_priors:requires_active_enforcement(nursery_social_hierarchy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, manufacturers_of_complex_toys).
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, parents_aspiring_upward).
narrative_ontology:constraint_beneficiary(nursery_social_hierarchy, marketing_systems).
narrative_ontology:constraint_victim(nursery_social_hierarchy, simple_toys_and_makers).
narrative_ontology:constraint_victim(nursery_social_hierarchy, children_seeking_authentic_play).
narrative_ontology:constraint_victim(nursery_social_hierarchy, imaginative_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SIMPLE TOY (SNARE) — A wooden block, a rag doll, a painted stick. Cannot exit the hierarchy; bears full extraction of dignity and play value. The simple toy is trapped in a system where its very simplicity — once its virtue — is now read as obsolescence and inferiority. Maximum experienced extraction. No advocacy, no counter-narrative available. The toy is used less, valued less, and culturally repositioned as a 'primitive predecessor' to the complex object.
constraint_indexing:constraint_classification(nursery_social_hierarchy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CHILD INHABITANT (TANGLED ROPE) — The child is told that complex toys teach more, develop faster, and signal intelligence. Constrained by parental purchasing decisions and peer culture. But also benefits from the toys that are provided — modern toys do offer some genuine play affordance and stimulation. The child experiences both the extraction (pressure to prefer complex over simple) and genuine coordination (the toy system does enable some forms of play). Mixed experience: asymmetric constraint but not total domination.
constraint_indexing:constraint_classification(nursery_social_hierarchy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE MANUFACTURER OF COMPLEX TOYS (ROPE) — Benefits from the hierarchy through increased demand, premium pricing, and cultural authority. Can arbitrage: they can shift production, licensing, and marketing to respond to market signals. The hierarchy appears to them as a coordination mechanism — they are coordinating parent aspirations with child development discourse to enable efficient production and distribution. Net beneficiary with exit options (arbitrage). Experiences the constraint as cooperative, not coercive.
constraint_indexing:constraint_classification(nursery_social_hierarchy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE TOY PRESERVATION MOVEMENT (SCAFFOLD) — Organized agents (Waldorf schools, toy libraries, slow childhood advocates, craftspeople) see the hierarchy as a temporary institutional distortion with a sunset: as research on play psychology accumulates, as parents recover memory of their own open-ended play, as digital fatigue becomes visible, alternative hierarchies (valuing imagination over complexity) are being reconstructed. Low effective extraction because these organized agents have agency and see an exit path. Has sunset clause: a generational recalibration away from complexity as status marker toward play richness, sensory engagement, and imaginative openness.
constraint_indexing:constraint_classification(nursery_social_hierarchy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE DEVELOPMENTAL PSYCHOLOGY ESTABLISHMENT (PITON) — The scientific literature on play has largely moved away from measuring 'complexity' as a direct proxy for developmental benefit — research shows open-ended materials outperform predetermined-function toys for learning outcomes. Yet the institutional discourse continues to cite complexity as a marker of educational quality. This is largely performative: the academy publishes papers showing simple-toy benefits but continues to credential manufacturers of complex toys, organize industry partnerships, and fund research through toy-industry sources. The piton persists through institutional inertia and funding incentives, not because the underlying science supports it. Theater ratio high.
constraint_indexing:constraint_classification(nursery_social_hierarchy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: THE ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational/universal perspective, the hierarchy might appear as a natural law: humans always sort objects by perceived sophistication, and mechanical complexity has always signaled status in cultures of abundance. This perspective sees the hierarchy as emerging naturally from human cognition and economic evolution. However, this risks naturalizing a contingent institutional arrangement — the historical record shows that simple toys held high cultural value for millennia, and their demotion is recent and specific to late-capitalist consumer culture. The 'natural law' framing conceals the extractive machinery.
constraint_indexing:constraint_classification(nursery_social_hierarchy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nursery_social_hierarchy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nursery_social_hierarchy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(nursery_social_hierarchy, TR),
    TR >= 0.70.

:- end_tests(nursery_social_hierarchy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does extract dignity and market share from simple toys, but the extraction is not total — simple toys retain some cultural value in certain subcultures (Waldorf, montessori, toy libraries), and parents retain agency in purchasing decisions. Much of what looks like extraction is partially legitimate coordination (complex toys do offer some genuine affordance). The value has risen from 0.22 to 0.38 over the interval, reflecting increasing marketing sophistication and cultural consolidation of the complexity-as-value norm. Suppression (0.52): Moderate-high. Significant barriers to reversing the hierarchy include: parental status anxiety, marketing investment in complexity framing, absence of simple-toy cultural narratives, peer conformity pressure, and institutional capture of developmental psychology discourse. But suppression is not total — organized movements exist and research contradicts the hierarchy. Theater ratio (0.68): High and rising. The 'educational value' claims attached to complex toys are substantially performative — the evidence base is weak or contradictory, yet the claims persist and intensify. Manufacturers invest heavily in theater (expert endorsements, developmental psychology language, research-adjacent marketing) that does not map to actual play outcomes. The theater ratio has increased from 0.42 to 0.68 over the interval as marketing sophistication has increased and as the psychological research base has become more available yet is increasingly ignored in public discourse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence from a single set of base properties. The manufacturers see a rope (coordination of parent aspirations with product communication). The toy preservation movement sees a scaffold (temporary institutional distortion being addressed through generational recalibration and alternative hierarchies). The developmental psychology establishment sees a piton (performative continuation of the complexity-as-value narrative despite contradictory research). Simple toys see a snare (trapped in a system that extracts their dignity with no exit). Children see tangled rope (both enabled and constrained by the hierarchy). The analytical observer risks seeing a mountain (naturalizing complexity-status as inherent human cognition) — but the structural data reveals this as a false summit. The perspectival gap is maximal: the beneficiary (manufacturers) and victims (simple toys, imaginative capacity) inhabit incommensurable classifications derived from the same structural constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to the constraint. Manufacturers of complex toys are institutional beneficiaries with arbitrage options — they experience low or negative directionality (d ~0.10), producing low experienced extractiveness chi through the beneficiary-arbitrage path. Simple toys are powerless victims with trapped exit — high d (~0.95) producing high f(d) and high experienced chi. Parents aspiring upward are powerful beneficiaries with constrained exit (mobile in some dimensions but constrained by peer/status pressure) — moderate d (~0.35) producing moderate chi. Children are moderate victims with constrained exit — d (~0.65) producing elevated chi but not maximum. The developmental psychology establishment is an institutional actor with arbitrage exit but captured by industry incentives — directionality override to d=0.35 captures the partially-captured institutional position. The toy preservation movement is organized with constrained exit (ideologically committed but resource-limited) — d ~0.55 producing moderate chi that matches the scaffold experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing that the apparent 'coordination' (manufacturers helping parents identify developmentally-valuable toys) and the apparent 'extraction' (simple toys losing market share and cultural standing) are simultaneous and structural. The constraint is genuinely a tangled rope: it performs real coordination work (manufacturers do communicate product information, which solves some informational problem for parents) while simultaneously extracting dignity and market value from competing toy frameworks. The mandatrophy is resolved by showing that both functions are real, that the coordination benefit flows primarily to manufacturers and aspirant parents while the extraction cost falls on simple-toy traditions and open-ended play culture, and that the asymmetry is maintained through active enforcement (marketing, peer pressure, discourse capture). The piton perspective on the developmental psychology establishment confirms the entanglement: the same institution that could correct the extraction through research dissemination is itself captured and maintaining performative validation of the hierarchy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_development_causation,
    'Does mechanical complexity in toys causally enhance child development, or is the correlation driven by parental investment level and educated parental engagement?',
    'Longitudinal controlled studies comparing simple vs complex toys with parental engagement held constant; analysis of confounding variables in existing literature',
    'If causation confirmed: complexity-as-status is justified. If parental engagement is primary driver: the hierarchy is pure marketing capture, extractiveness rises to 0.65+, classification shifts toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complexity_development_causation, empirical, 'Whether toy complexity drives development or parental investment does').

omega_variable(
    imagination_suppression_mechanism,
    'Do predetermined-function toys (with complete narrative/functionality) directly suppress imaginative capacity, or does suppression require repeated exposure and peer status pressure?',
    'Experimental design comparing single-toy vs multi-toy environments, open-ended vs predetermined functions, with imagination-task assessment; longitudinal tracking of imaginative output',
    'If single exposure suppresses: suppression value stays ~0.52. If only repeated/pressured exposure suppresses: the constraint requires active enforcement to maintain, confirming tangled_rope classification. If no suppression mechanism found: toys have genuinely low conflict, classification shifts to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imagination_suppression_mechanism, empirical, 'Mechanism of imagination suppression by predetermined-function toys').

omega_variable(
    slow_childhood_adoption_rate,
    'What adoption rate of open-ended play frameworks (toy libraries, Waldorf schools, unstructured-play mandates) would constitute a genuine sunset, vs what rate represents token cultural artifact?',
    'Market analysis of toy sales by category; education system curriculum analysis; parental choice data in comparison geographies; measurement of symbolic vs actual adoption',
    'If adoption reaches 30%+ in representative regions: scaffold sunset is structural, theater_ratio begins declining. If adoption plateaus < 10%: scaffold is aspirational, constraint persists indefinitely, extractiveness may increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slow_childhood_adoption_rate, empirical, 'Critical adoption threshold for genuine open-ended play movement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nursery_social_hierarchy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nursery_tr_t0, nursery_social_hierarchy, theater_ratio, 0, 0.42).
narrative_ontology:measurement(nursery_tr_t5, nursery_social_hierarchy, theater_ratio, 5, 0.58).
narrative_ontology:measurement(nursery_tr_t10, nursery_social_hierarchy, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(nursery_be_t0, nursery_social_hierarchy, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(nursery_be_t5, nursery_social_hierarchy, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(nursery_be_t10, nursery_social_hierarchy, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nursery_social_hierarchy, information_standard).
narrative_ontology:affects_constraint(nursery_social_hierarchy, consumer_status_signaling).
narrative_ontology:affects_constraint(nursery_social_hierarchy, parental_anxiety_systems).
narrative_ontology:affects_constraint(nursery_social_hierarchy, toy_industry_consolidation).

% DUAL FORMULATION NOTE:
% The nursery social hierarchy is downstream of broader status-signaling systems (consumer culture, parental anxiety about child development) but represents a distinct structural constraint operating within the toy domain. The upstream constraints have their own extractiveness values reflecting general consumer culture; the nursery hierarchy has its own extractiveness reflecting the specific educational-value framing applied to toys.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nursery_social_hierarchy, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
