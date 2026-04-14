% ============================================================================
% CONSTRAINT STORY: colossus_nero_inertia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_colossus_nero_inertia, []).

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
 *   constraint_id: colossus_nero_inertia
 *   human_readable: The Political and Physical Inertia of the Colossus of Nero
 *   domain: political/architectural/social
 *
 * SUMMARY:
 *   The Colossus of Nero (constructed 64-68 CE) was a 30-meter bronze statue
 *   dominating the Roman Forum and surrounding landscape. It functioned
 *   simultaneously as imperial legitimacy infrastructure, architectural
 *   achievement, public land enclosure, and instrument of psychological
 *   subordination. After Nero's death (68 CE), the statue entered a
 *   structural crisis: successor regimes could not easily remove it (high
 *   technical cost, political significance of Bronze), could not leave it
 *   unchanged (association with tyranny and damnatio memoriae), and could not
 *   ignore it (30 meters tall, visible from multiple city districts). This
 *   generated a 150+ year period of incremental reheading, reinscription, and
 *   symbolic reorientation — each successor attempting to repurpose rather
 *   than remove. The constraint exhibits all six types depending on observer
 *   position: natural law to the analytical view (immutable scale physics),
 *   pure extraction to the powerless (cannot exit its gaze), coordination to
 *   the imperial bureaucracy (technical and ritual infrastructure), mixed to
 *   the Senate (benefits from hierarchy, trapped by succession instability),
 *   temporary to reformers (sunset via regime change), and degraded
 *   performance to later administrative priests. The theater ratio rises over
 *   50 years (0.35 → 0.65) as the functional role atrophies and maintenance
 *   becomes performative — the statue persists through institutional
 *   momentum, not political necessity.
 *
 * KEY AGENTS:
 *   - Emperor Nero: Original beneficiary (institutional/arbitrage) — captures unification of architectural achievement and power projection; dead by succession point
 *   - The Populace: Primary victim (powerless/trapped) — forced occupancy under the statue's physical domination; no institutional recourse or exit
 *   - The Senate: Secondary powerful actor (powerful/constrained) — benefits from imperial cult coordination but trapped by succession instability and damnatio memoriae constraints
 *   - Imperial Bureaucracy: Institutional beneficiary (institutional/arbitrage) — engineers, architects, administrators experience the project as coordination and technical infrastructure
 *   - Post-Neronian Emperors (Vespasian, Hadrian, etc.): Organized reformers (organized/constrained) — face the political problem of inheriting a monument to the previous regime; constrained by removal costs, limited by need to establish own legitimacy
 *   - Cult Maintenance Apparatus: Institutional performers (institutional/arbitrage) — priests and scribes maintain rituals and offerings that gradually disconnect from actual governance
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing the monument as immutable physical law rather than contingent political choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(colossus_nero_inertia, 0.52).
domain_priors:suppression_score(colossus_nero_inertia, 0.48).
domain_priors:theater_ratio(colossus_nero_inertia, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(colossus_nero_inertia, extractiveness, 0.52).
narrative_ontology:constraint_metric(colossus_nero_inertia, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(colossus_nero_inertia, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(colossus_nero_inertia, tangled_rope).
narrative_ontology:human_readable(colossus_nero_inertia, "The Political and Physical Inertia of the Colossus of Nero").
narrative_ontology:topic_domain(colossus_nero_inertia, "political/architectural/social").

domain_priors:requires_active_enforcement(colossus_nero_inertia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(colossus_nero_inertia, imperial_legitimacy).
narrative_ontology:constraint_beneficiary(colossus_nero_inertia, architectural_patron_class).
narrative_ontology:constraint_victim(colossus_nero_inertia, public_land_commons).
narrative_ontology:constraint_victim(colossus_nero_inertia, alternative_monument_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE POWERLESS POPULACE (SNARE) — Citizens occupying the Forum cannot exit Nero's physical domination. The statue's 30-meter height enforces psychological subordination. Removal or criticism invokes state violence. Maximum experienced extraction: forced veneration, constrained speech, no institutional recourse.
constraint_indexing:constraint_classification(colossus_nero_inertia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SENATE (TANGLED ROPE) — Powerful agents benefit from the monument's coordination of imperial cult (shared ritual, status hierarchy), but are constrained by succession instability. After Nero's death, the statue becomes a trap: removing it signals rejection of imperial authority; keeping it broadcasts allegiance to a damnatio memoriae target. Both paths carry extraction costs. Mixed coordination and constraint.
constraint_indexing:constraint_classification(colossus_nero_inertia, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: IMPERIAL ADMINISTRATION (ROPE) — Institutional actors (architects, engineers, overseers) experience the project as pure coordination: shared technical challenge, unified supply chains, clear hierarchy. The statue serves as infrastructure for taxation legitimacy and trade centerpoint. Beneficiary position — extraction runs toward imperial power, not away.
constraint_indexing:constraint_classification(colossus_nero_inertia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POST-NERONIAN REFORMERS (SCAFFOLD) — Organized agents (Vespasian's faction, Senate reformers) see the statue as a temporary symbol of tyranny with a built-in sunset: succession creates legitimacy vacuum. New emperors can repurpose the site (replacing the head, converting to public bath complex). High suppression during Nero's reign; declining over the generation following his death. Temporary support structure for alternative legitimacy.
constraint_indexing:constraint_classification(colossus_nero_inertia, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: CULT MAINTENANCE APPARATUS (PITON) — By the 2nd-3rd centuries CE, the Colossus becomes a vestigial performance. The statue's functional role (projecting living emperor authority) has atrophied — actual imperial power flows through bureaucracy and legions. The maintenance of the monument persists through institutional inertia: priests make offerings, scribes record rituals, but the performance has become disconnected from governance. Theater ratio 0.65 reflects this degradation. Function decays; performance persists.
constraint_indexing:constraint_classification(colossus_nero_inertia, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / IMMUTABILITY (MOUNTAIN) — From a universal perspective, the Colossus appears as a natural law of political communication: large bronze monuments necessarily dominate landscapes and broadcast power. Removal is physically costly, political cost is permanent. From this view, the statue is an irreducible constraint of scale and publicity. However, this naturalizes what is actually contingent: imperial regimes can choose not to build 30-meter statues. The mountain classification masks the structural choice encoded in the constraint.
constraint_indexing:constraint_classification(colossus_nero_inertia, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(colossus_nero_inertia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(colossus_nero_inertia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(colossus_nero_inertia, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(colossus_nero_inertia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(colossus_nero_inertia, TR),
    TR >= 0.70.

:- end_tests(colossus_nero_inertia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts in multiple directions: from the populace (forced veneration, psychological subordination, land enclosure), from successor regimes (political entrapment requiring costly management), and from the civic commons (30 meters of Forum land devoted to singular monument). The value is not extreme (0.68) because the imperial bureaucracy genuinely benefits from the coordination function, and the extraction is not as total as a classical snare — some agents (the architects, the emperor's supporters) experience net benefit. The extraction is asymmetric and layered (characteristic of tangled rope), declining over time as the regime's political necessity fades. Suppression (0.48): Moderate. The constraint is enforced through state violence (criticism invokes punishment during Nero's reign) and psychological domination (the statue's scale). However, suppression is not total — successor regimes have agency to reorient rather than obey literally. The suppression declines after Nero's death as new regimes establish independent legitimacy. Theater ratio (0.65): Moderate-high. By the 2nd-3rd centuries, maintaining the Colossus becomes increasingly performative — the ritual functions persist (priests make offerings, officials record ceremonies) while the governance function (projecting living emperor authority) has atrophied. The performative content rises over the interval as the functional necessity declines.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates dramatic perspectival disagreement despite identical structural properties. The populace sees a snare (maximum extraction, no exit). The Senate sees tangled rope (mixed benefits and costs, but constrained by succession). The imperial bureaucracy sees rope (pure coordination, net benefit). The reformer faction sees scaffold with sunset clause (temporary problem with regime-change solution). The cult apparatus sees piton (degraded performance persisting through inertia). The analytical observer risks seeing mountain (naturalizing contingent choice as immutable law). Each reading is structurally justified by the agent's position and exit options. The perspectival gap is the entire story: the constraint is all six types simultaneously, and which type is 'real' depends entirely on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary across perspectives reflecting structural position: Powerless populace with trapped exit experiences maximum d (0.95) → high f(d) → high experienced extraction chi. Senate with powerful position but constrained exit experiences moderate d (~0.55) → moderate f(d) → balanced extraction chi. Imperial bureaucracy with institutional power and arbitrage options experiences low d (~0.15) → negative f(d) → negative/beneficial chi. Reformer organizations with organized power but constrained succession options experience moderate d (~0.50) → moderate f(d). The piton and mountain perspectives derive from theater ratio and naturalizing framing respectively, not from high directionality values. The beneficiary/victim declarations map to real flows: imperial legitimacy benefits from the monument (extraction toward imperial regime), public land commons are victimized (extraction toward imperial regime at commons expense), alternative monument advocates are victimized (suppressed by the Colossus's preemption of Forum space).
 *
 * MANDATROPHY ANALYSIS:
 *   The Colossus exemplifies mandatrophy resolution through perspectival decomposition. A naive reading would claim: 'Is it coordination (rope) or extraction (snare)?' The answer is: it is both, from different positions. The constraint coordinates imperial bureaucratic action and delivers legitimacy infrastructure (genuine rope for that agent). It simultaneously extracts from the powerless populace and constrains successor regimes (genuine snare/tangled rope from those positions). The constraint also degrades: the functional coordination value declines over time, and the theater ratio rises, moving the classification toward piton and away from rope. The mandatrophy is resolved not by choosing a single type, but by accepting that (1) the constraint is multiplex (all six types are legitimately observed), (2) the classifications differ because the agents occupy genuinely different structural positions, and (3) the temporal evolution (theater rising, extractiveness declining) indicates a lifecycle toward institutional inertia. The false summit in the analytical view reveals that calling the monument 'immutable' naturalizes what is actually a contingent political choice — it could be removed (high cost, but not impossible), and successor regimes have agency to repurpose or abandon it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_capital_reframing,
    'Can successor emperors successfully repurpose the Colossus as a symbol of their legitimacy rather than Nero''s tyranny?',
    'Historical analysis of reheading attempts, inscription changes, and ritual reorientation under Vespasian and Hadrian; literary sources documenting perception shifts',
    'If successful reframing: the statue transitions from snare to rope (pure coordination symbol). If reframing fails: the statue remains entangled extraction, forcing eventual removal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_capital_reframing, empirical, 'Whether symbolic repurposing can overcome the Nero association').

omega_variable(
    technical_removal_feasibility,
    'What is the actual physical and economic cost of removing the Colossus relative to maintaining it?',
    'Engineering analysis of bronze melting capacity, labor requirements, and material value; comparison with other monumental removals (Pharaonic colossi, Soviet statues)',
    'If removal cost is low: extraction is pure political choice, not physical necessity (reveals snare nature). If removal cost is high: extraction partly reflects genuine constraint (mountain component detected).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technical_removal_feasibility, empirical, 'Technical feasibility and cost of removal versus maintenance').

omega_variable(
    succession_legitimacy_lag,
    'How long does a successor regime require to establish independent legitimacy before the previous emperor''s monuments become optional rather than enforced?',
    'Comparative analysis across Roman dynasties and other monarchical transitions; timeline of damnatio memoriae enforcement and symbol replacement',
    'If lag is short (< 5 years): scaffold sunset is real. If lag is long (> 20 years): monument becomes structural piton rather than temporary support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_legitimacy_lag, empirical, 'Duration of monument-based legitimacy enforcement after succession').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(colossus_nero_inertia, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(colossus_tr_t0, colossus_nero_inertia, theater_ratio, 0, 0.35).
narrative_ontology:measurement(colossus_tr_t25, colossus_nero_inertia, theater_ratio, 25, 0.55).
narrative_ontology:measurement(colossus_tr_t50, colossus_nero_inertia, theater_ratio, 50, 0.65).

% Extraction over time
narrative_ontology:measurement(colossus_be_t0, colossus_nero_inertia, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(colossus_be_t25, colossus_nero_inertia, base_extractiveness, 25, 0.52).
narrative_ontology:measurement(colossus_be_t50, colossus_nero_inertia, base_extractiveness, 50, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(colossus_nero_inertia, enforcement_mechanism).
narrative_ontology:affects_constraint(colossus_nero_inertia, roman_imperial_succession_instability).
narrative_ontology:affects_constraint(colossus_nero_inertia, damnatio_memoriae_enforcement).
narrative_ontology:affects_constraint(colossus_nero_inertia, public_land_enclosure_roman_cities).

% DUAL FORMULATION NOTE:
% The Colossus operates at the intersection of three constraint families: (1) imperial legitimacy mechanisms (affects succession dynamics), (2) symbolic violence enforcement (affects damnatio memoriae systems), and (3) public commons enclosure (affects land access in urban contexts). This story focuses on the inertial constraint itself. Upstream constraints: regime durability, monument construction capacity. Downstream constraints: successor regime legitimacy, civic space allocation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(colossus_nero_inertia, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
