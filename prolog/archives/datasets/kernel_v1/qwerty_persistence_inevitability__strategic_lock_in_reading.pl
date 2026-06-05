% ============================================================================
% CONSTRAINT STORY: qwerty_persistence_inevitability__strategic_lock_in_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qwerty_persistence_inevitability__strategic_lock_in_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: qwerty_persistence_inevitability__strategic_lock_in_reading
 *   human_readable: QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)
 *   domain: technology_history/political_economy/institutional_analysis
 *
 * SUMMARY:
 *   In the 1870s–1890s, multiple keyboard layouts competed: QWERTY (Sholes),
 *   Dvorak (1932 development, but earlier precursors), and others. The
 *   typewriter manufacturers' 1893 cartel — Remington, Smith-Premier,
 *   Caligraph, and others — standardized on QWERTY as a coordination
 *   mechanism to prevent market fragmentation. This cartel enforcement took
 *   the form of (1) exclusive training partnerships with typing schools
 *   (Remington sponsored typing instruction that taught only QWERTY), (2)
 *   patent licensing agreements that locked competing manufacturers into
 *   QWERTY, and (3) deliberate suppression of Dvorak and other layouts during
 *   the 1930s–1950s even as ergonomic research documented superior
 *   alternatives. The constraint's extractive mechanism is the cartel's
 *   capture of the standardization function: they extract rents by
 *   controlling training, preventing layout switching, and forcing all
 *   typists into QWERTY dependency. The coordination function is genuine
 *   (standardization solves multiple-equilibrium fragmentation), but the
 *   extraction mechanism is engineered. Victims include all typists bearing
 *   ergonomic costs and retraining barriers; beneficiaries are the cartel
 *   members extracting control rents. This reading differs from the
 *   path-dependency reading by attributing QWERTY dominance to strategic
 *   enforcement rather than accident-plus-network-effects. Both readings can
 *   be true at different time scales: accident + first-mover (1870s–1890s)
 *   followed by cartel enforcement + path-dependency lock-in (1893 onward).
 *   The strategic reading highlights the manufacturing of the 'inevitability'
 *   framing itself.
 *
 * KEY AGENTS:
 *   - Typewriter Manufacturers Cartel (1893): Institutional beneficiary (institutional/arbitrage) — controls standardization, extracts rents through training partnerships, enforces QWERTY dominance via patent licensing and industry coordination
 *   - Typists (Clerical Workers, Journalists): Primary victims (powerless/trapped) — bear ergonomic costs and retraining barriers; cannot exit QWERTY without coordination risk
 *   - Competing Keyboard Designers (Dvorak Advocates): Secondary victims (moderate/constrained) — face cartel suppression and coordination lock-in; some agency through specialized markets but cannot displace standard
 *   - Typing Schools & Training Institutions: Institutional coordinators (institutional/constrained) — contracted by manufacturers to teach only QWERTY; bear some extraction risk if they defect
 *   - Dvorak & Ergonomic Researchers: Organized challengers (organized/constrained) — document superior layouts but lack market power to overcome coordination lock-in
 *   - Modern Digital Systems: Institutional piton bearers (institutional/arbitrage) — maintain QWERTY in software through path-dependent inertia even after mechanical constraints became obsolete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.52).
domain_priors:suppression_score(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.58).
domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope).
narrative_ontology:human_readable(qwerty_persistence_inevitability__strategic_lock_in_reading, "QWERTY Persistence as Manufacturer-Engineered Lock-In (Strategic Lock-In Reading)").
narrative_ontology:topic_domain(qwerty_persistence_inevitability__strategic_lock_in_reading, "technology_history/political_economy/institutional_analysis").

domain_priors:requires_active_enforcement(qwerty_persistence_inevitability__strategic_lock_in_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qwerty_persistence_inevitability__strategic_lock_in_reading, '54b4070d-69ad-4434-ae54-9ec05dd3317b').
narrative_ontology:cs_kernel_codification('54b4070d-69ad-4434-ae54-9ec05dd3317b', distributed).
narrative_ontology:cs_authority_grounding('54b4070d-69ad-4434-ae54-9ec05dd3317b', extraction).
narrative_ontology:cs_reading_relation('54b4070d-69ad-4434-ae54-9ec05dd3317b', qwerty_persistence_inevitability__path_dependency_reading, coexists_with).
narrative_ontology:cs_axiom('54b4070d-69ad-4434-ae54-9ec05dd3317b', foundational, qwerty_dominance_is_manufactured_extraction).
narrative_ontology:cs_axiom_status(qwerty_dominance_is_manufactured_extraction, holdable).
narrative_ontology:cs_axiom_grounding('54b4070d-69ad-4434-ae54-9ec05dd3317b', qwerty_dominance_is_manufactured_extraction, empirically_contingent).
narrative_ontology:cs_axiom('54b4070d-69ad-4434-ae54-9ec05dd3317b', foundational, manufactured_inevitability_obscures_beneficiary).
narrative_ontology:cs_axiom_status(manufactured_inevitability_obscures_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('54b4070d-69ad-4434-ae54-9ec05dd3317b', manufactured_inevitability_obscures_beneficiary, deontological).
narrative_ontology:cs_reference_frame('54b4070d-69ad-4434-ae54-9ec05dd3317b', competitive_keyboard_market_with_multiple_viable_layouts).
narrative_ontology:cs_drift_state('54b4070d-69ad-4434-ae54-9ec05dd3317b', contemporary_digital_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('54b4070d-69ad-4434-ae54-9ec05dd3317b', '').
narrative_ontology:cs_kernel_id(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, typewriter_manufacturers_1893_cartel).
narrative_ontology:constraint_beneficiary(qwerty_persistence_inevitability__strategic_lock_in_reading, mechanical_lock_in_extractors).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, typists_ergonomic_cost).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, competing_keyboard_designers).
narrative_ontology:constraint_victim(qwerty_persistence_inevitability__strategic_lock_in_reading, alternative_layout_innovators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TYPISTS BEARING ERGONOMIC COSTS (SNARE) — Trapped by coordination lock-in: cannot exit QWERTY without retraining cost and coordination risk (all machines, typing pools, employers standardized on QWERTY). Experiences maximum extraction with no structural exit. Carpal tunnel and repetitive strain costs are borne indefinitely by bodies locked into an inferior layout.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING KEYBOARD DESIGNERS (TANGLED ROPE) — Face both coordination benefits (can design knowing QWERTY is standard baseline) and extraction costs (cartel enforcement, coordination lock-in prevents adoption of superior designs). Some agency through specialized markets (ergonomic keyboards, programming layouts) but constrained from displacing the standard. Benefits from network effects; costs from standardization control.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TYPEWRITER MANUFACTURER CARTEL (ROPE) — Primary beneficiary with maximum arbitrage (control the standard, extract rents, coordinate industrywide training partnerships). Experiences the constraint as coordination mechanism: standardization solves the multiple-equilibrium problem (if each manufacturer supported different layouts, the market would fragment). The cartel's perspective is pure coordination gain — they extract rents through coordination, and coordination is their stated goal.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ERGONOMIC REFORM MOVEMENT (SCAFFOLD) — Organized agents (OSHA, ergonomic research, Dvorak advocates) see lock-in as a temporary institutional coordination failure addressable through policy intervention, worker organization, and alternative-layout development. Carpal tunnel syndrome documentation and occupational health regulations create institutional pressure toward sunset. But suppression mechanism (employer standardization requirements, training costs) persists. This is transitional coordination with a partial sunset: alternatives (Dvorak, programmatic key remapping) exist but remain marginal.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: QWERTY STANDARD AS PITON (INSTITUTIONAL INERTIA) — The standard persists through institutional momentum long after the cartel dissolved and its original justification (mechanical key-collision avoidance) became obsolete. Theater ratio is high: we maintain QWERTY because 'everyone uses QWERTY,' not because typewriter mechanisms require it. The performative aspect dominates — QWERTY is maintained through path-dependent inertia rather than active cartel enforcement. The standard's functional extraction mechanism has atrophied; theatrical legitimacy ('this is just how keyboards are') sustains it.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal scope, the analytical frame risks seeing QWERTY persistence as a natural coordinate on the landscape of possible equilibria: 'All societies converge on a single keyboard standard for coordination efficiency; QWERTY happened to be first; network effects make it immobile.' This perspective treats manufactured lock-in as inherent coordination necessity. The engine's false summit detector identifies this as naturalization of a strategic institutional arrangement.
constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(qwerty_persistence_inevitability__strategic_lock_in_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(qwerty_persistence_inevitability__strategic_lock_in_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(qwerty_persistence_inevitability__strategic_lock_in_reading, TR),
    TR >= 0.70.

:- end_tests(qwerty_persistence_inevitability__strategic_lock_in_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The cartel's deliberate standardization captures genuine coordination value (solving multiple-equilibrium fragmentation) but extracts additional rent through training control, patent licensing, and suppression of alternatives. The extraction is not maximal (snare-level) because competing layouts existed and some markets adopted alternatives — suppression was real but incomplete. The value reflects the hybrid nature: coordination function + engineered lock-in. Suppression (0.58): Moderate-high. The cartel enforced standardization through exclusive training contracts, patent licensing agreements that mandated QWERTY compatibility, and cultural normalization ('proper typing' means QWERTY). Dvorak faced cartel opposition and market fragmentation barriers, but was not absolutely forbidden — some specialists adopted it. Theater ratio (0.65): High and rising. In the 1870s–1890s mechanical era, QWERTY's dominance had a functional justification (key collision avoidance, though debated). By the 1920s–1970s, as typewriter design improved and digital technology emerged, QWERTY's justification became purely theatrical: we use it because everyone uses it, not because mechanical constraints demand it. Measurements show theater rising from 0.40 (functional justification exists) to 0.72 (performative institutional inertia). Extractiveness declined slightly from 1920 (0.58) to 1970 (0.48) as cartel enforcement mechanisms decayed and digital technology reduced switching costs, but persistence remained high through path-dependent institutional lock-in.
 *
 * PERSPECTIVAL GAP:
 *   Each agent's classification depends on their structural position relative to the extraction mechanism. The cartel sees pure coordination (Rope) — their stated goal is preventing fragmentation, and standardization achieves that. Typists see pure extraction (Snare) — they bear ergonomic costs with no exit. Competing designers see mixed coordination-extraction (Tangled Rope) — the standard enables baseline design but suppresses displacement. Ergonomic reformers see a temporary problem with policy solutions (Scaffold) — OSHA and occupational health regulations are building pressure toward layout change. The institutional standard itself is a Piton — QWERTY persists through inertia, not active cartel enforcement (the cartel dissolved), because switching still costs money even though the original justification (mechanical necessity) vanished. The analytical observer risks seeing a Natural Law (Mountain) — 'all markets converge on a single standard for efficiency' — but the structural data reveals this as a false summit: the manufacturing of inevitability itself is the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   The strategic lock-in reading attributes the beneficiary set to the 1893 cartel and its institutional successors, and the victim set to typists and competing designers. This assignment differs from the path-dependency reading, which would distribute benefits and costs differently (accident + network effects → no deliberate beneficiary). In the strategic reading, d-values for beneficiaries (cartel members) are low (d ≈ 0.15–0.25, they benefit from enforcement), while d-values for victims (typists) are high (d ≈ 0.85–0.95, they bear ergonomic and retraining costs). Competing designers occupy an intermediate position (d ≈ 0.55–0.70) because they both benefit from the standard baseline and are suppressed by cartel enforcement. The chi formula χ = ε × f(d) × σ(S) produces high χ for victims (high d → high f(d)) and low/negative χ for beneficiaries (low d → negative f(d)), which is diagnostically correct: victims experience this as snare (maximum extraction), beneficiaries experience it as rope (coordination gain). The perspectival gap arises because the same structural phenomenon (standardization) produces opposite directionality values depending on position.
 *
 * MANDATROPHY ANALYSIS:
 *   The strategic lock-in reading resolves mandatrophy by assigning a clear tangled_rope classification: genuine coordination function (standardization solves multiple-equilibrium fragmentation) + engineered extraction mechanism (cartel controls training, enforces lock-in, suppresses alternatives). Both functions are real. The mandate is 'manufactured inevitability': the cartel and its successors make standardization appear like a natural law of markets ('we all must use the same standard for efficiency') when it is actually a strategic institutional arrangement ('we all must use QWERTY because the manufacturers control training and prevent alternatives'). The beneficiary reading (rope — pure coordination from the cartel's perspective) and the victim reading (snare — pure extraction from the typist's perspective) are both structural realities generated by the same constraint, observed from different positions. No single classification is 'correct' because the classification is perspectival. The strategic reading differs from the path-dependency reading in that it makes the manufactured nature of the 'inevitability' visible — path dependency without strategic actors might produce convergence, but convergence without extraction. Strategic lock-in produces both convergence AND engineered extraction, which is structurally distinct.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cartel_enforcement_mechanism_evidence,
    'What empirical traces distinguish the 1893 cartel''s active standardization enforcement from retrospective narrative reconstruction?',
    'Archival analysis of manufacturer correspondence, patent licensing agreements, and typewriter salespeople training documentation; comparison of adoption timelines in markets with vs. without cartel presence',
    'If enforcement was minimal and QWERTY spread through accident + first-mover advantage: path_dependency_reading is correct, strategic_lock_in_reading overattributes agency. If enforcement was deliberate: strategic reading confirmed. If mixed: both readings capture real structural features at different time scales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cartel_enforcement_mechanism_evidence, empirical, 'Whether cartel enforcement vs. accident-plus-network explains QWERTY adoption').

omega_variable(
    ergonomic_cost_quantification,
    'What is the aggregate economic cost of QWERTY-induced repetitive strain injury vs. the coordination cost of switching to an alternative layout?',
    'Epidemiological data on carpal tunnel and RSI prevalence in typing-intensive professions; economic modeling of retraining cost vs. health-care cost; controlled studies of alternative layouts (Dvorak, Colemak) adoption in isolated populations',
    'If RSI cost >> switching cost: the constraint is clearly extractive (snare/tangled_rope confirmed). If switching cost >> RSI cost: QWERTY persistence appears rational despite suboptimality (implicates coordination necessity view). If costs are comparable: the beneficiary reading is crucial — who bears which cost?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ergonomic_cost_quantification, empirical, 'Magnitude of ergonomic costs vs. switching costs for keyboard layout change').

omega_variable(
    sibling_reading_foreclosure,
    'Do the strategic lock-in reading''s core axioms logically foreclose the path-dependency reading, or do they coexist as differently-weighted accounts of the same historical process?',
    'Historiographic analysis of whether a historian can coherently claim both ''QWERTY adoption was accident-driven in 1870s–1890s'' AND ''cartel-enforced lock-in began in 1893'' within the same causal framework; or whether ''intentional standardization control'' and ''accident plus network effects'' are incompatible framings of the same events',
    'If foreclosure holds: one reading''s core premise contradicts the other''s; only one can be true. If coexistence holds: different scholars can hold both without logical contradiction, as readings of overlapping but distinct causal chains. This is a conceptual question about how historical narratives combine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether strategic and path-dependency readings foreclose each other or coexist').

omega_variable(
    manufacturing_lock_in_vs_natural_coordination,
    'Is QWERTY persistence best explained as manufactured extraction that happens to coordinate, or as natural coordination that happens to benefit manufacturers?',
    'Counterfactual analysis: would an uncoordinated market have converged on QWERTY anyway? Would competing layouts have achieved equivalently high adoption in the absence of cartel standardization? Comparative analysis of non-cartelized keyboard markets (e.g., non-English-language keyboards) for adoption patterns.',
    'If manufactured extraction is primary: the constraint is tangled_rope at its core (extraction disguised as coordination). If natural coordination is primary and manufacturing is secondary: the constraint may be better classified as rope with a piton layer (original coordination, now degraded). This governs the strategic vs. path-dependency reading divide.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_lock_in_vs_natural_coordination, conceptual, 'Primary driver: manufacturer extraction or natural coordination efficiency?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qwerty_persistence_inevitability__strategic_lock_in_reading, 1870, 1970).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qwerty_lock_theater_1870, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(qwerty_lock_theater_1893_cartel, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 1, 0.55).
narrative_ontology:measurement(qwerty_lock_theater_1920_consolidation, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 2, 0.65).
narrative_ontology:measurement(qwerty_lock_theater_1970_digital, qwerty_persistence_inevitability__strategic_lock_in_reading, theater_ratio, 3, 0.72).

% Extraction over time
narrative_ontology:measurement(qwerty_lock_extractiveness_1870, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(qwerty_lock_extractiveness_1893_cartel, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 1, 0.52).
narrative_ontology:measurement(qwerty_lock_extractiveness_1920_consolidation, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(qwerty_lock_extractiveness_1970_digital_transition, qwerty_persistence_inevitability__strategic_lock_in_reading, base_extractiveness, 3, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(qwerty_lock_suppression_1870, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qwerty_lock_suppression_1893_cartel_enforcement, qwerty_persistence_inevitability__strategic_lock_in_reading, suppression_requirement, 1, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qwerty_persistence_inevitability__strategic_lock_in_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(qwerty_persistence_inevitability__strategic_lock_in_reading, 0.18).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, qwerty_persistence_inevitability__path_dependency_reading).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, dvorak_adoption_suppression_mechanism).
narrative_ontology:affects_constraint(qwerty_persistence_inevitability__strategic_lock_in_reading, typing_pool_cartel_control).

% DUAL FORMULATION NOTE:
% The QWERTY kernel admits two readings: path_dependency and strategic_lock_in. Each reading instantiates the same historical process with different causal attribution. The path_dependency reading emphasizes accident + network effects (early first-mover advantage, coordination necessity); the strategic reading emphasizes cartel enforcement + manufactured inevitability. Both readings affect downstream constraints (Dvorak suppression, typing pool control). The network captures the family structure: both readings are siblings linked to the parent kernel, and both affect the same downstream mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qwerty_persistence_inevitability__strategic_lock_in_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
