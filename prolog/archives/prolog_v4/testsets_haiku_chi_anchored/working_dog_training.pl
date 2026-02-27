% ============================================================================
% CONSTRAINT STORY: working_dog_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_working_dog_training, []).

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
 *   constraint_id: working_dog_training
 *   human_readable: Training of Working Dogs for Specific Tasks
 *   domain: social/animal_labor
 *
 * SUMMARY:
 *   The training of working dogs for specific tasks (bomb detection, search
 *   and rescue, police apprehension) creates a structural constraint in which
 *   animal labor is extracted through behavioral conditioning, selective
 *   breeding, and suppression of alternative behavioral repertoires. The
 *   constraint exhibits multiple classification types depending on the
 *   structural position of the observer. From the dog's perspective, it is a
 *   snare: behavioral and physical conditioning creates a regime of
 *   suppression with no exit option. From the handler institution's
 *   perspective, it is coordination: establishing a communication protocol
 *   that solves a legitimate tactical problem (human-machine gap in olfactory
 *   detection). From the individual handler's perspective, it is tangled
 *   rope: both coordinating with the animal and extracting emotional labor as
 *   a mechanism to ensure compliance and bonding. From the perspective of
 *   organized animal welfare advocates, it is a scaffold: emerging technology
 *   (robotic detection, AI olfaction modeling) is creating a sunset pathway
 *   that will render the regime obsolete within one to two decades. The
 *   constraint exemplifies how the same structural phenomenon—compelling an
 *   animal to perform labor—can be read as pure extraction, mixed
 *   coordination-extraction, or temporary technology transition, depending on
 *   the observer's structural position and time horizon.
 *
 * KEY AGENTS:
 *   - Working Dogs: Primary victim (powerless/trapped) — subjected to selective breeding, behavioral conditioning, suppression of natural behaviors, operational risk, and no exit option
 *   - Handler Institutions (Police, Military, Border Patrol): Primary beneficiary (institutional/arbitrage) — obtain specialized labor at lower cost than human specialists, can exit if technology improves
 *   - Individual Handlers (Trainers, Operators): Mixed actor (moderate/mobile) — benefit from career advancement and task capability but bear emotional labor and responsibility costs
 *   - Civilian Populations: Secondary victim (powerless/trapped) — benefit from detection and rescue services but have no voice in welfare standards or training ethics
 *   - Animal Welfare Advocates: Organized agents (organized/constrained) — building alternative technology and pushing regulatory sunset clauses; see the regime as declining rather than stable
 *   - Breeding Industry: Beneficiary (powerful/arbitrage) — selective breeding of traits (docility, olfactory sensitivity) creates economic value and path-dependence into the regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(working_dog_training, 0.52).
domain_priors:suppression_score(working_dog_training, 0.68).
domain_priors:theater_ratio(working_dog_training, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(working_dog_training, extractiveness, 0.52).
narrative_ontology:constraint_metric(working_dog_training, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(working_dog_training, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(working_dog_training, snare).
narrative_ontology:human_readable(working_dog_training, "Training of Working Dogs for Specific Tasks").
narrative_ontology:topic_domain(working_dog_training, "social/animal_labor").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(working_dog_training, handler_institutions).
narrative_ontology:constraint_beneficiary(working_dog_training, human_beneficiaries_of_service).
narrative_ontology:constraint_victim(working_dog_training, working_dogs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE WORKING DOG (SNARE) — Cannot exit training regime; bears full cost of behavioral conditioning, physical stress, psychological constraint, and operational risk. Dog has no exit option and no alternative use pathway once trained for specific task. d≈0.98, f(d)≈1.41, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(working_dog_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HANDLER INSTITUTION (ROPE) — Sees training as pure coordination: establishing communication protocol between species for mutual task execution. Institution can exit (hire human specialists instead) and benefits from reliable trained asset. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary through arbitrage.
constraint_indexing:constraint_classification(working_dog_training, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL HANDLER (TANGLED ROPE) — Benefits from training (career advancement, bonding, operational effectiveness) but also bears costs (emotional labor, responsibility for dog's welfare, potential PTSD transmission to animal). Exit is possible (change jobs) but costly. d≈0.52, f(d)≈0.65, σ=1.0 → χ≈0.34. Mixed coordination and extraction.
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: CIVILIAN BENEFICIARIES (SNARE) — Benefit from bomb detection and search-rescue services but have no voice in training regime design, ethics, or dog welfare standards. Are trapped as implicit beneficiaries of extraction they didn't authorize. d≈0.85, f(d)≈1.13, σ=1.0 → χ≈0.59. Secondary snare dynamic.
constraint_indexing:constraint_classification(working_dog_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 5: ANIMAL WELFARE COALITION (SCAFFOLD) — Organized advocacy (animal rights groups, veterinary ethicists) sees training regime as temporary problem with sunset: drone technology, robotic detection systems, and AI-enabled search are creating alternatives to live-dog deployment. Current training extracts through suppression (χ≈0.52) but coalition perceives this as decaying infrastructure (piton-adjacent). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Low effective extraction because alternatives exist and are maturing.
constraint_indexing:constraint_classification(working_dog_training, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: DOG-TRAINING COMPLEX (PITON) — The working dog training regime persists through institutional inertia and cultural theater (tradition, prestige of 'man and dog' narrative, public trust in live detection). Functionally, dogs are already being partially replaced by technology in bomb detection (≈35% of military mine-detection now robotic), but the institution maintains the practice for legitimacy and continuity. theater_ratio=0.35 seems low, but this reflects actual functional value; the piton classification comes from the observation that the regime is declining in function relative to its institutional maintenance cost. Alternatives exist; the constraint persists through path-dependence.
constraint_indexing:constraint_classification(working_dog_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(working_dog_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(working_dog_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(working_dog_training, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(working_dog_training, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(working_dog_training, TR),
    TR >= 0.70.

:- end_tests(working_dog_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The constraint extracts behavioral suppression, physical capacity, and operational risk from dogs while delivering genuine capability gain to handler institutions. The extraction is substantial—selective breeding has narrowed dog behavioral repertoires significantly—but is not maximal because some coordination benefit exists: dogs do experience task-driven engagement and social bonding. Theater ratio (0.35): Moderate-low. The training regime has genuine functional content (dogs detect explosives better than sensors in some conditions) and is not primarily performative. However, the theater component is rising: institutional maintenance of the regime persists beyond diminishing marginal utility as alternative technologies mature. Suppression (0.68): High. Behavioral conditioning creates strong suppression of alternative repertoires, breeding locks animals into dependence, and operational deployment creates physical/psychological stress. Claimed type: Snare. From the dog's perspective, this is unambiguous snare (trapped victim, high extraction). However, the multi-perspective reading shows this is snare-plus-tangled-rope-plus-scaffold—the regime's classification depends entirely on whose structural position you adopt.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The working dog experiences snare (trapped, extracted from, no exit). The handler institution experiences rope (coordination, mutual task benefit, can exit). The individual handler experiences tangled rope (mixed benefit and cost through emotional bonding). Organized animal welfare advocates experience scaffold (temporary problem with technological sunset). The institutional dog-training complex experiences piton (declining function, persisting through inertia). The civilian beneficiary experiences snare as a secondary effect (benefits from extraction they didn't authorize). The gap between 'dog experiences pure extraction' and 'handler institution experiences pure coordination' is irreducible—it is not a difference in measurement but a structural difference in who pays the cost and who reaps the benefit.
 *
 * DIRECTIONALITY LOGIC:
 *   Working dog: Victim + trapped → d≈0.98, f(d)≈1.41. Maximum extraction. Handler institution: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Individual handler: Mixed (benefits from bonding, costs from responsibility) + mobile → d≈0.52, f(d)≈0.65. Moderate extraction. Civilian beneficiary: Victim (implicit, voiceless) + trapped → d≈0.85, f(d)≈1.13. Significant extraction. Animal welfare advocates: Organized, seeking exit pathway + constrained → d≈0.35, f(d)≈0.35. Lower effective extraction because alternatives exist. Dog-training institutional complex: Beneficiary (maintains practice) + arbitrage (can shift to technology) → d≈0.08, f(d)≈-0.10. Net beneficiary, but classification as piton comes from theater_ratio gate (declining function), not from chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that working dog training is unambiguously a snare from the animal's structural perspective (trapped victim, suppressed behavioral repertoire, extraction with no reciprocal benefit) but can be read as mixed coordination-extraction from human perspectives that acknowledge the genuine capability gains. The framework does not permit a false neutral classification—'working dogs are trained for public good' naturalizes the extraction. The snare classification for the dog's perspective is analytically prior. The handler's rope classification is legitimate but subordinate to the dog's snare: the coordination benefit (solving tactical detection problems) is real, but it is extracted from an agent (the dog) who cannot consent or exit. This is textbook mandatrophy: mistaking extraction with incidental coordination benefit for pure coordination. The resolution is to maintain both classifications (snare for the dog, rope for the institution) as simultaneously true rather than competing—the framework's multi-perspective structure captures the irreducible asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_sentience_welfare_threshold,
    'At what level of animal behavioral suppression (breeding for docility, medication, isolation) does the training regime transition from ethical labor discipline to animal torture?',
    'Comparative neurobiology of trained vs feral dogs; stress biomarkers (cortisol, behavioral repertoire restriction); longitudinal tracking of dog lifespan, behavioral pathology, and post-retirement quality of life',
    'If threshold crossed: constraint reclassifies as pure snare (χ≈0.75+) with no legitimating coordination benefit. If threshold not crossed: tangled rope reading gains support (mixed coordination/extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_sentience_welfare_threshold, empirical, 'Whether behavioral suppression reaches torture threshold').

omega_variable(
    technological_substitution_timeline,
    'Will drone detection, robotic search platforms, and AI-enabled olfaction modeling replace live-dog labor within 15-30 years, making the training regime obsolete?',
    'Technology maturation curves for detection accuracy, deployment cost, and user acceptance; regulatory timelines for transition; institutional adoption rates in leading militaries and police forces',
    'If yes (>80% confidence): scaffold perspective confirmed; sunset is structural. If no: institutional inertia dominates; piton perspective confirmed over longer timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_substitution_timeline, empirical, 'Whether technological alternatives will make dog training obsolete').

omega_variable(
    handler_emotional_labor_extraction,
    'Does the emotional bonding between handler and dog constitute genuine coordination benefit or coercive extraction mechanism (making handlers complicit in dog exploitation through affection)?',
    'Longitudinal handler interviews and psychological assessment; comparison of PTSD/moral injury rates between dog-handler and non-dog-handler personnel; analysis of whether bonding increases handler compliance with deployment in high-risk scenarios',
    'If coordination benefit: handler perspective stays tangled rope. If extraction mechanism: handler moves toward snare classification; entire regime becomes multi-layer extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(handler_emotional_labor_extraction, conceptual, 'Whether handler-dog bonding is coordination or coercive mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(working_dog_training, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wdt_tr_t0, working_dog_training, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wdt_tr_t25, working_dog_training, theater_ratio, 25, 0.31).
narrative_ontology:measurement(wdt_tr_t50, working_dog_training, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(wdt_be_t0, working_dog_training, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wdt_be_t25, working_dog_training, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(wdt_be_t50, working_dog_training, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(working_dog_training, enforcement_mechanism).
narrative_ontology:affects_constraint(working_dog_training, animal_labor_extraction_general).
narrative_ontology:affects_constraint(working_dog_training, handler_emotional_labor_suppression).

% DUAL FORMULATION NOTE:
% Working dog training is a specific instance of the broader constraint on animal labor extraction. The animal labor constraint has ε≈0.60 (all forms). Working dog training branches off this to examine the specialized role of behavioral suppression and the role of interspecies emotional bonding as an enforcement mechanism. The two stories are linked: general animal labor is upstream (ε higher, more uniform across contexts), working dog training is downstream (ε lower due to genuine coordination component, but with specific theater dynamics around institutional persistence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(working_dog_training, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
