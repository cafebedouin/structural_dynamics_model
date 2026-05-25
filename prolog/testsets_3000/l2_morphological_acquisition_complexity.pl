% ============================================================================
% CONSTRAINT STORY: l2_morphological_acquisition_complexity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_l2_morphological_acquisition_complexity, []).

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
 *   constraint_id: l2_morphological_acquisition_complexity
 *   human_readable: L2 Morphological Acquisition Complexity Constraint
 *   domain: second_language_acquisition/cognitive_linguistics
 *
 * SUMMARY:
 *   L2 morphological acquisition complexity is a structural constraint that
 *   arises at the intersection of linguistic typology, cognitive processing,
 *   and institutional gatekeeping. Morphologically rich languages (Russian,
 *   Finnish, Arabic, Hungarian, Polish) impose an irreducible learning burden
 *   on adult learners that morphologically sparse languages (English,
 *   Vietnamese, Mandarin Chinese) do not. This constraint exhibits the full
 *   spectrum of DR classification from different perspectives: from the
 *   learner's view it is a snare (trapped in suppression), from cognitive
 *   efficiency's view it is rope (information-packing coordination), from
 *   language communities it is tangled rope (both coordination and
 *   extraction), from pedagogical innovators it is scaffold (temporary, with
 *   methods-based sunset), from traditional grammar pedagogy it is piton
 *   (performative ritual), and from the analytical observer it risks being
 *   rope (universal cognitive law) when it is actually tangled rope due to
 *   asymmetric gatekeeping. The extractiveness has increased over the
 *   interval as institutional precision demands have grown (international
 *   proficiency certification like CEFR has made morphological accuracy a
 *   high-stakes requirement) while theater_ratio has decreased (pedagogical
 *   innovation has reduced explicit grammar memorization in favor of implicit
 *   learning approaches). The tension is between the coordination function
 *   (morphology packs information efficiently) and the extraction function
 *   (the high barrier to L2 morphological competence maintains native-speaker
 *   status advantage and supports institutional gatekeeping through
 *   proficiency certification).
 *
 * KEY AGENTS:
 *   - Morphologically Complex Learners: Primary victims (powerless/trapped) — face irreducible cognitive complexity with no alternative pathway; all pathways to native-like competence require traversing the full morphological space
 *   - Cognitive Processing Efficiency: Primary beneficiary (institutional/arbitrage) — human cognition benefits from morphology's information-packing; native processing is efficient
 *   - Language Communities with L2 Pressure: Secondary actors (moderate/constrained) — experience both coordination benefits and extraction costs; asymmetric gatekeeping preserves native advantage
 *   - Low-Resource Language Communities: Tertiary victim — languages without well-developed pedagogical materials and SLA research (Basque, Icelandic, Finno-Ugric languages) impose even higher barriers because scaffolding resources are sparse
 *   - Pedagogical Innovation Communities: Organized agents (organized/mobile) — corpus linguists, SLA researchers, comprehensible input proponents developing scaffolding methods with sunset potential
 *   - Traditional Grammar Institutions: Institutional actor (institutional/arbitrage) — language academies, educational certification bodies maintaining explicit grammar pedagogy despite low functional effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent institutional constraint as a universal cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(l2_morphological_acquisition_complexity, 0.58).
domain_priors:suppression_score(l2_morphological_acquisition_complexity, 0.65).
domain_priors:theater_ratio(l2_morphological_acquisition_complexity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(l2_morphological_acquisition_complexity, extractiveness, 0.58).
narrative_ontology:constraint_metric(l2_morphological_acquisition_complexity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(l2_morphological_acquisition_complexity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(l2_morphological_acquisition_complexity, tangled_rope).
narrative_ontology:human_readable(l2_morphological_acquisition_complexity, "L2 Morphological Acquisition Complexity Constraint").
narrative_ontology:topic_domain(l2_morphological_acquisition_complexity, "second_language_acquisition/cognitive_linguistics").

domain_priors:requires_active_enforcement(l2_morphological_acquisition_complexity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(l2_morphological_acquisition_complexity, cognitive_processing_efficiency).
narrative_ontology:constraint_beneficiary(l2_morphological_acquisition_complexity, pedagogical_gatekeeping_institutions).
narrative_ontology:constraint_victim(l2_morphological_acquisition_complexity, morphologically_complex_learners).
narrative_ontology:constraint_victim(l2_morphological_acquisition_complexity, low_resource_language_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MORPHOLOGICALLY COMPLEX LEARNER (SNARE) — A learner acquiring a morphologically rich L2 (e.g., Russian, Finnish, Arabic) faces an irreducible complexity bottleneck: the target language's morphological system requires mastery of dozens of agreement paradigms, case systems, and allomorphic variations that have no shortcut. The learner cannot exit this constraint — all native-like competence requires traversing the full morphological space. High suppression from the cognitive demands and lack of alternative pathways; extraction experienced as the time and cognitive load imposed by morphological complexity that non-morphologically-rich speakers never encounter.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COGNITIVE PROCESSING EFFICIENCY (ROPE) — From the perspective of how human cognition actually works, morphological complexity is a coordination mechanism: it concentrates information into bound morphemes, reducing the number of lexical items needed and enabling predictive comprehension. Native speakers experience morphological systems as efficient — they coordinate meaning across inflected wordforms without conscious effort. The cognitive system benefits from the information-packing function of morphology. This perspective sees the constraint as pure coordination with minimal extraction overhead.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LANGUAGE COMMUNITY UNDER ACQUISITION PRESSURE (TANGLED ROPE) — For a language community with significant L2 learner populations (e.g., English learners acquiring Russian, Arabic learners acquiring Finnish), the morphological complexity serves both coordination (efficient information density) and extraction functions simultaneously. Communities benefit from the coordination efficiency that morphology provides native speakers, but also experience asymmetric extraction: the high barrier to L2 competence preserves native-speaker advantage in domains requiring precise morphological control, maintaining status and gatekeeping power. Exit is constrained — simplifying morphology would damage the language's information structure, but the status quo extracts competence asymmetrically.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PEDAGOGICAL INNOVATION MOVEMENTS (SCAFFOLD) — Organized pedagogical communities (corpus-based teaching, comprehensible input methods, task-based learning, morphosyntactic bootstrapping research) see the complexity bottleneck as temporary and solvable through method innovation. These approaches target the high suppression by creating alternative cognitive pathways (chunking morphologically complex utterances, implicit learning from frequency patterns, task contexts that scaffold morphological awareness). The sunset clause is real: as computational morphological analyzers and spaced repetition systems improve, learners can bypass memorization of full paradigms by pattern recognition and lookup. Effective extraction drops because organized agents have agency and emerging alternatives.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL GRAMMAR-TRANSLATION PEDAGOGY (PITON) — The institutional practice of teaching morphology through explicit rule-memorization and translation exercises is largely performative: the theater_ratio is high because explicit rule learning correlates poorly with acquisition outcomes in actual communication. Learners memorize paradigms for exams but fail to use them fluently. The pedagogical institution persists through inertia — it is easy to assess explicit knowledge, produces measurable test score improvements, and maintains institutional credibility — despite low functional effectiveness for real communication. The ritual of grammar-based morphology teaching survives degraded, maintained by institutional pressure rather than proven efficacy.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE UNIVERSALS VIEW (ROPE) — From a civilizational perspective examining how human cognition actually processes language, morphological complexity appears to be a pure coordination mechanism with no net extraction. All learners face the same cognitive architecture constraints; morphological systems are solutions to communication efficiency problems, not extractive hierarchies. Complexity is symmetrical — no agent extracts from another, the constraint simply reflects the structure of how language encodes and transmits information. However, the structural data contradicts this: victims exist (morphologically complex learners), suppression is high, and institutional gatekeeping appears in the base properties, suggesting the analytical rope classification misses the asymmetric extraction that emerges at learner and community levels.
constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(l2_morphological_acquisition_complexity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(l2_morphological_acquisition_complexity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(l2_morphological_acquisition_complexity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(l2_morphological_acquisition_complexity, TR),
    TR >= 0.70.

:- end_tests(l2_morphological_acquisition_complexity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting three components: (1) Cognitive suppression from the absolute complexity of morphological paradigms (genuine bottleneck); (2) Institutional suppression from the gatekeeping function of morphological proficiency certification (social bottleneck); (3) Asymmetric benefit accruing to native speakers and language institutions maintaining the gatekeeping. The value has increased over the interval (0.42 → 0.58) as CEFR and proficiency certification have made morphological accuracy a high-stakes requirement, turning a natural cognitive constraint into an extraction mechanism. Suppression (0.65): High, from cognitive demands (learners cannot reduce the morphological paradigm complexity without losing semantic/grammatical distinctions native speakers maintain), institutional demands (certification bodies require near-native accuracy), and social closure (communities maintain the complexity as native-speaker advantage marker). Theater_ratio (0.48): Moderate and declining. Traditional grammar pedagogy exhibits high theater (explicit paradigm memorization has low correlation with actual acquisition), but pedagogical innovation has reduced theater ratio as corpus-based and comprehensible input methods replace explicit rule-teaching. The declining theater ratio reflects genuine method improvement, not constraint degradation — the constraint remains high-extraction even as pedagogy becomes more functional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a cognitive bottleneck (morphological complexity is genuinely harder to process than morphologically simple structure) becomes an extraction mechanism through institutional gatekeeping. The learner sees a snare (trapped in suppression with no alternative). The cognitive system sees rope (morphology efficiently coordinates information). The language community sees tangled rope (both coordination and extraction). The pedagogical innovators see scaffold (solvable through method innovation and computational support). The traditional grammar institution sees itself as rope (coordination of learning through explicit instruction) but externally appears as piton (performative ritual). The analytical observer risks seeing rope (universal cognitive constraint) but the structural data reveals asymmetric extraction: gatekeeping institutions benefit from maintaining high morphological barriers; learners from morphologically rich L1s have lower d values (less extraction because they transfer knowledge) than learners from morphologically sparse L1s; low-resource language communities face higher suppression because scaffolding research is concentrated on majority languages. The perspectival gap is between the cognitive rope (morphology coordinates information) and the institutional snare (complexity gates proficiency).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each agent is determined by their power level, exit options, and relationship to the extraction flow. Morphologically complex learners are trapped (no exit from the cognitive complexity) with powerless status — their d approaches 1.0, producing high experienced extraction chi. Cognitive processing efficiency has institutional power and arbitrage exit (the processing system can switch to non-morphological languages) — their d approaches 0.0, producing negative or near-zero chi (they benefit, not extract). Language communities are moderate power with constrained exit (could simplify morphology but at communication efficiency cost) — their d is mid-range (0.5-0.6), producing moderate chi reflecting both coordination and extraction. Pedagogical innovators are organized power with mobile exit (can shift methods if old approaches fail) — their d is low-to-moderate reflecting both beneficiary status (they drive institutional change) and victim status (constrained by institutional inertia). Grammar institutions have institutional power with arbitrage exit (can shift pedagogical approach) — their d is very low, but their chi appears high because they enforce suppression through gatekeeping. The directionality chain creates asymmetry: those with low d (beneficiaries) experience minimal extraction; those with high d (trapped victims) experience maximum; those mid-range experience the tangled rope's mixed experience.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying the distinction between coordination and extraction in morphological systems. Coordination function is real: morphology packages information efficiently, reducing lexical inventory and enabling predictive comprehension. Extraction function is real: the high barrier to L2 competence preserves native-speaker status advantage and institutional gatekeeping power. Neither function is an illusion covering the other. The constraint is genuinely tangled rope: both functions coexist structurally. The tension appears in the measurements: extractiveness increases as institutional gatekeeping intensifies (CEFR certification, proficiency-indexed hiring), while theater_ratio decreases as pedagogical innovation makes methods more functional. The piton perspective correctly identifies that traditional grammar pedagogy is performative, but this performativity is layered on top of a real constraint, not replacing it. The scaffold perspective correctly identifies that alternative methods can reduce suppression (pedagogical innovation) and create a sunset path (computational morphology), but this path requires organizational power to implement — the scaffold exists only for organized agents. The snare perspective correctly identifies that individual learners face irreducible suppression and high extraction. The mandatrophy is resolved: all perspectives are structurally accurate from their positions. The constraint is simultaneously coordination (rope), extraction (snare), hybrid (tangled rope), degraded ritual (piton), and solvable (scaffold) depending on the observer's structural position and power to act.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    morphological_complexity_transferability,
    'Is morphological complexity in the L2 actually constraining, or is the constraint primarily a learner-internal artifact of cognitive processing order and L1 interference?',
    'Comparative acquisition studies: track learning curves for morphologically identical structures in typologically different L1 backgrounds; measure whether L1-to-L2 morphological distance predicts acquisition timeline or whether complexity is absolute',
    'If learner-internal/L1-dependent: constraint is partially rope (coordination adapted to L1 structure, not inherent extraction). If absolute: constraint is more snare-like (all morphologically complex learners face genuine suppression regardless of L1 background). Current evidence suggests mixed — some structures show L1 interference patterns, others show absolute complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(morphological_complexity_transferability, empirical, 'Whether morphological complexity is absolute or L1-dependent').

omega_variable(
    communicative_adequacy_threshold,
    'What level of morphological precision is actually required for native-like communication in real-world contexts, versus how much precision the institutional education system demands?',
    'Discourse analysis: measure error tolerance in natural conversation (do native speakers correct morphological errors?); compare error rates in fluent L2 speakers to explicit morphological knowledge levels; identify which morphological categories are actually necessary vs pedagogically emphasized',
    'If adequacy threshold is much lower than institutional demand: the constraint is largely extractive (suppression and complexity are artificially maintained for gatekeeping). If threshold aligns with demand: constraint has genuine coordination function. Evidence suggests learners achieve communicative competence at ~40-60% morphological accuracy, while institutional proficiency demands >90%.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(communicative_adequacy_threshold, empirical, 'Discrepancy between communicative adequacy and institutional precision demands').

omega_variable(
    scaffolding_mechanism_effectiveness,
    'Do corpus-based, comprehensible input, and task-based pedagogies actually reduce the suppression from morphological complexity, or do they merely postpone the problem until learners need productive accuracy?',
    'Longitudinal learning studies: measure acquisition trajectories for learners taught via different methods; assess whether corpus-based approaches reduce explicit memory burden or merely shift it temporally; track whether comprehensible input learners plateau before achieving morphological accuracy',
    'If effective: scaffold perspective is correct and sunset is real — alternative methods genuinely reduce the constraint''s force. If ineffective: pedagogical innovation is performative, and the constraint remains a structural snare regardless of method. Current evidence is mixed — implicit methods show different learning curves but eventually require explicit consolidation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_mechanism_effectiveness, empirical, 'Whether alternative pedagogical methods genuinely reduce morphological complexity suppression').

omega_variable(
    computational_morphology_deployment_barrier,
    'Will learner-facing computational morphological analysis tools (morphological analyzers, morpheme-aware spaced repetition systems) actually become deployed and usable, or will they remain research artifacts?',
    'Adoption tracking: measure integration of morphological analyzers into language learning platforms (Duolingo, Memrise, classroom systems); assess whether tool usability allows learners to bypass memorization or creates new scaffolding overhead',
    'If deployment succeeds: the scaffold sunset mechanism is real and extractiveness should decline generationally as tools mature. If tools remain niche research products: the sunset is aspirational, not structural, and the constraint persists at high extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(computational_morphology_deployment_barrier, empirical, 'Whether computational morphological tools will achieve real-world deployment and usability').

omega_variable(
    native_speaker_gatekeeping_intentionality,
    'Do native speakers and language institutions actively maintain morphological complexity as a gatekeeping mechanism, or is the extraction incidental to the constraint''s coordination function?',
    'Institutional analysis: examine language academy policies, professional certification requirements, and native-speaker attitudes toward morphological simplification proposals; assess whether resistance to morphological reform correlates with status preservation or with genuine communication requirements',
    'If intentional gatekeeping: the constraint is more snare-like than tangled_rope — the extraction mechanism is primary and coordination is secondary. If incidental: the constraint is more genuinely tangled_rope — extraction emerges from coordination without explicit enforcement. Evidence shows mixed intentionality — some gatekeeping (professional certification demands precise morphology), some incidental extraction (native speakers don''t perceive morphology as difficult because they learned it implicitly).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_gatekeeping_intentionality, conceptual, 'Whether morphological gatekeeping is intentional or incidental to coordination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(l2_morphological_acquisition_complexity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(l2morph_tr_t0, l2_morphological_acquisition_complexity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(l2morph_tr_t10, l2_morphological_acquisition_complexity, theater_ratio, 10, 0.48).
narrative_ontology:measurement(l2morph_tr_t20, l2_morphological_acquisition_complexity, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(l2morph_be_t0, l2_morphological_acquisition_complexity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(l2morph_be_t10, l2_morphological_acquisition_complexity, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(l2morph_be_t20, l2_morphological_acquisition_complexity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(l2_morphological_acquisition_complexity, information_standard).
narrative_ontology:boltzmann_floor_override(l2_morphological_acquisition_complexity, 0.12).
narrative_ontology:affects_constraint(l2_morphological_acquisition_complexity, l1_transfer_asymmetry).
narrative_ontology:affects_constraint(l2_morphological_acquisition_complexity, language_prestige_hierarchy).
narrative_ontology:affects_constraint(l2_morphological_acquisition_complexity, proficiency_certification_gatekeeping).

% DUAL FORMULATION NOTE:
% L2 morphological acquisition complexity is downstream of linguistic typology (why do some languages have richer morphology?) and upstream of language prestige hierarchies and proficiency certification systems. The constraint story focuses on the learning-time asymmetry (morphologically rich languages impose greater L2 burden). Related constraints include L1 transfer effects (learners with morphologically rich L1s face lower barriers due to structural similarity) and certification gatekeeping (institutions use morphological accuracy as a prestige-preservation mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(l2_morphological_acquisition_complexity, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
