% ============================================================================
% CONSTRAINT STORY: reflexive_awareness_paradox
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reflexive_awareness_paradox, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reflexive_awareness_paradox
 *   human_readable: Reflexive Awareness Paradox in Debiasing
 *   domain: epistemology/cognitive_science/philosophy_of_mind
 *
 * SUMMARY:
 *   The reflexive awareness paradox emerges when metacognitive awareness of
 *   cognitive biases and self-deception mechanisms becomes itself a tool for
 *   rationalization rather than correction. The constraint operates across
 *   therapeutic, educational, and professional contexts where insight into
 *   one's own cognitive processes is framed as the primary path to behavioral
 *   change. The structural tension is between the genuine coordination
 *   function of metacognitive frameworks (enabling communication about mental
 *   states, providing shared vocabulary for psychological phenomena, creating
 *   therapeutic alliance through collaborative exploration) and the
 *   extraction mechanism (meta-awareness substitutes for behavioral
 *   modification, adds cognitive load without improving decision quality,
 *   creates identity fusion that prevents recognition of the substitution).
 *   The constraint has degraded over the 30-year interval as metacognitive
 *   training has proliferated (mindfulness industry, debiasing workshops,
 *   executive coaching, therapeutic insight emphasis) while outcome evidence
 *   has remained weak or null. Theater ratio (0.78) reflects that much
 *   metacognitive practice is performative: elaborate self-examination
 *   rituals, bias identification exercises, and insight narratives are
 *   maintained despite limited behavioral change. The constraint is
 *   downstream of cognitive_efficiency_epistemic_cost (the brain's tendency
 *   to use heuristics creates the biases that metacognitive training claims
 *   to address) and rehearsal_identity_merger (the identity fusion mechanism
 *   that locks agents into metacognitive frameworks even when they don't
 *   work).
 *
 * KEY AGENTS:
 *   - Self-Aware Rationalizer: Primary victim (powerless/identity_locked) — agent whose self-concept is constituted through metacognitive practice; cannot exit because exit would require abandoning epistemic humility identity
 *   - Therapy Client: Secondary victim (moderate/constrained) — faces sunk cost barriers and relationship investment in insight-oriented approach; can technically exit to behavioral therapy but constrained by framework commitment
 *   - Clinical Psychology Field: Mixed position (organized/mobile) — professional community with genuine coordination function but also embedded extraction through insight-oriented framework dominance
 *   - Metacognitive Training Industry: Primary beneficiary (institutional/arbitrage) — captures revenue from debiasing workshops, executive coaching, mindfulness training; experiences constraint as pure coordination
 *   - Evidence-Based Practice Movement: Organized coalition (organized/mobile) — building alternative pathways through outcome measurement and behavioral intervention advocacy; sees constraint as temporary with sunset logic
 *   - Psychoanalytic Tradition: Degraded institutional framework (institutional/constrained) — maintains elaborate metacognitive apparatus despite weak outcome evidence; piton perspective reflects atrophied function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the paradox as inherent limit of recursive cognition rather than recognizing contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reflexive_awareness_paradox, 0.68).
domain_priors:suppression_score(reflexive_awareness_paradox, 0.72).
domain_priors:theater_ratio(reflexive_awareness_paradox, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reflexive_awareness_paradox, extractiveness, 0.68).
narrative_ontology:constraint_metric(reflexive_awareness_paradox, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(reflexive_awareness_paradox, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reflexive_awareness_paradox, snare).
narrative_ontology:human_readable(reflexive_awareness_paradox, "Reflexive Awareness Paradox in Debiasing").
narrative_ontology:topic_domain(reflexive_awareness_paradox, "epistemology/cognitive_science/philosophy_of_mind").

domain_priors:requires_active_enforcement(reflexive_awareness_paradox).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reflexive_awareness_paradox, epistemic_humility_narrative).
narrative_ontology:constraint_beneficiary(reflexive_awareness_paradox, metacognitive_training_industry).
narrative_ontology:constraint_beneficiary(reflexive_awareness_paradox, therapeutic_insight_frameworks).
narrative_ontology:constraint_victim(reflexive_awareness_paradox, actual_correction_capacity).
narrative_ontology:constraint_victim(reflexive_awareness_paradox, first_order_decision_quality).
narrative_ontology:constraint_victim(reflexive_awareness_paradox, behavioral_intervention_adoption).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SELF-AWARE RATIONALIZER (SNARE) — Identity-locked agent who has internalized metacognitive awareness as part of their self-concept ('I'm the kind of person who examines my biases'). This identity frame prevents recognition that meta-awareness itself has become a rationalization tool. Cannot exit because exit would require abandoning the epistemic humility identity that constitutes their self-image. Experiences maximum extraction: meta-awareness adds cognitive load without improving decision quality, but the identity lock prevents switching to simpler behavioral heuristics that would actually work.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: THE THERAPY CLIENT (SNARE) — Moderate power agent constrained by therapeutic framework investment (time, money, emotional commitment to insight-oriented approach). Can technically exit to behavioral therapy but faces sunk cost barriers and therapist relationship investment. Experiences high extraction: insight therapy promises self-understanding as the path to change, but meta-awareness of patterns often substitutes for behavioral modification. The constraint extracts resources (time, money, emotional energy) while delivering the experience of progress without measurable outcome improvement.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE CLINICAL PSYCHOLOGY FIELD (TANGLED ROPE) — Organized professional community with genuine coordination function (training standards, outcome measurement, ethical guidelines) but also embedded extraction. Benefits from insight-oriented framework dominance (longer treatment duration, higher status than behavioral approaches, intellectual complexity as professional boundary). Can exit to evidence-based behavioral protocols but faces institutional inertia and professional identity investment. Experiences mixed extraction: the metacognitive framework enables real therapeutic alliance and some genuine insight, but also sustains treatment modalities with weaker outcome evidence than behavioral alternatives.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: THE METACOGNITIVE TRAINING INDUSTRY (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as pure coordination: they are solving the legitimate problem of teaching people to think about thinking. Captures revenue from debiasing workshops, executive coaching, mindfulness training, and metacognitive skill development programs. Low effective extraction because they benefit from the constraint's existence and can pivot to alternative frameworks if this one loses market share. The industry sees itself as providing valuable epistemic tools, not as extracting from clients who would achieve better outcomes with simpler behavioral interventions.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE EVIDENCE-BASED PRACTICE MOVEMENT (SCAFFOLD) — Organized coalition (Cochrane Collaboration, APA Division 12, behavioral therapy advocates) building alternative pathways that bypass metacognitive theater. Sees the reflexive awareness paradox as a temporary coordination failure with a sunset: as outcome measurement becomes standard and behavioral interventions demonstrate superior effect sizes, the insight-oriented dominance will erode. Randomized controlled trials, treatment manuals, and outcome tracking are creating verification mechanisms that reveal when meta-awareness substitutes for behavioral change. Estimated sunset: 15-25 years for evidence-based norms to dominate clinical training and reimbursement structures.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PSYCHOANALYTIC TRADITION (PITON) — Institutional framework that has largely atrophied from its original function (treating neurosis through insight) but persists through cultural inertia, training institute infrastructure, and intellectual prestige. High theater ratio: the elaborate metacognitive apparatus (transference analysis, defense mechanism interpretation, unconscious motivation mapping) is maintained despite weak outcome evidence. The tradition sees its own process as degraded — practitioners acknowledge that classical analysis is rarely practiced, that outcomes are hard to measure, and that behavioral approaches often work faster — but the institutional structure persists because alternatives haven't fully replaced the cultural authority and intellectual framework it provides.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER / RECURSIVE COGNITION VIEW (MOUNTAIN) — From a civilizational/universal perspective, the reflexive awareness paradox appears as an inherent limit of recursive cognition: any system that models itself faces Gödelian incompleteness, and meta-awareness necessarily introduces new blind spots even as it illuminates old ones. This perspective sees the paradox as an immutable property of self-referential cognitive systems. However, the structural data contradicts this mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to recursive cognition' framing naturalizes what is actually a contingent therapeutic and educational practice. The paradox is not that meta-awareness cannot work in principle, but that specific institutional arrangements (insight-oriented therapy dominance, metacognitive training market incentives, professional identity investment) sustain frameworks where meta-awareness substitutes for behavioral change.
constraint_indexing:constraint_classification(reflexive_awareness_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reflexive_awareness_paradox_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(reflexive_awareness_paradox, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reflexive_awareness_paradox, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(reflexive_awareness_paradox, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(reflexive_awareness_paradox, TR),
    TR >= 0.70.

:- end_tests(reflexive_awareness_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant resources (time, money, cognitive effort, emotional energy) from agents who invest in metacognitive frameworks that promise self-understanding as the path to change. The extraction mechanism is the substitution of meta-awareness for behavioral modification: agents experience the subjective sense of progress through insight acquisition while actual decision quality and behavioral outcomes show minimal improvement. The value reflects that this is not pure extraction (some genuine insight does occur, therapeutic alliance has real value, metacognitive vocabulary enables useful communication) but the extraction is substantial and increasing over time. Suppression (0.72): High. Multiple mechanisms suppress alternatives: (1) Identity fusion — agents who have internalized metacognitive practice as part of their self-concept cannot recognize when meta-awareness itself becomes a rationalization tool. (2) Sunk cost — therapy clients and training participants face significant investment barriers to switching frameworks. (3) Professional norms — clinical psychology training emphasizes insight-oriented approaches, creating institutional momentum. (4) Market incentives — metacognitive training industry benefits from longer treatment duration and intellectual complexity. (5) Epistemic humility narrative — the cultural framing that 'examining your biases' is virtuous makes it difficult to question whether the examination itself is productive. Theater ratio (0.78): Very high. Much metacognitive practice is performative: elaborate self-examination rituals (journaling about cognitive distortions, bias identification exercises, transference analysis, mindfulness practice logs) are maintained despite weak outcome evidence. The theater has increased over the interval as metacognitive training has become more elaborate and widespread while behavioral change outcomes have not improved proportionally.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the gap between metacognitive awareness and behavioral change — appears as pure extraction (snare) from the victim perspectives, mixed coordination-extraction (tangled rope) from the organized professional community, pure coordination (rope) from the beneficiary industry, temporary problem with sunset (scaffold) from the evidence-based practice movement, degraded ritual (piton) from the psychoanalytic tradition, and immutable cognitive limit (mountain) from the analytical observer. The self-aware rationalizer and therapy client see snare because they bear the extraction directly and cannot exit. The clinical psychology field sees tangled rope because they experience both genuine coordination function (therapeutic alliance, shared vocabulary, training standards) and embedded extraction (insight-oriented dominance despite weaker outcome evidence). The metacognitive training industry sees rope because they are net beneficiaries. The evidence-based practice movement sees scaffold because they are building alternative pathways that will erode the constraint's extraction mechanism. The psychoanalytic tradition sees piton because they recognize their own degradation but persist through institutional inertia. The analytical observer sees mountain but this is a false summit — the paradox is not inherent to recursive cognition but is sustained by contingent institutional arrangements.
 *
 * DIRECTIONALITY LOGIC:
 *   The self-aware rationalizer is identity-locked (powerless/identity_locked) — their self-concept is constituted through metacognitive practice, making exit literally unthinkable from within their identity frame. They are a victim (meta-awareness adds cognitive load without improving decision quality) with no structural exit option, yielding very high d and maximum experienced extraction. The therapy client is constrained (moderate/constrained) — they face real but surmountable barriers to exit (sunk costs, relationship investment, framework commitment). They are also a victim (insight therapy extracts resources without proportional outcome improvement), yielding high d and high experienced extraction. The clinical psychology field is organized with mobile exit options but experiences mixed extraction — they are both beneficiaries (professional status, intellectual complexity, longer treatment duration) and victims (weaker outcome evidence than behavioral alternatives threatens professional legitimacy). The metacognitive training industry is institutional with arbitrage exit options and pure beneficiary status — they capture revenue from the constraint and can pivot to alternative frameworks if market demand shifts, yielding very low d and negative experienced extraction (they benefit from the constraint's existence). The evidence-based practice movement is organized with mobile exit options and sees the constraint as temporary — they are building alternative pathways and experience low extraction because they have agency and a clear exit strategy. The psychoanalytic tradition is institutional but constrained rather than arbitrage — they face institutional inertia and cannot easily pivot despite recognizing their own degradation, yielding moderate d. The analytical observer risks naturalizing the paradox as inherent to recursive cognition, but the structural data reveals this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that the reflexive awareness paradox is not 'either pure extraction or inherent cognitive limit' but rather a presheaf over observation sites. From the victim perspectives (identity-locked rationalizer, constrained therapy client), the constraint is a snare — meta-awareness substitutes for behavioral change and the identity lock or sunk costs prevent exit. From the beneficiary perspective (metacognitive training industry), the constraint is coordination — they are solving the legitimate problem of teaching people to think about thinking. From the organized professional community (clinical psychology field), the constraint is tangled rope — genuine coordination function exists alongside embedded extraction. From the evidence-based practice movement, the constraint is scaffold — a temporary coordination failure being resolved through outcome measurement and behavioral intervention advocacy. From the psychoanalytic tradition, the constraint is piton — a degraded ritual maintained through institutional inertia. From the analytical observer, the constraint appears as mountain (inherent limit of recursive cognition) but this is a false summit revealed by the structural data. The mandatrophy is not 'which type is correct?' but 'which perspective are you measuring from?' All six types are legitimate perspectival readings of the same structural data. The analytical observer's mountain is naturalization. The beneficiary's rope is their genuine experience. The scaffold is a real structural feature (evidence-based practice sunset). The piton is a real observation (psychoanalytic degradation). The snare is the powerless agent's structural reality. The tangled rope is the organized community's mixed experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metacognitive_accuracy_threshold,
    'At what level of metacognitive accuracy does awareness of bias actually enable correction rather than adding another rationalization layer?',
    'Longitudinal studies correlating metacognitive accuracy scores with behavioral change outcomes; identification of threshold effects where high metacognitive accuracy predicts improvement but moderate accuracy predicts stagnation or decline',
    'If threshold is low (accessible to most): the constraint is primarily institutional (training quality, therapeutic framework selection). If threshold is high (rare): the constraint has a genuine cognitive floor — most people cannot achieve sufficient metacognitive accuracy for awareness to help rather than harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metacognitive_accuracy_threshold, empirical, 'Metacognitive accuracy threshold for beneficial vs harmful awareness').

omega_variable(
    insight_vs_behavior_mechanism,
    'Is the insight-behavior gap a necessary feature of human cognition (knowing ≠ doing) or a contingent feature of insight-oriented therapeutic frameworks that don''t include behavioral implementation support?',
    'Comparison of insight therapy with behavioral implementation support vs insight therapy alone vs pure behavioral therapy; measurement of insight acquisition, behavioral change, and outcome improvement across conditions',
    'If necessary: mountain classification is correct — the paradox is inherent to human psychology. If contingent: the gap is an artifact of therapeutic framework design, and integrated approaches can bridge it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(insight_vs_behavior_mechanism, empirical, 'Whether insight-behavior gap is cognitive necessity or framework artifact').

omega_variable(
    meta_awareness_identity_fusion,
    'Does metacognitive training create identity fusion (self-concept becomes ''person who examines biases'') that prevents recognition when meta-awareness itself becomes a bias?',
    'Measurement of identity fusion with metacognitive practice; correlation with resistance to behavioral interventions; longitudinal tracking of whether metacognitive identity predicts stagnation in actual bias reduction',
    'If identity fusion is common: the constraint''s suppression mechanism is primarily cognitive (identity lock prevents exit). If rare: the constraint''s suppression is primarily institutional (professional norms, market incentives, training frameworks).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meta_awareness_identity_fusion, empirical, 'Whether metacognitive training creates identity lock').

omega_variable(
    debiasing_intervention_publication_bias,
    'Do published debiasing intervention studies overestimate effectiveness due to publication bias against null results and underreporting of meta-awareness backfire effects?',
    'Meta-analysis with publication bias correction (funnel plots, trim-and-fill, p-curve analysis); comparison of registered vs unregistered debiasing studies; file drawer analysis for unpublished negative results',
    'If publication bias is severe: the evidence base for metacognitive interventions is weaker than it appears, and the constraint''s extractiveness is higher (more resources invested in ineffective approaches). If minimal: the intervention effectiveness estimates are reliable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debiasing_intervention_publication_bias, empirical, 'Publication bias in debiasing intervention literature').

omega_variable(
    therapeutic_alliance_confound,
    'Is the measured effectiveness of insight-oriented therapy primarily due to therapeutic alliance quality rather than the metacognitive content, and would behavioral therapy with equivalent alliance quality produce superior outcomes?',
    'Randomized trials controlling for therapist warmth, empathy, and alliance quality across insight-oriented vs behavioral modalities; measurement of alliance quality as mediator vs moderator of treatment type effects',
    'If alliance is the primary mechanism: insight-oriented frameworks are extractive (they work despite the metacognitive content, not because of it, and behavioral approaches would work better with the same alliance). If metacognitive content adds value beyond alliance: the framework has genuine coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_alliance_confound, empirical, 'Therapeutic alliance as confound in insight therapy effectiveness').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reflexive_awareness_paradox, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reflex_aware_tr_t0, reflexive_awareness_paradox, theater_ratio, 0, 0.52).
narrative_ontology:measurement(reflex_aware_tr_t10, reflexive_awareness_paradox, theater_ratio, 10, 0.63).
narrative_ontology:measurement(reflex_aware_tr_t20, reflexive_awareness_paradox, theater_ratio, 20, 0.71).
narrative_ontology:measurement(reflex_aware_tr_t30, reflexive_awareness_paradox, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(reflex_aware_be_t0, reflexive_awareness_paradox, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(reflex_aware_be_t10, reflexive_awareness_paradox, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(reflex_aware_be_t20, reflexive_awareness_paradox, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(reflex_aware_be_t30, reflexive_awareness_paradox, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reflexive_awareness_paradox, identity_coordination).

% DUAL FORMULATION NOTE:
% The reflexive awareness paradox is downstream of cognitive_efficiency_epistemic_cost (the brain's heuristic tendencies create the biases that metacognitive training claims to address) and rehearsal_identity_merger (the identity fusion mechanism that locks agents into metacognitive frameworks). The paradox represents a distinct structural constraint with its own extractiveness value reflecting the career and resource asymmetry between metacognitive training providers and clients, separate from the upstream constraints' extractiveness values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reflexive_awareness_paradox, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
