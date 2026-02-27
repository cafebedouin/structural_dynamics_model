% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_extraordinary_narrative_shift, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: extraordinary_narrative_shift
 *   human_readable: The Narrative Framing of 'Extraordinary' Experience
 *   domain: social/psychological
 *
 * SUMMARY:
 *   The narrative framing of extraordinary experience describes how
 *   institutional authorities (credentialed mental health professionals,
 *   medical systems, legal frameworks) claim monopoly over interpreting
 *   experiences that fall outside ordinary consensus reality. An individual
 *   reports a transformative, transcendent, or anomalous experience — a
 *   visionary episode, past-life recall, spontaneous healing, contact with
 *   non-ordinary intelligences, profound synchronicity. The institutional
 *   apparatus responds by translating the experiencer's own narrative into
 *   its diagnostic categories: hallucination, delusion, dissociation,
 *   conversion disorder, or (in less severe cases) trauma-related intrusion
 *   or maladaptive coping. This constraint operates through narrative
 *   substitution. The direct experiencer's lived narrative — which may
 *   include profound meaning, integration, healing, or spiritual
 *   transformation — is systematically subordinated to the credentialed
 *   interpreter's pathology narrative. The experiencer loses epistemic
 *   authority over their own experience. The constraint exhibits strong
 *   theater properties: diagnostic rituals, assessment instruments, clinical
 *   interviews, and psychiatric formulation processes perform the authority
 *   of credentialed interpretation while often failing to explain the
 *   phenomena or predict outcomes. Alternative frameworks (spiritual
 *   traditions, indigenous psychology, somatic models, entheogenic research
 *   communities, narrative therapy) offer competing interpretations that
 *   honor the experiencer's narrative while contextualizing the experience
 *   differently. The constraint's extraction mechanism is the institutional
 *   denial of these alternatives' legitimacy in public/medical/legal
 *   discourse, creating a structural advantage for credentialed interpreters
 *   and a subordination of direct experiencers and alternative communities.
 *
 * KEY AGENTS:
 *   - Direct Experiencer: Primary victim (powerless/trapped) — loses epistemic authority over their own lived experience; trapped within institutional narrative frameworks
 *   - Alternative Interpretive Communities: Secondary victim (moderate/constrained) — spiritual traditions, indigenous knowledge systems, grassroots psychology communities whose narratives are systematically delegitimized
 *   - Credentialed Interpreters: Primary beneficiary (institutional/arbitrage) — monopolize authoritative narrative reframing; derive career legitimacy and institutional authority from narrative substitution
 *   - Diagnostic Apparatus: Institutional actor (institutional/arbitrage) — formal systems (DSM, ICD) that enforce narrative categories and gatekeep legitimate interpretation
 *   - Integrative Movements: Organized agents (organized/constrained) — trauma-informed care, somatic psychology, narrative therapy working to bridge institutional and alternative frameworks with sunset logic
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent epistemic hierarchies as inevitable features of how humans manage reality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.52).
domain_priors:suppression_score(extraordinary_narrative_shift, 0.58).
domain_priors:theater_ratio(extraordinary_narrative_shift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(extraordinary_narrative_shift, tangled_rope).
narrative_ontology:human_readable(extraordinary_narrative_shift, "The Narrative Framing of 'Extraordinary' Experience").
narrative_ontology:topic_domain(extraordinary_narrative_shift, "social/psychological").

domain_priors:requires_active_enforcement(extraordinary_narrative_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, institutional_narrative_authority).
narrative_ontology:constraint_beneficiary(extraordinary_narrative_shift, credentialed_interpreters).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, direct_experiencers).
narrative_ontology:constraint_victim(extraordinary_narrative_shift, alternative_interpretive_frameworks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DIRECT EXPERIENCER (SNARE) — Individual who has undergone an experience they understand as extraordinary. Trapped within a labyrinth of institutional narrative frameworks that authorize which interpretations count as legitimate. Cannot exit: their own lived experience is systematically subordinated to credentialed reinterpretation. Maximum experienced extraction because the constraint denies them epistemic authority over their own reality. No alternative narrative framework is available without severe social/professional cost.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ALTERNATIVE INTERPRETIVE COMMUNITY (TANGLED ROPE) — Groups offering non-institutional framings (spiritual traditions, indigenous knowledge systems, grassroots psychology communities). Constrained by institutional authority structures that delegitimize their narratives in public/medical/legal discourse, but also benefit from the constraint by maintaining boundary clarity and community identity. Significant extraction but not maximal — they have partial agency through parallel institutions and subcultural authority, though their framings are systematically subordinated at the official level.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDENTIALED INTERPRETERS (ROPE) — Psychiatrists, neuroscientists, psychologists, licensed therapists who monopolize authoritative narrative reframing. Experience the constraint as pure coordination: their institutional mandates require translating diverse experiences into standardized diagnostic categories and evidence-based frameworks. Benefits from first-mover narrative authority and the subordination of alternative interpretations. Can arbitrage between different institutional contexts (clinical, legal, academic). Net beneficiary — extraction flows toward this group through epistemic monopoly.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTEGRATIVE MOVEMENTS (SCAFFOLD) — Organized efforts (trauma-informed care, somatic psychology, narrative therapy, integrated medicine) to build bridges between institutional and alternative frameworks. See the extraordinary/ordinary binary as a temporary coordination failure with a sunset: as integrative models mature and gain institutional credibility, the enforcement requirement weakens. Suppression declines as alternative framings are incorporated into mainstream psychology and medicine. Sunset logic: 15-25 years for significant normalization of pluralistic narrative frameworks in clinical practice.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DIAGNOSTIC APPARATUS (PITON) — Formal psychiatric/psychological diagnostic systems (DSM, ICD) that categorize extraordinary experiences as mental illness, dissociation, hallucination, or delusion. The apparatus is substantially performative: clinicians apply diagnostic categories to experiences that do not neatly fit, maintain rituals of assessment despite poor predictive validity for some conditions, and perpetuate institutional gatekeeping through diagnostic authority. Theater ratio high because diagnosis often functions as narrative legitimation ritual rather than causal explanation. The system persists through institutional inertia despite mounting evidence that alternative frameworks (trauma models, psychosocial developmental models, somatic models) have equivalent or superior clinical outcomes.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, humans always have been and always will be required to choose between competing narrative frameworks for understanding extraordinary experience. The constraint is natural: interpretation itself requires selection among frameworks; no framework encompasses all experience; therefore, institutional narrative authority is an inevitable feature of how societies manage shared reality. However, the structural data contradicts the mountain classification — the engine will detect this as a false summit. The 'inevitability' naturalizes what is actually a contingent distribution of epistemic authority.
constraint_indexing:constraint_classification(extraordinary_narrative_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(extraordinary_narrative_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(extraordinary_narrative_shift, TR),
    TR >= 0.70.

:- end_tests(extraordinary_narrative_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The institutional constraint extracts significant epistemic value from direct experiencers by monopolizing narrative authority over their own experiences. The experiencer's meaning-making is replaced with the credentialed interpreter's pathology narrative, creating asymmetric power in defining what counts as 'real,' 'healthy,' or 'legitimate' experience. Extraction increases over time as diagnostic categories expand and institutional gatekeeping tightens around mental health discourse. Suppression (0.58): Moderate-high. Significant barriers prevent alternative interpretations: professional licensing requirements restrict who can offer competing narratives; medical insurance systems validate only institutional diagnoses; legal systems (involuntary commitment, custody disputes, disability determinations) enforce institutional narrative authority; stigma attaches to those who embrace alternative frameworks; and career costs punish professionals who legitimize non-institutional interpretations. But suppression is not total — alternative communities exist in protected niches (spiritual traditions, online communities, private practice settings), and some institutional spaces (narrative therapy, trauma-informed care) are gradually opening. Theater ratio (0.68): High and rising. Diagnostic assessment rituals perform authority more than they explain phenomena. Clinicians apply DSM categories to experiences that do not fit cleanly, maintain complex decision trees that require substantial social agreement to operate, conduct interviews that shape answers toward predetermined categories, and produce formulations that often fail to predict treatment response or long-term outcomes. The theater has increased over the measurement interval because diagnostic categories have proliferated (DSM-IV to DSM-5) and institutional requirements for documentation have intensified, expanding the performative apparatus required to maintain narrative authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits extreme perspectival divergence because the narrative substitution mechanism produces genuinely incompatible experiences depending on structural position. A clinician in a hospital sees themselves coordinating diverse experiences into a coherent diagnostic framework that enables treatment planning and insurance processing — they experience Rope. An experiencer of a visionary episode sees their own narrative authority systematically replaced with a pathology label that invalidates their experience as 'not real' — they experience Snare. A narrative therapist sees the same institutional categories as historically contingent constructs that can be decentered to honor the experiencer's own meaning-making — they experience Scaffold with sunset logic as pluralistic approaches gain credibility. A psychiatrist sees diagnostic categories as imperfect but necessary tools for clinical decision-making, maintaining the performative apparatus despite knowing it 'doesn't quite fit' — they experience Piton. A spiritual community sees institutional reframing as denying legitimate modes of understanding while their alternative frameworks remain available to those who know where to look — they experience Tangled Rope. The convergence of all six types from a single base structure demonstrates that the constraint's classification is entirely perspectival, not intrinsic.
 *
 * DIRECTIONALITY LOGIC:
 *   The pipeline derives directionality (d) from each agent's structural position. Credentialed interpreters are institutional/arbitrage — they benefit from narrative monopoly and can move across institutional contexts; their d is low (~0.15), producing negative effective extraction (chi) because the constraint subsidizes their authority. Direct experiencers are powerless/trapped — they cannot exit the institutional narrative framework without losing access to care, insurance, legal legitimacy; their d is high (~0.95), producing maximum effective extraction because they bear full cost of narrative subordination. Alternative communities are moderate/constrained — they cannot operate within institutional discourse but maintain parallel authority structures; their d is moderate (~0.65), producing significant but not maximal chi because they have partial agency through community institutions. Integrative movements are organized/constrained — they have agency through professional development and research programs but still operate within institutional constraints; their d is moderate (~0.55), producing chi in the Tangled Rope range. The perspectival gap emerges from this directionality distribution: the same constraint produces negative chi (beneficiary experience, Rope) for institutional actors and maximum positive chi (victim experience, Snare) for trapped direct experiencers.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by showing how narrative authority becomes an extractive mechanism when institutional frameworks claim monopoly over interpretation. The constraint avoids the naive mandatrophy (confusing Rope 'coordination of interpretation' with Snare 'monopoly on legitimacy') by distinguishing between: (1) the coordination function narratives serve (Rope from the credentialed perspective — frameworks enable clinical decision-making and social coordination), and (2) the extraction mechanism that emerges when institutional authority denies alternatives (Snare from the direct experiencer perspective — narrative monopoly subordinates lived meaning-making). The Tangled Rope classification acknowledges that the constraint has genuine coordination properties (helping people make sense of extraordinary experiences) AND genuine extraction properties (denying epistemic authority to direct experiencers and alternative communities). The Scaffold perspective confirms the distinction: if alternative frameworks were genuinely inferior, no sunset would be possible — the institutional narrative would be irreplaceable. The fact that integrative approaches are gradually incorporating alternative frameworks suggests that the extraction mechanism was distinguishable from the coordination function. The mandatrophy is resolved: the constraint is truly a hybrid, with coordination (legitimate need for narrative frameworks) entangled with extraction (institutional monopoly on legitimacy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    experiential_validity_threshold,
    'What constitutes sufficient evidence that an extraordinary experience is ''real'' independent of institutional narrative endorsement?',
    'Comparative analysis of outcome measures for experiencers whose narratives are institutionally validated vs those using alternative frameworks; longitudinal tracking of symptom resolution and functional recovery across narrative contexts',
    'If institutional validation is necessary for positive outcomes: credentialed interpreters'' authority is functionally justified. If outcomes are equivalent or superior in alternative frameworks: institutional monopoly is pure extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(experiential_validity_threshold, empirical, 'Whether extraordinary experiences require institutional narrative validation for health outcomes').

omega_variable(
    framework_incommensurability,
    'Are institutional and alternative narrative frameworks genuinely incommensurable or merely using different language for overlapping phenomena?',
    'Systematic mapping of diagnostic categories, symptom descriptions, and treatment protocols across frameworks; identification of cases where frameworks produce contradictory predictions or treatments',
    'If truly incommensurable: no coordination is possible, and the constraint is pure extraction (Snare). If translatable: frameworks are complementary, and the constraint is a coordination problem (Rope/Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_incommensurability, conceptual, 'Whether narrative frameworks are fundamentally incommensurable').

omega_variable(
    integrative_model_viability,
    'Can institutional credentialing systems genuinely incorporate alternative narrative frameworks without degrading to performative inclusion, or does institutional gatekeeping require maintaining hierarchy?',
    'Case studies of successful integration (trauma-informed care, somatic psychology adoption); analysis of whether integrated practitioners have equivalent epistemic authority or remain subordinate; longitudinal tracking of institutional resistance to framework expansion',
    'If genuine integration is possible: scaffold sunset is real, and constraint will naturally decline. If gatekeeping is structural: integrative movements represent false hope, and extraction will persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(integrative_model_viability, conceptual, 'Whether institutional integration of alternative frameworks can overcome gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(extraordinary_narrative_shift, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exnsh_tr_t0, extraordinary_narrative_shift, theater_ratio, 0, 0.52).
narrative_ontology:measurement(exnsh_tr_t10, extraordinary_narrative_shift, theater_ratio, 10, 0.62).
narrative_ontology:measurement(exnsh_tr_t20, extraordinary_narrative_shift, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(exnsh_be_t0, extraordinary_narrative_shift, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(exnsh_be_t10, extraordinary_narrative_shift, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(exnsh_be_t20, extraordinary_narrative_shift, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(extraordinary_narrative_shift, information_standard).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, psychiatric_diagnostic_authority).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, mental_health_treatment_legitimacy).
narrative_ontology:affects_constraint(extraordinary_narrative_shift, anomalous_experience_research_funding).

% DUAL FORMULATION NOTE:
% The narrative framing constraint is upstream of specific psychiatric diagnoses and mental health treatment legitimacy constraints. It establishes the epistemic framework within which those constraints operate. The constraint also affects research funding for anomalous experience study by controlling which frameworks receive institutional credibility and therefore research resources.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
