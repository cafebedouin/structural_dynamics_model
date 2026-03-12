% ============================================================================
% CONSTRAINT STORY: escape_belief_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_escape_belief_snare, []).

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
 *   constraint_id: escape_belief_snare
 *   human_readable: The Escape Belief Snare: Secondary Suffering from Avoidability Narratives
 *   domain: philosophy_of_suffering/existential_psychology/moral_philosophy
 *
 * SUMMARY:
 *   The escape belief snare operates through a structural inversion: the
 *   promise that suffering is avoidable creates secondary suffering
 *   (self-blame, expectation violation, identity distortion) that often
 *   exceeds the primary suffering the promise claims to address. This
 *   constraint is downstream of the suffering_ontology_mountain (the
 *   irreducibility of certain forms of suffering) but represents a distinct
 *   extractive mechanism: the cultural and commercial apparatus that profits
 *   from denying that ontological reality. The constraint has intensified
 *   over a 30-year interval (roughly 1995-2025) as the therapeutic industrial
 *   complex expanded, positive psychology became institutionalized, and
 *   self-help content achieved cultural saturation through digital media. The
 *   theater_ratio (0.75) reflects that much of the 'help' offered is
 *   performative: interventions are marketed as transformative but deliver
 *   marginal or temporary effects, with failure attributed to insufficient
 *   client commitment rather than false promise. The constraint exhibits all
 *   six DR types from different perspectives, making it a diagnostic exemplar
 *   for how identity-lock mechanisms operate at scale.
 *
 * KEY AGENTS:
 *   - Individuals Seeking Escape: Primary victim (powerless/identity_locked) — identity constituted through the escape project; cannot exit without abandoning self-concept as 'someone working on themselves'
 *   - Chronic Condition Sufferers: Primary victim (powerless/trapped) — materially trapped by medical dependency; avoidability narrative adds self-blame to unavoidable physical suffering
 *   - Therapeutic Industrial Complex: Primary beneficiary (institutional/arbitrage) — profits from perpetual engagement; extraction mechanism invisible from this position
 *   - Self-Help Publishers: Secondary beneficiary (institutional/arbitrage) — monetizes the avoidability narrative through content production
 *   - Wellness Industry Actors: Secondary beneficiary (institutional/arbitrage) — adjacent market actors (supplements, retreats, coaching) who benefit from the same cultural frame
 *   - Ethical Therapists: Mixed position (moderate/constrained) — embedded in extractive industry but providing genuine coordination; aware of the false promise but economically dependent
 *   - Existential Psychology Movement: Organized resistance (organized/mobile) — building alternative frameworks that reject avoidability narrative; sees constraint as temporary with cultural sunset
 *   - Positive Psychology Establishment: Degraded legitimator (institutional/arbitrage) — academic infrastructure that legitimized the narrative; sees own research as increasingly performative
 *   - Analytical Observer: Civilizational view (analytical/analytical) — recognizes both genuine coordination (some suffering IS reducible) and asymmetric extraction (secondary suffering exceeds primary)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(escape_belief_snare, 0.58).
domain_priors:suppression_score(escape_belief_snare, 0.68).
domain_priors:theater_ratio(escape_belief_snare, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(escape_belief_snare, extractiveness, 0.58).
narrative_ontology:constraint_metric(escape_belief_snare, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(escape_belief_snare, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(escape_belief_snare, snare).
narrative_ontology:human_readable(escape_belief_snare, "The Escape Belief Snare: Secondary Suffering from Avoidability Narratives").
narrative_ontology:topic_domain(escape_belief_snare, "philosophy_of_suffering/existential_psychology/moral_philosophy").

domain_priors:requires_active_enforcement(escape_belief_snare).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(escape_belief_snare, therapeutic_industrial_complex).
narrative_ontology:constraint_beneficiary(escape_belief_snare, self_help_publishers).
narrative_ontology:constraint_beneficiary(escape_belief_snare, wellness_industry_actors).
narrative_ontology:constraint_victim(escape_belief_snare, individuals_seeking_escape).
narrative_ontology:constraint_victim(escape_belief_snare, therapy_seeking_populations).
narrative_ontology:constraint_victim(escape_belief_snare, chronic_condition_sufferers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL SEEKING ESCAPE (SNARE) — Identity-locked rather than materially trapped: the agent is structurally mobile (could stop consuming self-help content, could exit therapeutic relationships) but their identity has become constituted through the escape project itself. Exit would require abandoning the self-concept as 'someone who is working on themselves' and accepting suffering as non-negotiable, which is unthinkable from within the frame. The constraint extracts through perpetual deferral: each failed intervention is reframed as insufficient commitment rather than false promise. Maximum experienced extraction because the identity lock prevents the agent from recognizing the extraction mechanism.
constraint_indexing:constraint_classification(escape_belief_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: CHRONIC CONDITION SUFFERER (SNARE) — Materially trapped by medical dependency and economic constraints. The avoidability narrative adds self-blame to unavoidable physical suffering: 'If I just tried harder, thought more positively, found the right protocol, I could escape this.' Cannot exit the constraint because they cannot exit the underlying condition, and the cultural saturation of escape narratives makes the secondary suffering (self-blame, expectation violation) inescapable. High extraction from both the material condition and the ideological overlay.
constraint_indexing:constraint_classification(escape_belief_snare, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THERAPEUTIC INDUSTRIAL COMPLEX (ROPE) — Primary beneficiary with arbitrage exit options. Experiences the constraint as pure coordination: they are solving the legitimate problem of helping people reduce suffering. The extraction mechanism is invisible from this position because the business model depends on perpetual engagement (suffering that is always almost-but-not-quite resolved). Can exit to adjacent markets (wellness, coaching, pharmaceuticals) if one sector becomes saturated. Net beneficiary — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(escape_belief_snare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ETHICAL THERAPIST (TANGLED ROPE) — Constrained by professional norms, licensing requirements, and economic dependency on client retention, but also genuinely providing coordination (some therapeutic interventions do reduce suffering). Experiences the constraint as mixed: aware that the 'you can escape all suffering' narrative is false and harmful, but embedded in an industry that profits from that narrative. Cannot fully exit without abandoning their profession, but has enough agency to resist the most extractive practices. Moderate extraction — bears some cost (ethical compromise, complicity) but also derives benefit (livelihood, genuine helping).
constraint_indexing:constraint_classification(escape_belief_snare, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXISTENTIAL PSYCHOLOGY MOVEMENT (SCAFFOLD) — Organized agents (existential therapists, Stoic philosophy revival, Buddhist psychology practitioners) building alternative frameworks that explicitly reject the avoidability narrative. See the constraint as temporary: as cultural literacy around existential acceptance grows, the escape belief loses its grip. Sunset mechanism is cultural — estimated 20-40 years for existential frameworks to achieve mainstream penetration and displace the avoidability narrative in therapeutic discourse. Low effective extraction because this coalition has exit options (can practice outside the mainstream therapeutic model) and sees a clear path to resolution.
constraint_indexing:constraint_classification(escape_belief_snare, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: POSITIVE PSYCHOLOGY ESTABLISHMENT (PITON) — The academic infrastructure that legitimized the avoidability narrative through 'science of happiness' research. Theater ratio is high: much of the research is methodologically weak (self-report bias, publication bias, replication failures), but the institutional apparatus persists through inertia and funding momentum. The establishment sees its own research as increasingly performative — maintained because the cultural demand for optimism-as-science remains strong, not because the empirical foundations are solid. Piton classification derives from theater gate and institutional inertia, not from high experienced extraction.
constraint_indexing:constraint_classification(escape_belief_snare, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/universal perspective, the constraint exhibits both genuine coordination (some suffering IS reducible through intervention) and asymmetric extraction (the avoidability narrative creates secondary suffering that exceeds the primary suffering it claims to address). The analytical position recognizes that the constraint is NOT a mountain (suffering's irreducibility is real, but the escape belief is a contingent cultural formation, not a natural law) and NOT pure extraction (some therapeutic interventions genuinely help). Tangled Rope classification reflects the structural ambiguity: the constraint coordinates real help while extracting through false promise.
constraint_indexing:constraint_classification(escape_belief_snare, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(escape_belief_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(escape_belief_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(escape_belief_snare, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(escape_belief_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(escape_belief_snare, TR),
    TR >= 0.70.

:- end_tests(escape_belief_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through three mechanisms: (1) economic extraction (perpetual consumption of therapeutic services and self-help content), (2) psychological extraction (self-blame and expectation violation create secondary suffering), and (3) temporal extraction (years or decades spent in the escape project that could have been spent in acceptance-based adaptation). The value reflects that the extraction is substantial but not total — some individuals do benefit from therapeutic intervention, and some suffering is genuinely reducible. The increase from 0.35 to 0.58 over the interval reflects cultural saturation: as the avoidability narrative became ambient ideology rather than opt-in belief system, exit options narrowed and extraction intensified. Suppression (0.68): High. Significant barriers to exit include: (1) identity fusion (the escape project becomes constitutive of self-concept), (2) cultural saturation (avoidability narrative is ambient in media, education, healthcare), (3) economic dependency (therapeutic relationships create financial and emotional sunk costs), (4) social reinforcement (peer groups organized around self-improvement), and (5) epistemic closure (failure is attributed to insufficient commitment, not false promise, preventing reality-testing). Suppression is not total because some agents (existential psychology movement, certain philosophical traditions) maintain alternative frameworks. Theater ratio (0.75): High. Much of the therapeutic and self-help apparatus is performative: interventions are marketed as transformative but deliver marginal effects; research is methodologically weak but institutionally legitimized; credentials and certifications proliferate without corresponding outcome improvements; the ritual of 'working on yourself' becomes more important than actual suffering reduction. The theater has increased over the interval as the industry professionalized and the positive psychology establishment provided academic cover for weak interventions.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single set of base properties, with identity-lock mechanisms playing a central role. Individuals seeking escape see pure extraction (Snare) because they are identity-locked: the constraint has become constitutive of their self-concept, making exit unthinkable. Chronic condition sufferers also see pure extraction (Snare) but are materially trapped rather than identity-locked: the avoidability narrative adds self-blame to unavoidable suffering. The therapeutic industrial complex sees pure coordination (Rope): they are solving the legitimate problem of helping people reduce suffering, and the extraction mechanism is invisible from their position. Ethical therapists see mixed coordination and extraction (Tangled Rope): aware that the avoidability narrative is false but embedded in an industry that profits from it. The existential psychology movement sees a temporary problem with a sunset (Scaffold): alternative frameworks are building cultural literacy around acceptance, and the escape belief will lose its grip as these frameworks achieve mainstream penetration. The positive psychology establishment sees its own degraded ritual (Piton): the research apparatus persists through institutional inertia despite weak empirical foundations. The analytical observer sees structural ambiguity (Tangled Rope): the constraint coordinates real help (some suffering IS reducible) while extracting through false promise (the avoidability narrative creates secondary suffering). No single type is 'the' answer — the presheaf over the observation site IS the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint exhibits strong directionality differentiation across agents. Individuals seeking escape are victims with identity_locked exit options: they are structurally mobile (could stop consuming self-help content) but functionally trapped because their identity is constituted through the escape project. The identity lock is the binding mechanism — exit would require abandoning the self-concept as 'someone who is working on themselves' and accepting suffering as non-negotiable, which is unthinkable from within the frame. This produces high d (victim + identity_locked → d ≈ 0.89) and correspondingly high experienced extraction. Chronic condition sufferers are victims with trapped exit options: materially constrained by medical dependency and economic barriers, with the avoidability narrative adding self-blame to unavoidable physical suffering. This produces maximum d (victim + trapped → d ≈ 0.95) and maximum experienced extraction. The therapeutic industrial complex is a beneficiary with arbitrage exit options: can shift between adjacent markets (therapy, coaching, wellness, pharmaceuticals) as demand shifts. This produces low d (beneficiary + arbitrage → d ≈ 0.05) and negative experienced extraction (the constraint subsidizes this agent). Ethical therapists are mixed: partly victims (constrained by professional norms and economic dependency) but also partly beneficiaries (derive livelihood from the system). This produces moderate d (mixed + constrained → d ≈ 0.55) and moderate experienced extraction. The existential psychology movement is organized with mobile exit options: can practice outside the mainstream therapeutic model and sees a clear sunset path. This produces low d (beneficiary of alternative framework + mobile → d ≈ 0.35) and low experienced extraction. The positive psychology establishment is institutional with arbitrage exit: maintains the academic legitimation apparatus but sees its own research as degraded. This produces low d (beneficiary + arbitrage → d ≈ 0.10) but the piton classification derives from the theater gate rather than from high extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR FOR IDENTITY-LOCK MECHANISMS: This constraint resolves the mandatrophy by showing that identity-lock exit options produce a distinct classification pattern. The identity-locked agent (individual seeking escape) sees Snare despite being structurally mobile, because the binding mechanism is cognitive rather than material. The materially trapped agent (chronic condition sufferer) also sees Snare but through a different mechanism (external barriers rather than internal identity fusion). The beneficiary (therapeutic industrial complex) sees Rope because the extraction runs toward them, not away from them. The mixed agent (ethical therapist) sees Tangled Rope because they experience both coordination and extraction. The organized resistance (existential psychology movement) sees Scaffold because they have exit options and see a sunset path. The degraded legitimator (positive psychology establishment) sees Piton because the apparatus persists through inertia despite low function. The analytical observer sees Tangled Rope because the constraint genuinely coordinates (some suffering is reducible) while genuinely extracting (secondary suffering from false promise). The mandatrophy is resolved by recognizing that all seven perspectives are legitimate structural readings, and the identity-lock mechanism is a real binding force distinct from material entrapment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_reducibility_threshold,
    'What proportion of human suffering is actually reducible through psychological/behavioral intervention versus structurally irreducible?',
    'Longitudinal outcome studies comparing intervention groups to control groups across diverse suffering types (grief, chronic pain, existential dread, social isolation); meta-analysis of effect sizes and durability',
    'If threshold > 70%: escape belief is mostly accurate, extraction is low, constraint reclassifies toward rope. If threshold < 30%: escape belief is mostly false, extraction is high, constraint remains snare. Current evidence suggests threshold ~40-50%, supporting tangled_rope from analytical perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_reducibility_threshold, empirical, 'Proportion of suffering reducible through intervention').

omega_variable(
    self_blame_causality,
    'Does the avoidability narrative cause self-blame, or does pre-existing self-blame make individuals susceptible to avoidability narratives?',
    'Longitudinal studies tracking self-blame levels before and after exposure to avoidability messaging; cross-cultural comparison of self-blame rates in cultures with vs without strong avoidability narratives',
    'If narrative causes self-blame: extraction mechanism is direct, constraint is snare from victim perspective. If self-blame precedes narrative: constraint is tangled_rope (coordinates pre-existing psychological pattern rather than creating it).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_blame_causality, empirical, 'Causal direction of self-blame and avoidability narrative').

omega_variable(
    expectation_violation_magnitude,
    'How much of the measured distress in therapy-seeking populations is primary suffering versus secondary suffering from expectation violation?',
    'Decomposition studies using structural equation modeling to separate variance attributable to underlying condition vs variance attributable to unmet expectations; comparison of distress levels in populations with vs without exposure to escape narratives',
    'If secondary suffering > 50% of total: extraction is severe, snare classification confirmed. If secondary suffering < 20%: extraction is moderate, constraint reclassifies toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expectation_violation_magnitude, empirical, 'Proportion of distress from expectation violation vs primary suffering').

omega_variable(
    identity_lock_reversibility,
    'Can individuals exit the escape-project identity frame through therapeutic intervention, or does the frame persist even after recognizing its falsity?',
    'Qualitative studies of individuals who have explicitly rejected the avoidability narrative; measurement of identity-frame persistence after cognitive recognition of the frame; comparison of exit rates across different therapeutic modalities (existential therapy vs CBT vs acceptance-based approaches)',
    'If frame is reversible: identity_locked exit option is temporary, constraint has lower long-term extraction. If frame persists: identity lock is structural, constraint extraction is durable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Reversibility of escape-project identity frame').

omega_variable(
    cultural_saturation_threshold,
    'At what level of cultural saturation does the avoidability narrative become inescapable (ambient ideology) versus remaining a choice (opt-in belief system)?',
    'Cross-cultural comparison of avoidability narrative prevalence and corresponding self-blame rates; historical analysis of cultural shifts in suffering narratives; measurement of ''ambient exposure'' levels in different media environments',
    'If already at saturation threshold: suppression is maximal, exit options are identity_locked or trapped for most agents. If below threshold: suppression is moderate, exit options remain constrained or mobile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_saturation_threshold, conceptual, 'Cultural saturation level of avoidability narrative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(escape_belief_snare, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(escape_belief_tr_t0, escape_belief_snare, theater_ratio, 0, 0.45).
narrative_ontology:measurement(escape_belief_tr_t10, escape_belief_snare, theater_ratio, 10, 0.6).
narrative_ontology:measurement(escape_belief_tr_t20, escape_belief_snare, theater_ratio, 20, 0.68).
narrative_ontology:measurement(escape_belief_tr_t30, escape_belief_snare, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(escape_belief_be_t0, escape_belief_snare, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(escape_belief_be_t10, escape_belief_snare, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(escape_belief_be_t20, escape_belief_snare, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(escape_belief_be_t30, escape_belief_snare, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(escape_belief_snare, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of suffering_ontology_mountain (the irreducibility of certain forms of suffering) but represents a distinct extractive mechanism: the cultural and commercial apparatus that profits from denying that ontological reality. The upstream constraint has ε ≈ 0.08 (mountain — suffering's irreducibility is a natural law); this constraint has ε = 0.58 (snare — the escape belief is a contingent cultural formation that extracts through false promise). The two constraints are linked but structurally distinct: one is about what suffering IS (ontology), the other is about what we are TOLD about suffering (ideology).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(escape_belief_snare, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
