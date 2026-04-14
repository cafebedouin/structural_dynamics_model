% ============================================================================
% CONSTRAINT STORY: metacognitive_illusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_metacognitive_illusion, []).

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
 *   constraint_id: metacognitive_illusion
 *   human_readable: Metacognitive Illusion: The Trap of Introspective Authority
 *   domain: epistemology/cognitive_science/psychology
 *
 * SUMMARY:
 *   The metacognitive illusion is the structural constraint that agents have
 *   privileged, reliable introspective access to the causes, content, and
 *   nature of their own cognitive processes. This assumption underlies legal
 *   responsibility, therapeutic practice, moral accountability, and everyday
 *   self-understanding. Yet decades of cognitive science (confabulation,
 *   implicit bias, post-hoc rationalization, blindsight, dissociative
 *   disorders) demonstrate that introspective reports diverge systematically
 *   from measurable neural and behavioral processes. The constraint operates
 *   as a snare: individual cognizers are trapped by their own conviction that
 *   they understand themselves, while institutional gatekeepers benefit from
 *   this conviction. The suppression is high (0.72) because alternatives —
 *   neuroscientific explanation, behavioral prediction, third-party
 *   assessment — are systematically dismissed as 'reductionist' or 'missing
 *   the point' of conscious experience. The theater ratio (0.68) reflects
 *   that institutions maintain introspective authority through ritual
 *   (confessional frameworks, therapeutic listening, legal testimony) rather
 *   than through verification of accuracy. Over the past 15 years (the
 *   measurement interval), as neuroscience has accumulated evidence that
 *   introspection is a degraded measurement modality, the theater ratio has
 *   risen (performative maintenance has intensified) and extractiveness has
 *   increased (agents are paying higher costs to maintain the fiction).
 *
 * KEY AGENTS:
 *   - Individual Cognizer: Primary victim (powerless/trapped) — believes they have self-knowledge but is systematically deceived; no exit from introspective conviction
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — law, psychology, education, therapy all benefit from introspective-authority assumption; have exit through third-party evidence when convenient
 *   - Therapist/Clinician: Secondary victim (moderate/identity_locked) — trained to privilege client introspection; identity fused with therapeutic alliance based on introspective authority; structurally mobile but cognitively trapped
 *   - Cognitive Science Research Community: Organized victim (organized/constrained) — sees the problem (generational awareness of neuroscientific contradiction) but faces publication bias and career constraints
 *   - Legal System: Institutional actor (institutional/arbitrage) — maintains introspective-authority framework through precedent and procedural custom; sees own process as degraded but continues due to path dependence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as natural law; false summit risk
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(metacognitive_illusion, 0.58).
domain_priors:suppression_score(metacognitive_illusion, 0.72).
domain_priors:theater_ratio(metacognitive_illusion, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(metacognitive_illusion, extractiveness, 0.58).
narrative_ontology:constraint_metric(metacognitive_illusion, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(metacognitive_illusion, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(metacognitive_illusion, snare).
narrative_ontology:human_readable(metacognitive_illusion, "Metacognitive Illusion: The Trap of Introspective Authority").
narrative_ontology:topic_domain(metacognitive_illusion, "epistemology/cognitive_science/psychology").

domain_priors:requires_active_enforcement(metacognitive_illusion).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(metacognitive_illusion, institutional_gatekeepers).
narrative_ontology:constraint_victim(metacognitive_illusion, individual_cognizers).
narrative_ontology:constraint_victim(metacognitive_illusion, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL COGNIZER (SNARE) — The agent is trapped in the illusion that introspective access reveals the true nature of their own cognitive processes. High suppression: alternatives (neuroscientific explanation, behavioral observation, third-party assessment) are actively dismissed as irrelevant or reductionist. The cognizer bears the cost of misdirected self-understanding while institutions benefit from the maintenance of introspective authority. No exit option — the agent cannot stop being introspectively convinced of their own authority.
constraint_indexing:constraint_classification(metacognitive_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSTITUTIONAL GATEKEEPERS (ROPE) — Psychology, philosophy, law, and education all benefit from the assumption that individuals have reliable introspective access to their own cognition. This enables institutions to hold agents responsible based on reported mental states, intentions, and self-awareness. Institutions experience the constraint as coordination — the alternative (denying introspective authority entirely) would collapse accountability frameworks. Net beneficiary with arbitrage exit: institutions can shift burden to third-party verification when convenient.
constraint_indexing:constraint_classification(metacognitive_illusion, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THERAPIST/CLINICIAN (SNARE) — Structurally mobile (could adopt purely neuroscientific model) but identity-locked by professional training, institutional credentialing, and therapeutic alliance norms that depend on treating client self-report as primary data. High suppression: neuroscientific explanations threaten therapeutic legitimacy ('your brain made you do it' contradicts therapeutic agency). The clinician experiences extraction through administrative burden of privileging introspection over objective measures, yet cannot exit without abandoning professional identity.
constraint_indexing:constraint_classification(metacognitive_illusion, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 4: COGNITIVE SCIENCE RESEARCH COMMUNITY (TANGLED ROPE) — Genuine coordination function: shared focus on understanding cognition enables cumulative knowledge. But asymmetric extraction persists: funding, journal prestige, and career advancement reward papers that preserve introspective-authority assumptions, while neuroscience papers challenging those assumptions face publication bias and resource scarcity. The community has agency (organized) and can see the problem (generational timescale), but faces constrained exit: shifting research paradigms requires costly retraining and journal resistance.
constraint_indexing:constraint_classification(metacognitive_illusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGAL SYSTEM (PITON) — Criminal and civil law depend on introspective authority for mens rea, intent, and voluntary action. The legal system maintains this framework through institutional inertia (precedent, procedural custom) rather than because it reliably predicts behavior or accurately captures moral responsibility. Theater ratio is high: legal proceedings privilege introspective testimony ('I intended...', 'I knew...') while neuroscientific evidence of compromised decision-making is marginalized or dismissed. The system sees its own process as degraded (neuroscience contradicts introspective findings) but continues due to path dependence.
constraint_indexing:constraint_classification(metacognitive_illusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE SUMMIT MOUNTAIN READING) — From a civilizational/universal analytical view, introspective authority appears immutable: all cognizers necessarily have privileged access to their own mental states. This naturalizes what is actually a contingent institutional arrangement — the elevation of introspection over third-party evidence. The mountain classification fails structural gates: extractiveness (0.58) exceeds mountain threshold (0.25), and suppression (0.72) exceeds mountain threshold (0.05). The analytical observer risks replicating the same cognitive capture that traps individual cognizers — treating the institutional arrangement as natural law rather than contingent structure.
constraint_indexing:constraint_classification(metacognitive_illusion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(metacognitive_illusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(metacognitive_illusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(metacognitive_illusion, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(metacognitive_illusion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(metacognitive_illusion, TR),
    TR >= 0.70.

:- end_tests(metacognitive_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Individual cognizers pay costs through systematic self-misunderstanding — they make life decisions, seek therapy, plead guilty in court based on introspective reports that neuroscience shows are often false. The extraction is not total (Snare threshold is 0.66) because some introspective access is veridical and useful. Suppression (0.72): High. Neuroscientific alternatives are actively dismissed: 'neuroscience can't explain consciousness', 'brain mechanisms don't negate responsibility', 'introspection is my direct access and science is indirect'. These are not accidental disagreements but deliberate suppression of competing explanations. Theater ratio (0.68): High. Institutional practices (legal testimony, therapeutic listening, confessional frameworks, self-help introspection) are performed as if they reliably access mental causation, despite evidence they do not. The performative content has increased over 15 years as neuroscience has mounted evidence — institutions respond by intensifying ritual rather than updating. The rising theater_ratio from 0.45 to 0.68 reflects this intensification of performative maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The individual cognizer experiences high extraction (Snare) — trapped by introspective conviction. Institutional gatekeepers experience pure coordination (Rope) — introspective authority enables accountability systems that seem beneficial to them. The therapist experiences extraction masked by identity fusion (Snare + identity_locked) — they cannot exit their reliance on client introspection without ceasing to be 'a therapist'. The research community experiences coordination with asymmetric extraction (Tangled Rope) — genuine research progress accompanied by publication bias against neuroscientific conclusions. The legal system sees its own degradation (Piton) — maintaining introspective-authority procedures through institutional inertia. The analytical observer risks replicating the same trap (false summit Mountain) — treating the institutional arrangement as natural law rather than contingent structure. The gap between the snare victim and the rope beneficiary is maximal.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) values are derived from structural position relative to the extraction flow. Individual cognizers (powerless + trapped) have maximum d (~0.95), experiencing full target status: they bear costs of self-misunderstanding with no exit. Institutional gatekeepers (institutional + arbitrage) have minimum d (~0.05), experiencing full beneficiary status: they capture accountability authority while maintaining exit through third-party evidence when needed. Therapists (moderate + identity_locked) have high d (~0.85) derived from victim status despite structural mobility — the identity lock prevents exercise of exit capacity. The research community (organized + constrained) has moderate-high d (~0.55) reflecting that they are partly targets (publication bias) and partly agents (can organize resistance). Legal system (institutional + arbitrage) has low d (~0.10) reflecting institutional beneficiary status. The analytical observer (analytical + analytical) has d (~0.72) reflecting observer position: not fully targeted but observing from outside the constraint. All d values feed the sigmoid f(d) to produce experienced extractiveness (chi).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's type varies by structural position, not by any ambiguity in the constraint itself. The individual cognizer's snare classification is robust: high extraction (0.58 base), high suppression (0.72), high chi from powerless perspective. The institutional gatekeeper's rope classification is also robust: they perceive low extraction because they are net beneficiaries. The analytical observer's false summit mountain reading is NOT robust — it violates the structural gates (extractiveness exceeds 0.25 threshold, suppression exceeds 0.05 threshold) and reflects cognitive capture rather than genuine natural law. The mandate is to classify the constraint at the analytical level as a Snare with false-summit mountain readings from institutional positions: the constraint is extractive, not natural law. The theater ratio increasing from 0.45 to 0.68 indicates that institutional responses to neuroscientific challenge are performative (doubling down on introspective authority rather than integrating evidence) rather than substantive — the classic signature of institutional degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    introspective_vs_neuroscientific_access,
    'Does introspective access reveal fundamentally different information than third-party neuroscientific measurement, or does it reveal the same processes through a different modality with systematic biases?',
    'Longitudinal comparison of introspective reports with fMRI, behavioral measures, and external validation across decision-making tasks. Track divergences and their predictive power for actual behavior.',
    'If fundamentally different: introspective authority is partially justified (Rope from institutional perspective). If same processes with systematic bias: introspection is a degraded measurement modality (Snare from all perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(introspective_vs_neuroscientific_access, empirical, 'Whether introspection accesses fundamentally distinct information from neuroscience').

omega_variable(
    suppression_mechanism_structural_vs_cognitive,
    'Is the high suppression (0.72) driven by structural institutional barriers (legal systems, credentialing) or by cognitive capture within individual cognizers (identity fusion with introspective authority)?',
    'Examine post-education suppression trajectories: do agents trained in neuroscience but practicing in institutional contexts (law, psychology) retain introspective-authority assumptions? Do they show depressed rates of neuroscientific explanation in institutional settings?',
    'If structural: suppression persists as long as institutions require introspective testimony. If cognitive: suppression may degrade faster with education and identity shift. Affects exit strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_cognitive, empirical, 'Mechanism of suppression: institutional vs. internalized').

omega_variable(
    accountability_framework_collapse_risk,
    'If introspective authority is fully abandoned, can legal and moral accountability systems function without it, or do they collapse into pure neuroscientific determinism?',
    'Theoretical analysis: construct alternative accountability frameworks grounded in behavioral control, causal responsibility, and neurological capacity for inhibition rather than introspected intent. Pilot implementations in experimental justice systems.',
    'If alternative frameworks viable: institutional gatekeepers have path out (Scaffold perspective). If collapse risk: institutions will resist neuroscientific challenge indefinitely (Snare from institutional view). Determines whether escape from extraction is possible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(accountability_framework_collapse_risk, conceptual, 'Viability of accountability without introspective authority').

omega_variable(
    identity_lock_mechanism_in_clinicians,
    'Is the clinician''s identity lock (identity_locked exit option) resoluble through professional retraining, or does therapeutic identity (rooted in trust in client self-report) prevent adoption of neuroscientific frameworks?',
    'Track outcomes of therapists trained in neuroscientific models: do they adopt the framework clinically, or does therapeutic practice pressure them back toward introspective-authority assumptions? Measure therapeutic alliance quality with neuroscientific vs. introspective explanations.',
    'If lock is resoluble: clinicians can exit (constrained exit becomes possible). If lock persists: therapists remain trapped despite intellectual agreement (identity_locked persists). Determines whether institutional mediation of introspective authority can be disrupted from within.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_clinicians, empirical, 'Whether identity lock in therapeutic professionals is reversible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(metacognitive_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meta_tr_t0, metacognitive_illusion, theater_ratio, 0, 0.45).
narrative_ontology:measurement(meta_tr_t5, metacognitive_illusion, theater_ratio, 5, 0.58).
narrative_ontology:measurement(meta_tr_t10, metacognitive_illusion, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(meta_be_t0, metacognitive_illusion, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(meta_be_t5, metacognitive_illusion, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(meta_be_t10, metacognitive_illusion, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(metacognitive_illusion, identity_coordination).
narrative_ontology:affects_constraint(metacognitive_illusion, confabulation_narrative_immunity).
narrative_ontology:affects_constraint(metacognitive_illusion, therapeutic_alliance_dependency).
narrative_ontology:affects_constraint(metacognitive_illusion, legal_mens_rea_framework).

% DUAL FORMULATION NOTE:
% The metacognitive illusion is upstream of specific institutional constraints that depend on introspective authority: confabulation dynamics (how agents construct false narratives), therapeutic alliance (how therapists rely on client self-report), and legal mens rea (how courts rely on introspected intent). Each downstream constraint has its own extractiveness value reflecting domain-specific factors; this story captures the generic structural basis all three share.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(metacognitive_illusion, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
