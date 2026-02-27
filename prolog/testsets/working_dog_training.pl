% ============================================================================
% CONSTRAINT STORY: working_dog_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: social/labor/animal_welfare
 *
 * SUMMARY:
 *   Working dog training for specialized tasks (bomb detection, narcotics
 *   detection, search and rescue, police apprehension) creates a complex
 *   constraint structure with multiple structural relationships. The
 *   constraint imposes total behavioral control on the dog (breeding,
 *   socialization, operant conditioning) to generate reliable performance in
 *   high-stakes human contexts. From the dog's perspective, this is maximum
 *   suppression and extraction: the dog cannot refuse training, cannot
 *   negotiate task assignments, and cannot exit the constraint absent
 *   retirement. From the handler institution's perspective, the constraint is
 *   primarily a coordination mechanism — establishing reliable canine
 *   behavior for safety-critical tasks reduces coordination costs and enables
 *   predictable deployment. From the perspective of individuals identified by
 *   drug or explosive detection dogs, the constraint extracts identification
 *   without consent and creates asymmetric legal consequences. The constraint
 *   has increased in extractiveness over time as institutions have expanded
 *   canine task deployment and intensified training protocols, while theater
 *   ratio has remained relatively stable because validation focuses on
 *   handler credentials rather than empirical detection accuracy.
 *
 * KEY AGENTS:
 *   - Working Dogs: Primary victims (powerless/trapped) — undergo total behavioral control via selective breeding, early socialization, operant conditioning; cannot refuse training or exit constraint
 *   - Handler Institutions: Primary beneficiaries (institutional/arbitrage) — law enforcement, military, search-and-rescue organizations benefit from reliable canine labor; high exit optionality to alternative detection methods
 *   - Individual Handlers: Secondary actors (moderate/constrained) — implement training regimes, experience institutional enforcement, have constrained exit (cannot refuse assignments without career risk)
 *   - Identified Individuals: Secondary victims (moderate/trapped) — persons detected by drug/explosive dogs experience asymmetric extraction without consent or capacity to dispute alerts
 *   - Canine Training Standards Authority: Institutional maintenance (institutional/arbitrage) — professional certifications, validation protocols sustain training regime through regulatory grandfathering
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function (reliable behavior for safety) combined with asymmetric extraction from dog's perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(working_dog_training, 0.58).
domain_priors:suppression_score(working_dog_training, 0.72).
domain_priors:theater_ratio(working_dog_training, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(working_dog_training, extractiveness, 0.58).
narrative_ontology:constraint_metric(working_dog_training, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(working_dog_training, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(working_dog_training, snare).
narrative_ontology:human_readable(working_dog_training, "Training of Working Dogs for Specific Tasks").
narrative_ontology:topic_domain(working_dog_training, "social/labor/animal_welfare").

domain_priors:requires_active_enforcement(working_dog_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(working_dog_training, handler_institutions).
narrative_ontology:constraint_beneficiary(working_dog_training, human_beneficiaries).
narrative_ontology:constraint_victim(working_dog_training, working_dogs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% The dog experiences maximum extraction: total behavioral control, restricted autonomy, coercive conditioning through reward/punishment, high suppression (cannot refuse, cannot negotiate, cannot exit). The constraint extracts physical labor and psychological compliance without the dog's capacity to consent. No alternatives visible within the dog's agency.
constraint_indexing:constraint_classification(working_dog_training, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Law enforcement, military, search-and-rescue organizations experience the constraint as pure coordination: establishing shared training protocols enables effective deployment of canine labor. Beneficiary with high exit optionality — can adopt alternative detection methods, adjust training regimens, or retire dogs. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(working_dog_training, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Handler experiences mixed coordination and extraction. Training protocols solve the legitimate coordination problem of establishing reliable canine behavior for safety-critical tasks. But handlers also experience institutional enforcement: mandatory training curricula, performance metrics, career consequences for inadequate control. Constrained exit — cannot refuse to train assigned dog or adopt non-sanctioned methods without career risk.
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Individuals identified by drug or explosive detection dogs experience extraction without consent. Once identified, they bear legal consequences. Suppression is near-total: cannot dispute dog's identification, cannot opt out of detection regime, cannot know training protocols that produced the alert. High experienced extraction — structural victim with no escape option during the detection event.
constraint_indexing:constraint_classification(working_dog_training, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% Formal training standards, professional certifications, and validation protocols for working dogs serve a real coordination function (ensuring reliable behavior) but have become substantially performative. Many jurisdictions rely on certified handler-dog teams despite significant false positive/negative rates. Theater ratio (0.38) reflects that validation focuses on handler credentials and protocol adherence rather than empirical detection accuracy. Institutional inertia: alternatives (electronic detection, human scent specialists) exist but certification bodies maintain traditional dog-training standards largely through regulatory grandfathering.
constraint_indexing:constraint_classification(working_dog_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a civilizational view, working dog training exhibits genuine coordination function (establishing reliable behavioral control for safety-critical tasks) AND asymmetric extraction (from the dog's perspective, maximum coercion; from institutional perspective, low cost). The constraint is not a pure snare because handlers and institutions do derive coordination benefits — they can reliably predict dog behavior. But the beneficiaries (institutions) impose suppression on both victims (dogs and detected individuals) without consent mechanisms. Classification: Tangled Rope with strong snare characteristics when viewed from the dog's perspective.
constraint_indexing:constraint_classification(working_dog_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
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
 *   Extractiveness (0.58): Moderate-high. From the dog's perspective, the constraint represents near-total behavioral extraction — the dog's actions are entirely controlled via conditioning, breeding, and institutional assignment. However, from the institutions' perspective, the extraction is moderate because alternative detection methods exist and handler institutions can deploy them without catastrophic loss. The score reflects the weighted perspective: primary victim (dog) experiences high extraction, but beneficiary has significant exit optionality, yielding a mixed value. Theater ratio (0.38): Moderate-low. Working dog training includes genuine coordination elements (reliable behavioral protocols) but also performative elements (handler certifications, validation rituals that don't directly measure detection accuracy). The theater ratio reflects that formal validation focuses on training protocol adherence rather than empirical detection performance. Suppression (0.72): High. Dogs have no negotiation capacity, cannot refuse training, cannot modify behavioral objectives, and are legally property of their institutions. Humans identified by dogs cannot dispute alerts or opt out of detection regimes. The only agents with low suppression are handler institutions, which have arbitrage options.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival gap between the beneficiary (handler institutions) and victims (dogs and identified individuals). Handler institutions perceive the constraint as pure coordination (Rope) — solving the legitimate problem of generating reliable canine behavior for safety-critical tasks. The dog perceives pure extraction (Snare) — total behavioral control with no exit option. Individual handlers perceive mixed coordination and enforcement (Tangled Rope) — they solve coordination problems but face institutional coercion. Identified individuals perceive pure extraction (Snare) — identification without consent, with asymmetric legal consequences. The analytical observer sees genuine coordination function combined with asymmetric extraction structure (Tangled Rope), recognizing that both elements are real: the constraint does solve coordination problems, AND it imposes extraction from victims who did not consent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Working dogs occupy maximum victim status (trapped exit, total behavioral suppression) yielding d ≈ 0.95, mapping to high experienced extractiveness f(d) ≈ 1.42. Handler institutions occupy beneficiary status (arbitrage exit, institutional power) yielding d ≈ 0.05, mapping to negative or near-zero f(d) ≈ -0.12 — extraction flows toward them, not away. Individual handlers occupy constrained victim status (cannot refuse institutional training mandates) yielding d ≈ 0.70, mapping to moderate f(d) ≈ 1.10. Identified individuals occupy trapped victim status outside the coordination mechanism (no consent, no escape during detection event) yielding d ≈ 0.90, mapping to high f(d) ≈ 1.35. The piton perspective reflects that institutional validation bodies maintain the training regime through regulatory inertia and grandfathering despite alternatives existing.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by disambiguating the dual function: working dog training is genuinely a coordination mechanism (solving the problem of reliable canine behavior for safety-critical contexts) AND genuinely an extraction mechanism (from the dog's perspective, and secondarily from the perspective of non-consenting identified individuals). The beneficiaries (institutions) have every incentive to frame the constraint as pure coordination ('we need reliable dogs for public safety'), while victims (dogs) experience it as pure snare ('total behavioral control, no exit'). The Tangled Rope classification captures the truth: both elements are real. The constraint cannot be justified as pure coordination (ignores asymmetric extraction from dogs), nor can it be dismissed as pure snare (genuine coordination function exists and beneficiaries do solve a real problem). The classification forces the analytical question: Under what conditions does the coordination benefit justify the extraction cost from the dog? Current consensus (dog welfare standards, training certification requirements) implicitly assumes the answer is 'yes' — but the mandatrophy analysis shows this assumption is hidden in the classification, not justified by it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    detection_accuracy_threshold,
    'What level of false positive/negative rates in canine detection justifies continued use of the training regime versus alternatives?',
    'Empirical validation studies comparing certified dog-team performance to electronic detection, handler accuracy, and population prevalence; cost-benefit analysis of error rates against human liberty interests',
    'If accuracy < 70%: training regime becomes unjustifiable extraction. If accuracy > 90%: coordination function is genuine and snare classification may not apply to identified individuals. Current literature suggests 60-85% range depending on context.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(detection_accuracy_threshold, empirical, 'Threshold for detection accuracy justifying the training regime').

omega_variable(
    dog_welfare_counterfactual,
    'Would the dogs in the training regime experience better or worse welfare if the constraint were removed entirely?',
    'Longitudinal welfare studies of retired working dogs vs purpose-bred companion dogs vs feral dogs; ethological assessment of behavioral suppression, stress indicators, lifespan, injury rates',
    'If training provides higher welfare than alternatives available to these genetic lines: extraction classification weakens (constraint becomes protective). If training causes measurable welfare degradation: snare classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dog_welfare_counterfactual, empirical, 'Comparative welfare outcomes if the constraint were removed').

omega_variable(
    consent_proxy_adequacy,
    'Can breeding selection, early socialization, and positive reward protocols constitute meaningful proxy consent for the dog to the behavioral constraint?',
    'Philosophical analysis of consent frameworks for non-human agents; empirical assessment of whether selectively bred dogs exhibit preference alignment with training objectives or evidence of genuine autonomy within the constraint',
    'If proxy consent is adequate: classification shifts from snare toward rope/tangled rope (coordination with consent mechanisms). If proxy consent is insufficient: snare classification stands unambiguously.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_proxy_adequacy, conceptual, 'Whether breeding and socialization constitute adequate proxy consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(working_dog_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wdt_tr_t0, working_dog_training, theater_ratio, 0, 0.32).
narrative_ontology:measurement(wdt_tr_t5, working_dog_training, theater_ratio, 5, 0.35).
narrative_ontology:measurement(wdt_tr_t10, working_dog_training, theater_ratio, 10, 0.38).

% Extraction over time
narrative_ontology:measurement(wdt_be_t0, working_dog_training, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(wdt_be_t5, working_dog_training, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(wdt_be_t10, working_dog_training, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(working_dog_training, enforcement_mechanism).
narrative_ontology:affects_constraint(working_dog_training, drug_enforcement_detection).
narrative_ontology:affects_constraint(working_dog_training, border_security_screening).
narrative_ontology:affects_constraint(working_dog_training, police_use_of_force_authority).

% DUAL FORMULATION NOTE:
% Working dog training is a constraint family with multiple decomposable elements: the training regime itself (high extraction from dogs' perspective), the detection technology (moderate accuracy with institutional benefit), and the institutional enforcement structure (sustained through piton-like mechanisms). This story focuses on the training regime; sibling constraints address detection accuracy and institutional deployment decisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(working_dog_training, analytical, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
