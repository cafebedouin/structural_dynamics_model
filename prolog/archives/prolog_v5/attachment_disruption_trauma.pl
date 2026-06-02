% ============================================================================
% CONSTRAINT STORY: attachment_disruption_trauma
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attachment_disruption_trauma, []).

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
 *   constraint_id: attachment_disruption_trauma
 *   human_readable: Attachment Disruption Trauma
 *   domain: developmental_psychology/trauma
 *
 * SUMMARY:
 *   Attachment disruption trauma is a constraint structured through the
 *   rupture of the primary caregiver relationship during critical
 *   developmental periods. The trauma operates as a snare for the child
 *   victim (powerless, trapped) but exhibits all six DR types depending on
 *   observer position. The adult survivor often becomes identity-locked to
 *   the original trauma pattern, experiencing themselves as constitutively
 *   unable to escape despite structural mobility. The constraint persists
 *   through internalized working models of relationships as dangerous,
 *   intermittent reinforcement cycles (moments of connection followed by
 *   betrayal), and identity fusion with the role assigned by the perpetuator.
 *   The suppression mechanism combines material factors (childhood
 *   dependency) with cognitive factors (shame, learned helplessness, identity
 *   fusion). The measurements show increasing extractiveness over the
 *   interval (0.45 → 0.72) as the child develops greater agency but
 *   internalizes the trauma pattern more deeply. Theater increases (0.35 →
 *   0.68) as the transmission becomes normalized and invisible — the trauma
 *   reproduces itself through 'natural' parenting patterns and relationship
 *   choices rather than overt coercion. Therapeutic intervention offers a
 *   genuine exit pathway (scaffold) by targeting both structural barriers and
 *   identity-frame shifts. Institutional contexts reproduce the constraint at
 *   scale through normalization and inertia (piton).
 *
 * KEY AGENTS:
 *   - Child Victim: Primary powerless/trapped agent — bears full extraction cost through developmental harm, neurobiological dysregulation, internal working models of danger
 *   - Adult Survivor: Primary moderate/identity_locked agent — structurally mobile but cognitively captured through identity fusion with the trauma role
 *   - Perpetuator (Individual): Beneficiary in the child perspective (extraction mechanism), but in institutional context may be secondary to organizational logic
 *   - Perpetuator (Institutional): Powerful/arbitrage or constrained depending on whether institution is intentionally extractive or functionally dysfunctional
 *   - Therapeutic System: Organized agents providing structured exit — attachment-based family therapy, trauma-informed care, evidence-based protocols
 *   - Intergenerational Transmission: Institutional inertia mechanism — perpetuates constraint through normalized parenting patterns across generations
 *   - Analytical Observer: Risk of naturalizing contingent caregiving arrangements as developmental necessity rather than institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attachment_disruption_trauma, 0.68).
domain_priors:suppression_score(attachment_disruption_trauma, 0.72).
domain_priors:theater_ratio(attachment_disruption_trauma, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attachment_disruption_trauma, extractiveness, 0.68).
narrative_ontology:constraint_metric(attachment_disruption_trauma, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(attachment_disruption_trauma, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attachment_disruption_trauma, snare).
narrative_ontology:human_readable(attachment_disruption_trauma, "Attachment Disruption Trauma").
narrative_ontology:topic_domain(attachment_disruption_trauma, "developmental_psychology/trauma").

domain_priors:requires_active_enforcement(attachment_disruption_trauma).

% --- Structural relationships ---
narrative_ontology:constraint_victim(attachment_disruption_trauma, child_victim).
narrative_ontology:constraint_victim(attachment_disruption_trauma, developmental_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAUMATIZED CHILD (SNARE) — Trapped in the relationship that caused the trauma. Lacks material exit routes (dependent on caregiver for survival, shelter, food). Interpersonal and institutional barriers prevent leaving. Bears full cost of attachment disruption through developmental harm, internal working models of relationships as dangerous, and neurobiological dysregulation. Maximum extraction without meaningful coordination benefit.
constraint_indexing:constraint_classification(attachment_disruption_trauma, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ADULT SURVIVOR (TANGLED ROPE) — Structurally mobile (can physically leave, has income, legal protections) but identity-locked to the original relationship through internalized patterns, fused identity ('I deserve this treatment'), and cognitive capture. Simultaneously experiences genuine coordination elements (learned caregiving behaviors, loyalty as identity anchor) alongside extraction (relationship patterns perpetuate the trauma dynamic). Moderate extraction in new contexts because the binding is now primarily cognitive rather than material.
constraint_indexing:constraint_classification(attachment_disruption_trauma, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: THERAPEUTIC INTERVENTION (SCAFFOLD) — Organized agents (attachment-based family therapy, trauma-informed care, evidence-based intervention protocols) provide structured exit pathway from the constraint. Theater ratio is low — the intervention focuses on real relational change, not performative compliance. Has clear sunset: as the survivor develops secure attachment capacity and processes the trauma, the intervention winds down. Effective only when the survivor can achieve the breakthrough shift from identity_locked to mobile exit options.
constraint_indexing:constraint_classification(attachment_disruption_trauma, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PERPETUATOR / INSTITUTIONAL CONTEXT (ROPE) — If the perpetuator is an institution (residential facility, religious organization, state custody) rather than individual caregiver, the constraint appears as coordination from the institution's perspective: maintaining control over dependent populations, managing compliance, extracting compliant behavior through attachment disruption as a mechanism. The institution sees this as functional social control, not trauma — reframing extraction as coordination to minimize cognitive dissonance.
constraint_indexing:constraint_classification(attachment_disruption_trauma, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 5: INTERGENERATIONAL TRANSMISSION (PITON) — Attachment disruption trauma reproduces itself across generations through internalized working models, parenting scripts, and identity fusion. The perpetuation appears 'natural' (this is how families work, how relationships function) and persists through institutional inertia despite causing documented harm. Theater is moderate — the transmission is visible in parenting behavior and relationship patterns, but the underlying mechanism (identity lock) remains opaque to participants. Functionally degraded because it reproduces harm without any coherent coordination benefit at the generational level.
constraint_indexing:constraint_classification(attachment_disruption_trauma, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From an evolutionary or culturally naturalized perspective, attachment disruption might appear as an immutable feature of hierarchical caregiving: dominance hierarchies require compliance mechanisms, trauma bonds create behavioral lock-in, and vulnerability in childhood produces extractability across the lifespan. This perspective risks naturalizing what is actually a contingent institutional and developmental choice. The engine flags this as a false summit: attachment disruption is not a law of nature but a consequence of specific caregiving arrangements.
constraint_indexing:constraint_classification(attachment_disruption_trauma, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attachment_disruption_trauma_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attachment_disruption_trauma, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attachment_disruption_trauma, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attachment_disruption_trauma, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attachment_disruption_trauma, TR),
    TR >= 0.70.

:- end_tests(attachment_disruption_trauma_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts developmental capacity, secure attachment capacity, relational health, and behavioral autonomy from the child victim. The extraction continues into adulthood through internalized patterns. The value reflects that extraction is substantial (coded in neurobiology and identity) but not absolute — some survivors develop resilience, some achieve therapeutic breakthrough, some form secure attachments despite history. Not 0.85+ because exit is possible with intervention. Suppression (0.72): High. Multiple layers: material (childhood dependency, legal status, geographic isolation), institutional (normalized family structures, limited access to alternative attachment figures), psychological (shame, fear, learned helplessness, identity fusion). Suppression increases over developmental time as the child internalizes the trauma pattern as 'how relationships work.' Theater ratio (0.55): Moderate-high. The constraint operates partly through overt abuse/rupture (lower theater) but increasingly through internalized patterns and normalized family dynamics that appear 'natural' (higher theater). Therapeutic contexts introduce genuine functional intervention (lower theater) but also performance of trauma recovery (higher theater). The intergenerational view is highly theatrical — the transmission appears as destiny or natural parenting instinct rather than as a reproducible institutional pattern.
 *
 * PERSPECTIVAL GAP:
 *   The adult survivor's identity_locked classification is the diagnostic pivot. This agent has structural mobility (income, housing options, legal protections) that would permit exit classified as mobile or constrained. But identity fusion with the trauma role — internalized belief that they deserve the treatment, fear of abandonment, loyalty to the perpetuator, self-concept constituted through caregiver role — creates a perceptual lock: exit is unthinkable from within the internalized frame. The gap between structural capacity (mobile) and perceived capacity (trapped within identity frame) reveals that the binding mechanism is cognitive rather than material. This is why the classification is tangled_rope rather than snare: there is genuine coordination content (loyalty, caregiving identity, learned interdependence) alongside the extraction, AND the agent could perceive mutability if their identity frame shifted (unlike mountain perception of immutability). The therapeutic intervention targets precisely this gap: helping the survivor shift from identity_locked to mobile exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to extraction flow. The child victim is at maximum d (trapped, powerless, full target of extraction). The adult survivor has lower d than the child (identity-locked produces lower d than trapped due to structural mobility), but still on the target side. The perpetuator (individual) has d ≈ 0.0 (full beneficiary) if extraction is intentional, higher d if acting within institutional script. The therapeutic system has d ≈ 0.5 (symmetric costs and benefits; helping entails relational vulnerability for therapist). The intergenerational view distributes d across multiple agents: perpetuators of prior generation have low d (beneficiaries), current generation victims have high d (targets), future generation victims have even higher d (trapped in system they did not create). The institutional context shifts beneficiary identity: individual perpetuator may be constrained agent within institutional logic, with true beneficiary being the organization's operational success.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint's classification as snare for the child (pure extraction without coordination) versus tangled_rope for the adult survivor (mixed coordination-extraction with identity lock) versus scaffold for the intervention (temporary coordination failure with sunset) demonstrates that mandatrophy is resolved through perspectival decomposition. The constraint is not 'really' one type — it IS all types simultaneously from different structural positions. The snare classification is correct from the powerless/trapped child's perspective. The tangled_rope classification is correct from the moderate/identity_locked adult's perspective. The scaffold classification is correct from the organized/mobile therapeutic system's perspective. The mandatrophy was the false question 'which type is objectively correct?' The resolution is: each type is correct from its observer position. The constraint's extractive power derives from this perspectival slippage — the adult survivor mistakes their identity-locked tangled_rope for a continuing snare, maintaining the extraction mechanism through internalized belief rather than external force.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_structural_entrapment,
    'In the adult survivor perspective, is the primary binding mechanism cognitive (identity fusion) or material (economic/legal dependency recreated in adulthood)?',
    'Longitudinal tracking of exit attempts: Do survivors who achieve economic independence and legal autonomy still report inability to leave relationships exhibiting similar trauma patterns? If yes, binding is identity-locked. If material barriers alone explain continued contact, binding is trapped/constrained rather than identity-locked.',
    'If identity-locked: classification remains tangled_rope; intervention must address cognitive frame shift in addition to material exit. If trapped: reclassify as higher extraction; material barriers dominate and must be removed first. If constrained: moderate extraction with cost-based exit pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_entrapment, empirical, 'Whether adult survivor binding is cognitive or material').

omega_variable(
    institutional_perpetuation_extraction_vs_coordination,
    'When attachment disruption occurs in institutional contexts (residential care, detention, religious community), is the mechanism primarily extractive control or genuine coordination dysfunction?',
    'Comparison of institutional outcomes: Do institutional populations with attachment disruption show better behavioral compliance (extraction success) or worse developmental outcomes (coordination failure)? Do institutions with lower attachment disruption show better long-term institutional stability?',
    'If primarily extractive: perpetuator perspective is rope with clear beneficiary (institutional order). If primarily dysfunction: perpetuator perspective is piton (the institution has internalized false belief that attachment disruption is necessary for function). If mixed: confirms tangled_rope classification for institutional view.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_perpetuation_extraction_vs_coordination, empirical, 'Whether institutional attachment disruption is extraction or coordination failure').

omega_variable(
    suppression_internalization_trajectory,
    'What proportion of measured suppression (0.72) is structural (material barriers: parental dependency, legal status, geographic isolation) versus internalized (cognitive barriers: identity fusion, shame, learned helplessness that persist after structural barriers are removed)?',
    'Post-exit suppression persistence: Measure suppression indicators (relationship maintenance, behavioral compliance, communication patterns) before, during, and after exit from the primary trauma relationship. If suppression drops immediately after material exit, structural. If suppression persists despite material freedom, internalized. Most cases will show both components.',
    'If primarily structural: escape interventions (legal, economic, geographic) will rapidly reduce constraint. If primarily internalized: survivor carries suppression mechanism post-exit and will require extensive identity-frame intervention (therapy, community reintegration) to achieve true freedom. If both: sequential intervention strategy needed (remove structural barriers first, then address internalized mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_trajectory, empirical, 'Structural vs internalized proportion of suppression').

omega_variable(
    therapeutic_intervention_ceiling_effect,
    'Does trauma-informed attachment-based therapy reliably shift survivors from identity_locked to mobile exit options, or does identity fusion often persist despite therapeutic success on other dimensions?',
    'Comparison of therapy outcomes: Measure both symptom reduction (PTSD, anxiety, depression scores) and identity fusion persistence (Implicit Association Test for self-concept, relational pattern replication in new relationships). Do survivors with symptom improvement still exhibit identity-locked relationship choices?',
    'If therapy reliably breaks identity lock: scaffold sunset is realistic and intervention can be time-limited. If identity lock persists despite symptom improvement: scaffold sunset is delayed and may require indefinite maintenance support or repeated intervention cycles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(therapeutic_intervention_ceiling_effect, empirical, 'Whether therapy reliably breaks identity lock').

omega_variable(
    intergenerational_transmission_inevitability,
    'What proportion of attachment-disrupted children reproduce similar patterns with their own children? Is transmission inevitable absent intervention, or are there natural circuit-breakers?',
    'Longitudinal family cohort studies: Track parenting patterns in children raised with significant attachment disruption. Identify which develop secure attachment capacity with their own children (transmission interrupted) versus which repeat patterns (transmission continues). Correlate with protective factors (alternative attachment figures, therapeutic intervention, reflective capacity).',
    'If transmission is near-inevitable (>70%): intergenerational piton is accurately classified as stable institutional pattern. If transmission can be interrupted (rates drop to <30% with intervention): piton is more degraded than initially classified; constraint shows vulnerability to targeted intervention. If transmission rates depend on specific family context: classification varies by institutional structure (state care vs family vs community).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergenerational_transmission_inevitability, empirical, 'Intergenerational transmission rates and circuit-breaker mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attachment_disruption_trauma, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(atta_tr_t0, attachment_disruption_trauma, theater_ratio, 0, 0.35).
narrative_ontology:measurement(atta_tr_t2, attachment_disruption_trauma, theater_ratio, 2, 0.45).
narrative_ontology:measurement(atta_tr_t5, attachment_disruption_trauma, theater_ratio, 5, 0.55).
narrative_ontology:measurement(atta_tr_t10, attachment_disruption_trauma, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(atta_be_t0, attachment_disruption_trauma, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(atta_be_t2, attachment_disruption_trauma, base_extractiveness, 2, 0.58).
narrative_ontology:measurement(atta_be_t5, attachment_disruption_trauma, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(atta_be_t10, attachment_disruption_trauma, base_extractiveness, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attachment_disruption_trauma, attachment_coordination).
narrative_ontology:boltzmann_floor_override(attachment_disruption_trauma, 0.12).
narrative_ontology:affects_constraint(attachment_disruption_trauma, intergenerational_trauma_transmission).
narrative_ontology:affects_constraint(attachment_disruption_trauma, insecure_attachment_patterns).
narrative_ontology:affects_constraint(attachment_disruption_trauma, relational_reenactment_cycles).

% DUAL FORMULATION NOTE:
% Attachment disruption trauma decomposes into a constraint family by developmental phase and agent perspective. Child victim story (snare, immediate/trapped). Adult survivor story (tangled_rope, biographical/identity_locked). Therapeutic intervention story (scaffold, generational/mobile). Intergenerational transmission story (piton, civilizational/constrained). Each story has distinct epsilon reflecting different observable: trauma as experienced harm (high epsilon), trauma as internalized pattern (medium epsilon), trauma as intervention target (low epsilon), trauma as cultural reproduction (medium-high epsilon). Stories linked through network.affects_constraints showing causal and institutional coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attachment_disruption_trauma, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
