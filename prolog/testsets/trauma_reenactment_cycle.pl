% ============================================================================
% CONSTRAINT STORY: trauma_reenactment_cycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trauma_reenactment_cycle, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trauma_reenactment_cycle
 *   human_readable: Trauma Reenactment Cycle
 *   domain: psychological/interpersonal
 *
 * SUMMARY:
 *   The trauma reenactment cycle is a structural constraint in which
 *   survivors of psychological or relational trauma compulsively reproduce
 *   circumstances, interactions, or emotional states that mirror the original
 *   traumatic event. The cycle operates through nervous system conditioning,
 *   state-dependent memory, intermittent reinforcement, and identity
 *   fusion—mechanisms that make exit appear impossible despite the absence of
 *   the original external threat. This constraint exemplifies how suppression
 *   can be wholly internalized (the survivor's own nervous system and
 *   identity enforce the pattern) while creating extraction effects identical
 *   to external coercion. The cycle affects not just the primary survivor but
 *   relationship partners, children, and institutional systems that attempt
 *   intervention. The theater ratio reflects that standard trauma therapy
 *   protocols often become performative rituals—clients improve on
 *   self-report measures while continuing to reenact the cycle in actual
 *   relationships. The extractiveness value (0.68) reflects that the cycle
 *   consumes enormous psychological, relational, and temporal resources while
 *   providing no genuine coordination benefit to anyone; the intermittent
 *   reinforcement schedule makes it maximally extinction-resistant.
 *
 * KEY AGENTS:
 *   - Primary Survivor: Victim (powerless/trapped or identity-locked) — bears full cost of reenactment pattern; experiences it as inevitable and unchangeable
 *   - Close Relational Partner: Mixed actor (moderate/constrained) — benefits from relational coordination but extracted by emotional labor, abuse cycles, and caretaking compression
 *   - Children in Household: Secondary victims (powerless/trapped) — trapped by dependent status and internalization of caretaking/appeasement roles; intergenerational transmission of reenactment patterns
 *   - Mental Health Treatment System: Institutional actor (institutional/arbitrage) — maintains therapy protocols that often become performative; benefits from sustained client population requiring ongoing treatment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the reenactment as inevitable feature of trauma neuroscience rather than a constraint that can be disrupted
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trauma_reenactment_cycle, 0.68).
domain_priors:suppression_score(trauma_reenactment_cycle, 0.75).
domain_priors:theater_ratio(trauma_reenactment_cycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trauma_reenactment_cycle, extractiveness, 0.68).
narrative_ontology:constraint_metric(trauma_reenactment_cycle, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(trauma_reenactment_cycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trauma_reenactment_cycle, snare).
narrative_ontology:human_readable(trauma_reenactment_cycle, "Trauma Reenactment Cycle").
narrative_ontology:topic_domain(trauma_reenactment_cycle, "psychological/interpersonal").

% --- Structural relationships ---
narrative_ontology:constraint_victim(trauma_reenactment_cycle, trauma_survivor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAUMA SURVIVOR (SNARE) — Trapped in compulsive reenactment patterns with no perceived exit. Suppression is total: the survivor's nervous system generates the constraint through conditioned responses, intermittent reinforcement cycles, and state-dependent memory. Behavioral patterns feel inevitable and unchangeable from within the trauma frame. Maximum extractiveness experienced — the reenactment consumes agency, time, relationships, and emotional resources.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: TRAUMA SURVIVOR—IDENTITY-LOCKED (SNARE) — The survivor's self-concept is constituted through the trauma narrative and reenactment patterns. Identity has fused with the role the cycle assigns: the wounded one, the repeater, the one who 'always ends up here.' Exit would require abandoning not just the behavior but the identity built within it. Structurally, the survivor may have exit capacity (financial resources, social support, geographic mobility) that approaches 'constrained' rather than 'trapped,' but the identity lock prevents recognizing or exercising these capacities. The classification remains Snare because the binding mechanism—identity fusion—is as effective as material barriers.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 3: CLOSE RELATIONAL PARTNER (TANGLED ROPE) — The partner experiences both coordination and extraction. Genuine coordination function exists: supporting the survivor, managing household stability, navigating the survivor's state changes. But asymmetric extraction is embedded: emotional labor, role compression (caregiver/lover/parent), unpredictable conflict cycles, suppressed autonomy, vulnerability to state-dependent abuse. The partner is constrained by emotional bonds, financial entanglement, caregiving obligations, and fear of abandonment. Significant but not maximal extraction—the partner has some agency through the relationship itself.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: CHILDREN IN HOUSEHOLD (SNARE) — Trapped in reenactment dynamics without exit capacity or agency. The cycle reproduces intergenerationally: children internalize trauma activation patterns, normalize conflict escalation, and fuse identity with caretaking or appeasement roles. Suppression is compounded—material barriers (dependent status, geographic immobility) plus internalized patterns that teach the child to manage the survivor's state. The cycle self-perpetuates through modeling and conditioning.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 5: MENTAL HEALTH TREATMENT SYSTEM (PITON) — Standard trauma therapy protocols (CBT, EMDR, exposure therapy) are designed to interrupt reenactment cycles but often become performative: attending sessions, completing worksheets, narrating the trauma story in approved formats, without durable interruption of the underlying cycle. The theater ratio reflects that the system's primary function—breaking the cycle—is often degraded by insurance constraints, protocol rigidity, therapist capacity limits, and the fact that intellectual understanding of trauma rarely disrupts nervous system patterning. The treatment system persists as institutionalized ritual despite low functional verification of cycle interruption.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER—NATURAL LAW READING (MOUNTAIN FALSE SUMMIT) — A naturalized framing argues that trauma reenactment is an inherent feature of how the nervous system encodes threat: once a pattern is learned, repeated activation is inevitable; the cycle cannot be exited because it is written into biology. This perspective risks treating the reenactment cycle as an immutable law of trauma neuroscience rather than recognizing it as a constraint that can be disrupted through polyvagal regulation, somatic intervention, relational safety, or neuroterapy. The false summit detector flags this: the 'inevitability' derives from suppressed alternatives and institutional inertia in trauma treatment, not from natural law.
constraint_indexing:constraint_classification(trauma_reenactment_cycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trauma_reenactment_cycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trauma_reenactment_cycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trauma_reenactment_cycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trauma_reenactment_cycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trauma_reenactment_cycle, TR),
    TR >= 0.70.

:- end_tests(trauma_reenactment_cycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The cycle consumes psychological, relational, emotional, time, and opportunity costs from the survivor. Measurement trajectory shows escalation from 0.45 to 0.68 over the interval, indicating accumulation—the cycle deepens or becomes more pervasive without intervention. The escalation reflects classic snare dynamics: intermittent reinforcement strengthens the pattern, and failed escape attempts (including therapy that addresses behavior but not nervous system patterning) reinforce the belief that exit is impossible. Suppression (0.75): Very high. The suppression is primarily internalized—the survivor's own nervous system, identity frame, and conditioned threat responses enforce the pattern. While material barriers (financial dependency, fear of abandonment, custody threats) may exist, the primary mechanism is nervous system dysregulation and identity fusion. The survivor experiences the cycle as neurologically driven and identity-constitutive, not as externally imposed. This internalization is why escape attempts fail—the survivor cannot outrun their own nervous system. Theater ratio (0.65): High-moderate. Standard trauma therapy (CBT, exposure therapy, EMDR) shows high reported improvement but low functional cycle interruption. The theater reflects that symptom reduction in clinical metrics does not translate to actual behavioral change in relationships. Many survivors complete therapy programs while continuing to reenact the cycle with romantic partners, repeat relationship patterns, or reproduce conflict dynamics. The gap between clinical improvement narratives and functional cycle persistence is diagnostic of theater.
 *
 * PERSPECTIVAL GAP:
 *   The maximal perspectival gap appears between the survivor's Snare classification (complete powerlessness and entrapment) and the analytical observer's false Mountain (naturalized inevitability). The survivor experiences the constraint as real and inescapable; the naturalized observer treats it as a law of trauma neuroscience. Both are wrong in different ways: the survivor is identity-locked rather than trapped (has more agency than perceived), and the observer is naturalizing a contingent institutional arrangement. The partner's Tangled Rope classification reveals embedded extraction within coordination—the survival mechanism requires the partner's labor and emotional absorption. The institutional treatment system's Piton classification reveals that standard protocols have become performative: they address symptom narratives rather than nervous system patterning.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality in this constraint is complex because there is no identifiable beneficiary of the reenactment cycle—no agent receives net benefit from the pattern's perpetuation. The primary beneficiary is the absence of exit, which paradoxically serves the survivor's identity-locked status. From the survivor's perspective (powerless/trapped): d ≈ 0.95, yielding maximum f(d) ≈ 1.42—they bear full extractiveness. From the identity-locked survivor's perspective: d ≈ 0.89, yielding f(d) ≈ 1.28—identity lock reduces d slightly from total entrapment (the survivor has more structural options than they perceive). From the relational partner's perspective (moderate/constrained): d ≈ 0.65, yielding f(d) ≈ 1.00—mixed extraction, moderate agency through the relationship. From children's perspective (powerless/trapped): d ≈ 0.90, yielding f(d) ≈ 1.38—high extraction through intergenerational transmission. The institutional treatment system (institutional/arbitrage): d ≈ 0.10, yielding f(d) ≈ -0.05—the system subtly benefits from sustained treatment demand, experiencing the constraint as coordination and business model. No beneficiaries field has been declared because no structural group benefits from the cycle's perpetuation in a genuine sense—the system's benefit is parasitic on the survivor's entrapment.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint exhibits the classic mandatrophy contradiction—it appears simultaneously as Snare (pure extraction, no coordination function) and as the site of intensive therapeutic intervention claiming coordination value. The resolution is that the 'coordination' in therapeutic contexts is institutional, not structural: therapy coordinates treatment demand, funding flows, and professional identity—not the actual cycle interruption. The snare classification is correct from the survivor's perspective; the therapeutic system's coordination is institutional parasitism. The extractiveness value of 0.68 reflects that the cycle is high-extraction but not maximal-extraction (0.85+) because some survivors do achieve interruption through intensive interventions (somatic therapy, relational safety, nervous system regulation techniques) that operate outside standard protocols. The cycle is not absolutely inescapable, but the structural suppression and identity lock make escape appear impossible and require interventions beyond talk-based protocols. The mandatrophy is resolved by recognizing two separate constraint families: (1) the trauma reenactment cycle itself (high-extraction Snare), and (2) the institutional treatment system's response (Piton theater). These should be decomposed into separate stories with network links.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_depth,
    'To what degree is the survivor''s identity fusion with trauma patterns a distinct structural mechanism versus an emergent property of prolonged reenactment exposure?',
    'Longitudinal analysis of identity shifts during trauma processing: does identity differentiation from trauma precede or follow reenactment interruption? Pre/post identity coherence measurement in survivors undergoing somatic or relational interventions.',
    'If fusion is primary mechanism: identity-locked classification is correct, and standard talk therapy addressing behavior will fail to interrupt the cycle. If fusion is emergent: behavioral interruption may be sufficient to catalyze identity shift. Changes which interventions are prioritized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Whether identity fusion is primary mechanism or emergent property of reenactment').

omega_variable(
    intergenerational_mechanism,
    'Is intergenerational transmission of trauma reenactment primarily via observational learning/modeling, via epigenetic stress encoding, or via relational dynamics in which the child''s nervous system synchronizes with the caregiver''s dysregulation?',
    'Comparison of intergenerational transmission rates across: direct household exposure vs. separated childhood, biological vs. adoptive kinship, relational closeness vs. geographic distance. Physiological markers of nervous system synchronization in parent-child dyads during survivor dysregulation.',
    'If modeling: prevention via safe alternative models. If epigenetic: intervention requires addressing parental stress reduction. If relational synchrony: intervention requires disrupting co-regulation patterns. Each mechanism demands different structural intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_mechanism, empirical, 'Mechanism of intergenerational transmission of trauma reenactment').

omega_variable(
    intermittent_reinforcement_schedule,
    'Does the reenactment cycle operate via intermittent reinforcement (variable ratio or variable interval schedule)? If so, what is the reinforcement structure—reconciliation after conflict, state reduction through escalation, relational repair, or avoidance of deeper abandonment anxiety?',
    'Behavioral mapping of cycle phases with reinforcement identification: detailed sequence of activation, escalation, peak, resolution, and return to baseline. Measurement of intervals and triggers. Comparison to canonical intermittent reinforcement schedules (most resistant to extinction).',
    'If variable-ratio intermittent reinforcement confirmed: cycle is maximally extinction-resistant; standard extinction-based exposure therapy will be ineffective. Requires intervention that disrupts the reinforcement source (typically relational repair or state reduction), not just behavioral exposure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intermittent_reinforcement_schedule, empirical, 'Whether reenactment cycle operates via intermittent reinforcement schedule').

omega_variable(
    suppression_internalization_boundary,
    'Is the measured suppression (0.75) structural (material barriers: financial dependency, geographic isolation, legal constraints, custody threats) or internalized (cognitive patterns: shame, deservingness narratives, distorted threat assessment) or both? If both, what proportion is each?',
    'Intervention sequencing analysis: does removal of structural barriers alone enable exit, or do internalized suppression patterns persist after barriers are removed? Longitudinal tracking of survivors in relocated/separated situations. Cognitive restructuring trials with barrier removal held constant.',
    'If primarily structural: remove barriers and exit becomes possible. If primarily internalized: barrier removal alone insufficient—must address identity lock and cognitive patterns. If mixed: determines intervention priority sequencing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_boundary, empirical, 'Suppression mechanism: structural vs. internalized').

omega_variable(
    treatment_theater_mechanism,
    'Why do standard trauma therapy protocols often show low functional cycle interruption despite high reported symptom improvement? Is the theater driven by: measurement bias (clients reporting improvement to please therapist or meet insurance requirements), protocol rigidity (therapy addressing narrative understanding rather than nervous system patterning), therapist capacity limits, or fundamental incompleteness of talk-based intervention for somatic trauma encoding?',
    'Comparison of self-reported symptom improvement vs. objective cycle metrics (frequency, duration, intensity of reenactment episodes) tracked by clients between sessions. Neurophysiological markers of nervous system dysregulation before/after sessions. Long-term follow-up of functional cycle status (not symptom reports) in therapy graduates.',
    'If measurement bias: treatment efficacy is overstated; protocols may be working but metrics are theater. If protocol rigidity: somatic and relational approaches needed. If therapist capacity: system requires more intensive intervention models. If somatic encoding incompleteness: talk-based interventions are fundamentally limited.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treatment_theater_mechanism, empirical, 'Why trauma treatment shows high self-reported improvement but low functional cycle interruption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trauma_reenactment_cycle, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(treauma_tr_t0, trauma_reenactment_cycle, theater_ratio, 0, 0.35).
narrative_ontology:measurement(treauma_tr_t5, trauma_reenactment_cycle, theater_ratio, 5, 0.5).
narrative_ontology:measurement(treauma_tr_t10, trauma_reenactment_cycle, theater_ratio, 10, 0.65).
narrative_ontology:measurement(treauma_tr_t15, trauma_reenactment_cycle, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(treauma_be_t0, trauma_reenactment_cycle, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(treauma_be_t5, trauma_reenactment_cycle, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(treauma_be_t10, trauma_reenactment_cycle, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(treauma_be_t15, trauma_reenactment_cycle, base_extractiveness, 15, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trauma_reenactment_cycle, attachment_coordination).
narrative_ontology:boltzmann_floor_override(trauma_reenactment_cycle, 0.12).
narrative_ontology:affects_constraint(trauma_reenactment_cycle, relational_abuse_dynamic).
narrative_ontology:affects_constraint(trauma_reenactment_cycle, intergenerational_trauma_transmission).
narrative_ontology:affects_constraint(trauma_reenactment_cycle, institutional_trauma_treatment_theater).

% DUAL FORMULATION NOTE:
% The trauma reenactment cycle decomposes into multiple structurally distinct constraints: (1) the nervous system conditioning cycle itself (high extractiveness, primarily internalized suppression, snare classification), (2) relational dynamics in which reenactment is embedded (tangled rope from partner perspective), (3) intergenerational transmission mechanisms, and (4) the institutional treatment system's response. Each has different epsilon values and different intervention leverage points. This story focuses on the primary cycle mechanism. Sister stories addressing relational dynamics and institutional treatment theater should be written separately and linked via network.affects_constraints. The attachment_coordination type reflects that trauma bonding involves genuine emotional attachment coordination alongside extraction—unlike purely extractive constraints, the relational field contains real bonding elements that are exploited by the reenactment cycle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
