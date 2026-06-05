% ============================================================================
% CONSTRAINT STORY: articles_of_confederation__requisition_failure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_articles_of_confederation__requisition_failure_reading, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: articles_of_confederation__requisition_failure_reading
 *   human_readable: Articles of Confederation: Requisition Failure Reading
 *   domain: legal/doctrinal/constitutional
 *
 * SUMMARY:
 *   This constraint documents one reading of the Articles of Confederation
 *   kernel: the Articles failed because Congress could requisition but never
 *   tax, leaving war debts unpaid and the army starving while states ignored
 *   federal requests for revenue. This reading frames the constraint as a
 *   pure-coordination mechanism (Rope) in which states are free-riding on a
 *   system that depends entirely on voluntary compliance. The requisition
 *   system has zero extractiveness (no agent is forced to pay) and zero
 *   suppression (states face no barriers to refusing requisitions). The
 *   constraint is not broken—it is working exactly as designed: a voluntary
 *   coordination system that requires state consent. The reading localizes
 *   failure in state unwillingness to coordinate rather than in the Articles'
 *   structure. This reading coexists with two sibling readings: the
 *   state_sovereignty_design_reading (which frames state refusal to pay as
 *   correct exercise of reserved sovereignty, not free-riding) and the
 *   unanimity_trap_reading (which locates the true failure in the
 *   unanimity-amendment rule that made reform impossible). All three readings
 *   are compatible with the historical record; they differ in how they
 *   classify the causal mechanisms and what they identify as the constraint's
 *   core dysfunction.
 *
 * KEY AGENTS:
 *   - Continental Congress: Institutional coordinator (institutional/arbitrage) — requisitions are its sole revenue mechanism; Congress has no enforcement capacity, only negotiation authority
 *   - Non-Paying States (esp. New York, Massachusetts): Beneficiaries in this reading (organized/mobile) — retain full sovereignty over tax decisions; free-ride on collective defense while refusing to contribute
 *   - Continental Army: Victim (powerless/trapped) — depends on requisitioned state payments that never arrive; faces starvation and mutiny; cannot exit the dependency
 *   - War Creditors (foreign governments, private contractors): Victim (powerless/trapped) — owed substantial sums; cannot compel payment through legal mechanism; contracts depend on congressional ability to requisition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the requisition system as a pure-coordination mechanism working as designed; locates failure in state preferences rather than constitutional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(articles_of_confederation__requisition_failure_reading, 0.0).
domain_priors:suppression_score(articles_of_confederation__requisition_failure_reading, 0.0).
domain_priors:theater_ratio(articles_of_confederation__requisition_failure_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(articles_of_confederation__requisition_failure_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(articles_of_confederation__requisition_failure_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(articles_of_confederation__requisition_failure_reading, theater_ratio, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(articles_of_confederation__requisition_failure_reading, rope).
narrative_ontology:human_readable(articles_of_confederation__requisition_failure_reading, "Articles of Confederation: Requisition Failure Reading").
narrative_ontology:topic_domain(articles_of_confederation__requisition_failure_reading, "legal/doctrinal/constitutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(articles_of_confederation__requisition_failure_reading, '26d00a81-d9cd-4d46-88ea-d0431e145831').
narrative_ontology:cs_kernel_codification('26d00a81-d9cd-4d46-88ea-d0431e145831', fixed_text).
narrative_ontology:cs_authority_grounding('26d00a81-d9cd-4d46-88ea-d0431e145831', lineage).
narrative_ontology:cs_interpretation_layer_present('26d00a81-d9cd-4d46-88ea-d0431e145831').
narrative_ontology:cs_reading_relation('26d00a81-d9cd-4d46-88ea-d0431e145831', articles_of_confederation__state_sovereignty_design_reading, forecloses).
narrative_ontology:cs_reading_relation('26d00a81-d9cd-4d46-88ea-d0431e145831', articles_of_confederation__unanimity_trap_reading, coexists_with).
narrative_ontology:cs_axiom('26d00a81-d9cd-4d46-88ea-d0431e145831', foundational, requisition_binding_obligation_exists).
narrative_ontology:cs_axiom_status(requisition_binding_obligation_exists, holdable).
narrative_ontology:cs_axiom_grounding('26d00a81-d9cd-4d46-88ea-d0431e145831', requisition_binding_obligation_exists, conventional).
narrative_ontology:cs_axiom('26d00a81-d9cd-4d46-88ea-d0431e145831', foundational, voluntary_coordination_inadequate_for_collective_defense).
narrative_ontology:cs_axiom_status(voluntary_coordination_inadequate_for_collective_defense, holdable).
narrative_ontology:cs_axiom_grounding('26d00a81-d9cd-4d46-88ea-d0431e145831', voluntary_coordination_inadequate_for_collective_defense, empirically_contingent).
narrative_ontology:cs_reference_frame('26d00a81-d9cd-4d46-88ea-d0431e145831', federal_binding_authority).
narrative_ontology:cs_drift_state('26d00a81-d9cd-4d46-88ea-d0431e145831', constitutional_convention_1787, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('26d00a81-d9cd-4d46-88ea-d0431e145831', '2026-02-26T15:30:00Z').
narrative_ontology:cs_kernel_id(articles_of_confederation__requisition_failure_reading, articles_of_confederation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(articles_of_confederation__requisition_failure_reading, non_paying_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: STATE LEGISLATURES (ROPE) — States retain full sovereign authority to refuse or grant requisitions. The constraint is pure voluntary coordination. No suppression of alternatives; states explicitly choose not to pay, prioritizing local revenue needs. No extraction occurs because payment is purely volitional. The Articles work perfectly as designed: a coordination mechanism that states can opt out of without penalty.
constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 2: CONTINENTAL ARMY & WAR CREDITORS (SNARE) — Cannot exit the funding dependency. The army cannot function without payment; creditors cannot exit contracts already made. Congress has no coercive power to extract revenue, leaving creditors and soldiers trapped in a system where voluntary contributions fail. High suppression: no mechanism to compel payment exists. Extractiveness is zero—no one is being extracted from—but the constraint traps victims in a dependency with no escape. This is a snare of omission: the system's refusal to extract is itself the mechanism of harm.
constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTINENTAL CONGRESS (ROPE) — Congress functions as a coordination mechanism that translates state preferences into collective action. Congress has no enforcement power, only negotiation capacity. From Congress's perspective, the constraint is working as designed: requisitions are a pure coordination instrument that reveals state willingness to contribute. Congress experiences the system as a coordination problem it is solving, not an extraction mechanism.
constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — At the civilizational/universal analytical level, the Articles instantiate a pure-coordination regime with zero coercive capacity. This reading classifies the entire institutional arrangement as Rope: the requisition mechanism coordinates voluntary contributions without suppression of alternatives or extraction from agents. The constraint's extractiveness is zero because no agent is forced to contribute; suppression is zero because no agent faces barriers to refusing payment. From this perspective, the Articles are working perfectly—they are a coordination system that depends entirely on state consent, and that dependence is a feature, not a bug.
constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(articles_of_confederation__requisition_failure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(articles_of_confederation__requisition_failure_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(articles_of_confederation__requisition_failure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.0): The requisition system has zero extractiveness because payment is purely voluntary. No agent is forced to contribute; states that refuse payment face no legal or military consequences. The mechanism creates no extraction surplus—there is no coercive power, no hidden tax, no mandatory transfer. Suppression (0.0): States face no barriers to refusing requisitions. The full sovereign authority to decide tax and spending remains with state legislatures. Congress cannot compel payment through law, army enforcement, or withholding of benefits. Suppression is zero because the alternative (refusing to pay) is always available and always costless for the refusing state. Theater ratio (0.0): The requisition system is purely functional—there is no performative element. Requisitions are direct requests for payment; whether states comply is a question of preference and capacity, not ritual or theater. Classification (Rope): A pure-coordination mechanism where all agents voluntarily coordinate without extraction or suppression. The constraint binds through consent, not coercion. The requisition system solves the coordination problem of how thirteen independent states can fund a collective military—it solves it badly (states don't cooperate), but the mechanism itself is pure coordination.
 *
 * PERSPECTIVAL GAP:
 *   This reading produces minimal perspectival gap because all perspectives classify as Rope. From every structural position—state legislatures, Congress, the army, creditors, the analytical observer—the constraint is pure voluntary coordination. The gap this reading obscures is the gap between this reading and its siblings. The state_sovereignty_design_reading would argue that from the state legislatures' perspective, the constraint is not free-riding but correct exercise of authority—they are not beneficiaries, they are sovereigns exercising reserved rights. The unanimity_trap_reading would argue that the true constraint is not the requisition mechanism but the amendment rule, and this reading misidentifies the causal failure by treating voluntary coordination as if it could have succeeded if states had simply chosen to cooperate. The perspectival consensus around Rope in this reading is a feature, not evidence of correctness—it reflects the reading's own framing assumptions, not the structure of the phenomenon.
 *
 * DIRECTIONALITY LOGIC:
 *   In this reading, non-paying states are beneficiaries because they receive the benefit of collective defense (war effort is paid for by other states willing to contribute) while refusing to pay their proportional share—classic free-riding on a public good. The d-value for non-paying states is low (they benefit from the system) despite their ability to refuse payment (mobile exit options). The army and creditors are not classified as extractors or extractees in the traditional sense—they are victims of the constraint's failure, not victims of extraction. No agent is extracting from them because the constraint is purely voluntary. The constraint's harm comes from omission (the lack of extraction power) rather than commission (actual extraction). This is structurally distinct from a snare, where extraction is active; here the harm is that the mechanism cannot extract even when extraction would fund necessary collective defense.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_coordination_vs_structural_failure,
    'Is the requisition system a functioning pure-coordination mechanism (Rope) that is simply being refused, or is it a structurally failed system that mistakes voluntary for coordination-binding?',
    'Counterfactual: if all states had voluntarily paid all requisitions on time, would the Articles have been constitutionally adequate? If yes, the system is Rope (pure coordination working as designed). If no, the system was always inadequate regardless of state compliance, indicating the reading was wrong.',
    'If Rope is correct reading: states are free-riding on a working coordination mechanism; reform requires incentive alignment, not constitutional redesign. If wrong: the constraint is not pure coordination but a fundamental structural mismatch between the revenue need and the coordination mechanism, making the constraint something other than Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_coordination_vs_structural_failure, conceptual, 'Whether requisition failure is coordination breakdown or structural design inadequacy').

omega_variable(
    contested_reading_status,
    'Does the requisition_failure_reading correctly identify the kernel''s actual structure, or does it naturalize state non-compliance as an acceptable feature of a working system?',
    'This omega documents the kernel contest itself: the sibling reading (state_sovereignty_design_reading) offers an alternative framing in which the very features this reading treats as failures (states refusing payment, central powerlessness) are treated as the system''s correct operation. The dispute cannot be resolved empirically—both readings are compatible with the historical record. Resolution requires a commitment about what the Articles were designed to accomplish.',
    'If state_sovereignty_design_reading is the correct framing: requisition_failure_reading misclassifies features-as-bugs. If requisition_failure_reading is correct: state_sovereignty_design_reading naturalizes dysfunction as design. The two readings foreclose each other at the level of what ''correct operation'' means.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contested_reading_status, conceptual, 'Whether requisition failure is a reading of a flawed design or a misidentification of a working system').

omega_variable(
    unanimity_veto_relationship,
    'Is the requisition failure reading independent of the unanimity-amendment trap, or is the impossibility of requisition reform due to the unanimity rule?',
    'Counterfactual: if the Articles had included a supermajority amendment rule allowing revenue reform without unanimity, would the requisition failure have been resolved? If yes, the requisition failure is a downstream consequence of the unanimity trap, not a separate reading of the kernel. If no, the requisition failure is a structural feature of the voluntary-contribution design independent of amendment mechanics.',
    'If dependent on unanimity trap: this reading''s classification may collapse into unanimity_trap_reading once amendment reform is possible. If independent: this reading captures a design defect distinct from the amendment-rule defect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_veto_relationship, empirical, 'Whether requisition failure is independent of unanimity amendment rule').

omega_variable(
    beneficiary_identity_ambiguity,
    'Are non-paying states beneficiaries (free-riding on a coordination system) or are they correctly exercising sovereign authority to refuse an improper demand?',
    'The answer depends on whether the Articles bound states to pay requisitions. If yes, refusal is free-riding (states are beneficiaries of collective defense without bearing costs). If no, refusal is legitimate exercise of reserved authority (states are not beneficiaries, they are exercising rights). This dispute is what separates this reading from the state_sovereignty_design_reading.',
    'If free-riding: this reading correctly identifies extraction structure (beneficiary group exists). If legitimate exercise of authority: the beneficiary declaration is wrong, and the classification should change (no extraction is occurring, states are simply exercising rights).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_identity_ambiguity, conceptual, 'Whether state non-payment is free-riding or legitimate sovereignty exercise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(articles_of_confederation__requisition_failure_reading, 1781, 1789).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(articles_of_confederation__requisition_failure_reading, resource_allocation).
narrative_ontology:affects_constraint(articles_of_confederation__requisition_failure_reading, articles_of_confederation__state_sovereignty_design_reading).
narrative_ontology:affects_constraint(articles_of_confederation__requisition_failure_reading, articles_of_confederation__unanimity_trap_reading).

% DUAL FORMULATION NOTE:
% The articles_of_confederation kernel has three structurally distinct readings, each with its own constraint story and ε value. requisition_failure_reading treats the Articles as a pure-coordination system (Rope, ε=0.0) where states free-ride on collective defense. state_sovereignty_design_reading treats the Articles as a successful mechanism for preserving state sovereignty (Rope with different semantics) and would reject the free-riding framing as misapplied success criterion. unanimity_trap_reading locates the true structural failure in the amendment rule, not the revenue mechanism. All three readings are compatible with the historical record; they differ in their framing of what constitutes 'correct operation' and which mechanism is the site of failure. Each story instantiates one reading; the network relationships declare how the readings influence each other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
