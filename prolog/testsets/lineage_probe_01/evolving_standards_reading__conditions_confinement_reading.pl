% ============================================================================
% CONSTRAINT STORY: evolving_standards_reading__conditions_confinement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evolving_standards_reading__conditions_confinement_reading, []).

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
 *   constraint_id: evolving_standards_reading__conditions_confinement_reading
 *   human_readable: Conditions of Confinement Reading: Eighth Amendment Protections Inside Prison
 *   domain: constitutional_law/criminal_procedure
 *
 * SUMMARY:
 *   The conditions-confinement reading interprets the Eighth Amendment's
 *   prohibition on cruel and unusual punishment to follow the incarcerated
 *   person inside the prison walls. This reading establishes that punishment
 *   includes the conditions of confinement — deliberate indifference to
 *   medical need or violence by prison officials violates the Clause itself,
 *   not merely statutory duties. The reading is one interpretation within the
 *   larger landscape of Eighth Amendment doctrine, coexisting with (but
 *   distinct from) the death penalty narrowing reading (which carves out
 *   categories of defendants — the intellectually disabled, juveniles, and
 *   non-homicide offenders — from capital punishment) and the juvenile
 *   culpability reading (which brings developmental neuroscience into
 *   culpability determinations). The structural delta for this reading is
 *   distinct: suppression of warehouse indifference (the practice of treating
 *   incarcerated persons as warehoused masses without individual medical or
 *   safety claims); beneficiary identification as the incarcerated; victim
 *   set as unaccountable prison administration; extractiveness bounded by the
 *   minimal humanity the Clause enforces. The constraint exhibits tangled
 *   rope structure: genuine coordination function (establishing uniform
 *   minimum standards for all facilities) alongside asymmetric extraction
 *   (incarcerated persons gain paper protections with weak enforcement;
 *   prison officials bear litigation costs and constraints on
 *   efficiency-first logic). Theater emerges as the enforcement machinery —
 *   grievance procedures, medical review committees, incident documentation —
 *   persists with limited effect on actual conditions.
 *
 * KEY AGENTS:
 *   - Incarcerated persons: Primary beneficiary under this reading (powerless/trapped) — gain nominal constitutional protection against deliberate indifference; enforcement is structurally weak
 *   - Prison advocacy organizations: Secondary actor (moderate/constrained) — litigate to enforce the Clause; benefit from constitutional framework while bearing resource costs and institutional resistance
 *   - Progressive federal judiciary: Institutional interpreter (institutional/arbitrage) — expands the Clause to suppress warehouse indifference; benefits from enhanced judicial authority; sees the reading as pure coordination
 *   - Prison administration / conservative legislature: Institutional actor (institutional/constrained) — views the reading as extractive intrusion; cannot exit constitutional supremacy but advocates resource-limiting interpretations
 *   - Compliance theater apparatus: Institutional machinery (institutional/constrained) — maintains performative grievance and medical review structures that appear to enforce but minimally prevent deliberate indifference
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing this reading as immutable moral law rather than one contestable interpretation among siblings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evolving_standards_reading__conditions_confinement_reading, 0.48).
domain_priors:suppression_score(evolving_standards_reading__conditions_confinement_reading, 0.65).
domain_priors:theater_ratio(evolving_standards_reading__conditions_confinement_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evolving_standards_reading__conditions_confinement_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(evolving_standards_reading__conditions_confinement_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(evolving_standards_reading__conditions_confinement_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evolving_standards_reading__conditions_confinement_reading, tangled_rope).
narrative_ontology:human_readable(evolving_standards_reading__conditions_confinement_reading, "Conditions of Confinement Reading: Eighth Amendment Protections Inside Prison").
narrative_ontology:topic_domain(evolving_standards_reading__conditions_confinement_reading, "constitutional_law/criminal_procedure").

domain_priors:requires_active_enforcement(evolving_standards_reading__conditions_confinement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(evolving_standards_reading__conditions_confinement_reading, 'dc947c10-3e48-4e43-96f1-2c2ff1ea8179').
narrative_ontology:cs_kernel_codification('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', fixed_text).
narrative_ontology:cs_authority_grounding('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', lineage).
narrative_ontology:cs_interpretation_layer_present('dc947c10-3e48-4e43-96f1-2c2ff1ea8179').
narrative_ontology:cs_reading_relation('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', evolving_standards_reading__death_penalty_narrowing_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', evolving_standards_reading__juvenile_culpability_reading, influences).
narrative_ontology:cs_axiom('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', foundational, confinement_conditions_constitutionally_bounded).
narrative_ontology:cs_axiom_status(confinement_conditions_constitutionally_bounded, holdable).
narrative_ontology:cs_axiom_grounding('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', confinement_conditions_constitutionally_bounded, deontological).
narrative_ontology:cs_axiom('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', foundational, deliberate_indifference_suppression_binding).
narrative_ontology:cs_axiom_status(deliberate_indifference_suppression_binding, holdable).
narrative_ontology:cs_axiom_grounding('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', deliberate_indifference_suppression_binding, deontological).
narrative_ontology:cs_reference_frame('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', evolving_standards_constitutional_minimalism).
narrative_ontology:cs_drift_state('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', contemporary_carceral_expansion, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('dc947c10-3e48-4e43-96f1-2c2ff1ea8179', '').
narrative_ontology:cs_kernel_id(evolving_standards_reading__conditions_confinement_reading, evolving_standards_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evolving_standards_reading__conditions_confinement_reading, incarcerated_persons).
narrative_ontology:constraint_victim(evolving_standards_reading__conditions_confinement_reading, unaccountable_prison_administration).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCARCERATED PERSON (SNARE) — Faces deliberate indifference to medical need or violence with no exit: the constitutional claim exists but enforcement is structurally weak. Trapped by the sentence; the Clause's suppression of warehouse indifference is the only protection available, and that protection is inconsistently enforced. Maximum experienced extraction through confinement conditions despite nominal constitutional boundary.
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRISON ADVOCATE ORGANIZATION (TANGLED ROPE) — Operates within litigation and administrative channels to enforce the Clause inside prisons. Benefits from the constitutional framework (it legitimizes their work) while bearing costs of coordinating across fragmented jurisdiction and hostile institutional response. Real coordination function (establishing standards, documenting violations) alongside asymmetric extraction (their work is resource-intensive and often defeated by procedural delays).
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROGRESSIVE FEDERAL JUDICIARY (ROPE) — Interprets the Clause expansively to suppress warehouse indifference. Experiences the reading as pure coordination: clarifying constitutional boundaries enables prisons to operate within those bounds. Net beneficiary of the interpretive tradition (judicial authority is enhanced by expansive constitutional reading). Sees the constraint as establishing necessary minimum standards.
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSERVATIVE PRISON ADMINISTRATION/LEGISLATURE (SNARE) — Views the conditions-confinement reading as extractive intrusion into institutional autonomy. Cannot exit the constitutional claim (it is binding law) but experiences it as suppressing legitimate warehouse efficiency. High experienced extraction through compliance costs and litigation liability. Exit option is constrained by constitutional supremacy, not trapped — they could advocate amendment, but cannot currently ignore the Clause.
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, snare,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, the conditions-confinement reading appears as a fixed principle of human dignity: punishment includes the sentence, and the state may not inflict needless suffering. This perspective sees the Clause boundary as an immutable moral law — not a contingent interpretation of text but a fundamental constraint on state power. However, the structural data reveals this as a false summit: the reading is one interpretation among sibling readings (death penalty narrowing, juvenile culpability), and enforcement turns on contestable institutional choices.
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: PRISON COMPLIANCE THEATER (PITON) — The compliance infrastructure (grievance procedures, medical review committees, incident documentation) persists as largely performative ritual: it appears to enforce the Clause but actual prevention of deliberate indifference is minimal. Theater ratio reflects that compliance machinery produces reports, findings, and recommendations but incarcerated persons remain in dangerous conditions. The ritual persists through institutional inertia and legal visibility requirements, not because it effectively suppresses warehouse indifference.
constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolving_standards_reading__conditions_confinement_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolving_standards_reading__conditions_confinement_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evolving_standards_reading__conditions_confinement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(evolving_standards_reading__conditions_confinement_reading, TR),
    TR >= 0.70.

:- end_tests(evolving_standards_reading__conditions_confinement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The reading imposes real constraints on prison resource allocation and operational logic — medical care must meet constitutional floors, violence prevention cannot be wholly warehoused. However, extractiveness is not extreme (snare boundary is 0.66) because the reading also genuinely coordinates: it establishes uniform minimum standards that enable all facilities to operate under known constitutional bounds. Measurements show rising extractiveness from 0.38 to 0.48 over 30 years as the reading has become more actionable through precedent. Suppression (0.65): High. Structural barriers to enforcing the Clause inside prisons are substantial: access to courts is limited, incarcerated persons have minimal resources for litigation, prison officials control factual narratives, and federal deference to institutional judgment remains high despite the reading's nominal breadth. Measurements show rising suppression (0.58 → 0.65) as prison systems have developed sophisticated compliance theater that appears to address the Clause while maintaining warehouse conditions. Theater ratio (0.58): Moderate-high. The compliance infrastructure (grievance procedures, medical committees, documentation) produces visible compliance activity with minimal actual prevention of deliberate indifference. The reading itself is not theatrical — it rests on genuine constitutional principle — but its enforcement has become increasingly theatrical as the gap between nominal protections and actual conditions has persisted.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal. Incarcerated persons experience snare classification — trapped in conditions that nominally violate the Clause but with enforcement so weak that deliberate indifference persists. Prison advocacy organizations experience tangled rope — the Clause provides legitimate coordination framework but also requires resource-intensive enforcement against institutional resistance. Progressive judiciary experiences rope — the reading is pure coordination (setting standards for facilities to follow). Prison administration experiences snare — the Clause imposes costs with no coordination benefit they recognize. Compliance theater apparatus experiences piton — the machinery persists through inertia and legal visibility, not because it prevents harm. The analytical observer risks mountain — naturalizing this reading as immutable dignity principle rather than one interpretation among contested alternatives. This perspectival diversity is diagnostic: when a single structural phenomenon produces all six classification types, the constraint is a kernel reading where the interpretive contest determines which experience is foreground.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural position of each agent relative to this reading. Incarcerated persons are beneficiaries with trapped exit (no choice but to remain in prison, no choice about conditions); their d-value approaches 1.0 (full target of extraction despite nominal protection). Prison advocacy organizations benefit from the constitutional framework while facing constrained exit (litigation strategy is their only institutional option); their d reflects moderate extraction. Progressive judiciary sees beneficiary status (enhanced interpretive authority) and arbitrage exit (they can choose interpretive bounds); their d approaches 0.0 (full beneficiary, minimal extraction). Prison administration sees victim status (constrained by constitutional limits) and constrained exit (cannot abandon operations); their d is elevated, approaching snare-range d. The analytical observer at civilizational scope has d ≈ 0.72 (analytical canonical), which feeds into false summit detection: the mountain classification risks naturalizing one contestable reading as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING RESOLVES THE MANDATROPHY: The conditions-confinement reading does not exhaust the constitutional meaning of the Clause. It coexists with (and is partly in tension with) the death penalty narrowing reading (which carves out categories from capital punishment) and the juvenile culpability reading (which reads developmental science into culpability standards). These are not alternative measurements of the same constraint — they are different readings that contest how the Clause evolves. The mandatrophy is resolved by recognizing this as a commitment-system constraint where the kernel is the Clause's text and the reading is one live interpretation among siblings. The false summit in Perspective 5 is intentional: the analytical observer who naturalizes this reading as immutable law has adopted one interpretive position without acknowledging the kernel contest. The engine's false summit detection will flag this, revealing that the mountain classification depends on which reading is adopted, not on the reading's intrinsic status.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_indifference_threshold,
    'What subjective awareness standard for prison officials constitutes ''deliberate indifference'' sufficient to violate the Clause?',
    'Case law evolution tracking the conscious disregard standard; empirical studies of prison official knowledge of conditions and medical needs',
    'If threshold is high (must know of specific risk): most warehouse indifference escapes the Clause''s reach (extractiveness rises toward snare boundary). If threshold is low (should have known, negligence suffices): the reading suppresses more indifference (extractiveness falls toward rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_indifference_threshold, empirical, 'Subjective awareness threshold for deliberate indifference').

omega_variable(
    competing_constitutional_reading,
    'Is this reading (conditions follow prisoner inside) one interpretation of the Clause among contested alternatives, or does it exhaust the Clause''s meaning?',
    'Comparison with sibling readings (death penalty narrowing, juvenile culpability); analysis of whether all three readings can coexist in a single constitutional framework or whether they instantiate incompatible premises about how the Clause evolves',
    'If reading is one among coexisting alternatives: the constraint is a kernel reading with real interpretive contest (false summit detection applies). If reading exhausts the Clause''s binding meaning: the constraint approaches mountain classification (but beneficiary declaration already flags FSM candidate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competing_constitutional_reading, conceptual, 'Whether conditions-confinement is the only coherent Clause reading or one among contestable alternatives').

omega_variable(
    enforcement_capacity_structural_gap,
    'Is the gap between the Clause''s nominal protection and actual prison conditions a resolvable enforcement problem or a structural property of carceral institutions?',
    'Longitudinal measurement of prison conditions pre- and post-enforcement; analysis of whether increased litigation and compliance resources actually reduce deliberate indifference or merely produce theater',
    'If gap is enforcement problem: resources and political will could close it; suppression could be substantially reduced. If gap is structural: the Clause establishes a floor that carceral logic perpetually undercuts; suppression remains high regardless of enforcement effort.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_structural_gap, empirical, 'Whether enforcement gap is remediable or structural to carceral institutions').

omega_variable(
    reading_kernel_identity,
    'What is the kernel that this reading interprets? Is it the text ''No cruel and unusual punishment'' alone, or a broader commitment to human dignity in state custody?',
    'Historical and doctrinal analysis of the Eighth Amendment''s ratification and evolution; comparison of how different readers (narrow textualists, living constitutionalists, natural law theorists) identify the kernel',
    'If kernel is text alone: competing readings must respect textual bounds (death penalty narrowing and juvenile culpability readings constrained). If kernel is dignity principle: readings contest which dignity principles bind (conditions, capacity, culpability — all potentially co-valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Identity of the kernel this reading instantiates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evolving_standards_reading__conditions_confinement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esc_cc_tr_t0, evolving_standards_reading__conditions_confinement_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(esc_cc_tr_t15, evolving_standards_reading__conditions_confinement_reading, theater_ratio, 15, 0.52).
narrative_ontology:measurement(esc_cc_tr_t30, evolving_standards_reading__conditions_confinement_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(esc_cc_be_t0, evolving_standards_reading__conditions_confinement_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(esc_cc_be_t15, evolving_standards_reading__conditions_confinement_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(esc_cc_be_t30, evolving_standards_reading__conditions_confinement_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(esc_cc_su_t0, evolving_standards_reading__conditions_confinement_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(esc_cc_su_t15, evolving_standards_reading__conditions_confinement_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(esc_cc_su_t30, evolving_standards_reading__conditions_confinement_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evolving_standards_reading__conditions_confinement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(evolving_standards_reading__conditions_confinement_reading, evolving_standards_reading__death_penalty_narrowing_reading).
narrative_ontology:affects_constraint(evolving_standards_reading__conditions_confinement_reading, evolving_standards_reading__juvenile_culpability_reading).

% DUAL FORMULATION NOTE:
% The evolving_standards_reading kernel contains three readings: conditions_confinement_reading (this constraint), death_penalty_narrowing_reading, and juvenile_culpability_reading. Each reading interprets the Eighth Amendment from a distinct doctrinal angle. Network edges link all three: conditions_confinement_reading affects the other two because establishing that the Clause follows prisoners inside creates a broader framework within which death penalty narrowing and juvenile culpability readings must operate. The siblings affect back to this reading through their implications for how culpability and dignity are constitutionally protected.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(evolving_standards_reading__conditions_confinement_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
