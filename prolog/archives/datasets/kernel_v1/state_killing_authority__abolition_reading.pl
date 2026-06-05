% ============================================================================
% CONSTRAINT STORY: state_killing_authority__abolition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_killing_authority__abolition_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: state_killing_authority__abolition_reading
 *   human_readable: State Killing Authority — Abolition Reading (Categorical Rights-Based Prohibition)
 *   domain: criminal_justice/legal_philosophy/political_theory
 *
 * SUMMARY:
 *   The abolition reading of state killing authority treats the prohibition
 *   on capital punishment as a categorical, non-negotiable constraint
 *   grounded in inalienable human rights. This reading holds that no
 *   legitimate authority exists — whether derived from social contract,
 *   utilitarian logic, retributive desert, or state necessity — to take the
 *   life of a condemned person. The constraint is immutable at the
 *   civilizational and biological levels: the condemned person cannot consent
 *   to permanent error, and death is irreversible. The abolition reading
 *   instantiates a deontological kernel claim: humans possess irreducible
 *   moral status that cannot be suspended by any authority, legal procedure,
 *   or consequentialist calculation. This distinguishes it structurally from
 *   the retributive reading (which grounds state authority in proportional
 *   desert and treats moral status as variable by severity of wrong) and the
 *   deterrence reading (which grounds state authority in aggregate utility
 *   and treats execution as instrumentally justified if deterrence is
 *   empirically demonstrable). The three readings coexist in contemporary
 *   legal and political discourse, held by different constituencies within
 *   pluralist democracies, international institutions, and moral philosophy
 *   communities. The abolition reading has become institutionalized in
 *   international human rights law (UNCRC, ECHR, CAT) and has achieved
 *   majority adoption across nation-states (144 abolitionist or de facto
 *   abolitionist states vs. 55 retentionist as of 2024), yet remains
 *   contested in U.S. jurisprudence and some other common-law jurisdictions
 *   where retributive and deterrence framings retain institutional authority.
 *
 * KEY AGENTS:
 *   - International Human Rights Regimes: Institutional authority (analytical/analytical) — UN Convention on the Rights of the Child, European Convention on Human Rights, African Charter on Human and Peoples' Rights; enforce the abolition reading through treaty law and soft law pressure
 *   - Abolition-Movement States: Institutional beneficiaries (institutional/arbitrage) — nations that have adopted abolition and use it as part of their international legitimacy narrative; capture moral authority in human rights discourse
 *   - Retentionist States: Institutional defenders of alternative readings (institutional/arbitrage) — maintain deterrence or retributive authority; experience abolition reading as constraining their sovereign authority
 *   - Condemned Persons (Actual and Potential): Primary victims (powerless/trapped) — bear irreversible consequences; have zero exit options; the constraint is immutable for them
 *   - Moral Philosophy / Legal Scholarship Communities: Organized interpreters (organized/analytical) — debate the three readings; produce evidence and arguments that sibling readings attempt to refute
 *   - Families of Murder Victims: Secondary stakeholders in retentionist jurisdictions (moderate/constrained) — experience the retributive reading as permitting proportional justice; experience the abolition reading as constraint on that justice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_killing_authority__abolition_reading, 0.08).
domain_priors:suppression_score(state_killing_authority__abolition_reading, 0.02).
domain_priors:theater_ratio(state_killing_authority__abolition_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_killing_authority__abolition_reading, mountain).
narrative_ontology:human_readable(state_killing_authority__abolition_reading, "State Killing Authority — Abolition Reading (Categorical Rights-Based Prohibition)").
narrative_ontology:topic_domain(state_killing_authority__abolition_reading, "criminal_justice/legal_philosophy/political_theory").

domain_priors:emerges_naturally(state_killing_authority__abolition_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_killing_authority__abolition_reading, '67bf1757-05da-4331-8a39-7943684b12ee').
narrative_ontology:cs_kernel_codification('67bf1757-05da-4331-8a39-7943684b12ee', formalized).
narrative_ontology:cs_authority_grounding('67bf1757-05da-4331-8a39-7943684b12ee', lineage).
narrative_ontology:cs_interpretation_layer_present('67bf1757-05da-4331-8a39-7943684b12ee').
narrative_ontology:cs_reading_relation('67bf1757-05da-4331-8a39-7943684b12ee', state_killing_authority__retributive_reading, forecloses).
narrative_ontology:cs_reading_relation('67bf1757-05da-4331-8a39-7943684b12ee', state_killing_authority__deterrence_reading, influences).
narrative_ontology:cs_axiom('67bf1757-05da-4331-8a39-7943684b12ee', foundational, moral_status_inalienable).
narrative_ontology:cs_axiom_status(moral_status_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('67bf1757-05da-4331-8a39-7943684b12ee', moral_status_inalienable, deontological).
narrative_ontology:cs_axiom('67bf1757-05da-4331-8a39-7943684b12ee', secondary, irreversibility_incompatible_with_state_authority).
narrative_ontology:cs_axiom_status(irreversibility_incompatible_with_state_authority, holdable).
narrative_ontology:cs_axiom_grounding('67bf1757-05da-4331-8a39-7943684b12ee', irreversibility_incompatible_with_state_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('67bf1757-05da-4331-8a39-7943684b12ee', unconditional_right_to_life).
narrative_ontology:cs_drift_state('67bf1757-05da-4331-8a39-7943684b12ee', contemporary_post_wwii_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('67bf1757-05da-4331-8a39-7943684b12ee', '').
narrative_ontology:cs_kernel_id(state_killing_authority__abolition_reading, state_killing_authority).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABOLITION FRAMEWORK / DEONTOLOGICAL NATURAL LAW (MOUNTAIN) — From the framework of inalienable human rights, the prohibition on state killing is categorical and immutable: the condemned person retains irreducible moral status regardless of offense, deterrence calculus, or state utility. No legitimate authority exists to suspend this status. The constraint emerges from first principles of human dignity and is not negotiable through policy, efficiency, or social contract logic. Zero degrees of freedom for the state's killing authority.
constraint_indexing:constraint_classification(state_killing_authority__abolition_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: CONDEMNED PERSON / IRREVERSIBILITY (MOUNTAIN) — For the condemned person, the constraint is immutable: once executed, death is permanent and irreversible. The structural impossibility of undoing error, wrongful conviction, or moral category mistake makes capital punishment inherently incompatible with any procedural justice framework that permits human error. The constraint is binding absolutely, with zero escape routes.
constraint_indexing:constraint_classification(state_killing_authority__abolition_reading, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL HUMAN RIGHTS SYSTEM (MOUNTAIN) — Post-WWII human rights regimes (UN Convention on the Rights of the Child, European Convention on Human Rights, African Charter) treat the prohibition on state killing of civilians and lawfully convicted persons as a foundational, non-derogable norm. The constraint appears as immutable international law, not policy preference. Transcends individual state sovereign authority.
constraint_indexing:constraint_classification(state_killing_authority__abolition_reading, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_killing_authority__abolition_reading_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(state_killing_authority__abolition_reading, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_killing_authority__abolition_reading, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, ExtMetricName, E),
    domain_priors:suppression_score(state_killing_authority__abolition_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(state_killing_authority__abolition_reading),
    narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(state_killing_authority__abolition_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(state_killing_authority__abolition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Minimal. The abolition reading contains no systematic asymmetric extraction — it prohibits extraction (killing) rather than enabling it. The minimal non-zero value reflects measurement artifact: any institutional claim has some epistemic or advocacy cost, but the core substance of the constraint is non-extractive. Suppression (0.02): Negligible. The constraint suppresses nothing and enables nothing; it is a categorical prohibition. No suppression of alternatives is required because the prohibition is self-contained. Theater ratio (0.15): Very low. The abolition reading's legitimacy derives from deontological reasoning, not from performative ritual. There is no institutional theater around the constraint itself — the constraint is a clear, stable, well-reasoned prohibition. (By contrast, the retributive reading exhibits theater through sentencing rituals, trial procedures, and appeals processes; the deterrence reading exhibits theater through publicity of executions; the abolition reading produces theater only when institutions resist it, but that theater is in the resistance, not in the constraint itself.) Mountain gates: Accessibility collapse (0.92) reflects the constraint's extreme stability across legal regimes, philosophical traditions, and time periods — once adopted, abolitionist jurisprudence does not reverse. Resistance (0.08) reflects minimal structural opposition at the civilizational level; even retentionist states acknowledge the constraint's legitimacy in principle (they claim exception on grounds of necessity, sovereignty, or deterrence, not by denying the constraint's force).
 *
 * PERSPECTIVAL GAP:
 *   Minimal perspectival gap is expected for a genuine mountain constraint. All three perspectives (analytical observer, condemned person, international system) classify as mountain because the constraint is immutable across all contexts. The gap exists between THIS reading and the SIBLING readings (retributive, deterrence), not within the abolition reading itself. If a perspective emerged that saw the abolition reading as mutable or negotiable, that perspective would be measuring a different constraint or rejecting the kernel's authority — not seeing the same constraint differently.
 *
 * DIRECTIONALITY LOGIC:
 *   The abolition reading has no beneficiary or victim in the extractive sense — it is not an extraction mechanism that benefits one party at the expense of another. The condemned person is a victim in the normative sense (subject to a prohibition that protects them) but not in the structural sense (not bearing extraction). The constraint's directionality is therefore zero and flat across all perspectives: all agents, regardless of power level, exit options, or scope, experience the same immutable prohibition. The mountain classification derives from the constraint's invariance across all indexical contexts, not from directionality calculation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_identity_deontological_vs_empirical,
    'Is the abolition reading grounded in deontological human rights (intrinsic moral status that cannot be overridden by consequences) or in empirical claims about deterrence failure and irreversibility risk?',
    'Identify the core distinguishing premise: does the framework hold that ''state killing is impermissible because some persons possess inherent rights that the state cannot violate'' (deontological), or ''state killing is impermissible because empirical evidence shows it fails as deterrence and irreversibility creates unacceptable error risk'' (empirical)? This determines whether the reading forecloses the deterrence reading or merely influences it.',
    'If deontological grounding: the abolition reading FORECLOSES the deterrence reading (utilitarianism about execution cannot coexist with inalienable-rights framework). If empirical grounding: the abolition reading INFLUENCES the deterrence reading (challenges its factual premises but allows the deterrence reading to persist if new data emerged supporting deterrent effect). The distinctions affect which sibling axioms are holdable vs. overridden.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity_deontological_vs_empirical, conceptual, 'Whether abolition reading''s core is deontological or empirical').

omega_variable(
    condemned_person_moral_status_criterion,
    'What specific criterion grounds the condemned person''s irreducible moral status in the abolition reading — personhood at all times, capacity for reform, inviolable dignity, or relational membership in the human community?',
    'Trace the framework''s justification for why execution is categorically impermissible. Different criteria (personhood, dignity, reform capacity, relational belonging) have different structural relationships to the retributive and deterrence readings.',
    'If criterion is personhood or dignity (non-negotiable): forecloses retributive reading (which treats moral status as variable by desert). If criterion is capacity for reform: influences but does not foreclose retributive reading (retributivists might argue desert is permanent). If criterion is relational belonging: influences deterrence reading (community cannot authorize destruction of its own members).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(condemned_person_moral_status_criterion, conceptual, 'What grounds irreducible moral status in abolition framework').

omega_variable(
    irreversibility_as_logical_vs_empirical,
    'Is irreversibility (the logical fact that death is permanent) the SAME constraint as the abolition reading, or is it a SEPARATE empirical support mechanism that reinforces the deontological reading?',
    'Distinguish: (1) Logical irreversibility — death cannot be undone, therefore ANY system permitting error (and human systems DO permit error) is logically incompatible with executing humans. (2) Empirical irreversibility — wrongful convictions occur at measured rates; therefore, the expected value of execution includes a term for irreversible error cost. These produce different structural relationships to other readings.',
    'If logical constraint: abolition reading stands independently; irreversibility is a separate mountain constraint in the family. If empirical support: irreversibility strengthens the empirical critique of deterrence/retribution but does not independently foreclose them. Affects how the engine networks these constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_as_logical_vs_empirical, conceptual, 'Whether irreversibility is intrinsic to abolition reading or separate support mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_killing_authority__abolition_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(state_kill_abol_tr_t0, state_killing_authority__abolition_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(state_kill_abol_tr_t5, state_killing_authority__abolition_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(state_kill_abol_tr_t10, state_killing_authority__abolition_reading, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(state_kill_abol_be_t0, state_killing_authority__abolition_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(state_kill_abol_be_t5, state_killing_authority__abolition_reading, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(state_kill_abol_be_t10, state_killing_authority__abolition_reading, base_extractiveness, 10, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_killing_authority__abolition_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_killing_authority__abolition_reading, state_killing_authority__retributive_reading).
narrative_ontology:affects_constraint(state_killing_authority__abolition_reading, state_killing_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_killing_authority__abolition_reading, irreversibility_constraint).
narrative_ontology:affects_constraint(state_killing_authority__abolition_reading, wrongful_conviction_risk).

% DUAL FORMULATION NOTE:
% The state_killing_authority kernel has three structurally distinct constraint readings: abolition_reading (this constraint, ε=0.08, Mountain), retributive_reading (ε≈0.55, Tangled Rope), and deterrence_reading (ε≈0.50, Tangled Rope). Each reading instantiates a different constraint family. The abolition reading is upstream in the network — it provides logical constraints that downstream readings must acknowledge or refute. Separate constraints (irreversibility_constraint, wrongful_conviction_risk) provide empirical support mechanisms that strengthen but do not themselves constitute the abolition reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
