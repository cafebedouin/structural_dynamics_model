% ============================================================================
% CONSTRAINT STORY: security_arms_amendments__third_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_third_amendment, []).

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
 *   constraint_id: security_arms_amendments__third_amendment
 *   human_readable: Third Amendment: Prohibition on Peacetime Quartering of Soldiers
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The Third Amendment forbids the quartering of soldiers in private homes
 *   in peacetime without the consent of the householder. It is one of the
 *   least litigated amendments to the Constitution, appearing in only a
 *   handful of cases since ratification. The constraint represents the
 *   founders' direct structural response to a specific imperial abuse: the
 *   practice of forcing colonists to house and feed British soldiers during
 *   the occupation. Unlike the Second Amendment, which addresses the power
 *   asymmetry between individual and state through the right to bear arms,
 *   the Third Amendment protects the inviolability of private domestic space
 *   as a limit on the state's power to conscript private property for
 *   military logistics. The constraint is framed as a mountain — an immutable
 *   prohibition on a class of state action — but it becomes analytically
 *   interesting precisely because it is so lightly tested and so contingent
 *   on a specific historical abuse that is no longer a serious practical
 *   threat to contemporary householders. The measurement trajectory shows
 *   minimal theater ratio increase over 250 years (0.12 → 0.18), reflecting
 *   that the constitutional prohibition has remained stable in force even as
 *   the practical threat has receded. The low extractiveness (0.08) is the
 *   signature of a genuine natural law boundary rather than an extraction
 *   mechanism: there is no ongoing systematic extraction by one group from
 *   another; the constraint simply prevents a certain class of state action.
 *
 * KEY AGENTS:
 *   - Private Householders: Primary beneficiaries (powerless/trapped biographical, institutional arbitrage civilizational) — the class protected from military quartering without consent. The constraint exists to prevent extraction of their domestic space and resources (housing, food, labor) for military logistics.
 *   - Military Institution: Implicit target (institutional/arbitrage) — the logistical pressure to house troops is suppressed; the military's ability to requisition private homes is the extraction mechanism that the prohibition forecloses.
 *   - Constitutional Order: Institutional enforcer (institutional/arbitrage) — maintains and interprets the prohibition across centuries. Benefits from the constraint as a boundary that protects the constitutional architecture's legitimacy.
 *   - State Power (Civilizational): Abstract agent (analytical/analytical) — from the long view, the Third Amendment represents a limit on statecraft itself: the state cannot treat private domestic space as a commons available for its use. This is a structural boundary of liberal constitutional orders.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(security_arms_amendments__third_amendment, 0.08).
domain_priors:suppression_score(security_arms_amendments__third_amendment, 0.02).
domain_priors:theater_ratio(security_arms_amendments__third_amendment, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, extractiveness, 0.08).
narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(security_arms_amendments__third_amendment, mountain).
narrative_ontology:human_readable(security_arms_amendments__third_amendment, "Third Amendment: Prohibition on Peacetime Quartering of Soldiers").
narrative_ontology:topic_domain(security_arms_amendments__third_amendment, "constitutional/political").

domain_priors:emerges_naturally(security_arms_amendments__third_amendment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(security_arms_amendments__third_amendment, 'a4a5e836-f7a6-4b2a-a4aa-0384456679bc').
narrative_ontology:cs_kernel_codification('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', formalized).
narrative_ontology:cs_authority_grounding('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', lineage).
narrative_ontology:cs_interpretation_layer_present('a4a5e836-f7a6-4b2a-a4aa-0384456679bc').
narrative_ontology:cs_reading_relation('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', security_arms_amendments__second_amendment, coexists_with).
narrative_ontology:cs_axiom('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', foundational, private_home_inviolable_peacetime).
narrative_ontology:cs_axiom_status(private_home_inviolable_peacetime, holdable).
narrative_ontology:cs_axiom_grounding('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', private_home_inviolable_peacetime, deontological).
narrative_ontology:cs_axiom('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', foundational, state_cannot_conscript_private_property_without_consent).
narrative_ontology:cs_axiom_status(state_cannot_conscript_private_property_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', state_cannot_conscript_private_property_without_consent, deontological).
narrative_ontology:cs_reference_frame('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', household_inviolability_from_military_conscription).
narrative_ontology:cs_drift_state('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', contemporary_peace, gap(stable, minor, true)).
narrative_ontology:cs_created_at('a4a5e836-f7a6-4b2a-a4aa-0384456679bc', '').
narrative_ontology:cs_kernel_id(security_arms_amendments__third_amendment, security_arms_amendments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(security_arms_amendments__third_amendment, private_householders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HOUSEHOLDER (MOUNTAIN) — A private citizen in colonial America faced with military quartering has no exit option; the constraint is absolute and irreducible. From the perspective of someone whose home is claimed for military use, the prohibition is a fixed point of constitutional law — unchangeable and necessary for the household's integrity.
constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMMUNITY / GENERATIONAL VIEW (MOUNTAIN) — Across generations, the Third Amendment represents a foundational limit on state power to invade private domestic space. The constraint is not contingent on political opinion or resource availability; it is a structural protection built into constitutional architecture. Even with constrained exit options (one can challenge via courts), the underlying limit is immutable.
constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, mountain,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL INSTITUTIONAL ORDER (MOUNTAIN) — From the perspective of constitutional institutions operating across civilizational timescales, the Third Amendment codifies a limit on the state's power over private domestic space that is fundamental and unchangeable within the constitutional framework. The institutional actor with arbitrage options (the state) experiences this as an immutable boundary, not a negotiable constraint.
constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal analytical perspective, the Third Amendment instantiates a principle about the inviolability of private domestic space that appears to be a structural feature of liberal constitutional orders: the state cannot unilaterally transform a private home into a public resource without consent. This reads as a natural law of constitutional architecture.
constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(security_arms_amendments__third_amendment_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(security_arms_amendments__third_amendment, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, ExtMetricName, E),
    domain_priors:suppression_score(security_arms_amendments__third_amendment, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(security_arms_amendments__third_amendment),
    narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(security_arms_amendments__third_amendment, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(security_arms_amendments__third_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The Third Amendment codifies a prohibition, not an ongoing extraction mechanism. The constraint prevents a potential extraction (forced quartering) rather than managing an existing one. The low value reflects that there is no systematic asymmetric flow of resources from one group to another; the constraint is purely protective/prohibitory. Suppression (0.02): Minimal. The legal prohibition is clear and universally enforced; there is no ambiguity about what counts as peacetime quartering, and no significant pressure to violate the rule. Wartime quartering remains constitutionally permissible (though regulated by statute), but the peacetime prohibition is absolute. Theater ratio (0.15): Low. The constraint requires minimal performative activity; it is enforced through straightforward legal prohibition rather than through institutional ritual or symbolic action. The slight theater reflects the ceremonial aspects of constitutional interpretation and periodic reaffirmation by courts, but the core prohibition is substantive, not performative. Emerges naturally (true): The constraint is framed by the constitutional text as a natural boundary on state action — the phrase 'shall not be quartered' is an absolute prohibition, not a contingent rule subject to legislative override. From the perspective of constitutional architecture, the inviolability of private domestic space appears as a bedrock principle rather than a negotiable coordination rule.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits unusual perspective convergence: all four perspectives classify identically as mountain. This is not an error — it reflects the constraint's genuine immutability across different observer positions. A householder, a community, an institution, and an analytical observer all agree that peacetime quartering is categorically forbidden. The gap is not in classification but in *significance*. For the householder, the Third Amendment is a vital protection against a concrete threat (even if historically distant). For the institution, it is a constitutional boundary they interpret. For the analytical observer, it is a principle about the limits of state power. For the community across generations, it is a foundational guarantee. All perspectives see the same constraint (mountain) but experience its salience differently.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experience of the constraint is modulated by their structural position and exit options. A householder with trapped exit options experiences maximum d-value (the constraint protects them absolutely from a threat they cannot escape). A constitutional institution with arbitrage options experiences low d-value (the constraint is a boundary they interpret and work within, not a binding personal constraint). The analytical observer at civilizational scope experiences d-value close to 0.5 (the constraint is neither targeting nor favoring them — it is a structural feature of the order they observe). The chi formula applies uniformly: low base extractiveness, low suppression, minimal scope amplification → consistently low effective extraction across all perspectives. The perspectival gap here is not about disagreement on classification (all perspectives agree: mountain) but about the *experience* of why the constraint matters. For the householder, it is existential protection; for the institution, it is a structural boundary; for the analytical observer, it is a civilizational principle. The classification converges because the constraint is genuinely immutable across all contexts.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy to resolve here. The Third Amendment is genuinely a mountain constraint — a natural law of constitutional architecture that prohibits a class of state action. The extractiveness is minimal (0.08), suppression is minimal (0.02), and all perspectives converge on the same classification. The constraint does not pretend to be coordination while extracting; it is purely protective. However, the analytical omega variable raises a meta-question: is this really a natural law, or a historically contingent prohibition on a specific abuse whose threat has largely receded? If the latter, the constraint might be reclassified as a piton (degraded, maintained through institutional inertia and symbolic importance rather than living practical necessity) or even a rope (coordination mechanism for household protection). The low theater ratio (0.15) argues against piton; the high accessibility collapse (0.92) and low resistance (0.08) argue for mountain. The constraint remains a mountain as long as the principle (no uncompensated state seizure of private property for military use) is actively enforced and not merely ceremonial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_prohibition,
    'Is the Third Amendment a codification of a natural-law limit on state power, or a historically contingent response to a specific imperial abuse that benefits identifiable agents (householders) and constrains others (military logistics)?',
    'Examine whether the principle (state cannot unilaterally convert private property to public use without consent) appears across other constitutional systems independently of direct colonial influence. If universal, natural law candidate; if unique to Anglo-American tradition post-quartering abuse, constructed constraint with identifiable beneficiaries.',
    'If natural law: mountain classification stands unchanged. If constructed: false summit detection triggers; beneficiaries present in base_properties → engine reclassifies to tangled_rope or rope depending on military extraction magnitude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_prohibition, conceptual, 'Whether the Third Amendment prohibition is a natural law principle or a historically contingent anti-extraction mechanism').

omega_variable(
    quartering_extraction_suppression,
    'In peacetime, how much of the suppression against quartering is structural (legal prohibition) vs. purely historical (no longer a serious practical threat)?',
    'Historical analysis: Are there documented post-ratification attempts to quarter soldiers in private homes? Are there contemporary jurisdictions where similar military billeting occurs without explicit prohibition? Comparison to jurisdictions with identical legal prohibitions but higher documented attempts.',
    'If suppression is entirely legal/symbolic (no practical quartering attempted post-ratification): constraint is piton-adjacent (performative enforcement of obsolete threat). If active suppression of residual military logistics pressure persists: mountain remains intact as meaningful boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(quartering_extraction_suppression, empirical, 'Degree to which Third Amendment suppression is active legal boundary vs. historical relic').

omega_variable(
    reading_sibling_scope_difference,
    'Does the Second Amendment reading of security_arms_amendments instantiate a different extraction mechanism or threat model than the Third Amendment reading?',
    'Structural comparison: Second Amendment concerns individual-state power asymmetry (armed population vs armed state); Third Amendment concerns state power over domestic space (quartering vs household sanctity). These are distinct vulnerability classes. Examine whether both readings operate from the same originary abuse (colonial overreach) or different abuses.',
    'If different abuses: readings coexist but do not structurally constrain each other. If same abuse: readings may influence each other (how one is enforced affects the other''s threat model).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_sibling_scope_difference, conceptual, 'Structural relationship between Second and Third Amendment readings of security_arms_amendments kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(security_arms_amendments__third_amendment, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(third_amend_tr_t0, security_arms_amendments__third_amendment, theater_ratio, 0, 0.12).
narrative_ontology:measurement(third_amend_tr_t125, security_arms_amendments__third_amendment, theater_ratio, 125, 0.15).
narrative_ontology:measurement(third_amend_tr_t250, security_arms_amendments__third_amendment, theater_ratio, 250, 0.18).

% Extraction over time
narrative_ontology:measurement(third_amend_be_t0, security_arms_amendments__third_amendment, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(third_amend_be_t125, security_arms_amendments__third_amendment, base_extractiveness, 125, 0.08).
narrative_ontology:measurement(third_amend_be_t250, security_arms_amendments__third_amendment, base_extractiveness, 250, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(security_arms_amendments__third_amendment, enforcement_mechanism).
narrative_ontology:affects_constraint(security_arms_amendments__third_amendment, second_amendment).

% DUAL FORMULATION NOTE:
% The Third Amendment and Second Amendment are both readings of the security_arms_amendments kernel. They address different security vulnerabilities: the Third protects domestic space from military conscription; the Second protects individual armed capacity from state monopolization. The network edge represents this structural kinship — both readings emerged from colonial-era abuse and ground constitutional protection in the same foundational principle (limiting military power over civilian life). However, the two readings operate on different targets and produce different extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
