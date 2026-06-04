% ============================================================================
% CONSTRAINT STORY: crisis_machinery__dictatorship_term_limited
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_crisis_machinery__dictatorship_term_limited, []).

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
 *   constraint_id: crisis_machinery__dictatorship_term_limited
 *   human_readable: Classical Dictatorship: Term-Limited Emergency Authority
 *   domain: legal/political_philosophy/constitutional_design
 *
 * SUMMARY:
 *   The classical Roman dictatorship represents a specific institutional
 *   design for emergency governance: total executive command concentrated in
 *   a single named office, with authority strictly bounded by a defined term
 *   (typically six months) and a specific mandate (repel invasion, restore
 *   the state, conduct elections). The form embodies the principle that
 *   emergency power can be legitimate and non-extractive if and only if it is
 *   temporary, task-specific, and voluntarily relinquished. Cincinnatus
 *   exemplifies the archetype: appointed dictator to repel invasion or settle
 *   internal disorder, accomplishes the task, and lays down power at the
 *   deadline to return to private life. This constraint operates at the
 *   intersection of legal form (the dictatorship as office), political
 *   philosophy (emergency vs. normal authority), and historical practice
 *   (whether the institutional design actually constrains behavior or merely
 *   performs constraint). The constraint's extractiveness (0.38) reflects
 *   moderate asymmetry: suppression of checks and veto is severe during the
 *   emergency window, but the designed sunset clause and task specificity
 *   create a built-in ceiling on extraction. The classical reading treats
 *   this as a successful coordination mechanism — beneficiaries gain
 *   crisis-response capacity, victims (individual appeal and institutional
 *   checks) experience temporary suppression that is algorithmically bounded.
 *   This reading exists in contestation with senatus_consultum_ultimum (which
 *   eliminates the term limit and task specificity) and Sulla's inversion
 *   (which converted the temporary office into an indefinite power to
 *   restructure the state).
 *
 * KEY AGENTS:
 *   - Unified Executive Command (institutional/arbitrage): Beneficiary — acquires decisive authority during crisis; exits by constitutional requirement at six-month term
 *   - Individual Citizens within Jurisdiction (powerless/trapped): Victim — lose appeal and veto during emergency window; cannot exit the constraint
 *   - Provincial Elites (moderate/constrained): Secondary victims and partial beneficiaries — suspended authority but benefit from rapid crisis resolution; constrained exit until term expires
 *   - Senate and People (institutional/arbitrage): Formal beneficiary and authorizing body — solves collective action problem by delegating to unified command; retains sovereignty and reacquires authority at deadline
 *   - The Dictator (powerful/mobile): Individual holding office — experiences high functional authority but normative pressure and institutional expectation to relinquish at deadline
 *   - Constitutional Framework (institutional/arbitrage): Maintains the institutional form across time; by late Republic becomes mostly vestigial (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(crisis_machinery__dictatorship_term_limited, 0.38).
domain_priors:suppression_score(crisis_machinery__dictatorship_term_limited, 0.48).
domain_priors:theater_ratio(crisis_machinery__dictatorship_term_limited, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(crisis_machinery__dictatorship_term_limited, extractiveness, 0.38).
narrative_ontology:constraint_metric(crisis_machinery__dictatorship_term_limited, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(crisis_machinery__dictatorship_term_limited, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(crisis_machinery__dictatorship_term_limited, scaffold).
narrative_ontology:human_readable(crisis_machinery__dictatorship_term_limited, "Classical Dictatorship: Term-Limited Emergency Authority").
narrative_ontology:topic_domain(crisis_machinery__dictatorship_term_limited, "legal/political_philosophy/constitutional_design").

domain_priors:requires_active_enforcement(crisis_machinery__dictatorship_term_limited).
narrative_ontology:has_sunset_clause(crisis_machinery__dictatorship_term_limited).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(crisis_machinery__dictatorship_term_limited, 'fa30b225-d1f9-4bf9-9dd6-b031f05c777d').
narrative_ontology:cs_kernel_codification('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', formalized).
narrative_ontology:cs_authority_grounding('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', lineage).
narrative_ontology:cs_interpretation_layer_present('fa30b225-d1f9-4bf9-9dd6-b031f05c777d').
narrative_ontology:cs_reading_relation('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', crisis_machinery__senatus_consultum_ultimum, coexists_with).
narrative_ontology:cs_reading_relation('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', crisis_machinery__sulla_constitutional_reaction, forecloses).
narrative_ontology:cs_axiom('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', foundational, emergency_authority_requires_temporal_boundary).
narrative_ontology:cs_axiom_status(emergency_authority_requires_temporal_boundary, holdable).
narrative_ontology:cs_axiom_grounding('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', emergency_authority_requires_temporal_boundary, deontological).
narrative_ontology:cs_axiom('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', foundational, task_specificity_constrains_scope).
narrative_ontology:cs_axiom_status(task_specificity_constrains_scope, holdable).
narrative_ontology:cs_axiom_grounding('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', task_specificity_constrains_scope, instrumental).
narrative_ontology:cs_reference_frame('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', classical_bounded_emergency_office).
narrative_ontology:cs_drift_state('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', late_republic_institutional_evolution, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fa30b225-d1f9-4bf9-9dd6-b031f05c777d', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(crisis_machinery__dictatorship_term_limited, crisis_machinery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(crisis_machinery__dictatorship_term_limited, unified_executive_command).
narrative_ontology:constraint_beneficiary(crisis_machinery__dictatorship_term_limited, crisis_response_capacity).
narrative_ontology:constraint_victim(crisis_machinery__dictatorship_term_limited, institutional_checks).
narrative_ontology:constraint_victim(crisis_machinery__dictatorship_term_limited, appeal_and_veto_mechanisms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUPPRESSED CITIZEN (SNARE) — Individual caught in the dictatorship's jurisdiction faces total command authority with no appeal, no veto, no recourse. The six-month term is irrelevant to the trapped agent experiencing immediate extraction; they cannot exit. Experiences maximum suppression within the emergency window.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PROVINCIAL ELITE (TANGLED ROPE) — Regional actors benefit from unified command's ability to coordinate rapid response to crisis (external threat, internal breakdown), but their traditional authority is suspended for the duration. They experience both coordination benefit (crisis solved faster) and extraction (loss of authority and veto). Constrained exit — they can wait out the term but cannot oppose openly.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE SENATE (ROPE) — Institutional beneficiary that grants dictatorship authority; experiences the constraint as coordination mechanism. The Senate solves a collective action problem (cannot act decisively in crisis under normal procedures) by granting temporary unified command. High arbitrage — the Senate exits by definition at the six-month term, retains formal sovereignty, and gains the benefit of crisis resolution. Minimal suppression from their position; the institutional machinery works as designed.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: THE DICTATORSHIP OFFICE (SCAFFOLD) — Organized actors (the dictator, their staff, the crisis response apparatus) see this as temporary support structure with enforced sunset. The office has agency to act decisively during the emergency but is structurally bound by the six-month term and specific mandate (e.g., 'restore the city,' 'repel invasion'). Low effective extraction because the sunset is built-in and the office is designed to be laid down. The constraint's entire function is to enable rapid, authorized action and then dissolve.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE DICTATOR — CLASSICAL MODEL (ROPE) — Individual holding office experiences high authority but also high normative pressure to lay down power at the deadline. The classical model (Cincinnatus archetype) sees this as coordination: the office grants temporary unified command in exchange for voluntary relinquishment. The dictator's mobile exit option (they can leave the office and return to private life) makes this rope rather than snare. Theater is low — the dictator's actual power during the emergency is functional, not performative; the term limit is enforced by cultural/institutional expectation, not by institutional machinery that can constrain a determined actor.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: THE CONSTITUTIONAL FRAMEWORK — PITON (CIVILIZATIONAL) — From a civilizational timescale, the classical dictatorship becomes a vestigial constitutional form. Its functional role was real in small republics facing existential threats, but as state capacity grew and alternative coordination mechanisms developed, the office persisted through institutional inertia. By the late Republic, the dictatorship becomes mostly theatrical — formally available but rarely used according to its original constraints (Sulla inverted the form; senatus consultum ultimum replaced it). This perspective sees the constraint as degraded, maintained by tradition rather than by any genuine necessity.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: THE ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From an abstract analytical position, emergency power with suppressed checks is an immutable feature of any political order facing existential threat. The analytical observer risks classifying the dictatorship as a natural law: 'in crisis, centralized command is necessary; alternatives cannot function fast enough.' However, the structured data contradicts this — the classical dictatorship is a designed institutional form, not a natural law. This perspective instantiates the false-summit risk: confusing 'empirically necessary in a specific historical context' with 'logically immutable.' The engine's false-summit detector will flag this.
constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(crisis_machinery__dictatorship_term_limited_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(crisis_machinery__dictatorship_term_limited, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(crisis_machinery__dictatorship_term_limited, TR),
    TR >= 0.70.

:- end_tests(crisis_machinery__dictatorship_term_limited_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, with temporal decay. At the crisis onset (t=0), extractiveness is high (0.65) — suppression of checks is severe and the full impact of unified command hits the populace immediately. By mid-term (t=3), extractiveness drops to 0.42 as the crisis stabilizes and the deadline becomes psychologically closer. By the expiration point (t=6), extractiveness collapses to 0.18 — suppressed checks and veto are restored, the dictator relinquishes office, and the constraint dissolves. The base measure of 0.38 reflects the time-averaged extraction across the interval. This trajectory is the hallmark of a true scaffold: extraction is concentrated in the urgent phase, then mechanically reduced as the term expires. Suppression (0.48): Moderate-high, also with temporal decay. At crisis onset, suppression of appeal and veto is severe (0.72) — individuals and institutions have no recourse. By mid-term (0.55), suppression remains high but the population begins to anticipate restoration of normal process. By term end (0.08), suppression drops to near-zero — all suppressed mechanisms are restored by design. The 0.48 base reflects the averaged suppression across the term. Theater ratio (0.35): Low, indicating functional rather than performative activity. During the emergency, the dictator's authority is exercised for actual crisis response — conducting military operations, issuing decrees that solve coordination problems, organizing resource mobilization. The theater is minimal because the office's entire point is functional action, not ritual. As the constraint enters its late phase (t=6), theater rises slightly (0.38) as the office becomes more symbolic — the dictator may perform ceremonial relinquishment or symbolic steps toward restoration.
 *
 * PERSPECTIVAL GAP:
 *   The full perspectival range spans from snare (powerless citizen, immediate horizon) through tangled rope (provincial elite with mixed extraction and coordination benefit) to rope (Senate's coordination benefit) to scaffold (the dictatorship office itself, with enforced sunset) to piton (the form becomes vestigial in later periods) to the analytical observer's false-summit mountain (naturalizing what is actually a designed institutional form). The key gap is between the beneficiary's experience (rope/scaffold: 'this solves our crisis coordination problem') and the victim's experience (snare: 'I have no appeal or veto and cannot exit'). A second gap opens between the classical term-limited reading (beneficiaries benefit from crisis response capacity, victims face temporary suppression with an algorithmic ceiling) and the Sulla inversion reading (the office expands into indefinite power to restructure the state, converting temporary suspension into potential permanent transformation). The gaps reveal why the form is contested: the classical reading's boundary conditions (six months, specific task) are the exact points where Sulla's inversion breaks the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies by agent's structural position. The beneficiary (unified executive command) has arbitrage exit — the Senate grants authority and reacquires it at the deadline, making d ≈ 0.15 (beneficiary with planned exit path) and f(d) ≈ -0.01, producing near-zero or negative effective extraction chi for this agent. They experience the constraint as coordination. The victims (individual citizens, institutional checks) have trapped exit (cannot exit the dictatorship's jurisdiction) or constrained exit (can wait but cannot actively oppose), producing d ≈ 0.85–0.95, f(d) ≈ 1.15–1.42, and high experienced extraction chi. The Senate authorizing the dictatorship has arbitrage exit similar to the office itself — they experience rope-type extraction. The Cincinnatus-model dictator has mobile exit (can lay down power voluntarily), producing d ≈ 0.55, f(d) ≈ 0.75. This agent's classification varies by time horizon: at immediate timescale, the dictator experiences snare (trapped by the emergency); at biographical timescale, they experience rope (they can exit voluntarily and do). The civilization-scale observer risks identity_locked institutional analysis, seeing emergency power as natural necessity rather than as constructed institutional choice.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    term_limit_enforceability,
    'Is the six-month term limit legally enforced by institutional machinery or socially enforced by cultural expectation and reputation cost?',
    'Historical analysis of dictators who attempted to extend their mandate; examination of institutional mechanisms that could prevent or compel relinquishment; comparative study of legal vs cultural enforcement in other temporary office structures',
    'If institutional enforcement: the term limit is a hard gate on extractiveness. If cultural enforcement: the term limit depends on the dictator''s social position and internalization of norms — Cincinnatus''s voluntary relinquishment proves the form works, but Sulla''s extended tenure proves it can fail. This affects whether the constraint is genuinely scaffold (sunset enforced) or whether it coexists with piton (theatrical sunset that can be overridden).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(term_limit_enforceability, empirical, 'Whether term limit is legally enforced or culturally dependent').

omega_variable(
    task_specificity_constraint,
    'Does naming a specific task (restore the city, repel invasion, write the constitution) actually constrain the dictator''s authority in practice, or does it function as a formality that the executive power can reinterpret or expand?',
    'Analysis of historical dictatorships: scope of actions taken vs. stated mandate; examination of how ''task'' is framed and reframed; comparison of narrow-mandate vs broad-mandate dictatorships and their actual scope creep',
    'If task meaningfully constrains: the dictatorship is a true scaffold — suppression is bounded by design. If task is reinterpretable: the dictatorship is or becomes a snare disguised as a scaffold — the formal limit is performative. This directly affects whether extractiveness is correctly valued at 0.38 or should be higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(task_specificity_constraint, empirical, 'Whether task mandate actually constrains dictatorial authority or functions as formality').

omega_variable(
    necessity_vs_construction,
    'Is emergency dictatorship a natural law of political order (immutable response to existential crisis) or a constructed institutional choice among alternatives?',
    'Comparative constitutional history: do all political systems facing existential threats adopt dictatorship-like structures? Do any successfully use alternative mechanisms (distributed command, rapid delegation, organized council)? What allows some systems to avoid centralized emergency authority?',
    'If natural law: the constraint should be mountain, and any beneficiaries are incidental to an immutable need. If constructed choice: the constraint is tangled rope or scaffold (beneficiaries benefit from this specific design choice) and the false-summit signature should fire. This omega documents the kernel contest: dictatorship_term_limited treats emergency power as a designed form with built-in limits, while the analytical view risks naturalizing it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_construction, conceptual, 'Whether emergency authority is natural law or constructed institutional choice').

omega_variable(
    sibling_reading_interaction,
    'How does the senatus_consultum_ultimum (blank-check decree) relate to the term-limited dictatorship — is it a replacement of the dictatorship or a parallel mechanism?',
    'Historical and legal analysis of the relationship between dictatorship and senatus consultum ultimum; timeline of when each was used; doctrinal statements from Roman jurists about their relationship; examination of whether the blank-check decree was adopted as a substitute for dictatorship or as a supplementary tool',
    'If replacement: the dictatorship''s scope narrowed and the blank-check mechanism supplanted it, suggesting the term-limited form became obsolete (piton reading). If parallel: both coexist, suggesting different reading communities maintained different institutional forms (coexists_with relation). This affects the reading_relations declaration in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_interaction, empirical, 'Relationship between dictatorship and senatus consultum ultimum').

omega_variable(
    sulla_inversion_scope,
    'When Sulla inverted the dictatorship (indefinite tenure, ''to write the constitution''), did he claim to operate within the same institutional form or explicitly adopt a new form?',
    'Textual analysis of Sulla''s proclamations and justifications; legal commentary from contemporary and later sources; examination of whether Sulla''s act was described as an extension of dictatorship or as its negation and replacement',
    'If within same form (just abusing it): the dictatorship_term_limited reading forecloses the Sulla reading within a single framework — they cannot coexist as readings of the same institutional commitment. If Sulla explicitly broke with the form: the readings coexist, representing different parties'' commitments (the classical tradition vs Sulla''s inversion). This determines the forecloses vs coexists_with relation in reading_relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sulla_inversion_scope, empirical, 'Whether Sulla''s dictatorship was within or outside the classical form').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(crisis_machinery__dictatorship_term_limited, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dictatorship_term_limited_tr_t0, crisis_machinery__dictatorship_term_limited, theater_ratio, 0, 0.28).
narrative_ontology:measurement(dictatorship_term_limited_tr_t3, crisis_machinery__dictatorship_term_limited, theater_ratio, 3, 0.32).
narrative_ontology:measurement(dictatorship_term_limited_tr_t6, crisis_machinery__dictatorship_term_limited, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(dictatorship_term_limited_be_t0, crisis_machinery__dictatorship_term_limited, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(dictatorship_term_limited_be_t3, crisis_machinery__dictatorship_term_limited, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(dictatorship_term_limited_be_t6, crisis_machinery__dictatorship_term_limited, base_extractiveness, 6, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(dictatorship_term_limited_su_t0, crisis_machinery__dictatorship_term_limited, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(dictatorship_term_limited_su_t3, crisis_machinery__dictatorship_term_limited, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(dictatorship_term_limited_su_t6, crisis_machinery__dictatorship_term_limited, suppression_requirement, 6, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(crisis_machinery__dictatorship_term_limited, enforcement_mechanism).
narrative_ontology:affects_constraint(crisis_machinery__dictatorship_term_limited, crisis_machinery__senatus_consultum_ultimum).
narrative_ontology:affects_constraint(crisis_machinery__dictatorship_term_limited, crisis_machinery__sulla_constitutional_reaction).

% DUAL FORMULATION NOTE:
% The crisis_machinery kernel has three structurally distinct readings with different epsilon values and beneficiary/victim configurations. dictatorship_term_limited (this constraint) models the classical bounded form with ε=0.38, suppression capped by design. senatus_consultum_ultimum models the blank-check decree with higher ε and suppression (no term, no task boundary). sulla_constitutional_reaction models the inversion with indefinite tenure and proscription as method. All three are readings of the same kernel (Roman emergency authority in crisis), but each has distinct ε values reflecting their different boundary conditions. Link all three via network.affects_constraints to establish the family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
