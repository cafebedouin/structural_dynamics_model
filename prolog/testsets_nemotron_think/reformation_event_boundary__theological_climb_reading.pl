% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Reformation as Theological Climb: Justification by Faith Alone
 *   domain: historical_epistemology/religious_history/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the theological_climb_reading of the
 *   contested kernel 'reformation_event_boundary'. The reading asserts that
 *   Luther's recovery of justification by faith alone (1517-1521) was a
 *   genuine doctrinal breakthrough — a climb to a higher theological truth —
 *   that structurally necessitated institutional separation from Rome. The
 *   constraint is the event boundary itself: the claim that the Reformation
 *   IS this theological climb, periodized tightly 1517-1555 (Theses to
 *   Augsburg Peace). The reading claims Mountain type: the doctrinal truth
 *   emerges naturally from scripture (emerges_naturally=true), collapses
 *   alternatives (high accessibility_collapse), and meets fierce resistance
 *   (high resistance) not because it extracts but because it overturns a
 *   millennium of institutionalized error. Beneficiaries (believers freed
 *   from false doctrine) and victims (Catholic institutional authority) are
 *   declared, triggering FSM evaluation — the central omega question.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.12).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.22).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Climb: Justification by Faith Alone").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '24ffe84e-fcdb-4ae8-8339-409b7479803d').
narrative_ontology:cs_kernel_codification('24ffe84e-fcdb-4ae8-8339-409b7479803d', fixed_text).
narrative_ontology:cs_authority_grounding('24ffe84e-fcdb-4ae8-8339-409b7479803d', lineage).
narrative_ontology:cs_interpretation_layer_present('24ffe84e-fcdb-4ae8-8339-409b7479803d').
narrative_ontology:cs_reading_relation('24ffe84e-fcdb-4ae8-8339-409b7479803d', reformation_event_boundary__political_swap_reading, coexists_with).
narrative_ontology:cs_reading_relation('24ffe84e-fcdb-4ae8-8339-409b7479803d', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('24ffe84e-fcdb-4ae8-8339-409b7479803d', foundational, justification_by_faith_alone_is_scriptural_truth).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_scriptural_truth, holdable).
narrative_ontology:cs_axiom_grounding('24ffe84e-fcdb-4ae8-8339-409b7479803d', justification_by_faith_alone_is_scriptural_truth, theological).
narrative_ontology:cs_axiom('24ffe84e-fcdb-4ae8-8339-409b7479803d', secondary, institutional_separation_was_necessitated_by_doctrinal_truth).
narrative_ontology:cs_axiom_status(institutional_separation_was_necessitated_by_doctrinal_truth, holdable).
narrative_ontology:cs_axiom_grounding('24ffe84e-fcdb-4ae8-8339-409b7479803d', institutional_separation_was_necessitated_by_doctrinal_truth, theological).
narrative_ontology:cs_reference_frame('24ffe84e-fcdb-4ae8-8339-409b7479803d', apostolic_gospel_purity).
narrative_ontology:cs_drift_state('24ffe84e-fcdb-4ae8-8339-409b7479803d', late_medieval_papal_church, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('24ffe84e-fcdb-4ae8-8339-409b7479803d', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_institutional_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, luther_and_reformers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, secular_rulers_german_territories).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, justification_by_faith_alone).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, scripture_alone_authority).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, priesthood_of_all_believers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lay Christians who receive the recovered gospel of justification by faith alone. Their religious identity fuses with the doctrinal breakthrough; exit means abandoning the truth they have recognized. They gain theological assurance and direct access to scripture but lose the mediating sacramental framework.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine, beneficiary,
    organized, generational, identity_locked, continental).

% The papal hierarchy and curial structure that defined Western Christianity for a millennium. The theological correction strips its exclusive claim to mediate salvation, triggering loss of territorial sovereignty, revenue streams (indulgences, benefices), and jurisdictional monopoly. It cannot exit the constraint because the constraint IS the loss of its defining authority.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_institutional_authority, payer,
    institutional, civilizational, trapped, continental).

% The theologians and preachers who articulate and defend the recovered doctrine. They set the interpretive agenda (sola scriptura, sola fide) and bear the personal risk of excommunication and imperial ban. They benefit from the doctrinal vindication but are constrained by the need to maintain doctrinal coherence across fracturing reform movements.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, luther_and_reformers, agenda_setter,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, luther_and_reformers, beneficiary).

% Princes and city councils that adopt the Reformation to seize church lands, assert territorial sovereignty, and control ecclesiastical appointments. They arbitrage between papal and imperial authority. This reading treats their gain as secondary consequence, not primary driver.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers_german_territories, beneficiary,
    powerful, biographical, arbitrage, regional).

% Historians and polemicists who argue the Reformation was fundamentally a political asset-grab using theology as cover. They are excluded from this reading's framework because the reading's core premise (theology as autonomous driver) forecloses their causal primacy claim.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, political_swap_proponents, excluded,
    moderate, biographical, trapped, continental).

% Scholars who hold that theological, political, economic, and social causes are irreducible and simultaneous. They are excluded not by logical foreclosure but by this reading's periodization tightness (1517-1555) which their multi-causal framework dissolves.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, composite_overdetermination_proponents, excluded,
    moderate, biographical, constrained, continental).

% Scholars evaluating the doctrinal claims on their own terms — patristic continuity, scriptural warrant, systematic coherence. They neither collect nor pay; they assess whether the climb reading's theological premises hold.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historical_theologians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Christian believers around the recovered apostolic gospel: justification by faith alone through scripture alone, replacing a mediated sacramental system with direct access to divine promise. Solves the coordination problem of 'how is a sinner made right with God?' by anchoring assurance in Christ's finished work rather than ecclesiastical mediation.
% TRANSFER_FUNCTION: Transfers soteriological authority and ecclesiastical assets from the Roman curia to local congregations and territorial churches. Moves the power to define orthodoxy, dispense grace, and control church property from a centralized hierarchy to decentralized confessionally-defined bodies. The transfer is framed as restoration, not innovation.
% ABSENT_VOICES: The political_swap_proponents (who see theology as post-hoc rationalization for asset seizure) and composite_overdetermination_proponents (who reject single-driver periodization) are structurally excluded. The former are excluded because this reading's founding axiom forecloses their causal claim; the latter because this reading's tight periodization (1517-1555) dissolves their multi-century causal web.
% DISAPPEARANCE_RATIONALE: If the theological climb reading vanished — if justification by faith alone were not a scriptural recovery but a novel invention — the entire Protestant confessional world (liturgy, polity, piety, mission) would lose its founding warrant. The Catholic Church would not have been theologically corrected; the Council of Trent would have no doctrinal opponent; the modern map of Christian denominations would not exist.
% FOUNDING_PROBLEM: The medieval church had obscured the gospel by teaching that justification requires human cooperation with grace (merit, indulgences, sacramental works), creating a system where assurance of salvation was impossible and ecclesiastical mediation became a revenue engine. The founding problem was the loss of the apostolic doctrine of free justification.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Reformers themselves (Luther's 1518 Heidelberg Disputation, Melanchthon's 1521 Loci Communes) and by Catholic respondents at the Diet of Worms (1521) and Council of Trent (1545-1563) who explicitly condemn sola fide as heresy — confirming the doctrinal dispute was real and central. Corroboration from outside the beneficiary set comes from Erasmus (who criticized the merit system but rejected the break) and from secular chroniclers (e.g., Sleidanus) who document the doctrinal conflict as the engine of events.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, ExtMetricName, E),
    domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is low (0.12) because the reading frames the constraint as doctrinal recovery, not rent extraction. The slight rise over the interval reflects the accretion of confessional polity (ordinations, visitations, consistories) which the reading sees as necessary fruit, not parasitic growth. Suppression (0.22) captures the Catholic Church's coercive resistance (edicts, inquisitions, wars) which the reading treats as the old regime suppressing the truth, not the truth suppressing dissent. Theater_ratio (0.08) remains low because confessional standardization serves doctrinal fidelity. Accessibility_collapse (0.88) is high: once the gospel is seen as free promise, the merit system becomes unintelligible. Resistance (0.72) is high because the institutional church fights for its life.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (Catholic Church) experiences the constraint as catastrophic extraction (loss of sovereignty, revenue, monopoly). The beneficiary seats (believers, Reformers) experience it as liberation and vindication. The engine computes this divergence from the structural data: same constraint, opposite effective extraction signs. The claimed_type (mountain) reflects the agenda_setter/beneficiary perspective; the payer seat would compute as snare/tangled_rope. This divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Believers are identity_locked beneficiaries: their self-constitution fuses with the recovered gospel (d ~ 0.1). The Catholic Church is a trapped payer: it cannot exit the loss of its defining authority (d ~ 0.95). Luther_and_reformers are constrained agenda_setters who bear personal risk but set the interpretive frame (d ~ 0.3). Secular_rulers are arbitrage beneficiaries who exploit the opening (d ~ 0.2). The excluded seats (political_swap, composite) are trapped/constrained because this reading's axioms foreclose or dissolve their frameworks. Observers are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (loss of apostolic justification doctrine) is contested: the reading says it was real and solved; the Catholic Church says the doctrine was never lost (Trent's decree on justification); secular historians say the problem was institutional corruption, not doctrine. The constraint persists not because the founding problem is dead (mandatrophy) but because the doctrinal claim remains live for confessing Protestants. No mandatrophy_resolved declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_mountain_vs_constructed_boundary,
    'Is the Reformation event boundary a genuine theological mountain (scriptural truth emerging naturally) or a constructed constraint that benefits Protestant confessional identity?',
    'Patristic and medieval exegesis survey: does sola fide have continuous witness from Augustine through Bernard of Clairvaux to Luther, or is it a 16th-century innovation? If continuous, the mountain claim strengthens; if innovative, FSM triggers.',
    'If FSM triggers, the constraint reclassifies to tangled_rope: genuine coordination (doctrinal recovery) with asymmetric extraction (Church loses institutional position, secular rulers gain assets). The beneficiary/victim structure would be confirmed as structural, not incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_mountain_vs_constructed_boundary, conceptual, 'False summit mountain detection: natural-law vs. constructed ambiguity for the Reformation event boundary.').

omega_variable(
    committer_kernel_reading_identity,
    'This constraint is one reading (theological_climb_reading) of kernel reformation_event_boundary. How does the sibling readings'' structural delta change the constraint classification?',
    'Compare the three readings'' beneficiary/victim structures and claimed_types. Political_swap_reading makes secular_rulers agenda_setters and believers/papacy victims (snare). Composite_overdetermination_reading dissolves single-driver periodization, likely yielding tangled_rope with multiple beneficiary/victim pairs across causal strands.',
    'Documents the committer-frame structure: kernel_id, reading_id, sibling_ids, and the expected structural delta (climb vs swap vs composite). Routes the kernel contest into the omega layer per Rule 2.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_reading_identity, conceptual, 'Commitment kernel reading identity: theological_climb_reading of reformation_event_boundary.').

omega_variable(
    periodization_tightness_vs_longue_duree,
    'Does the tight periodization (1517-1555) artificially isolate the theological climb from its medieval preconditions (Wycliffe, Hus, conciliarism, devotio moderna) and long-term confessionalization?',
    'Historiographical comparison: if pre-Reformation reform movements share the same doctrinal core, the climb extends backward; if confessionalization (1555-1648) is the true institutional crystallization, the climb extends forward. Either dissolution challenges the 1517-1555 boundary.',
    'If periodization dissolves, the constraint''s interval and measurement grid must expand, altering drift detection and potentially the claimed_type (mountain requires stable referent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_tightness_vs_longue_duree, empirical, 'Whether the 1517-1555 boundary is structurally defensible or an artifact of the reading''s framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_theological_climb_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(reformation_theological_climb_tr_t6, reformation_event_boundary__theological_climb_reading, theater_ratio, 6, 0.03).
narrative_ontology:measurement(reformation_theological_climb_tr_t12, reformation_event_boundary__theological_climb_reading, theater_ratio, 12, 0.04).
narrative_ontology:measurement(reformation_theological_climb_tr_t19, reformation_event_boundary__theological_climb_reading, theater_ratio, 19, 0.06).
narrative_ontology:measurement(reformation_theological_climb_tr_t26, reformation_event_boundary__theological_climb_reading, theater_ratio, 26, 0.07).
narrative_ontology:measurement(reformation_theological_climb_tr_t38, reformation_event_boundary__theological_climb_reading, theater_ratio, 38, 0.08).

% Extraction over time
narrative_ontology:measurement(reformation_theological_climb_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(reformation_theological_climb_be_t6, reformation_event_boundary__theological_climb_reading, base_extractiveness, 6, 0.07).
narrative_ontology:measurement(reformation_theological_climb_be_t12, reformation_event_boundary__theological_climb_reading, base_extractiveness, 12, 0.09).
narrative_ontology:measurement(reformation_theological_climb_be_t19, reformation_event_boundary__theological_climb_reading, base_extractiveness, 19, 0.1).
narrative_ontology:measurement(reformation_theological_climb_be_t26, reformation_event_boundary__theological_climb_reading, base_extractiveness, 26, 0.11).
narrative_ontology:measurement(reformation_theological_climb_be_t38, reformation_event_boundary__theological_climb_reading, base_extractiveness, 38, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(reformation_theological_climb_su_t0, reformation_event_boundary__theological_climb_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(reformation_theological_climb_su_t6, reformation_event_boundary__theological_climb_reading, suppression_requirement, 6, 0.18).
narrative_ontology:measurement(reformation_theological_climb_su_t12, reformation_event_boundary__theological_climb_reading, suppression_requirement, 12, 0.2).
narrative_ontology:measurement(reformation_theological_climb_su_t19, reformation_event_boundary__theological_climb_reading, suppression_requirement, 19, 0.21).
narrative_ontology:measurement(reformation_theological_climb_su_t26, reformation_event_boundary__theological_climb_reading, suppression_requirement, 26, 0.22).
narrative_ontology:measurement(reformation_theological_climb_su_t38, reformation_event_boundary__theological_climb_reading, suppression_requirement, 38, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.08).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the reformation_event_boundary kernel. This reading (theological_climb) claims Mountain with beneficiaries (believers) and victims (Catholic Church). Political_swap_reading claims Snare with secular_rulers as agenda_setters/beneficiaries and papacy/believers as victims. Composite_overdetermination_reading claims Tangled Rope with multiple beneficiary/victim pairs across causal strands. All three share the same historical referent but instantiate different constraints with different ε values, per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__theological_climb_reading, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
