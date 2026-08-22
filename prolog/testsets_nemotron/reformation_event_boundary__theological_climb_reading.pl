% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
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
 *   This constraint story models the Reformation as a theological climb: a
 *   genuine doctrinal breakthrough (justification by faith alone) that
 *   restructured the Christian epistemic field. The reading treats Luther's
 *   rediscovery as an event of scriptural recovery, not political
 *   opportunism. The constraint is the doctrinal claim itself, which
 *   generates a new kernel reading of scripture that forecloses the prior
 *   arrangement. The Catholic Church is the victim of this correction — not
 *   because it was exploited, but because its epistemic authority was
 *   structurally incompatible with the recovered doctrine. Beneficiaries are
 *   believers freed from false doctrine. Periodization is tight: 1517
 *   (Theses) to 1555 (Augsburg Peace), the moment the climb achieved
 *   institutional settlement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.12).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.18).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, mountain).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Reformation as Theological Climb: Justification by Faith Alone").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical_epistemology/religious_history/commitment_system").

domain_priors:emerges_naturally(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '12d3cbd6-c7ce-4f14-85ba-98178bd574a7').
narrative_ontology:cs_kernel_codification('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', fixed_text).
narrative_ontology:cs_authority_grounding('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', lineage).
narrative_ontology:cs_interpretation_layer_present('12d3cbd6-c7ce-4f14-85ba-98178bd574a7').
narrative_ontology:cs_reading_relation('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', reformation_event_boundary__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', foundational, justification_by_fait_alone_is_scriptural_recovery).
narrative_ontology:cs_axiom_status(justification_by_fait_alone_is_scriptural_recovery, holdable).
narrative_ontology:cs_axiom_grounding('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', justification_by_fait_alone_is_scriptural_recovery, deontological).
narrative_ontology:cs_axiom('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', foundational, institutional_separation_required_by_doctrinal_necessity).
narrative_ontology:cs_axiom_status(institutional_separation_required_by_doctrinal_necessity, holdable).
narrative_ontology:cs_axiom_grounding('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', institutional_separation_required_by_doctrinal_necessity, deontological).
narrative_ontology:cs_reference_frame('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', pre_reformation_sacramental_economy).
narrative_ontology:cs_drift_state('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', augsburg_settlement_1555, gap(stable, minor, true)).
narrative_ontology:cs_created_at('12d3cbd6-c7ce-4f14-85ba-98178bd574a7', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_laity).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_as_institutional_epistemic_authority).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, secular_rulers_protestant).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, secular_rulers_catholic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Monk and professor whose scriptural reading triggered the institutional rupture. His exit was foreclosed by his own theological conviction: having seen the doctrine, he could not unsee it or return to the prior arrangement without betraying conscience. The constraint's authority derived from his refusal to recant.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, martin_luther, agenda_setter,
    institutional, generational, identity_locked, continental).

% Theologians and preachers (Melanchthon, Zwingli, Calvin, Bucer) who adopted the doctrinal breakthrough and built churches around it. They gained doctrinal coherence and institutional autonomy but faced imperial condemnation and internal schism. Exit back to Rome was doctrinally impossible; lateral exit to other Protestant camps was constrained by confessional boundaries.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformers, beneficiary,
    organized, generational, constrained, continental).

% Ordinary believers in German territories, Switzerland, the Low Countries, and Scandinavia who gained vernacular scripture, congregational worship, and assurance of justification without sacerdotal mediation. Their exit options were limited by territorial confessionalization (cuius regio, eius religio) — they could migrate but not choose freely.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_laity, beneficiary,
    powerless, biographical, constrained, local).

% The universal class of Christians for whom the gospel of free justification was the operative good. Their 'exit' from the prior arrangement was not spatial but epistemic: once the doctrine was apprehended, the former framework collapsed as a viable option for conscience. They are the constraint's intended beneficiaries in the theological logic of the reading.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine, beneficiary,
    powerless, biographical, identity_locked, universal).

% The papacy, curia, and episcopal hierarchy lost monopoly control over doctrinal definition, sacramental economy, and territorial allegiance in half of Europe. The constraint extracted their epistemic authority, fiscal base, and juridical reach. Their exit was structurally constrained: they could not concede the doctrinal point without dissolving their self-understanding as the Church; they could only counter-reform, repress, or negotiate coexistence.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_as_institutional_epistemic_authority, payer,
    institutional, civilizational, constrained, global).

% Princes and city councils (Saxony, Hesse, Zurich, Geneva) who adopted the Reformation to seize church lands, control appointments, and legitimize sovereignty. They gained assets and autonomy but became bound to defend the new confession militarily and politically. Their exit was arbitrage-grade: they could (and some did) switch confessions for political advantage.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers_protestant, beneficiary,
    powerful, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, secular_rulers_protestant, agenda_setter).

% Emperor Charles V, Bavarian and Austrian dukes, Spanish and Italian rulers who bore the costs of suppression, war, and diplomatic containment. They paid in blood and treasure to maintain the old order but could not extract compliance from Protestant territories after 1555. Exit from the conflict required conceding the very authority they fought to preserve.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, secular_rulers_catholic, payer,
    powerful, biographical, constrained, regional).

% Scholars (Lutheran, Reformed, Catholic, secular) who read the Reformation as a doctrinal event whose primary causality is the recovery of apostolic theology. They see the constraint as a climb: a genuine epistemic breakthrough that restructured the religious field. Their situation is analytical — they do not bear the constraint's costs or collect its rents.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, historians_theological_tradition, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a new epistemic foundation for Christian life: justification by faith alone, mediated through scripture rather than sacramental hierarchy. This solved the coordination problem of assurance — how a sinner knows they stand right with God — by replacing a complex, priest-mediated economy of merit with a direct promise apprehended in hearing.
% TRANSFER_FUNCTION: Transferred epistemic authority from the magisterium (pope, councils, canon law) to scripture as interpreted by the individual conscience and the preaching office. Transferred material resources (church lands, tithes, benefices) from the old church to secular rulers and new Protestant institutions. Transferred the burden of salvation anxiety from the laity (who carried it under the penitential system) to Christ (who bears it in the gospel).
% ABSENT_VOICES: The peasant rebels of 1524-25 (who invoked the gospel against serfdom and were crushed by Lutheran princes), the Anabaptists (who pushed the priesthood of all believers beyond the reformers' settling point and were excluded by both confessions), the Jews (whom Luther vilified when they did not convert), and the global South (where the Reformation arrived as a European export, not a scriptural recovery). These voices were structurally excluded from the constraint's founding settlement.
% DISAPPEARANCE_RATIONALE: If the theological climb reading vanished — if justification by faith alone were not a genuine doctrinal breakthrough but a mere pretext — the Protestant churches would lose their founding legitimacy, the Catholic Counter-Reformation would lose its defining opponent, and the modern map of confessional Europe would dissolve into a purely political rearrangement. The constraint's disappearance would rearrange the world because the doctrinal claim is the load-bearing element in this reading.
% FOUNDING_PROBLEM: The late medieval church had burdened consciences with a salvation economy of works, indulgences, and sacramental mediation that could not deliver the assurance it promised. The theologian's crisis (Luther's Anfechtung) was the subjective index of a systemic failure: the gospel had been obscured by a merit-based religion that made God a judge to be appeased rather than a father to be trusted.
% FOUNDING_PROBLEM_CORROBORATION: Luther's own testimony (Table Talk, letters, the 1545 Preface to the Latin Works) attests the experiential crisis. Catholic historians of the period (Janssen, Pastor) concede the pastoral failures of the indulgence system but deny the doctrinal solution was necessary or scriptural. Secular historians (MacCulloch, Ozment) document the systemic corruption but debate whether it was the primary driver. No corroboration exists outside the benefiting parties that the founding problem *required* institutional schism rather than internal reform — the Council of Trent's reforms demonstrate the Catholic Church's own capacity for self-correction.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
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
 *   Extractiveness is low (0.12) because the constraint's operation in this reading is primarily coordinative: it aligns believers around a recovered truth. The material transfers (church lands, tithes) are secondary effects, not the constraint's function. Suppression is low (0.18) but nonzero: the constraint required active enforcement against the old hierarchy (excommunications, bans, wars) but this enforcement was defensive — protecting the doctrinal space, not extracting from it. Theater ratio is near zero (0.05): the constraint's performative and functional dimensions are aligned; the preaching of the gospel is both the coordination mechanism and the constraint's content. Accessibility collapse is moderate-high (0.62): once the doctrine is apprehended, the prior framework becomes epistemically inaccessible to conscience. Resistance is high (0.71): the constraint met fierce resistance from the institutional church, the empire, and rival reformers — resistance is the hallmark of a genuine climb against an established order.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer's seat, the constraint is a mountain (doctrinal truth that cannot be otherwise). From the Catholic institutional seat, it is a snare (extraction of authority under cover of theology). From the secular ruler's seat, it is a rope or tangled_rope (coordination opportunity with material transfer). The engine computes these divergences from the structural data; the claimed_type 'mountain' reflects the reading's own self-understanding as a doctrinal climb.
 *
 * DIRECTIONALITY LOGIC:
 *   Luther and the reformers are agenda_setters with identity_locked exit: they cannot unsee the doctrine. Protestant laity and believers_freed are beneficiaries with constrained or identity_locked exit: the doctrine restructures their epistemic horizon. The Catholic Church is a victim (institutional epistemic authority extracted) with constrained exit: it cannot concede without self-dissolution. Secular rulers split: Protestant rulers are beneficiaries/agenda_setters with arbitrage exit (political opportunism possible); Catholic rulers are payers with constrained exit. The analytical observer seat sees the full structure without bearing cost or collecting gain.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem (salvation assurance under a merit economy) remains contested — Trent reformed the abuses but retained the sacramental framework the reformers rejected. The mandate has not atrophied: Protestant churches still organize around sola fide, and the Catholic Church still defines itself against it. No mandatrophy resolution: the climb persists as a live epistemic fault line.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    climb_vs_swap_primacy,
    'Is the doctrinal breakthrough the primary causal driver of the Reformation, or is it a theological veneer over a political asset seizure?',
    'Counterfactual analysis: if Luther had never existed, would a similar doctrinal rupture have occurred from within the erasmian-humanist reform current? If the Habsburg-Valois rivalry had not created political space, would the doctrinal movement have been suppressed like Hus''s?',
    'If political_swap is primary, this reading''s claimed_type (mountain) is a false summit — the constraint is actually a tangled_rope or snare where theology coordinates political extraction. If climb is primary, the mountain claim holds and the political reading is a downstream effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climb_vs_swap_primacy, conceptual, 'Causal primacy of theological innovation vs. political realignment').

omega_variable(
    beneficiary_structure_ambiguity,
    'Are the declared beneficiaries (believers_freed) the genuine beneficiaries of the constraint, or are secular rulers the true capture class using theology as cover?',
    'Comparative analysis of resource flows: track church lands, tithes, and benefices transferred to secular rulers vs. measurable improvement in lay spiritual welfare (literacy, catechesis, poor relief) in Protestant vs. Catholic territories 1555-1648.',
    'If secular rulers are the concentrated beneficiaries and lay welfare gains are marginal or delayed, the constraint reclassifies from mountain to tangled_rope (coordination + asymmetric extraction). If lay welfare gains are substantial and precede political consolidation, the mountain claim is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_structure_ambiguity, empirical, 'Whether the constraint''s beneficiary structure matches its theological self-presentation').

omega_variable(
    periodization_boundary,
    'Does the constraint''s natural periodization end at 1555 (Augsburg), or does the climb continue through the confessionalization era (1555-1648) and the Enlightenment?',
    'Historiographical analysis: does the Peace of Augsburg represent the climb''s institutional settlement (cuius regio as the climb''s political form), or is it a premature freeze that the climb later breaks through (Calvinist expansion, Pietism, liberal theology)?',
    'If the climb extends beyond 1555, the interval is too narrow and extractiveness may be understated (later confessional coercion). If 1555 is the true settlement, the low extractiveness holds and the constraint is a genuine mountain with a defined historical terminus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_boundary, conceptual, 'Whether the climb''s historical boundary is 1555 or extends through confessionalization').

omega_variable(
    kernel_reading_identity,
    'Does this reading (theological_climb) foreclose the political_swap_reading, or do they coexist as live positions held by different parties?',
    'Logical analysis of the readings'' core premises: does ''justification by faith alone is a genuine scriptural recovery'' logically entail ''secular rulers did not exploit the dispute for asset seizure''? Or can both be true simultaneously in different causal registers?',
    'If forecloses, the readings are mutually exclusive frameworks — the kernel has no stable composite. If coexists_with, the kernel supports a stable pluralism of readings. The engine computes foreclosure from cs_structure axioms; this omega documents the author''s judgment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Structural relationship between theological_climb and political_swap readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 1517, 1555).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1517, reformation_event_boundary__theological_climb_reading, theater_ratio, 1517, 0.02).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1521, reformation_event_boundary__theological_climb_reading, theater_ratio, 1521, 0.03).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1525, reformation_event_boundary__theological_climb_reading, theater_ratio, 1525, 0.04).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1530, reformation_event_boundary__theological_climb_reading, theater_ratio, 1530, 0.04).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1545, reformation_event_boundary__theological_climb_reading, theater_ratio, 1545, 0.05).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_tr_t1555, reformation_event_boundary__theological_climb_reading, theater_ratio, 1555, 0.05).

% Extraction over time
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1517, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1517, 0.05).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1521, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1521, 0.08).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1525, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1525, 0.1).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1530, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1530, 0.11).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1545, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1545, 0.12).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_be_t1555, reformation_event_boundary__theological_climb_reading, base_extractiveness, 1555, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1517, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1517, 0.05).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1521, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1521, 0.1).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1525, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1525, 0.15).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1530, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1530, 0.16).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1545, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1545, 0.17).
narrative_ontology:measurement(reformation_event_boundary__theological_climb_reading_su_t1555, reformation_event_boundary__theological_climb_reading, suppression_requirement, 1555, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_event_boundary__theological_climb_reading, 0.08).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, tridentine_reform_constraint).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, confessionalization_constraint).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, westphalian_sovereignty_constraint).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories in the reformation_event_boundary family. The theological_climb_reading asserts a mountain-type doctrinal climb (1517-1555). The political_swap_reading asserts a snare-type political asset seizure using theology as cover. The composite_overdetermination_reading asserts a tangled_rope-type irreducibly multi-causal event. They share the kernel_id reformation_event_boundary and are linked via affects_constraints. The ε values differ sharply: climb reading ε≈0.12 (coordinative), swap reading ε≈0.75 (extractive), composite ε≈0.45 (hybrid).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_event_boundary__theological_climb_reading, powerful, 0.2).
constraint_indexing:directionality_override(reformation_event_boundary__theological_climb_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
