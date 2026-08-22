% ============================================================================
% CONSTRAINT STORY: reformation_composite__theological_fragmentation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_composite__theological_fragmentation_reading, []).

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
 *   constraint_id: reformation_composite__theological_fragmentation_reading
 *   human_readable: Theological Fragmentation as Constraint on Reformation Outcomes
 *   domain: historical_epistemology/religious_history/political_economy
 *
 * SUMMARY:
 *   The theological fragmentation reading treats the Reformation's
 *   confessional divisions as a constraint system: competing soteriological
 *   commitments (justification by faith alone vs. covenant election vs.
 *   believers' baptism) and ecclesiological commitments (visible church vs.
 *   gathered church vs. papal church) generate structurally incompatible
 *   denominations. Each denomination requires boundary enforcement
 *   (confessional subscription, discipline, exclusion) to maintain its
 *   coherence. The coordination function is real — each system provides
 *   salvation assurance and communal identity — but the extraction is
 *   asymmetric: denominational leadership and confessional theologians
 *   benefit from the boundaries they police, while boundary-crossers and
 *   suppressed radicals bear the costs. This reading instantiates the kernel
 *   'reformation_composite' as a kernel-reading story; its sibling readings
 *   are political_realignment_reading and technological_mediation_reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_composite__theological_fragmentation_reading, 0.35).
domain_priors:suppression_score(reformation_composite__theological_fragmentation_reading, 0.45).
domain_priors:theater_ratio(reformation_composite__theological_fragmentation_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(reformation_composite__theological_fragmentation_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_composite__theological_fragmentation_reading, tangled_rope).
narrative_ontology:human_readable(reformation_composite__theological_fragmentation_reading, "Theological Fragmentation as Constraint on Reformation Outcomes").
narrative_ontology:topic_domain(reformation_composite__theological_fragmentation_reading, "historical_epistemology/religious_history/political_economy").

domain_priors:requires_active_enforcement(reformation_composite__theological_fragmentation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_composite__theological_fragmentation_reading, '1344bfea-6139-48f5-b48d-1f03cedf44a7').
narrative_ontology:cs_kernel_codification('1344bfea-6139-48f5-b48d-1f03cedf44a7', fixed_text).
narrative_ontology:cs_authority_grounding('1344bfea-6139-48f5-b48d-1f03cedf44a7', lineage).
narrative_ontology:cs_interpretation_layer_present('1344bfea-6139-48f5-b48d-1f03cedf44a7').
narrative_ontology:cs_reading_relation('1344bfea-6139-48f5-b48d-1f03cedf44a7', reformation_composite__political_realignment_reading, coexists_with).
narrative_ontology:cs_reading_relation('1344bfea-6139-48f5-b48d-1f03cedf44a7', reformation_composite__technological_mediation_reading, coexists_with).
narrative_ontology:cs_axiom('1344bfea-6139-48f5-b48d-1f03cedf44a7', foundational, confessional_boundary_necessary_for_salvation_assurance).
narrative_ontology:cs_axiom_status(confessional_boundary_necessary_for_salvation_assurance, holdable).
narrative_ontology:cs_axiom_grounding('1344bfea-6139-48f5-b48d-1f03cedf44a7', confessional_boundary_necessary_for_salvation_assurance, deontological).
narrative_ontology:cs_axiom('1344bfea-6139-48f5-b48d-1f03cedf44a7', foundational, doctrinal_pluralism_ontologically_real_not_merely_political).
narrative_ontology:cs_axiom_status(doctrinal_pluralism_ontologically_real_not_merely_political, holdable).
narrative_ontology:cs_axiom_grounding('1344bfea-6139-48f5-b48d-1f03cedf44a7', doctrinal_pluralism_ontologically_real_not_merely_political, deontological).
narrative_ontology:cs_reference_frame('1344bfea-6139-48f5-b48d-1f03cedf44a7', post_apostolic_church_unity).
narrative_ontology:cs_drift_state('1344bfea-6139-48f5-b48d-1f03cedf44a7', westphalian_confessional_settlement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1344bfea-6139-48f5-b48d-1f03cedf44a7', '').
narrative_ontology:cs_kernel_id(reformation_composite__theological_fragmentation_reading, reformation_composite).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, denominational_leadership).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, confessional_theologians).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, print_entrepreneurs_specialized).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, lay_adherents_boundary_crossers).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, interconfessional_merchants).
narrative_ontology:constraint_victim(reformation_composite__theological_fragmentation_reading, radical_reformers_suppressed).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(reformation_composite__theological_fragmentation_reading, imperial_estates_princes).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_fide_doctrine).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, sola_scriptura_principle).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, priesthood_of_all_believers).
narrative_ontology:constraint_vindicates(reformation_composite__theological_fragmentation_reading, confessional_subscription_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Formulate and enforce confessional boundaries through catechisms, synods, and disciplinary structures. Benefit from confessional loyalty that secures tithes, land, and political recognition. Exit means abandoning the institutional identity they embody; their authority is constituted by the boundary they police.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, denominational_leadership, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, denominational_leadership, beneficiary).

% Produce the doctrinal architecture that makes each denomination a coherent system. Their professional standing, patronage, and intellectual legacy depend on the fragmentation persisting — a unified church would render their distinctive systematic contributions redundant. Exit would mean repudiating their life's work.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, confessional_theologians, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, confessional_theologians, agenda_setter).

% Specialize in confessional-specific imprints — Lutheran catechisms, Reformed commentaries, Anabaptist martyrologies. Fragmentation creates protected niche markets; unification would collapse their product differentiation. Can pivot to other print lines but lose the confessional captive audience.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, print_entrepreneurs_specialized, beneficiary,
    moderate, biographical, constrained, regional).

% Caught between confessional boundaries in mixed marriages, border regions, or trade networks. Bear the cost of duplicate baptisms, excluded communion, inheritance disputes, and social ostracism. Exit requires conversion that severs kinship and community ties — identity-locked by relational embeddedness.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, lay_adherents_boundary_crossers, payer,
    powerless, biographical, identity_locked, local).

% Operate across confessional lines in the Holy Roman Empire, Low Countries, and Swiss Confederacy. Pay transaction costs: separate guild memberships, confessional oaths for market access, smuggling banned texts, bribes to cross boundaries. Can relocate but lose established networks and capital.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, interconfessional_merchants, payer,
    moderate, biographical, constrained, continental).

% Spiritualists, anti-Trinitarians, and communal Anabaptists whose theology refuses confessional codification. Suppressed by all major confessions (Lutheran, Reformed, Catholic) as threats to the confessional order. Exit means recantation or execution; no recognized confessional home exists for them.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, radical_reformers_suppressed, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, radical_reformers_suppressed, excluded).

% Use cuius regio eius religio to fix territorial confession, gaining church lands and jurisdictional independence from Rome. Benefit from the fragmentation they help cement, but can switch confessions for political advantage (e.g., Calvinist Palatinate, Lutheran Saxony) — mobile at the institutional level.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, imperial_estates_princes, agenda_setter,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(reformation_composite__theological_fragmentation_reading, imperial_estates_princes, beneficiary).

% Track the fragmentation as a threat to universal jurisdiction. Author the Counter-Reformation response (Trent, Jesuits, Index) but do not bear the constraint's costs or collect its benefits within the fragmented space — analytical seat outside the coordination/extraction structure.
narrative_ontology:constraint_stakeholder(reformation_composite__theological_fragmentation_reading, papal_curial_observers, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides each confessional community a complete, self-authenticating doctrinal system: salvation assurance, communal discipline, and political recognition without requiring a single visible church. Solves the coordination problem of 'how do we know we are the true church?' for multiple groups simultaneously.
% TRANSFER_FUNCTION: Moves lay loyalty, tithes, and political allegiance from a putatively universal church to particular confessional establishments; moves interpretive authority from magisterium to confessional text; moves boundary-enforcement costs onto boundary-crossers and dissenters.
% ABSENT_VOICES: The radical reformers (Spiritualists, anti-Trinitarians, communal Anabaptists) who refused confessional codification entirely — they would object that the constraint makes their theology impossible, but they were executed, exiled, or driven underground by all confessional establishments. Also absent: the pre-Reformation 'common Christian' who experienced the church as a single sacramental system — their world was destroyed, not consulted.
% DISAPPEARANCE_RATIONALE: If confessional boundaries dissolved overnight, the institutional churches would lose their distinctive property claims, disciplinary monopolies, and political recognition treaties; theologians would lose their confessional chairs; print entrepreneurs would lose protected markets; boundary-crossers would lose their duplicate costs but also their communal anchors; the entire Westphalian confessional settlement would unwind.
% FOUNDING_PROBLEM: How to secure salvation assurance and communal coherence when the universal church's sacramental mediation is rejected? Each confessional answer (Lutheran justification, Reformed covenant, Anabaptist believers' church) requires a boundary to function — the fragmentation is the solution's necessary form.
% FOUNDING_PROBLEM_CORROBORATION: Confessional historians (Lutheran, Reformed, Catholic) attest the problem remains live for their traditions. Secular historians (e.g., Brad Gregory, Carlos Eire) attest the problem is historically superseded — modern pluralism makes the salvation-assurance question unintelligible in its original terms. No consensus across the beneficiary/non-beneficiary divide.
narrative_ontology:disappearance_verdict(reformation_composite__theological_fragmentation_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_composite__theological_fragmentation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_composite__theological_fragmentation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(reformation_composite__theological_fragmentation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_composite__theological_fragmentation_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_composite__theological_fragmentation_reading_tests).
:- end_tests(reformation_composite__theological_fragmentation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) is moderate: the confessional system does deliver coordination (salvation assurance, communal discipline) but extracts via boundary enforcement costs pushed onto non-beneficiaries. Suppression (0.45) reflects active enforcement: censorship, exile, execution of radicals, cuius regio territorial fixing. Theater (0.25) rises over time as confessional orthodoxy becomes more performative (subscription formulas, ritualized controversy) relative to the original theological urgency. Accessibility collapse (0.6) is high because once you accept a confessional system, alternatives appear as damnation or chaos — but lower than a mountain because confessional switching does occur. Resistance (0.55) is significant: Peasants' War, radical Reformation, crypto-confessionalism, and eventually Enlightenment critique all resist the confessional order.
 *
 * PERSPECTIVAL GAP:
 *   From the confessional insider seat, the fragmentation is a mountain — the true church cannot compromise on salvation truth. From the boundary-crosser seat, it is a snare — boundaries extract costs without their consent. From the prince's seat, it is a rope — cuius regio coordinates territorial religion. From the radical reformer's seat, it is a piton — the confessional machinery persists theatrically after the Spirit has departed. The engine computes these per-seat types from the structural data; this reading's claimed_type (tangled_rope) reflects the system-level structural hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Denominational leadership and confessional theologians are identity-locked beneficiaries: their authority and identity are constituted by the boundaries they enforce (d ~ 0.15). Print entrepreneurs are constrained beneficiaries: they profit from fragmentation but could pivot (d ~ 0.3). Lay boundary-crossers and radical reformers are identity-locked or trapped payers: exit severs their relational world or costs their lives (d ~ 0.85-0.95). Merchants are constrained payers: mobile enough to relocate but pay high transaction costs (d ~ 0.6). Princes are mobile agenda-setters: they use the constraint for sovereignty but can switch confessions (d ~ 0.4). Papal observers are analytical (d ~ 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (salvation assurance without papal mediation) remains live for confessional traditions but is contested as historically superseded by secular pluralism. The mandate has not atrophied into pure inertia (not a piton) because confessional boundaries still allocate real resources (church property, school systems, marriage law in some jurisdictions). But the extraction-to-coordination ratio has shifted: early Reformation urgency has given way to confessional maintenance, raising theater. The constraint is not 'resolved mandatrophy' — the coordination function persists for beneficiaries while extraction persists for payers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_primacy,
    'Is the confessional boundary structurally generated by theological incompatibility, or is theological incompatibility the discourse through which political sovereignty asserts itself?',
    'Counterfactual analysis: if theological consensus had been reached at Marburg (1529) or Regensburg (1541), would political fragmentation have persisted anyway? Comparative study of confessionalization in territories with vs. without sovereign prince involvement.',
    'If political primacy holds, the constraint''s extraction is misattributed — the real agenda_setter is the prince, not the theologian; the theological_fragmentation_reading would be a cover story for the political_realignment_reading. If theological primacy holds, the coordination function is genuine and the political reading is downstream.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_primacy, conceptual, 'Whether theology or politics is the constraint''s generative structure.').

omega_variable(
    print_technology_necessity,
    'Could confessional fragmentation have achieved continental scale and persistence without the printing press, or is the technology a necessary condition for the constraint''s scope?',
    'Compare fragmentation patterns in high-print vs. low-print regions (e.g., German lands vs. Scandinavia vs. Eastern Europe). Model counterfactual diffusion speeds for confessional documents without movable type.',
    'If print is necessary, the technological_mediation_reading identifies a structural precondition without which this constraint would be local/episodic. If not necessary, print accelerates but does not generate the constraint — the theological reading stands on its own coordination logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(print_technology_necessity, empirical, 'Whether printing press is necessary condition or accelerator for the constraint''s scope.').

omega_variable(
    salvation_assurance_coherence,
    'Does each confessional system genuinely deliver the salvation assurance it promises to its adherents, or is the assurance itself a constructed effect of the boundary enforcement?',
    'Longitudinal study of adherent anxiety across confessional boundaries; comparative analysis of conversion narratives (do converts report resolved assurance or new anxiety?). Theological analysis of whether the assurance logic is internally coherent or circular.',
    'If assurance is constructed by the boundary, the coordination function is illusory — the constraint is snare, not tangled_rope. If assurance is genuine, the coordination function is real and the extraction is the price of a real good.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(salvation_assurance_coherence, conceptual, 'Whether the coordination function (salvation assurance) is real or boundary-constructed.').

omega_variable(
    kernel_reading_boundary,
    'Does this reading foreclose the political_realignment_reading or technological_mediation_reading, or do all three coexist as structurally independent causal layers?',
    'Formal causal modeling: test whether the theological fragmentation variable retains explanatory power when political and technological variables are controlled. Historiographical meta-analysis: do scholars who hold one reading necessarily reject the others, or do they layer them?',
    'If forecloses, the kernel has genuine logical exclusivity — the readings are mutually exclusive frameworks. If coexists_with, the kernel is a multi-causal composite and each reading captures a real structural layer. This determines the reading_relations declarations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Structural relationship between this reading and its sibling readings of the reformation_composite kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_composite__theological_fragmentation_reading, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t1517, reformation_composite__theological_fragmentation_reading, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(refo_tr_t1525, reformation_composite__theological_fragmentation_reading, theater_ratio, 1525, 0.12).
narrative_ontology:measurement(refo_tr_t1530, reformation_composite__theological_fragmentation_reading, theater_ratio, 1530, 0.15).
narrative_ontology:measurement(refo_tr_t1545, reformation_composite__theological_fragmentation_reading, theater_ratio, 1545, 0.18).
narrative_ontology:measurement(refo_tr_t1555, reformation_composite__theological_fragmentation_reading, theater_ratio, 1555, 0.2).
narrative_ontology:measurement(refo_tr_t1580, reformation_composite__theological_fragmentation_reading, theater_ratio, 1580, 0.22).
narrative_ontology:measurement(refo_tr_t1618, reformation_composite__theological_fragmentation_reading, theater_ratio, 1618, 0.24).
narrative_ontology:measurement(refo_tr_t1648, reformation_composite__theological_fragmentation_reading, theater_ratio, 1648, 0.25).

% Extraction over time
narrative_ontology:measurement(refo_be_t1517, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(refo_be_t1525, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1525, 0.22).
narrative_ontology:measurement(refo_be_t1530, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1530, 0.28).
narrative_ontology:measurement(refo_be_t1545, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1545, 0.31).
narrative_ontology:measurement(refo_be_t1555, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1555, 0.33).
narrative_ontology:measurement(refo_be_t1580, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1580, 0.34).
narrative_ontology:measurement(refo_be_t1618, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1618, 0.35).
narrative_ontology:measurement(refo_be_t1648, reformation_composite__theological_fragmentation_reading, base_extractiveness, 1648, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t1517, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1517, 0.2).
narrative_ontology:measurement(refo_su_t1525, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1525, 0.3).
narrative_ontology:measurement(refo_su_t1530, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1530, 0.38).
narrative_ontology:measurement(refo_su_t1545, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1545, 0.42).
narrative_ontology:measurement(refo_su_t1555, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1555, 0.43).
narrative_ontology:measurement(refo_su_t1580, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1580, 0.44).
narrative_ontology:measurement(refo_su_t1618, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1618, 0.45).
narrative_ontology:measurement(refo_su_t1648, reformation_composite__theological_fragmentation_reading, suppression_requirement, 1648, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_composite__theological_fragmentation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(reformation_composite__theological_fragmentation_reading, 0.08).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__political_realignment_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, reformation_composite__technological_mediation_reading).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, westphalian_settlement).
narrative_ontology:affects_constraint(reformation_composite__theological_fragmentation_reading, confessionalization_state_formation).

% DUAL FORMULATION NOTE:
% This reading (theological_fragmentation) and its siblings form the reformation_composite constraint family. Theological reading: ε=0.35, tangled_rope (coordination + asymmetric extraction). Political reading: expected ε higher, snare-flavored (sovereignty extraction). Technological reading: expected ε lower, rope-flavored (coordination infrastructure). All three share the 1517-1648 interval but author different beneficiary/victim structures and different claimed_types. Linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, institutional, 0.15).
constraint_indexing:directionality_override(reformation_composite__theological_fragmentation_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
