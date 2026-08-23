% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__composite_overdetermination_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: reformation_event_boundary__composite_overdetermination_reading
 *   human_readable: Composite Overdetermination Regime in Reformation Historiography
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   Since the mid-twentieth century, Reformation historiography has been
 *   governed by a standing arrangement: adequate explanation requires
 *   simultaneous treatment of doctrinal innovation, institutional collapse,
 *   political realignment, and denominational proliferation, with no single
 *   causal driver permitted to rank first and no single completion date
 *   permitted to close the event. This file instantiates the
 *   composite_overdetermination_reading of the reformation_event_boundary
 *   kernel; the sibling readings (theological_climb_reading,
 *   political_swap_reading) are separate constraint files linked via
 *   network.affects_constraints. The epsilon referent is the standing
 *   historiographical arrangement itself — the multi-causal regime of
 *   seminars, journals, hiring, and canon formation — assessed by this
 *   reading's own lights: the regime is genuinely coordinative (it ended the
 *   confessional-ownership wars over the Reformation's meaning and organizes
 *   a four-stream research program no monocausal school ever sustained) while
 *   extracting real costs (a decade-scale training levy on entrants, devalued
 *   confessional careers, comprehension losses exported to public memory).
 *   Claimed type and metrics are authored independently: the claim states
 *   tangled_rope because both a coordination function and asymmetric
 *   extraction are structurally present and actively enforced; the metrics
 *   state what the regime's operation descriptively shows. Where the engine's
 *   per-seat computations diverge from the claim, that divergence is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - university_history_departments: agenda setter (institutional/constrained) — administers hiring, peer review, and curricula; collects the credential rents
 *   - professional_reformation_historians: primary beneficiary (organized/identity_locked) — careers and self-conception fused with the multi-causal craft
 *   - academic_publishers: secondary beneficiary (organized/mobile) — monetizes the monograph and journal output the program generates
 *   - confessional_tradition_scholars: primary payer (organized/constrained) — church and seminary historians whose framings the gatekeeping devalues
 *   - history_entrants_graduate_students: payer (powerless/trapped) — bears the decade-scale training levy
 *   - public_history_audiences: diffuse payer (powerless/mobile) — inherit myth wherever complexity exceeds reach
 *   - popular_narrative_history_writers: excluded voice (moderate/mobile) — would supply readable synthesis, kept outside legitimacy
 *   - historiographers_of_historiography: analytical observer (analytical/analytical) — tracks how the regime's categories were formed without paying or collecting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.52).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.48).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Composite Overdetermination Regime in Reformation Historiography").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, '8c2c5944-3528-4c4b-8da2-070ba20bc1cd').
narrative_ontology:cs_kernel_codification('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', distributed).
narrative_ontology:cs_authority_grounding('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', expertise).
narrative_ontology:cs_interpretation_layer_present('8c2c5944-3528-4c4b-8da2-070ba20bc1cd').
narrative_ontology:cs_reading_relation('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_axiom('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', foundational, reformation_irreducibly_composite).
narrative_ontology:cs_axiom_status(reformation_irreducibly_composite, holdable).
narrative_ontology:cs_axiom_grounding('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', reformation_irreducibly_composite, empirically_contingent).
narrative_ontology:cs_axiom('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', secondary, no_shared_completion_point).
narrative_ontology:cs_axiom_status(no_shared_completion_point, holdable).
narrative_ontology:cs_axiom_grounding('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', no_shared_completion_point, empirically_contingent).
narrative_ontology:cs_reference_frame('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', parallel_streams_composite_event).
narrative_ontology:cs_drift_state('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', post_quincentenary_global_turn, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8c2c5944-3528-4c4b-8da2-070ba20bc1cd', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, professional_reformation_historians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, university_history_departments).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, academic_publishers).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, confessional_tradition_scholars).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, history_entrants_graduate_students).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, public_history_audiences).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, professional_reformation_historians).
narrative_ontology:constraint_vindicates(reformation_event_boundary__composite_overdetermination_reading, historical_overdetermination_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Staff hiring and tenure committees, edit the field's journals through faculty boards, and set graduate curricula. They admit cohorts of students whose dissertations staff the next generation of seminars, and their rankings depend on the publication volume the multi-stream program generates. Stepping off the paradigm would mean retooling faculty lines and course requirements against entrenched internal interests, so administration continues the existing gatekeeping by default.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, university_history_departments, agenda_setter,
    institutional, generational, constrained, global).

% Built careers on commanding several languages, the theological sources, imperial politics, and print culture at once; their standing rests on demonstrating mastery of all four streams in review essays and monographs. Their scholarly self-conception is bound up with the multi-causal craft — writing a single-driver account would read as a betrayal of their training — so even those chafing at the workload keep defending the standard.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, professional_reformation_historians, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, professional_reformation_historians, payer).

% Run the monograph series and journals through which the field's output circulates; library subscriptions and series sales scale with the volume of multi-stream scholarship. They can redirect catalogs toward other periods if the field contracts, so their commitment is portfolio-deep rather than existential.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, academic_publishers, beneficiary,
    organized, biographical, mobile, global).

% Work from church archives, seminaries, and denominational institutes; their framings begin from confessional identity — Catholic, Lutheran, Reformed, Radical — and the regime's journals and hiring markets treat such starting points as apologetics rather than scholarship. They publish in denominational venues with smaller circulation, and abandoning their confessional standpoint would dissolve the constituency they serve.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, confessional_tradition_scholars, payer,
    organized, generational, constrained, global).

% Face a decade of language acquisition, paleography, doctrinal literacy, and archival apprenticeship before producing acceptable work; mid-program withdrawal forfeits the sunk investment, and non-academic options discount the credential heavily. They supply the labor — teaching assistance, archival processing, index checking — on which the seminar system runs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, history_entrants_graduate_students, payer,
    powerless, biographical, trapped, continental).

% Meet the Reformation through anniversary commemorations, documentaries, school curricula, and museum exhibits. The composite account exceeds what a lecture or article can carry, so inherited myths — the theses nailed to the door, Luther alone against the world — fill the space; their recourse is disengagement from academic accounts altogether, which returns them to myth by another route.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, public_history_audiences, payer,
    powerless, immediate, mobile, global).

% Would compress the transformation into readable single-arc narratives for trade audiences; the review apparatus that certifies academic legitimacy does not recognize their genre, and their sales success is treated as evidence of oversimplification rather than merit. They stand outside the conversation that sets what counts as knowing this event.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, popular_narrative_history_writers, excluded,
    moderate, biographical, mobile, global).

% Study how the regime's categories — confessionalization, composite event, social discipline — were formed, funded, and propagated; they attend its conferences and read its journals without submitting to its gates, and their analyses neither draw its salaries nor pay its tolls.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, historiographers_of_historiography, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, university_history_departments).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the study of a transformation that ran on four fronts at once — doctrine, institutions, territorial politics, and denominational formation — by assigning each stream specialists who share archives, citation conventions, conference circuits, and review standards, so that no stream's findings are lost to the others and no single-stream account closes the question prematurely.
% TRANSFER_FUNCTION: Moves a decade-scale training levy from entrants to the seminar system; moves legitimacy and page-space away from confessional and popular framings toward credentialed multi-stream synthesis; moves status, subscriptions, and ranking capital to departments and presses; and moves public memory toward expert mediation — or, failing that, leaves it to myth.
% ABSENT_VOICES: Popular narrative writers, lay denominational educators, and historians of global Christianity sit outside the seminar-and-peer-review circuit that produced the composite consensus; they would press for teachable arcs, identity-usable pasts, and de-centered geographies. Their absence lets a specialist agreement present itself as the whole conversation.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand a trained workforce with no shared program: journals would re-sort along confessional and national lines, graduate training would lose its four-stream template, departments would re-litigate hiring criteria, and public memory would default entirely to denominational mythmaking — the field's present shape depends on the regime continuing.
% FOUNDING_PROBLEM: Early twentieth-century Reformation historiography was fought as proxy war between confessional monopolies — Protestant and Catholic each claiming the event entire — and between national schools appropriating it for state narratives; meanwhile new social and archival evidence fit none of the single-owner accounts. The composite regime was built to end the ownership wars by making exclusive claims methodologically illegitimate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the joint Catholic-Lutheran commemoration of the 2017 quincentenary — shared penitential statements and papal participation in the Lund liturgy — attests that the confessional-ownership war the regime was built to end has substantially receded; curriculum bodies and public-history surveys attest from the demand side that the comprehension gap the regime leaves behind is real. No party inside the benefiting set attests either half alone.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__composite_overdetermination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__composite_overdetermination_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__composite_overdetermination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits at 0.52 because the regime's costs are real but bounded: a decade-scale training levy on entrants, devalued confessional careers, and comprehension losses pushed onto public memory, weighed against genuine knowledge output. Suppression sits at 0.48 and is structural rather than internalized — gatekeeping operates through review, hiring, and canon formation, not belief indoctrination — and it is authored unscaled, since only extractiveness is scaled by directionality and scope downstream. Theater_ratio at 0.28 reflects a mature paradigm whose obligatory gestures (the ritual acknowledgment that the picture is complex, citation of the canonical syntheses) increasingly accompany rather than produce findings. Accessibility_collapse at 0.42: monocausal and popular accounts persist in textbooks, pulpits, and trade shelves, so alternatives are devalued, not erased. Resistance at 0.45: confessional institutes, global-Christianity scholarship, and trade history mount standing objections. All three tracked series share one time grid (1950-2026, seven points) so no metric row borrows another's endpoints; extractiveness crests around 2001 and eases slightly as digital venues and the global turn widen what counts, theater plateaus at paradigm maturity, and the enforcement requirement falls from its 1989 professionalization peak. The composite reading further holds that the four sub-processes run as parallel patterns rather than sequence, so the regime's classification deliberately inherits a superposition: whichever stream a seat is positioned against determines which face of the arrangement that seat meets.
 *
 * PERSPECTIVAL GAP:
 *   The departmental seat experiences the arrangement as quality control its own members would volunteer for; the entrant seat experiences it as a toll booth with uncertain redemption; the confessional seat experiences it as marginalization wearing methodological neutrality; the publisher seat experiences it as a stable product line. Same structure, four incompatible phenomenologies. The engine computes per-seat types from power, exit, and directionality; the spread between rigorous standard and extractive gate is the datum, not an inconsistency to be edited away.
 *
 * DIRECTIONALITY LOGIC:
 *   Departments derive near the beneficiary end: they set the rules and collect the rents. Senior historians derive low d as declared beneficiaries, but their identity_locked exit cuts both ways — locked beneficiaries cannot flee if the regime sours, which is why the latent-drift question matters. Publishers, with arbitrage-grade mobility, sit nearest the subsidy end of the beneficiary side. Confessional scholars derive high d: declared victims, organized but venue-constrained. Entrants derive the highest d: victims with trapped exit and no coalition mechanism yet — the classic profile in which coalition power is the untested remedy. Public audiences are declared victims, but their exit is trivial disengagement, which damps their effective extraction well below what the raw victim declaration implies; their cost is paid in myth, not money.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — ending confessional and national ownership wars over the Reformation's meaning — has largely receded, as the joint 2017 commemoration attests from outside the benefiting parties, yet the regime's extraction has not fallen commensurately. That mismatch keeps the genealogy signal at contested rather than dead: identity-driven history is resurging globally, so the anti-monopoly function may be needed again. Reading the arrangement as pure coordination would erase the documented gatekeeping levy; reading it as pure extraction would erase the four-stream research program; the tangled reading holds both facts. The forward risk is inertial drift: if theater keeps rising while stream-specific findings plateau, the regime will be maintained by ritual complexity rather than function — theater_ratio is the early-warning line, and the completion-point omega marks where a successor arrangement would have to anchor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of the reformation_event_boundary kernel — the composite_overdetermination_reading; what would the sibling readings (theological_climb_reading, political_swap_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative classification of the sibling files: each reading authors its own epsilon, beneficiary/victim sets, and periodization anchor over the same kernel; the disagreement localizes to (a) whether causal drivers are ranked at all and (b) which terminal date closes the event.',
    'Adopting a sibling reading collapses the four parallel streams into one ranked driver, changing the victim set (climb: indulgence-payers and doctrinal dissenters; swap: peasants of 1525 and dispossessed religious), the completion point, and likely the computed type of the historiographical regime.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame record: kernel, this reading, siblings, and the locus of the dispute.').

omega_variable(
    completion_point_divergence,
    'Which terminal date closes the Reformation — 1555 (Augsburg settlement), 1577 (Formula of Concord), 1648 (Westphalia), or none — and does any single close survive the composite frame?',
    'Track each stream''s own completion markers separately (doctrinal codification, asset settlement, confessional consolidation) and test whether a shared boundary survives aggregation.',
    'If no shared boundary exists, periodization contest is a structural feature of the composite reading rather than an error awaiting correction; if one exists, the composite frame overstates irreducibility and the regime''s warrant narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(completion_point_divergence, conceptual, 'Periodization ambiguity inherent to composite framing.').

omega_variable(
    sub_event_victim_set_variance,
    'Which population bears the arrangement''s costs depends on which sub-event is foregrounded — peasants in the 1525 war (political stream), dispossessed religious under dissolution (institutional stream), Anabaptist martyrs (proliferation stream), indulgence-payers (theological stream); which victim set governs classification?',
    'Author per-stream sub-stories with their own victim declarations and compare per-seat classifications; aggregate only where victim sets overlap.',
    'Per-seat effective extraction varies materially by foregrounded stream; a single aggregated victim set would misstate directionality for every seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sub_event_victim_set_variance, empirical, 'Victim-set variance across foregrounded sub-events.').

omega_variable(
    complexity_incentive_confound,
    'Is the overdetermination thesis held because the evidence forces it, or because the profession''s incentive structure rewards complexity and penalizes readable synthesis?',
    'Compare fields of comparable archival richness where monocausal accounts prevailed; test whether composite regimes correlate with professionalization intensity independently of evidence quality.',
    'If incentive-driven, the regime''s measured extraction understates reality and the arrangement drifts toward gatekeeping that suppresses exits; if evidence-forced, the current epsilon stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(complexity_incentive_confound, empirical, 'Whether the composite consensus tracks the evidence or disciplinary self-interest.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1950, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(reformation_composite_od_tr_t1950, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t1950, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t1963, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1963, 0.15).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t1963, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t1976, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1976, 0.19).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t1976, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t1989, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1989, 0.23).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t1989, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t2001, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2001, 0.26).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t2001, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t2014, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2014, 0.28).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t2014, observed).
narrative_ontology:measurement(reformation_composite_od_tr_t2026, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(reformation_composite_od_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(reformation_composite_od_be_t1950, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement_basis(reformation_composite_od_be_t1950, observed).
narrative_ontology:measurement(reformation_composite_od_be_t1963, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1963, 0.36).
narrative_ontology:measurement_basis(reformation_composite_od_be_t1963, observed).
narrative_ontology:measurement(reformation_composite_od_be_t1976, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1976, 0.44).
narrative_ontology:measurement_basis(reformation_composite_od_be_t1976, observed).
narrative_ontology:measurement(reformation_composite_od_be_t1989, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement_basis(reformation_composite_od_be_t1989, observed).
narrative_ontology:measurement(reformation_composite_od_be_t2001, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2001, 0.55).
narrative_ontology:measurement_basis(reformation_composite_od_be_t2001, observed).
narrative_ontology:measurement(reformation_composite_od_be_t2014, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2014, 0.54).
narrative_ontology:measurement_basis(reformation_composite_od_be_t2014, observed).
narrative_ontology:measurement(reformation_composite_od_be_t2026, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(reformation_composite_od_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(reformation_composite_od_su_t1950, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement_basis(reformation_composite_od_su_t1950, observed).
narrative_ontology:measurement(reformation_composite_od_su_t1963, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1963, 0.4).
narrative_ontology:measurement_basis(reformation_composite_od_su_t1963, observed).
narrative_ontology:measurement(reformation_composite_od_su_t1976, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1976, 0.5).
narrative_ontology:measurement_basis(reformation_composite_od_su_t1976, observed).
narrative_ontology:measurement(reformation_composite_od_su_t1989, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1989, 0.56).
narrative_ontology:measurement_basis(reformation_composite_od_su_t1989, observed).
narrative_ontology:measurement(reformation_composite_od_su_t2001, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2001, 0.54).
narrative_ontology:measurement_basis(reformation_composite_od_su_t2001, observed).
narrative_ontology:measurement(reformation_composite_od_su_t2014, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2014, 0.51).
narrative_ontology:measurement_basis(reformation_composite_od_su_t2014, observed).
narrative_ontology:measurement(reformation_composite_od_su_t2026, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2026, 0.48).
narrative_ontology:measurement_basis(reformation_composite_od_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, political_swap_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into at least three structurally distinct historiographical constraints — one per reading of the reformation_event_boundary kernel. Each reading authors epsilon over a different referent arrangement (the regime its own frame licenses), with different victim sets and different completion anchors; forcing one story to span all three would make epsilon observer-dependent. This file carries the composite reading's epsilon; the climb and swap files carry theirs; affects_constraints links the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
