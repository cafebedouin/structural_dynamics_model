% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__composite_overdetermination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Composite Overdetermination Norm for Reformation Classification
 *   domain: historical_epistemology/religious_history/commitment_system_analysis
 *
 * SUMMARY:
 *   The standing arrangement under contest is the historical profession's
 *   treatment of 'the Reformation' as an irreducibly composite,
 *   multiply-caused event — enforced through journal review baselines, survey
 *   curricula, handbook series, and commemorative scripting. The arrangement
 *   solves a real coordination problem (it ended a century of confessional
 *   historiographic warfare by giving rival communities a shared object)
 *   while extracting from single-driver research programs, whose claims are
 *   systematically recast as 'factors' within a frame they may not challenge
 *   as wholes. Constraint family note (epsilon-invariance decomposition): the
 *   colloquial label 'the Reformation' covers structurally distinct claims —
 *   what drove it, who gained, when it completed — and this corpus
 *   instantiates them as separate reading-stories of the kernel
 *   reformation_event_boundary. This file authors ONLY the
 *   composite-overdetermination reading: its epsilon (0.52) is authored for
 *   the composite-treatment norm as this reading sees it, over the fixed
 *   referent of the standing arrangement; the theological-climb and
 *   political-swap readings are separate files with their own epsilon, victim
 *   sets, and classifications. KEY AGENTS (by structural relationship): -
 *   composite_framework_historians: Agenda-setting beneficiary
 *   (institutional/identity_locked) — administers the frame and collects its
 *   authority - survey_textbook_publishers: Beneficiary
 *   (institutional/arbitrage) — commercializes the standardized account,
 *   commercially mobile - denominational_commemoration_bodies: Incidental
 *   beneficiary (organized/mobile) — consumes the shared non-polemical script
 *   episodically - theological_primacy_scholars: Payer
 *   (organized/constrained) — doctrinal-primacy program demoted to 'factor' -
 *   political_primacy_scholars: Payer (moderate/constrained) —
 *   political-primacy program demoted to 'factor' -
 *   congregational_memory_keepers: Excluded voice (powerless/trapped) — local
 *   memory boundaries overridden without consultation -
 *   philosophy_of_history_analysts: Analytical observer — sees the full
 *   periodization structure without a stake
 *
 * KEY AGENTS:
 *   - composite_framework_historians: agenda_setter + beneficiary (institutional/identity_locked) — runs the journals, seminars, and review baselines; professional identity fused with the multi-causal method
 *   - survey_textbook_publishers: beneficiary (institutional/arbitrage) — sells the standardized account; commercial exit always open
 *   - denominational_commemoration_bodies: beneficiary (organized/mobile) — uses the shared script for anniversaries and heritage programming
 *   - theological_primacy_scholars: payer (organized/constrained) — doctrinal-primacy theses received as one factor among several
 *   - political_primacy_scholars: payer (moderate/constrained) — princely-interest theses received as one factor among several
 *   - congregational_memory_keepers: excluded (powerless/trapped) — parish-level event boundaries overridden by curricular periodization
 *   - philosophy_of_history_analysts: observer (analytical/analytical) — studies the periodization machinery from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__composite_overdetermination_reading, 0.52).
domain_priors:suppression_score(reformation_event_boundary__composite_overdetermination_reading, 0.42).
domain_priors:theater_ratio(reformation_event_boundary__composite_overdetermination_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(reformation_event_boundary__composite_overdetermination_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__composite_overdetermination_reading, tangled_rope).
narrative_ontology:human_readable(reformation_event_boundary__composite_overdetermination_reading, "Composite Overdetermination Norm for Reformation Classification").
narrative_ontology:topic_domain(reformation_event_boundary__composite_overdetermination_reading, "historical_epistemology/religious_history/commitment_system_analysis").

domain_priors:requires_active_enforcement(reformation_event_boundary__composite_overdetermination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__composite_overdetermination_reading, 'd2365f0d-24e1-4f66-939f-e3ce7881c3fb').
narrative_ontology:cs_kernel_codification('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', distributed).
narrative_ontology:cs_authority_grounding('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', expertise).
narrative_ontology:cs_interpretation_layer_present('d2365f0d-24e1-4f66-939f-e3ce7881c3fb').
narrative_ontology:cs_reading_relation('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', reformation_event_boundary__theological_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_axiom('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', foundational, no_single_driver_capture_sufficient).
narrative_ontology:cs_axiom_status(no_single_driver_capture_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', no_single_driver_capture_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', foundational, simultaneous_strand_irreducibility).
narrative_ontology:cs_axiom_status(simultaneous_strand_irreducibility, holdable).
narrative_ontology:cs_axiom_grounding('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', simultaneous_strand_irreducibility, empirically_contingent).
narrative_ontology:cs_reference_frame('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', irreducible_composite_eventhood).
narrative_ontology:cs_drift_state('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', contemporary_global_history_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('d2365f0d-24e1-4f66-939f-e3ce7881c3fb', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, composite_framework_historians).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, survey_textbook_publishers).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__composite_overdetermination_reading, denominational_commemoration_bodies).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, theological_primacy_scholars).
narrative_ontology:constraint_victim(reformation_event_boundary__composite_overdetermination_reading, political_primacy_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the syntheses, edit the flagship journals, and chair the seminars through which 'the Reformation' is defined as a multiply-caused whole. They set the review baselines that ask every submission to situate its claim among theological, institutional, political, and social strands. Their careers, citation networks, and curricular authority are bound to the frame's continuation; senior members' professional identities formed inside it, and stepping outside would mean disavowing their life's methodological work.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, composite_framework_historians, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__composite_overdetermination_reading, composite_framework_historians, beneficiary).

% Commission, produce, and sell the survey texts and course packages in which the composite account is standardized. A stable canonical frame lowers their editorial risk and lets editions roll forward with modest revision; because they sell across many markets, they can shift emphasis quickly if demand moves, and their attachment to any particular framing is commercial rather than scholarly.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, survey_textbook_publishers, beneficiary,
    institutional, biographical, arbitrage, global).

% Plan anniversaries, exhibitions, and heritage programming — quincentenaries, museum seasons, civic festivals — using a shared non-polemical narrative that lets Lutheran, Reformed, Catholic, and municipal bodies participate in the same events. They draw legitimacy and audiences from a frame no participant's confession condemns; their dependence is episodic, tied to commemorative cycles rather than daily practice.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, denominational_commemoration_bodies, beneficiary,
    organized, generational, mobile, continental).

% Argue that doctrinal development — above all justification by faith — was the generative center from which the rest followed. Under review norms that require multi-causal situating, their theses are received as 'the theological factor' rather than as candidate explanations of the whole; they publish in confessional and specialist venues while the central journals and survey courses assign them supporting roles.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, theological_primacy_scholars, payer,
    organized, biographical, constrained, continental).

% Argue that princely interest, church-asset seizure, and sovereignty-building drove the break with Rome, with doctrine as its banner. Their proposals meet the same review baseline and are recast as 'the political factor'; some find niches in national-history venues where the political register is native, but the general account they must answer to is not theirs.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, political_primacy_scholars, payer,
    moderate, biographical, constrained, continental).

% Keep parish anniversaries, founder legends, and local dates through which congregations mark their own origins — boundaries that rarely coincide with academic periodization. When curricula and commemorations adopt the scholarly frame, local dates are quietly overridden; these custodians are rarely consulted and cannot leave the memories they keep.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, congregational_memory_keepers, excluded,
    powerless, generational, trapped, local).

% Examine how periodization schemes are built, defended, and abandoned. They take no side among the competing accounts; they observe which completion points the profession tracks, how the composite baseline disciplines submissions, and what it would take for any single-driver account to reopen the question.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__composite_overdetermination_reading, philosophy_of_history_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__composite_overdetermination_reading, composite_framework_historians).
narrative_ontology:fixing_cost_class(reformation_event_boundary__composite_overdetermination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives the historical profession a single shared object — 'the Reformation' — that confessional, national, and disciplinary subfields can study comparatively without first settling confessional disputes; standardizes periodization vocabulary (1517, 1555, 1648 markers) so that teaching, citation, and commemoration can proceed across communities that remember the period differently.
% TRANSFER_FUNCTION: Moves interpretive authority and explanatory primacy away from single-driver accounts (theological or political) toward multi-causal synthesis traditions; concretely, moves journal space, curricular hours, citation share, and commemorative framing toward composite-framework scholars and the publishing infrastructure that standardizes their account.
% ABSENT_VOICES: Congregational memory-keepers and lay custodians of Reformation memory sit outside the conversation entirely: the periodization they live by is overridden in curricula and commemorations without consultation. Lay readers seeking a usable past are represented only through the survey industry's mediation. Neither group would recognize the profession's event boundaries as their own.
% DISAPPEARANCE_RATIONALE: If the composite norm vanished overnight, the field would reorganize around confessional and national grand narratives or fragment into disconnected micro-histories: survey courses, handbook series, and journal review baselines would need rebuilding, commemoration bodies would lose the shared script that lets rival confessions co-sponsor events, and primacy claims currently held at bay would return as the organizing questions of the subfield.
% FOUNDING_PROBLEM: Late nineteenth- and early twentieth-century historiography was polarized into confessional grand narratives — Protestant triumphalist and Catholic polemical accounts — that made 'the Reformation' unteachable as a shared object and blocked comparative research across the confessional divide. The composite frame was built to end that war by making the event everyone's.
% FOUNDING_PROBLEM_CORROBORATION: Methodological critics in philosophy of history and periodization studies, sitting outside the benefiting parties, corroborate both halves: the original confessional-polemics problem was real and severe, and the frame did suppress it. Corroboration for the claim that the problem REMAINS live comes almost exclusively from the frame's own administrators; confessional archivists document that polemical historiography has sharply declined, which supports the 'problem substantially solved' side of the contest. No neutral source attests that the founding problem is fully live today.
narrative_ontology:disappearance_verdict(reformation_event_boundary__composite_overdetermination_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__composite_overdetermination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__composite_overdetermination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Claim and metrics are authored independently. The claim is tangled_rope because the arrangement possesses both required components: a genuine coordination function (a shared, comparable object ending confessional historiographic warfare) AND asymmetric extraction through the same structure (single-driver programs pay in explanatory primacy while the administering seat collects authority), held in place by active enforcement (review baselines, curricular gatekeeping, commemorative scripting). The metrics describe observed operation: extractiveness 0.52 reflects real but soft costs — demoted primacy, constrained publication venues — not ruin; suppression 0.42 reflects normative enforcement rather than prohibition, since rival readings remain publishable and taught as alternatives; theater_ratio 0.31 reflects the growing share of 'complexity' invocations that perform thoroughness instead of engaging rival claims ('as we all know, the Reformation was overdetermined'); accessibility_collapse 0.35 is low because monocausal alternatives remain partly reachable through confessional and specialist venues; resistance 0.55 is substantial because revisionist surges recur and must be repeatedly absorbed. The temporal series run on one shared grid (1900–2020, seven points, all three metrics at every point): extractiveness climbs through the mid-century consolidation of the synthesis tradition, peaks around 1980, and plateaus as the frame becomes routine; suppression_requirement rises with the enforcement build-out and then eases slightly as compliance normalizes; theater_ratio rises monotonically as ritual invocation replaces engagement. The plateau-and-absorb dynamic is quasi-cyclical at finer resolution — periodic revisionist flares (1960s–80s social history, later global-history reframings) raise resistance temporarily and are converted into additional 'strands' — but the coarse grid captures the net ratchet rather than the flares.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administrator seat compute differently from identical nominal standing (all are credentialed academics). From the administrator seat the arrangement is a hard-won peace that finally made the subject researchable and teachable across confessions; from the constrained payer seats the same review baseline operates as a closure device — their questions are pre-answered as 'factors' before submission. The differentiation is carried by exit options, not global power: publishers hold arbitrage-grade exit (they sell whatever frame sells), commemoration bodies hold mobile episodic attachment, senior synthesists are identity_locked (their methodological identity IS the frame), and primacy scholars are constrained to specialist venues. The excluded memory-keepers experience a fourth version: not argument but quiet override of the dates by which their communities live.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. composite_framework_historians sit nearest the beneficiary end (they collect the frame's authority and set its rules; identity_lock amplifies their investment). survey_textbook_publishers derive low d from beneficiary status, damped further by arbitrage exit — they are beneficiaries who can leave cheaply. denominational_commemoration_bodies are incidental beneficiaries with mobile exit. The two primacy-scholar groups sit near the target end: they bear the transfer of explanatory primacy, and constrained exit (specialist venues only) traps them nearer full-target than their organizational resources alone would suggest. congregational_memory_keepers are excluded rather than coordinated — they are outside the derivation chain but bear the frame's periodization as an override of local memory. Scope is continental-to-global for the scholarly seats (harder verification, modest amplification of effective extraction) and local for the memory-keepers.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against mislabeling in both directions. Reading the arrangement as pure coordination (rope) would hide the absorption mechanism by which rival explanatory programs are demoted rather than engaged — the frame converts counter-evidence into additional 'strands,' which is extraction riding on coordination. Reading it as pure extraction (snare) would erase the genuine achievement: the frame really did end a century of confessional historiographic warfare and really does enable comparative work no confessional narrative could. Tangled rope holds both facts. The R5 mismatch consumer should watch the founding_problem_status x disappearance_verdict pair: status is contested and the verdict is world_rearranges, so no zombie flag fires yet — but if the confessional-polemics problem is confirmed dead while the frame persists by inertia, the arrangement drifts toward theatrical maintenance of a solved problem, and the theater_ratio series (rising steadily) is the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index_and_sibling_delta,
    'Does instantiating the composite reading correctly locate the kernel disagreement in the single-driver sufficiency premise, and would the sibling readings (theological_climb_reading, political_swap_reading) produce structurally different victim/beneficiary sets and epsilon values as the family decomposition predicts?',
    'Cross-reading comparison once the sibling files exist: check that each sibling''s victim set centers the constituency its primacy thesis demotes, and that epsilon diverges across readings over the fixed referent of the standing arrangement.',
    'If the siblings'' structures converge on this reading''s, the kernel is mis-drawn and should be redrawn (perhaps splitting periodization into its own kernel); if they diverge as predicted, the family decomposition is validated and contamination analysis can run across the triangle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_index_and_sibling_delta, conceptual, 'Committer-frame omega: this story is one reading of reformation_event_boundary; sibling readings are separate constraints whose structural deltas this omega tracks.').

omega_variable(
    unfalsifiability_absorption_mechanism,
    'Is the composite frame''s repeated conversion of monocausal challenges into additional ''strands'' a rational response to genuine historical multiplicity, or an unfalsifiable absorption device that treats all counter-evidence as confirmation?',
    'Search the record for monocausal accounts that predicted novel findings the composite frame missed (rather than merely re-describing known material); count absorption events where a primacy challenge changed the frame versus ones it merely added a strand to.',
    'If absorption dominates, the frame''s effective extraction is higher than the authored 0.52 suggests — the mechanism extracts falsifiability itself from rivals — and the arrangement trends toward snare; if genuine multiplicity dominates, extraction is overstated and the rope component is larger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfalsifiability_absorption_mechanism, empirical, 'Whether the frame''s resistance to primacy claims is epistemically warranted or self-sealing.').

omega_variable(
    periodization_completion_point_ambiguity,
    'Which completion point defines ''the end of the Reformation'' — theological settlement (~1580 confessional formulae), institutional stabilization (varying by polity), political settlement (1555 or 1648), or denominational consolidation (~1700) — and is any choice forced by the evidence rather than by which strand the periodizer foregrounds?',
    'No purely empirical resolution: the choice tracks foregrounded strand by construction. Track whether convergent usage ever emerges across subfields, or whether completion-point pluralism persists indefinitely.',
    'Each completion point assigns the frame''s costs to a different generation of scholars and communities; persistent ambiguity means the constraint''s temporal boundary — and therefore whose extraction is measured — remains a framing choice, sustaining the overdetermination claim itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(periodization_completion_point_ambiguity, conceptual, 'The event''s endpoint is strand-relative; periodization contests are structural, not evidential.').

omega_variable(
    global_history_expansion_effect,
    'Does the global-history turn strengthen the composite frame (more strands, wider scope, reinforced irreducibility) or dissolve it (the object fragments into connected but separately-bounded processes that no longer compose a single ''Reformation'')?',
    'Track whether global-history syntheses continue to organize under the title ''the Reformation'' or migrate to replacement categories (confessionalization, evangelical reformations, Eurasian Christian encounters) in titles, curricula, and funding calls.',
    'Strengthening raises the frame''s scope and thus effective extraction amplification; dissolution would retire the constraint as its object evaporates — a death by success distinct from mandatrophy, with different network consequences for the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_history_expansion_effect, empirical, 'Whether contemporary historiography reinforces or dissolves the composite object.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__composite_overdetermination_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ref_comp_od_tr_t1900, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(ref_comp_od_tr_t1920, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1920, 0.14).
narrative_ontology:measurement(ref_comp_od_tr_t1940, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1940, 0.19).
narrative_ontology:measurement(ref_comp_od_tr_t1960, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1960, 0.24).
narrative_ontology:measurement(ref_comp_od_tr_t1980, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 1980, 0.28).
narrative_ontology:measurement(ref_comp_od_tr_t2000, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(ref_comp_od_tr_t2020, reformation_event_boundary__composite_overdetermination_reading, theater_ratio, 2020, 0.31).

% Extraction over time
narrative_ontology:measurement(ref_comp_od_be_t1900, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(ref_comp_od_be_t1920, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1920, 0.34).
narrative_ontology:measurement(ref_comp_od_be_t1940, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1940, 0.4).
narrative_ontology:measurement(ref_comp_od_be_t1960, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(ref_comp_od_be_t1980, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(ref_comp_od_be_t2000, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2000, 0.53).
narrative_ontology:measurement(ref_comp_od_be_t2020, reformation_event_boundary__composite_overdetermination_reading, base_extractiveness, 2020, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(ref_comp_od_su_t1900, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1900, 0.25).
narrative_ontology:measurement(ref_comp_od_su_t1920, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1920, 0.3).
narrative_ontology:measurement(ref_comp_od_su_t1940, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1940, 0.38).
narrative_ontology:measurement(ref_comp_od_su_t1960, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1960, 0.45).
narrative_ontology:measurement(ref_comp_od_su_t1980, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 1980, 0.44).
narrative_ontology:measurement(ref_comp_od_su_t2000, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2000, 0.43).
narrative_ontology:measurement(ref_comp_od_su_t2020, reformation_event_boundary__composite_overdetermination_reading, suppression_requirement, 2020, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__composite_overdetermination_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__theological_climb_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__composite_overdetermination_reading, reformation_event_boundary__political_swap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the kernel reformation_event_boundary decomposes into three reading-stories because the colloquial label 'the Reformation' conflates structurally distinct claims — the driver question (what caused it), the beneficiary question (who gained), and the boundary question (when it began and ended). Each reading carries its own epsilon over the fixed referent of the standing arrangement: this composite reading authors the overdetermination norm's structure; theological_climb_reading authors the doctrinal-breakthrough account; political_swap_reading authors the realignment account. The composite reading is downstream-authoritative in one sense (its frame supplies the review baseline both siblings must answer) yet logically prior to neither — its foundational axiom negates both siblings' primacy theses, and both siblings negate its irreducibility axiom. Edges are declared bidirectionally aware: this file links to both siblings; each sibling should link back and to the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
