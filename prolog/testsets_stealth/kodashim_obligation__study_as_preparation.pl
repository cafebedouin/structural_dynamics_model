% ============================================================================
% CONSTRAINT STORY: kodashim_obligation__study_as_preparation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_obligation__study_as_preparation, []).

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
 *   constraint_id: kodashim_obligation__study_as_preparation
 *   human_readable: Kodashim Study as Messianic Preparation (Binding-but-Unperformable Reading)
 *   domain: religious/legal/textual_preservation
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kodashim_obligation kernel:
 *   study_as_preparation, under which sacrificial law remains fully binding
 *   but unperformable in the Temple's absence, and the mandated study of the
 *   sacrificial orders exists to preserve technical knowledge for the day
 *   performance resumes. The colloquial label 'studying korbanot' covers
 *   three structurally distinct claims — preparation (this file), enactment
 *   (study_as_performance, where study performs the cosmic function now), and
 *   archive (study_as_archive, where the law is defunct and study is
 *   historical identity-maintenance) — decomposed per the epsilon-invariance
 *   principle into separate stories linked by network edges. The epsilon
 *   referent here is the standing arrangement under contest: the
 *   binding-but-unperformable obligation sustained through
 *   study-as-preparation, assessed BY THIS READING'S OWN LIGHTS. By its own
 *   lights the arrangement is largely benign — study is instrumental
 *   preparation, the burden is light, the deferral is providential — hence
 *   low extractiveness; but even on its own terms the current generation
 *   bears a real cost: atonement rites it owes remain unperformed, with
 *   repair deferred, and it maintains expertise it cannot use. Claimed type
 *   and metrics are independent authored facts: I claim tangled_rope because
 *   the structure holds both a genuine coordination function
 *   (intergenerational knowledge preservation against atrophy) and a temporal
 *   extraction asymmetry (present payers, future collectors) under active
 *   enforcement, while the metric values honestly report that the extraction
 *   is small. The engine computes per-seat classifications from the
 *   structural data; divergence between my claim and any computed seat is
 *   signal, not error. Time points map to years since 1950 (T0=1950,
 *   T75=2025); all measurement points are observed.
 *
 * KEY AGENTS:
 *   - - halakhic_authorities: agenda-setting seat (institutional/identity_locked) — administers the binding ruling and the curriculum mandates that keep the sacrificial orders in circulation
 *   - - current_exile_generation: primary bearer (moderate/constrained) — carries the suspended obligation, the deferred repair, and the maintenance costs; incidentally receives identity and meaning
 *   - - yeshiva_students: concentrated study burden (moderate/mobile) — the required-curriculum seat with real intra-world mobility
 *   - - messianic_restoration_generation: designated collector (powerless/trapped) — inherits the preserved capability if restoration arrives; takes no present action
 *   - - temple_readiness_movement: incidental collector and promoter (organized/mobile) — converts the preparation premise into funded projects
 *   - - reform_and_conservative_movements: excluded objectors (organized/mobile) — deny the binding premise from outside the framework
 *   - - academic_observer_seat: analytical observer (analytical/analytical) — sees the full two-millennium record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_obligation__study_as_preparation, 0.28).
domain_priors:suppression_score(kodashim_obligation__study_as_preparation, 0.22).
domain_priors:theater_ratio(kodashim_obligation__study_as_preparation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, extractiveness, 0.28).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(kodashim_obligation__study_as_preparation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_obligation__study_as_preparation, tangled_rope).
narrative_ontology:human_readable(kodashim_obligation__study_as_preparation, "Kodashim Study as Messianic Preparation (Binding-but-Unperformable Reading)").
narrative_ontology:topic_domain(kodashim_obligation__study_as_preparation, "religious/legal/textual_preservation").

domain_priors:requires_active_enforcement(kodashim_obligation__study_as_preparation).
narrative_ontology:has_sunset_clause(kodashim_obligation__study_as_preparation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_obligation__study_as_preparation, '4e893cde-cc7f-4357-8f2c-70fc13ef77d8').
narrative_ontology:cs_kernel_codification('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', formalized).
narrative_ontology:cs_authority_grounding('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', lineage).
narrative_ontology:cs_interpretation_layer_present('4e893cde-cc7f-4357-8f2c-70fc13ef77d8').
narrative_ontology:cs_reading_relation('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', kodashim_obligation__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', kodashim_obligation__study_as_archive, forecloses).
narrative_ontology:cs_axiom('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', foundational, sacrificial_law_remains_binding).
narrative_ontology:cs_axiom_status(sacrificial_law_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', sacrificial_law_remains_binding, theological).
narrative_ontology:cs_axiom('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', foundational, study_preserves_capability_for_resumption).
narrative_ontology:cs_axiom_status(study_preserves_capability_for_resumption, holdable).
narrative_ontology:cs_axiom_grounding('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', study_preserves_capability_for_resumption, instrumental).
narrative_ontology:cs_axiom('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', secondary, physical_temple_required_for_performance).
narrative_ontology:cs_axiom_status(physical_temple_required_for_performance, holdable).
narrative_ontology:cs_axiom_grounding('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', physical_temple_required_for_performance, conventional).
narrative_ontology:cs_reference_frame('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', binding_law_pending_restoration).
narrative_ontology:cs_drift_state('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', contemporary_post_temple_mount_access, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('4e893cde-cc7f-4357-8f2c-70fc13ef77d8', '').
narrative_ontology:cs_kernel_id(kodashim_obligation__study_as_preparation, kodashim_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, messianic_restoration_generation).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, temple_readiness_movement).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, current_exile_generation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_obligation__study_as_preparation, current_exile_generation).
narrative_ontology:constraint_victim(kodashim_obligation__study_as_preparation, yeshiva_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Posekim, yeshiva leadership, and rabbinic courts who maintain the ruling that sacrificial law retains full normative force despite the Temple's absence, set curriculum requirements that keep the sacrificial orders in the study cycle, and adjudicate how the obligation applies without a functioning altar. They inherit the position from prior generations of deciders and transmit it to successors; abandoning it would mean repudiating the received law they exist to uphold.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, halakhic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Observant communities worldwide who carry the obligation in its suspended form: they fund and staff the schools that teach the sacrificial orders, recite the relevant passages in liturgy, and live with the tradition's account that atonement rites they cannot perform remain owed, with repair deferred until restoration. They receive communal identity, textual continuity, and meaning from the same practice that burdens them. Leaving the practice would mean leaving the communal and familial world that gives it shape.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, current_exile_generation, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, current_exile_generation, beneficiary).

% Advanced Talmud students for whom the sacrificial orders are a required curriculum portion: dense technical material with no current practical application. Many experience mastery of it as prestigious and identity-confirming; some find it arid. Compliance while enrolled is near-universal, but students retain real mobility between institutions and tracks, and attrition out of the academy world altogether is an existing valve.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, yeshiva_students, payer,
    moderate, biographical, mobile, global).

% The future community the tradition anticipates as certain, which would inherit a working technical culture of sacrificial service on the day a rebuilt Temple resumes operation: trained officiants, written procedure, settled practice. They take no action and bear no present burden; everything the arrangement accumulates is held in trust for them. Whether they ever arrive is precisely what the framework asserts and outsiders doubt.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, messianic_restoration_generation, beneficiary,
    powerless, civilizational, trapped, global).

% Organizations such as the Temple Institute and allied research bodies that convert the preparation premise into concrete projects: vessel reconstruction, priestly training, architectural and agrarian research, public education. They raise funds and recruit on the strength of the restoration claim, drawing institutional standing and livelihood from the very frame they promote, and could redirect their efforts if the frame lost support.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, temple_readiness_movement, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(kodashim_obligation__study_as_preparation, temple_readiness_movement, agenda_setter).

% Denominations that rejected the restorationist premise outright: they hold sacrificial law superseded or reinterpreted, teach the material as history or liturgy without binding force, and would object to any public arrangement premised on resumed sacrifice. They sit outside the halakhic framework's jurisdiction and have already exited it.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, reform_and_conservative_movements, excluded,
    organized, generational, mobile, continental).

% Historians and scholars of rabbinic Judaism who study how the post-destruction community kept an unperformable legal corpus alive, comparing it with other traditions' handling of interrupted ritual systems. They describe the mechanism and its two-millennium record without standing inside its normative claims.
narrative_ontology:constraint_stakeholder(kodashim_obligation__study_as_preparation, academic_observer_seat, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_obligation__study_as_preparation, messianic_restoration_generation).
narrative_ontology:fixing_cost_class(kodashim_obligation__study_as_preparation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves an intergenerational knowledge-preservation problem: procedural competence in sacrificial service cannot be reconstructed quickly from texts alone under crisis conditions, so distributed mandatory study keeps the technical corpus alive at population scale, ensuring that a restoration would find practitioners, teachers, and settled practice ready rather than starting from archival silence.
% TRANSFER_FUNCTION: Moves study-time, curricular attention, and institutional resources from the current generation of observers and students toward a future generation's capability; within the framework it also moves merit and the credit of preparation from present effort to the future act of restoration.
% ABSENT_VOICES: Jews who regard the sacrificial system as superseded or ethically problematic sit outside the conversation by framework boundary — the halakhic process does not admit their premise as a vote. Animal-welfare perspectives raised about resumed sacrifice have no standing seat in classical sources. Within the community, students who find the material arid lack any channel for contesting curriculum composition.
% DISAPPEARANCE_RATIONALE: If the obligation and its study-apparatus vanished overnight, yeshiva curricula would shed the sacrificial orders within a generation, the restoration-readiness organizations would lose their legal warrant and funding rationale, liturgical references to sacrifice would lose their preparatory reading, and the community's entire posture toward the Temple idea — mourned, prayed-for, studied — would reorganize around memory rather than readiness.
% FOUNDING_PROBLEM: After the Temple's destruction, a community bound by laws it believed divinely commanded faced a crisis: the laws remained binding but unperformable, and without continuous engaged transmission the procedural knowledge would atrophy beyond recovery, leaving any future restoration without the human infrastructure to resume service. The arrangement was built to keep an unperformable obligation alive without performance.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the post-destruction rabbinic corpus itself documents the crisis response; academic historians of the Yavnean period independently attest the deliberate preservation motive; and comparative ritual studies showing rapid technique-loss in traditions that discontinued practice corroborate that the atrophy risk was real — as does the fact that the knowledge demonstrably survived two millennia of non-performance, which is the arrangement working. No living party attests the problem is dead from inside the framework; the archive-reading's denial comes from outside it.
narrative_ontology:disappearance_verdict(kodashim_obligation__study_as_preparation, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_obligation__study_as_preparation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_obligation__study_as_preparation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_obligation__study_as_preparation, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_obligation__study_as_preparation, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_obligation__study_as_preparation_tests).
:- end_tests(kodashim_obligation__study_as_preparation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28) because the per-participant cost is a fractional curriculum share, participants overwhelmingly experience study as intrinsically valuable, and the payoff — preserved capability — is a real option within the framework's own commitments. Suppression is low-moderate (0.22): enforcement runs through education and communal norm rather than sanction, and exit exists though it is costly. Theater ratio (0.22) reflects a real functional core — commentaries, practical halakha of the service, priestly training, vessel reconstruction — alongside a growing devotional-recitation share that comforts without retaining technique. Accessibility collapse is moderate (0.45): alternatives are plentiful (other tractates, the sibling readings themselves, exit from observance), so understanding the arrangement does not foreclose substitutes. Resistance is low (0.15): the material is honored, not contested, inside the framework. The measurement series run on ONE shared grid (T=0,15,30,45,60,75) with every tracked metric authored at every point. Suppression_requirement is tracked deliberately rather than left static: the story's enforcement picture genuinely changed over the interval — the practical-restoration movement built expectation machinery (curricular emphasis, readiness advocacy, funded training) that hardened communal norms around korbanot fluency, a mild ratchet visible in the 0.16-to-0.23 rise. Extractiveness and theater drift gently upward for the same reason: readiness investment raises both the resource flow and the symbolic share.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is a faithful custodianship: the authorities bear governance costs, collect legitimacy, and cannot abandon the position without dissolving their own office. From the payer seats the same structure is a light but real tax — attention and deferment paid now for a benefit collected by others later. From the designated collector's seat nothing is felt at all: the beneficiary is structurally silent, which is precisely why the asymmetry never generates internal resistance. The excluded denominations compute a fourth thing: a defunct system mistaken for a live one. The engine derives these divergences from power, exit, and role data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. The messianic_restoration_generation sits at the full-beneficiary end (d near 0.0): everything flows to them, they are trapped by nonexistence, and scope is global. The current_exile_generation sits well toward the target end (d near 0.8): they bear the deferred repair and the maintenance burden with only constrained exit, moderated by their secondary beneficiary position (identity, meaning, textual continuity accrue to them now). Yeshiva_students, the concentrated payer seat, derive high d but their mobility damps effective extraction relative to the locked general community. The temple_readiness_movement derives low d from its beneficiary role; its dual agenda-setting position is noted but its extraction is incidental, not structural. The halakhic_authorities have no beneficiary/victim declaration — they are stewards, not collectors — so the derivation chain would fall back to a power-atom default that risks misreading custodianship as either capture or subsidy; the directionality override sets them near-symmetric (d=0.45) to encode governance costs balanced against legitimacy gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: the Temple does not stand, the laws remain binding on the framework's own terms, and the atrophy risk the arrangement was built against is real every year restoration has not come — so mandatrophy is NOT resolved, and the boolean is authored false on that basis rather than keyed to any metric. The classification prevents mislabeling in both directions: calling this a pure rope ignores the genuine temporal asymmetry (present payers, future collector who cannot reciprocate); calling it a snare ignores that the coordination function is real, the burden is light, and the victim set is a whole generation diffused across two millennia rather than a squeezed class. The dangerous drift vector is zombie-hood: a sunset clause (restoration) that has been pending for two millennia and absorbs every non-arrival as 'not yet' behaves less like a termination condition and more like an unfalsifiable warrant. The sunset_operativity omega tracks exactly this; if the clause is theatrical, the arrangement is a steady-state institution wearing transitional clothing, and the tangled_rope reading hardens. If restoration is treated as a genuine operative condition, the arrangement retains its transitional character and the low extraction is the honest price of an open option.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_contingency,
    'Is the messianic restoration a genuine future event that will mature the arrangement''s payoff, or an indefinitely deferrable horizon that makes the preparation function unfalsifiable?',
    'Not resolvable from inside the framework, which holds restoration certain by faith; resolvable only by observing whether practical restoration capability demonstrably increases over generations or the readiness apparatus reproduces itself without converging.',
    'If the horizon is infinitely deferrable, the temporal extraction asymmetry becomes a permanent one-way transfer from every present generation to a collector who never arrives, pushing the arrangement toward snare-flavored computation; if restoration is treated as a live possibility, the arrangement is a genuine intergenerational option and the low extraction stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_contingency, conceptual, 'Whether the arrangement''s deferred payoff can ever mature.').

omega_variable(
    reading_instantiation_ambiguity,
    'Does observed kodashim study practice actually instantiate the preparation reading, or does it operationally blend into the performance reading (devotional enactment) or the archive reading (identity maintenance)?',
    'Survey what practitioners avow and what curricula optimize for: technical retention and procedural testing indicate preparation; recitation-as-devotion without retention indicates performance; historical-critical treatment indicates archive. The temple_readiness_movement''s growth is a preparation-side signal.',
    'Under the performance reading the epsilon referent shifts (study is the function, not preparation for it) and the deferred-repair victim set changes; under the archive reading the binding claim dies and the arrangement reduces to identity coordination. This file''s classification holds only for the preparation instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_instantiation_ambiguity, empirical, 'Which sibling reading best describes actual practice.').

omega_variable(
    knowledge_atrophy_rate,
    'How fast would sacrificial-procedural knowledge actually decay without the mandated study obligation?',
    'Comparative historical data on ritual-technique loss in communities that discontinued practice (e.g., traditions that lost sacrificial or liturgical craft within generations versus the Samaritan Passover sacrifice''s continuous transmission), plus modern learning-science estimates on skill decay without rehearsal.',
    'If texts alone suffice for reconstruction, the coordination function shrinks and the obligation''s marginal contribution drops, weakening the rope half of the hybrid; if hands-on transmission is irreplaceable, the coordination function is strong and the arrangement''s justification firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_atrophy_rate, empirical, 'Whether the knowledge-preservation function is load-bearing.').

omega_variable(
    voluntary_participation_selection,
    'Is the current generation genuinely a bearing party, or a self-selected population for whom the study is intrinsically rewarding and therefore no imposition at all?',
    'Revealed-preference data: retention and curriculum-choice behavior where electives exist, attrition patterns at the study-to-practice boundary, and whether members who de-emphasize kodashim face sanction or merely indifference.',
    'If participation is effectively voluntary, the extraction asymmetry largely dissolves and the arrangement computes nearer pure rope; if the obligation binds indifferent members through communal pressure, the hybrid''s extraction half strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntary_participation_selection, empirical, 'Whether the payer seat experiences cost or reward.').

omega_variable(
    sunset_operativity,
    'Is the declared sunset condition (Temple restoration) an operative termination clause, or a theatrical one that absorbs all counter-evidence as ''not yet''?',
    'Analyze the clause''s falsifiability structure: specify what observable state of the world would count as the condition failing, and test whether the tradition''s interpretive machinery could absorb any outcome whatsoever.',
    'If theatrical, the arrangement is a steady-state institution wearing transitional clothing — the scaffold-flavored charity of the sunset clause evaporates and the tangled_rope reading hardens; if operative, the arrangement retains genuine transitional character and its low extraction is the honest price of an open option.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_operativity, conceptual, 'Whether the arrangement''s built-in termination condition can actually fire.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_obligation__study_as_preparation, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_prep_tr_t0, kodashim_obligation__study_as_preparation, theater_ratio, 0, 0.14).
narrative_ontology:measurement(kodashim_study_prep_tr_t15, kodashim_obligation__study_as_preparation, theater_ratio, 15, 0.16).
narrative_ontology:measurement(kodashim_study_prep_tr_t30, kodashim_obligation__study_as_preparation, theater_ratio, 30, 0.17).
narrative_ontology:measurement(kodashim_study_prep_tr_t45, kodashim_obligation__study_as_preparation, theater_ratio, 45, 0.19).
narrative_ontology:measurement(kodashim_study_prep_tr_t60, kodashim_obligation__study_as_preparation, theater_ratio, 60, 0.21).
narrative_ontology:measurement(kodashim_study_prep_tr_t75, kodashim_obligation__study_as_preparation, theater_ratio, 75, 0.22).

% Extraction over time
narrative_ontology:measurement(kodashim_study_prep_be_t0, kodashim_obligation__study_as_preparation, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(kodashim_study_prep_be_t15, kodashim_obligation__study_as_preparation, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(kodashim_study_prep_be_t30, kodashim_obligation__study_as_preparation, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(kodashim_study_prep_be_t45, kodashim_obligation__study_as_preparation, base_extractiveness, 45, 0.26).
narrative_ontology:measurement(kodashim_study_prep_be_t60, kodashim_obligation__study_as_preparation, base_extractiveness, 60, 0.27).
narrative_ontology:measurement(kodashim_study_prep_be_t75, kodashim_obligation__study_as_preparation, base_extractiveness, 75, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_study_prep_su_t0, kodashim_obligation__study_as_preparation, suppression_requirement, 0, 0.16).
narrative_ontology:measurement(kodashim_study_prep_su_t15, kodashim_obligation__study_as_preparation, suppression_requirement, 15, 0.17).
narrative_ontology:measurement(kodashim_study_prep_su_t30, kodashim_obligation__study_as_preparation, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(kodashim_study_prep_su_t45, kodashim_obligation__study_as_preparation, suppression_requirement, 45, 0.2).
narrative_ontology:measurement(kodashim_study_prep_su_t60, kodashim_obligation__study_as_preparation, suppression_requirement, 60, 0.22).
narrative_ontology:measurement(kodashim_study_prep_su_t75, kodashim_obligation__study_as_preparation, suppression_requirement, 75, 0.23).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_obligation__study_as_preparation, information_standard).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_performance).
narrative_ontology:affects_constraint(kodashim_obligation__study_as_preparation, kodashim_obligation__study_as_archive).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'studying korbanot' decomposes into three structurally distinct claims sharing one kernel (kodashim_obligation). This file (study_as_preparation) holds the upstream epistemic position on binding-status — it affirms the law's continued force, which the archive reading denies and the performance reading presupposes differently. The preparation reading influences its siblings: its insistence that restoration is structurally required creates the conditions under which the performance reading's Temple-indifference claim becomes controversial, and its live-binding premise is precisely what the archive reading rejects. Each member authors its own epsilon over the same standing arrangement by its own lights; the epsilons differ because the readings differ, not because the referent does.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_obligation__study_as_preparation, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
