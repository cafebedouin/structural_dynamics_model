% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrificial-Law Study as Non-Normative Cultural Archive
 *   domain: religious_law/ritual_studies/textual_tradition
 *
 * SUMMARY:
 *   This file authors the archival_preservation reading of the
 *   sacrifice_obligation_continuity kernel as a clean, epsilon-invariant
 *   constraint: after the Jerusalem rite became unperformable, a standing
 *   arrangement grew up in which the sacrificial legislation continues to be
 *   taught, edited, digitized, exhibited, and studied — under this reading's
 *   lights with no normative claim attached and participation wholly
 *   voluntary. The epsilon referent is that standing study-and-stewardship
 *   arrangement as this reading assesses it, never any sibling's
 *   obligation-bearing alternative. Claim and metrics are independent
 *   authored facts: the claim is rope (genuine, minimally coercive
 *   coordination of cultural memory); the metrics describe near-inert
 *   operation (epsilon 0.07, suppression 0.04, theater 0.16) — any divergence
 *   the engine computes from these inputs is the datum, not an error to
 *   reconcile. Assumptions stated openly: the interval 0-100 maps to
 *   1920-2020 (institutionalization of academic Jewish studies through the
 *   present heritage economy); group names denote real actors
 *   (ordination-track academies, mass study-cycle networks, critical-edition
 *   publishers); the excluded authority seat is commentary-grade only per R3
 *   and drives no classification input.
 *
 * KEY AGENTS:
 *   - talmudic_academies_and_seminaries: curriculum administrator and mild receipt seat (institutional/constrained) — schedules the corpus, employs teachers, collects enrollment-linked funds
 *   - heritage_study_participants: voluntary participant-beneficiaries (organized/mobile) — enter and leave study cycles at will
 *   - rabbinic_literature_scholars: professional beneficiaries (moderate/mobile) — edit and teach the corpus as literature and history
 *   - diaspora_identity_communities: collective beneficiaries (organized/generational) — carry the curriculum as continuity programming
 *   - academic_publishers_and_digitizers: commercial beneficiaries (organized/arbitrage) — sell editions and databases, catalogs redirectable at will
 *   - museums_and_heritage_foundations: institutional beneficiaries (generational/continental) — fund exhibits and public translation of the material
 *   - halakhic_authorities_upholding_binding_force: excluded dissenting seat (powerful/identity_locked) — object from outside the curatorial venues
 *   - historians_of_religion: analytical observer — document the conversion of ritual law into heritage
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.07).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.04).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.16).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.07).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.04).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.16).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.18).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrificial-Law Study as Non-Normative Cultural Archive").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/ritual_studies/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '13826785-0f83-4a3e-bd31-f9593bdf629b').
narrative_ontology:cs_kernel_codification('13826785-0f83-4a3e-bd31-f9593bdf629b', fixed_text).
narrative_ontology:cs_authority_grounding('13826785-0f83-4a3e-bd31-f9593bdf629b', expertise).
narrative_ontology:cs_interpretation_layer_present('13826785-0f83-4a3e-bd31-f9593bdf629b').
narrative_ontology:cs_reading_relation('13826785-0f83-4a3e-bd31-f9593bdf629b', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('13826785-0f83-4a3e-bd31-f9593bdf629b', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('13826785-0f83-4a3e-bd31-f9593bdf629b', sacrifice_obligation_continuity__messianic_suspension, influences).
narrative_ontology:cs_axiom('13826785-0f83-4a3e-bd31-f9593bdf629b', foundational, sacrificial_obligation_terminated_not_latent).
narrative_ontology:cs_axiom_status(sacrificial_obligation_terminated_not_latent, holdable).
narrative_ontology:cs_axiom_grounding('13826785-0f83-4a3e-bd31-f9593bdf629b', sacrificial_obligation_terminated_not_latent, empirically_contingent).
narrative_ontology:cs_axiom('13826785-0f83-4a3e-bd31-f9593bdf629b', secondary, study_confers_no_normative_standing).
narrative_ontology:cs_axiom_status(study_confers_no_normative_standing, holdable).
narrative_ontology:cs_axiom_grounding('13826785-0f83-4a3e-bd31-f9593bdf629b', study_confers_no_normative_standing, conventional).
narrative_ontology:cs_reference_frame('13826785-0f83-4a3e-bd31-f9593bdf629b', terminal_cessation_heritage_custody).
narrative_ontology:cs_drift_state('13826785-0f83-4a3e-bd31-f9593bdf629b', contemporary, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('13826785-0f83-4a3e-bd31-f9593bdf629b', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, talmudic_academies_and_seminaries).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, rabbinic_literature_scholars).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, heritage_study_participants).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, diaspora_identity_communities).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, academic_publishers_and_digitizers).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, museums_and_heritage_foundations).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, terminal_cessation_of_sacrificial_obligation).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__archival_preservation, textual_corpus_as_cultural_artifact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set the sequence of tractates students encounter in ordination-track and degree programs, including the sacrificial orders, and employ the teachers who deliver them. Tuition and enrollment-linked gifts follow a curriculum recognizable as covering the whole classical corpus. Dropping the sacrificial orders is possible but would unsettle alumni expectations, faculty appointments, and the institution's presentation as custodian of the complete tradition, so revisiting the sequence is a recurring governance decision rather than a settled matter.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, talmudic_academies_and_seminaries, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, talmudic_academies_and_seminaries, beneficiary).

% Edit, translate, annotate, and teach the sacrificial legislation as literature and history; earn salaries, grants, and reputational standing from publications on these materials. Nothing binds them to this corpus — adjacent fields offer parallel career paths — and individual scholars routinely move between them over a working life.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, rabbinic_literature_scholars, beneficiary,
    moderate, biographical, mobile, global).

% Join daily or weekly study cycles that pass through the sacrificial tractates, in congregations, online platforms, and community classes. Attendance is voluntary; participants report gaining textual literacy, companionship, and a sense of continuity, and may stop attending without sanction. Some mark completions publicly, which feeds the ceremonial life around the practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, heritage_study_participants, beneficiary,
    organized, biographical, mobile, global).

% Carry the study curriculum inside communal schooling and adult-education programming, treating fluency in the classical corpus as part of intergenerational continuity. Revising the curriculum to drop the sacrificial orders is deliberatively available but carries identity and donor-relations costs, so change happens slowly and rarely.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, diaspora_identity_communities, beneficiary,
    organized, generational, constrained, global).

% Print critical editions, translations, and searchable databases of the sacrificial materials; revenue scales with course adoptions and institutional subscriptions. Catalogs can be redirected to other titles freely; these houses carry the corpus because it sells steadily to institutions, not because anything obligates its publication.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, academic_publishers_and_digitizers, beneficiary,
    organized, biographical, arbitrage, global).

% Fund exhibitions, scale models, and digital reconstructions of the ancient service, and underwrite lecture series translating the sacrificial legislation for general audiences. Program decisions follow board priorities and visitor interest; the subject competes with other heritage topics for the same budgets.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, museums_and_heritage_foundations, beneficiary,
    institutional, generational, constrained, continental).

% Decisors, academy heads, and movement leaders who hold that the sacrificial commandments retain force — whether presently unperformable, study-substituted, or suspended pending restoration. They regard presenting the corpus as mere heritage as a misstatement of its standing and voice this objection in their own journals, schools, and courts; they do not sit on the museum boards, foundation panels, or university committees where heritage curricula are actually set.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, halakhic_authorities_upholding_binding_force, excluded,
    powerful, generational, identity_locked, global).

% Document how ritual law converts into heritage across generations — tracking curricula, museum programming, and readership — without administering or funding any of it. Their accounts are cited by all sides and bind none.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, historians_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__archival_preservation, talmudic_academies_and_seminaries).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__archival_preservation, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission of a large, technically demanding corpus across dispersed generations and communities: standardized editions, shared pedagogical sequences, and common terminology keep the sacrificial legislation legible to non-specialists — solving the collective problem that such corpora decay quickly without coordinated teaching.
% TRANSFER_FUNCTION: Moves tuition, enrollment-linked funding, and endowed support from students and donors to academies, publishers, and heritage institutions; moves scholarly labor and attention from professionals to the corpus; moves cultural capital and communal belonging to participants. Every transfer rides on voluntary enrollment; none is compelled by normative claim.
% ABSENT_VOICES: Halakhic authorities who hold the sacrificial commandments binding, study-substituted, or suspended are not seated in the curatorial bodies that set heritage curricula; their objection — that de-normativizing the corpus misstates its standing — is voiced in their own forums and enters this frame only as outside commentary. Descendants who read the corpus as record of loss rather than heritage asset are similarly unseated. Their absence lets the archive framing present itself as the obvious reading of the material.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the rearrangement is real but contained: courses disappear from catalogs, publishing lines and database development halt, museum programming loses a staple, and study cycles shorten their routes. The texts themselves survive in libraries; what dies is the living transmission infrastructure that keeps them readable. Nothing catastrophic follows, because no one owes anything under this arrangement — but careers, revenue lines, and communal calendars demonstrably depend on it.
% FOUNDING_PROBLEM: After the destruction of the Temple made the sacrificial rite unperformable, the tradition faced the problem of keeping a vast, technically dense legislative corpus — the orders of offerings, the service descriptions, their ritual arithmetic — comprehensible, without which scripture, liturgy, and subsequent legal layers could not be read at all. The study-and-preservation arrangement was built to transmit that comprehension across generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: philologists and academic historians of religion document that technical corpora require continuous pedagogical infrastructure to remain readable and track declining Aramaic literacy as a concrete preservation risk; library circulation and database usage statistics show sustained but concentrated dependence on institutional teaching; heritage-conservation assessments treat the ritual-service knowledge as fragile intangible heritage. None of these attestors collects tuition, endowment income, or sales from the arrangement.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.07, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.07: the only frictions are bundled curricula (students in ordination tracks meet assigned tractates whether or not drawn to them), credential premiums, and subscription pricing — real but marginal, and all downstream of voluntary enrollment. Suppression 0.04: there is no enforcement machinery to speak of; leaving a study cycle, a program, or a catalog carries no sanction beyond ordinary social texture, so the scalar records ambient expectation rather than coercion. Theater_ratio 0.16: completion ceremonies, anniversary galas, and heritage packaging are growing, but the core function — keeping a technically forbidding corpus legible — is performed and externally corroborated. Accessibility_collapse 0.18: alternatives remain fully available (other corpora, other framings, abstention), so understanding the arrangement forecloses almost nothing. Resistance 0.10: a demandless practice invites little opposition; the visible contest comes from the excluded authority seat disputing the framing, not from anyone paying a price under it. All measurements share one six-point grid (0, 20, 40, 60, 80, 100 on the 1920-2020 mapping); both tracked series are authored at every point. A suppression_requirement series is intentionally omitted: enforcement capacity is static at approximately nil across the century, a picture the scalar already carries — authoring a flat series would add noise, not information.
 *
 * PERSPECTIVAL GAP:
 *   Divergence here is subtler than payer-versus-beneficiary, since nearly every interior seat benefits. From inside an ordination program the assigned tractates feel lightly compulsory — the academy seat experiences its own bundling as pedagogical duty. From the study-cycle seat the same pages are pure gift, entered and left at will. From the excluded authority seat the identical folios are commandments whose de-normativization is itself the scandal. Same text, three lived situations. The engine computes per-seat classifications from the power, exit, and role data supplied above; this commentary supplies the structural asymmetries, not the adjudication.
 *
 * DIRECTIONALITY LOGIC:
 *   Every party declared in base_properties is a beneficiary, so derived directionality sits near the subsidy pole for all interior seats and effective extraction dampens toward zero — matching the reading's no-normative-force premise. Suppression is authored as a raw structural property and is not scaled; extractiveness alone rides directionality and scope in the engine's arithmetic. The excluded authority seat derives a target-side d, but it stands outside this arrangement: its costs are carried in the sibling files' constraint spaces, where the same corpus is obligation-bearing — per R3 its presence here steers no classification input. gain_flow names talmudic_academies_and_seminaries because the only receipts in the picture — tuition and enrollment-linked funds — land on the seat that also sets the curriculum; fixing_cost is cheap because dissolving the arrangement amounts to a curriculum revision and a catalog decision, not a structural teardown.
 *
 * MANDATROPHY ANALYSIS:
 *   The piton temptation is strong on silhouette: an obligation-shaped practice persisting long after its original mandate (fulfillment through engagement) has lapsed. Three facts block that reading here. First, honest relabeling: custodians openly teach the corpus as heritage and literature; nothing is maintained under a false pretense of obligation, and theater_ratio stays far from piton range. Second, the preservation function is live — external attestors (philologists, conservators, usage statistics) confirm the corpus decays without this infrastructure — so the R5 mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges: no zombie flag. Third, the cost-asymmetry test fails in the piton direction: academies could drop the sacrificial orders cheaply and do restructure curricula periodically, yet keep them because the legibility function is real, not because anyone is trapped. The mandatrophy verdict is transformation, not death: the mandate migrated from fulfillment to stewardship. The guard runs the other way too — the voluntariness omega tracks whether curriculum bundling quietly reintroduces extraction at the margin, protecting against rope-washing a mild capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This file instantiates only the archival_preservation reading of kernel sacrifice_obligation_continuity; which of the four readings governs a given study setting determines whether the practice carries normative force at all — the master uncertainty for the family''s epsilon.',
    'Track curricular self-description across denominations and institutions (obligation-framed versus heritage-framed syllabi), plus adoption and attrition data wherever a setting switches framings.',
    'If an obligation-bearing sibling reading governs a setting, non-participants acquire target-side directionality and the family''s epsilon rises sharply; this file''s near-zero extraction profile is valid only under archival governance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Which kernel reading governs a given study setting; locates the four-way normative-status dispute.').

omega_variable(
    corpus_readership_decay_risk,
    'Does declining technical literacy (Talmudic Aramaic, mishnaic terminology) eventually sever the preservation function, leaving ceremony without comprehension?',
    'Longitudinal reader-competency cohorts; annotation-dependence metrics in digital databases (share of users who can parse unmediated text without layered glosses).',
    'If the function severs while the forms persist, theater_ratio climbs toward the piton signature despite voluntariness, and the honest-relabeling defense of the rope claim fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(corpus_readership_decay_risk, empirical, 'Whether stewardship keeps pace with literacy decline in the transmitted corpus.').

omega_variable(
    voluntariness_boundary_soft_pressure,
    'Inside close-knit programs and communities, does social expectation constitute soft compulsion — do participants experience real sanction for opting out of assigned sacrificial tractates?',
    'Exit interviews and attrition studies measuring the perceived cost of skipping or dropping the sacrificial orders within enrolled populations.',
    'Material soft pressure would push suppression above the authored 0.04 and concentrate mild extraction on students, drifting seat-level classification toward tangled_rope at the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_boundary_soft_pressure, empirical, 'Where the voluntariness boundary sits under communal expectation.').

omega_variable(
    custody_framing_underdetermination,
    'The declared CS framing (fixed-text kernel, expertise-grounded custodianship, voluntary participation) is not the only coherent one: an alternative framing reads the academies as wielding the fixed corpus to justify bundled, fee-bearing programs — an extraction-adjacent authority. Signals guiding the declared choice: voluntary enrollment, open alternatives, no barrier to abstention.',
    'Compare settings where enrollment is unbundled (open courses, standalone seminars) against bundled ordination tracks; sharp divergence in extraction indicators by bundling strengthens the alternative framing.',
    'Adopting the alternative framing moves the story toward tangled_rope at the margin and re-reads gain_flow as capture rather than incidental receipt; the declared framing keeps it rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(custody_framing_underdetermination, conceptual, 'Framing under-determination in the custodial authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0, 0.09).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 20, 0.08).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 40, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 60, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 80, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t80, observed).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 100, 0.16).
narrative_ontology:measurement_basis(sacr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 0, 0.04).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 20, 0.05).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 40, 0.05).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 60, 0.06).
narrative_ontology:measurement_basis(sacr_be_t60, observed).
narrative_ontology:measurement(sacr_be_t80, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 80, 0.06).
narrative_ontology:measurement_basis(sacr_be_t80, observed).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 100, 0.07).
narrative_ontology:measurement_basis(sacr_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, information_standard).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'studying sacrifice law today' decomposes per the epsilon-invariance principle into four structurally distinct constraints. This file (archival_preservation) carries epsilon ~0.07, no victims, rope shape. study_as_performance makes the same activity obligation-bearing — non-participants acquire target-side d and epsilon rises. performance_only installs a permanent unfulfilled debt — the highest-extraction member. messianic_suspension prices readiness — latent obligation with maintenance costs. Edges from this reading: forecloses toward study_as_performance (study-with-normative-force and study-without-normative-force are direct negations; no single framework holds both) and toward performance_only (obligation-in-force-unperformable and obligation-terminated are direct negations); influences toward messianic_suspension (shared present-tense lapse, competing resumption modalities — successful archival custody erodes the trained-readiness substrate suspension depends on without logically eliminating it). Links route contamination propagation across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
