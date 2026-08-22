% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodshim Study Mandate under the Messianic Deferral Reading
 *   domain: religious/halakhic/commitment-system
 *
 * SUMMARY:
 *   This story authors ONE reading — messianic_deferral — of the contested
 *   kernel kodashim_commandment_status: the standing arrangement by which the
 *   sacrificial (kodashim) commandments, unperformable since the destruction
 *   of the Temple, are held to remain eternally binding in suspension, with
 *   elite study of the sacrificial corpus mandated as maintenance of
 *   readiness for messianic restoration. The arrangement under contest is the
 *   study-readiness mandate itself: curricular allocation, institutional
 *   prestige, and communal funding flow toward sustaining commandments no one
 *   may presently perform, on the strength of a restoration whose arrival no
 *   one can date. Extraction in this reading is opportunity cost —
 *   present-generation study capacity and communal attention subordinated to
 *   a future contingency — assessed by this reading's own lights, which
 *   accept that subordination as the price of fidelity. Sibling readings
 *   instantiate different constraints with different epsilon and victim sets
 *   and are authored as separate linked stories; they are not averaged into
 *   this one. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine transmission-and-readiness coordination carrying
 *   asymmetric opportunity cost), while the metrics are authored
 *   independently from the arrangement's observable operation. KEY AGENTS (by
 *   structural relationship): - rabbinic_academy_leadership: Agenda-setting
 *   beneficiary (institutional/identity_locked) — sets curriculum, collects
 *   continuity, funding, and authority - talmudic_students: Primary target
 *   (powerless/constrained) — bears the opportunity cost - communal_laity:
 *   Dual-positioned funder (organized/constrained) — pays and is
 *   subordinated; collects identity continuity -
 *   messianic_restoration_movements: Secondary beneficiary (organized/mobile)
 *   — draws mobilization energy from the deferral - temple_activists:
 *   Excluded challenger (organized/trapped) — rejects deferral, barred from
 *   curriculum-setting - practical_halakhic_authorities: Excluded voice
 *   (institutional/constrained) — would reprioritize applicable law -
 *   academic_jewish_studies_scholars: Analytical observer
 *   (institutional/analytical) — attests the structure from outside the
 *   commitment
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.48).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.34).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.34).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.22).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodshim Study Mandate under the Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment-system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, 'a30ff2ef-9e16-4051-8f77-6e91beea0030').
narrative_ontology:cs_kernel_codification('a30ff2ef-9e16-4051-8f77-6e91beea0030', fixed_text).
narrative_ontology:cs_authority_grounding('a30ff2ef-9e16-4051-8f77-6e91beea0030', lineage).
narrative_ontology:cs_interpretation_layer_present('a30ff2ef-9e16-4051-8f77-6e91beea0030').
narrative_ontology:cs_reading_relation('a30ff2ef-9e16-4051-8f77-6e91beea0030', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('a30ff2ef-9e16-4051-8f77-6e91beea0030', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_axiom('a30ff2ef-9e16-4051-8f77-6e91beea0030', foundational, commandment_eternally_binding).
narrative_ontology:cs_axiom_status(commandment_eternally_binding, holdable).
narrative_ontology:cs_axiom_grounding('a30ff2ef-9e16-4051-8f77-6e91beea0030', commandment_eternally_binding, deontological).
narrative_ontology:cs_axiom('a30ff2ef-9e16-4051-8f77-6e91beea0030', foundational, study_prepares_but_does_not_discharge).
narrative_ontology:cs_axiom_status(study_prepares_but_does_not_discharge, holdable).
narrative_ontology:cs_axiom_grounding('a30ff2ef-9e16-4051-8f77-6e91beea0030', study_prepares_but_does_not_discharge, deontological).
narrative_ontology:cs_axiom('a30ff2ef-9e16-4051-8f77-6e91beea0030', secondary, restoration_preparedness_outweighs_present_optimization).
narrative_ontology:cs_axiom_status(restoration_preparedness_outweighs_present_optimization, holdable).
narrative_ontology:cs_axiom_grounding('a30ff2ef-9e16-4051-8f77-6e91beea0030', restoration_preparedness_outweighs_present_optimization, instrumental).
narrative_ontology:cs_reference_frame('a30ff2ef-9e16-4051-8f77-6e91beea0030', suspended_pending_messianic_restoration).
narrative_ontology:cs_drift_state('a30ff2ef-9e16-4051-8f77-6e91beea0030', contemporary_study_culture, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a30ff2ef-9e16-4051-8f77-6e91beea0030', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_academy_leadership).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_restoration_movements).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, talmudic_students).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, communal_laity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, communal_laity).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, commandment_eternality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Heads the yeshivot and ordination institutions that set the curriculum weighting the sacrificial tractates and administer the interpretive tradition that transmits them. Collects the enrollment, funding, and authority flows that depend on the mandate's continuation. Their standing is constituted by continuity with the transmitted corpus; stepping outside the deferral frame would mean dismantling the role that defines them.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_academy_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Spend the formative biographical years mastering sacrificial-law tractates they will never apply. Entry is subsidized and socially expected in their communities; departure carries layered costs — family expectation, stipend loss, marriage-market positioning, and loss of communal standing. They bear the opportunity cost the mandate runs on.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, talmudic_students, payer,
    powerless, biographical, constrained, global).

% Fund the academies and send them their children; receive identity continuity and the assurance that nothing of the commandment has been lost. Their day-to-day practical questions route to applied-law channels while elite prestige concentrates on the suspended corpus. They have voice in funding but not in curricular weighting.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, communal_laity, payer,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, communal_laity, beneficiary).

% Organize around the expectation the deferral keeps alive. The suspended-but-binding framing supplies their mobilizing premise — a commandment awaiting them — and they recruit and raise funds on it. They do not administer the study mandate and could redirect their organizing elsewhere if the frame changed.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_restoration_movements, beneficiary,
    organized, generational, mobile, global).

% Reject the deferral from inside the tradition: they press for concrete restoration steps now and read study-as-readiness as a way of deferring indefinitely while appearing faithful. They are excluded from curriculum-setting and institutional governance, and within the current framework they cannot perform the commandments either.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, temple_activists, excluded,
    organized, immediate, trapped, regional).

% Adjudicate the community's living law — Sabbath, finance, family, medicine. They argue elite capacity belongs with applicable domains and watch prestige and talent concentrate on the suspended corpus. They are inside the tradition but outside the curricular conversation that allocates it.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, practical_halakhic_authorities, excluded,
    institutional, generational, constrained, global).

% Study the arrangement from outside the commitment: its history, sociology, and economics. They attest the founding crisis and trace the mandate's institutional growth, and hold no stake in its continuation or repeal.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, academic_jewish_studies_scholars, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__messianic_deferral, rabbinic_academy_leadership).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__messianic_deferral, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves uninterrupted transmission of the sacrificial corpus and keeps the commandment-community oriented toward eventual restoration — solving the post-destruction problem of how a performance-centered commandment survives the loss of its performance site without conceding abrogation.
% TRANSFER_FUNCTION: Moves study hours, curricular prestige, tuition, and communal funding toward maintenance of a non-performable corpus; moves interpretive authority and institutional continuity to the academy class; moves present-generation capacity away from immediately applicable halakhic and communal needs.
% ABSENT_VOICES: Temple-restoration activists would object that deferral is indefinite postponement dressed as fidelity and that practical preparation steps are being refused; they sit outside curriculum-setting bodies. Practical-halakhic authorities would object that elite capacity is misallocated away from living law; they hold no seat in the Kodshim-prestige conversation. Secular and marginal community members would object that the deferral renders the center ritually vacant; they are outside the funding and governance loop entirely.
% DISAPPEARANCE_RATIONALE: If the study-readiness mandate vanished overnight, the sacrificial corpus would lose its trained expositors within a generation or two, the restoration doctrine would lose its operational arm (nothing would be kept ready), academy curricula would reorganize around applicable law, and messianic movements would lose a mobilizing structure. The world rearranges because identifiable institutions, careers, and movements are organized around the mandate.
% FOUNDING_PROBLEM: After the destruction of the Second Temple, the central sacrificial commandments became unperformable; the community needed a way to preserve their binding status and its own covenantal continuity without a functioning altar.
% FOUNDING_PROBLEM_CORROBORATION: The founding crisis itself is corroborated outside the beneficiary set: the post-70 Yavneh-era restructuring recorded in early rabbinic sources, Maimonides' codification of the restoration expectation (written before the modern academy expansion that now benefits from the mandate), and academic Jewish studies historiography of post-destruction Judaism. Dissenting internal readings corroborate the crisis while denying this reading's solution. No external source attests that the founding problem REMAINS live — that status is asserted only from within the benefiting tradition and is flagged as such.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the transfer is real but paid in opportunity cost rather than wealth — years of elite study capacity, curricular prestige, and communal funding directed to a non-performable corpus, with present-applicable needs explicitly subordinated. Suppression (0.34) is normative and curricular rather than coercive: the mandate is enforced through curriculum control, prestige hierarchy, and communal expectation; exit exists but carries layered social cost. Theater ratio (0.26) is low-to-moderate because within this reading study is functional (readiness maintenance), though a growing recitation-without-mastery stratum contributes ritualized activity. Accessibility collapse (0.42): alternatives persist — applied-halakha tracks, other communal roles — but inside committed communities the Kodshim track carries prestige that narrows exercised choice. Resistance (0.22): the classic internal challenge ('why study what we cannot do') recurs and is absorbed; no organized opposition exists inside the mandate's institutions. The three metric series share one eight-point grid (interval 0-77, approximating 1948-2025) so the engine samples aligned rows; trajectories are monotonic ratchets, not cycles — the arrangement lacks crisis-reform oscillation in this interval, and the slow accumulation is itself the finding. Suppression is authored as a raw structural property and is not scaled by power or scope; extractiveness is scaled by the engine from directionality and the global scope of the academy system. Dispersed students could in principle coordinate a curricular objection, but placement across many institutions and dependence on academy certification keep coalition formation weak.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the academy-leadership seat the arrangement is fidelity: the commandment is eternal, study is the only available service, and the curriculum is continuity itself — beneficiary-side directionality yields low effective extraction. From the student seat the same structure operates as enforced deferral: biographical years priced against a horizon no one controls — target-side directionality amplifies effective extraction. The laity seat sits near symmetric: funded continuity received, practical priorities surrendered. The excluded seats register the arrangement as postponement (temple activists) and misallocation (practical-halakhic authorities) respectively; their exclusion is commentary-grade and drives no override.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows: talmudic_students (victim, powerless, constrained exit) derive near the full-target end — they pay the opportunity cost and cannot arbitrage it. rabbinic_academy_leadership (beneficiary and agenda-setter, institutional, identity_locked) derive near the full-beneficiary end — the mandate constitutes their authority, so the lock reinforces the subsidy side. messianic_restoration_movements (beneficiary, organized, mobile) sit low-d: they collect mobilization energy from the deferral without administering it. communal_laity carry dual roles (payer primary, beneficiary secondary) and derive near symmetric — funding and subordination out, identity continuity in. No directionality overrides are authored: the beneficiary/victim plus exit data already place each seat correctly, and the dual-role laity is handled by role declaration rather than override.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two symmetrical mislabels. Read as rope (the tradition's self-description — faithful preservation of an eternal commandment), the asymmetric opportunity cost and the subordination of present needs disappear. Read as snare (the critic's description — empty ritualism extracting tuition and years under an undatable promise), the genuine transmission function vanishes: the corpus, the analytic method, and the community's covenantal continuity are real coordination goods this arrangement has carried. Tangled_rope holds both halves. On mandatrophy: the founding problem (post-destruction survival of a performance-centered commandment) is live within this reading — the suspension persists and restoration has not arrived — so no mandatrophy_resolved declaration is authored. The lifecycle risk runs the other way: if the restoration horizon proves indefinitely extendable (see omega restoration_horizon_verifiability), the rising theater_ratio series marks the drift path toward piton, and the rising base_extractiveness series supplies the accumulation signature for abductive investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story authors one reading (messianic_deferral) of the kodashim_commandment_status kernel; which structural facts would change under the sibling readings?',
    'Not resolvable internally — reading adoption is a commitment act. Cross-reading comparison via the linked family stories (study_as_performance, performance_only) exposes which metrics move with the reading rather than with the referent.',
    'Under study_as_performance the victim set shifts from opportunity-cost bearers to those denied present fulfillment; under performance_only the arrangement loses its binding premise entirely and epsilon collapses toward archival curiosity. Beneficiary/victim structure and effective extraction are reading-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame indexicality: epsilon and victim set are properties of this reading, not of the kernel.').

omega_variable(
    restoration_horizon_verifiability,
    'The deferral''s justification rests on a restoration event whose arrival no observation can date; does an indefinitely extendable horizon convert readiness-maintenance into unfalsifiable deferral?',
    'No in-principle empirical resolution; monitor behavioral proxies — whether the tradition treats the horizon as action-guiding (practical preparation, site contingencies) or purely textual.',
    'If the horizon functions as indefinitely extendable, the coordination claim thins toward inertial maintenance (piton drift, rising theater_ratio); if action-guiding, the tangled_rope coordination half strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_horizon_verifiability, conceptual, 'Whether the undatable restoration horizon sustains or dissolves the coordination justification.').

omega_variable(
    kodshim_curriculum_share,
    'What fraction of elite study capacity is actually allocated to the non-performable sacrificial corpus, and what would the marginal yield of reallocating it to applicable law be?',
    'Curriculum-hour audits across yeshivot and kollelim; comparative outcome tracking of parallel cohorts on applied-halakha tracks.',
    'A large audited share substantiates the victim declaration and the 0.48 epsilon; a small share collapses the extraction claim toward rope-level coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kodshim_curriculum_share, empirical, 'Magnitude of the opportunity-cost transfer the mandate runs on.').

omega_variable(
    student_exit_cost_composition,
    'Is student retention in the Kodshim track driven by structural dependency (stipends, communal expectation, marriage-market positioning) or by internalized identity fusion with the study role?',
    'Post-exit trajectory analysis of students who left the track: if deference to the mandate persists after structural ties are severed, the internalized component is substantial.',
    'Internalized retention raises effective suppression above the structural 0.34 measure; purely structural retention locates enforcement in institutional machinery that reform could reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(student_exit_cost_composition, empirical, 'Structural versus internalized composition of the mandate''s hold on its payers.').

omega_variable(
    readiness_actionability,
    'Would the studied material actually support performance upon restoration — does study produce procedurally usable readiness, or primarily textual fluency detached from sacrificial practice?',
    'Compare study outputs (analytic competency, sugya mastery) against the procedural demands a reconstructed service would impose; expert elicitation spanning halakhic and practical-technical domains.',
    'Genuinely actionable readiness validates the coordination function; nominal readiness shifts weight toward theater and weakens the tangled_rope gate from the coordination side.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(readiness_actionability, empirical, 'Whether the readiness the mandate produces is real or nominal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 77).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(koda_tr_t0, observed).
narrative_ontology:measurement(koda_tr_t11, kodashim_commandment_status__messianic_deferral, theater_ratio, 11, 0.16).
narrative_ontology:measurement_basis(koda_tr_t11, observed).
narrative_ontology:measurement(koda_tr_t22, kodashim_commandment_status__messianic_deferral, theater_ratio, 22, 0.18).
narrative_ontology:measurement_basis(koda_tr_t22, observed).
narrative_ontology:measurement(koda_tr_t33, kodashim_commandment_status__messianic_deferral, theater_ratio, 33, 0.2).
narrative_ontology:measurement_basis(koda_tr_t33, observed).
narrative_ontology:measurement(koda_tr_t44, kodashim_commandment_status__messianic_deferral, theater_ratio, 44, 0.22).
narrative_ontology:measurement_basis(koda_tr_t44, observed).
narrative_ontology:measurement(koda_tr_t55, kodashim_commandment_status__messianic_deferral, theater_ratio, 55, 0.23).
narrative_ontology:measurement_basis(koda_tr_t55, observed).
narrative_ontology:measurement(koda_tr_t66, kodashim_commandment_status__messianic_deferral, theater_ratio, 66, 0.25).
narrative_ontology:measurement_basis(koda_tr_t66, observed).
narrative_ontology:measurement(koda_tr_t77, kodashim_commandment_status__messianic_deferral, theater_ratio, 77, 0.26).
narrative_ontology:measurement_basis(koda_tr_t77, observed).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.36).
narrative_ontology:measurement_basis(koda_be_t0, observed).
narrative_ontology:measurement(koda_be_t11, kodashim_commandment_status__messianic_deferral, base_extractiveness, 11, 0.39).
narrative_ontology:measurement_basis(koda_be_t11, observed).
narrative_ontology:measurement(koda_be_t22, kodashim_commandment_status__messianic_deferral, base_extractiveness, 22, 0.41).
narrative_ontology:measurement_basis(koda_be_t22, observed).
narrative_ontology:measurement(koda_be_t33, kodashim_commandment_status__messianic_deferral, base_extractiveness, 33, 0.43).
narrative_ontology:measurement_basis(koda_be_t33, observed).
narrative_ontology:measurement(koda_be_t44, kodashim_commandment_status__messianic_deferral, base_extractiveness, 44, 0.45).
narrative_ontology:measurement_basis(koda_be_t44, observed).
narrative_ontology:measurement(koda_be_t55, kodashim_commandment_status__messianic_deferral, base_extractiveness, 55, 0.46).
narrative_ontology:measurement_basis(koda_be_t55, observed).
narrative_ontology:measurement(koda_be_t66, kodashim_commandment_status__messianic_deferral, base_extractiveness, 66, 0.47).
narrative_ontology:measurement_basis(koda_be_t66, observed).
narrative_ontology:measurement(koda_be_t77, kodashim_commandment_status__messianic_deferral, base_extractiveness, 77, 0.48).
narrative_ontology:measurement_basis(koda_be_t77, observed).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(koda_su_t0, observed).
narrative_ontology:measurement(koda_su_t11, kodashim_commandment_status__messianic_deferral, suppression_requirement, 11, 0.25).
narrative_ontology:measurement_basis(koda_su_t11, observed).
narrative_ontology:measurement(koda_su_t22, kodashim_commandment_status__messianic_deferral, suppression_requirement, 22, 0.27).
narrative_ontology:measurement_basis(koda_su_t22, observed).
narrative_ontology:measurement(koda_su_t33, kodashim_commandment_status__messianic_deferral, suppression_requirement, 33, 0.29).
narrative_ontology:measurement_basis(koda_su_t33, observed).
narrative_ontology:measurement(koda_su_t44, kodashim_commandment_status__messianic_deferral, suppression_requirement, 44, 0.31).
narrative_ontology:measurement_basis(koda_su_t44, observed).
narrative_ontology:measurement(koda_su_t55, kodashim_commandment_status__messianic_deferral, suppression_requirement, 55, 0.32).
narrative_ontology:measurement_basis(koda_su_t55, observed).
narrative_ontology:measurement(koda_su_t66, kodashim_commandment_status__messianic_deferral, suppression_requirement, 66, 0.33).
narrative_ontology:measurement_basis(koda_su_t66, observed).
narrative_ontology:measurement(koda_su_t77, kodashim_commandment_status__messianic_deferral, suppression_requirement, 77, 0.34).
narrative_ontology:measurement_basis(koda_su_t77, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).

% DUAL FORMULATION NOTE:
% Decomposition per the epsilon-invariance principle: 'the status of the kodashim commandments' is one colloquial label covering three structurally distinct arrangements with distinct epsilon, beneficiary/victim sets, and failure modes. This story (messianic_deferral) authors the suspended-pending-restoration arrangement at moderate epsilon (0.48, opportunity-cost extraction with present-generation needs subordinated). The commandment-eternality premise is what the siblings contest; family links run through network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
