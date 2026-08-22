% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__performance_only, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Sacrificial Commandments as Suspended Husk Without Altar (Performance-Only Reading)
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This story authors the performance_only reading of the
 *   kodashim_commandment_status kernel: sacrifice laws are structurally
 *   contingent on the existence of the Temple and its altar. Under this
 *   reading, without the physical instrument, the commandment is suspended —
 *   a husk retained in the canonical corpus but without present operative
 *   force. This is distinct from the sibling reading study_as_performance,
 *   which holds that intellectual engagement with the laws itself constitutes
 *   fulfillment (no suspension at all — the kernel remains occupied), and
 *   from messianic_deferral, which holds the commandment is temporally paused
 *   but substantively intact, with study serving to maintain readiness. The
 *   performance_only reading's distinguishing claim is that neither of those
 *   framings rescues present operative force: the commandment genuinely
 *   lapses without the altar, full stop, pending literal restoration. This
 *   story's ε is authored for the performance_only reading's own account of
 *   what is happening when institutions nonetheless continue treating
 *   Kodashim study as a central curricular commitment despite the reading's
 *   own suspension verdict — the referent is the standing arrangement
 *   (continued heavy institutional investment in a self-declared-suspended
 *   commandment), not the reading's own preferred resolution.
 *
 * KEY AGENTS:
 *   - yeshiva_kodashim_faculty: institutional beneficiary — professional identity and prestige built on study of self-declared-suspended law
 *   - publishing_houses_of_talmudic_commentary: organized beneficiary — captive commentary market
 *   - students_diverted_from_applicable_halakha: powerless payer — formative years spent on inoperative material
 *   - communities_needing_applied_scholarly_labor: powerless payer — bear opportunity cost of talent misallocation
 *   - curriculum_setting_rabbinic_authorities: institutional agenda-setter — administers allocation, could redirect it
 *   - proponents_of_sibling_readings: excluded — hold study_as_performance or messianic_deferral positions not engaged with on this reading's own terms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.71).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.48).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.71).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Sacrificial Commandments as Suspended Husk Without Altar (Performance-Only Reading)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious_studies/halakhic_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, 'a5cce8e8-e080-4f15-a17f-d5c475c8bcfe').
narrative_ontology:cs_kernel_codification('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', fixed_text).
narrative_ontology:cs_authority_grounding('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', lineage).
narrative_ontology:cs_interpretation_layer_present('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe').
narrative_ontology:cs_reading_relation('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', foundational, commandment_requires_operative_instrumentality).
narrative_ontology:cs_axiom_status(commandment_requires_operative_instrumentality, holdable).
narrative_ontology:cs_axiom_grounding('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', commandment_requires_operative_instrumentality, conventional).
narrative_ontology:cs_axiom('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', foundational, study_without_instrument_does_not_constitute_fulfillment).
narrative_ontology:cs_axiom_status(study_without_instrument_does_not_constitute_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', study_without_instrument_does_not_constitute_fulfillment, conventional).
narrative_ontology:cs_reference_frame('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', temple_era_operative_sacrificial_cult).
narrative_ontology:cs_drift_state('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a5cce8e8-e080-4f15-a17f-d5c475c8bcfe', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, publishing_houses_of_talmudic_commentary).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, students_diverted_from_applicable_halakha).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communities_needing_applied_scholarly_labor).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, commandments_require_operative_instrumentality).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, performance_is_the_sole_fulfillment_mode).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Build careers, curricula, and institutional prestige teaching the tractates on sacrificial law. Under the performance-only reading these laws are formally suspended husks with no operative content, yet the faculty's professional identity, publication record, and teaching appointments are built entirely on sustained deep study of exactly this suspended material. They benefit from continued allocation of student time and institutional funding to this subject regardless of its practical status.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty, beneficiary,
    institutional, generational, identity_locked, national).

% Produce and sell commentaries, study guides, and reference works on Kodashim tractates to a captive scholarly market. Revenue depends on sustained demand for study of laws this reading holds to be inoperative pending Temple restoration. They can diversify into other tractates but the Kodashim market segment is a going concern they have no incentive to shrink.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, publishing_houses_of_talmudic_commentary, beneficiary,
    organized, generational, mobile, national).

% Spend years of yeshiva curriculum time mastering sacrificial procedure, altar geometry, and priestly qualification rules that this reading holds have no current operative application. Their scholarly labor and formative years are the resource being spent; curriculum requirements and communal expectation make skipping this material costly to their standing, even though the reading itself declares the underlying commandment suspended.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, students_diverted_from_applicable_halakha, payer,
    powerless, biographical, constrained, local).

% Face shortages of halakhic expertise in domains with active, operative application (family law, financial law, contemporary medical halakha) while a substantial share of elite scholarly talent is channeled into study of a commandment this reading classifies as a non-operative husk. They bear the opportunity cost of talent misallocation without any say in curricular priorities.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communities_needing_applied_scholarly_labor, payer,
    powerless, generational, trapped, national).

% Determine yeshiva curricula and the relative weight given to Kodashim versus operative tractates. They administer the study allocation and could redirect it, but face strong institutional and traditional pressure to maintain comprehensive Talmudic coverage regardless of operative status, since curricular completeness is itself a marker of institutional legitimacy.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, curriculum_setting_rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Hold that study itself constitutes fulfillment (study_as_performance) or that the commandment remains substantively live pending restoration (messianic_deferral). Within this performance-only reading's own framework, their positions are treated as alternative theological stances rather than engaged with on this reading's terms; they are not part of the internal accounting this reading gives for why study continues despite its own suspension verdict.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, proponents_of_sibling_readings, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__performance_only, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a stable, transmitted body of technical knowledge about sacrificial procedure across generations so that if the Temple were rebuilt, priestly and communal knowledge of correct practice would not have to be reconstructed from scratch.
% TRANSFER_FUNCTION: Moves scholarly attention, institutional funding, and formative educational years from students and communities toward faculty positions, publishing revenue, and institutional prestige built on a body of law this reading itself classifies as currently non-operative.
% ABSENT_VOICES: Proponents of study_as_performance and messianic_deferral would object that this reading undersells the value of continued engagement; they are structurally present in the broader tradition but excluded from this reading's own internal justification, which treats the commandment as genuinely suspended rather than as fulfilled through study or as substantively (not merely formally) awaiting restoration.
% DISAPPEARANCE_RATIONALE: If the performance-only reading's classification of Kodashim as a suspended husk were fully acted upon — curricula reallocated accordingly — yeshiva faculty positions tied to Kodashim specialization would contract, publishing revenue from Kodashim commentary would fall, and scholarly labor currently absorbed by this subject would become available for operative halakhic domains facing documented expertise shortages.
% FOUNDING_PROBLEM: The classical rabbinic system needed a principled way to explain why commandments tied to a destroyed Temple and absent altar are not simply violated every day by their non-performance — the performance-contingency doctrine solves this by holding that the commandment's operative force is suspended along with its instrument.
% FOUNDING_PROBLEM_CORROBORATION: Halakhic authorities within the performance-only tradition affirm the doctrine is a live, correct account of commandment structure. Independent observers outside the benefiting faculty and publishing interests — including comparative religion scholars and internal critics advocating curricular reform — attest that whatever the doctrine's theological correctness, its practical effect has been to entrench continued elite study investment in a domain the doctrine's own logic marks as currently non-operative, without a corresponding accounting of the diverted scholarly opportunity cost.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_commandment_status__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.71 by interval end because under this reading's own logic, the commandment has no present operative content, yet resources continue to flow toward its study as though it retained comparable weight to genuinely live commandments — the gap between declared status (suspended husk) and actual resource allocation (undiminished or increasing) is the extraction. Theater ratio is high and rising (0.50 to 0.78) because an increasing share of the activity is performative maintenance of a scholarly tradition rather than functional preparation for operative use — there is no altar, no near-term restoration project, and no mechanism by which continued study changes operative readiness in a verifiable way. Suppression is moderate (0.48): no one is coerced into Kodashim study, but strong institutional and communal expectation constrains real exit for students and faculty alike. Accessibility collapse is moderate (0.42) because alternative curricular allocations (more time on operative halakha) are conceptually available and are argued for by real reformist voices — the alternative is not fully suppressed, just institutionally disfavored. Resistance is moderate (0.55), reflecting active internal reform arguments and rising external comparative-religion commentary on the opportunity cost.
 *
 * PERSPECTIVAL GAP:
 *   From the yeshiva faculty and publishing seats, continued Kodashim study is unremarkable coordination — comprehensive Torah study naturally includes law the community holds sacred regardless of present operability, and no one is forced to specialize. From the seat of students diverted from applicable halakha and communities needing applied scholarly labor, the same allocation looks like a piton: an institutionally entrenched practice that persists by inertia and prestige rather than by any live coordination function, since the reading's own doctrine holds the underlying commandment inoperative. The engine's per-seat computation is expected to diverge sharply between the beneficiary/agenda-setter seats and the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva faculty and publishers are declared beneficiaries: they collect career capital and revenue from sustained Kodashim study without bearing the opportunity cost of the diverted labor, so their derived directionality sits near the beneficiary end. Students and under-served communities are declared victims: they supply the diverted scholarly years and forgo the applied expertise, so their derived directionality sits near the full-target end. Curriculum-setting authorities are agenda-setters with constrained exit of their own — they administer the allocation but operate under strong traditional pressure toward comprehensive coverage, which is why they are not simply coded as beneficiaries despite their formal control.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is precise: has the mandate for intensive Kodashim study outlived its founding function under this reading's own terms? The founding problem (explaining non-violation of a commandment whose instrument is destroyed) is fully solved by the suspension doctrine itself — no further ongoing study is structurally required to sustain that theological resolution. Yet the institutional apparatus built around Kodashim study (faculty lines, publishing markets, curricular hours) has generational momentum independent of that founding function. Classifying this as piton rather than snare matters: no single concentrated beneficiary is capturing extraction through coercive enforcement — enrollment and study allocation are not compelled by force — but a genuine cost-asymmetry exists (curriculum-setters could redirect hours; the diffuse cost to underserved communities is real but no single administrator bears it enough to force change) with a documented theater component (performative comprehensive coverage sustaining institutional identity past the point the reading's own doctrine says it need).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the performance_only reading (suspension as genuine lapse), the study_as_performance reading (study itself fulfills), or the messianic_deferral reading (substantive pause, not lapse) the structurally correct account of what continued Kodashim study accomplishes?',
    'No empirical resolution is available in principle — this is a doctrinal/theological dispute about the nature of commandment-fulfillment that different halakhic authorities resolve differently within their own traditions. Resolution would require either doctrinal consensus (historically absent across the relevant traditions) or literal Temple restoration rendering the dispute moot.',
    'If study_as_performance is the correct reading, the extraction this story documents disappears entirely — study is the commandment''s fulfillment, not a cost imposed while awaiting a separate operative trigger. If messianic_deferral is correct, the extraction is substantially lower — study serves a genuine readiness-maintenance function with real (if deferred) value. Only under performance_only does the full extraction profile authored here obtain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which of three live kernel readings correctly characterizes ongoing Kodashim study is theologically contested and not resolvable by external evidence.').

omega_variable(
    opportunity_cost_measurement,
    'How much documented expertise shortage in operative halakhic domains is actually attributable to talent diverted toward Kodashim study, versus other causes (demographic decline in religious scholarship generally, competing secular career paths, etc.)?',
    'Comparative study of yeshiva curricular allocation versus documented halakhic expertise gaps in family law, financial law, and medical halakha, controlling for other factors affecting scholarly labor supply.',
    'If the shortage is substantially attributable to Kodashim allocation, the victim classification and extraction magnitude are well-supported. If the shortage has other primary causes, the extractiveness score authored here (0.71) may overstate the actual cost imposed on communities_needing_applied_scholarly_labor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_measurement, empirical, 'Whether the diverted-labor extraction claim is causally well-founded or overstated relative to other explanations for expertise shortages.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.5).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.58).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.64).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__performance_only, theater_ratio, 60, 0.7).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__performance_only, theater_ratio, 80, 0.75).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.78).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__performance_only, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__performance_only, base_extractiveness, 80, 0.68).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.71).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__performance_only, 0.1).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_commandment_status kernel. performance_only (this story) authors high extraction because it holds the commandment genuinely suspended while institutional study investment continues undiminished. study_as_performance authors near-zero extraction because it holds study itself IS the fulfillment — no gap between doctrine and practice exists under that reading. messianic_deferral authors intermediate extraction because it holds the pause is substantive rather than a lapse, giving continued study a genuine (if deferred-value) readiness function. All three share the same underlying kernel (Temple-dependent sacrificial commandments) but diverge sharply in ε because they diverge on what continued study accomplishes doctrinally — per the ε-invariance principle, this required three separate constraint stories rather than one story with a variable interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
