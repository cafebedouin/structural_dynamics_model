% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: kodashim_commandment_status__performance_only
 *   human_readable: Kodashim Commandment Status — Performance-Contingent Reading (Sacrifice Law Suspended Absent Altar)
 *   domain: religious/halakhic/institutional
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only reading of the
 *   kodashim_commandment_status kernel: the classical halakhic position that
 *   commandments contingent on Temple service (korbanot, the sacrificial
 *   order) are structurally suspended once the altar is destroyed, becoming a
 *   husk with no operative force until restoration. This is distinct from the
 *   sibling readings — messianic_deferral (suspended-but-not-obsolete, study
 *   maintains readiness) and study_as_performance (study itself IS
 *   fulfillment) — which are separate constraint stories, not alternative
 *   measurements of this one. Under this reading, continued institutional
 *   investment in Kodashim study without acknowledging its performative
 *   suspension increasingly functions as scholarly theater: real transmission
 *   value persists, but the theater_ratio rises as curricular time,
 *   publication volume, and prestige allocation grow disproportionate to any
 *   operative legal function.
 *
 * KEY AGENTS:
 *   - yeshiva_kodashim_faculty: institutional beneficiary — career and funding depend on sustained study demand
 *   - students_diverted_from_applicable_halakha: constrained payer — biographical years spent on inoperative law
 *   - communities_underserved_by_redirected_scholarly_labor: trapped payer — diffuse cost of scholarly attention misallocation
 *   - publishing_houses_of_talmudic_commentary: organized beneficiary — captive commentary market
 *   - halakhic_decisors: analytical observer — adjudicate between readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__performance_only, 0.68).
domain_priors:suppression_score(kodashim_commandment_status__performance_only, 0.42).
domain_priors:theater_ratio(kodashim_commandment_status__performance_only, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(kodashim_commandment_status__performance_only, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__performance_only, piton).
narrative_ontology:human_readable(kodashim_commandment_status__performance_only, "Kodashim Commandment Status — Performance-Contingent Reading (Sacrifice Law Suspended Absent Altar)").
narrative_ontology:topic_domain(kodashim_commandment_status__performance_only, "religious/halakhic/institutional").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__performance_only, '9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1').
narrative_ontology:cs_kernel_codification('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', fixed_text).
narrative_ontology:cs_authority_grounding('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', lineage).
narrative_ontology:cs_interpretation_layer_present('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1').
narrative_ontology:cs_reading_relation('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', kodashim_commandment_status__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', foundational, performance_contingent_obligation_voids_absent_altar).
narrative_ontology:cs_axiom_status(performance_contingent_obligation_voids_absent_altar, holdable).
narrative_ontology:cs_axiom_grounding('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', performance_contingent_obligation_voids_absent_altar, conventional).
narrative_ontology:cs_axiom('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', secondary, study_without_operative_performance_is_not_fulfillment).
narrative_ontology:cs_axiom_status(study_without_operative_performance_is_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', study_without_operative_performance_is_not_fulfillment, deontological).
narrative_ontology:cs_reference_frame('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', operative_temple_cult_framework).
narrative_ontology:cs_drift_state('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', post_destruction_rabbinic_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('9144c3d1-2cef-4fa6-8d92-fd9932ed0dc1', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__performance_only, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__performance_only, publishing_houses_of_talmudic_commentary).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, students_diverted_from_applicable_halakha).
narrative_ontology:constraint_victim(kodashim_commandment_status__performance_only, communities_underserved_by_redirected_scholarly_labor).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, performative_contingency_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__performance_only, temple_dependent_commandment_class).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach and publish on tractates Zevachim, Menachot, and related sacrificial law as a core curricular pillar. Their professional standing, publication record, and institutional funding depend on treating this body of law as a live, rigorous subject of study, even though under this reading the underlying commandment is inoperative absent the altar. They set the curriculum weighting that channels student years into this material.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty, beneficiary,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__performance_only, yeshiva_kodashim_faculty, agenda_setter).

% Spend multiple years of intensive study mastering sacrificial law under curricular expectation, time that under this reading is not fulfilling an operative commandment but studying a suspended one. Their exit is constrained by ordination requirements, communal expectation, and the fact that mastery of Kodashim carries prestige value independent of its practical applicability.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, students_diverted_from_applicable_halakha, payer,
    powerless, biographical, constrained, national).

% Rely on the same scholarly class for applicable halakhic guidance — family law, financial law, contemporary ethical questions — but a share of available scholarly attention and institutional prestige is allocated instead to a body of law this reading holds inoperative. They have no direct voice in curricular allocation and bear the diffuse cost of under-resourced applicable rulings.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, communities_underserved_by_redirected_scholarly_labor, payer,
    powerless, generational, trapped, national).

% Produce and sell commentary volumes, study guides, and reference works on Kodashim tractates to a captive scholarly market. Revenue depends on sustained study demand regardless of whether the underlying commandment is live or husk.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, publishing_houses_of_talmudic_commentary, beneficiary,
    organized, generational, arbitrage, national).

% Hold that the commandment is temporally suspended, not obsolete, and that study maintains readiness for restoration — they would object to this reading's husk framing as understating the commandment's ongoing normative force, but this constraint story is authored from the performance_only seat and does not adjudicate their claim.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, messianic_deferral_advocates, excluded,
    organized, civilizational, identity_locked, national).

% Hold that studying sacrifice law itself fulfills the commandment, making continued study non-diversionary by definition — they would reject this reading's premise that the commandment is presently inoperative, but are not parties within this constraint's own framework.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, study_as_performance_advocates, excluded,
    organized, civilizational, identity_locked, national).

% Rule on which classes of commandment remain operative without a functioning Temple and altar; they adjudicate disputes between the performance_only, study_as_performance, and messianic_deferral readings without being direct beneficiaries or payers of the curricular allocation question.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__performance_only, halakhic_decisors, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves textual and legal continuity of the sacrificial system across generations by maintaining a scholarly community capable of transmitting, interpreting, and correctly applying Kodashim law should the Temple ever be rebuilt — a genuine transmission-preservation function distinct from present performative fulfillment.
% TRANSFER_FUNCTION: Moves scholarly years, institutional funding, publication attention, and communal prestige toward mastery of a commandment class this reading holds inoperative, and away from applicable halakhic domains (civil law, family law, contemporary ethics) that could use the same scholarly labor.
% ABSENT_VOICES: Communities awaiting rulings on applicable law are not in the curricular-allocation conversation; study_as_performance and messianic_deferral advocates are excluded from this reading's own framework by definition, since admitting either premise would dissolve the husk classification.
% DISAPPEARANCE_RATIONALE: If the performance_only reading's classification were universally accepted and curricula reallocated accordingly, yeshiva institutional structure and publishing revenue tied to Kodashim study would visibly rearrange; but many practitioners already hold the study_as_performance or messianic_deferral view in practice regardless of formal doctrine, so the world may not rearrange as much as the reading implies — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: The commandment structure was built to regulate an operative Temple cult: what may be sacrificed, by whom, under what conditions, at what altar. The founding problem was correct performance of an active ritual system.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Second Temple period and comparative religion scholars outside the yeshiva system attest that the operative cultic problem ended with the Temple's destruction in 70 CE; some halakhic authorities within the tradition itself (maintaining the performance_only reading) corroborate this from inside, holding the commandment formally suspended — but the corroboration from outside the beneficiary set (academic historians, non-beneficiary halakhic minimalists) is the more decisive attestation, since faculty who benefit from continued study investment have institutional incentive to blur the dead/live distinction.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__performance_only, contested).
narrative_ontology:founding_problem_status(kodashim_commandment_status__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_commandment_status__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__performance_only, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises from 0.35 to 0.68 across the interval, tracking the accumulation of institutional infrastructure (curricula, publishing houses, endowed chairs) built around Kodashim study long after the founding cultic problem became dead by this reading's own lights. Theater ratio rises even faster (0.40 to 0.71) because an increasing share of the activity is performative maintenance of scholarly identity and institutional prestige rather than either operative legal application or genuine future-readiness preparation. Accessibility collapse is moderate (0.4) — study_as_performance and messianic_deferral remain live alternative framings within the tradition, so alternatives to the husk reading have not collapsed; resistance is moderate (0.55) because many practitioners resist the husk classification precisely because it devalues their ongoing scholarly investment.
 *
 * DIRECTIONALITY LOGIC:
 *   Yeshiva faculty and commentary publishers sit near the beneficiary end: they collect prestige, funding, and revenue from sustained study demand regardless of the commandment's operative status. Students and underserved communities sit near the target end: students pay in diverted years under a constrained, identity-adjacent exit (leaving Kodashim study track carries reputational cost within observant scholarly communities), and underserved communities pay diffusely and are trapped in the sense that they have no lever over curricular allocation at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy question here is sharp: the founding problem (operative Temple sacrifice) is dead by this reading's own premise, yet the institutional mandate for intensive Kodashim study persists at growing scale. Classifying this as piton rather than snare matters — no single concentrated beneficiary is capturing extraction the way a snare's payer-to-beneficiary transfer would require; instead, diffuse institutional inertia (curricular tradition, prestige structures, publishing infrastructure) sustains the arrangement without any party needing to actively defend it against exit. Faculty benefit incidentally from an inertial structure they did not design to extract; classifying as tangled_rope would overstate the coordination function relative to the accumulating theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    husk_vs_dormant_distinction,
    'Is a Temple-contingent commandment, once the Temple is destroyed, genuinely voided (husk, no residual normative content) or merely dormant (residual normative content that resumes on restoration, per messianic_deferral)?',
    'This is fundamentally a question of halakhic legal theory rather than empirical fact: it would be resolved (within a given tradition) by an authoritative ruling on the category of commandments ''dependent on the land/Temple'' (teluyot ba''aretz) and whether such dependency voids or merely suspends obligation. No external empirical test resolves it; it is a live doctrinal dispute.',
    'If dormant rather than voided is the correct within-tradition reading, the extractiveness computed here is overstated — continued study would be legitimate readiness-maintenance (as in messianic_deferral) rather than diversion from an actually-dead function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_vs_dormant_distinction, conceptual, 'Whether Temple-contingent law is voided (husk) or merely dormant pending restoration — the kernel''s central interpretive fork.').

omega_variable(
    coupling_between_readings_and_resource_allocation,
    'Does the choice of reading (performance_only vs. study_as_performance vs. messianic_deferral) actually drive curricular and funding allocation, or do institutions maintain Kodashim study at similar intensity regardless of which doctrinal reading their faculty formally hold?',
    'Comparative study of curricular hours and funding allocation across institutions/movements that explicitly hold different readings of the kernel (e.g., movements with strong messianic_deferral commitments vs. those with weaker Temple-restoration expectation) would show whether reading choice is causally load-bearing or epiphenomenal to institutional behavior.',
    'If institutional behavior is invariant across readings, the performance_only classification here is descriptively accurate for actual practice but the doctrinal dispute among readings is largely rhetorical rather than resource-allocating — weakening the claim that THIS reading''s endorsement would change real extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coupling_between_readings_and_resource_allocation, empirical, 'Whether doctrinal reading choice causally drives resource allocation or merely rationalizes allocation set by other institutional forces.').

omega_variable(
    faculty_self_interest_in_reading_selection,
    'To what extent does yeshiva faculty''s professional and institutional self-interest bias the reading they adopt, given that study_as_performance and messianic_deferral both justify continued investment while performance_only (this reading) implies their core subject matter is inoperative?',
    'Track historical shifts in which reading gained prominence in different periods/communities and correlate with institutional funding pressures or external legitimacy challenges to the study tradition.',
    'If reading adoption correlates strongly with institutional self-interest, that supports treating the sibling readings (which this story''s beneficiaries would prefer) as partly motivated reasoning rather than independent doctrinal conclusions — reinforcing rather than undermining this reading''s structural analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(faculty_self_interest_in_reading_selection, conceptual, 'Whether reading selection among faculty tracks self-interest in continued study investment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__performance_only, theater_ratio, 0, 0.4).
narrative_ontology:measurement(koda_tr_t20, kodashim_commandment_status__performance_only, theater_ratio, 20, 0.48).
narrative_ontology:measurement(koda_tr_t40, kodashim_commandment_status__performance_only, theater_ratio, 40, 0.55).
narrative_ontology:measurement(koda_tr_t60, kodashim_commandment_status__performance_only, theater_ratio, 60, 0.61).
narrative_ontology:measurement(koda_tr_t80, kodashim_commandment_status__performance_only, theater_ratio, 80, 0.67).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__performance_only, theater_ratio, 100, 0.71).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__performance_only, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(koda_be_t20, kodashim_commandment_status__performance_only, base_extractiveness, 20, 0.44).
narrative_ontology:measurement(koda_be_t40, kodashim_commandment_status__performance_only, base_extractiveness, 40, 0.52).
narrative_ontology:measurement(koda_be_t60, kodashim_commandment_status__performance_only, base_extractiveness, 60, 0.59).
narrative_ontology:measurement(koda_be_t80, kodashim_commandment_status__performance_only, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__performance_only, base_extractiveness, 100, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__performance_only, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_commandment_status kernel, decomposed per the ε-invariance principle because the natural-language label 'status of sacrifice commandments' covers structurally distinct claims with different ε values: performance_only (this story, high extractiveness — continued study is diversion from applicable law), study_as_performance (near-mountain/rope — study itself is the fulfillment, no diversion), and messianic_deferral (low-moderate extractiveness — study is legitimate future-readiness preparation, not misallocation). All three share the same underlying kernel text (Temple-dependent commandment status) but diverge on whether present study constitutes fulfillment, preparation, or waste.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
