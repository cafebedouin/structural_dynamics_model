% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__substitution_archive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__substitution_archive, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: kodashim_corpus__substitution_archive
 *   human_readable: Kodashim as Substitution Archive: Prayer/Study Superseding Sacrifice
 *   domain: religious_studies/rabbinic_judaism/commitment_system_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the Kodashim kernel: the
 *   substitution-archive reading, in which prayer and Torah study are held to
 *   have replaced sacrifice outright, and the Talmudic order of Kodashim
 *   functions as a memorial archive documenting a superseded practice rather
 *   than a live or dormant obligation. Under this reading, the rabbinic
 *   text-study apparatus is not merely a coping mechanism but the successor
 *   institution — the kernel of sacrificial obligation has been discharged
 *   and closed, not held in abeyance. This creates moderate extraction: the
 *   doctrine claims theological continuity with the sacrificial system
 *   (invoking Hosea's 'let our lips substitute for bulls') while functionally
 *   denying that restoration is owed or even coherent, which sidelines those
 *   who hold sacrifice as a live or dormant obligation. Two sibling readings
 *   of the same kernel — performance_only (Kodashim as dormant blueprint
 *   awaiting messianic restoration) and study_as_exercise (study itself
 *   performs the mitzvah, kernel remains occupied) — are NOT part of this
 *   story; they are separate constraints linked via
 *   network.affects_constraints, per the ε-invariance principle. Each reading
 *   has its own beneficiary/victim structure and its own ε.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__substitution_archive, 0.42).
domain_priors:suppression_score(kodashim_corpus__substitution_archive, 0.38).
domain_priors:theater_ratio(kodashim_corpus__substitution_archive, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_corpus__substitution_archive, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__substitution_archive, tangled_rope).
narrative_ontology:human_readable(kodashim_corpus__substitution_archive, "Kodashim as Substitution Archive: Prayer/Study Superseding Sacrifice").
narrative_ontology:topic_domain(kodashim_corpus__substitution_archive, "religious_studies/rabbinic_judaism/commitment_system_theory").

domain_priors:requires_active_enforcement(kodashim_corpus__substitution_archive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__substitution_archive, '1fa83f08-9dec-4a2b-afaa-fe415bfbcba0').
narrative_ontology:cs_kernel_codification('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', fixed_text).
narrative_ontology:cs_authority_grounding('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', lineage).
narrative_ontology:cs_interpretation_layer_present('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0').
narrative_ontology:cs_reading_relation('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', kodashim_corpus__study_as_exercise, influences).
narrative_ontology:cs_axiom('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', foundational, sacrifice_obligation_discharged_by_substitution).
narrative_ontology:cs_axiom_status(sacrifice_obligation_discharged_by_substitution, holdable).
narrative_ontology:cs_axiom_grounding('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', sacrifice_obligation_discharged_by_substitution, conventional).
narrative_ontology:cs_axiom('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', secondary, restoration_not_owed_absent_new_prophetic_mandate).
narrative_ontology:cs_axiom_status(restoration_not_owed_absent_new_prophetic_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', restoration_not_owed_absent_new_prophetic_mandate, deontological).
narrative_ontology:cs_reference_frame('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', temple_centered_sacrificial_order).
narrative_ontology:cs_drift_state('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', post_destruction_rabbinic_consolidation, gap(codification_collapse, substantial, true)).
narrative_ontology:cs_created_at('1fa83f08-9dec-4a2b-afaa-fe415bfbcba0', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__substitution_archive, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, sacrificial_restoration_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_corpus__substitution_archive, ordinary_worshippers).
narrative_ontology:constraint_victim(kodashim_corpus__substitution_archive, ordinary_worshippers).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, prayer_as_valid_substitute_for_sacrifice).
narrative_ontology:constraint_vindicates(kodashim_corpus__substitution_archive, hosea_lips_for_bulls_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Yeshivot, seminaries, and the rabbinic interpretive establishment administer the standing liturgical and pedagogical order in which prayer and Torah study occupy the functional space sacrifice once held. They author the doctrine (grounded in Hosea 14:3, 'let our lips substitute for bulls') that study of Kodashim itself discharges whatever obligation the sacrificial system carried, and they hold the institutional authority — seminary curricula, communal funding, rabbinic ordination — that depends on this reading remaining settled. Their exit from the arrangement is effectively arbitrage: they can reframe the doctrine's emphasis at will without any external check, since no competing authority adjudicates the kernel.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions, beneficiary).

% Communities and individuals (some Temple Mount movements, some maximalist restorationist streams) who hold that physical sacrifice remains the living obligation and that Kodashim documents a suspended practice, not a superseded one. Under the substitution-archive reading their position is characterized as obsolete or theologically naive by the institutions that administer mainstream liturgical practice; they bear the cost of having their preferred practice treated as archived rather than dormant. Their exit is constrained: they can form minority institutions (some exist) but cannot dislodge the mainstream doctrinal consensus that funds and staffs the dominant textual tradition.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, sacrificial_restoration_seekers, payer,
    moderate, generational, constrained, national).

% Lay congregants receive a coherent, practicable religious life — prayer three times daily, a study curriculum — without needing priestly lineage, a Temple, or ritual purity apparatus. They also lose access to whatever spiritual claims sacrificial practice made (atonement mechanics, tangible offering) that some liturgy still gestures toward without fulfilling. Most have no stake in adjudicating the kernel contest and simply inherit whichever reading their community's institutions teach.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, ordinary_worshippers, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__substitution_archive, ordinary_worshippers, payer).

% Hold that Kodashim is a dormant blueprint awaiting messianic restoration, not something superseded. Their position is structurally sidelined by the substitution-archive reading's dominant institutional weight, though it remains a live minority position within Orthodox eschatology rather than something formally excommunicated.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, performance_only_adherents, excluded,
    moderate, civilizational, constrained, national).

% Hold (following a talmudic tradition, e.g. Taanit 27b and related sources) that the study of Kodashim IS the performance of the sacrificial mitzvah — the kernel remains occupied through intellectual engagement rather than replaced by prayer. This reading competes directly with the substitution-archive framing for institutional emphasis within the same yeshiva world.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, study_as_exercise_adherents, excluded,
    moderate, civilizational, constrained, national).

% Study the historical transition from Temple-centered sacrificial Judaism to rabbinic prayer-and-study Judaism following 70 CE, documenting how doctrinal claims of continuity (substitution, memorial, performative-study) function to legitimate the rabbinic institutional order that emerged after the Temple's destruction.
narrative_ontology:constraint_stakeholder(kodashim_corpus__substitution_archive, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__substitution_archive, rabbinic_text_study_institutions).
narrative_ontology:fixing_cost_class(kodashim_corpus__substitution_archive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, portable religious practice (fixed liturgy, communal study cycle) that lets Jewish religious life continue and cohere across the diaspora without a Temple, priesthood, or sacrificial infrastructure — genuinely solving the collective-action problem of maintaining unified practice after 70 CE.
% TRANSFER_FUNCTION: Moves religious authority, funding, and interpretive legitimacy from a lineage-based priestly/sacrificial system toward a text-study/rabbinic system; moves theological credibility away from restorationist positions (which are cast as naive or premature) toward the institutions that administer prayer and study as the normative present-tense practice.
% ABSENT_VOICES: Performance_only adherents and study_as_exercise adherents both hold live alternative readings of the same kernel but are structurally sidelined in mainstream liturgical education, which teaches the substitution doctrine (Hosea's lips-for-bulls) as settled rather than as one contested reading among three.
% DISAPPEARANCE_RATIONALE: If the substitution-archive doctrine specifically (as opposed to prayer/study practice itself) were abandoned tomorrow, mainstream liturgical practice would likely continue unchanged in the short term — but the doctrinal ground on which non-restoration is justified would evaporate, reopening the question of whether prayer is a full substitute or a stopgap, with direct implications for restorationist movements' legitimacy claims and for how seminaries teach Kodashim.
% FOUNDING_PROBLEM: After the Second Temple's destruction in 70 CE, sacrificial worship became physically impossible; the rabbinic movement needed a theological and practical answer to how religious obligation continues without an altar, priesthood, or Temple.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion (outside the rabbinic institutions that benefit from the doctrine) corroborate that the practical problem — how to maintain religious continuity post-70 CE — was real and acute. But the specific doctrinal claim that prayer/study *fully substitutes* for and thereby archives (rather than merely defers) the sacrificial obligation is attested primarily by the rabbinic tradition itself; restorationist movements and some Orthodox eschatological streams dispute that the substitution is final rather than provisional, and no outside corroborating authority adjudicates between the readings.
narrative_ontology:disappearance_verdict(kodashim_corpus__substitution_archive, contested).
narrative_ontology:founding_problem_status(kodashim_corpus__substitution_archive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__substitution_archive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__substitution_archive, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__substitution_archive, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__substitution_archive_tests).
:- end_tests(kodashim_corpus__substitution_archive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) rather than high because the coordination function is genuine and substantial — prayer and study did solve a real civilizational continuity problem after 70 CE — but the doctrine layers a specific, contestable finality claim (full substitution, not provisional deferral) on top of that genuine coordination, and that finality claim advantages the institutions that teach it as settled while disadvantaging restorationist theology. Suppression is moderate-low (0.38) because no coercive apparatus enforces the doctrine against restorationists — they remain a tolerated minority position — but institutional/curricular dominance functions as soft suppression of the alternative readings. Theater ratio is elevated (0.55) because a great deal of continued Kodashim study operates as performative continuity-signaling (studying laws of a Temple that does not exist) whose practical function is closer to doctrinal maintenance of the substitution claim than to functional legal preparation.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic text-study institutions are the structural beneficiary: their pedagogical, financial, and interpretive authority depends on Kodashim study occupying the 'archive of the superseded' rather than 'active blueprint' or 'ongoing performance' slot, so d sits near the beneficiary end for them. Sacrificial restoration seekers are the target: the substitution-archive doctrine directly delegitimizes their theological position as obsolete, so d sits near the target end. Ordinary worshippers sit closer to symmetric — real coordination benefit (a practicable, portable religious life) alongside diffuse loss (foreclosure of the sacrificial-atonement framework some liturgy still gestures toward).
 *
 * MANDATROPHY ANALYSIS:
 *   The genuine coordination function (post-Temple religious continuity) prevents this from being classified as pure extraction (snare): prayer and study really did solve a civilizational problem with minimal coercive overhead in the sense that no one was forced into synagogue attendance by state violence. But the mandate — the specific claim that the substitution is FINAL rather than provisional — has outlived clean justification: the 'problem' (no Temple) remains live 1950 years later, yet the doctrinal apparatus increasingly functions to foreclose restoration questions rather than merely to explain the present absence of a Temple. This is why tangled_rope (not rope): a real coordination function coexists with an asymmetric cost imposed on restorationist theology through the same doctrinal structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    final_substitution_vs_provisional_deferral,
    'Is the rabbinic doctrine that prayer/study replaced sacrifice a claim of FINAL substitution (Kodashim as closed archive) or merely PROVISIONAL deferral pending restoration (Kodashim as dormant blueprint)?',
    'Close reading of classical sources (Hosea 14:3, Talmud Taanit 27b, Maimonides'' Mishneh Torah Hilchot Melachim on the restoration of sacrifice under a future Davidic king) against how contemporary institutions actually teach the doctrine — whether restoration is affirmed as a future obligation or quietly dropped.',
    'If the mainstream doctrine is actually provisional deferral (closer to performance_only) rather than final substitution, this story''s classification as tangled_rope built on a finality claim would be overstated, and the extraction against restoration seekers would be lower — they would be a legitimate future claimant, not a delegitimized minority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(final_substitution_vs_provisional_deferral, conceptual, 'Whether the substitution-archive reading''s finality claim is doctrinally accurate or an institutional overreach beyond the classical sources it cites.').

omega_variable(
    kernel_framing_underdetermination,
    'Is the correct unit of analysis the single kernel (kodashim_corpus, with three competing readings) or three genuinely separate historical-institutional artifacts that happen to share a textual corpus?',
    'Trace whether the three readings emerged as explicit alternatives in rabbinic discourse (supporting single-kernel framing) or developed independently in different communities with little cross-reference (supporting decomposition into unrelated constraints).',
    'If the readings are historically independent rather than explicit alternatives to one another, treating them as siblings of one kernel (with reading_relations) may impose a contest structure the historical record does not support, though it would not change this story''s own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel/reading structure accurately models the historical relationship among the three doctrinal positions.').

omega_variable(
    institutional_capture_vs_genuine_theology,
    'Does the substitution-archive doctrine persist because it is the rabbinic community''s genuine theological conclusion, or because it is institutionally convenient — sacrificial restoration would relocate religious authority away from text-study institutions toward a restored priesthood and Temple administration?',
    'Examine whether rabbinic institutions that hold the substitution-archive view have historically opposed or obstructed practical restoration efforts (e.g., Temple Mount access advocacy) beyond safety/political concerns, which would suggest institutional self-interest rather than pure theological conviction.',
    'If institutional self-interest is a significant driver, the beneficiary declaration (rabbinic_text_study_institutions) is well-founded and extraction may be understated rather than overstated; if the doctrine is purely theological with institutions merely following conviction, extraction may be closer to a rope (coordination without asymmetric extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_vs_genuine_theology, empirical, 'Whether beneficiary institutions hold the doctrine for theological or self-interested institutional reasons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__substitution_archive, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__substitution_archive, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(koda_tr_t0, projected).
narrative_ontology:measurement(koda_tr_t200, kodashim_corpus__substitution_archive, theater_ratio, 200, 0.4).
narrative_ontology:measurement(koda_tr_t600, kodashim_corpus__substitution_archive, theater_ratio, 600, 0.45).
narrative_ontology:measurement(koda_tr_t1000, kodashim_corpus__substitution_archive, theater_ratio, 1000, 0.48).
narrative_ontology:measurement(koda_tr_t1500, kodashim_corpus__substitution_archive, theater_ratio, 1500, 0.52).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__substitution_archive, theater_ratio, 1950, 0.55).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__substitution_archive, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(koda_be_t0, projected).
narrative_ontology:measurement(koda_be_t200, kodashim_corpus__substitution_archive, base_extractiveness, 200, 0.33).
narrative_ontology:measurement(koda_be_t600, kodashim_corpus__substitution_archive, base_extractiveness, 600, 0.36).
narrative_ontology:measurement(koda_be_t1000, kodashim_corpus__substitution_archive, base_extractiveness, 1000, 0.38).
narrative_ontology:measurement(koda_be_t1500, kodashim_corpus__substitution_archive, base_extractiveness, 1500, 0.4).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__substitution_archive, base_extractiveness, 1950, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_corpus__substitution_archive, suppression_requirement, 0, 0.25).
narrative_ontology:measurement_basis(koda_su_t0, projected).
narrative_ontology:measurement(koda_su_t200, kodashim_corpus__substitution_archive, suppression_requirement, 200, 0.28).
narrative_ontology:measurement(koda_su_t600, kodashim_corpus__substitution_archive, suppression_requirement, 600, 0.3).
narrative_ontology:measurement(koda_su_t1000, kodashim_corpus__substitution_archive, suppression_requirement, 1000, 0.33).
narrative_ontology:measurement(koda_su_t1500, kodashim_corpus__substitution_archive, suppression_requirement, 1500, 0.36).
narrative_ontology:measurement(koda_su_t1950, kodashim_corpus__substitution_archive, suppression_requirement, 1950, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__substitution_archive, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__substitution_archive, 0.1).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__substitution_archive, kodashim_corpus__study_as_exercise).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kodashim_corpus kernel, decomposed per the ε-invariance principle rather than authored as a single constraint with an observable-dependent ε. substitution_archive (this story, tangled_rope, ε=0.42) claims prayer/study fully replaced sacrifice, benefiting rabbinic text-study institutions at the cost of restoration-seeking communities. performance_only (sibling, likely scaffold or rope, lower ε) holds Kodashim as a dormant blueprint awaiting messianic restoration — no substitution claim, so no delegitimization of restorationists. study_as_exercise (sibling, likely rope, lowest ε) holds that study itself performs the mitzvah, keeping the kernel continuously occupied without either a substitution claim or a deferral claim. All three read the same textual corpus (the Mishnaic/Talmudic order of Kodashim) but authorize structurally different relationships to sacrificial restoration, hence three separate constraints rather than one with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
