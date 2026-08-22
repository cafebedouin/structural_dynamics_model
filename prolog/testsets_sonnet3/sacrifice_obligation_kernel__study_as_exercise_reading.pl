% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__study_as_exercise_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__study_as_exercise_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__study_as_exercise_reading
 *   human_readable: Torah Study as Fulfillment of the Sacrifice Obligation (Talmud Torah k'neged Korbanot Reading)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates one reading within a four-way contested kernel
 *   over the sacrifice obligation's post-Temple status. Under
 *   study_as_exercise_reading, the study of sacrificial law (hilchot
 *   korbanot) is held to BE the mitzvah's genuine exercise — the obligation
 *   is 'occupied' through intellectual engagement rather than left
 *   unperformed or suspended. This is a distinct structural claim from
 *   performance_only_reading (which holds study is merely preparatory and the
 *   obligation remains materially unmet), from messianic_suspension_reading
 *   (which holds the obligation is held in abeyance, not transformed, pending
 *   restoration), and from symbolic_archive_reading (which drops the halakhic
 *   claim entirely and treats the material as cultural memory). Each of these
 *   is authored as its own constraint with its own epsilon; this file authors
 *   only the study-as-exercise claim, per the eps-invariance principle —
 *   averaging across readings would produce an incoherent composite epsilon
 *   that describes no actual position any party holds.
 *
 * KEY AGENTS:
 *   - rabbinic_scholarly_authority: institutional/arbitrage — articulates and benefits from interpretive centrality this reading confers
 *   - yeshiva_institutions: organized/constrained — curricular and institutional legitimacy rides on the doctrine's validity
 *   - observant_laity_without_temple_access: moderate/constrained — beneficiaries of a resolved rather than open theological gap
 *   - performance_literalist_communities: excluded — hold a minority view marginalized by this reading's dominance
 *   - comparative_legal_historians: analytical/analytical — external corroborating observers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__study_as_exercise_reading, 0.28).
domain_priors:theater_ratio(sacrifice_obligation_kernel__study_as_exercise_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__study_as_exercise_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__study_as_exercise_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__study_as_exercise_reading, "Torah Study as Fulfillment of the Sacrifice Obligation (Talmud Torah k'neged Korbanot Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__study_as_exercise_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__study_as_exercise_reading, 'a053a849-2f2f-472a-bb18-b4e6a5f82dcc').
narrative_ontology:cs_kernel_codification('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', fixed_text).
narrative_ontology:cs_authority_grounding('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', lineage).
narrative_ontology:cs_interpretation_layer_present('a053a849-2f2f-472a-bb18-b4e6a5f82dcc').
narrative_ontology:cs_reading_relation('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', foundational, intellectual_engagement_constitutes_mitzvah_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_constitutes_mitzvah_performance, holdable).
narrative_ontology:cs_axiom_grounding('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', intellectual_engagement_constitutes_mitzvah_performance, conventional).
narrative_ontology:cs_axiom('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', foundational, obligation_mode_transforms_rather_than_suspends).
narrative_ontology:cs_axiom_status(obligation_mode_transforms_rather_than_suspends, holdable).
narrative_ontology:cs_axiom_grounding('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', obligation_mode_transforms_rather_than_suspends, conventional).
narrative_ontology:cs_reference_frame('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', temple_era_sacrificial_performance).
narrative_ontology:cs_drift_state('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a053a849-2f2f-472a-bb18-b4e6a5f82dcc', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_scholarly_authority).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__study_as_exercise_reading, observant_laity_without_temple_access).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, talmud_torah_kneged_kol_hamitzvot).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__study_as_exercise_reading, study_occupies_obligation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Articulates and transmits the doctrine (rooted in readings of Menachot 110a and later codifiers) that engaged study of the sacrificial order is itself the mitzvah's exercise, not mere preparation for it. This ruling determines what counts as fulfillment for an obligation with no available Temple performance, and the authority that can make this determination retains interpretive centrality over a core area of law that would otherwise sit dormant and outside anyone's jurisdiction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_scholarly_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__study_as_exercise_reading, rabbinic_scholarly_authority, beneficiary).

% Structure substantial curricular time (seder kodashim study) around the premise that this study is not academic but is itself religiously efficacious mitzvah-performance. The reading underwrites the institutional legitimacy and fundraising case for dedicating resources to a body of law with no present real-world application outside study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).

% Live under a commandment count that includes sacrificial obligations they structurally cannot perform (no Temple, no altar, no priesthood in functioning order). Under this reading, engaging the relevant Talmudic and codified material lets them regard the obligation as actively discharged rather than as a standing unmet duty or a suspended one, resolving what would otherwise be an anxious gap between obligation and practice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, observant_laity_without_temple_access, beneficiary,
    moderate, biographical, constrained, global).

% Some strands (particularly those emphasizing restorationist practice or maintaining active priestly-lineage preparation) would hold that only physical performance fulfills the mitzvah and that study, however valuable, cannot substitute for it. Their view is present in the halakhic literature but functions here mainly as a foil; this reading's dominance in mainstream rabbinic Judaism marginalizes their claim to a settled resolution of the fulfillment question.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, performance_literalist_communities, excluded,
    moderate, generational, constrained, national).

% Study how post-Temple rabbinic Judaism reconstituted an obligation that lost its material precondition, and how the study-as-exercise doctrine functioned historically to preserve both the law's authority and the interpreting class's centrality across two millennia without a Temple.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__study_as_exercise_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent, livable answer to a genuine problem: an entire body of commanded practice became physically impossible with the Temple's destruction, and without some resolution, adherents would face either permanent unmet obligation (psychologically and theologically destabilizing) or an implicit admission that large parts of the law had simply lapsed. The study-as-exercise doctrine keeps the corpus alive as active law and gives individuals a concrete, performable way to stand in right relation to the commandment.
% TRANSFER_FUNCTION: Moves interpretive authority and institutional legitimacy toward those who can teach and adjudicate the relevant Talmudic corpus (yeshiva scholars, rabbinic decisors) and away from any claim that the obligation is simply suspended, dormant, or archival — no material transfer between adherents, but a durable transfer of jurisdictional standing to the interpreting class.
% ABSENT_VOICES: Adherents drawn to a more restorationist or literalist reading (that only actual sacrifice fulfills the command) are structurally marginal to this reading's own account; the messianic-suspension camp, which holds the obligation is simply held in abeyance rather than transformed, is a live sibling position but not represented within this reading's own framework as an open question.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the dominant sibling that would likely fill its place (messianic suspension, or performance-only) would leave the underlying yeshiva curriculum largely intact in practice (study would continue) but would strip it of independent religious efficacy — making current study preparatory or archival rather than itself mitzvah-fulfilling. Institutions and curricula would likely persist by inertia; the doctrinal self-understanding of millions of daily learners would change materially. Whether 'the world' rearranges depends on whether one weighs institutional practice (largely stable) or theological self-conception (substantially altered) more heavily — hence contested.
% FOUNDING_PROBLEM: The Temple's destruction in 70 CE eliminated the physical means of fulfilling a substantial share of Torah commandments (the sacrificial order), leaving rabbinic Judaism with a body of binding law it could no longer perform and needed to account for without abandoning the law's authority.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic Judaism (a seat outside the rabbinic authority that benefits from the doctrine) corroborate that the study-as-fulfillment move is a documented post-destruction adaptation appearing in tannaitic and amoraic sources, not a retrospective invention of modern institutions — though those same historians also note the doctrine's continuing institutional utility is a separate, later-layered fact from its original theological motivation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__study_as_exercise_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__study_as_exercise_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__study_as_exercise_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__study_as_exercise_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).
:- end_tests(sacrifice_obligation_kernel__study_as_exercise_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) and rising only slightly across the interval: this reading does not require anyone to pay a material or coercive cost for its operation — no victim set exists because the read itself (unlike a snare or tangled rope) does not transfer resources from a payer class to a beneficiary class through enforced compliance. What it does confer is durable interpretive jurisdiction to rabbinic authority, captured here as beneficiary status rather than extraction, since no identifiable party is worse off by the doctrine's operation. Suppression (0.28) is moderate rather than low because the doctrine does function to marginalize sibling readings (performance-only, literalist restorationism) within mainstream discourse, even without coercive enforcement — this is soft normative suppression through institutional dominance, not force. Theater ratio rises modestly (0.08 to 0.20) reflecting a mild drift toward the study's mnemonic/identity-performance function growing relative to its original doctrinal-resolution function as centuries pass without restoration, but it stays well below piton-indicating thresholds. Accessibility collapse (0.40) and resistance (0.25) are both moderate-low: this is a genuinely contestable theological position with live sibling readings, not a totalizing natural-law-style closure, but it faces relatively little active resistance because it resolves a real anxiety (unperformable obligation) in a way most within its tradition find satisfying rather than imposed.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic authority seat, this reading is a doctrinally grounded, ancient solution to a genuine crisis — coordination in its purest form, restoring coherence to a body of law that would otherwise sit as permanent unmet obligation. From the performance-literalist seat (excluded here, but present in the kernel as a sibling reading), the same doctrine looks like a convenient reinterpretation that lets the interpreting class retain jurisdiction over material it can no longer be tested against in practice, since study cannot be checked for correctness against actual sacrificial performance the way ritual practice could. The engine should register this divergence structurally: the beneficiary seat computes low extraction and rope-like coordination; a hypothetical literalist-payer seat (not authored here, since this reading declares no victims) would compute the doctrine very differently — but that computation belongs to performance_only_reading, not to this file.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholarly authority sits closest to the beneficiary end: it sets the interpretive terms, retains institutional centrality, and bears no cost from the doctrine's operation — arbitrage-grade exit in the sense that its position is not contingent on any one adjudication going a particular way. Yeshiva institutions and observant laity are also beneficiaries, though with more constrained exit (their religious lives and institutional structures are built around the doctrine's continued validity, making their beneficiary status somewhat identity-adjacent even though it is not extraction). No victim group is declared because no party's obligations, resources, or standing are structurally worsened by this reading's dominance — the performance-literalist communities lose relative doctrinal standing, not material position, which is why they are modeled as excluded rather than as payers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an unperformable commanded practice needing theological resolution) remains live by this reading's own lights — the Temple has not been rebuilt, so the study-as-exercise mechanism continues to do the work it was built to do, rather than persisting as inertia after its function lapsed. This distinguishes the constraint from mandatrophy: there is no dead function being defended by a doctrine that outlived it, because the absence of a Temple that originally motivated the doctrine is the same absence that continues to motivate it today. The corroboration from historians outside the rabbinic beneficiary class supports treating founding_problem_status as live rather than as a self-serving retrospective claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_fulfillment_vs_authority_interest,
    'Is the study-as-exercise doctrine a good-faith theological resolution to a genuine crisis of unperformable obligation, or does its persistence owe substantially to the interpretive jurisdiction and institutional legitimacy it confers on the rabbinic scholarly class that articulates and transmits it?',
    'Comparative analysis of the doctrine''s earliest attestations (tannaitic/amoraic sources, particularly Menachot 110a and parallel materials) against the institutional interests of the classes that preserved and elaborated it over subsequent centuries; examination of whether the doctrine''s elaboration tracks periods of rabbinic institutional consolidation.',
    'If the doctrine is substantially interest-driven rather than purely theologically motivated, this reading would need reclassification toward tangled_rope (genuine coordination function for laity''s psychological/theological need, but also asymmetric extraction of interpretive authority) rather than pure rope. Currently authored as rope on the assessment that no identifiable victim bears a cost, but the beneficiary concentration in rabbinic authority is a live signal worth flagging.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_fulfillment_vs_authority_interest, conceptual, 'Whether the study-as-fulfillment doctrine is genealogically theological or partly authority-interested.').

omega_variable(
    sibling_reading_foreclosure_pressure,
    'Does the dominance of study_as_exercise_reading in mainstream rabbinic institutions functionally foreclose messianic_suspension_reading and performance_only_reading as live options for ordinary adherents, even though the framework does not logically require foreclosure?',
    'Survey of contemporary halakhic curricula and popular religious education materials to determine whether sibling readings are presented as live minority positions or effectively absent from lay religious formation.',
    'If sibling readings are functionally absent from lay formation despite remaining logically coexistent within the tradition, the suppression metric (0.28) may understate the doctrine''s practical effect on accessible alternatives, and accessibility_collapse (0.40) may be authored too low.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure_pressure, empirical, 'Whether institutional dominance produces de facto foreclosure of sibling kernel readings despite formal coexistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__study_as_exercise_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 700, 0.13).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1100, 0.16).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1500, 0.18).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, theater_ratio, 1950, 0.2).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 300, 0.06).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 700, 0.08).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1100, 0.1).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1500, 0.11).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__study_as_exercise_reading, base_extractiveness, 1950, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__study_as_exercise_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__study_as_exercise_reading, sacrifice_obligation_kernel__symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposing the natural-language label 'the sacrifice law obligation post-Temple' per the eps-invariance principle. Each sibling reading (performance_only, messianic_suspension, study_as_exercise [this file], symbolic_archive) authors its own epsilon, beneficiary/victim structure, and claimed_type because the readings make structurally distinct claims about what the obligation currently requires and who occupies it. study_as_exercise_reading is authored here as low-extraction rope-flavored coordination with rabbinic authority as beneficiary and no victim set; performance_only_reading would author a live unmet-obligation structure; messianic_suspension_reading would author a maintained-readiness structure without claiming present fulfillment; symbolic_archive_reading would author near-zero halakhic stakes entirely. All four are linked via affects_constraints as members of one contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
