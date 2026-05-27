% ============================================================================
% CONSTRAINT STORY: performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_only, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: performance_only
 *   human_readable: Sacrifice Commandment: Performance-Only Reading (Halakhic Constraint)
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   The performance-only reading of the sacrifice commandment establishes
 *   that commandments tied to the Temple are suspended (not nullified) during
 *   Diaspora exile when no legitimate performance venue exists. This reading
 *   maintains Torah integrity across civilizational discontinuity—the
 *   commandments remain valid law but cannot be obeyed until the Temple is
 *   rebuilt. The constraint operates through a legal fiction: treating
 *   suspended commandments as continuous obligations that happen to be
 *   unperformable. This reading is one of three competing interpretations
 *   within Jewish halakhic tradition. The study-as-performance reading treats
 *   intense study of sacrifice laws as a performative equivalent to actual
 *   sacrifice, reframing the commandment as achievable through scholarship.
 *   The archive-maintenance reading treats the detailed preservation of
 *   sacrifice knowledge as valuable irrespective of performance, emphasizing
 *   documentation over fulfillment. The performance-only reading forecloses
 *   both alternatives by insisting that performance requires the Temple;
 *   study and archive cannot substitute for the actual act. This creates a
 *   structural trap: 1,900 years of mandatory scholarship devoted to
 *   commandments that cannot be obeyed, under the authority of a tradition
 *   that has defined them as perpetually suspended. The high theater ratio
 *   (0.85) reflects that the intellectual labor—analyzing bird species, blood
 *   disposition, meal-offering ratios—serves no performative function, yet
 *   the study is halakhically mandatory. The extractiveness (0.68) measures
 *   the diversion of scholarly attention and identity from living law,
 *   conscripted into perpetual study of dead commandments.
 *
 * KEY AGENTS:
 *   - The Living Commandment: Primary victim (powerless/trapped) — cannot be fulfilled, cannot be abandoned; infinite obligation with zero legitimate performance pathway
 *   - Jewish Scholarly Community: Secondary victim (moderate/constrained) — conscripted into perpetual study of unperformable acts; identity-locked in tradition of interpretation
 *   - Rabbinic Interpretive Authority: Primary beneficiary (institutional/arbitrage) — controls the suspension doctrine; maintains authority through interpretive gatekeeping; benefits from ability to define what constitutes legitimate interpretation
 *   - The Halakhic System: Tertiary actor (institutional/arbitrage) — preserved through the performance-only constraint but degraded in function (theater increases while practical relevance decreases)
 *   - Temple-Reconstruction Movement: Secondary beneficiary (organized/mobile) — has structural interest in overturning the performance-only constraint but lacks sufficient power to redefine the interpretation
 *   - Analytical Observer: Witness (analytical/analytical) — risks naturalizing contingent interpretive choice as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_only, 0.68).
domain_priors:suppression_score(performance_only, 0.72).
domain_priors:theater_ratio(performance_only, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_only, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_only, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(performance_only, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_only, snare).
narrative_ontology:human_readable(performance_only, "Sacrifice Commandment: Performance-Only Reading (Halakhic Constraint)").
narrative_ontology:topic_domain(performance_only, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_only, '3a449931-557a-481f-a20f-05dad8aed2d3').
narrative_ontology:cs_created_at('3a449931-557a-481f-a20f-05dad8aed2d3', '').
narrative_ontology:cs_kernel_codification('3a449931-557a-481f-a20f-05dad8aed2d3', fixed_text).
narrative_ontology:cs_authority_grounding('3a449931-557a-481f-a20f-05dad8aed2d3', lineage).
narrative_ontology:cs_interpretation_layer_present('3a449931-557a-481f-a20f-05dad8aed2d3').
narrative_ontology:cs_kernel_id(performance_only, sacrifice_commandment).
narrative_ontology:cs_reading_relation('3a449931-557a-481f-a20f-05dad8aed2d3', study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('3a449931-557a-481f-a20f-05dad8aed2d3', archive_maintenance, coexists_with).
narrative_ontology:cs_axiom('3a449931-557a-481f-a20f-05dad8aed2d3', foundational, performance_requires_temple).
narrative_ontology:cs_axiom_status(performance_requires_temple, holdable).
narrative_ontology:cs_axiom('3a449931-557a-481f-a20f-05dad8aed2d3', foundational, suspension_distinct_from_nullification).
narrative_ontology:cs_axiom_status(suspension_distinct_from_nullification, holdable).
narrative_ontology:cs_reference_frame('3a449931-557a-481f-a20f-05dad8aed2d3', torah_commandment_coherence_across_exile).
narrative_ontology:cs_drift_state('3a449931-557a-481f-a20f-05dad8aed2d3', contemporary_period, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_only, rabbinic_interpretive_authority).
narrative_ontology:constraint_victim(performance_only, jewish_legal_scholarship).
narrative_ontology:constraint_victim(performance_only, living_commandment_performance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LIVING COMMANDMENT (SNARE) — The sacrificial commandments (korbanot) are trapped in a paradox: Torah mandates them, but 1,900 years of Diaspora exile removed the only legitimate performance venue (the Temple). The commandment cannot be fulfilled, only studied. Exit is impossible — one cannot exit Judaism and retain halakhic status; one cannot nullify Torah commandments. Maximum experienced extraction: infinite obligation with zero legitimate performance pathway.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JEWISH SCHOLARLY COMMUNITY (SNARE) — Scholars are conscripted into perpetual study of laws that cannot be obeyed. Talmudic, medieval, and modern commentators invest massive intellectual labor analyzing sacrifice details (bird species, blood disposition, meal-offering ratios) that have no performative consequence. The study is mandatory — abandoning it would constitute halakhic negligence — but the extraction is clear: centuries of cognitive resources directed at unperformable acts. Exit is constrained by identity fusion with scholarly tradition and institutional pressure, not merely economic cost.
constraint_indexing:constraint_classification(performance_only, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — From this reading, the performance-only constraint solves a genuine coordination problem: it maintains the commandment's legal status (pikuach) without requiring the Temple rebuild or permitting Diaspora Jews to improvise unauthorized sacrificial practice. The suspension-not-nullification doctrine preserves halakhic coherence and protects rabbinic authority to interpret suspension conditions. This perspective experiences the constraint as enabling—a coordination mechanism, not extraction. Beneficiary status derives from control over what counts as legitimate suspension.
constraint_indexing:constraint_classification(performance_only, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: THE HALAKHIC SYSTEM (PITON) — The performance-only reading preserves a legal fiction: treating unperformable commandments as suspended-but-valid maintains the system's coherence and authority structure. However, the constraint's primary function (preventing illegal sacrifice in Diaspora) was accomplished centuries ago. The continued enforcement of the performance-only restriction now serves mainly to maintain rabbinical authority over interpretive boundaries rather than to coordinate substantive religious practice. Theater ratio is high: elaborate reasoning about hypothetical sacrifice scenarios in a context where this knowledge serves no practical function. The ritual persists through institutional inertia.
constraint_indexing:constraint_classification(performance_only, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TEMPLE-RECONSTRUCTION MOVEMENT (TANGLED ROPE) — Organized actors (Jewish Temple Institute, certain right-wing religious Zionists) see the performance-only constraint as hybrid: it coordinates with genuine halakhic requirements (preventing unauthorized sacrifice) but also extracts by delegitimizing any Temple reconstruction effort as presumptuous. The movement experiences both genuine coordination need and asymmetric constraint. They are mobile (could exit through secular nationalism or non-Orthodox practice) but experience the performance-only reading as restriction on their legitimate aspiration. Enforcement is active—mainstream rabbinic authority enforces the suspension, not through legal mechanism but through interpretive legitimacy.
constraint_indexing:constraint_classification(performance_only, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, some commandments are logically tied to conditions that no longer exist. The Temple is gone; no legitimate venue for sacrifice remains. Therefore, the commandment is naturally suspended—not by rabbinic fiat, but by factual circumstance. The performance-only constraint appears as an immutable feature of Jewish legal reality, like an irreducible logical limit. However, this is a false summit: the mountain classification masks a choice (treating suspension as permanent vs. contingent) that different halakhic traditions resolve differently. The analytical observer risks naturalizing what is actually a normative commitment to maintain Torah coherence across civilizational discontinuity.
constraint_indexing:constraint_classification(performance_only, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_only_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(performance_only, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(performance_only, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(performance_only, TR),
    TR >= 0.70.

:- end_tests(performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The performance-only reading creates a structural trap where scholarly labor is conscripted into perpetual study of unperformable commandments. The extraction escalates over the 1,900-year interval as the initial defensive purpose (preventing unauthorized Diaspora sacrifice) becomes institutionalized as permanent interpretation. The beneficiary (rabbinic interpretive authority) extracts from the victim (scholarly community and the living commandment itself) through maintaining the suspension-not-nullification doctrine. The extractiveness value reflects that the primary function of the constraint is no longer coordination but authority maintenance. Suppression (0.72): High. The constraint suppresses alternatives through multiple mechanisms: (1) the obligation doctrine makes abandoning sacrifice study halakhically illegitimate; (2) the suspension-not-nullification framework forecloses the study-as-performance alternative by defining performance as requiring the Temple; (3) institutional consensus among rabbinic authorities enforces interpretive closure; (4) the tradition of identity-locked scholarship internalizes the suppression. Exit is structurally blocked for anyone embedded in halakhic practice. Theater ratio (0.85): Very high and increasing. The constraint exhibits peak theatrical content: elaborate scholastic analysis of sacrifice details that produce no behavioral change, no coordination benefit, and no material outcome. The theater increases over time as the original emergency (preventing illegal sacrifice) recedes and the maintenance mechanism becomes purely institutional. By the modern period, the study of sacrificial law serves primarily to demonstrate scholarly competence within the tradition, not to address any living legal problem. The measurement trajectory shows the constraint degrading from coordination (T=0, when emergency prevention was actual function) to pure extraction (T=1900, when the study serves institutional authority maintenance).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence grounded in structural position within the halakhic system. The rabbinic authority sees a coordination mechanism (Rope)—the performance-only doctrine solves the genuine problem of preventing illegal Diaspora sacrifice while maintaining Torah integrity. The scholarly community sees a snare—they are trapped in mandatory study of unperformable commandments. The halakhic system itself sees its function degrading over time (Piton)—the constraint's original defensive purpose gives way to institutional theater. The temple-reconstruction movement sees a barrier to legitimate aspiration (Tangled Rope)—the constraint both maintains legal coherence and blocks their goal. The analytical observer risks seeing a natural law (Mountain)—the absence of the Temple naturally suspends the commandment—but this masks a normative choice about how to treat Torah when performance becomes impossible. The perspectival gap widens over the 1,900-year interval: the constraint's original function (emergency prevention) becomes increasingly obscured by institutional maintenance, making it appear natural to later generations when it was actually a contingent interpretation choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The performance-only reading locates directionality through structural relationships to the suspension doctrine. Rabbinic interpretive authority benefits from control over suspension: they decide what counts as suspension versus nullification, what conditions permit restoration, and whether alternatives (study-as-performance, archive maintenance) are legitimate. This beneficiary status derives from the power to define interpretive boundaries. d(rabbinic_authority) ≈ 0.10 (low—strong beneficiary with arbitrage exit). The scholarly community is the primary victim: they are conscripted into perpetual study without performative consequence. d(scholarly_community) ≈ 0.85 (high—trapped by obligation doctrine and identity fusion). The living commandment itself—the abstract requirement of the Torah—is victimized by being held in perpetual suspension: it cannot be fulfilled, yet cannot be nullified. d(living_commandment) ≈ 0.95 (maximum—entirely victimized with no exit). The Temple-reconstruction movement occupies an intermediate position: they have some power (organized actors) and some exit (could leave Orthodoxy), but experience the performance-only constraint as blocking their legitimate aspiration. d(reconstruction_movement) ≈ 0.65 (moderate-to-high). The engine computes chi from these d values and the constraint's ε via the sigmoid f(d). Beneficiaries experience low chi (extraction runs away from them). Victims experience high chi (extraction targeted at them).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is one reading of the sacrifice_commandment kernel. The mandatrophy question is: which reading (performance-only, study-as-performance, archive-maintenance) correctly instantiates the halakhic law? If one reading is correct, the other two represent false constraints and should not generate separate stories. If the readings coexist as legitimate alternatives within Jewish tradition, each generates a separate constraint story. The performance-only reading asserts that the distinction between suspension and nullification is structurally grounded in Torah and halakhic logic: commandments tied to specific conditions (Temple, monarchy, land-presence) are naturally suspended when conditions change. Study cannot substitute for performance because the commandment requires the specific performative act. This reading is empirically dominant in modern Orthodox interpretation but historically contested. The study-as-performance reading (sibling constraint, separate story) claims Talmudic grounding for treating intense study of sacrifice laws as performatively equivalent. The archive-maintenance reading (third sibling) treats preservation of sacrifice knowledge as valuable on its own terms. The performance-only reading forecloses study-as-performance by asserting that performance requires the Temple—study is merely preparation, not performance itself. This is logically foreclosing: if the performance-only reading's core premise (commandments require specific conditions) is true, then study-as-performance is false. The three readings cannot coexist within a single halakhic framework. However, they DO coexist as competing traditions within Jewish practice: modern Orthodox Judaism dominates the performance-only reading, while some Kabbalistic and Hasidic traditions preserve elements of study-as-performance, and conservative and reconstructionist movements treat archive-maintenance as primary. Mandatrophy is resolved by recognizing that the question 'which reading is correct?' is itself answered differently by different halakhic authorities. The constraint documents a single reading, along with omegas that identify the unresolved interpretive contest.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_nullification_boundary,
    'What constitutes the difference between a suspended commandment and a nullified one, and is this distinction grounded in Torah structure or rabbinic convention?',
    'Comparative analysis of how other commandments are treated when conditions change (commandments tied to Temple, to monarchy, to land-presence); examination of whether suspension doctrine applies uniformly or is ad-hoc per commandment; investigation of whether pre-Talmudic sources anticipate long-term Diaspora or treat it as temporary',
    'If distinction is grounded in Torah: performance-only constraint is mountain (natural consequence of commandment structure). If ad-hoc rabbinic convention: constraint is snare or tangled rope (interpretive authority maintaining authority through legalism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_nullification_boundary, conceptual, 'Whether suspension/nullification distinction is structural or conventional').

omega_variable(
    study_as_performance_empirical_content,
    'Does the sibling ''study-as-performance'' reading have empirical grounding in halakhic sources, or is it a modern hermeneutical invention?',
    'Systematic review of Talmudic sources for statements treating sacrifice study as equivalent to performance; dating of key sources and tracing through medieval and modern commentary; comparison with treatment of other commandments where study is invoked as substitute',
    'If empirically grounded: reading coexists (both performance-only and study-as-performance were live options in tradition). If modern invention: performance-only forecloses it (the two readings represent different historical layers, not coexisting interpretive options).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_as_performance_empirical_content, empirical, 'Whether study-as-performance reading has Talmudic grounding').

omega_variable(
    scholar_identity_lock_vs_structural_obligation,
    'To what extent is the constraint experienced as identity-locked (scholars cannot imagine abandoning sacrifice study because it constitutes scholarly identity) versus structurally imposed (obligation doctrine enforces the study regardless of willingness)?',
    'Ethnographic observation of how scholars discuss sacrifice study: whether they invoke obligation doctrine or express identity identification with the tradition; investigation of whether secular Jewish scholars who opt out experience identity crisis or merely neglect a legal requirement; comparison with how scholars approach other unperformable commandments (e.g., Temple sacrificial service)',
    'If identity-locked: constraint''s suppression is internalized (binding mechanism is cognitive); victims experience it as inescapable self-concept rather than external coercion. If structural-obligation: suppression is external enforcement, and exit is theoretically possible through rejecting halakhic framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_identity_lock_vs_structural_obligation, empirical, 'Mechanism of scholar entrapment in sacrifice study').

omega_variable(
    temple_reconstruction_interpretive_legitimacy,
    'Is the rabbinic blocking of Temple reconstruction grounded in substantive halakhic reasoning, or is it sustained primarily through control of interpretive legitimacy (modern rabbinical consensus refusing to treat reconstruction seriously)?',
    'Analysis of whether objections to Temple reconstruction cite specific halakhic barriers (messiah preconditions, purity law impossibilities) versus invocation of authority consensus; investigation of whether alternative halakhic reasoning could permit reconstruction; examination of how the objection has evolved as technological and political circumstances changed',
    'If substantive halakhic barriers: reconstruction is naturally foreclosed, and the performance-only constraint is grounded in law. If legitimacy control: the constraint is enforced through interpretive gatekeeping, making it more extraction-like than purely coordinating.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temple_reconstruction_interpretive_legitimacy, conceptual, 'Source of rabbinic blocking of Temple reconstruction').

omega_variable(
    mandatrophy_reading_contest,
    'This constraint is one reading of the sacrifice_commandment kernel. The sibling readings (study_as_performance, archive_maintenance) represent alternative interpretive traditions. Which reading is the actual halakhic law, or do they coexist as competing legitimate interpretations?',
    'Empirical: trace which reading controls modern halakhic practice and halakhic court decisions. Conceptual: assess whether the readings are logically compatible or foreclosing. Institutional: determine whether contemporary rabbinical authorities treat them as coexisting options or as conflicting claims requiring resolution.',
    'If one reading forecloses others: one constraint story is false, and mandatrophy is resolved by factual discovery. If readings coexist: all three are legitimate, and mandatrophy reflects genuine divergence in halakhic tradition. If readings are being collapsed into one: classify the collapse mechanism (institutional authority choosing single reading) as the actual constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_reading_contest, empirical, 'Whether sibling readings are coexisting or foreclosing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_only, 0, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_only, theater_ratio, 0, 0.62).
narrative_ontology:measurement(perf_tr_t500, performance_only, theater_ratio, 500, 0.75).
narrative_ontology:measurement(perf_tr_t1000, performance_only, theater_ratio, 1000, 0.85).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_only, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(perf_be_t500, performance_only, base_extractiveness, 500, 0.62).
narrative_ontology:measurement(perf_be_t1000, performance_only, base_extractiveness, 1000, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_only, identity_coordination).
narrative_ontology:affects_constraint(performance_only, study_as_performance).
narrative_ontology:affects_constraint(performance_only, archive_maintenance).

% DUAL FORMULATION NOTE:
% The sacrifice_commandment kernel decomposes into three structurally distinct constraint stories. The performance-only reading (this story) asserts that commandments require specific performance venues; it forecloses the study-as-performance reading but coexists with archive-maintenance. The three stories share the same base domain (how to relate to Temple-tied commandments) but have different ε values reflecting different views of what constitutes legitimate fulfillment. The performance-only reading's high extractiveness (0.68) reflects that it conscripts 1,900 years of mandatory scholarship into unperformable study. The study-as-performance reading would show lower extractiveness if study itself counts as performance. The archive-maintenance reading would show moderate extractiveness if preservation is treated as a fulfillment mechanism. Each story gets its own perspectives and measurements. The network edges link them as readings of the same kernel, with the performance-only story exerting structural pressure on the siblings by foreclosing alternatives through asserting the performance-requirement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(performance_only, powerless, 0.95).
constraint_indexing:directionality_override(performance_only, moderate, 0.85).
constraint_indexing:directionality_override(performance_only, institutional, 0.1).
constraint_indexing:directionality_override(performance_only, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
