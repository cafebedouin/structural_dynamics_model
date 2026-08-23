% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Abrahamic Covenant: Isaac-Exclusive Reading
 *   domain: religious/theological/institutional
 *
 * SUMMARY:
 *   This constraint story models the Isaac-exclusive reading of the Abrahamic
 *   covenant (Genesis 17:19-21), where God confirms the covenant through
 *   Isaac and explicitly distinguishes him from Ishmael ('But my covenant I
 *   will establish with Isaac'). This reading, dominant in rabbinic Judaism,
 *   constructs a covenantal boundary that includes the Jewish people
 *   (descended through Isaac/Jacob) and excludes Ishmaelite claimants and the
 *   later Islamic tradition that traces prophetic succession through Ishmael.
 *   The constraint operates as a religious identity boundary with high
 *   extractiveness for those excluded: they are denied covenantal status,
 *   land promise, and chosen-people standing. The coordination function is
 *   genuine — Jewish peoplehood, halakhic continuity, and communal survival
 *   across millennia — but it is entangled with asymmetric extraction that
 *   renders the Ishmael/Islamic line as structurally outside the covenant.
 *   The reading requires active enforcement through interpretive tradition
 *   (midrash, halakha, liturgy), communal boundary maintenance, and
 *   educational transmission. Over the interval (roughly 500 BCE to present),
 *   extractiveness has risen as the boundary hardened against competing
 *   claims (early Christianity, Islam, modern secular challenges), theater
 *   has increased as ritual performance of chosenness partly substitutes for
 *   substantive covenantal life, and suppression has intensified as the
 *   interpretive fence against rival readings grew.
 *
 * KEY AGENTS:
 *   - jewish_people: Primary beneficiary (institutional/identity_locked) — receives covenantal identity, land promise, chosenness
 *   - rabbinic_tradition: Agenda setter (institutional/analytical) — interprets, enforces, transmits the exclusivity reading
 *   - ishmaelite_claimants: Primary victim (powerless/trapped) — excluded from covenantal promises by definitional fiat
 *   - islamic_tradition: Primary victim (institutional/identity_locked) — claims Abrahamic inheritance through Ishmael, structurally opposed
 *   - christian_supersessionist_tradition: Excluded observer (institutional/analytical) — claims fulfillment/replacement, not direct exclusion from this reading's boundary
 *   - secular_scholars: Observer (analytical/analytical) — analyzes the boundary without covenantal stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.75).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.7).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Abrahamic Covenant: Isaac-Exclusive Reading").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious/theological/institutional").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '1529e47c-51c7-4300-aae5-1f4ba1a56e8c').
narrative_ontology:cs_kernel_codification('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', fixed_text).
narrative_ontology:cs_authority_grounding('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', lineage).
narrative_ontology:cs_interpretation_layer_present('1529e47c-51c7-4300-aae5-1f4ba1a56e8c').
narrative_ontology:cs_reading_relation('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_reading_relation('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', foundational, covenant_exclusive_to_isaac_line).
narrative_ontology:cs_axiom_status(covenant_exclusive_to_isaac_line, holdable).
narrative_ontology:cs_axiom_grounding('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', covenant_exclusive_to_isaac_line, deontological).
narrative_ontology:cs_axiom('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', foundational, ishmael_excluded_from_covenantal_promise).
narrative_ontology:cs_axiom_status(ishmael_excluded_from_covenantal_promise, holdable).
narrative_ontology:cs_axiom_grounding('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', ishmael_excluded_from_covenantal_promise, deontological).
narrative_ontology:cs_reference_frame('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', sinai_covenantal_framework).
narrative_ontology:cs_drift_state('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', contemporary_interfaith_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1529e47c-51c7-4300-aae5-1f4ba1a56e8c', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_people).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_tradition).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, divine_election_of_isaac_line).
narrative_ontology:constraint_vindicates(abrahamic_covenant__isaac_covenant_reading, covenantal_particularism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives covenantal identity, chosenness, land promise, and halakhic framework through the Isaac-exclusive reading. The boundary defines who is 'in' the people of Israel. Exit means leaving the covenantal community — conversion out, assimilation, or joining another Abrahamic tradition — which is experienced as existential rupture, not mere preference change. The coordination benefit is real (survival, continuity, ethical framework) but the identity lock makes the extraction on others invisible from inside.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, jewish_people, beneficiary,
    organized, generational, identity_locked, global).

% Interprets, enforces, and transmits the Isaac-exclusive reading through midrash, halakha, liturgy, and education. Controls conversion standards, defines who counts as Jewish, and maintains the interpretive fence against rival readings. Collects institutional authority (not material rents) from administering the boundary. Exit is analytical — a scholar could adopt a different hermeneutic — but the institutional role makes this professionally and communally costly.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, rabbinic_tradition, agenda_setter,
    institutional, civilizational, analytical, global).

% Descendants of Ishmael (in biblical genealogy) and pre-Islamic Arabian claimants to Abrahamic inheritance. Excluded by the Genesis 17:19-21 reading's definitional fiat: 'in Isaac shall your seed be called.' No exit from the exclusion — the constraint defines them out of covenantal standing before they can contest it. Their structural position is trapped: the boundary is imposed from outside their framework.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, ishmaelite_claimants, payer,
    powerless, generational, trapped, global).

% Claims Abrahamic covenant through Ishmael → Muhammad prophetic succession. The Qur'an re-narrates the binding (Ishmael, not Isaac) and presents the Isaac-exclusive reading as a textual corruption (tahrif). Bears the extractive weight of being structurally defined as 'outside' the covenant by the sibling reading. Exit means abandoning Islamic self-understanding — identity_locked. The constraint's persistence actively suppresses Islamic covenantal claims in Jewish and Christian discourse.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, islamic_tradition, payer,
    institutional, civilizational, identity_locked, global).

% Reads the Abrahamic covenant as fulfilled and superseded in Christ; the church becomes the new Israel. Not a direct victim of the Isaac-exclusive boundary (which they read as obsolete), but excluded from the intra-Jewish/Islamic contest over which lineage inherits. Their reading creates a third structural position that neither the Isaac nor Ishmael reading can fully accommodate. Would object to both exclusive lineage claims if present in the conversation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, christian_supersessionist_tradition, excluded,
    institutional, civilizational, analytical, global).

% Analyze the covenantal boundary as historical, literary, and sociological phenomenon. No covenantal stake. Their exit is analytical — they can change frameworks freely. They provide the external corroboration for the founding problem genealogy (see six_questions).
narrative_ontology:constraint_stakeholder(abrahamic_covenant__isaac_covenant_reading, secular_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains Jewish peoplehood, covenantal continuity, and halakhic coherence across diaspora and millennia by defining a clear, transmitted boundary of belonging (Isaac/Jacob line) against competing Abrahamic claims.
% TRANSFER_FUNCTION: Moves covenantal standing, land promise, chosenness, and interpretive authority from the Ishmaelite/Islamic line to the Isaac/Jewish line. The transfer is not material but ontological: who counts as the covenant people, who inherits the promises, who holds the hermeneutic keys.
% ABSENT_VOICES: Pre-rabbinic Second Temple groups that held more inclusive Abrahamic views (e.g., some Hellenistic Jewish writers, Jubilees' broader framing); early Islamic voices that engaged the textual dispute directly (Qur'anic correction narrative); modern liberal Jewish and Muslim thinkers who seek a post-exclusionary Abrahamic theology. They are excluded by the hardening of the interpretive fence in all three traditions.
% DISAPPEARANCE_RATIONALE: If the Isaac-exclusive reading vanished overnight, Jewish covenantal identity would lose its defining boundary against Islam and Christianity. The Jewish people would face a reconstitution crisis: on what basis does the covenant continue? Conversion standards, liturgy, and communal self-understanding would all shift. The Islamic tradition would lose its primary structural foil for its own Ishmael-centric claim. The Abrahamic triangle would collapse into a different configuration.
% FOUNDING_PROBLEM: How to maintain a distinct covenantal peoplehood (ethical monotheism, mitzvot, land) amid empire, exile, and competing universalist claims (Hellenism, Christianity, Islam) that absorb or erase particularist identity.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition attests the problem is live (ongoing exile, assimilation, antisemitism). Historical-critical scholars (e.g., Schiffman, Boyarin, Levenson) attest the particularist boundary was a Second Temple construction, not the original Abrahamic form — the founding problem is substantially a rabbinic response to historical pressure, not the kernel's own logic. Islamic tradition attests the problem is superseded (Muhammad restores the inclusive Abrahamic way). No single corroborator outside the beneficiary set confirms the problem remains live in its original form.
narrative_ontology:disappearance_verdict(abrahamic_covenant__isaac_covenant_reading, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__isaac_covenant_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__isaac_covenant_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.75) is high because the constraint denies covenantal standing to an entire lineage and its civilizational heirs — a totalizing exclusion, not a marginal cost. Suppression (0.7) is substantial: the boundary is maintained by active interpretive labor (midrashic fence-building, halakhic conversion standards, liturgical reinforcement), not passive drift. Theater (0.4) reflects that chosenness discourse and ritual performance partly substitute for the lived covenantal obligations they signify. Accessibility collapse (0.8) is high: once the Isaac-exclusive frame is accepted, alternative Abrahamic framings (Ishmael-inclusive, universalist) appear as category errors rather than live options. Resistance (0.5) is moderate: rival readings persist (Islamic, Christian, secular) but cannot penetrate the interpretive enclosure from outside. The measurement series share a single time grid (0, 500, 1000, 1500, 2000, 2500) so the engine samples all metrics at the same points.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat, the constraint is a rope (pure coordination: Jewish survival requires this boundary). From the Islamic seat, it is a snare (pure extraction: the boundary exists to deny their prophetic lineage). From the Jewish people seat, it is a tangled rope (genuine coordination of peoplehood entangled with the cost of excluding cousins). The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the generating model's structural assessment.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic tradition (agenda_setter) sits at d ≈ 0.1 — it administers the boundary and collects institutional authority from it. The Jewish people (beneficiary) sit at d ≈ 0.2 — they receive identity and continuity but bear the costs of boundary maintenance (persecution risk, particularist obligations). Ishmaelite claimants and Islamic tradition (payers) sit at d ≈ 0.9 — they bear the full extractive weight of exclusion with no exit from the categorization (identity_locked). Christian supersessionists are excluded from this reading's beneficiary/victim structure entirely — they operate a different reading of the same kernel. Secular scholars are analytical observers (d = 0.5 by default). The engine derives these from beneficiary/victim declarations + exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (covenantal particularism as vehicle for ethical monotheism) remains contested — rabbinic tradition says live, historical-critical scholarship says dead, Islamic tradition says superseded. The constraint persists despite the founding problem's contested status because the coordination function (Jewish continuity) has become self-justifying. No single beneficiary captures the extraction; the arrangement is maintained by distributed institutional inertia, not concentrated rent-seeking. This prevents mislabeling as pure snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exclusion_mechanism_ambiguity,
    'Is the exclusion of Ishmael a textual necessity of Genesis 17:19-21 or an interpretive choice that serves institutional boundary maintenance?',
    'Comparative philological analysis of the Hebrew text alongside early interpretive traditions (Second Temple, rabbinic, patristic, early Islamic) to trace when and how the exclusivity reading hardened.',
    'If textual necessity, the constraint approaches mountain-like fixity for adherents; if interpretive choice, the extraction is contingent on hermeneutical decisions that could shift, reclassifying toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_ambiguity, conceptual, 'Whether Ishmael''s exclusion is structurally required by the kernel or constructed by the reading.').

omega_variable(
    coordination_extraction_boundary,
    'Does Jewish covenantal continuity genuinely require the exclusion of Ishmael''s line, or could the coordination function (identity, peoplehood, mitzvot) operate with a more inclusive Abrahamic framing?',
    'Historical sociology of Jewish communities that maintained continuity without hard Ishmael-exclusion (e.g., certain medieval Andalusian, Ottoman, or modern liberal communities) compared to those that hardened the boundary.',
    'If exclusion is not functionally necessary for coordination, the constraint is a snare using coordination as cover; if necessary, it remains a tangled_rope with genuine coordination-extraction entanglement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the coordination function structurally depends on the asymmetric extraction.').

omega_variable(
    kernel_reading_frame,
    'This constraint is the isaac_covenant_reading of the abrahamic_covenant kernel. Sibling readings: ishmael_covenant_reading, christian_supersessionist_reading. What structural elements do the readings contest?',
    'Map each reading''s beneficiary/victim sets, claimed_type, and axioms to identify the precise structural disagreement locus (exclusion scope, authority grounding, fulfillment status).',
    'Clarifies whether the kernel supports multiple stable constraints (coexistence) or forces foreclosure between readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame, conceptual, 'Commitment-system framing: kernel identity, reading identity, and sibling contestation structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t0, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.3).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.35).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t1000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.38).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t1500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.39).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t2000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_tr_t2500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2500, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_tr_t2500, observed).

% Extraction over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t0, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.62).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.68).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t1000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.72).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t1500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.74).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t2000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_be_t2500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2500, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_be_t2500, observed).

% Suppression requirement over time
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t0, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t1000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.68).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t1500, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.69).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t2000, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_su_t2500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_su_t2500, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=2500
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_01, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(class), 0, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_01, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_02, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(class), 2500, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_02, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_03, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(individual), 0, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_03, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_04, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(individual), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_04, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_05, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(organizational), 0, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_05, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_06, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(organizational), 2500, 0.8).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_06, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_07, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(structural), 0, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_07, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_08, abrahamic_covenant__isaac_covenant_reading, accessibility_collapse(structural), 2500, 0.85).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_08, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_09, abrahamic_covenant__isaac_covenant_reading, resistance(class), 0, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_09, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_10, abrahamic_covenant__isaac_covenant_reading, resistance(class), 2500, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_10, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_11, abrahamic_covenant__isaac_covenant_reading, resistance(individual), 0, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_11, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_12, abrahamic_covenant__isaac_covenant_reading, resistance(individual), 2500, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_12, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_13, abrahamic_covenant__isaac_covenant_reading, resistance(organizational), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_13, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_14, abrahamic_covenant__isaac_covenant_reading, resistance(organizational), 2500, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_14, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_15, abrahamic_covenant__isaac_covenant_reading, resistance(structural), 0, 0.35).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_15, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_16, abrahamic_covenant__isaac_covenant_reading, resistance(structural), 2500, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_16, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_17, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(class), 0, 0.3).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_17, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_18, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(class), 2500, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_18, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_19, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(individual), 0, 0.25).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_19, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_20, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(individual), 2500, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_20, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_21, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(organizational), 0, 0.35).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_21, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_22, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(organizational), 2500, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_22, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_23, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(structural), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_23, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_24, abrahamic_covenant__isaac_covenant_reading, stakes_inflation(structural), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_24, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_25, abrahamic_covenant__isaac_covenant_reading, suppression(class), 0, 0.45).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_25, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_26, abrahamic_covenant__isaac_covenant_reading, suppression(class), 2500, 0.65).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_26, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_27, abrahamic_covenant__isaac_covenant_reading, suppression(individual), 0, 0.4).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_27, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_28, abrahamic_covenant__isaac_covenant_reading, suppression(individual), 2500, 0.6).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_28, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_29, abrahamic_covenant__isaac_covenant_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_29, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_30, abrahamic_covenant__isaac_covenant_reading, suppression(organizational), 2500, 0.7).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_30, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_31, abrahamic_covenant__isaac_covenant_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_31, observed).
narrative_ontology:measurement(abrahamic_covenant__isaac_covenant_reading_grid_32, abrahamic_covenant__isaac_covenant_reading, suppression(structural), 2500, 0.75).
narrative_ontology:measurement_basis(abrahamic_covenant__isaac_covenant_reading_grid_32, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.08).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, land_promise_constraint).

% DUAL FORMULATION NOTE:
% This story is one member of the abrahamic_covenant constraint family. The three readings (isaac_covenant_reading, ishmael_covenant_reading, christian_supersessionist_reading) plus the land_promise_constraint form a linked set where each reading's ε and beneficiary/victim structure differ substantially. The isaac_reading's exclusivity clause (Gen 17:19-21) is cited as evidence by the christian_supersessionist_reading for its replacement claim, creating a structural influence edge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, institutional, 0.15).
constraint_indexing:directionality_override(abrahamic_covenant__isaac_covenant_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
