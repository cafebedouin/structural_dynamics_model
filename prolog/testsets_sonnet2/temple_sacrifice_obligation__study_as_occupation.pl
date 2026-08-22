% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Talmudic Study of Sacrificial Law as Fulfillment of Korbanot Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This story instantiates the study_as_occupation reading of the
 *   temple_sacrifice_obligation kernel: the claim, rooted in Hosea 14:3 and
 *   Talmudic dicta equating study of sacrificial law with its performance,
 *   that engaging the relevant halakhic texts (particularly tractates of
 *   Seder Kodashim) is not merely preparatory or archival but constitutes the
 *   obligation's live, present-tense discharge. This reading has become the
 *   dominant curricular framing in mainstream rabbinic education. The
 *   theater_ratio trend upward reflects growing institutional and liturgical
 *   ceremony around study-as-performance (formal siyyum completions,
 *   ritualized recitation of sacrificial passages in daily prayer) without a
 *   corresponding rise in extraction — this is a genuinely low-cost,
 *   low-coercion resolution to an intractable practical problem.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.22).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Talmudic Study of Sacrificial Law as Fulfillment of Korbanot Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, 'e4398116-0764-4567-af7d-42bd5d3cbe15').
narrative_ontology:cs_kernel_codification('e4398116-0764-4567-af7d-42bd5d3cbe15', fixed_text).
narrative_ontology:cs_authority_grounding('e4398116-0764-4567-af7d-42bd5d3cbe15', lineage).
narrative_ontology:cs_interpretation_layer_present('e4398116-0764-4567-af7d-42bd5d3cbe15').
narrative_ontology:cs_reading_relation('e4398116-0764-4567-af7d-42bd5d3cbe15', temple_sacrifice_obligation__study_as_archiving, forecloses).
narrative_ontology:cs_reading_relation('e4398116-0764-4567-af7d-42bd5d3cbe15', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_axiom('e4398116-0764-4567-af7d-42bd5d3cbe15', foundational, study_constitutes_legal_performance).
narrative_ontology:cs_axiom_status(study_constitutes_legal_performance, holdable).
narrative_ontology:cs_axiom_grounding('e4398116-0764-4567-af7d-42bd5d3cbe15', study_constitutes_legal_performance, conventional).
narrative_ontology:cs_axiom('e4398116-0764-4567-af7d-42bd5d3cbe15', secondary, obligation_remains_presently_dischargeable).
narrative_ontology:cs_axiom_status(obligation_remains_presently_dischargeable, holdable).
narrative_ontology:cs_axiom_grounding('e4398116-0764-4567-af7d-42bd5d3cbe15', obligation_remains_presently_dischargeable, conventional).
narrative_ontology:cs_reference_frame('e4398116-0764-4567-af7d-42bd5d3cbe15', temple_era_sacrificial_performance).
narrative_ontology:cs_drift_state('e4398116-0764-4567-af7d-42bd5d3cbe15', post_destruction_rabbinic_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e4398116-0764-4567-af7d-42bd5d3cbe15', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, yeshiva_study_institutions).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, observant_laity).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, torah_study_equals_sacrifice_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_occupation, oral_prayer_and_study_substitution_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Codifies and transmits the ruling (rooted in readings of Hosea 14:3, 'so we will render for bulls the offering of our lips,' and Talmudic statements that study of Temple service is equivalent to performing it) that engaging the sacrificial tractates constitutes the halakhically operative continuation of the mitzvah. Their institutional authority, curriculum centrality, and professional identity are built on this reading being sound; abandoning it would mean either declaring the obligation permanently unfulfillable or ceding ground to messianic-suspension readings that deprioritize the study tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class, agenda_setter,
    institutional, civilizational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholarly_class, beneficiary).

% Structure significant portions of the curriculum (Seder Kodashim and related tractates) around the premise that this study is not merely academic preparation but the present-tense discharge of a live commandment. This elevates the study's stakes and institutional funding case, since students and donors are told the learning itself performs a divine service rather than merely archiving one.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, yeshiva_study_institutions, beneficiary,
    organized, generational, mobile, global).

% Live under an obligation whose primary mode (animal sacrifice at a now-nonexistent Temple) is structurally impossible to perform. Under this reading, engaging with the relevant texts (in daily liturgy recitations of sacrificial passages, or through dedicated study) discharges the obligation without requiring restoration of the Temple, resolving what would otherwise be a standing state of unavoidable transgression or spiritual limbo.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, observant_laity, beneficiary,
    moderate, biographical, constrained, global).

% Hold that the obligation is suspended, not satisfied, pending literal Temple restoration, and that treating study as full occupation of the mitzvah risks dulling eschatological urgency and cultivating complacency about physical rebuilding. Their view is present in the tradition's margins (some kabbalistic and religious-Zionist streams) but is structurally sidelined by the study-as-occupation reading's dominance in mainstream rabbinic curricula.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, messianic_restorationist_factions, excluded,
    moderate, civilizational, identity_locked, global).

% Examine the historical development of the study-substitution doctrine, tracing its emergence in response to the practical crisis of Temple destruction and its consolidation across geonic and later rabbinic literature. Neither party to the doctrine's religious stakes nor bound by its institutional incentives.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_occupation, textual_critical_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the practical crisis of a communally binding obligation whose sole prescribed mode of performance became physically impossible in 70 CE — it prevents the tradition from either declaring mass permanent transgression or abandoning the mitzvah category altogether, and gives the community a continuously performable substitute that requires no infrastructure beyond texts and teachers.
% TRANSFER_FUNCTION: Moves religious and institutional legitimacy toward text-based scholarship and its custodians (yeshivot, rabbinic authorities) and away from any priestly or infrastructural apparatus that sacrificial performance would require; moves psychological and spiritual resolution toward observant laity who would otherwise carry an unfulfillable obligation.
% ABSENT_VOICES: Messianic-restorationist factions who hold the obligation is suspended rather than satisfied are structurally outside the mainstream curricular conversation; a historical-critical voice noting the doctrine emerged as a response to crisis, not as a pre-existing halakhic principle, is also largely absent from the traditional framing though present in academic study.
% DISAPPEARANCE_RATIONALE: If the study-as-occupation reading were repudiated, the sacrificial tractates would lose their claim to be a live, obligation-discharging practice and would revert to either archival status (study_as_archiving) or the obligation would sit as suspended and unfulfilled (messianic_suspension) — curricular centrality, communal psychological resolution, and the standing of Kodashim study within the yeshiva system would all shift.
% FOUNDING_PROBLEM: The Temple's destruction in 70 CE made the primary mode of the sacrificial commandments (animal offerings at a specific site by an active priesthood) structurally impossible to perform, leaving the community with a binding obligation and no means of literal compliance.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities and yeshiva institutions attest the founding problem remains live and is actively solved by study (they are also the reading's chief beneficiaries). Historical-critical scholars outside the beneficiary set corroborate that the doctrine developed specifically as a post-destruction adaptation, supporting the reading's account of its own origin, but do not corroborate that study fully discharges the obligation in a normative sense — that remains a claim internal to the tradition, contested by messianic-restorationist voices who hold the problem is not solved but merely deferred.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_occupation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_occupation, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.12) because under this reading no party is structurally deprived of anything by the arrangement — the obligation is fulfilled through freely undertaken study rather than compelled tribute, and no victim class exists (the schema's tangled_rope/snare gates correctly do not apply). Suppression is modest (0.22): the reading does exert some pressure against the messianic-suspension alternative by making it institutionally and psychologically costly to view the obligation as merely deferred, but no coercive apparatus enforces the study-as-occupation view over dissenting streams. Accessibility_collapse is moderate-high (0.65) since, once internalized, the study framing genuinely displaces the intuitive alternative reading (unfulfillable obligation) for most adherents. Resistance is low (0.15), consistent with a genuinely low-friction coordination solution — messianic-restorationist pushback exists but is a minority position, not an active contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic scholarly class and yeshiva institutions are near-full beneficiaries: their authority and curricular centrality depend on this reading being sound, and their exit is identity-locked or institutionally anchored. Observant laity are genuine beneficiaries too — this reading resolves what would otherwise be a standing unfulfillable obligation, converting a source of guilt or anxiety into an achievable devotional practice, at essentially no cost to them. No agent occupies a target/victim position under this reading, which is the structural point of the coordination function it performs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction rendering literal sacrifice impossible) is permanent, not resolved — the mandate is not obsolete, it has been re-routed onto a substitute performable mode. This is not mandatrophy in the extractive sense (a mandate persisting past its function to benefit an agenda_setter) because the substitute mode (study) genuinely and continuously discharges the coordination function (giving the community a livable relationship to an otherwise-impossible commandment) rather than persisting as empty ritual. The rising theater_ratio bears watching, however: if ceremonial elaboration around study-completion continues to outpace the study's substantive content, T17-style extraction accumulation could eventually emerge from within a currently benign arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_substitution_doctrinal_status,
    'Is study-as-occupation a genuine halakhic mechanism of mitzvah-fulfillment recognized within the authoritative chain of transmission, or a pastorally motivated accommodation that has hardened into doctrine without ever being formally adjudicated as equivalent to performance?',
    'Comparative analysis of the doctrine''s treatment across geonic responsa, major codifiers (Rambam, Shulchan Aruch commentaries), and later authorities — tracking whether the equivalence claim is asserted as formal legal substitution or as devotional consolation language that later interpreters over-read as substitution.',
    'If the doctrine is formal substitution, this reading''s classification as a genuine low-extraction coordination solution is well-grounded; if it is consolation language retroactively elevated to doctrinal status, the reading functions closer to a legitimizing narrative for the scholarly class''s institutional centrality, which would push the classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_substitution_doctrinal_status, conceptual, 'Whether study-as-fulfillment is formally adjudicated halakha or elevated devotional rhetoric.').

omega_variable(
    kernel_reading_selection_pressure,
    'Why has study_as_occupation become the dominant curricular reading relative to study_as_archiving and messianic_suspension, and is that dominance a function of doctrinal merit or of which reading best serves the institutional interests of the scholarly class that transmits the tradition?',
    'Historical tracing of when and where each reading gained institutional traction, cross-referenced against the material interests (curricular funding, communal status) of the transmitting institutions at each period.',
    'If dominance tracks institutional interest rather than independent doctrinal argument, the low ε authored here may understate a subtler extraction: the reading''s function in securing scholarly-class status rather than purely resolving the laity''s practical/spiritual problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether curricular dominance of this reading tracks doctrine or institutional self-interest.').

omega_variable(
    messianic_urgency_dampening,
    'Does normalizing study as full occupation of the obligation reduce communal urgency toward messianic restoration and literal Temple rebuilding, and if so, is that dampening a feature (psychological stability) or a cost (theological complacency) of this reading?',
    'Comparative sociological study of restorationist activism and eschatological engagement in communities where study-as-occupation is emphasized versus communities emphasizing messianic-suspension.',
    'If dampening is substantial, the reading''s low extractiveness for laity may be offset by a diffuse cost to the tradition''s own restorationist theology — a cost this reading''s own metrics do not capture since it is authored from the study_as_occupation reading''s own lights.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_urgency_dampening, preference, 'Whether this reading trades eschatological urgency for present-day psychological resolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(temp_tr_t0, projected).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 400, 0.18).
narrative_ontology:measurement_basis(temp_tr_t400, projected).
narrative_ontology:measurement(temp_tr_t900, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 900, 0.21).
narrative_ontology:measurement_basis(temp_tr_t900, projected).
narrative_ontology:measurement(temp_tr_t1400, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1400, 0.24).
narrative_ontology:measurement_basis(temp_tr_t1400, projected).
narrative_ontology:measurement(temp_tr_t1700, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1700, 0.26).
narrative_ontology:measurement_basis(temp_tr_t1700, projected).
narrative_ontology:measurement(temp_tr_t1950, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 1950, 0.28).
narrative_ontology:measurement_basis(temp_tr_t1950, projected).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(temp_be_t0, projected).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 400, 0.09).
narrative_ontology:measurement_basis(temp_be_t400, projected).
narrative_ontology:measurement(temp_be_t900, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 900, 0.1).
narrative_ontology:measurement_basis(temp_be_t900, projected).
narrative_ontology:measurement(temp_be_t1400, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1400, 0.11).
narrative_ontology:measurement_basis(temp_be_t1400, projected).
narrative_ontology:measurement(temp_be_t1700, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1700, 0.11).
narrative_ontology:measurement_basis(temp_be_t1700, projected).
narrative_ontology:measurement(temp_be_t1950, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement_basis(temp_be_t1950, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_obligation__study_as_occupation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
