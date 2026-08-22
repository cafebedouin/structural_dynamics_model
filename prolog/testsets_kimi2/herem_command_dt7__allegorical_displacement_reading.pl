% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__allegorical_displacement_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: herem_command_dt7__allegorical_displacement_reading
 *   human_readable: Herem as Allegorical Spiritual Warfare (Typological Displacement Reading)
 *   domain: biblical_hermeneutics/religious_ethics/commitment_system
 *
 * SUMMARY:
 *   This constraint instantiates the allegorical_displacement_reading of the
 *   herem_command_dt7 kernel. In the patristic and subsequent allegorical
 *   traditions, Deuteronomy 7's command to devote the Hittites, Amorites, and
 *   Canaanites to destruction (herem) is read as typological: the 'nations'
 *   are placeholders for spiritual enemiesâsin, temptation, and demonic
 *   viceâwhile 'conquest' becomes the internal moral warfare of ascetic
 *   self-discipline. The reading relocates the entire constraint to the
 *   internal spiritual domain, collapsing human victimhood to abstract vices
 *   and eliminating interethnic extraction entirely. It functions as a
 *   hermeneutic and ethical coordination device that preserves scriptural
 *   authority without licensing ethnic violence.
 *
 * KEY AGENTS:
 *   - patristic_allegorical_tradition (institutional/identity_locked) â administers the typological interpretive key across generations
 *   - nonviolent_hermeneutic_community (organized/constrained) â benefits from ethical coordination and non-violent textual normativity
 *   - literalist_separation_adherents (organized/mobile) â excluded from this reading's normative framework; hold the competing durable-separation reading
 *   - historical_critical_scholars (institutional/analytical) â observe the constraint's function and genealogy from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.02).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.05).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Allegorical Spiritual Warfare (Typological Displacement Reading)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical_hermeneutics/religious_ethics/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '7023e1a5-ea2e-4c32-9459-4cb39e8db047').
narrative_ontology:cs_kernel_codification('7023e1a5-ea2e-4c32-9459-4cb39e8db047', fixed_text).
narrative_ontology:cs_authority_grounding('7023e1a5-ea2e-4c32-9459-4cb39e8db047', lineage).
narrative_ontology:cs_interpretation_layer_present('7023e1a5-ea2e-4c32-9459-4cb39e8db047').
narrative_ontology:cs_reading_relation('7023e1a5-ea2e-4c32-9459-4cb39e8db047', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('7023e1a5-ea2e-4c32-9459-4cb39e8db047', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('7023e1a5-ea2e-4c32-9459-4cb39e8db047', foundational, herem_typological_referent).
narrative_ontology:cs_axiom_status(herem_typological_referent, holdable).
narrative_ontology:cs_axiom_grounding('7023e1a5-ea2e-4c32-9459-4cb39e8db047', herem_typological_referent, theological).
narrative_ontology:cs_axiom('7023e1a5-ea2e-4c32-9459-4cb39e8db047', foundational, spiritual_sense_normative).
narrative_ontology:cs_axiom_status(spiritual_sense_normative, holdable).
narrative_ontology:cs_axiom_grounding('7023e1a5-ea2e-4c32-9459-4cb39e8db047', spiritual_sense_normative, theological).
narrative_ontology:cs_reference_frame('7023e1a5-ea2e-4c32-9459-4cb39e8db047', patristic_allegorical_sensus).
narrative_ontology:cs_drift_state('7023e1a5-ea2e-4c32-9459-4cb39e8db047', post_historical_critical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('7023e1a5-ea2e-4c32-9459-4cb39e8db047', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, nonviolent_hermeneutic_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the typological interpretive key that maps Deuteronomy 7's nations onto spiritual vices such as sin and temptation. Transmits the reading through liturgical, homiletic, and catechetical practice across centuries. Does not collect material rents; its continuity depends on the identity-fusion of the tradition with the allegorical method itself.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, patristic_allegorical_tradition, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Believers and communities who rely on the allegorical reading to preserve normative commitment to the text while rejecting ethnic violence. They receive ethical coordination and identity coherence; they bear only the ordinary costs of ascetic self-discipline and internal moral vigilance.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, nonviolent_hermeneutic_community, beneficiary,
    organized, generational, constrained, global).

% Hold the durable_separation reading and are structurally excluded from normative authority within allegorical communities. They would object that the text's plain sense refers to historical peoples and that allegory evacuates historical and moral accountability.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, literalist_separation_adherents, excluded,
    organized, generational, mobile, global).

% Analyze the herem text and its interpretive history from outside theological commitment. Document the allegorical reading's emergence, linguistic challenges, and social function without administering the constraint or bearing its costs.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critical_scholars, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates religious communities around a non-violent interpretation of herem by mapping the command onto internal spiritual enemies, preventing literal application against human ethnic groups while preserving scriptural normativity.
% TRANSFER_FUNCTION: Moves the locus of destructive moral agency inwardâfrom external ethnic targets to internal vices such as sin, temptation, and spiritual corruption within the believer.
% ABSENT_VOICES: Literalist and durable-separation advocates who read herem as prescribing or permitting historical group-boundary enforcement are structurally excluded from the normative conversation within allegorical communities; they would argue the text means what it says.
% DISAPPEARANCE_RATIONALE: Without this reading, the nonviolent hermeneutic community would lose its primary framework for maintaining textual normativity without ethnic violence. Believers would be forced toward contextual-supersession or literalist framings, rearranging the community's ethical boundaries, liturgical self-understanding, and relationship to scriptural authority.
% FOUNDING_PROBLEM: The literal herem command appears to mandate the destruction of entire ethnic groups, creating a moral crisis for communities that hold the text as normative yet reject ethnic violence and genocide.
% FOUNDING_PROBLEM_CORROBORATION: Critical biblical scholars and moral philosophers outside the allegorical tradition attest that the literal herem command poses an ongoing ethical crisis; they corroborate the founding problem's existence while frequently disputing the allegorical solution.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.02, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__allegorical_displacement_reading_tests).
:- end_tests(herem_command_dt7__allegorical_displacement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.02 because the only 'victims' of this constraint are abstract vices (sin, temptation), not human agents; no material or asymmetric psychic extraction from persons is structurally entailed. Suppression is 0.05 because persistence depends on tradition and identity-locked adherence rather than coercion. Theater_ratio rises to 0.25 at interval end because modern historical criticism has increased the performative labor required to maintain the allegorical frame as a credible reading. Accessibility_collapse is 0.82: within communities committed to the patristic spiritual sense, literal-historical alternatives are nearly inaccessible as normative options. Resistance is 0.25 from historical-critical and literalist quarters. The measurement series share a single time grid (0â1800 CE) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The allegorical community experiences this constraint as liberation from violent literalism and as a coherent spiritual identity. The literalist-excluded seat experiences it as a hermeneutical evasion that dissolves textual accountability. Historical-critical observers see a strategically adaptive reading that solves an ethical crisis by semantic displacement. The engine should compute these seats differently: the beneficiary seat sees low-directionality coordination, while excluded and observer seats register higher directionality toward a constructed rather than natural arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   The nonviolent_hermeneutic_community is the declared beneficiary, receiving ethical coordination and identity coherence; structural derivation assigns it a low d near the subsidy end. The patristic_allegorical_tradition is the agenda_setter, not a beneficiary in the extractive sense; without victim or beneficiary declaration, its d reverts toward the institutional fallback. There are no human payers. Abstract vices are rhetorically 'destroyed' but are not agents and do not appear in the victims array. The absence of a human victim set is structurally decisive: the constraint cannot compute as a snare or tangled rope regardless of observer disagreement.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the allegorical reading as a snare (no human victims), a false-summit mountain (no beneficiaries profit from a constructed 'natural law'), or a piton (the coordination function is live, not atrophied). The authored claim of rope is structurally grounded: the reading solves a genuine coordination problemâhow to maintain commitment to a violent text without enacting ethnic violenceâwithout asymmetric extraction from any human seat. If the allegorical frame were dead and maintained only by inertia, it would compute as piton; the live founding_problem_status and ongoing community reliance block that path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    herem_referent_ambiguity,
    'Are the ''nations'' in Deuteronomy 7 irreducibly historical-ethnic referents, or legitimately open to typological displacement?',
    'Comparative philology and history-of-exegesis analysis: determining whether the text''s original communicative content permits or resists allegorical relocation.',
    'If the historical referent is locked, the allegorical reading is a constructed evasion and may reclassify as scaffold or high-theater rope; if open, the reading remains a structurally valid coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(herem_referent_ambiguity, conceptual, 'Whether the kernel''s referent is historically fixed or semantically open.').

omega_variable(
    suppression_of_literal_sense,
    'Does the allegorical reading''s persistence within its communities depend on suppressing the literal-historical sense of the text?',
    'Ethnography of interpretive communities: measuring whether literal-historical readings are merely disagreed with or are structurally excluded from catechesis, liturgy, and communal discourse.',
    'If suppression of the literal sense is structurally required, authored suppression should be revised upward and the constraint may drift toward tangled rope; if the literal sense is freely acknowledged but deemed secondary, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_literal_sense, empirical, 'Whether the constraint actively suppresses alternative hermeneutic access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(here_tr_t300, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 300, 0.08).
narrative_ontology:measurement(here_tr_t600, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(here_tr_t900, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(here_tr_t1200, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1200, 0.15).
narrative_ontology:measurement(here_tr_t1500, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(here_tr_t1800, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 1800, 0.25).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(here_be_t300, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 300, 0.01).
narrative_ontology:measurement(here_be_t600, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 600, 0.02).
narrative_ontology:measurement(here_be_t900, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 900, 0.02).
narrative_ontology:measurement(here_be_t1200, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1200, 0.02).
narrative_ontology:measurement(here_be_t1500, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1500, 0.03).
narrative_ontology:measurement(here_be_t1800, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 1800, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(here_su_t300, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 300, 0.03).
narrative_ontology:measurement(here_su_t600, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 600, 0.03).
narrative_ontology:measurement(here_su_t900, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 900, 0.04).
narrative_ontology:measurement(here_su_t1200, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1200, 0.04).
narrative_ontology:measurement(here_su_t1500, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(here_su_t1800, herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 1800, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, durable_separation_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, contextual_supersession_reading).

% DUAL FORMULATION NOTE:
% The herem_command_dt7 kernel decomposes into three structurally distinct constraints per the epsilon-invariance principle. The allegorical_displacement_reading relocates referents to the spiritual domain, collapsing human victimhood and eliminating interethnic extraction. The durable_separation_reading maintains ethnic or group referents and encodes bounded identity. The contextual_supersession_reading treats the ethnic referent as historically bounded and superseded. Each carries a distinct epsilon, stakeholder topology, and victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
