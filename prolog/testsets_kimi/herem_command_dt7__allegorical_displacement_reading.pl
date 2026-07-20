% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__allegorical_displacement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Herem as Allegorical Displacement Reading (Deuteronomy 7)
 *   domain: biblical hermeneutics/religious ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the allegorical_displacement_reading
 *   of the herem_command_dt7 kernel: the command in Deuteronomy 7 to destroy
 *   the nations is read typologically, so that nations become spiritual
 *   enemies (sin, temptation) and conquest becomes interior moral warfare.
 *   The reading relocates the entire constraint to the internal spiritual
 *   domain, collapsing the victim set to abstract vices and eliminating
 *   extractiveness on interethnic relations. It is one of three sibling
 *   readings; the others are contextual_supersession_reading and
 *   durable_separation_reading.
 *
 * KEY AGENTS:
 *   - devotional_readers (beneficiary/organized/mobile) â gain a non-violent hermeneutic
 *   - spiritual_expositors (agenda_setter/moderate/mobile) â maintain the allegorical method
 *   - historical_critical_scholars (excluded/institutional/analytical) â resist the displacement of historical referents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__allegorical_displacement_reading, 0.08).
domain_priors:suppression_score(herem_command_dt7__allegorical_displacement_reading, 0.12).
domain_priors:theater_ratio(herem_command_dt7__allegorical_displacement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(herem_command_dt7__allegorical_displacement_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__allegorical_displacement_reading, rope).
narrative_ontology:human_readable(herem_command_dt7__allegorical_displacement_reading, "Herem as Allegorical Displacement Reading (Deuteronomy 7)").
narrative_ontology:topic_domain(herem_command_dt7__allegorical_displacement_reading, "biblical hermeneutics/religious ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__allegorical_displacement_reading, '905f638e-3ad2-4c05-ba76-686c6ecc9f74').
narrative_ontology:cs_kernel_codification('905f638e-3ad2-4c05-ba76-686c6ecc9f74', fixed_text).
narrative_ontology:cs_authority_grounding('905f638e-3ad2-4c05-ba76-686c6ecc9f74', lineage).
narrative_ontology:cs_interpretation_layer_present('905f638e-3ad2-4c05-ba76-686c6ecc9f74').
narrative_ontology:cs_reading_relation('905f638e-3ad2-4c05-ba76-686c6ecc9f74', herem_command_dt7__durable_separation_reading, forecloses).
narrative_ontology:cs_reading_relation('905f638e-3ad2-4c05-ba76-686c6ecc9f74', herem_command_dt7__contextual_supersession_reading, coexists_with).
narrative_ontology:cs_axiom('905f638e-3ad2-4c05-ba76-686c6ecc9f74', foundational, typological_nation_referent).
narrative_ontology:cs_axiom_status(typological_nation_referent, holdable).
narrative_ontology:cs_axiom_grounding('905f638e-3ad2-4c05-ba76-686c6ecc9f74', typological_nation_referent, theological).
narrative_ontology:cs_axiom('905f638e-3ad2-4c05-ba76-686c6ecc9f74', foundational, interior_combat_mandate).
narrative_ontology:cs_axiom_status(interior_combat_mandate, holdable).
narrative_ontology:cs_axiom_grounding('905f638e-3ad2-4c05-ba76-686c6ecc9f74', interior_combat_mandate, deontological).
narrative_ontology:cs_reference_frame('905f638e-3ad2-4c05-ba76-686c6ecc9f74', interior_moral_warfare).
narrative_ontology:cs_drift_state('905f638e-3ad2-4c05-ba76-686c6ecc9f74', modern_historical_critical_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('905f638e-3ad2-4c05-ba76-686c6ecc9f74', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__allegorical_displacement_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__allegorical_displacement_reading, devotional_readers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a non-violent hermeneutical framework that interprets Deuteronomic herem as an interior typology for moral struggle against sin and temptation, preserving scriptural authority without endorsing ethnic violence. Exit means adopting literal-historical, supersessionist, or rejectionist readings.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, devotional_readers, beneficiary,
    organized, generational, mobile, global).

% Maintain the allegorical method and typological tradition, instructing communities to map the commanded destruction of nations onto the eradication of vices within the soul. They sustain the interpretive labor without capturing material rents from those they teach.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, spiritual_expositors, agenda_setter,
    moderate, generational, mobile, global).

% Academic biblical scholars who read Deuteronomy 7 as reflecting ancient Near Eastern conquest ideology with specific ethnic referents. Their methodological commitments are structurally excluded from the allegorical community's interpretive frame; they would contest the displacement of historical referents into abstract psychology.
narrative_ontology:constraint_stakeholder(herem_command_dt7__allegorical_displacement_reading, historical_critical_scholars, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates how believing communities read biblical conquest commands by mapping ethnic referents to interior spiritual vices, solving the collective hermeneutical problem of retaining scriptural authority without justifying inter-group violence.
% TRANSFER_FUNCTION: Moves the semantic content of nations and conquest from the domain of ethnic relations to the domain of individual moral psychology, transferring the believer's duty from external warfare to internal ascetic discipline.
% ABSENT_VOICES: Historical-critical scholars and literalist communities who affirm the ethnic referentiality of the nations are methodologically absent from the allegorical frame; they would argue that the typological reading evacuates the text of historical specificity and material violence.
% DISAPPEARANCE_RATIONALE: Devotional communities that structure moral self-cultivation around this typology would lose their primary textual anchor for spiritual warfare; without it, they would need to adopt literal, supersessionist, or rejectionist frameworks, rearranging pastoral teaching and ethical formation.
% FOUNDING_PROBLEM: Biblical texts present divinely mandated conquest of specific ethnic groups; later communities holding these texts as authoritative need a framework that preserves the text without mandating contemporary ethnic violence or boundary maintenance.
% FOUNDING_PROBLEM_CORROBORATION: Supersessionist theologians and historical-critical ethicists outside the allegorical tradition attest that the ethical problem of herem remains unresolved for communities of scripture; secular biblical scholars confirm the historical reality of the conquest commands, corroborating that the founding tension is genuine.
narrative_ontology:disappearance_verdict(herem_command_dt7__allegorical_displacement_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__allegorical_displacement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__allegorical_displacement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__allegorical_displacement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__allegorical_displacement_reading, 0.08, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.08 because the reading deliberately displaces ethnic violence and has no human victims; the only cost is voluntary ascetic self-discipline. Suppression is low (0.12) because the reading does not actively suppress alternatives, though it excludes historical-critical method from its own communal frame. Theater ratio is low (0.10) because the allegorical function is genuine interpretive work rather than performative maintenance. Accessibility collapse (0.45) reflects that once the typological frame is adopted, literal alternatives lose internal legitimacy, though they remain widely available externally. Resistance (0.30) captures the sustained scholarly resistance from historical-critical exegesis.
 *
 * PERSPECTIVAL GAP:
 *   The devotional_reader seat experiences the constraint as benevolent coordination (a way to keep scripture without violence), while the historical_critical_scholar seat experiences it as a hermeneutical evacuation that suppresses historical meaning. The engine computes this divergence from the same structural data: low extractiveness and low suppression produce rope from the beneficiary side, but the excluded seat's analytical distance yields a different computed classification if seated there.
 *
 * DIRECTIONALITY LOGIC:
 *   Devotional_readers are beneficiaries (low d) because the constraint subsidizes their moral framework. Spiritual_expositors are near-symmetric (moderate d) because they maintain the frame without capturing material rents. Historical_critical_scholars are effectively excluded rather than targeted; no extraction flows to them, but their methodological alternative is structurally ignored within the allegorical community.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by distinguishing the allegorical reading from the durable_separation reading (which would be snare or tangled_rope due to ethnic victims) and from the contextual_supersession reading (which may be scaffold). The low extractiveness and absence of human victims keep this reading in the rope domain, while the kernel contest ensures it is not mistaken for a natural-law mountain or an atrophied piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    allegorical_extraction_ambiguity,
    'Does the allegorical displacement of herem extract ascetic labor or psychological suppression from devotional readers despite having no ethnic victims?',
    'Phenomenological and psychological study of believers formed by spiritual warfare frameworks; comparison of guilt, anxiety, and identity-fusion metrics against non-allegorical devotional communities.',
    'If measurable psychological or ascetic extraction exists, the constraint may reclassify as tangled_rope with devotional_readers as victims; if absent, it remains a low-extraction rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(allegorical_extraction_ambiguity, empirical, 'Whether internalized spiritual warfare extracts from believers').

omega_variable(
    committer_kernel_relation,
    'Is the allegorical reading''s foreclosure of durable_separation grounded in a logically necessary hermeneutical premise, or a conventionally stabilized interpretive choice?',
    'Analysis of whether the typological premise is entailed by the kernel''s formal properties or is a selectable reading strategy among others.',
    'If conventionally stabilized, the forecloses edge to durable_separation is a community boundary rather than a logical necessity, and the kernel may admit additional coexisting readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_kernel_relation, conceptual, 'Nature of the foreclosure relation to sibling readings').

omega_variable(
    kernel_reading_epsilon_invariance,
    'Does the allegorical_displacement reading''s epsilon invariance hold if evaluated through the observable of communal behavior rather than textual meaning?',
    'Decompose into behavioral and semantic constraints if observables diverge; otherwise treat as single constraint.',
    'If behavioral and semantic evaluations yield different epsilon values, this reading must split into two linked constraints per the epsilon-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_epsilon_invariance, conceptual, 'Observable decomposition test for this kernel reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__allegorical_displacement_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_allegorical_tr_t0, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(herem_allegorical_tr_t10, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(herem_allegorical_tr_t20, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(herem_allegorical_tr_t30, herem_command_dt7__allegorical_displacement_reading, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(herem_allegorical_be_t0, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(herem_allegorical_be_t10, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 10, 0.04).
narrative_ontology:measurement(herem_allegorical_be_t20, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 20, 0.06).
narrative_ontology:measurement(herem_allegorical_be_t30, herem_command_dt7__allegorical_displacement_reading, base_extractiveness, 30, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(herem_command_dt7__allegorical_displacement_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__allegorical_displacement_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__allegorical_displacement_reading, durable_separation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the herem_command_dt7 kernel, decomposed from the colloquial label herem per the epsilon-invariance principle. The allegorical_displacement reading, contextual_supersession reading, and durable_separation reading are structurally distinct constraints with different epsilon values, victim sets, and types. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
