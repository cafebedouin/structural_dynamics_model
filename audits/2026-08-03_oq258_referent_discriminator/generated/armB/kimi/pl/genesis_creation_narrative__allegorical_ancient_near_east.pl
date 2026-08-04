% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics
 *
 * SUMMARY:
 *   The constraint is the interpretive regime that reads Genesis 1-2 as
 *   Ancient Near Eastern mythopoetic literature rather than as historical
 *   chronicle or scientific framework. It coordinates mainline theological
 *   and scholarly communities by decoupling the text from adjudication over
 *   cosmology and biology. As a kernel reading
 *   (allegorical_ancient_near_east), it is one of three structurally distinct
 *   constraints derived from the genesis_creation_narrative kernel,
 *   differentiated by textual ontology and epistemic grounding. The
 *   constraint exhibits low extraction and low suppression because it does
 *   not enforce itself on non-adherents; it functions as coordination for
 *   those who adopt it.
 *
 * KEY AGENTS:
 *   - biblical_scholars_ane: Agenda-setter (institutional/global) — establishes ANE comparative method and curricular norms
 *   - mainline_religious_communities: Beneficiary (organized/global) — gains a viable hermeneutic that preserves scriptural engagement without scientific conflict
 *   - conservative_religious_communities: Excluded (organized/global) — holds competing literal reading, not party to this constraint
 *   - secular_academics: Observer (institutional/global) — studies the hermeneutical shift as instance of religious adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.12).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.08).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.12).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '35591604-090c-4e60-b244-bc4f1024c19a').
narrative_ontology:cs_kernel_codification('35591604-090c-4e60-b244-bc4f1024c19a', fixed_text).
narrative_ontology:cs_authority_grounding('35591604-090c-4e60-b244-bc4f1024c19a', expertise).
narrative_ontology:cs_interpretation_layer_present('35591604-090c-4e60-b244-bc4f1024c19a').
narrative_ontology:cs_reading_relation('35591604-090c-4e60-b244-bc4f1024c19a', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('35591604-090c-4e60-b244-bc4f1024c19a', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('35591604-090c-4e60-b244-bc4f1024c19a', foundational, text_as_ane_mythopoetry).
narrative_ontology:cs_axiom_status(text_as_ane_mythopoetry, holdable).
narrative_ontology:cs_axiom_grounding('35591604-090c-4e60-b244-bc4f1024c19a', text_as_ane_mythopoetry, empirically_contingent).
narrative_ontology:cs_axiom('35591604-090c-4e60-b244-bc4f1024c19a', foundational, scriptural_decoupling_from_science).
narrative_ontology:cs_axiom_status(scriptural_decoupling_from_science, holdable).
narrative_ontology:cs_axiom_grounding('35591604-090c-4e60-b244-bc4f1024c19a', scriptural_decoupling_from_science, conventional).
narrative_ontology:cs_reference_frame('35591604-090c-4e60-b244-bc4f1024c19a', ane_mythopoetic_norm).
narrative_ontology:cs_drift_state('35591604-090c-4e60-b244-bc4f1024c19a', contemporary_church_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('35591604-090c-4e60-b244-bc4f1024c19a', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, biblical_scholars_ane).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_communities).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, comparative_religion_method).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, narrative_theology_priority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and teaches the Ancient Near Eastern comparative method for reading Genesis, curates peer-reviewed scholarship identifying the text with ancient cosmologies, and sets seminary and university curricular norms for biblical interpretation.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, biblical_scholars_ane, agenda_setter,
    institutional, generational, mobile, global).

% Use the ANE mythopoetic reading in liturgy, adult education, and science-religion dialogue to maintain scriptural authority and theological engagement without conflict with evolutionary biology or modern cosmology.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_religious_communities, beneficiary,
    organized, biographical, constrained, global).

% Hold literal-historical readings of Genesis and reject the ANE mythopoetic classification; they circulate in separate institutional spheres and are not party to the scholarly consensus that sustains this constraint.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, conservative_religious_communities, excluded,
    organized, generational, mobile, global).

% Study the hermeneutical shift as a social and historical phenomenon, treating the non-literal reading as an instance of religious adaptation to modernity without adjudicating its theological validity.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_academics, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates biblical interpretation among communities seeking to maintain theological engagement with Genesis 1-2 without asserting historical-scientific claims about cosmology, biology, or human origins.
% TRANSFER_FUNCTION: Transfers interpretive authority and textual status from literal-historical reading communities to comparative literary and mainline theological frameworks; minimal material extraction.
% ABSENT_VOICES: Conservative evangelical and fundamentalist communities holding literal-historical readings are excluded from the scholarly guild and mainline liturgical spaces where this reading is normative; their counter-readings circulate in separate institutional spheres.
% DISAPPEARANCE_RATIONALE: Mainline seminaries, science-religion dialogue programs, and progressive liturgical communities depend on this reading to manage the text's authority; its disappearance would force a hermeneutical crisis and potential retreat into literalism or secular disengagement.
% FOUNDING_PROBLEM: How to preserve the theological and literary authority of Genesis 1-2 after modern historical criticism, comparative Ancient Near Eastern studies, and evolutionary science rendered straightforward literal-historical readings untenable in educated contexts.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians of the Ancient Near East and comparative religion scholars corroborate the literary parallels independently of theological interest; mainline denominational educators attest to the ongoing need for non-literal strategies to retain scriptural engagement.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.12, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.12) because the constraint does not materially extract from participants; it is an interpretive framework. Suppression is minimal (0.08) because literal alternatives are not actively suppressed—they simply coexist in different communities. Theater ratio is low (0.15): there is some performative maintenance of the framework in academic publishing and denominational statements, but the coordination is largely functional. Accessibility collapse is low (0.25) because the literal reading remains widely available. Resistance is mild (0.20) from conservative communities who reject the reading, but this resistance is directed at the scholarly guild rather than the constraint's own enforcement machinery.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (biblical scholars) experiences the constraint as a discovery of textual genre and comparative method. The beneficiary seat (mainline communities) experiences it as a relief from cognitive dissonance. The excluded seat (conservative communities) experiences it as a rival claim with no binding force. The engine will compute these seats differently: scholars and mainline communities as near-beneficiary directionality, conservative communities as outside the constraint's scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholars, mainline communities) derive coordination and legitimacy from the constraint; their directionality sits near the beneficiary end. There are no declared victims because the constraint does not extract from those it governs; excluded parties are outside the constraint rather than targets. The absence of victims and the low suppression profile support the rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling by clearly distinguishing its coordination function (preserving theological engagement) from any extraction function. It has no enforcement mechanism beyond scholarly consensus and curricular transmission. If the founding problem (science-religion conflict over Genesis) were dead, the constraint might become a piton; but corroboration indicates the problem remains live in global Christianity, so the coordination is still functional rather than inertial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'How would the classification change if this reading were evaluated as one of multiple competing readings of the same kernel rather than as a standalone constraint?',
    'Comparison with sibling constraint stories for literal_young_earth and theistic_evolutionary readings to identify which structural elements vary with reading choice.',
    'This reading''s low extractiveness and rope classification depend on decoupling the text from historical-scientific claims; sibling readings re-couple the text to empirical reality and show higher extraction or suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Commitment-system framing ambiguity for kernel reading location').

omega_variable(
    genre_empirical_status,
    'Is the ANE mythopoetic classification of Genesis 1-2 empirically established by comparative literary evidence, or is it a conventional scholarly construct?',
    'Further ANE archaeological and textual discoveries could corroborate or destabilize the comparative parallels; postcolonial critique could reframe the comparative method as imperial imposition.',
    'If the classification is primarily conventional, the constraint''s authority rests on scholarly consensus rather than discovered fact, making it more vulnerable to paradigm shifts in biblical studies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_empirical_status, empirical, 'Whether ANE genre classification is discovered or constructed').

omega_variable(
    dominion_normativity_residual,
    'Does the allegorical reading completely evacuate normative force from the dominion metaphor, or does retrieval theology preserve ethical claims within the mythopoetic frame?',
    'Analysis of ethical theology and ecological hermeneutics that draw on Genesis 1-2 despite the allegorical frame.',
    'If normative force persists, the constraint may carry more extraction than the pure allegorical frame suggests, as communities still feel bound by the text''s ethical imperatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dominion_normativity_residual, conceptual, 'Residual normativity in allegorical dominion reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.12).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 50, 0.14).
narrative_ontology:measurement(gene_tr_t100, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 50, 0.11).
narrative_ontology:measurement(gene_be_t100, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 100, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(genesis_creation_narrative__allegorical_ancient_near_east, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% Part of the genesis_creation_narrative constraint family. Decomposed per the epsilon-invariance principle because each reading (allegorical ANE, literal young-earth, theistic evolutionary) carries a distinct textual ontology, different beneficiary/victim structures, and different epistemic warrants, producing different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
