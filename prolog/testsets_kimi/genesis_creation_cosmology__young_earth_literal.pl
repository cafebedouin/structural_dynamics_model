% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__young_earth_literal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__young_earth_literal, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: genesis_creation_cosmology__young_earth_literal
 *   human_readable: Young Earth Literalist Creationism (24-Hour Days, ~6â10k Years)
 *   domain: religious/theological/educational
 *
 * SUMMARY:
 *   This constraint is the young_earth_literal reading of the
 *   genesis_creation_cosmology kernel. The shared kernel is the text of
 *   Genesis 1â2, which supports multiple structurally distinct readings.
 *   This reading asserts that the text describes six literal 24-hour days of
 *   creation occurring approximately 6,000â10,000 years ago. It operates as
 *   a fixed-text commitment system with lineage authority, a dense
 *   interpretive apologetics layer, and active enforcement against
 *   evolutionary pedagogy and deep-time cosmology. The sibling
 *   readingsâtheistic_evolution and literary_frameworkâare structurally
 *   distinct constraints with different epsilon profiles and victim sets.
 *
 * KEY AGENTS:
 *   - biblical_literalist_institutions (agenda_setter/institutional/identity_locked) â administer and enforce the literal reading
 *   - literalist_communities (beneficiary/organized/identity_locked) â receive identity cohesion and boundary maintenance
 *   - science_educators (payer/moderate/constrained) â bear professional subordination of empirical method to textual authority
 *   - evolutionary_researchers (payer/moderate/constrained) â bear epistemic marginalization
 *   - students_in_literalist_schools (payer/powerless/trapped) â bear educational exclusion from consensus science
 *   - science_education_alliances (observer/institutional/analytical) â contest the constraint from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, 0.82).
domain_priors:suppression_score(genesis_creation_cosmology__young_earth_literal, 0.9).
domain_priors:theater_ratio(genesis_creation_cosmology__young_earth_literal, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, extractiveness, 0.82).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(genesis_creation_cosmology__young_earth_literal, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__young_earth_literal, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__young_earth_literal, "Young Earth Literalist Creationism (24-Hour Days, ~6â10k Years)").
narrative_ontology:topic_domain(genesis_creation_cosmology__young_earth_literal, "religious/theological/educational").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__young_earth_literal).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__young_earth_literal, 'f1eea970-5c46-407b-81d2-96d34a827aa3').
narrative_ontology:cs_kernel_codification('f1eea970-5c46-407b-81d2-96d34a827aa3', fixed_text).
narrative_ontology:cs_authority_grounding('f1eea970-5c46-407b-81d2-96d34a827aa3', lineage).
narrative_ontology:cs_interpretation_layer_present('f1eea970-5c46-407b-81d2-96d34a827aa3').
narrative_ontology:cs_reading_relation('f1eea970-5c46-407b-81d2-96d34a827aa3', genesis_creation_cosmology__theistic_evolution, forecloses).
narrative_ontology:cs_reading_relation('f1eea970-5c46-407b-81d2-96d34a827aa3', genesis_creation_cosmology__literary_framework, forecloses).
narrative_ontology:cs_axiom('f1eea970-5c46-407b-81d2-96d34a827aa3', foundational, genesis_literal_historical_narrative).
narrative_ontology:cs_axiom_status(genesis_literal_historical_narrative, holdable).
narrative_ontology:cs_axiom_grounding('f1eea970-5c46-407b-81d2-96d34a827aa3', genesis_literal_historical_narrative, theological).
narrative_ontology:cs_axiom('f1eea970-5c46-407b-81d2-96d34a827aa3', foundational, empirical_method_subordinate_to_text).
narrative_ontology:cs_axiom_status(empirical_method_subordinate_to_text, holdable).
narrative_ontology:cs_axiom_grounding('f1eea970-5c46-407b-81d2-96d34a827aa3', empirical_method_subordinate_to_text, theological).
narrative_ontology:cs_reference_frame('f1eea970-5c46-407b-81d2-96d34a827aa3', literal_historical_hermeneutic).
narrative_ontology:cs_drift_state('f1eea970-5c46-407b-81d2-96d34a827aa3', post_empirical_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('f1eea970-5c46-407b-81d2-96d34a827aa3', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, biblical_literalist_institutions).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__young_earth_literal, literalist_communities).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, science_educators).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, evolutionary_researchers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, public_education_systems).
narrative_ontology:constraint_victim(genesis_creation_cosmology__young_earth_literal, students_in_literalist_schools).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the literal hermeneutic through denominational standards, educational accreditation, and apologetics publishing; enforce conformity by framing alternative readings as compromise or apostasy; their institutional identity is fused with the fixed-text reading.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, biblical_literalist_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Receive a totalizing worldview that integrates cosmology, morality, and community identity; members are taught to distrust external epistemic authorities and to view the literal reading as foundational to group belonging.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, literalist_communities, beneficiary,
    organized, generational, identity_locked, regional).

% In affected jurisdictions, must avoid, disclaim, or provide equal time for creationist alternatives to evolution and deep time; professional autonomy is subordinated to politically negotiated curriculum standards.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, science_educators, payer,
    moderate, biographical, constrained, national).

% Their research findings are publicly contested and excluded from educational materials in literalist-controlled jurisdictions; epistemic authority is systematically subordinated to textual interpretation.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, evolutionary_researchers, payer,
    moderate, biographical, constrained, national).

% Bear administrative and legal costs of curriculum battles, textbook selection disputes, and compliance with legislation that mandates creationist-friendly framing of science standards.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, public_education_systems, payer,
    institutional, generational, constrained, national).

% Receive science education that systematically omits or misrepresents evolutionary biology, geology, and cosmology; exit requires leaving the school system or challenging community and family identity.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, students_in_literalist_schools, payer,
    powerless, biographical, trapped, local).

% Monitor and legally challenge creationist encroachment into public science education; defend empirical method and peer review as the legitimate basis for curricular content.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__young_earth_literal, science_education_alliances, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__young_earth_literal, biblical_literalist_institutions).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a unified religious community around a single, non-negotiable cosmological narrative derived from a fixed textual reading, eliminating hermeneutical pluralism and binding members to a shared identity.
% TRANSFER_FUNCTION: Moves epistemic authority from empirical scientific method and consensus to a fixed textual interpretation; moves educational and curricular control from public and secular institutions to literalist religious authorities; moves cognitive trust from scientific educators to apologetic institutions.
% ABSENT_VOICES: Mainstream Old Testament scholars who read Genesis 1â2 against its Ancient Near Eastern literary context, and evolutionary biologists practicing in restrictive jurisdictions, are structurally excluded from curriculum-setting and denominational authority; their readings are preemptively categorized as unfaithful.
% DISAPPEARANCE_RATIONALE: The dissolution of the literal 24-hour reading would remove the primary boundary marker for young-earth institutions; their educational networks, apologetics funding, and member identity cohesion would face centrifugal pressure; science classrooms would reintegrate deep time and evolutionary biology without the current institutional friction.
% FOUNDING_PROBLEM: How to preserve the authority of a fixed sacred text and the cohesion of a religious community when modern scientific cosmology appears to contradict a straightforward historical-grammatical reading of Genesis 1â2.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion and mainline theologians attest that the literal six-day reading as a mandatory boundary marker is largely a product of 20th-century fundamentalist consolidation, not an ancient consensus; biblical literalist institutions assert the problem is eternal. Corroboration from outside the beneficiary set comes from secular and religious historians who document the rise of young-earth creationism as a specific modern movement.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__young_earth_literal, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__young_earth_literal, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__young_earth_literal, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_cosmology__young_earth_literal, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__young_earth_literal, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__young_earth_literal_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_cosmology__young_earth_literal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_cosmology__young_earth_literal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.82) is high because the constraint systematically subordinates empirical scientific education to a non-empirical textual authority, extracting epistemic autonomy from educators, researchers, and students. Suppression (0.90) is near-ceiling because the reading's persistence requires active suppression of evolutionary pedagogy and deep-time cosmology in institutions the literalists influence. Theater_ratio (0.58) is elevated: as empirical evidence accumulated over the interval, an increasing share of institutional activity shifted to apologetic defense and legislative lobbying rather than direct empirical engagement. Accessibility_collapse (0.85) is high because, within literalist communities, alternative hermeneutics or scientific accounts collapse as live options once the literal frame is accepted. Resistance (0.40) is moderate: the scientific and educational communities resist, but are often politically outmatched by organized literalist institutions in curriculum-setting jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   The biblical_literalist_institutions experience the constraint as necessary identity-coordination preserving divine authority and community salvation; their seat computes toward identity-coordination with low effective extraction. The science_educators, evolutionary_researchers, and students experience it as epistemic extraction and educational capture; their seats compute as high-extraction targets. The engine derives this divergence from the same structural dataâthe beneficiary/victim asymmetry and the identity_locked exit of the coordinated community versus the trapped or constrained exit of the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (biblical_literalist_institutions, literalist_communities) are structurally near d=0.0: the constraint subsidizes their authority and identity. Victims (science_educators, evolutionary_researchers, public_education_systems, students_in_literalist_schools) are structurally near d=1.0: the constraint extracts epistemic autonomy, curricular control, and scientific literacy from them. The science_education_alliances observer seat sits at analytical exit with no directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the victim declarations and suppression metrics, this could be misread as rope (coordination of a religious community around shared belief) or falsely as mountain (claimed divine origin). The mandatrophy guard is satisfied by the explicit victim set, the active enforcement requirement, and the accumulating extractiveness over time. The founding problemâcommunity cohesion under scientific challengeâmay once have been live, but the constraint now persists as asymmetric extraction even where the coordination function could be served by less extractive readings (e.g., the literary_framework reading).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of evolutionary pedagogy accomplished through external structural barriers (school board control, state legislation) or through internalized identity-lock where community members reject empirical method as a matter of faith?',
    'Compare suppression levels in jurisdictions with explicit legal bans versus those with cultural and institutional norms only; observe whether suppression persists after structural barriers are removed.',
    'If primarily internalized, effective extraction persists even if structural barriers fall; the constraint behaves as a distributed snare rather than a top-down enforcement mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structurally enforced or identity-internalized.').

omega_variable(
    kernel_reading_separability,
    'Does the young-earth literal reading represent a genuinely distinct constraint from its literary-framework sibling, or are they different descriptions of the same institutional power structure?',
    'Historical genealogy: trace whether the same institutions enforce both readings at different times, or whether they are structurally separate communities.',
    'If the same institutions oscillate between readings depending on political convenience, the epsilon is unstable and the constraint should be decomposed into an institutional-power story separate from the hermeneutic story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_separability, conceptual, 'Whether the sibling readings are structurally separable constraints.').

omega_variable(
    reading_relation_ambiguity,
    'Does the young_earth_literal reading logically foreclose its siblings within a single framework, or do they merely compete as live positions in public discourse?',
    'Examine whether any theological institution or denomination simultaneously maintains young-earth literalism and theistic evolution as both live options; if none exist, forecloses is structurally accurate.',
    'If forecloses is correct, the constraint family is a zero-sum competition for interpretive dominance; if coexists_with is correct, the constraint is one factional position among many without logical exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_relation_ambiguity, conceptual, 'Whether the sibling reading relation is logical foreclosure or factional coexistence.').

omega_variable(
    textual_authority_vs_empirical_priority,
    'Is the subordination of empirical method to textual authority a reversible policy choice or an irreversible identity commitment for the agenda-setting institutions?',
    'Observe institutional behavior when scientific and textual claims definitively conflict: if the institution eventually accommodates, the commitment is revisable; if it doubles down, it is identity-fused.',
    'Revisability would indicate a snare vulnerable to empirical refutation; irreversibility would indicate identity-coordination extraction with near-mountain-level inertia within the community.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_authority_vs_empirical_priority, empirical, 'Whether textual authority priority is a reversible policy or fused identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__young_earth_literal, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(yec_literal_tr_t0, genesis_creation_cosmology__young_earth_literal, theater_ratio, 0, 0.3).
narrative_ontology:measurement(yec_literal_tr_t10, genesis_creation_cosmology__young_earth_literal, theater_ratio, 10, 0.35).
narrative_ontology:measurement(yec_literal_tr_t20, genesis_creation_cosmology__young_earth_literal, theater_ratio, 20, 0.42).
narrative_ontology:measurement(yec_literal_tr_t30, genesis_creation_cosmology__young_earth_literal, theater_ratio, 30, 0.48).
narrative_ontology:measurement(yec_literal_tr_t40, genesis_creation_cosmology__young_earth_literal, theater_ratio, 40, 0.52).
narrative_ontology:measurement(yec_literal_tr_t50, genesis_creation_cosmology__young_earth_literal, theater_ratio, 50, 0.55).
narrative_ontology:measurement(yec_literal_tr_t60, genesis_creation_cosmology__young_earth_literal, theater_ratio, 60, 0.58).

% Extraction over time
narrative_ontology:measurement(yec_literal_be_t0, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(yec_literal_be_t10, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(yec_literal_be_t20, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(yec_literal_be_t30, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(yec_literal_be_t40, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 40, 0.75).
narrative_ontology:measurement(yec_literal_be_t50, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 50, 0.79).
narrative_ontology:measurement(yec_literal_be_t60, genesis_creation_cosmology__young_earth_literal, base_extractiveness, 60, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(yec_literal_su_t0, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(yec_literal_su_t10, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(yec_literal_su_t20, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(yec_literal_su_t30, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(yec_literal_su_t40, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(yec_literal_su_t50, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 50, 0.88).
narrative_ontology:measurement(yec_literal_su_t60, genesis_creation_cosmology__young_earth_literal, suppression_requirement, 60, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__young_earth_literal, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__theistic_evolution).
narrative_ontology:affects_constraint(genesis_creation_cosmology__young_earth_literal, genesis_creation_cosmology__literary_framework).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the genesis_creation_cosmology kernel. It is structurally distinct from the theistic_evolution and literary_framework readings because its epsilon (high extraction, high suppression, victim set includes scientific consensus) differs radically from the negligible extraction of the literary_framework reading and the moderate coordination-extraction profile of theistic_evolution. Decomposition follows the Îµ-invariance principle: the same text supports multiple structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
