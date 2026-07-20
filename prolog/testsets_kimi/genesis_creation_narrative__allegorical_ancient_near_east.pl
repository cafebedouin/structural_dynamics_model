% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as ANE Mythopoetic Literature (Allegorical Reading)
 *   domain: religious_studies/biblical_hermeneutics
 *
 * SUMMARY:
 *   The constraint is the dominant allegorical Ancient Near Eastern reading
 *   of Genesis 1-2 within academic biblical studies. It binds scholars,
 *   seminaries, and secular universities to a hermeneutical regime that
 *   classifies the text as mythopoetic literature rather than history or
 *   science. While this coordinates the guild and defuses science-religion
 *   conflict, it simultaneously extracts interpretive authority from
 *   confessional communities and strips the text of normative force. Key
 *   agents by structural relationship: historical-critical scholars set the
 *   agenda; mainline seminaries and secular universities benefit from
 *   academic peace; confessional theologians and evangelical students bear
 *   the costs of delegitimization and identity strain; literalist communities
 *   are excluded from the conversation.
 *
 * KEY AGENTS:
 *   - historical_critical_scholars: agenda_setter (institutional/mobile) â defines and enforces the ANE hermeneutical frame
 *   - mainline_seminaries: beneficiary (institutional/constrained) â gains respectability, pays in confessional dilution
 *   - secular_universities: beneficiary (institutional/mobile) â keeps biblical studies inside secular academic norms
 *   - confessional_theologians: payer (organized/identity_locked) â loses textual authority, must adopt foreign frame to speak
 *   - evangelical_students: payer (powerless/trapped) â must reproduce ANE reading to obtain credentials
 *   - literalist_communities: excluded (organized/constrained) â absent from scholarly discourse except as object
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.58).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.62).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.58).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as ANE Mythopoetic Literature (Allegorical Reading)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '1b532c1a-0a0a-4aff-9e39-0d9fc393cf55').
narrative_ontology:cs_kernel_codification('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', fixed_text).
narrative_ontology:cs_authority_grounding('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', expertise).
narrative_ontology:cs_interpretation_layer_present('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55').
narrative_ontology:cs_reading_relation('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', foundational, text_as_ane_mythopoetic_literature).
narrative_ontology:cs_axiom_status(text_as_ane_mythopoetic_literature, holdable).
narrative_ontology:cs_axiom_grounding('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', text_as_ane_mythopoetic_literature, empirically_contingent).
narrative_ontology:cs_axiom('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', foundational, no_adjudicative_authority_over_modern_science).
narrative_ontology:cs_axiom_status(no_adjudicative_authority_over_modern_science, holdable).
narrative_ontology:cs_axiom_grounding('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', no_adjudicative_authority_over_modern_science, conventional).
narrative_ontology:cs_reference_frame('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', ane_temple_inauguration_myth).
narrative_ontology:cs_drift_state('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', modern_scientific_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1b532c1a-0a0a-4aff-9e39-0d9fc393cf55', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, mainline_seminaries).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, secular_universities).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, confessional_theologians).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, evangelical_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the hermeneutical standards that classify Genesis 1-2 as Ancient Near Eastern mythopoetic literature. Control hiring, peer review, and curriculum in biblical studies. Benefit from shared methodological coherence, guild solidarity, and the cultural capital of interpreting a contested text without fighting science-religion battles.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_scholars, agenda_setter,
    institutional, generational, mobile, global).

% Adopt the ANE reading to maintain academic accreditation and respectability, avoiding direct conflict between science faculty and biblical studies faculty. Their graduates receive credentials recognized by the broader academy, but shifting to a confessional model would risk institutional standing and enrollment.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, mainline_seminaries, beneficiary,
    institutional, generational, constrained, national).

% House religion departments that operate under the same scholarly norms as other humanities. The ANE reading keeps biblical studies inside the tent of secular academic respectability by removing claims that would trigger conflict with natural-science colleagues or First Amendment scrutiny.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_universities, beneficiary,
    institutional, generational, mobile, global).

% Hold that Genesis retains normative theological and ethical authority, including dominion-stewardship frameworks. Within the academy they must adopt the ANE frame to publish and teach as scholars; outside the academy their readings are labeled pre-critical. Their theological identity is fused with the text's authority, making exit costly.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, confessional_theologians, payer,
    organized, generational, identity_locked, national).

% Must learn and reproduce the ANE mythopoetic reading to pass examinations, write acceptable theses, and obtain credentials. Their home communities often read Genesis literally or theistically; adopting the scholarly frame creates social and cognitive dissonance, but leaving the program aborts their vocational path.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, evangelical_students, payer,
    powerless, biographical, trapped, regional).

% Are not present in the scholarly conversation except as objects of analysis. Their readings are cited in footnotes as specimens of fundamentalism rather than as live interpretive options. They would object that the text is being stripped of authority, but they are outside the guild's discourse.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_communities, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, diffuse).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared hermeneutical framework that allows biblical scholars, seminaries, and universities to read Genesis 1-2 consistently without entering into irresolvable conflict with modern cosmology and biology. Coordinates the guild around comparative Ancient Near Eastern literary methods and a common disciplinary language.
% TRANSFER_FUNCTION: Moves interpretive authority and academic legitimacy from confessional and literalist readings to the historical-critical scholarly guild. Transfers the text's normative functionsâsuch as dominion ethicsâinto the category of ancient cultic convention, thereby stripping them of direct modern action-guiding force.
% ABSENT_VOICES: Literalist and young-earth communities are structurally excluded from the academic conversation. Confessional theologians are sometimes present in the room but must speak through the ANE frame to be recognized as scholars rather than as believers.
% DISAPPEARANCE_RATIONALE: If the allegorical ANE reading vanished as the dominant scholarly paradigm, biblical studies departments would lose their common methodological ground and fragment into competing literalist, theistic-evolutionary, and literary-critical camps. Seminary curricula would reorganize around catechism or apologetics; the science-religion conflict would re-enter institutions that currently avoid it through this interpretive settlement.
% FOUNDING_PROBLEM: The collision between traditional biblical authority and the rise of modern historical and natural-scientific criticism in the nineteenth and twentieth centuries, which threatened to expel biblical studies from the university or reduce Scripture to an object of apologetics.
% FOUNDING_PROBLEM_CORROBORATION: Mainline seminaries and secular universities attest the problem is resolved by the ANE reading's decoupling. Confessional theologians and evangelical institutions attest the 'solution' was a disciplinary capitulation that created a new problem of textual devaluation. Independent historians of the science-religion interface corroborate the historical conflict but remain methodologically neutral on whether the ANE reading is the correct resolution.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the reading systematically transfers the text's adjudicative and normative authority to the scholarly guild. Suppression (0.62) reflects active boundary maintenance: literalist readings are barred from peer-reviewed venues and tenure-track appointments. Theater ratio (0.45) captures the increasing performance of 'neutral historical criticism' that masks the normative commitment to methodological naturalism. Accessibility collapse (0.70) indicates that once the ANE frame is accepted, literalist readings become intellectually invisible. Resistance (0.55) registers ongoing pushback from evangelical institutions and confessional networks.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (historical-critical scholars) experiences the constraint as rope: it solved a genuine disciplinary coordination problem and enables productive ANE research. The confessional and evangelical payer seats experience the same structure as extraction: their textual authority has been confiscated and their exit options are locked by identity or degree requirements. The engine computes this divergence from the structural data rather than the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (scholars, mainline seminaries, secular universities) receive low directionality because the constraint subsidizes their institutional stability and scholarly freedom. Victims (confessional theologians, evangelical students) receive high directionality because the constraint extracts textual authority from them and demands costly frame-switching. The identity-locked exit of confessional theologians amplifies their effective extraction; the trapped exit of students locks them into paying the constraint's costs biographically.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled rope prevents two errors: (1) treating the constraint as pure rope would ignore the asymmetric extraction from confessional communities and the active suppression of literalist readings; (2) treating it as pure snare would ignore the genuine coordination function it serves for the academy by providing a common hermeneutical language and avoiding science-religion warfare. The mandate has not fully atrophiedâthe coordination problem is still live for the guildâso piton is inappropriate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the allegorical ANE reading foreclose all theological normativity from Genesis 1-2, or only adjudicative authority over natural science?',
    'Comparative analysis of how the same scholarly community treats other Torah texts (e.g., Exodus law, prophetic ethics) that they continue to treat as normative or theologically generative.',
    'If the reading forecloses all normativity, its extraction from religious communities is deeper than the metrics suggest; if only scientific adjudication, it may coexist with theological ethics including non-literal dominion frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Ambiguity about the scope of the reading''s decoupling from normativity').

omega_variable(
    suppression_of_alternatives,
    'Is the marginalization of literalist readings in the academy a necessary consequence of scholarly rigor, or an active suppression that maintains guild boundaries?',
    'Bibliometric and curriculum analysis comparing citation rates, tenure decisions, and hiring patterns across confessional and non-confessional institutions.',
    'If active suppression, the constraint''s suppression and effective extraction are higher than methodological consensus alone would justify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternatives, empirical, 'Whether guild boundaries are maintained by rigor or coercion').

omega_variable(
    ane_empirical_vulnerability,
    'To what extent does the allegorical reading''s empirical contingencyâits dependence on comparative ANE evidenceâmake it vulnerable to axiom-overriding if new archaeological or textual evidence challenges the temple-inauguration hypothesis?',
    'Monitoring of ANE archaeological and literary discoveries for evidence that would reclassify the genre or Sitz im Leben of Genesis 1-2.',
    'If comparative ANE evidence shifts substantially, the reading could drift toward conventional grounding or collapse, altering its structural relationship to literalist siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ane_empirical_vulnerability, empirical, 'Empirical vulnerability of the ANE myth classification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(genesis_ane_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.2).
narrative_ontology:measurement(genesis_ane_tr_t10, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 10, 0.28).
narrative_ontology:measurement(genesis_ane_tr_t20, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 20, 0.35).
narrative_ontology:measurement(genesis_ane_tr_t30, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 30, 0.4).
narrative_ontology:measurement(genesis_ane_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.43).
narrative_ontology:measurement(genesis_ane_tr_t50, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(genesis_ane_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(genesis_ane_be_t10, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(genesis_ane_be_t20, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(genesis_ane_be_t30, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(genesis_ane_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(genesis_ane_be_t50, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(genesis_ane_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(genesis_ane_su_t10, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(genesis_ane_su_t20, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(genesis_ane_su_t30, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(genesis_ane_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(genesis_ane_su_t50, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 50, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary).

% DUAL FORMULATION NOTE:
% The genesis_creation_narrative kernel decomposes into three structurally distinct constraints: the allegorical ANE reading (this file), the literal young-earth reading, and the theistic evolutionary reading. Each has a different epsilon, beneficiary/victim structure, and classification. They compete for interpretive authority over the same fixed text, and the allegorical reading's dominance creates structural pressure on both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
