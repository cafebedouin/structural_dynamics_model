% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   This constraint models the interpretation of Genesis 1-2 as ancient Near
 *   Eastern mythopoetic literature, focusing on its theological rather than
 *   historical or scientific claims. This reading decouples the text from
 *   modern scientific inquiry, asserting that its purpose is to convey truths
 *   about God, humanity, and creation's order, not to provide a literal
 *   account of origins. It is presented as a 'mountain' because, within this
 *   interpretive framework, the text's literary genre and theological intent
 *   are treated as fixed, unchangeable features, making any scientific or
 *   historical claims irrelevant to its primary meaning. The low
 *   extractiveness and suppression reflect that this reading is largely
 *   self-enforcing among its adherents, offering intellectual coherence
 *   rather than imposing costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.1).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.05).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, mountain).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:emerges_naturally(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '6d435f7f-f68f-4245-9e95-814fa119e851').
narrative_ontology:cs_kernel_codification('6d435f7f-f68f-4245-9e95-814fa119e851', fixed_text).
narrative_ontology:cs_authority_grounding('6d435f7f-f68f-4245-9e95-814fa119e851', expertise).
narrative_ontology:cs_interpretation_layer_present('6d435f7f-f68f-4245-9e95-814fa119e851').
narrative_ontology:cs_reading_relation('6d435f7f-f68f-4245-9e95-814fa119e851', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('6d435f7f-f68f-4245-9e95-814fa119e851', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('6d435f7f-f68f-4245-9e95-814fa119e851', foundational, genesis_is_ancient_near_eastern_mythopoetic_literature).
narrative_ontology:cs_axiom_status(genesis_is_ancient_near_eastern_mythopoetic_literature, holdable).
narrative_ontology:cs_axiom_grounding('6d435f7f-f68f-4245-9e95-814fa119e851', genesis_is_ancient_near_eastern_mythopoetic_literature, conventional).
narrative_ontology:cs_axiom('6d435f7f-f68f-4245-9e95-814fa119e851', foundational, genesis_has_no_adjudicative_authority_over_cosmology_biology).
narrative_ontology:cs_axiom_status(genesis_has_no_adjudicative_authority_over_cosmology_biology, holdable).
narrative_ontology:cs_axiom_grounding('6d435f7f-f68f-4245-9e95-814fa119e851', genesis_has_no_adjudicative_authority_over_cosmology_biology, deontological).
narrative_ontology:cs_reference_frame('6d435f7f-f68f-4245-9e95-814fa119e851', ancient_literary_context_theological_purpose).
narrative_ontology:cs_drift_state('6d435f7f-f68f-4245-9e95-814fa119e851', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6d435f7f-f68f-4245-9e95-814fa119e851', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, theologians).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, religious_educators).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, theological_truth_not_scientific_truth).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ancient_near_eastern_context_is_primary).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a hermeneutic that allows Genesis to be read for theological truth without conflict with modern science, preserving the text's relevance and authority in a secular age. This reading frees them from defending literalist interpretations.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theologians, beneficiary,
    institutional, generational, mobile, global).

% Find it easier to teach Genesis in a way that resonates with students familiar with scientific consensus, avoiding perceived contradictions and making religious instruction more intellectually coherent. They gain pedagogical flexibility.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_educators, beneficiary,
    organized, biographical, mobile, national).

% Observe this reading as a resolution to potential conflicts between religious texts and scientific findings. They are not directly governed by the constraint but benefit from the intellectual peace it offers in public discourse.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientists, observer,
    institutional, generational, analytical, universal).

% Are excluded from the interpretive framework of this reading, as their core premise of Genesis as literal history is directly contradicted. They would object to the demotion of the text's historical claims.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literal_young_earth_creationists, excluded,
    organized, generational, identity_locked, global).

% While sharing the goal of reconciling faith and science, they might find this reading's complete decoupling of Genesis from scientific claims to be too strong, preferring an interpretation where Genesis provides a theological framework for evolutionary processes.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionists, observer,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation of Genesis 1-2 within a framework that respects both ancient literary context and modern scientific understanding, allowing religious adherents to hold both without cognitive dissonance.
% TRANSFER_FUNCTION: Transfers interpretive authority over scientific and historical matters away from the Genesis text to scientific inquiry, while retaining theological authority for the text.
% ABSENT_VOICES: Literal young-earth creationists are absent from the interpretive conversation that defines this reading; they would argue that this reading undermines the authority and inerrancy of scripture by denying its historical claims.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the intellectual peace between science and religion would be severely disrupted for many adherents. Theologians and educators would struggle to reconcile Genesis with scientific findings, leading to increased cognitive dissonance and potential loss of faith for some, or a resurgence of literalist interpretations.
% FOUNDING_PROBLEM: The perceived conflict between the Genesis creation accounts and modern scientific discoveries (cosmology, geology, biology) created intellectual and theological crises for believers.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science and religion, philosophers of religion, and surveys of religious adherents consistently corroborate the ongoing tension between literalist biblical interpretations and scientific consensus. This problem is widely acknowledged outside of the specific theological communities benefiting from this reading.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, ExtMetricName, E),
    domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(genesis_creation_narrative__allegorical_ancient_near_east),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.05) and suppression (0.1) reflect that this reading primarily offers a framework for intellectual reconciliation rather than imposing coercive costs. Its 'mountain' classification stems from the assertion that the literary genre and theological purpose of Genesis are inherent to the text and its ancient context, thus unchangeable. The high accessibility collapse (0.9) means that once this interpretive lens is adopted, alternative literalist readings become intellectually untenable within the framework. Resistance is low (0.05) from within this interpretive community, though significant from outside (e.g., literalists).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of theologians and educators, this reading is a liberating framework that allows for intellectual integrity. From the perspective of literal young-earth creationists, it is a betrayal of biblical authority. The engine's classification will reflect the low extraction and suppression inherent to the interpretive framework itself, while acknowledging the 'excluded' status of those who reject it.
 *
 * DIRECTIONALITY LOGIC:
 *   Theologians and religious educators are beneficiaries (d near 0.0) as this reading resolves conflicts and enhances the intellectual credibility of their work. Scientists and theistic evolutionists are observers (d near 0.5), benefiting from the reduced conflict but not directly governed. Literal young-earth creationists are excluded (d near 1.0) as their core interpretive method is rejected by this framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genre_determination_objectivity,
    'Is the determination of Genesis 1-2 as ''mythopoetic literature'' an objective literary analysis, or is it influenced by the desire to reconcile with modern science?',
    'Comparative literary analysis of ancient Near Eastern texts by scholars with no stake in the science-religion debate; historical-critical method applied without theological presuppositions.',
    'If primarily driven by reconciliation, the ''emerges_naturally'' claim for this reading''s genre interpretation is weakened, potentially shifting its classification from Mountain to a more constructed type (e.g., Rope or Tangled Rope) for those who adopt it for pragmatic reasons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genre_determination_objectivity, conceptual, 'Objectivity of genre classification for Genesis 1-2.').

omega_variable(
    dominion_metaphor_normative_force,
    'Does the ''dominion'' metaphor in Genesis 1-2, when read mythopoetically, retain any normative force for environmental ethics, or is its ethical implication entirely lost?',
    'Analysis of contemporary theological and ethical discourse that adopts this reading: do they derive specific ethical mandates (e.g., stewardship) from it, or is it purely descriptive of humanity''s place?',
    'If the dominion metaphor loses all normative force, this reading might be seen as extracting ethical guidance from the text, potentially increasing its effective extraction for those seeking such guidance. If it retains a transformed normative force, the reading''s coordination function for ethics is preserved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_metaphor_normative_force, preference, 'Ethical implications of the dominion metaphor in a mythopoetic reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1950, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1950, 0.0).
narrative_ontology:measurement(gene_tr_t1970, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1970, 0.0).
narrative_ontology:measurement(gene_tr_t1990, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1990, 0.0).
narrative_ontology:measurement(gene_tr_t2010, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2010, 0.0).
narrative_ontology:measurement(gene_tr_t2024, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(gene_be_t1950, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1950, 0.05).
narrative_ontology:measurement(gene_be_t1970, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1970, 0.05).
narrative_ontology:measurement(gene_be_t1990, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(gene_be_t2010, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2010, 0.05).
narrative_ontology:measurement(gene_be_t2024, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1950, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1950, 0.1).
narrative_ontology:measurement(gene_su_t1970, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(gene_su_t1990, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(gene_su_t2010, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2010, 0.1).
narrative_ontology:measurement(gene_su_t2024, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'Genesis Creation Narrative' kernel. It focuses on the mythopoetic interpretation, decoupling the text from scientific claims. It contrasts with literalist and theistic evolutionary readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
