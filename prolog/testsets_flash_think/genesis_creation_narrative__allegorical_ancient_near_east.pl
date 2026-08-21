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
 *   human_readable: Genesis Creation Narrative as Ancient Near Eastern Mythopoetic Literature
 *   domain: Religious Studies/Biblical Hermeneutics/Science-Religion Interface
 *
 * SUMMARY:
 *   This constraint story models the interpretive framework that reads
 *   Genesis 1-2 as ancient Near Eastern mythopoetic literature, explicitly
 *   decoupling it from historical or scientific claims. This reading aims to
 *   resolve perceived conflicts between biblical accounts and modern
 *   scientific understanding by assigning Genesis a theological, rather than
 *   scientific, authority. It is one reading of the
 *   'genesis_creation_narrative' kernel, distinct from literal-historical or
 *   theistic-evolutionary interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.08).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.12).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.08).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis Creation Narrative as Ancient Near Eastern Mythopoetic Literature").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "Religious Studies/Biblical Hermeneutics/Science-Religion Interface").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, 'f0a96951-72d3-4883-ad9e-4d57b635de44').
narrative_ontology:cs_kernel_codification('f0a96951-72d3-4883-ad9e-4d57b635de44', fixed_text).
narrative_ontology:cs_authority_grounding('f0a96951-72d3-4883-ad9e-4d57b635de44', expertise).
narrative_ontology:cs_interpretation_layer_present('f0a96951-72d3-4883-ad9e-4d57b635de44').
narrative_ontology:cs_reading_relation('f0a96951-72d3-4883-ad9e-4d57b635de44', genesis_creation_narrative__literal_young_earth, forecloses).
narrative_ontology:cs_reading_relation('f0a96951-72d3-4883-ad9e-4d57b635de44', genesis_creation_narrative__theistic_evolutionary, influences).
narrative_ontology:cs_axiom('f0a96951-72d3-4883-ad9e-4d57b635de44', foundational, genesis_is_mythopoetic_literature).
narrative_ontology:cs_axiom_status(genesis_is_mythopoetic_literature, holdable).
narrative_ontology:cs_axiom_grounding('f0a96951-72d3-4883-ad9e-4d57b635de44', genesis_is_mythopoetic_literature, conventional).
narrative_ontology:cs_axiom('f0a96951-72d3-4883-ad9e-4d57b635de44', foundational, bible_not_science_text).
narrative_ontology:cs_axiom_status(bible_not_science_text, holdable).
narrative_ontology:cs_axiom_grounding('f0a96951-72d3-4883-ad9e-4d57b635de44', bible_not_science_text, conventional).
narrative_ontology:cs_reference_frame('f0a96951-72d3-4883-ad9e-4d57b635de44', historical_critical_interpretive_framework).
narrative_ontology:cs_drift_state('f0a96951-72d3-4883-ad9e-4d57b635de44', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f0a96951-72d3-4883-ad9e-4d57b635de44', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, religious_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, scientific_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, literal_creationists).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, historical_critical_method).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, scientific_autonomy).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, theological_interpretation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Genesis 1-2 within its historical and literary context as ancient mythopoetic literature, emphasizing its theological rather than scientific claims. They benefit from a coherent framework that reconciles faith and modern knowledge.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_scholars, agenda_setter,
    institutional, generational, analytical, global).

% Find intellectual and spiritual coherence by understanding Genesis as non-scientific, allowing them to embrace both religious faith and scientific understanding without perceived conflict. They are liberated from having to choose between the two.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, scientific_believers, beneficiary,
    moderate, biographical, mobile, global).

% Their literal-historical interpretation of Genesis is rejected and marginalized by this reading, which they perceive as undermining biblical authority. They bear the cost of intellectual displacement and potential alienation from mainstream religious scholarship.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literal_creationists, payer,
    powerless, biographical, identity_locked, local).

% Observe the theological reconciliation efforts. Their scientific work is unaffected by this interpretive framework, as it explicitly removes Genesis from the domain of scientific claims. They are neither beneficiaries nor victims, but their field's autonomy is affirmed.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, secular_scientists, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, diffuse).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding of Genesis 1-2 as non-scientific, allowing religious faith and scientific inquiry to coexist without conflict for adherents, and providing a framework for theological interpretation.
% TRANSFER_FUNCTION: Transfers interpretive authority over Genesis 1-2 from literal-historical and scientific claims to mythopoetic, theological, and moral claims, shifting the domain of its normative force.
% ABSENT_VOICES: Literal creationists are structurally excluded from the interpretive conversation within this framework, as their foundational premise of biblical inerrancy in scientific matters is rejected. They would argue for the text's historical-scientific accuracy.
% DISAPPEARANCE_RATIONALE: If this interpretive framework vanished, the perceived conflict between ancient religious texts and modern scientific findings would intensify for many believers, forcing a starker choice between faith and reason, or leading to more fundamentalist readings. The intellectual landscape of science-religion dialogue would be significantly altered.
% FOUNDING_PROBLEM: The perceived conflict between ancient religious texts (Genesis 1-2) and modern scientific understanding (cosmology, evolution), leading to intellectual dissonance for believers and skepticism from non-believers.
% FOUNDING_PROBLEM_CORROBORATION: Religious scholars, theologians, and many lay believers attest to the ongoing need for such reconciliation. Independent surveys of religious populations consistently show a desire for harmony between faith and science, corroborating the problem's persistence from outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.08, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low because this reading liberates individuals from intellectual dissonance, rather than imposing costs. Suppression is low as it does not actively suppress scientific inquiry or alternative theological views (beyond rejecting their scientific claims). Theater ratio is minimal, as the interpretation is primarily intellectual and academic. Accessibility collapse is low because it opens up, rather than closes off, avenues for understanding both science and faith. Resistance is low from within this framework, as it is itself a response to external resistance (from science to literalism). The claimed type is 'rope' because it functions as a coordination mechanism for understanding, allowing different domains of knowledge to coexist harmoniously for its adherents.
 *
 * PERSPECTIVAL GAP:
 *   For religious scholars and scientific believers, this reading is a beneficial framework that resolves conflict. For literal creationists, however, it represents a significant cost, as their foundational interpretive premise is undermined. The engine will compute this divergence based on the declared stakeholder roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious scholars and scientific believers are beneficiaries, as this reading provides a coherent framework for reconciling faith and science. Literal creationists are payers, as their interpretive framework is challenged and marginalized by this reading. Secular scientists are observers, as their domain is explicitly separated from the biblical text by this interpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by clearly defining the boundaries of Genesis's authority. It avoids the 'snare' of intellectual coercion by not forcing scientific claims onto the text, and avoids the 'tangled rope' of trying to extract scientific data from a theological narrative. Its persistence is tied to the ongoing need for science-faith reconciliation, which remains a live problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_boundary,
    'Does this reading fully resolve the tension between religious texts and scientific findings, or merely reframe it by shifting the domain of authority?',
    'Longitudinal studies of adherents'' intellectual satisfaction and engagement with both science and faith; analysis of how this reading influences broader cultural debates.',
    'If it merely reframes, the underlying tension might persist in other forms, potentially leading to new interpretive constraints. If it fully resolves, its ''rope'' classification is strongly affirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_boundary, conceptual, 'Whether the interpretive boundary truly resolves conflict or merely displaces it.').

omega_variable(
    impact_on_literalists_extraction,
    'Is the marginalization of literal-historical interpretations by this reading a form of extraction from those who hold such views, or a necessary consequence of intellectual progress?',
    'Sociological studies on the psychological and social costs borne by individuals who shift from literal to allegorical interpretations, or who maintain literal views in contexts where they are marginalized.',
    'If the costs are significant and coercive, the extractiveness of this reading (from the literalist seat) might be higher than currently assessed, potentially pushing it towards a ''tangled_rope'' for that specific seat. If the costs are primarily intellectual and voluntary, the current assessment holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_literalists_extraction, empirical, 'Assessing the ''extraction'' experienced by those whose interpretations are marginalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1800, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1800, 0.08).
narrative_ontology:measurement(gene_tr_t1850, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(gene_tr_t1900, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1900, 0.06).
narrative_ontology:measurement(gene_tr_t1950, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(gene_tr_t2000, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(gene_tr_t2024, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(gene_be_t1800, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement(gene_be_t1850, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1850, 0.12).
narrative_ontology:measurement(gene_be_t1900, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1900, 0.1).
narrative_ontology:measurement(gene_be_t1950, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 1950, 0.09).
narrative_ontology:measurement(gene_be_t2000, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(gene_be_t2024, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 2024, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1800, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(gene_su_t1850, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1850, 0.18).
narrative_ontology:measurement(gene_su_t1900, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1900, 0.15).
narrative_ontology:measurement(gene_su_t1950, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 1950, 0.13).
narrative_ontology:measurement(gene_su_t2000, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2000, 0.12).
narrative_ontology:measurement(gene_su_t2024, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 2024, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
