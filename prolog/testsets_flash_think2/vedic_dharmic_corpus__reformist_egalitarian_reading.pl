% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__reformist_egalitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__reformist_egalitarian_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Interpretation of Vedic-Dharmic Texts
 *   domain: religious/social/political
 *
 * SUMMARY:
 *   This constraint story instantiates the 'reformist_egalitarian_reading' of
 *   the 'vedic_dharmic_corpus' kernel. This reading asserts that the meaning
 *   of sacred texts must conform to constitutional equality principles, that
 *   caste hierarchy is a historical accretion rather than scriptural essence,
 *   and that rational critique supersedes traditional authority. It is a
 *   contested interpretation that seeks to transform social and religious
 *   norms in India. The claimed type is 'tangled_rope' because it aims to
 *   coordinate a new, inclusive social order while actively extracting power
 *   and privilege from traditional, orthodox institutions, requiring ongoing
 *   enforcement and struggle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.6).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Interpretation of Vedic-Dharmic Texts").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social/political").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'afadf506-8cd3-46f5-8878-118bf8df2acf').
narrative_ontology:cs_kernel_codification('afadf506-8cd3-46f5-8878-118bf8df2acf', fixed_text).
narrative_ontology:cs_authority_grounding('afadf506-8cd3-46f5-8878-118bf8df2acf', expertise).
narrative_ontology:cs_interpretation_layer_present('afadf506-8cd3-46f5-8878-118bf8df2acf').
narrative_ontology:cs_reading_relation('afadf506-8cd3-46f5-8878-118bf8df2acf', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('afadf506-8cd3-46f5-8878-118bf8df2acf', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('afadf506-8cd3-46f5-8878-118bf8df2acf', foundational, textual_meaning_conforms_to_constitutional_equality).
narrative_ontology:cs_axiom_status(textual_meaning_conforms_to_constitutional_equality, holdable).
narrative_ontology:cs_axiom_grounding('afadf506-8cd3-46f5-8878-118bf8df2acf', textual_meaning_conforms_to_constitutional_equality, deontological).
narrative_ontology:cs_axiom('afadf506-8cd3-46f5-8878-118bf8df2acf', foundational, caste_hierarchy_is_historical_accretion_not_scriptural_essence).
narrative_ontology:cs_axiom_status(caste_hierarchy_is_historical_accretion_not_scriptural_essence, holdable).
narrative_ontology:cs_axiom_grounding('afadf506-8cd3-46f5-8878-118bf8df2acf', caste_hierarchy_is_historical_accretion_not_scriptural_essence, empirically_contingent).
narrative_ontology:cs_reference_frame('afadf506-8cd3-46f5-8878-118bf8df2acf', constitutional_egalitarian_framework).
narrative_ontology:cs_drift_state('afadf506-8cd3-46f5-8878-118bf8df2acf', contemporary_india, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('afadf506-8cd3-46f5-8878-118bf8df2acf', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, social_reformers).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_state_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_caste_elites).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_principles).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, rational_inquiry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively advocate for this reading, as it directly addresses historical discrimination and provides a framework for dignity and equality. Their identity is deeply intertwined with the struggle for social justice within the religious and constitutional framework.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, identity_locked, national).

% Intellectuals and activists who champion the reinterpretation of religious texts through a lens of equality and rationality, often facing social backlash but driven by ethical commitments.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, social_reformers, beneficiary,
    moderate, biographical, constrained, national).

% The Indian state, through its constitution and legal apparatus, provides a framework for equality that this reading seeks to align religious practice with. It acts as an enforcer of secular principles, sometimes in tension with traditional religious authority.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_state_institutions, agenda_setter,
    institutional, generational, mobile, national).

% Traditional religious bodies that resist this reading, as it challenges their hereditary authority, ritual monopolies, and established social order. They bear the cost of losing interpretive control and social privilege.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions, payer,
    institutional, generational, constrained, national).

% Individuals and families who benefit from the traditional caste hierarchy and resist any reinterpretation that undermines their social status, economic advantages, or ritual prerogatives.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditional_caste_elites, payer,
    powerful, biographical, constrained, regional).

% While often promoting spiritual equality through devotion, these traditions may not directly engage with the rational-critical or constitutional-alignment aspects of this reading, operating on a parallel but distinct path to reform.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, bhakti_devotional_traditions, excluded,
    organized, generational, mobile, national).

% Academics and researchers who study the historical development of religious texts, social structures, and reform movements, providing critical analysis of the claims made by all parties.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, analytical_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__reformist_egalitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate a shared understanding of the Vedic-Dharmic corpus that aligns religious meaning with constitutional equality principles and rational critique, fostering a more inclusive and just social order.
% TRANSFER_FUNCTION: Transfers interpretive authority and social privilege away from hereditary Brahminical institutions and traditional caste elites towards principles of equality, rational inquiry, and constitutional values. It transfers social dignity, access to religious life, and political agency to historically marginalized groups.
% ABSENT_VOICES: Hardline traditionalists and fundamentalist groups who reject any reinterpretation of scripture based on external principles (constitutional law, rationality) and maintain the divine ordination and immutability of caste hierarchy. They are excluded from the discourse of reform by their own refusal to engage with its premises.
% DISAPPEARANCE_RATIONALE: If this reformist reading vanished, the legal and social struggle for equality within the religious framework would lose a crucial interpretive and legitimizing tool. This would likely lead to a resurgence of traditional hierarchies, undermine secular constitutional principles in India, and deepen social divisions, forcing a reorganization of legal and social advocacy.
% FOUNDING_PROBLEM: The historical problem of pervasive social inequality, discrimination, and exclusion (particularly caste-based) justified by traditional interpretations of religious texts, leading to widespread human suffering, denial of dignity, and societal fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Dalit rights organizations, human rights commissions, and independent sociological studies consistently corroborate the ongoing problem of caste discrimination and the necessity of interpretive reform. Legal challenges and legislative debates further attest to the live status of this founding problem.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).
:- end_tests(vedic_dharmic_corpus__reformist_egalitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The 'extractiveness' (0.45) reflects the ongoing cost and effort required to establish and maintain this reading against entrenched traditional power, which resists the extraction of its authority. 'Suppression' (0.60) indicates the active enforcement, often through legal and social movements, needed to counter the suppressive force of traditional authority and to implement egalitarian principles. The 'theater_ratio' is low (0.10) because this reading is fundamentally about genuine social and interpretive change, not performative maintenance of an atrophied function. 'Resistance' is high (0.75) as this reading directly challenges centuries of tradition and faces significant opposition from orthodox groups. 'Accessibility collapse' is low (0.20) because this reading aims to open up religious and social participation, rather than restrict it.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Dalit movements and social reformers, this reading is a 'rope' or 'scaffold' that coordinates liberation and builds a more just society. From the perspective of orthodox institutions and caste elites, it is a 'snare' that unjustly extracts their divinely ordained authority and tradition. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements and social reformers are clear beneficiaries, as this reading directly empowers them and validates their struggle for equality. Secular state institutions act as agenda-setters and beneficiaries by aligning religious interpretation with constitutional values. Orthodox Brahminical institutions and traditional caste elites are victims, as this reading directly challenges and seeks to dismantle their inherited authority and privilege. Bhakti devotional traditions are 'excluded' in the sense that their approach to equality is distinct and often less focused on rational-critical textual engagement or constitutional alignment, though they may share some goals.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling by recognizing the dual nature of the reformist reading: it coordinates a new social order (rope-like function) but does so by actively extracting power from an existing, entrenched system (snare-like function for the traditionalists). It is not a 'piton' because it is actively contested and enforced, not merely maintained by inertia. It is not a 'scaffold' because its goal is a new, stable interpretive framework, not a temporary transition to an external state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_source,
    'To what extent does the persistence and adoption of this reformist reading depend on state legal enforcement versus internal acceptance and reinterpretation within religious communities?',
    'Comparative analysis of jurisdictions with varying degrees of state intervention in religious affairs, or longitudinal studies tracking the adoption of this reading in the absence of direct legal mandates.',
    'If primarily dependent on state enforcement, the ''suppression'' metric might be higher, reflecting external coercion. If internal acceptance is key, the ''resistance'' metric might decrease over time as the reading gains organic traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_source, empirical, 'The relative influence of state power vs. internal religious reform on the reading''s adoption.').

omega_variable(
    caste_as_accretion_consensus,
    'Is the claim that ''caste hierarchy is historical accretion rather than scriptural essence'' a widely accepted scholarly consensus, or is it still a contested academic and theological position?',
    'Survey of leading Indologists, religious scholars, and theologians across diverse institutions; analysis of peer-reviewed publications and major theological debates.',
    'If widely accepted, it strengthens the ''rational critique'' aspect of this reading and weakens the legitimacy claims of opposing readings. If highly contested, it highlights the ''conceptual'' nature of the struggle and the difficulty of achieving consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(caste_as_accretion_consensus, empirical, 'Scholarly consensus on the origins of caste relative to scriptural texts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(vedi_tr_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1965, 0.1).
narrative_ontology:measurement(vedi_tr_t1983, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1983, 0.1).
narrative_ontology:measurement(vedi_tr_t2001, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(vedi_tr_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(vedi_tr_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1947, 0.35).
narrative_ontology:measurement(vedi_be_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1965, 0.38).
narrative_ontology:measurement(vedi_be_t1983, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1983, 0.41).
narrative_ontology:measurement(vedi_be_t2001, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2001, 0.43).
narrative_ontology:measurement(vedi_be_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2015, 0.44).
narrative_ontology:measurement(vedi_be_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1947, 0.5).
narrative_ontology:measurement(vedi_su_t1965, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1965, 0.53).
narrative_ontology:measurement(vedi_su_t1983, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1983, 0.56).
narrative_ontology:measurement(vedi_su_t2001, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2001, 0.58).
narrative_ontology:measurement(vedi_su_t2015, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2015, 0.59).
narrative_ontology:measurement(vedi_su_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__reformist_egalitarian_reading, identity_coordination).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_constitutional_secularism).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, caste_based_affirmative_action).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__hereditary_monopoly_reading).
narrative_ontology:affects_constraint(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus__bhakti_devotional_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
