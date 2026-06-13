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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vedic_dharmic_corpus__reformist_egalitarian_reading
 *   human_readable: Reformist Egalitarian Reading of Vedic Dharmic Corpus
 *   domain: religious/social_stratification/interpretive_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the reformist-egalitarian interpretation of
 *   the Vedic Dharmic corpus, which asserts that textual meaning must align
 *   with constitutional equality principles, caste hierarchy is a historical
 *   accretion rather than scriptural essence, and rational critique
 *   supersedes traditional authority. It is a reading that actively
 *   challenges established power structures and seeks to reframe religious
 *   legitimacy around modern egalitarian values. This reading is entangled
 *   with the Indian state's legal apparatus, which provides enforcement for
 *   its principles.
 *
 * KEY AGENTS:
 *   - dalit_movements: Primary beneficiary (organized/constrained) — actively advocates for this reading.
 *   - secular_reformers: Secondary beneficiary (moderate/mobile) — provides intellectual and political backing.
 *   - orthodox_brahminical_institutions: Primary payer (institutional/identity_locked) — resists loss of traditional authority.
 *   - traditionalist_communities: Secondary payer (moderate/identity_locked) — experiences loss of social order.
 *   - indian_state_judiciary: Agenda setter (institutional/constrained) — enforces constitutional equality, supporting this reading.
 *   - academic_scholars_of_religion: Analytical observer (analytical/analytical) — provides critical analysis.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.45).
domain_priors:suppression_score(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.3).
domain_priors:theater_ratio(vedic_dharmic_corpus__reformist_egalitarian_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__reformist_egalitarian_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__reformist_egalitarian_reading, tangled_rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__reformist_egalitarian_reading, "Reformist Egalitarian Reading of Vedic Dharmic Corpus").
narrative_ontology:topic_domain(vedic_dharmic_corpus__reformist_egalitarian_reading, "religious/social_stratification/interpretive_legitimacy").

domain_priors:requires_active_enforcement(vedic_dharmic_corpus__reformist_egalitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__reformist_egalitarian_reading, 'ead042c5-d2fe-4887-bcd0-4ad14cad8571').
narrative_ontology:cs_kernel_codification('ead042c5-d2fe-4887-bcd0-4ad14cad8571', fixed_text).
narrative_ontology:cs_authority_grounding('ead042c5-d2fe-4887-bcd0-4ad14cad8571', lineage).
narrative_ontology:cs_interpretation_layer_present('ead042c5-d2fe-4887-bcd0-4ad14cad8571').
narrative_ontology:cs_reading_relation('ead042c5-d2fe-4887-bcd0-4ad14cad8571', vedic_dharmic_corpus__hereditary_monopoly_reading, forecloses).
narrative_ontology:cs_reading_relation('ead042c5-d2fe-4887-bcd0-4ad14cad8571', vedic_dharmic_corpus__bhakti_devotional_reading, coexists_with).
narrative_ontology:cs_axiom('ead042c5-d2fe-4887-bcd0-4ad14cad8571', foundational, constitutional_equality_supremacy).
narrative_ontology:cs_axiom_status(constitutional_equality_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('ead042c5-d2fe-4887-bcd0-4ad14cad8571', constitutional_equality_supremacy, conventional).
narrative_ontology:cs_axiom('ead042c5-d2fe-4887-bcd0-4ad14cad8571', foundational, caste_as_historical_accretion).
narrative_ontology:cs_axiom_status(caste_as_historical_accretion, holdable).
narrative_ontology:cs_axiom_grounding('ead042c5-d2fe-4887-bcd0-4ad14cad8571', caste_as_historical_accretion, empirically_contingent).
narrative_ontology:cs_reference_frame('ead042c5-d2fe-4887-bcd0-4ad14cad8571', post_independence_secular_state).
narrative_ontology:cs_drift_state('ead042c5-d2fe-4887-bcd0-4ad14cad8571', contemporary_hindutva_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('ead042c5-d2fe-4887-bcd0-4ad14cad8571', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__reformist_egalitarian_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_reformers).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__reformist_egalitarian_reading, marginalized_communities).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_communities).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, constitutional_equality_doctrine).
narrative_ontology:constraint_vindicates(vedic_dharmic_corpus__reformist_egalitarian_reading, rational_critique_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively advocate for this reading, which grants them spiritual and social legitimacy, challenging centuries of exclusion. They benefit from the reinterpretation of texts and the legal backing of equality principles, but face strong social resistance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, dalit_movements, beneficiary,
    organized, generational, constrained, national).

% Support this reading as it aligns with modern democratic and egalitarian values. They provide intellectual and political backing, seeking to modernize religious discourse and reduce social inequalities. Their influence is primarily through public discourse and legal advocacy.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, secular_reformers, beneficiary,
    moderate, biographical, mobile, national).

% Bear the cost of losing their traditional ritual and interpretive monopoly. This reading directly challenges their authority and the social structures from which they derive power and status. They resist through theological arguments, social pressure, and political lobbying.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, orthodox_brahminical_institutions, payer,
    institutional, generational, identity_locked, regional).

% Experience a loss of social order and perceived spiritual purity as their inherited caste-based practices are delegitimized. They are often identity-locked, finding their self-concept and community structure deeply intertwined with traditional interpretations.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, traditionalist_communities, payer,
    moderate, generational, identity_locked, local).

% Enforces constitutional equality, which implicitly and explicitly supports this reading over traditionalist ones. Its rulings can mandate temple entry, prohibit discrimination, and influence educational curricula, thereby actively shaping the interpretive landscape. It acts as a coercive force against traditionalist resistance.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, indian_state_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Analyze the historical development of caste, scriptural interpretations, and the impact of reform movements. They provide critical analysis of the textual basis for various readings and the social dynamics of their contestation, without directly participating in the enforcement or extraction.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__reformist_egalitarian_reading, academic_scholars_of_religion, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Seeks to coordinate a diverse society around principles of equality and justice by reinterpreting religious texts to align with modern human rights and constitutional values, fostering social cohesion across traditional divides.
% TRANSFER_FUNCTION: Transfers spiritual and social legitimacy from hereditary Brahminical authority to individuals and groups previously marginalized, while also transferring the burden of conforming to egalitarian norms onto traditionalist institutions.
% ABSENT_VOICES: Extremist fundamentalist groups who reject constitutional authority over religious texts are largely excluded from mainstream discourse and legal processes, but their resistance manifests in social unrest and political pressure.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and social landscape would revert to a more traditionalist, caste-hierarchical interpretation, leading to increased social stratification, renewed discrimination, and a significant setback for equality movements. The Indian state's secular and egalitarian foundations would be severely undermined.
% FOUNDING_PROBLEM: The historical problem of deep-seated caste discrimination and social inequality justified by traditional religious interpretations, leading to systemic injustice and social fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Dalit movements, secular human rights organizations, and the Indian judiciary consistently attest that caste discrimination remains a live problem, requiring ongoing reform and reinterpretation. International human rights bodies also corroborate the persistence of caste-based discrimination, providing external validation for the problem's live status.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__reformist_egalitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__reformist_egalitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__reformist_egalitarian_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vedic_dharmic_corpus__reformist_egalitarian_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate because this reading extracts legitimacy and social capital from traditionalist institutions and individuals, reallocating it to previously marginalized groups. Suppression (0.30) is present as the Indian state's legal framework actively suppresses caste discrimination and enforces equality, thereby constraining traditionalist practices. Theater ratio (0.20) is low, as the efforts to implement this reading are genuinely aimed at social reform, though some performative aspects may exist in political discourse. The increasing extractiveness over time reflects the growing assertiveness of egalitarian movements and the state's increasing intervention.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Dalit movements and secular reformers, this reading is a necessary and just reinterpretation that corrects historical wrongs. For orthodox Brahminical institutions and traditionalist communities, it is an imposition that undermines sacred tradition and their social standing. The Indian state judiciary views it as a constitutional imperative. The engine will compute these divergent classifications based on the declared roles, power, and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Dalit movements and secular reformers are beneficiaries, as this reading empowers them and validates their claims, leading to a low directionality (d). Orthodox Brahminical institutions and traditionalist communities are payers, as they lose authority and face pressure to change, resulting in a high directionality (d). The Indian state judiciary, as the agenda setter, benefits from upholding constitutional principles and maintaining social order, placing its directionality closer to the beneficiary end, but with the costs of enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Tangled Rope because it genuinely coordinates a diverse society around egalitarian principles (beneficiaries) while simultaneously extracting legitimacy and power from traditionalist structures (victims) through active enforcement by the state. It prevents mislabeling by acknowledging both the coordination function (social cohesion, equality) and the asymmetric extraction (loss of traditional privilege). The 'live' status of the founding problem (caste discrimination) confirms its ongoing mandate, preventing it from being a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scriptural_interpretation_ambiguity,
    'To what extent can the Vedic Dharmic corpus genuinely be interpreted to support egalitarian principles without selective reading or outright rejection of certain passages?',
    'Comprehensive philological and historical analysis of the texts, alongside comparative studies of interpretive traditions, to assess the internal coherence of the egalitarian reading.',
    'If the egalitarian reading requires significant recontextualization or rejection of core texts, its legitimacy as a ''reading'' of the corpus (rather than a new doctrine) is weakened, potentially increasing resistance and the perceived ''extraction'' from traditionalists. If it is demonstrably coherent, its moral and intellectual force is amplified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_interpretation_ambiguity, conceptual, 'Ambiguity regarding the textual basis for egalitarian interpretations within the Vedic Dharmic corpus.').

omega_variable(
    state_enforcement_legitimacy,
    'Is the Indian state''s enforcement of constitutional equality in religious matters perceived as legitimate intervention or secular overreach by a majority of the population?',
    'Sociological surveys, analysis of electoral outcomes, and public discourse studies to gauge popular acceptance of state intervention in religious interpretive disputes.',
    'If perceived as overreach, the state''s enforcement may generate greater resistance and delegitimize the reformist reading in the eyes of many, increasing the effective suppression required. If seen as legitimate, it strengthens the constraint''s persistence and reduces the ''cost'' of enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_enforcement_legitimacy, empirical, 'Public perception of the legitimacy of state intervention in religious interpretive matters.').

omega_variable(
    mandatrophy_of_caste_system,
    'Has the functional mandate of the caste system (e.g., division of labor, social stability) truly atrophied, or do traditionalists perceive it as still serving a vital, albeit contested, purpose?',
    'Ethnographic studies and historical analysis of traditionalist communities to understand their internal justifications for caste, alongside economic and sociological analysis of its contemporary functional relevance.',
    'If traditionalists'' claims of functional mandate are found to be widely held and internally coherent (even if contested externally), the ''dead'' status of the founding problem becomes more ''contested'', increasing the perceived extraction from their perspective. If the mandate is demonstrably atrophied, it strengthens the reformist position.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_caste_system, conceptual, 'Whether the functional mandate of the caste system has truly atrophied or is still perceived as vital by traditionalists.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__reformist_egalitarian_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1947, 0.1).
narrative_ontology:measurement(vedi_tr_t1967, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(vedi_tr_t1987, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 1987, 0.18).
narrative_ontology:measurement(vedi_tr_t2007, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2007, 0.19).
narrative_ontology:measurement(vedi_tr_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(vedi_be_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement(vedi_be_t1967, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1967, 0.35).
narrative_ontology:measurement(vedi_be_t1987, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 1987, 0.4).
narrative_ontology:measurement(vedi_be_t2007, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2007, 0.43).
narrative_ontology:measurement(vedi_be_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t1947, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1947, 0.2).
narrative_ontology:measurement(vedi_su_t1967, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement(vedi_su_t1987, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 1987, 0.28).
narrative_ontology:measurement(vedi_su_t2007, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2007, 0.29).
narrative_ontology:measurement(vedi_su_t2024, vedic_dharmic_corpus__reformist_egalitarian_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
