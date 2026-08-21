% ============================================================================
% CONSTRAINT STORY: vedic_dharmic_corpus__bhakti_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vedic_dharmic_corpus__bhakti_devotional_reading, []).

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
 *   constraint_id: vedic_dharmic_corpus__bhakti_devotional_reading
 *   human_readable: Bhakti Devotional Access to Divine Authority
 *   domain: religious_authority/social_stratification
 *
 * SUMMARY:
 *   This constraint represents the 'bhakti devotional' reading of the Vedic
 *   Dharmic corpus, which asserts that sincere devotion (bhakti) to the
 *   divine provides direct spiritual access, bypassing traditional
 *   caste-based ritual requirements. It challenges the notion that spiritual
 *   authority is solely determined by birth into a Brahmin lineage, offering
 *   a more egalitarian path to spiritual realization. While it significantly
 *   reduces the extractiveness of the hereditary system, it does not fully
 *   dismantle the broader social stratification.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4).
domain_priors:suppression_score(vedic_dharmic_corpus__bhakti_devotional_reading, 0.35).
domain_priors:theater_ratio(vedic_dharmic_corpus__bhakti_devotional_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(vedic_dharmic_corpus__bhakti_devotional_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vedic_dharmic_corpus__bhakti_devotional_reading, rope).
narrative_ontology:human_readable(vedic_dharmic_corpus__bhakti_devotional_reading, "Bhakti Devotional Access to Divine Authority").
narrative_ontology:topic_domain(vedic_dharmic_corpus__bhakti_devotional_reading, "religious_authority/social_stratification").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vedic_dharmic_corpus__bhakti_devotional_reading, '4f020e15-0539-4938-83a7-ffee48081531').
narrative_ontology:cs_kernel_codification('4f020e15-0539-4938-83a7-ffee48081531', fixed_text).
narrative_ontology:cs_authority_grounding('4f020e15-0539-4938-83a7-ffee48081531', practice).
narrative_ontology:cs_interpretation_layer_present('4f020e15-0539-4938-83a7-ffee48081531').
narrative_ontology:cs_reading_relation('4f020e15-0539-4938-83a7-ffee48081531', vedic_dharmic_corpus__hereditary_monopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('4f020e15-0539-4938-83a7-ffee48081531', vedic_dharmic_corpus__reformist_egalitarian_reading, coexists_with).
narrative_ontology:cs_axiom('4f020e15-0539-4938-83a7-ffee48081531', foundational, sincere_devotion_is_supreme).
narrative_ontology:cs_axiom_status(sincere_devotion_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('4f020e15-0539-4938-83a7-ffee48081531', sincere_devotion_is_supreme, theological).
narrative_ontology:cs_axiom('4f020e15-0539-4938-83a7-ffee48081531', foundational, birth_is_irrelevant_to_spiritual_merit).
narrative_ontology:cs_axiom_status(birth_is_irrelevant_to_spiritual_merit, holdable).
narrative_ontology:cs_axiom_grounding('4f020e15-0539-4938-83a7-ffee48081531', birth_is_irrelevant_to_spiritual_merit, deontological).
narrative_ontology:cs_reference_frame('4f020e15-0539-4938-83a7-ffee48081531', universal_spiritual_access).
narrative_ontology:cs_drift_state('4f020e15-0539-4938-83a7-ffee48081531', contemporary_pluralistic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('4f020e15-0539-4938-83a7-ffee48081531', '').
narrative_ontology:cs_kernel_id(vedic_dharmic_corpus__bhakti_devotional_reading, vedic_dharmic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas).
narrative_ontology:constraint_beneficiary(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_saints_and_gurus).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_caste_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Leaders and proponents of devotional movements who establish new spiritual lineages and communities, challenging traditional hierarchies by offering direct access to the divine through devotion. They gain spiritual authority and followers.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_saints_and_gurus, agenda_setter,
    organized, generational, mobile, regional).

% Individuals from all social strata, including those traditionally excluded from ritual authority, who find spiritual fulfillment and community through direct devotion. They benefit from accessible spiritual paths and reduced reliance on hereditary intermediaries.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, devotees_of_all_varnas, beneficiary,
    moderate, biographical, mobile, local).

% Hereditary custodians of ritual and textual authority who see their exclusive role and social standing challenged by the rise of devotional movements. They bear the cost of diminished influence and loss of monopoly over spiritual mediation.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, traditional_brahminical_priesthood, payer,
    institutional, generational, constrained, national).

% Those whose social and political power is intertwined with the traditional caste hierarchy. While bhakti primarily challenges spiritual authority, its egalitarian implications can indirectly undermine the broader social stratification they benefit from.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, hereditary_caste_elites, payer,
    powerful, generational, constrained, national).

% Academics and intellectuals who study the historical and sociological impact of bhakti movements, analyzing their role in social change and spiritual democratization within the broader Dharmic tradition.
narrative_ontology:constraint_stakeholder(vedic_dharmic_corpus__bhakti_devotional_reading, reformist_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vedic_dharmic_corpus__bhakti_devotional_reading, bhakti_saints_and_gurus).
narrative_ontology:fixing_cost_class(vedic_dharmic_corpus__bhakti_devotional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates spiritual communities around shared devotional practices and accessible paths to the divine, fostering social cohesion and individual spiritual agency across traditional social divisions.
% TRANSFER_FUNCTION: Transfers spiritual authority and access from birthright and ritual exclusivity to individual sincerity and devotion; transfers social belonging from rigid caste structures to inclusive devotional communities.
% ABSENT_VOICES: Hardline traditionalists who reject any challenge to birth-based ritual monopoly and divinely ordained varna hierarchy. They are often marginalized or actively resisted by bhakti movements.
% DISAPPEARANCE_RATIONALE: If the bhakti devotional path vanished, spiritual access would largely revert to being mediated by birth and complex rituals, reinforcing traditional caste hierarchies and limiting individual spiritual agency. The social and spiritual landscape would become significantly more rigid and exclusive.
% FOUNDING_PROBLEM: The spiritual exclusion and rigid social stratification imposed by a purely hereditary ritual system, leading to spiritual alienation and lack of direct access to the divine for many.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the emergence of bhakti movements, the writings of bhakti saints, and contemporary sociological analyses of caste and religious practice, which attest to the ongoing challenges of social stratification and spiritual access within Dharmic traditions.
narrative_ontology:disappearance_verdict(vedic_dharmic_corpus__bhakti_devotional_reading, world_rearranges).
narrative_ontology:founding_problem_status(vedic_dharmic_corpus__bhakti_devotional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vedic_dharmic_corpus__bhakti_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vedic_dharmic_corpus__bhakti_devotional_reading, 0.4, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).
:- end_tests(vedic_dharmic_corpus__bhakti_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.40) is moderate because while bhakti offers a bypass, the traditional caste system and its associated social costs persist. Suppression (0.35) is relatively low for this constraint itself, as it represents a path of spiritual liberation rather than coercion, though it faces resistance from traditionalists. Theater ratio (0.15) is low, reflecting the sincerity and directness of devotional practice. Accessibility collapse (0.40) is moderate, as bhakti opens up spiritual avenues previously restricted. Resistance (0.50) is moderate, as bhakti movements have historically faced opposition from established hierarchies but also gained widespread popular support.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of bhakti adherents, this constraint is a liberating 'rope' that provides universal spiritual access. From the perspective of traditional Brahminical authorities, it is a challenge to divinely ordained order, potentially seen as a 'snare' that undermines dharma. The engine's classification will reflect the structural reality of the bypass, not the traditionalists' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Bhakti saints and devotees of all varnas are beneficiaries, gaining spiritual authority and access. The traditional Brahminical priesthood and hereditary caste elites are victims, as their monopoly on spiritual mediation and social hierarchy is challenged. The constraint's operation shifts spiritual power dynamics, reducing the 'd' value for those previously excluded and increasing it for those who maintained exclusive control.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bhakti_social_impact_ambiguity,
    'To what extent does direct devotional access truly dismantle social caste hierarchies, versus merely providing a spiritual bypass that leaves social structures largely intact?',
    'Sociological studies tracking inter-caste relations, marriage patterns, and economic mobility within and outside bhakti communities over generations.',
    'If social hierarchies remain largely intact, the effective extractiveness of the broader caste system (a sibling constraint) remains high, even if spiritual access is democratized. This reading''s ''rope'' classification might be re-evaluated as a ''tangled_rope'' if it inadvertently reinforces social stratification while offering spiritual solace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bhakti_social_impact_ambiguity, empirical, 'The actual social impact of bhakti on caste stratification.').

omega_variable(
    interpretive_legitimacy_source,
    'Does the spiritual authority gained through bhakti ultimately seek validation from, or remain independent of, traditional Brahminical textual interpretation and ritual authority?',
    'Analysis of how bhakti traditions engage with or reinterpret Vedic texts, and whether their spiritual leaders are recognized by or operate entirely outside traditional religious institutions.',
    'If bhakti traditions consistently seek validation from traditional structures, their independence is compromised, and the ''hereditary_monopoly_reading'' might exert more ''influences'' pressure than currently assessed. If fully independent, this reading''s ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_legitimacy_source, conceptual, 'The source and independence of bhakti''s interpretive legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vedic_dharmic_corpus__bhakti_devotional_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vedi_tr_t0, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(vedi_tr_t20, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement(vedi_tr_t40, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(vedi_tr_t60, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 60, 0.12).
narrative_ontology:measurement(vedi_tr_t80, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(vedi_tr_t100, vedic_dharmic_corpus__bhakti_devotional_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(vedi_be_t0, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vedi_be_t20, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(vedi_be_t40, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(vedi_be_t60, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(vedi_be_t80, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 80, 0.38).
narrative_ontology:measurement(vedi_be_t100, vedic_dharmic_corpus__bhakti_devotional_reading, base_extractiveness, 100, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(vedi_su_t0, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(vedi_su_t20, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(vedi_su_t40, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 40, 0.37).
narrative_ontology:measurement(vedi_su_t60, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 60, 0.35).
narrative_ontology:measurement(vedi_su_t80, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 80, 0.33).
narrative_ontology:measurement(vedi_su_t100, vedic_dharmic_corpus__bhakti_devotional_reading, suppression_requirement, 100, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vedic_dharmic_corpus__bhakti_devotional_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
