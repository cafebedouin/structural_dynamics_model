% ============================================================================
% CONSTRAINT STORY: dharmasastra_corpus__reformist_contextual
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dharmasastra_corpus__reformist_contextual, []).

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
 *   constraint_id: dharmasastra_corpus__reformist_contextual
 *   human_readable: Dharmasastra: Reformist Contextual Reading
 *   domain: religious_law/textual_interpretation/normative_authority
 *
 * SUMMARY:
 *   This constraint represents the 'reformist contextual' reading of
 *   Dharmasastra, which seeks to interpret the ancient Hindu legal and
 *   ethical texts by separating a universal ethical core (dharma as righteous
 *   conduct) from time-bound social prescriptions, particularly those related
 *   to caste (varna/jati) and gender. This reading aims to maintain the
 *   texts' authority and relevance in modern society by adapting them to
 *   contemporary ethical standards. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates the tradition's continuity but still
 *   carries a symbolic extraction from historically marginalized groups by
 *   not fully repudiating the textual basis of their disadvantage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dharmasastra_corpus__reformist_contextual, 0.45).
domain_priors:suppression_score(dharmasastra_corpus__reformist_contextual, 0.3).
domain_priors:theater_ratio(dharmasastra_corpus__reformist_contextual, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, extractiveness, 0.45).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dharmasastra_corpus__reformist_contextual, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dharmasastra_corpus__reformist_contextual, tangled_rope).
narrative_ontology:human_readable(dharmasastra_corpus__reformist_contextual, "Dharmasastra: Reformist Contextual Reading").
narrative_ontology:topic_domain(dharmasastra_corpus__reformist_contextual, "religious_law/textual_interpretation/normative_authority").

domain_priors:requires_active_enforcement(dharmasastra_corpus__reformist_contextual).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dharmasastra_corpus__reformist_contextual, 'b7e5933b-1e05-47a4-b223-06631249ece5').
narrative_ontology:cs_kernel_codification('b7e5933b-1e05-47a4-b223-06631249ece5', fixed_text).
narrative_ontology:cs_authority_grounding('b7e5933b-1e05-47a4-b223-06631249ece5', lineage).
narrative_ontology:cs_interpretation_layer_present('b7e5933b-1e05-47a4-b223-06631249ece5').
narrative_ontology:cs_reading_relation('b7e5933b-1e05-47a4-b223-06631249ece5', dharmasastra_corpus__orthodox_literalist, coexists_with).
narrative_ontology:cs_reading_relation('b7e5933b-1e05-47a4-b223-06631249ece5', dharmasastra_corpus__abolitionist_rejection, coexists_with).
narrative_ontology:cs_axiom('b7e5933b-1e05-47a4-b223-06631249ece5', foundational, dharma_as_universal_ethics_is_primary).
narrative_ontology:cs_axiom_status(dharma_as_universal_ethics_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('b7e5933b-1e05-47a4-b223-06631249ece5', dharma_as_universal_ethics_is_primary, deontological).
narrative_ontology:cs_axiom('b7e5933b-1e05-47a4-b223-06631249ece5', foundational, social_prescriptions_are_historically_contingent).
narrative_ontology:cs_axiom_status(social_prescriptions_are_historically_contingent, holdable).
narrative_ontology:cs_axiom_grounding('b7e5933b-1e05-47a4-b223-06631249ece5', social_prescriptions_are_historically_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('b7e5933b-1e05-47a4-b223-06631249ece5', dharmic_ethical_core).
narrative_ontology:cs_drift_state('b7e5933b-1e05-47a4-b223-06631249ece5', contemporary_global_ethics, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b7e5933b-1e05-47a4-b223-06631249ece5', '').
narrative_ontology:cs_kernel_id(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, reformist_hindu_scholars).
narrative_ontology:constraint_beneficiary(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, lower_caste_communities_symbolic).
narrative_ontology:constraint_victim(dharmasastra_corpus__reformist_contextual, women_symbolic).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Dharmasastra texts to emphasize universal ethical principles (dharma) while re-contextualizing or de-emphasizing caste-based and gender-specific prescriptions as historically contingent. They seek to maintain the texts' authority in a modern context.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, reformist_hindu_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from a reading that allows them to engage with contemporary ethical standards and social justice movements without fully abandoning traditional texts. This reading helps maintain relevance and attract younger, globally-minded adherents.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, modern_hindu_institutions, beneficiary,
    organized, biographical, mobile, national).

% While direct legal enforcement of caste is largely absent, this reading still implicitly acknowledges a historical hierarchy, which can perpetuate symbolic disadvantage and social stigma, even if not material extraction. Their identity is often tied to the tradition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, lower_caste_communities_symbolic, payer,
    powerless, generational, identity_locked, local).

% Benefit from the softening of overtly patriarchal rules but may still experience subtle forms of exclusion or prescribed roles derived from reinterpreted textual passages, maintaining a symbolic rather than strictly enforced disadvantage.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, women_symbolic, payer,
    moderate, biographical, constrained, local).

% Adhere to a literal interpretation of Dharmasastra, including caste and gender prescriptions. They are excluded from the reformist discourse's agenda-setting, viewing it as a dilution of sacred tradition.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, orthodox_literalist_adherents, excluded,
    organized, generational, constrained, regional).

% Critique any interpretation that retains elements of caste or gender hierarchy, advocating for universal human rights and equality. They observe the internal debates within Hinduism but operate from an external normative framework.
narrative_ontology:constraint_stakeholder(dharmasastra_corpus__reformist_contextual, secular_human_rights_advocates, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interpretation and application of ancient religious texts to maintain their relevance and authority for a modern, ethically conscious global Hindu community, bridging tradition with contemporary values.
% TRANSFER_FUNCTION: Transfers interpretive authority from rigid, literal adherence to a more flexible, contextual approach, allowing modern institutions to retain adherents and legitimacy, while symbolically extracting from historically marginalized groups by not fully repudiating the textual basis of their disadvantage.
% ABSENT_VOICES: Abolitionist voices who reject Dharmasastra entirely are absent from this reformist conversation, as their position would undermine the very textual authority the reformists seek to preserve. Orthodox literalists are also excluded from the reformist agenda-setting, as their views are deemed incompatible with modern ethics.
% DISAPPEARANCE_RATIONALE: If this reformist contextual reading vanished, modern Hindu institutions would face a crisis of legitimacy, either reverting to more orthodox interpretations (losing progressive adherents) or fully abandoning the texts (losing traditionalists). The global discourse on Hinduism's role in contemporary ethics would fundamentally shift.
% FOUNDING_PROBLEM: The challenge of reconciling ancient, socially stratified religious texts with modern, egalitarian ethical norms and global human rights standards, threatening the relevance and moral standing of Hinduism.
% FOUNDING_PROBLEM_CORROBORATION: Scholars of comparative religion and sociology of religion, as well as internal debates within Hindu communities globally, corroborate the ongoing tension between tradition and modernity as a live and pressing issue for the faith's future.
narrative_ontology:disappearance_verdict(dharmasastra_corpus__reformist_contextual, world_rearranges).
narrative_ontology:founding_problem_status(dharmasastra_corpus__reformist_contextual, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dharmasastra_corpus__reformist_contextual, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dharmasastra_corpus__reformist_contextual, 'none', 1).
narrative_ontology:epsilon_provenance(dharmasastra_corpus__reformist_contextual, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dharmasastra_corpus__reformist_contextual_tests).
:- end_tests(dharmasastra_corpus__reformist_contextual_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the softening of direct enforcement but the persistence of symbolic hierarchy. Suppression (0.30) is low, as direct coercion is minimal, but social pressure and identity-lock mechanisms still operate. Theater ratio (0.20) is low, as the reformist project is a genuine attempt at reinterpretation, not mere performance. The temporal measurements show a decreasing extractiveness and suppression over time, reflecting the ongoing efforts to liberalize interpretations and the declining social enforcement of traditional hierarchies.
 *
 * PERSPECTIVAL GAP:
 *   Reformist scholars perceive this reading as a necessary and beneficial adaptation, a 'rope' that preserves the tradition. However, from the perspective of historically marginalized groups, it may still feel like a 'tangled_rope' or even a 'snare' due to the lingering symbolic weight of the texts and the incomplete repudiation of past injustices. The engine's classification as 'tangled_rope' captures this hybridity.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist scholars and modern Hindu institutions are beneficiaries, as this reading allows them to navigate modernity while preserving tradition. Lower caste communities and women are symbolic payers; while direct material extraction is reduced, the continued acknowledgment of texts that historically justified their subordination still imposes a cost in terms of social status and identity. Orthodox literalists are excluded, as their views are incompatible with the reformist agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading attempts to resolve mandatrophy by re-contextualizing the original mandate (social order in ancient India) to a new one (ethical guidance in modern society). The 'tangled_rope' classification prevents mislabeling it as pure coordination, acknowledging the residual extraction from its historical baggage, while also not dismissing its genuine coordination function for the tradition's continuity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    symbolic_vs_material_extraction,
    'To what extent does the ''symbolic'' extraction from lower caste communities and women translate into tangible social or economic disadvantage in contemporary practice?',
    'Empirical sociological studies measuring disparities in access, opportunity, and social acceptance in communities influenced by this reading, compared to those under other readings or secular norms.',
    'If symbolic extraction correlates strongly with material disadvantage, the constraint''s effective extractiveness would be higher, pushing it closer to a Snare. If the correlation is weak, the ''tangled_rope'' classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_vs_material_extraction, empirical, 'Distinguishing between symbolic and material forms of extraction.').

omega_variable(
    textual_authority_vs_ethical_autonomy,
    'Is the attempt to preserve textual authority (even through reinterpretation) inherently in tension with full ethical autonomy and equality for all individuals, or can the two be fully reconciled?',
    'Philosophical and theological analysis of the internal coherence of the reformist position, and long-term observation of whether new ethical challenges lead to further reinterpretation or a reassertion of textual limits.',
    'If an inherent tension exists, the ''tangled_rope'' aspect is more fundamental and less resolvable through reinterpretation alone. If full reconciliation is possible, the constraint could evolve towards a ''rope'' over time.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_authority_vs_ethical_autonomy, conceptual, 'The conceptual tension between preserving textual authority and achieving full ethical equality.').

omega_variable(
    reformist_legitimacy_source,
    'Does the reformist reading derive its legitimacy primarily from internal theological arguments for reinterpretation, or from external pressure to conform to global ethical norms?',
    'Content analysis of reformist scholarly publications and institutional statements, and interviews with key figures, to identify the dominant justifications for their interpretive approach.',
    'If external pressure is the primary driver, the constraint''s persistence is more contingent on external social forces and less on internal textual coherence, potentially making it more fragile or susceptible to ''theater'' if the external pressure wanes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reformist_legitimacy_source, empirical, 'Source of legitimacy for the reformist interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dharmasastra_corpus__reformist_contextual, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dhar_tr_t1950, dharmasastra_corpus__reformist_contextual, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(dhar_tr_t1970, dharmasastra_corpus__reformist_contextual, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(dhar_tr_t1990, dharmasastra_corpus__reformist_contextual, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(dhar_tr_t2010, dharmasastra_corpus__reformist_contextual, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(dhar_tr_t2024, dharmasastra_corpus__reformist_contextual, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(dhar_be_t1950, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(dhar_be_t1970, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(dhar_be_t1990, dharmasastra_corpus__reformist_contextual, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(dhar_be_t2010, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2010, 0.47).
narrative_ontology:measurement(dhar_be_t2024, dharmasastra_corpus__reformist_contextual, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(dhar_su_t1950, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(dhar_su_t1970, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(dhar_su_t1990, dharmasastra_corpus__reformist_contextual, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(dhar_su_t2010, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2010, 0.32).
narrative_ontology:measurement(dhar_su_t2024, dharmasastra_corpus__reformist_contextual, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dharmasastra_corpus__reformist_contextual, identity_coordination).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__orthodox_literalist).
narrative_ontology:affects_constraint(dharmasastra_corpus__reformist_contextual, dharmasastra_corpus__abolitionist_rejection).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Dharmasastra corpus kernel. This 'reformist contextual' reading attempts to bridge tradition and modernity, influencing both orthodox and abolitionist positions by offering an alternative path for engagement with the texts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
