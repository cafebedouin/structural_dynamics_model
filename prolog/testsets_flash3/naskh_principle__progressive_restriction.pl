% ============================================================================
% CONSTRAINT STORY: naskh_principle__progressive_restriction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__progressive_restriction, []).

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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh Principle: Progressive Restriction Reading
 *   domain: islamic_jurisprudence/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'progressive restriction' reading of the
 *   Naskh principle in Quranic hermeneutics. It posits that later Quranic
 *   revelations progressively restricted permissions rather than abrogating
 *   earlier rulings, viewing the movement from permissive to restrictive as
 *   divine pedagogy. This reading benefits evolutionary legal interpretations
 *   and modernist reformers by providing a framework for adapting Islamic
 *   law, while it extracts from traditionalist scholars and lay practitioners
 *   who rely on a more direct or comprehensive application of all verses.
 *   This is one reading of the 'naskh_principle' kernel, distinct from
 *   'classical_abrogation' and 'contextual_harmonization'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.65).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.7).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.65).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh Principle: Progressive Restriction Reading").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '2fd4009b-fcfd-47b6-ba8c-8fc5286427ce').
narrative_ontology:cs_kernel_codification('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', fixed_text).
narrative_ontology:cs_authority_grounding('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', lineage).
narrative_ontology:cs_interpretation_layer_present('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce').
narrative_ontology:cs_reading_relation('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', foundational, divine_pedagogy_evolves).
narrative_ontology:cs_axiom_status(divine_pedagogy_evolves, holdable).
narrative_ontology:cs_axiom_grounding('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', divine_pedagogy_evolves, deontological).
narrative_ontology:cs_axiom('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', foundational, later_revelation_restricts_earlier).
narrative_ontology:cs_axiom_status(later_revelation_restricts_earlier, holdable).
narrative_ontology:cs_axiom_grounding('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', later_revelation_restricts_earlier, conventional).
narrative_ontology:cs_reference_frame('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', pedagogical_divine_guidance).
narrative_ontology:cs_drift_state('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2fd4009b-fcfd-47b6-ba8c-8fc5286427ce', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, evolutionary_legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, modernist_reformers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, traditionalist_scholars).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, lay_practitioners_citing_permissive_texts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for this reading, interpreting earlier permissive verses as transitional steps in divine pedagogy. They gain interpretive authority by presenting a coherent, evolving legal framework that aligns with modern sensibilities while maintaining textual integrity. They actively enforce this interpretation through academic discourse and fatwas.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_scholars, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from this reading as it provides a theological basis for legal and social reforms, allowing for a more flexible application of Islamic law in contemporary contexts. They use this interpretation to justify changes in practice without directly challenging the Quran's authority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, modernist_reformers, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of this reading as it undermines their methodology of deriving law from all verses, potentially invalidating their established interpretations. They resist this reading, arguing it misrepresents the nature of revelation and introduces unwarranted innovation.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditionalist_scholars, payer,
    institutional, generational, constrained, global).

% Are confused or disenfranchised when their understanding of earlier, more permissive verses is deemed superseded or merely 'transitional.' They may feel their direct engagement with the text is undermined, leading to a sense of disempowerment in their religious practice.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, lay_practitioners_citing_permissive_texts, payer,
    powerless, immediate, identity_locked, local).

% Their methodology of direct abrogation is implicitly challenged by this reading, which prefers 'restriction' over 'invalidation.' While not directly paying, their interpretive framework is sidelined in favor of a more nuanced, pedagogical approach.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, classical_abrogation_proponents, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for reconciling apparent contradictions or shifts in legal rulings within the Quran, allowing for a coherent and evolving understanding of divine law over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from a literal, chronological abrogation model to a pedagogical, developmental model, benefiting scholars who seek to present Islamic law as adaptable and progressively revealed.
% ABSENT_VOICES: Strict literalists who insist on the equal and timeless validity of all verses without chronological or pedagogical distinctions would object, arguing that any interpretation that diminishes the direct applicability of a verse is a distortion of divine intent. They are often marginalized in mainstream academic discourse on hermeneutics.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the coherence of an evolutionary legal framework would collapse, leading to renewed interpretive chaos regarding seemingly contradictory verses. Scholars would struggle to present a unified, progressive vision of Islamic law, and traditionalist interpretations might gain ground, potentially leading to more rigid applications of earlier texts.
% FOUNDING_PROBLEM: The apparent contradictions and chronological shifts in legal rulings within the Quran, particularly between earlier, more permissive verses and later, more restrictive ones, posed a challenge to the coherence and consistency of divine revelation.
% FOUNDING_PROBLEM_CORROBORATION: Scholars across various schools of thought acknowledge the interpretive challenge posed by these textual dynamics. While the proposed solutions differ, the existence of the 'problem' itself is widely corroborated by centuries of jurisprudential debate and textual analysis, even by those who advocate for different solutions.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__progressive_restriction, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__progressive_restriction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__progressive_restriction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__progressive_restriction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the interpretive authority claimed by proponents of this reading, which redefines the applicability of certain verses, effectively 'taxing' alternative interpretations. Suppression (0.7) is high because this reading requires active enforcement through scholarly consensus, fatwas, and educational curricula to marginalize competing hermeneutical approaches. The theater ratio is low (0.1) as the interpretive work is genuinely functional in resolving textual tensions, even if it serves specific ideological ends. The increasing extractiveness and suppression over time reflect the growing institutionalization and defense of this reading against challenges.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of progressive restriction view it as a necessary and elegant solution to textual complexities, a 'rope' that coordinates understanding. Opponents, however, experience it as a 'snare' that extracts interpretive freedom and imposes a specific, often anachronistic, reading onto the divine text. The engine's classification will capture this divergence from the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary legal scholars and modernist reformers are beneficiaries (low d) as this reading provides a powerful tool for their interpretive and reform agendas. Traditionalist scholars and lay practitioners who cite earlier permissive texts are victims (high d) because their interpretive methods or practices are undermined or invalidated by this framework. The constraint subsidizes the former by granting them interpretive leverage and extracts from the latter by imposing a specific hermeneutical lens.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_chronology_accuracy,
    'How reliably can the precise chronological order of Quranic revelations be established to support the ''progressive restriction'' argument?',
    'Further historical and textual critical studies to refine the chronology of revelation, cross-referenced with early Islamic historical accounts and exegetical traditions.',
    'If chronology is found to be less certain, the empirical basis for ''progressive restriction'' weakens, potentially shifting interpretive authority back to ''contextual harmonization'' or ''classical abrogation'' readings. If confirmed, it strengthens this reading''s claim to textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_chronology_accuracy, empirical, 'Uncertainty regarding the precise chronological order of Quranic verses, which is foundational to this reading.').

omega_variable(
    divine_pedagogy_vs_abrogation_intent,
    'Is the ''progressive restriction'' interpretation of divine pedagogy a conceptual framing that avoids the theological implications of direct abrogation, or does it genuinely reflect the Quran''s internal logic?',
    'Comparative theological analysis across different scriptural traditions regarding divine communication and legal evolution, alongside internal Quranic linguistic and thematic studies to discern authorial intent.',
    'If primarily a conceptual framing, its legitimacy might be seen as instrumental rather than intrinsic, potentially reducing its authority for those seeking direct divine intent. If intrinsic, it solidifies its position as a robust hermeneutical principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_pedagogy_vs_abrogation_intent, conceptual, 'Ambiguity in whether ''progressive restriction'' is a theological construct or an inherent feature of the Quranic text.').

omega_variable(
    impact_on_lay_practice,
    'What is the actual impact of this reading on the religious practices and interpretive autonomy of lay practitioners who may not be aware of or agree with its scholarly nuances?',
    'Sociological and ethnographic studies of Muslim communities, observing how different interpretive frameworks are adopted or resisted in daily religious life and personal legal decisions.',
    'If the impact is found to be significantly disempowering or confusing for lay practitioners, it raises ethical questions about the accessibility and practical implications of scholarly hermeneutics, potentially leading to calls for more inclusive interpretive methodologies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_lay_practice, empirical, 'The unmeasured practical consequences of this scholarly reading on the broader Muslim populace.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nask_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(nask_be_t30, naskh_principle__progressive_restriction, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(nask_be_t40, naskh_principle__progressive_restriction, base_extractiveness, 40, 0.64).
narrative_ontology:measurement(nask_be_t50, naskh_principle__progressive_restriction, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nask_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(nask_su_t30, naskh_principle__progressive_restriction, suppression_requirement, 30, 0.68).
narrative_ontology:measurement(nask_su_t40, naskh_principle__progressive_restriction, suppression_requirement, 40, 0.69).
narrative_ontology:measurement(nask_su_t50, naskh_principle__progressive_restriction, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
