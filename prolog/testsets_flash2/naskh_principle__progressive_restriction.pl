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
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh Principle: Progressive Restriction Reading
 *   domain: islamic_jurisprudence/hermeneutics
 *
 * SUMMARY:
 *   This constraint represents the 'progressive restriction' reading of the
 *   Naskh (abrogation) principle in Quranic hermeneutics. It posits that
 *   later Quranic revelations progressively restricted permissions rather
 *   than directly abrogating earlier rulings. This movement from permissive
 *   to restrictive is understood as divine pedagogy, not textual
 *   invalidation. This reading benefits scholars and reformers seeking to
 *   present Islamic law as dynamic and adaptable, while challenging
 *   traditionalist interpretations and marginalizing those who adhere to
 *   earlier, more permissive textual understandings. The constraint is
 *   claimed as a Rope by its proponents, emphasizing its coordination
 *   function in resolving textual tensions, but its operation is
 *   substantially extractive and actively enforced, making it a Tangled Rope
 *   from an analytical perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.65).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.7).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.65).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh Principle: Progressive Restriction Reading").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/hermeneutics").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58').
narrative_ontology:cs_kernel_codification('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', fixed_text).
narrative_ontology:cs_authority_grounding('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', lineage).
narrative_ontology:cs_interpretation_layer_present('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58').
narrative_ontology:cs_reading_relation('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', naskh_principle__classical_abrogation, coexists_with).
narrative_ontology:cs_reading_relation('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', naskh_principle__contextual_harmonization, coexists_with).
narrative_ontology:cs_axiom('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', foundational, divine_pedagogy_in_revelation).
narrative_ontology:cs_axiom_status(divine_pedagogy_in_revelation, holdable).
narrative_ontology:cs_axiom_grounding('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', divine_pedagogy_in_revelation, theological).
narrative_ontology:cs_axiom('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', foundational, later_revelation_restricts_earlier_permissions).
narrative_ontology:cs_axiom_status(later_revelation_restricts_earlier_permissions, holdable).
narrative_ontology:cs_axiom_grounding('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', later_revelation_restricts_earlier_permissions, conventional).
narrative_ontology:cs_reference_frame('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', quranic_text_as_evolving_guidance).
narrative_ontology:cs_drift_state('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', contemporary_islamic_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('5e78bf38-9ce1-4e16-b8b4-3b0af61d9c58', '').
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

% These scholars benefit from a hermeneutic that allows for a dynamic understanding of Islamic law, aligning it with modern ethical and social developments. They gain interpretive authority by presenting the Quran as a progressively revealed text.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, evolutionary_legal_scholars, beneficiary,
    institutional, generational, mobile, global).

% Reformers use this reading to advocate for legal and social changes, arguing that earlier, more permissive rulings were temporary and the later, more restrictive ones represent the final, evolved divine intent. This provides a theological basis for their reform efforts.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, modernist_reformers, beneficiary,
    organized, biographical, constrained, national).

% These scholars find their interpretive authority challenged by this reading, as it reinterprets established understandings of abrogation. They bear the cost of having to defend classical methodologies against a framework that re-orders textual priority.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditionalist_scholars, payer,
    institutional, generational, constrained, global).

% Individuals who rely on earlier, more permissive interpretations for their personal or communal practice find their understanding invalidated or marginalized. They are identity-locked by their commitment to a particular reading of the sacred text and community norms.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, lay_practitioners_citing_permissive_texts, payer,
    powerless, immediate, identity_locked, local).

% These institutions, such as universities and fatwa councils, are the primary arbiters of Quranic interpretation. They administer and enforce the dominant hermeneutical principles, including this reading, shaping legal discourse and practice.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, islamic_legal_institutions, agenda_setter,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent framework for understanding apparent chronological shifts in Quranic legal rulings, allowing for a unified and evolving divine pedagogy rather than contradictory commands.
% TRANSFER_FUNCTION: Transfers interpretive authority from literal chronological abrogation or broad contextualization to a model of divine pedagogical progression, benefiting those who advocate for a dynamic legal evolution.
% ABSENT_VOICES: Early Islamic jurists who established the classical abrogation theory are absent from contemporary debates, their original reasoning often reinterpreted or dismissed by proponents of progressive restriction. Their voices would emphasize the directness of abrogation.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the coherence of modern Islamic legal reform efforts would be severely undermined. Scholars would struggle to reconcile permissive and restrictive verses without a pedagogical framework, leading to significant theological and legal disarray.
% FOUNDING_PROBLEM: The problem of reconciling seemingly contradictory or chronologically shifting legal injunctions within the Quran, particularly the movement from more permissive to more restrictive rulings.
% FOUNDING_PROBLEM_CORROBORATION: Scholars across various schools of thought, including those who disagree with this specific resolution, acknowledge the historical and ongoing challenge of reconciling these verses. The problem is attested by centuries of jurisprudential debate, not just by the beneficiaries of this reading.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.65) stems from the re-ordering of interpretive authority, which marginalizes alternative readings and the scholars who champion them. Suppression (0.70) is high because this reading requires active enforcement by legal institutions to establish its dominance over competing hermeneutics, effectively suppressing the application of earlier, more permissive texts. The theater ratio (0.20) is relatively low, as the pedagogical justification is genuinely held by its proponents, though it serves to legitimize the interpretive shift. The increasing extractiveness and suppression over time reflect the growing institutionalization and enforcement of this reading within modern Islamic legal discourse.
 *
 * PERSPECTIVAL GAP:
 *   Proponents of progressive restriction view it as a necessary and coherent interpretive tool (Rope-like), resolving textual tensions and enabling legal evolution. However, from the perspective of traditionalist scholars and lay practitioners, it functions as an extractive mechanism (Snare-like) that re-orders divine intent and invalidates their established practices, enforced by institutional authority. The engine's classification as Tangled Rope captures this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Evolutionary legal scholars and modernist reformers are beneficiaries (low d) as this reading provides a framework for their interpretive and reform agendas. Traditionalist scholars and lay practitioners citing permissive texts are victims (high d) as their established understandings are challenged or invalidated. Islamic legal institutions act as agenda-setters, enforcing this reading and benefiting from the interpretive coherence it provides, thus having a low d.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_pedagogy_vs_abrogation,
    'Is the movement from permissive to restrictive rulings truly divine pedagogy, or is it a form of abrogation re-framed to avoid textual invalidation?',
    'Deep textual analysis of early Islamic legal commentaries and linguistic studies of Quranic terms for ''abrogation'' and ''restriction'' to discern the original intent and usage.',
    'If it is primarily a re-framing of abrogation, the ''pedagogy'' aspect might be seen as a rhetorical cover, increasing the perceived extractiveness and theater ratio. If genuine pedagogy, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_pedagogy_vs_abrogation, conceptual, 'Distinguishing between genuine pedagogical intent and re-framed abrogation.').

omega_variable(
    institutional_enforcement_legitimacy,
    'To what extent does the institutional enforcement of this reading derive from its inherent theological coherence versus the power dynamics within contemporary Islamic legal institutions?',
    'Sociological study of fatwa councils and jurisprudential academies, analyzing how this reading is promoted and how dissenting views are treated, alongside a theological assessment of its internal consistency.',
    'If enforcement is primarily power-driven, the suppression metric is more indicative of coercion than coordination, pushing the classification closer to a Snare. If coherence-driven, the Rope aspect is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_enforcement_legitimacy, empirical, 'Source of enforcement legitimacy: theological coherence vs. institutional power.').

omega_variable(
    impact_on_lay_practice,
    'How significantly does this reading impact the daily religious and legal practices of lay Muslims who may not be aware of or agree with its nuances?',
    'Ethnographic studies and surveys of Muslim communities in various regions, assessing their understanding and application of Quranic rulings, particularly those affected by this interpretive shift.',
    'A high impact on lay practice, especially if unacknowledged or resisted, would increase the perceived victimhood and extractiveness for the ''lay_practitioners_citing_permissive_texts'' seat, strengthening the Snare aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_lay_practice, empirical, 'Real-world impact of interpretive shifts on lay religious practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t1950, naskh_principle__progressive_restriction, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(nask_tr_t1970, naskh_principle__progressive_restriction, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(nask_tr_t1990, naskh_principle__progressive_restriction, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(nask_tr_t2010, naskh_principle__progressive_restriction, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(nask_tr_t2024, naskh_principle__progressive_restriction, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(nask_be_t1950, naskh_principle__progressive_restriction, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(nask_be_t1970, naskh_principle__progressive_restriction, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(nask_be_t1990, naskh_principle__progressive_restriction, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(nask_be_t2010, naskh_principle__progressive_restriction, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(nask_be_t2024, naskh_principle__progressive_restriction, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t1950, naskh_principle__progressive_restriction, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(nask_su_t1970, naskh_principle__progressive_restriction, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement(nask_su_t1990, naskh_principle__progressive_restriction, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(nask_su_t2010, naskh_principle__progressive_restriction, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(nask_su_t2024, naskh_principle__progressive_restriction, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
