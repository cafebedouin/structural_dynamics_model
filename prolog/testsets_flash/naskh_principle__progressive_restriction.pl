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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__progressive_restriction
 *   human_readable: Naskh Principle: Progressive Restriction Reading
 *   domain: islamic_jurisprudence/quranic_hermeneutics/legal_theory
 *
 * SUMMARY:
 *   The 'progressive restriction' reading of the Naskh principle posits that
 *   Quranic revelation moved from more permissive to more restrictive
 *   rulings, representing a divine pedagogical process rather than outright
 *   abrogation of earlier texts. This interpretation holds that later, more
 *   restrictive verses clarify or complete earlier, more general ones,
 *   establishing the final legal intent. It benefits traditional and
 *   conservative jurists by providing a framework to prioritize later, often
 *   more restrictive, rulings, while disadvantaging liberal or contextualist
 *   interpreters who seek to apply earlier, more permissive texts to
 *   contemporary issues.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__progressive_restriction, 0.6).
domain_priors:suppression_score(naskh_principle__progressive_restriction, 0.7).
domain_priors:theater_ratio(naskh_principle__progressive_restriction, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, extractiveness, 0.6).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(naskh_principle__progressive_restriction, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__progressive_restriction, tangled_rope).
narrative_ontology:human_readable(naskh_principle__progressive_restriction, "Naskh Principle: Progressive Restriction Reading").
narrative_ontology:topic_domain(naskh_principle__progressive_restriction, "islamic_jurisprudence/quranic_hermeneutics/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__progressive_restriction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__progressive_restriction, '791a4d2b-273e-4099-9669-e887a5e04cd5').
narrative_ontology:cs_kernel_codification('791a4d2b-273e-4099-9669-e887a5e04cd5', formalized).
narrative_ontology:cs_authority_grounding('791a4d2b-273e-4099-9669-e887a5e04cd5', lineage).
narrative_ontology:cs_interpretation_layer_present('791a4d2b-273e-4099-9669-e887a5e04cd5').
narrative_ontology:cs_reading_relation('791a4d2b-273e-4099-9669-e887a5e04cd5', naskh_principle__classical_abrogation, influences).
narrative_ontology:cs_reading_relation('791a4d2b-273e-4099-9669-e887a5e04cd5', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_axiom('791a4d2b-273e-4099-9669-e887a5e04cd5', foundational, divine_pedagogy_in_revelation).
narrative_ontology:cs_axiom_status(divine_pedagogy_in_revelation, holdable).
narrative_ontology:cs_axiom_grounding('791a4d2b-273e-4099-9669-e887a5e04cd5', divine_pedagogy_in_revelation, theological).
narrative_ontology:cs_axiom('791a4d2b-273e-4099-9669-e887a5e04cd5', foundational, later_revelation_completes_earlier).
narrative_ontology:cs_axiom_status(later_revelation_completes_earlier, holdable).
narrative_ontology:cs_axiom_grounding('791a4d2b-273e-4099-9669-e887a5e04cd5', later_revelation_completes_earlier, theological).
narrative_ontology:cs_reference_frame('791a4d2b-273e-4099-9669-e887a5e04cd5', gradual_divine_guidance).
narrative_ontology:cs_drift_state('791a4d2b-273e-4099-9669-e887a5e04cd5', contemporary_reformist_challenges, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('791a4d2b-273e-4099-9669-e887a5e04cd5', '').
narrative_ontology:cs_kernel_id(naskh_principle__progressive_restriction, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, traditional_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, conservative_scholars).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, liberal_reformers).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, contextualist_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__progressive_restriction, broader_muslim_community).
narrative_ontology:constraint_victim(naskh_principle__progressive_restriction, broader_muslim_community).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, divine_pedagogy_doctrine).
narrative_ontology:constraint_vindicates(naskh_principle__progressive_restriction, gradualism_in_revelation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adhere to and propagate the 'progressive restriction' reading, which provides a clear methodology for legal rulings and reinforces their authority within established Islamic legal schools. Their professional identity is often tied to this interpretive framework.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, traditional_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Benefit from the clarity and perceived stability offered by this reading, which aligns with their preference for more restrictive interpretations and provides a strong basis for their fatwas and teachings. They gain legitimacy by upholding this established interpretive method.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, conservative_scholars, beneficiary,
    organized, biographical, constrained, global).

% Bear the cost of this reading as it systematically undermines their attempts to derive more flexible or inclusive legal rulings from earlier, more permissive Quranic texts. Their interpretive freedom is constrained, and their arguments often face institutional resistance.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, liberal_reformers, payer,
    moderate, generational, identity_locked, global).

% Find their methodology of harmonizing all verses within their specific historical and social contexts challenged by the 'progressive restriction' reading, which prioritizes later texts. This limits the scope and acceptance of their interpretive work.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, contextualist_interpreters, payer,
    moderate, biographical, constrained, global).

% Receives a consistent, albeit often more restrictive, body of legal rulings, which can provide clarity and stability in religious practice. However, they also bear the cost of reduced interpretive diversity and potentially less adaptable legal frameworks for modern challenges.
narrative_ontology:constraint_stakeholder(naskh_principle__progressive_restriction, broader_muslim_community, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__progressive_restriction, broader_muslim_community, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__progressive_restriction, traditional_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__progressive_restriction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent hermeneutical framework for resolving apparent contradictions or developments in Quranic legal rulings, ensuring a unified understanding of divine intent and legal application across different periods of revelation.
% TRANSFER_FUNCTION: Transfers interpretive authority and legal precedence from earlier, more permissive Quranic texts to later, more restrictive ones, effectively channeling legal outcomes towards a specific, often conservative, direction. It also transfers legitimacy to those who uphold this interpretive method.
% ABSENT_VOICES: Early Islamic scholars who held different views on naskh, or those who emphasized the perpetual validity of all Quranic verses without chronological supersession, are largely absent from the contemporary discourse dominated by this reading. Their arguments for broader interpretive flexibility are marginalized.
% DISAPPEARANCE_RATIONALE: If the 'progressive restriction' reading vanished, the entire edifice of Islamic legal theory would need to be re-evaluated. Apparent contradictions in the Quran would lack a dominant resolution mechanism, leading to a proliferation of interpretive approaches and potentially a more diverse, but also more fragmented, body of Islamic law. The authority of many traditional legal rulings would be challenged.
% FOUNDING_PROBLEM: The problem of apparent contradictions or chronological developments in Quranic verses, particularly in legal matters, which required a systematic method for deriving consistent and authoritative rulings.
% FOUNDING_PROBLEM_CORROBORATION: The problem of textual consistency in the Quran is universally acknowledged within Islamic scholarship. However, the 'progressive restriction' solution is contested. Traditional institutions and scholars attest it is a live problem requiring this solution, while liberal and contextualist scholars argue the problem is better addressed through other hermeneutical methods, corroborated by historical evidence of diverse early interpretations.
narrative_ontology:disappearance_verdict(naskh_principle__progressive_restriction, world_rearranges).
narrative_ontology:founding_problem_status(naskh_principle__progressive_restriction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__progressive_restriction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(naskh_principle__progressive_restriction, 'none', 1).

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
 *   The constraint is classified as a Tangled Rope because it offers a coordination function (a clear methodology for resolving apparent textual conflicts and establishing legal precedent) but also involves significant asymmetric extraction. The extraction arises from the effective suppression of alternative interpretations and the marginalization of scholars who prioritize earlier, more permissive texts. Active enforcement is required through scholarly consensus, fatwas, and institutional authority to maintain this interpretive hierarchy. The relatively low theater ratio reflects that the pedagogical justification is genuinely held by many adherents, even if its application leads to extractive outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Traditional jurists and conservative scholars perceive this principle as a necessary and divinely guided method for legal consistency and moral development (a Rope or even a Mountain). Liberal reformers and contextualist interpreters, however, experience it as a Snare, as it systematically restricts their ability to derive more inclusive or adaptable rulings from the Quranic text, effectively 'extracting' interpretive freedom and imposing a more rigid legal framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists and conservative scholars are beneficiaries (d near 0.0) as this reading solidifies their interpretive authority and provides a clear methodology for legal rulings. Liberal reformers and contextualist interpreters are victims (d near 1.0) because their interpretive approaches, which might emphasize earlier, more permissive verses, are systematically undermined or deemed less authoritative. The broader Muslim community is a mixed bag, with some benefiting from perceived clarity and stability, and others bearing the costs of restricted legal options.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the 'progressive restriction' reading as a pure Mountain (natural law) or a pure Rope (benign coordination). While it offers a coordination function for legal interpretation, its active enforcement and the identifiable victims of its application reveal its extractive nature. The 'divine pedagogy' narrative, while a genuine part of the theological justification, also serves as a cover for the power dynamics inherent in legal interpretation, where certain readings gain ascendancy and others are suppressed. It is not a Piton, as it is actively maintained and benefits specific groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_principle_kernel_reading,
    'Is this constraint a genuine reflection of divine intent, or a hermeneutical construct that serves to consolidate legal authority?',
    'Comparative textual analysis across early Islamic legal schools, and examination of the historical development of the ''progressive restriction'' interpretation.',
    'If a construct, the constraint''s extractiveness is higher, as it serves to suppress alternative interpretations and centralize interpretive power. If divine intent, the constraint is a legitimate ''mountain'' of legal theory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naskh_principle_kernel_reading, conceptual, 'This constraint is the ''progressive restriction'' reading of the ''naskh_principle'' kernel. Sibling readings include ''classical_abrogation'' and ''contextual_harmonization''.').

omega_variable(
    impact_on_earlier_texts,
    'To what extent does the ''progressive restriction'' reading invalidate or merely contextualize earlier, more permissive Quranic verses for contemporary application?',
    'Analysis of fatwas and legal rulings that explicitly address the application of earlier verses in light of later restrictions, particularly in areas like women''s rights or interfaith relations.',
    'If earlier texts are effectively invalidated for contemporary practice, the suppression of alternative legal arguments is higher. If they retain contextual validity, the suppression is lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_earlier_texts, empirical, 'The ''progressive restriction'' reading implies that earlier permissive texts are transitional, not permanent law. This impacts those who seek to apply these earlier texts in modern contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__progressive_restriction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__progressive_restriction, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nask_tr_t10, naskh_principle__progressive_restriction, theater_ratio, 10, 0.1).
narrative_ontology:measurement(nask_tr_t20, naskh_principle__progressive_restriction, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__progressive_restriction, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nask_be_t10, naskh_principle__progressive_restriction, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(nask_be_t20, naskh_principle__progressive_restriction, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__progressive_restriction, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(nask_su_t10, naskh_principle__progressive_restriction, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(nask_su_t20, naskh_principle__progressive_restriction, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__progressive_restriction, identity_coordination).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__classical_abrogation).
narrative_ontology:affects_constraint(naskh_principle__progressive_restriction, naskh_principle__contextual_harmonization).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'naskh_principle' kernel. Each reading represents a different structural claim about how Quranic verses relate to each other, leading to different classifications and stakeholder impacts. This 'progressive_restriction' reading emphasizes divine pedagogy and gradual legal development.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
