% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__bodily_autonomy_primary, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: mandate_legitimacy_scope__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Medical Mandates
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'mandate_legitimacy_scope' kernel. It asserts that any medical
 *   intervention without informed consent, regardless of potential collective
 *   benefit, constitutes a violation of fundamental bodily integrity. When
 *   mandates are present, individuals coerced into compliance become victims,
 *   and the state is framed as a rights violator. This reading leads to a
 *   high extractiveness score for mandates, as they are seen as extracting
 *   fundamental rights from individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(mandate_legitimacy_scope__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(mandate_legitimacy_scope__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Mandates").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '85d23e29-d410-4d7c-b4f2-05e0f3a8890f').
narrative_ontology:cs_kernel_codification('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', formalized).
narrative_ontology:cs_authority_grounding('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', lineage).
narrative_ontology:cs_interpretation_layer_present('85d23e29-d410-4d7c-b4f2-05e0f3a8890f').
narrative_ontology:cs_reading_relation('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', foundational, informed_consent_unwaivable).
narrative_ontology:cs_axiom_status(informed_consent_unwaivable, holdable).
narrative_ontology:cs_axiom_grounding('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', informed_consent_unwaivable, deontological).
narrative_ontology:cs_reference_frame('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', individual_rights_supremacy).
narrative_ontology:cs_drift_state('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', contemporary_pandemic_response, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('85d23e29-d410-4d7c-b4f2-05e0f3a8890f', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, individual_rights_doctrine).
narrative_ontology:constraint_vindicates(mandate_legitimacy_scope__bodily_autonomy_primary, informed_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face loss of employment, education, or public services due to vaccine mandates, viewing these as direct violations of their bodily integrity and autonomy. Their options are compliance or severe social/economic exclusion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Groups and legal organizations that champion individual rights against state overreach in medical decisions. They benefit from the clarity of this reading in their advocacy, but also bear the cost of defending individuals against mandates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates, beneficiary,
    organized, generational, constrained, global).

% Government bodies responsible for public health, who, under this reading, are seen as violating fundamental rights when implementing mandates. They are the enforcers of the mandates that this reading deems illegitimate.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% The abstract concept of societal well-being and disease prevention. Under this reading, its benefits are considered secondary to individual rights and cannot justify coercive measures. It is excluded from the moral calculus of legitimate intervention.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health, excluded,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading does not acknowledge a legitimate coordination function for mandates that override bodily autonomy, viewing them as pure coercion.
% TRANSFER_FUNCTION: Transfers the burden of medical risk and decision-making from the state or collective to the individual, while transferring the 'benefit' of collective safety to the general population at the cost of individual rights.
% ABSENT_VOICES: The 'voice' of collective public health benefit is structurally absent from the primary moral consideration, as individual bodily autonomy is deemed paramount. Those who prioritize collective good over individual medical choice are effectively excluded from the foundational ethical framework.
% DISAPPEARANCE_RATIONALE: If this constraint (the principle of primary bodily autonomy) disappeared, the legal and ethical landscape for public health interventions would fundamentally shift, allowing for much broader state powers to compel medical procedures, leading to a rearrangement of individual rights and state authority.
% FOUNDING_PROBLEM: The historical problem of state or medical authority overriding individual bodily integrity, particularly in vulnerable populations or for eugenic purposes.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, human rights organizations, and historical records corroborate the ongoing relevance of protecting individual bodily autonomy against coercive state power, citing past abuses and contemporary challenges to medical freedom.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.9) because the constraint views any coerced medical intervention as a severe violation of a fundamental right, regardless of the perceived benefit. Suppression is high (0.85) because the state actively enforces mandates, leaving individuals with few viable alternatives to compliance. Resistance is also high (0.95) reflecting the strong opposition from those who hold this view. Theater ratio is low (0.1) as the constraint's operation is direct and not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state public health authorities, mandates are a necessary coordination mechanism for collective well-being. From the perspective of this reading, they are a snare that extracts fundamental rights. The engine will compute this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated-coerced individuals are direct victims (high d) as their bodily autonomy is directly targeted. Medical autonomy advocates are beneficiaries in the sense that their core principle is vindicated, but they also bear costs in defending it. State public health authorities are the agenda-setters and enforcers, acting as the source of the perceived rights violation. Collective public health is an 'excluded' non-agent, as its claims are subordinated.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_benefit_weighting,
    'To what extent, if any, can collective benefit legitimately override individual bodily autonomy in medical decisions?',
    'Philosophical and legal consensus on the hierarchy of rights, or a constitutional amendment explicitly defining the limits of state power in public health emergencies.',
    'If collective benefit is given any weight, the extractiveness of mandates would decrease, and the classification might shift towards a Tangled Rope or even Rope, depending on the degree of proportionality. If it remains zero, the Snare classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_benefit_weighting, preference, 'The irreducible uncertainty regarding the moral weighting of individual vs. collective good in public health.').

omega_variable(
    mandate_necessity_empirical,
    'Are vaccine mandates empirically necessary to achieve public health goals, or are less restrictive alternatives equally effective?',
    'Rigorous epidemiological studies comparing outcomes in jurisdictions with and without mandates, controlling for confounding factors.',
    'If mandates are shown to be empirically unnecessary, it would further strengthen the Snare classification by removing any instrumental justification. If they are shown to be uniquely effective, it would not change this reading''s classification (as it prioritizes autonomy regardless) but would highlight the tension with other readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_necessity_empirical, empirical, 'Empirical necessity of mandates vs. less restrictive alternatives.').

omega_variable(
    reading_structural_delta,
    'How would the structural classification of mandates change if a sibling reading (e.g., ''public_health_primary'') were adopted as the dominant framework?',
    'Analysis of legal precedents and policy outcomes under a framework prioritizing public health, observing shifts in victim/beneficiary sets and extractiveness.',
    'Under ''public_health_primary'', unvaccinated-coerced individuals might shift from ''victims'' to ''beneficiaries'' (of collective safety), and the state from ''rights violator'' to ''coordinator'', leading to a lower extractiveness and a Rope or Tangled Rope classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_structural_delta, conceptual, 'Impact of adopting a sibling reading on the structural classification of medical mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__bodily_autonomy_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 5, 0.9).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 10, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 5, 0.85).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel, focusing on individual bodily autonomy. Other readings (public_health_primary, proportionality_reading) offer alternative framings of mandate legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
