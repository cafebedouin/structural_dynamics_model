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
 *   This constraint story represents the 'bodily_autonomy_primary' reading of
 *   the 'mandate_legitimacy_scope' kernel. It asserts that any medical
 *   intervention, including vaccination, without informed consent,
 *   constitutes a violation of fundamental bodily integrity, regardless of
 *   any purported collective benefit. Under this reading, vaccine mandates
 *   are seen as a coercive mechanism that extracts individual rights and
 *   suppresses dissent, classifying the constraint as a Snare. The high
 *   extractiveness and suppression reflect the direct infringement on
 *   individual liberty and the severe consequences for non-compliance.
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
narrative_ontology:constraint_metric(mandate_legitimacy_scope__bodily_autonomy_primary, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(mandate_legitimacy_scope__bodily_autonomy_primary, "Bodily Autonomy as Primary in Medical Mandates").
narrative_ontology:topic_domain(mandate_legitimacy_scope__bodily_autonomy_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__bodily_autonomy_primary, '24dc4206-c7f5-47a9-911f-3d451cd112b6').
narrative_ontology:cs_kernel_codification('24dc4206-c7f5-47a9-911f-3d451cd112b6', formalized).
narrative_ontology:cs_authority_grounding('24dc4206-c7f5-47a9-911f-3d451cd112b6', extraction).
narrative_ontology:cs_interpretation_layer_present('24dc4206-c7f5-47a9-911f-3d451cd112b6').
narrative_ontology:cs_reading_relation('24dc4206-c7f5-47a9-911f-3d451cd112b6', mandate_legitimacy_scope__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('24dc4206-c7f5-47a9-911f-3d451cd112b6', mandate_legitimacy_scope__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('24dc4206-c7f5-47a9-911f-3d451cd112b6', foundational, bodily_integrity_is_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('24dc4206-c7f5-47a9-911f-3d451cd112b6', bodily_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('24dc4206-c7f5-47a9-911f-3d451cd112b6', foundational, informed_consent_is_non_negotiable).
narrative_ontology:cs_axiom_status(informed_consent_is_non_negotiable, holdable).
narrative_ontology:cs_axiom_grounding('24dc4206-c7f5-47a9-911f-3d451cd112b6', informed_consent_is_non_negotiable, deontological).
narrative_ontology:cs_reference_frame('24dc4206-c7f5-47a9-911f-3d451cd112b6', absolute_bodily_autonomy_framework).
narrative_ontology:cs_drift_state('24dc4206-c7f5-47a9-911f-3d451cd112b6', contemporary_pandemic_response_era, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('24dc4206-c7f5-47a9-911f-3d451cd112b6', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__bodily_autonomy_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who face loss of employment, education, or public services due to vaccine mandates, viewing these mandates as a direct violation of their bodily integrity and a coercive medical intervention without consent. Their options are compliance or severe social/economic exclusion.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, unvaccinated_coerced_individuals, payer,
    powerless, immediate, trapped, national).

% Groups and legal organizations that champion individual rights over collective benefit in medical decisions, actively resisting mandates through legal challenges, protests, and public discourse. They bear the costs of litigation and public opposition.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, medical_autonomy_advocates, payer,
    organized, generational, constrained, global).

% Government bodies responsible for public health policy, which, under this reading, are seen as violating fundamental rights by imposing mandates. They enforce the mandates and justify them on public health grounds, but are viewed as the source of rights violations.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, state_public_health_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% The abstract concept of societal well-being and disease prevention, which is often cited as the justification for mandates. Under this reading, it is not a legitimate beneficiary if it comes at the cost of individual bodily autonomy.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(mandate_legitimacy_scope__bodily_autonomy_primary, collective_public_health).

% Judicial bodies tasked with interpreting constitutional rights, including bodily autonomy, against state powers. They adjudicate challenges to mandates and their rulings can either uphold or strike down the constraint.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__bodily_autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading denies a legitimate coordination function for mandates that violate bodily autonomy, viewing them as coercive rather than coordinative.
% TRANSFER_FUNCTION: Transfers individual bodily control and decision-making power from individuals to the state, in exchange for a claimed (but, under this reading, illegitimate) collective health benefit.
% ABSENT_VOICES: Future generations who might inherit a precedent of diminished bodily autonomy, and individuals in other jurisdictions where such mandates are not enforced, would object to the erosion of fundamental rights.
% DISAPPEARANCE_RATIONALE: If the constraint (i.e., the state's power to mandate medical interventions without consent) disappeared, individuals would regain full bodily autonomy, mandates would cease, and public health strategies would need to fundamentally reorganize around voluntary participation and less coercive measures.
% FOUNDING_PROBLEM: The perceived problem mandates aim to solve is the spread of infectious disease and the protection of public health, particularly vulnerable populations.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities and many medical professionals attest the problem is live and mandates are necessary. However, medical autonomy advocates and legal scholars, from outside the benefiting parties, argue that the 'problem' is being used to justify an overreach of state power that violates foundational rights, and that less restrictive alternatives exist.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.9) because the constraint directly takes away a fundamental right (bodily autonomy) from individuals. Suppression is also high (0.85) due to the severe social and economic penalties for non-compliance with mandates (e.g., job loss, exclusion from public spaces). The theater ratio is low (0.1) because the constraint's function is direct coercion, not performative maintenance. Resistance is high (0.9) reflecting the strong opposition from individuals and advocacy groups who view these mandates as an unacceptable overreach.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state public health authorities, mandates are a necessary Rope or Scaffold for collective well-being. From the perspective of individuals whose bodily autonomy is prioritized, the same mandates are a Snare, directly violating fundamental rights. The engine's classification will highlight this divergence based on the declared structural relationships and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated-coerced individuals and medical autonomy advocates are the primary targets/victims, bearing the full cost of the constraint (high d). State public health authorities are the agenda-setters and enforcers, benefiting from the power to compel compliance (low d). Collective public health is an abstract beneficiary, but its 'benefit' is viewed as illegitimate if achieved through rights violations. Constitutional courts act as observers, evaluating the legality and ethical implications.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently views mandates as a form of extraction, rather than a coordination mechanism that has atrophied. The 'mandate' itself is the problem, not a degraded solution. The classification as a Snare prevents mislabeling it as a Rope or Scaffold, which would imply a legitimate coordination function or temporary support that this reading explicitly denies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_collective_benefit,
    'Is the collective benefit derived from mandates (e.g., reduced disease transmission) a legitimate justification for infringing on individual bodily autonomy?',
    'Conceptual analysis and ethical deliberation within constitutional law and public health ethics, potentially informed by empirical data on the magnitude of collective benefit vs. individual harm.',
    'If collective benefit is deemed to legitimately override bodily autonomy, the constraint might be reclassified towards a Tangled Rope or even a Rope (from the public health perspective); if not, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_collective_benefit, conceptual, 'The fundamental ethical conflict between individual rights and collective good in public health.').

omega_variable(
    alternatives_to_coercion,
    'Are there less restrictive, non-coercive alternatives to mandates that could achieve comparable public health outcomes?',
    'Empirical studies comparing public health outcomes in jurisdictions with and without mandates, and analysis of the effectiveness of voluntary public health campaigns.',
    'If effective, less restrictive alternatives exist, the justification for mandates weakens, reinforcing the Snare classification. If no effective alternatives exist, the ''necessity'' argument for mandates gains strength, potentially shifting the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternatives_to_coercion, empirical, 'Availability and efficacy of non-coercive public health interventions.').


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
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__bodily_autonomy_primary, base_extractiveness, 10, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__bodily_autonomy_primary, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__bodily_autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
