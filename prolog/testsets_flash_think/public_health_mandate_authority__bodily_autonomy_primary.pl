% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__bodily_autonomy_primary, []).

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
 *   constraint_id: public_health_mandate_authority__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary in Public Health Mandates
 *   domain: public_health_law/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'bodily_autonomy_primary' reading
 *   of the 'public_health_mandate_authority' kernel. From this perspective,
 *   any public health mandate requiring non-consensual medical intervention
 *   is a categorical violation of individual bodily sovereignty. The
 *   constraint is classified as a Snare because its claimed coordination
 *   function (public health protection) is seen as cover for the extraction
 *   of individual autonomy, enforced through coercion. The metrics reflect
 *   this high level of perceived extraction and suppression.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, 0.9).
domain_priors:suppression_score(public_health_mandate_authority__bodily_autonomy_primary, 0.85).
domain_priors:theater_ratio(public_health_mandate_authority__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, extractiveness, 0.9).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__bodily_autonomy_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__bodily_autonomy_primary, snare).
narrative_ontology:human_readable(public_health_mandate_authority__bodily_autonomy_primary, "Bodily Autonomy as Primary in Public Health Mandates").
narrative_ontology:topic_domain(public_health_mandate_authority__bodily_autonomy_primary, "public_health_law/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__bodily_autonomy_primary, '3e20b9c9-1228-47bb-a574-d7fa3ac32db7').
narrative_ontology:cs_kernel_codification('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', formalized).
narrative_ontology:cs_authority_grounding('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', extraction).
narrative_ontology:cs_interpretation_layer_present('3e20b9c9-1228-47bb-a574-d7fa3ac32db7').
narrative_ontology:cs_reading_relation('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', public_health_mandate_authority__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', public_health_mandate_authority__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', foundational, bodily_autonomy_is_absolute).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', bodily_autonomy_is_absolute, deontological).
narrative_ontology:cs_axiom('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', foundational, collective_benefit_cannot_justify_individual_harm).
narrative_ontology:cs_axiom_status(collective_benefit_cannot_justify_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', collective_benefit_cannot_justify_individual_harm, deontological).
narrative_ontology:cs_reference_frame('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', absolute_bodily_sovereignty).
narrative_ontology:cs_drift_state('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', contemporary_mandate_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('3e20b9c9-1228-47bb-a574-d7fa3ac32db7', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__bodily_autonomy_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, healthcare_system).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_bodily_autonomy).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__bodily_autonomy_primary, advocates_for_bodily_autonomy).
narrative_ontology:constraint_victim(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are directly subjected to mandates, facing restrictions on employment, travel, or public participation if they do not comply. From this reading's perspective, they bear the direct harm of non-consensual medical intervention.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, unvaccinated_individuals, payer,
    powerless, immediate, trapped, national).

% Issue and enforce public health mandates, claiming to protect collective well-being. From this reading's perspective, they benefit from increased control and compliance, even if the justification is illegitimate.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_authorities, agenda_setter,
    institutional, generational, mobile, national).

% Adhere to the principle of absolute bodily sovereignty and view any mandate as a violation. Their resistance is often rooted in deeply held ethical or philosophical convictions, making compliance a profound personal cost.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, individuals_seeking_bodily_autonomy, payer,
    moderate, biographical, identity_locked, global).

% Are vulnerable to infectious diseases and rely on herd immunity for protection. In this reading, their need for protection does not justify infringing on the bodily autonomy of others, thus they are excluded from the victim set of the mandate itself, though they may suffer other harms.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, immunocompromised_individuals, excluded,
    powerless, immediate, trapped, local).

% Benefit from the principle of bodily autonomy being upheld, as it aligns with their core values and advocacy goals. They actively resist mandates and seek to dismantle the authority that imposes them.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, advocates_for_bodily_autonomy, beneficiary,
    organized, generational, mobile, global).

% Believe in the primacy of collective health and the necessity of mandates. From this reading's perspective, they bear the cost of the principle of bodily autonomy being prioritized, as it limits the tools available for public health protection.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, public_health_primary_advocates, payer,
    organized, generational, constrained, global).

% Benefits from reduced strain during public health crises due to higher compliance with mandates. This reading views this benefit as a consequence of the illegitimate extraction of individual autonomy.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__bodily_autonomy_primary, healthcare_system, beneficiary,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The mandate *claims* to coordinate collective action to mitigate public health threats and protect vulnerable populations, ensuring the stability of healthcare infrastructure.
% TRANSFER_FUNCTION: Transfers individual bodily sovereignty and decision-making power to public health authorities, in exchange for a *claimed* collective health benefit.
% ABSENT_VOICES: Those who believe in a strong social contract where individual liberties can be reasonably constrained for collective good, or those who are severely impacted by the absence of mandates (e.g., some immunocompromised individuals), are marginalized in this categorical framing.
% DISAPPEARANCE_RATIONALE: If public health mandate authority vanished overnight, public health policy would undergo a radical transformation, shifting entirely to voluntary measures. This would fundamentally alter the relationship between the state and individuals regarding health interventions, leading to a reorganization of public health strategies and potentially different disease outcomes.
% FOUNDING_PROBLEM: The perceived problem of widespread infectious disease threats that require collective action and intervention to protect the population and prevent healthcare system collapse.
% FOUNDING_PROBLEM_CORROBORATION: Advocates for bodily autonomy attest that the founding problem is one of state overreach and the erosion of fundamental rights. Public health bodies and public_health_primary_advocates attest that the problem of disease spread and healthcare system strain remains live, justifying mandates. The corroboration is split along ideological lines.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__bodily_autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(public_health_mandate_authority__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__bodily_autonomy_primary, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) and suppression (0.85) scores reflect the view that mandates impose a severe, non-consensual cost on individuals, with alternatives (non-compliance) being severely collapsed (0.8). Resistance is high (0.75) due to the fundamental nature of the perceived violation. The theater ratio is low (0.1) because the harm is direct and not primarily performative. The claimed type is Snare because the coordination story (public health) is considered a cover for the extraction of bodily autonomy.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, mandates are a necessary Rope or Scaffold for collective well-being. However, from the 'bodily_autonomy_primary' reading, the same structure is experienced as a Snare, extracting fundamental rights. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Unvaccinated individuals and those seeking bodily autonomy are full targets (payers/victims) of the constraint, bearing the direct costs of compliance or non-compliance. Public health authorities and the healthcare system are beneficiaries, gaining compliance and reduced system strain, even if this reading views their benefit as illegitimate. Advocates for bodily autonomy benefit from the principle being upheld, while public_health_primary_advocates bear the cost of its non-application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_legitimacy_framing,
    'Is the public health mandate a legitimate exercise of state power for collective good, or a categorical violation of individual rights?',
    'Resolution depends on the adopted foundational ethical framework (e.g., utilitarian vs. deontological ethics) and legal interpretation of constitutional rights.',
    'If framed as legitimate, the constraint would reclassify towards Rope or Scaffold; if framed as a categorical violation, it remains a Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_legitimacy_framing, conceptual, 'Fundamental disagreement on the ethical and legal legitimacy of public health mandates.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social exclusion) or internalized (fear of social stigma, perceived duty)?',
    'Post-mandate suppression trajectory: if suppression persists after legal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — individuals carry the suppression with them after formal enforcement ends.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for compliance with mandates.').

omega_variable(
    collective_benefit_quantification,
    'Can the collective benefit of public health mandates (e.g., lives saved, healthcare capacity preserved) be objectively quantified and compared against individual autonomy costs?',
    'Epidemiological modeling, economic analysis of health outcomes, and public health impact assessments. However, this reading would argue that even quantifiable benefits cannot justify a categorical violation.',
    'If collective benefits are demonstrably negligible, it strengthens the Snare classification. If benefits are substantial, it highlights the tension with the categorical bodily autonomy claim, but does not resolve the categorical violation argument for this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_benefit_quantification, empirical, 'Quantification of collective benefits vs. individual costs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__bodily_autonomy_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__bodily_autonomy_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 0, 0.85).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 5, 0.88).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__bodily_autonomy_primary, base_extractiveness, 10, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 5, 0.83).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__bodily_autonomy_primary, suppression_requirement, 10, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_health_mandate_authority__bodily_autonomy_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
