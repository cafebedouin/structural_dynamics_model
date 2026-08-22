% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Bodily Autonomy as Primary Constraint on Medical Coercion
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint asserts that medical intervention without consent is
 *   categorically impermissible, prioritizing individual bodily autonomy
 *   above any collective benefit. It is a reading of the
 *   'coercion_legitimacy_boundary' kernel, specifically the
 *   'bodily_autonomy_primary' interpretation. While framed as a 'rope' by its
 *   proponents, its operation imposes costs on vulnerable populations and
 *   public health efforts, leading to a moderate extractiveness score. The
 *   constraint's persistence relies on strong ethical and legal advocacy
 *   rather than active enforcement against individuals, but it actively
 *   suppresses alternative public health framings.
 *
 * KEY AGENTS:
 *   - individuals_seeking_autonomy: Primary beneficiary (moderate/mobile) — protected from coercion
 *   - medical_ethics_advocates: Agenda setter (organized/analytical) — defends the principle
 *   - immunocompromised_individuals: Primary payer (powerless/trapped) — bears increased health risk
 *   - public_health_authorities: Payer (institutional/constrained) — limited in policy options
 *   - proportionality_advocates: Excluded (organized/constrained) — their alternative framing is suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.2).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary Constraint on Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '8c0a19ca-fc4a-4319-a3ec-5403ba6865c8').
narrative_ontology:cs_kernel_codification('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', formalized).
narrative_ontology:cs_authority_grounding('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', lineage).
narrative_ontology:cs_interpretation_layer_present('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8').
narrative_ontology:cs_reading_relation('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', foundational, individual_bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(individual_bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', individual_bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', secondary, informed_consent_unwaivable).
narrative_ontology:cs_axiom_status(informed_consent_unwaivable, holdable).
narrative_ontology:cs_axiom_grounding('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', informed_consent_unwaivable, conventional).
narrative_ontology:cs_reference_frame('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', nuremberg_code_principles).
narrative_ontology:cs_drift_state('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', contemporary_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8c0a19ca-fc4a-4319-a3ec-5403ba6865c8', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_seeking_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_ethics_advocates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, informed_consent_doctrine).
narrative_ontology:constraint_vindicates(coercion_legitimacy_boundary__bodily_autonomy_primary, individual_rights_philosophy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the constraint by having their right to refuse medical intervention upheld, even when it might contribute to collective risk. They experience freedom from state-mandated procedures.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_seeking_autonomy, beneficiary,
    moderate, biographical, mobile, national).

% These groups actively promote and defend the principle of bodily autonomy and informed consent as foundational to medical practice and law. They shape legal and ethical discourse around medical interventions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_ethics_advocates, agenda_setter,
    organized, generational, analytical, global).

% These individuals bear the cost of this constraint by being exposed to higher risks of infectious disease from unvaccinated populations, as collective immunity is not prioritized over individual choice. Their health and safety are directly impacted.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, immediate, trapped, local).

% These authorities are constrained in their ability to implement broad public health measures, such as mandatory vaccinations, even during epidemics, due to the prioritization of individual autonomy. They face challenges in controlling disease spread.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, biographical, constrained, national).

% These advocates argue for a nuanced approach where coercion is permissible if proportional to the public health threat. They are excluded from the 'categorical impermissibility' framing of this reading.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_advocates, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear ethical and legal boundary for medical practice, ensuring that individual consent is paramount and fostering trust between patients and medical professionals.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from individuals who refuse intervention to vulnerable populations and public health systems, in exchange for upholding individual liberty.
% ABSENT_VOICES: Advocates for a 'public health primary' approach, who would argue that collective harm prevention should, in certain severe circumstances, override individual autonomy. They are absent from the foundational premise of this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, medical practice would fundamentally change, potentially allowing for compulsory interventions. Public health policy would shift dramatically towards collective good, and individual rights discourse would be severely diminished.
% FOUNDING_PROBLEM: The historical problem of involuntary medical experimentation and coercive state control over individual bodies, leading to abuses of power and violations of human dignity.
% FOUNDING_PROBLEM_CORROBORATION: Medical ethics boards, human rights organizations, and historical records of medical abuses corroborate the ongoing relevance of protecting individual bodily integrity against state or medical overreach. Legal scholars outside the immediate beneficiary group also attest to its foundational importance.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).
:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the societal cost of foregone collective health benefits and increased risk to vulnerable groups. Suppression (0.20) is low because it's not about coercing individuals, but rather suppressing alternative policy framings that would allow for coercion. Theater ratio (0.10) is low as the constraint is genuinely functional in protecting individual rights, not merely performative. Accessibility collapse (0.30) is moderate, as alternatives (e.g., public health mandates) are conceptually available but legally and ethically constrained. Resistance (0.15) is low, as the principle is widely accepted in many legal and ethical frameworks, though contested by public health advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals seeking autonomy and medical ethics advocates, this constraint is a fundamental protection (a rope). From the perspective of immunocompromised individuals and public health authorities, it imposes significant costs and limits their ability to ensure collective well-being (leaning towards a snare or tangled rope). The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals seeking autonomy and medical ethics advocates are clear beneficiaries, as the constraint directly upholds their core values and rights. Immunocompromised individuals are victims, as they bear the health risks of reduced collective immunity. Public health authorities are also victims, as their mandate to protect the population is constrained. Proportionality advocates are excluded, as their alternative framing is not considered within this constraint's categorical impermissibility.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (protecting individual autonomy from medical coercion) remains live and highly relevant, especially in light of historical abuses and ongoing debates about state power. There is no evidence of mandatrophy; its function is actively defended and its core problem is still present. The classification prevents mislabeling it as a snare by acknowledging its genuine coordination function for individual rights, even while recognizing its extractive effects on other groups.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold,
    'At what threshold of collective harm (e.g., disease severity, transmission rate) would the ''categorical impermissibility'' of this reading be challenged or overridden by a ''public health primary'' or ''proportionality'' reading?',
    'Legal precedent from supreme court rulings during severe pandemics, or international human rights jurisprudence on public health emergencies.',
    'If a clear threshold exists and is crossed, this reading''s categorical nature would be reclassified, potentially shifting it towards a ''tangled_rope'' or ''snare'' from the perspective of public health authorities, or even a ''mountain'' if the collective harm is deemed an irreducible limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold, conceptual, 'The point at which collective harm might challenge individual autonomy.').

omega_variable(
    victim_set_expansion,
    'Does the ''immunocompromised_individuals'' group fully capture the victim set, or are there other groups (e.g., healthcare workers, economic sectors) that bear significant, unacknowledged costs from this constraint?',
    'Comprehensive epidemiological and economic impact studies during a public health crisis, disaggregated by population group and sector.',
    'If the victim set is significantly larger or more diverse, the overall extractiveness of this constraint would be higher, potentially pushing its classification closer to a ''snare'' from a societal perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_set_expansion, empirical, 'Whether the full scope of those harmed by the constraint is captured.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1947, 0.05).
narrative_ontology:measurement(coer_tr_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(coer_tr_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(coer_tr_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(coer_tr_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2020, 0.12).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1947, 0.3).
narrative_ontology:measurement(coer_be_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(coer_be_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(coer_be_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2010, 0.42).
narrative_ontology:measurement(coer_be_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2020, 0.48).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1947, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1947, 0.1).
narrative_ontology:measurement(coer_su_t1970, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1970, 0.12).
narrative_ontology:measurement(coer_su_t1990, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(coer_su_t2010, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(coer_su_t2020, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2020, 0.25).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_mandate_legitimacy).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_research_ethics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
