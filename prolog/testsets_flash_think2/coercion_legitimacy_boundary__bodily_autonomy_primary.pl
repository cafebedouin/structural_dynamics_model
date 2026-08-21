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
 *   This constraint represents the 'bodily_autonomy_primary' reading of the
 *   'coercion_legitimacy_boundary' kernel. It asserts that medical
 *   intervention without individual consent is categorically impermissible,
 *   regardless of any potential collective benefit. This reading prioritizes
 *   individual liberty and bodily integrity as an absolute boundary against
 *   state or collective compulsion in healthcare. While it protects
 *   individual autonomy, its operation leads to a moderate level of
 *   extraction from vulnerable populations and public health efforts, as
 *   collective harm-prevention tools are restricted.
 *
 * KEY AGENTS:
 *   - individuals_asserting_autonomy: Primary beneficiary (moderate/mobile) — protected from intervention
 *   - civil_liberties_advocates: Secondary beneficiary (organized/analytical) — vindicates their principles
 *   - mandate_enforcers: Beneficiary (institutional/constrained) — relieved of enforcement burden (as per prompt instruction)
 *   - immunocompromised_individuals: Primary victim (powerless/trapped) — exposed to increased risk
 *   - public_health_authorities: Payer (institutional/constrained) — constrained in policy tools
 *   - healthcare_providers: Payer (organized/constrained) — manage consequences of restricted interventions
 *   - proportionality_advocates: Excluded (organized/analytical) — their nuanced position is foreclosed
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
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Bodily Autonomy as Primary Constraint on Medical Coercion").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, '2477410c-9112-4001-92a4-e917170aae7f').
narrative_ontology:cs_kernel_codification('2477410c-9112-4001-92a4-e917170aae7f', formalized).
narrative_ontology:cs_authority_grounding('2477410c-9112-4001-92a4-e917170aae7f', lineage).
narrative_ontology:cs_interpretation_layer_present('2477410c-9112-4001-92a4-e917170aae7f').
narrative_ontology:cs_reading_relation('2477410c-9112-4001-92a4-e917170aae7f', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('2477410c-9112-4001-92a4-e917170aae7f', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('2477410c-9112-4001-92a4-e917170aae7f', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('2477410c-9112-4001-92a4-e917170aae7f', bodily_integrity_absolute, deontological).
narrative_ontology:cs_axiom('2477410c-9112-4001-92a4-e917170aae7f', foundational, individual_rights_precede_collective_utility).
narrative_ontology:cs_axiom_status(individual_rights_precede_collective_utility, holdable).
narrative_ontology:cs_axiom_grounding('2477410c-9112-4001-92a4-e917170aae7f', individual_rights_precede_collective_utility, deontological).
narrative_ontology:cs_reference_frame('2477410c-9112-4001-92a4-e917170aae7f', liberal_rights_tradition).
narrative_ontology:cs_drift_state('2477410c-9112-4001-92a4-e917170aae7f', contemporary_public_health_crises, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2477410c-9112-4001-92a4-e917170aae7f', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These individuals benefit from the categorical impermissibility of non-consensual medical intervention, retaining full control over their bodies and medical decisions, even if it means foregoing collective benefits.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, individuals_asserting_autonomy, beneficiary,
    moderate, immediate, mobile, local).

% Advocacy groups championing individual rights and freedoms see this constraint as a fundamental protection against state overreach, reinforcing their core mission and legal precedents.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, civil_liberties_advocates, beneficiary,
    organized, generational, analytical, national).

% As per prompt instruction: these agents benefit from the clear legal boundary that prevents them from having to enforce potentially unpopular or legally challenged medical mandates, simplifying their operational scope and reducing political friction.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, mandate_enforcers, beneficiary,
    institutional, biographical, constrained, national).

% These individuals bear the cost of increased exposure to infectious diseases when others decline vaccination or other interventions, as their own health is directly impacted by the collective's choices. They have limited options for self-protection.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, local).

% These authorities are constrained in their ability to implement broad public health measures, such as mandatory vaccination or testing, which they believe are necessary for collective harm reduction. They must find alternative, less effective, strategies.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, payer,
    institutional, generational, constrained, national).

% Providers face increased risk of exposure and moral distress when they cannot recommend or enforce interventions they deem medically necessary for public safety, and must manage the consequences of preventable disease spread.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers, payer,
    organized, biographical, constrained, local).

% These advocates argue for a nuanced approach where coercion is permissible under specific, severe circumstances, but their position is foreclosed by the categorical nature of this reading.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, proportionality_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between individual liberty and state power in medical decisions, establishing a clear boundary that protects individual bodily autonomy from compelled intervention.
% TRANSFER_FUNCTION: Transfers the burden of managing collective health risks from the individual (via compelled intervention) to the collective (via less coercive measures) or to vulnerable individuals (via increased exposure). It also transfers the cost of potential public health crises to the healthcare system and society at large.
% ABSENT_VOICES: Those who prioritize collective survival or the protection of vulnerable populations above individual autonomy in all circumstances are structurally excluded from this categorical framing. Proportionality advocates are also excluded, as their nuanced position is not admitted by the absolute nature of this constraint.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, states could legally compel medical interventions, fundamentally altering the social contract regarding individual liberty and public health. This would lead to widespread resistance, legal challenges, or a dramatic shift in public compliance with state medical directives.
% FOUNDING_PROBLEM: Historical abuses of medical power, eugenics, forced sterilization, and state-mandated medical procedures without individual consent, which led to severe violations of human dignity and trust in medical institutions.
% FOUNDING_PROBLEM_CORROBORATION: International human rights law, medical ethics codes (e.g., Nuremberg Code, Helsinki Declaration), and patient advocacy groups widely corroborate the ongoing relevance and necessity of protecting bodily autonomy against potential state or medical overreach. Recent debates during pandemics also highlight its continued salience.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The `extractiveness` is set to moderate (0.45) as per the prompt's 'moderate ε from non-enforcement' instruction, reflecting the costs borne by vulnerable populations and public health efforts due to the categorical nature of this constraint. `Suppression` is low (0.20) because the constraint's primary function is to *prevent* suppression of individual autonomy, though it suppresses the options of public health authorities. `Theater_ratio` is low (0.10) as the principle is genuinely held and enforced, not performative. `Accessibility_collapse` is moderate (0.50) because while it preserves individual options, it collapses options for collective health management. `Resistance` is moderate-high (0.60) from those who bear the costs (immunocompromised, public health authorities) and advocate for alternative framings.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of individuals asserting autonomy and civil liberties advocates, this constraint is a fundamental protection, preventing extraction and upholding rights. However, from the perspective of immunocompromised individuals and public health authorities, the same constraint imposes significant costs and limits their ability to protect health, thus operating as an extractive force. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Individuals asserting autonomy and civil liberties advocates are clear beneficiaries, as the constraint directly protects their interests. Mandate enforcers are listed as beneficiaries per prompt instruction, interpreted as benefiting from clear legal boundaries and reduced political friction. Immunocompromised individuals are victims due to increased exposure risk. Public health authorities and healthcare providers are payers, as their tools and capacity to manage collective health are constrained by this categorical imperative.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (protecting bodily autonomy) is considered a live and fundamental ethical principle. The contest is not about its function atrophying, but about its scope and priority relative to other values like collective health. The 'moderate ε from non-enforcement' reflects the ongoing tension and costs of upholding this categorical boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_legitimacy_kernel_framing,
    'Is this constraint a foundational principle of individual liberty, or an impediment to effective collective health action?',
    'Conceptual analysis of ethical frameworks and legal precedents; societal consensus on the hierarchy of values during public health crises.',
    'If primarily an impediment, its classification might shift towards a Snare or Tangled Rope for the collective, rather than a protective Rope for the individual. If foundational, its protective classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coercion_legitimacy_kernel_framing, conceptual, 'Framing of bodily autonomy as absolute vs. contextual.').

omega_variable(
    extraction_attribution_ambiguity,
    'Is the ''extraction'' from immunocompromised individuals a direct consequence of this constraint''s categorical nature, or a secondary effect of policy choices made under this constraint (e.g., failure to implement alternative protections)?',
    'Empirical study of policy alternatives: if robust non-coercive protections for vulnerable groups are feasible and effective, the extraction is less directly attributable to the constraint itself.',
    'If directly attributable, the constraint''s extractive nature (Tangled Rope) is reinforced. If secondary, the constraint might be re-evaluated as a purer Rope for individual rights, with the extraction being a separate policy failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_attribution_ambiguity, empirical, 'Direct vs. indirect attribution of harm to the constraint.').

omega_variable(
    mandate_enforcer_beneficiary_rationale,
    'Does the ''mandate_enforcers'' stakeholder truly benefit from this constraint, or are they primarily constrained by it, with any ''benefit'' being a secondary relief from political pressure?',
    'Qualitative interviews with mandate enforcers regarding their preferences and perceived operational impacts of the constraint; analysis of institutional statements on legal clarity vs. policy effectiveness.',
    'If they are primarily constrained, their role would shift to ''payer'', increasing the overall perceived extraction of the constraint from institutional actors and potentially shifting its classification towards a Snare for public health institutions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_enforcer_beneficiary_rationale, empirical, 'Clarifying the true structural position of mandate enforcers.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 20, 0.1).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 30, 0.1).
narrative_ontology:measurement(coer_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.1).
narrative_ontology:measurement(coer_tr_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(coer_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.44).
narrative_ontology:measurement(coer_be_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 30, 0.2).
narrative_ontology:measurement(coer_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(coer_su_t50, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
