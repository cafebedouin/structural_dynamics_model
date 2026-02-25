% ============================================================================
% CONSTRAINT STORY: eu_safe_third_country_policy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_safe_third_country_policy, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_safe_third_country_policy
 *   human_readable: EU 'Safe Third Country' Asylum Policy
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The EU's Migration and Asylum Pact includes a 'Safe Third Country'
 *   provision, allowing member states to declare an asylum application
 *   inadmissible and deport the applicant to a designated non-EU country.
 *   This policy aims to manage migration flows and deter arrivals by
 *   externalizing asylum processing. The core conflict is between the EU's
 *   goal of state-level coordination and control, and the international legal
 *   obligations owed to individual asylum seekers, creating a structure with
 *   both a claimed coordination function and a severe extractive effect.
 *
 * KEY AGENTS:
 *   - Asylum Seekers: Primary victim (powerless/trapped) — Subject to deportation, their right to seek asylum is extracted.
 *   - EU Member States/Institutions: Primary beneficiary (institutional/arbitrage) — Benefit by reducing domestic asylum processing burdens and gaining political control over migration.
 *   - 'Safe Third Country' Governments: Secondary beneficiary (organized/mobile) — Receive financial and political benefits for accepting deportees.
 *   - Human Rights Organizations: Analytical observer (analytical/analytical) — Challenge the legality and morality of the policy, highlighting the extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_safe_third_country_policy, 0.68).
domain_priors:suppression_score(eu_safe_third_country_policy, 0.8).
domain_priors:theater_ratio(eu_safe_third_country_policy, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_safe_third_country_policy, extractiveness, 0.68).
narrative_ontology:constraint_metric(eu_safe_third_country_policy, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(eu_safe_third_country_policy, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_safe_third_country_policy, tangled_rope).
narrative_ontology:human_readable(eu_safe_third_country_policy, "EU 'Safe Third Country' Asylum Policy").
narrative_ontology:topic_domain(eu_safe_third_country_policy, "geopolitical/legal").

domain_priors:requires_active_enforcement(eu_safe_third_country_policy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_safe_third_country_policy, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_safe_third_country_policy, eu_border_agencies).
narrative_ontology:constraint_beneficiary(eu_safe_third_country_policy, third_country_governments).
narrative_ontology:constraint_victim(eu_safe_third_country_policy, asylum_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE ASYLUM SEEKER (SNARE) — From the perspective of an individual whose claim is deemed inadmissible and who faces deportation to a non-EU country, the policy is a pure trap. It extracts their fundamental right to have their asylum case heard where they lodged it, with no perceivable coordination benefit for them. High ε and suppression, combined with a trapped status, create a classic Snare. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.86.
constraint_indexing:constraint_classification(eu_safe_third_country_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE EU INSTITUTION (ROPE) — For the EU Council or a member state government, this policy is a coordination mechanism to manage migration flows, share the 'burden,' and create a unified external border policy. They operate from a position of arbitrage, negotiating which countries are 'safe' and controlling the enforcement apparatus. The extraction is externalized and thus invisible from this perspective. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.09.
constraint_indexing:constraint_classification(eu_safe_third_country_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE) — An observer like a human rights organization or legal scholar sees both sides. They recognize the stated goal of interstate coordination (the Rope function) while also documenting the severe, asymmetric extraction imposed on asylum seekers (the Snare function). The constraint requires active enforcement and has clear victims and beneficiaries, meeting the definition of a Tangled Rope.
constraint_indexing:constraint_classification(eu_safe_third_country_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE 'SAFE THIRD COUNTRY' (ROPE) — For a non-EU country government that partners with the EU, the arrangement is a transactional Rope. They receive financial aid, diplomatic legitimacy, or other concessions in exchange for accepting deportees. While they incur costs, their ability to negotiate the terms (mobile exit) frames it as a mutually beneficial coordination agreement from their institutional standpoint.
constraint_indexing:constraint_classification(eu_safe_third_country_policy, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_safe_third_country_policy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eu_safe_third_country_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eu_safe_third_country_policy, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eu_safe_third_country_policy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eu_safe_third_country_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high, representing the removal of a fundamental right to have an asylum claim heard. Suppression (0.80) is very high because an asylum seeker within the EU system has no alternative; they are subject to this process without consent or an ability to opt-out. Theater Ratio (0.60) is significant; the policy is framed in the language of order, safety, and legality, which critics argue masks a simple abdication of responsibility.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. EU institutions see a Rope, a technical solution for coordinating a complex problem across 27 member states. The asylum seeker experiences a Snare, a legal trap that extinguishes their hope and rights without recourse. The 'safe third country' also sees a Rope, but a transactional one. The analytical perspective of Tangled Rope is crucial because it acknowledges the reality of both the coordination claim and the extractive harm, refusing to accept either perspective as the complete story.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by the structural asymmetry. The beneficiary (EU) has arbitrage exit; they define the rules and partners, leading to a negative effective extraction (χ) and a Rope classification. The victim (asylum seeker) is trapped, with no exit, leading to a maximally high directionality ('d') value, which amplifies the base extractiveness into a clear Snare (χ > 0.66). The policy's structure itself determines these opposing classifications.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a primary example of preventing mandatrophy. A naive analysis might accept the EU's framing of the policy as a coordination mechanism (Rope). The Deferential Realism framework, by mandating indexing to the powerless/trapped perspective, reveals the underlying Snare. It correctly classifies the overall system as a Tangled Rope, a coordination structure that is inextricably linked to, and legitimized by, its function as an extractive mechanism against a vulnerable population.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_definition_integrity,
    'Is the designation of a ''safe third country'' based on objective, verifiable human rights standards, or is it a political designation of convenience?',
    'Independent, on-the-ground audits of judicial independence, human rights conditions, and due process for asylum seekers in designated third countries.',
    'If ''safety'' is a political fiction to enable deportations, the constraint''s Rope component is purely theatrical, making it a pure Snare. If safety standards are genuinely met, it remains a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(safety_definition_integrity, empirical, 'Whether the ''safe country'' designation is factual or political').

omega_variable(
    non_refoulement_compliance,
    'Does the policy of deporting to a third country, which may then deport the person further (chain refoulement), effectively violate the principle of non-refoulement under international law?',
    'A binding legal ruling by an international court (e.g., European Court of Human Rights) on a case arising from this policy.',
    'A ruling of non-compliance would legally classify the constraint as an illegal Snare. A ruling of compliance would uphold its status as a (contested) Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_refoulement_compliance, conceptual, 'Whether the policy violates the international law principle of non-refoulement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_safe_third_country_policy, 2020, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_s_tr_t2020, eu_safe_third_country_policy, theater_ratio, 2020, 0.45).
narrative_ontology:measurement(eu_s_tr_t2022, eu_safe_third_country_policy, theater_ratio, 2022, 0.55).
narrative_ontology:measurement(eu_s_tr_t2024, eu_safe_third_country_policy, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(eu_s_be_t2020, eu_safe_third_country_policy, base_extractiveness, 2020, 0.55).
narrative_ontology:measurement(eu_s_be_t2022, eu_safe_third_country_policy, base_extractiveness, 2022, 0.62).
narrative_ontology:measurement(eu_s_be_t2024, eu_safe_third_country_policy, base_extractiveness, 2024, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_safe_third_country_policy, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_safe_third_country_policy, uk_rwanda_asylum_plan).
narrative_ontology:affects_constraint(eu_safe_third_country_policy, schengen_area_border_controls).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
