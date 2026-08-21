% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Primary Coercion Legitimacy Boundary
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of the
 *   coercion legitimacy boundary, where the state's power to compel medical
 *   intervention is justified when collective harm-prevention outweighs
 *   individual autonomy. This reading prioritizes population-level health
 *   outcomes, leading to high extractiveness from individuals whose autonomy
 *   is overridden and high suppression to ensure compliance. The metrics
 *   reflect a robust, actively enforced system where individual resistance is
 *   met with significant state power. This is one reading of the
 *   'coercion_legitimacy_boundary' kernel, distinct from
 *   'bodily_autonomy_primary' and 'proportionality_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.78).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.85).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Primary Coercion Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, '9e0a44f3-a018-4851-ad3c-21741fa8d285').
narrative_ontology:cs_kernel_codification('9e0a44f3-a018-4851-ad3c-21741fa8d285', formalized).
narrative_ontology:cs_authority_grounding('9e0a44f3-a018-4851-ad3c-21741fa8d285', lineage).
narrative_ontology:cs_interpretation_layer_present('9e0a44f3-a018-4851-ad3c-21741fa8d285').
narrative_ontology:cs_reading_relation('9e0a44f3-a018-4851-ad3c-21741fa8d285', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('9e0a44f3-a018-4851-ad3c-21741fa8d285', coercion_legitimacy_boundary__proportionality_reading, influences).
narrative_ontology:cs_axiom('9e0a44f3-a018-4851-ad3c-21741fa8d285', foundational, collective_health_priority).
narrative_ontology:cs_axiom_status(collective_health_priority, holdable).
narrative_ontology:cs_axiom_grounding('9e0a44f3-a018-4851-ad3c-21741fa8d285', collective_health_priority, deontological).
narrative_ontology:cs_axiom('9e0a44f3-a018-4851-ad3c-21741fa8d285', foundational, state_duty_to_protect_population).
narrative_ontology:cs_axiom_status(state_duty_to_protect_population, holdable).
narrative_ontology:cs_axiom_grounding('9e0a44f3-a018-4851-ad3c-21741fa8d285', state_duty_to_protect_population, conventional).
narrative_ontology:cs_reference_frame('9e0a44f3-a018-4851-ad3c-21741fa8d285', historical_public_health_sovereignty).
narrative_ontology:cs_drift_state('9e0a44f3-a018-4851-ad3c-21741fa8d285', contemporary_rights_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('9e0a44f3-a018-4851-ad3c-21741fa8d285', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they interpret and enforce policies that prioritize collective well-being, including mandating interventions. They face political and legal challenges but are structurally empowered to act.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Cannot safely receive certain medical interventions or are at high risk from infectious diseases. They benefit directly from high population immunity and reduced transmission, as their individual autonomy is already constrained by their health status.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from reduced disease burden, protection of healthcare systems, and the ability to participate in social and economic life with lower risk. They generally support measures that ensure collective safety, even if it means some individual restrictions.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Are compelled to undergo medical interventions (e.g., vaccination) or face restrictions on their activities (e.g., employment, travel). They bear the direct cost of the intervention or the social/economic cost of non-compliance, experiencing a direct infringement on their bodily autonomy.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Face a direct conflict between their religious beliefs and state mandates for medical intervention. Their refusal is often deeply tied to their identity, making exit (compliance) a profound personal cost, leading to social and legal penalties.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_objectors, payer,
    moderate, biographical, identity_locked, national).

% Monitor and challenge state actions that infringe on individual rights, including bodily autonomy. They analyze the legal and ethical boundaries of public health mandates, often representing individuals who feel coerced.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_advocates, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to prevent the spread of infectious diseases and protect vulnerable populations, ensuring that individual choices do not disproportionately harm the community.
% TRANSFER_FUNCTION: Transfers the burden of collective health risk from the general population and vulnerable individuals to those who refuse medical interventions, by compelling compliance or restricting their activities.
% ABSENT_VOICES: Individuals who have suffered severe adverse reactions to mandated interventions, or those whose deeply held beliefs are systematically overridden, are often marginalized in policy debates, their experiences framed as outliers rather than valid objections.
% DISAPPEARANCE_RATIONALE: If the state's power to compel medical intervention for collective good vanished, public health crises would escalate, vulnerable populations would be at greater risk, and the social contract around collective responsibility for health would fundamentally shift, leading to widespread societal reorganization.
% FOUNDING_PROBLEM: The problem of preventing widespread disease and protecting vulnerable populations from infectious threats, where individual actions have collective consequences.
% FOUNDING_PROBLEM_CORROBORATION: Medical professionals, epidemiologists, and public health historians universally corroborate the ongoing nature of infectious disease threats and the historical necessity of collective action. While the specific balance with individual rights is contested, the underlying problem is not.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__public_health_primary, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because individuals are compelled to act against their will, incurring personal costs (e.g., medical risk, loss of liberty) for the collective good. Suppression is also high (0.85) as the state actively enforces mandates through legal penalties, social restrictions, and public messaging, with little theatricality (0.1) as the function is direct and coercive. Accessibility collapse is moderate (0.65) as some alternatives (e.g., remote work) may exist, but core social participation is often restricted. Resistance is high (0.7) due to strong individual and ideological opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and the general public, this constraint is a necessary 'tangled rope' that coordinates collective safety while imposing costs. From the perspective of unvaccinated individuals and religious objectors, it is a 'snare' that extracts their autonomy under the guise of coordination. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, immunocompromised individuals, and the general public are beneficiaries, as they gain from reduced disease transmission and protected healthcare capacity. Unvaccinated individuals and religious objectors are victims, bearing the direct costs of compelled intervention or exclusion. The 'public health primary' reading structurally positions these groups as targets for the greater good.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (collective harm prevention) is considered live and urgent by its proponents, preventing mandatrophy. The high extractiveness and suppression are seen as necessary costs of maintaining this mandate, not as signs of atrophy. The classification as a Tangled Rope acknowledges both the coordination function and the asymmetric extraction, preventing mislabeling as pure extraction (Snare) or pure coordination (Rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_necessity_threshold,
    'At what level of collective harm risk does individual autonomy become legitimately subordinate to public health mandates?',
    'Ethical consensus building among diverse stakeholders, informed by epidemiological data and constitutional jurisprudence, leading to a clear, publicly accepted threshold.',
    'A higher threshold would shift the constraint towards ''bodily_autonomy_primary'' or ''proportionality_reading'', reducing extractiveness from individuals. A lower threshold would reinforce the ''public_health_primary'' stance, potentially increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_necessity_threshold, preference, 'The ethical threshold for overriding individual autonomy for public health.').

omega_variable(
    efficacy_of_less_coercive_measures,
    'Are there less coercive public health interventions (e.g., education, incentives, voluntary measures) that could achieve comparable collective harm-prevention outcomes?',
    'Comparative effectiveness research across different jurisdictions and policy regimes, evaluating the impact of voluntary vs. mandated interventions on disease transmission and population immunity.',
    'If less coercive measures are found to be equally effective, the high suppression and extractiveness of this constraint would be deemed unnecessary, shifting its classification towards a ''snare'' due to unjustified coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(efficacy_of_less_coercive_measures, empirical, 'Whether coercive measures are truly necessary for public health goals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal penalties, social exclusion) or internalized (fear of social stigma, moral obligation to community)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-exclusion from social activities) after legal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient to legal challenges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t5, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(coer_tr_t10, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(coer_tr_t15, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 15, 0.1).
narrative_ontology:measurement(coer_tr_t20, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(coer_be_t5, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(coer_be_t10, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(coer_be_t15, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 15, 0.77).
narrative_ontology:measurement(coer_be_t20, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 20, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(coer_su_t5, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(coer_su_t10, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(coer_su_t15, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 15, 0.83).
narrative_ontology:measurement(coer_su_t20, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 20, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
