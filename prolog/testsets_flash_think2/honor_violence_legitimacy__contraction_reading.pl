% ============================================================================
% CONSTRAINT STORY: honor_violence_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_violence_legitimacy__contraction_reading, []).

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
 *   constraint_id: honor_violence_legitimacy__contraction_reading
 *   human_readable: Honor Redefined to Exclude Violence (Contraction Reading)
 *   domain: historical_sociology/legal_anthropology/commitment_systems
 *
 * SUMMARY:
 *   This constraint, the 'contraction_reading' of the
 *   honor_violence_legitimacy kernel, describes the historical process by
 *   which honor itself was redefined to exclude violence, rendering dueling
 *   'structurally unthinkable.' This reading emphasizes the internal
 *   conceptual shift in social norms rather than external costs as the
 *   primary driver of dueling's decline. The base properties reflect the
 *   state of the redefined honor code at the end of the interval, where it
 *   functions as a beneficial coordination mechanism, while the temporal
 *   measurements illustrate the transition from a more violent to a
 *   non-violent understanding of honor.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_violence_legitimacy__contraction_reading, 0.2).
domain_priors:suppression_score(honor_violence_legitimacy__contraction_reading, 0.3).
domain_priors:theater_ratio(honor_violence_legitimacy__contraction_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(honor_violence_legitimacy__contraction_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_violence_legitimacy__contraction_reading, rope).
narrative_ontology:human_readable(honor_violence_legitimacy__contraction_reading, "Honor Redefined to Exclude Violence (Contraction Reading)").
narrative_ontology:topic_domain(honor_violence_legitimacy__contraction_reading, "historical_sociology/legal_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_violence_legitimacy__contraction_reading, '0393eb90-1c0e-4cc0-a1aa-96363538c53d').
narrative_ontology:cs_kernel_codification('0393eb90-1c0e-4cc0-a1aa-96363538c53d', implicit).
narrative_ontology:cs_authority_grounding('0393eb90-1c0e-4cc0-a1aa-96363538c53d', practice).
narrative_ontology:cs_interpretation_layer_present('0393eb90-1c0e-4cc0-a1aa-96363538c53d').
narrative_ontology:cs_reading_relation('0393eb90-1c0e-4cc0-a1aa-96363538c53d', honor_violence_legitimacy__drop_reading, coexists_with).
narrative_ontology:cs_reading_relation('0393eb90-1c0e-4cc0-a1aa-96363538c53d', honor_violence_legitimacy__composite_reading, coexists_with).
narrative_ontology:cs_axiom('0393eb90-1c0e-4cc0-a1aa-96363538c53d', foundational, honor_excludes_violence).
narrative_ontology:cs_axiom_status(honor_excludes_violence, holdable).
narrative_ontology:cs_axiom_grounding('0393eb90-1c0e-4cc0-a1aa-96363538c53d', honor_excludes_violence, deontological).
narrative_ontology:cs_axiom('0393eb90-1c0e-4cc0-a1aa-96363538c53d', secondary, social_order_trumps_individual_redress).
narrative_ontology:cs_axiom_status(social_order_trumps_individual_redress, holdable).
narrative_ontology:cs_axiom_grounding('0393eb90-1c0e-4cc0-a1aa-96363538c53d', social_order_trumps_individual_redress, conventional).
narrative_ontology:cs_reference_frame('0393eb90-1c0e-4cc0-a1aa-96363538c53d', non_violent_honor_framework).
narrative_ontology:cs_drift_state('0393eb90-1c0e-4cc0-a1aa-96363538c53d', post_enlightenment_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('0393eb90-1c0e-4cc0-a1aa-96363538c53d', '').
narrative_ontology:cs_kernel_id(honor_violence_legitimacy__contraction_reading, honor_violence_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, society_at_large).
narrative_ontology:constraint_beneficiary(honor_violence_legitimacy__contraction_reading, legal_authorities).
narrative_ontology:constraint_victim(honor_violence_legitimacy__contraction_reading, traditional_honor_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the reduction of violence and increased social stability due to the redefinition of honor. Experiences a safer public sphere and more predictable social interactions.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, society_at_large, beneficiary,
    organized, generational, constrained, national).

% Their monopoly on legitimate violence is strengthened, and their authority is less challenged by private acts of redress. They benefit from a more orderly society and reduced legal burdens related to dueling.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, legal_authorities, beneficiary,
    institutional, generational, analytical, national).

% Bear the cost of social ostracization and legal penalties if they attempt to adhere to older, violent codes of honor. Their identity is tied to a concept of honor that is no longer socially legitimate, making exit from this identity difficult.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, traditional_honor_adherents, payer,
    powerless, biographical, identity_locked, local).

% Actively advocated for and articulated the redefinition of honor, shaping public discourse and influencing legal and social norms. They benefit from the success of their intellectual and moral projects.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, moral_philosophers_and_social_reformers, agenda_setter,
    powerful, generational, mobile, continental).

% Study the historical processes of honor redefinition and the decline of dueling, analyzing the social, legal, and philosophical shifts without direct participation in the constraint's operation.
narrative_ontology:constraint_stakeholder(honor_violence_legitimacy__contraction_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a non-violent framework for resolving honor disputes, preventing cycles of retaliatory violence and fostering a more stable social order.
% TRANSFER_FUNCTION: Transfers the right to violent self-redress from individuals to the state and social consensus, and transfers the social cost of violence into social stigma for those who violate the new norm.
% ABSENT_VOICES: Those who died in duels under the old code, or those who were marginalized for refusing to duel, are absent. Also, those who still believe in the old code but are now silenced by social consensus and legal frameworks.
% DISAPPEARANCE_RATIONALE: If the redefinition of honor vanished overnight, societies would likely revert to more violent forms of dispute resolution, or new, potentially unstable, honor codes would emerge, leading to significant social upheaval and a challenge to state authority.
% FOUNDING_PROBLEM: Cycles of violence and death stemming from honor disputes, undermining social stability, state authority, and individual safety.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal records, philosophical treatises from the Enlightenment era, and sociological analyses corroborate the problem of dueling as a significant social ill. While violence in disputes remains a problem, the specific problem of dueling for honor is largely resolved, as attested by legal historians and social scientists.
narrative_ontology:disappearance_verdict(honor_violence_legitimacy__contraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_violence_legitimacy__contraction_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_violence_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(honor_violence_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_violence_legitimacy__contraction_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_violence_legitimacy__contraction_reading_tests).
:- end_tests(honor_violence_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.2) is low because the redefined honor code is largely beneficial, promoting social stability. Suppression (0.3) is also low, as the norm is primarily maintained through internalized social stigma rather than overt coercion. Theater ratio is minimal (0.1) as the shift was a genuine conceptual and behavioral change. Accessibility collapse (0.9) is high because dueling became genuinely unthinkable for most, and resistance (0.1) is low, reflecting the widespread acceptance of the new norm. The claimed type is 'rope' because it functions as a beneficial coordination mechanism for society, even though it imposed costs on a marginalized group of 'traditional honor adherents' during its establishment.
 *
 * PERSPECTIVAL GAP:
 *   While the redefined honor code is largely seen as beneficial by society and legal authorities, those adhering to older traditions experienced a loss of a previously legitimate means of redress. The engine's per-seat classification would reflect this divergence, with beneficiaries experiencing a 'rope' and payers experiencing a 'snare' or 'tangled_rope' due to the loss of their traditional 'exit' (dueling).
 *
 * DIRECTIONALITY LOGIC:
 *   Society at large and legal authorities are beneficiaries, gaining from reduced violence and strengthened state authority. Traditional honor adherents are payers, losing the social legitimacy of their previous practices and facing social stigma. Moral philosophers and social reformers acted as agenda-setters, driving the conceptual shift. Analytical historians observe this process from an analytical distance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    conceptual_vs_external_causation,
    'Was the decline of dueling primarily due to the conceptual redefinition of honor (as this reading suggests) or due to external costs and state suppression (as the ''drop_reading'' suggests)?',
    'Comparative historical analysis across different societies with varying degrees of state capacity and philosophical movements, examining the timing and sequence of decline drivers.',
    'If external costs were primary, this constraint''s ''rope'' classification might be too benign, and the ''drop_reading'' would be more accurate. If conceptual redefinition was primary, this reading''s emphasis on internal shifts is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conceptual_vs_external_causation, empirical, 'Primary cause of dueling''s decline: conceptual shift vs. external costs.').

omega_variable(
    structural_unthinkability_depth,
    'How deeply ''unthinkable'' did dueling truly become? Does a latent potential for honor-related violence persist, merely expressed in new forms, as the ''composite_reading'' might imply?',
    'Sociological studies of contemporary honor cultures and violence, examining whether the underlying logic of honor-based redress has truly vanished or merely transformed.',
    'If latent potential for violence remains significant, the ''accessibility_collapse'' metric might be overstated, and the ''rope'' classification might miss a persistent extractive element in the ''composite_reading''s'' view.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(structural_unthinkability_depth, conceptual, 'Depth of dueling''s ''unthinkability'' and persistence of honor-related violence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_violence_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hono_tr_t1700, honor_violence_legitimacy__contraction_reading, theater_ratio, 1700, 0.1).
narrative_ontology:measurement(hono_tr_t1740, honor_violence_legitimacy__contraction_reading, theater_ratio, 1740, 0.1).
narrative_ontology:measurement(hono_tr_t1780, honor_violence_legitimacy__contraction_reading, theater_ratio, 1780, 0.1).
narrative_ontology:measurement(hono_tr_t1820, honor_violence_legitimacy__contraction_reading, theater_ratio, 1820, 0.1).
narrative_ontology:measurement(hono_tr_t1860, honor_violence_legitimacy__contraction_reading, theater_ratio, 1860, 0.1).
narrative_ontology:measurement(hono_tr_t1900, honor_violence_legitimacy__contraction_reading, theater_ratio, 1900, 0.1).

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1700, 0.6).
narrative_ontology:measurement(hono_be_t1740, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1740, 0.45).
narrative_ontology:measurement(hono_be_t1780, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1780, 0.35).
narrative_ontology:measurement(hono_be_t1820, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1820, 0.28).
narrative_ontology:measurement(hono_be_t1860, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1860, 0.23).
narrative_ontology:measurement(hono_be_t1900, honor_violence_legitimacy__contraction_reading, base_extractiveness, 1900, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1740, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1740, 0.55).
narrative_ontology:measurement(hono_su_t1780, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1780, 0.45).
narrative_ontology:measurement(hono_su_t1820, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1820, 0.38).
narrative_ontology:measurement(hono_su_t1860, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1860, 0.33).
narrative_ontology:measurement(hono_su_t1900, honor_violence_legitimacy__contraction_reading, suppression_requirement, 1900, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_violence_legitimacy__contraction_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'honor_violence_legitimacy' kernel, focusing on the conceptual redefinition of honor. Sibling readings ('drop_reading', 'composite_reading') offer alternative or combined explanations for dueling's decline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
