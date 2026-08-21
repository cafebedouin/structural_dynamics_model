% ============================================================================
% CONSTRAINT STORY: income_support_commitment__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__freedom_floor_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: income_support_commitment__freedom_floor_reading
 *   human_readable: Unconditional Income Support (Freedom Floor Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint story analyzes unconditional income support from the
 *   'freedom floor' reading, which posits it as a mechanism to enhance
 *   individual autonomy, dignity, and capacity to exit precarious labor or
 *   abusive situations. It is one reading of the broader
 *   'income_support_commitment' kernel, contrasting with views that emphasize
 *   dependency or targeting efficiency. From this perspective, the constraint
 *   primarily functions as a coordination mechanism to distribute resources
 *   universally, with minimal inherent extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__freedom_floor_reading, 0.15).
domain_priors:suppression_score(income_support_commitment__freedom_floor_reading, 0.1).
domain_priors:theater_ratio(income_support_commitment__freedom_floor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(income_support_commitment__freedom_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_commitment__freedom_floor_reading, "Unconditional Income Support (Freedom Floor Reading)").
narrative_ontology:topic_domain(income_support_commitment__freedom_floor_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__freedom_floor_reading, '20621353-4790-4e53-85ea-19e2fff42036').
narrative_ontology:cs_kernel_codification('20621353-4790-4e53-85ea-19e2fff42036', formalized).
narrative_ontology:cs_authority_grounding('20621353-4790-4e53-85ea-19e2fff42036', practice).
narrative_ontology:cs_interpretation_layer_present('20621353-4790-4e53-85ea-19e2fff42036').
narrative_ontology:cs_reading_relation('20621353-4790-4e53-85ea-19e2fff42036', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('20621353-4790-4e53-85ea-19e2fff42036', income_support_commitment__targeting_efficiency_reading, coexists_with).
narrative_ontology:cs_axiom('20621353-4790-4e53-85ea-19e2fff42036', foundational, income_as_human_right).
narrative_ontology:cs_axiom_status(income_as_human_right, holdable).
narrative_ontology:cs_axiom_grounding('20621353-4790-4e53-85ea-19e2fff42036', income_as_human_right, deontological).
narrative_ontology:cs_axiom('20621353-4790-4e53-85ea-19e2fff42036', foundational, autonomy_as_social_good).
narrative_ontology:cs_axiom_status(autonomy_as_social_good, holdable).
narrative_ontology:cs_axiom_grounding('20621353-4790-4e53-85ea-19e2fff42036', autonomy_as_social_good, deontological).
narrative_ontology:cs_reference_frame('20621353-4790-4e53-85ea-19e2fff42036', universal_social_dividend_framework).
narrative_ontology:cs_drift_state('20621353-4790-4e53-85ea-19e2fff42036', contemporary_policy_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('20621353-4790-4e53-85ea-19e2fff42036', '').
narrative_ontology:cs_kernel_id(income_support_commitment__freedom_floor_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, all_citizens).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, abuse_survivors).
narrative_ontology:constraint_beneficiary(income_support_commitment__freedom_floor_reading, artists_entrepreneurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, employers).
narrative_ontology:constraint_victim(income_support_commitment__freedom_floor_reading, taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives unconditional income, gaining a floor of economic security that enhances autonomy and bargaining power in the labor market and life choices.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, all_citizens, beneficiary,
    organized, biographical, mobile, national).

% Gains financial recognition and stability for unpaid care work, reducing economic dependency and allowing for greater choice in care arrangements.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, constrained, local).

% Gains leverage to refuse exploitative labor, improving working conditions and wages across the low-wage sector by increasing their exit capacity.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, immediate, constrained, local).

% Gains financial independence, providing a crucial means to exit abusive relationships or situations without economic coercion.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, abuse_survivors, beneficiary,
    powerless, immediate, trapped, local).

% Gains financial runway to pursue creative or innovative projects without immediate market pressure, fostering cultural and economic dynamism.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, artists_entrepreneurs, beneficiary,
    moderate, biographical, mobile, national).

% Contributes to the funding through taxes and faces increased labor costs and reduced supply for low-wage jobs, potentially driving innovation or automation.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, employers, payer,
    powerful, biographical, arbitrage, national).

% Contributes to the funding of the program through taxes, potentially benefiting from increased social stability, reduced crime, and economic activity.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Administers the universal income program, manages tax collection, ensures equitable distribution, and monitors its social and economic impacts.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, government_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Analyzes the policy's theoretical underpinnings and empirical outcomes regarding autonomy, dignity, and labor market effects, contributing to public discourse and policy refinement.
narrative_ontology:constraint_stakeholder(income_support_commitment__freedom_floor_reading, welfare_state_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__freedom_floor_reading, all_citizens).
narrative_ontology:fixing_cost_class(income_support_commitment__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal basic standard of living, ensuring social stability, individual agency, and a floor of economic security for all citizens, decoupling income from labor market participation.
% TRANSFER_FUNCTION: Moves direct financial support from the national tax base (all taxpayers) to all citizens, enabling greater individual autonomy and capacity to exit precarious or exploitative situations.
% ABSENT_VOICES: Advocates for strict means-testing or work requirements, who would argue against universality and for targeted support based on demonstrated need, fearing work disincentives or fiscal unsustainability.
% DISAPPEARANCE_RATIONALE: If unconditional income support vanished overnight, many individuals would lose their enhanced autonomy and exit capacity, reverting to precarious labor or dependency. The labor market dynamics would shift back towards employer dominance, and social safety nets would become more conditional and stigmatizing, leading to significant social and economic reorganization.
% FOUNDING_PROBLEM: Poverty, precarity, and lack of bargaining power for workers, leading to exploitation, limited individual freedom, and social instability.
% FOUNDING_PROBLEM_CORROBORATION: Social policy researchers, labor unions, and human rights organizations consistently document ongoing issues of poverty, precarity, and power imbalances in labor markets, corroborating the continued relevance of the founding problem from an external, non-beneficiary perspective.
narrative_ontology:disappearance_verdict(income_support_commitment__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(income_support_commitment__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__freedom_floor_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__freedom_floor_reading_tests).
:- end_tests(income_support_commitment__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is low (0.15) because, from this reading's perspective, the primary function is resource distribution for social benefit, not rent-seeking; any 'extraction' is a necessary cost of coordination (e.g., administrative overhead, tax collection). Suppression is low (0.10) because the policy aims to reduce existing forms of economic coercion and enhance individual freedom, rather than impose new ones. Theater ratio is very low (0.05) as the direct transfer of income is a functional, not performative, activity. Resistance is high (0.70) due to significant political and ideological opposition to the concept and its funding implications.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of this kernel, such as the 'dependency_trap_reading' or 'targeting_efficiency_reading', would likely compute higher extractiveness (e.g., from taxpayers or those who perceive themselves as 'net contributors') or identify different victims (e.g., those whose work ethic is 'eroded'). This story, however, adheres strictly to the 'freedom_floor_reading' where the policy is a net enabler.
 *
 * DIRECTIONALITY LOGIC:
 *   All citizens, particularly those in precarious positions (caregivers, precarious workers, abuse survivors, artists/entrepreneurs), are direct beneficiaries, experiencing enhanced autonomy and exit options (low d). Employers and taxpayers are payers, contributing to the system (higher d, but not 'victims' in this reading as the system is seen as a collective good). Government agencies are agenda-setters, managing the system. There are no 'victims' from this reading's perspective, as the universal nature of the support eliminates means-test stigma and the benefits are seen to outweigh the collective costs.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dependency_vs_autonomy_ambiguity,
    'Does unconditional income support primarily foster autonomy and dignity, or does it create a ''dependency trap'' that atrophies skills and increases state reliance?',
    'Longitudinal empirical studies tracking labor market participation, skill development, and reported well-being of recipients in various pilot programs over several generations.',
    'If dependency is dominant, the constraint''s effective suppression might be higher (internalized suppression), and its classification could shift towards a ''tangled_rope'' or even ''snare'' if the ''freedom'' narrative is found to be cover for a new form of control. If autonomy is confirmed, the ''rope'' classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dependency_vs_autonomy_ambiguity, empirical, 'Whether the policy''s primary effect is to enable freedom or foster dependency.').

omega_variable(
    funding_sustainability_ambiguity,
    'Can universal income support be funded sustainably at a level that genuinely enables autonomy without causing excessive taxation, inflation, or economic distortion?',
    'Macroeconomic modeling and real-world implementation data from large-scale pilot programs, assessing fiscal impact, inflation rates, and labor market adjustments.',
    'If unsustainable, the constraint''s long-term viability as a ''rope'' is challenged, potentially leading to a ''piton'' (if maintained theatrically despite fiscal collapse) or a ''snare'' (if maintained through coercive taxation). If sustainable, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_sustainability_ambiguity, empirical, 'The fiscal and economic sustainability of universal income support.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the reduction in suppression truly structural (external barriers removed) or does it merely shift the burden of economic precarity without fundamentally altering power dynamics?',
    'Post-implementation analysis of wage growth in low-wage sectors, unionization rates, and reported worker satisfaction/leverage. If these improve significantly, suppression is structurally reduced.',
    'If suppression is merely shifted or remains largely intact, the ''rope'' classification might be too optimistic, and the constraint could be re-evaluated as a ''tangled_rope'' if hidden extractions persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in the context of economic precarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__freedom_floor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__freedom_floor_reading, theater_ratio, 5, 0.05).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__freedom_floor_reading, theater_ratio, 10, 0.06).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__freedom_floor_reading, theater_ratio, 15, 0.06).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__freedom_floor_reading, theater_ratio, 20, 0.07).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__freedom_floor_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__freedom_floor_reading, base_extractiveness, 5, 0.16).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__freedom_floor_reading, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__freedom_floor_reading, base_extractiveness, 15, 0.17).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__freedom_floor_reading, base_extractiveness, 20, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__freedom_floor_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(inco_su_t5, income_support_commitment__freedom_floor_reading, suppression_requirement, 5, 0.1).
narrative_ontology:measurement(inco_su_t10, income_support_commitment__freedom_floor_reading, suppression_requirement, 10, 0.11).
narrative_ontology:measurement(inco_su_t15, income_support_commitment__freedom_floor_reading, suppression_requirement, 15, 0.11).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__freedom_floor_reading, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
