% ============================================================================
% CONSTRAINT STORY: substance_control_authority__legalization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_substance_control_authority__legalization_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: substance_control_authority__legalization_reading
 *   human_readable: State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)
 *   domain: public_health_policy/criminal_justice/political_economy
 *
 * SUMMARY:
 *   This constraint describes the state's authority to regulate drug markets
 *   as legal commerce, focusing on quality control, access restrictions, and
 *   taxation, as an alternative to prohibition. It aims to eliminate illegal
 *   markets and protect public health through regulation rather than
 *   criminalization. This is one reading of the broader
 *   'substance_control_authority' kernel, specifically the
 *   'legalization_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(substance_control_authority__legalization_reading, 0.3).
domain_priors:suppression_score(substance_control_authority__legalization_reading, 0.2).
domain_priors:theater_ratio(substance_control_authority__legalization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(substance_control_authority__legalization_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(substance_control_authority__legalization_reading, rope).
narrative_ontology:human_readable(substance_control_authority__legalization_reading, "State Authority to Regulate Drug Markets as Legal Commerce (Legalization Reading)").
narrative_ontology:topic_domain(substance_control_authority__legalization_reading, "public_health_policy/criminal_justice/political_economy").

domain_priors:requires_active_enforcement(substance_control_authority__legalization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(substance_control_authority__legalization_reading, '2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84').
narrative_ontology:cs_kernel_codification('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', formalized).
narrative_ontology:cs_authority_grounding('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', lineage).
narrative_ontology:cs_interpretation_layer_present('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84').
narrative_ontology:cs_reading_relation('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', substance_control_authority__prohibition_reading, forecloses).
narrative_ontology:cs_reading_relation('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', substance_control_authority__harm_reduction_reading, coexists_with).
narrative_ontology:cs_axiom('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', foundational, drug_markets_regulable_commerce).
narrative_ontology:cs_axiom_status(drug_markets_regulable_commerce, holdable).
narrative_ontology:cs_axiom_grounding('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', drug_markets_regulable_commerce, conventional).
narrative_ontology:cs_axiom('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', foundational, public_health_via_regulation).
narrative_ontology:cs_axiom_status(public_health_via_regulation, holdable).
narrative_ontology:cs_axiom_grounding('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', public_health_via_regulation, instrumental).
narrative_ontology:cs_reference_frame('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', liberal_regulatory_state).
narrative_ontology:cs_drift_state('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2b1de78a-4b1a-4b81-bfd6-ba4f6ddb2e84', '').
narrative_ontology:cs_kernel_id(substance_control_authority__legalization_reading, substance_control_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, state_regulatory_agencies).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, consumers).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, licensed_producers_distributors).
narrative_ontology:constraint_victim(substance_control_authority__legalization_reading, illegal_market_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(substance_control_authority__legalization_reading, criminal_justice_system).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establishes and enforces regulations for production, distribution, and sale of previously illicit substances. Benefits from increased tax revenue and reduced criminal justice costs. Bears the cost of developing and maintaining regulatory infrastructure.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, state_regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Gains access to regulated, quality-controlled substances, reducing health risks associated with adulterated products and avoiding criminal penalties for possession/use. Pays taxes on purchases.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, consumers, beneficiary,
    organized, biographical, mobile, local).

% Operates legally in a new market, generating profits and tax revenue. Subject to strict regulatory oversight and licensing fees. Benefits from market stability and reduced competition from illegal sources.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, licensed_producers_distributors, beneficiary,
    powerful, biographical, constrained, national).

% Loses market share and revenue as legal alternatives emerge. Faces continued law enforcement pressure for operating outside the legal framework. Their business model is directly undermined by legalization.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, illegal_market_actors, payer,
    powerless, immediate, trapped, local).

% Monitors public health outcomes (e.g., rates of use, addiction, overdose) following legalization. Advocates for evidence-based policies to mitigate potential harms and maximize public health benefits.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, public_health_advocates, observer,
    organized, generational, analytical, national).

% Experiences reduced caseloads related to drug offenses, allowing resources to be reallocated to other areas. Benefits from a decrease in drug-related crime and associated social disorder.
narrative_ontology:constraint_stakeholder(substance_control_authority__legalization_reading, criminal_justice_system, beneficiary,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the production, distribution, and consumption of previously illicit substances within a legal framework, ensuring product safety, controlling access, and generating tax revenue, while displacing illegal markets.
% TRANSFER_FUNCTION: Transfers tax revenue from consumers and licensed businesses to the state; transfers market share from illegal actors to legal ones; transfers regulatory oversight from criminal enforcement to public health and commerce agencies.
% ABSENT_VOICES: Those who believe all drug use is inherently immoral or socially destructive, regardless of legal status, are often marginalized in legalization debates, as are those who profit from the illegal market and would resist its dismantling.
% DISAPPEARANCE_RATIONALE: If state authority to regulate legal drug markets vanished, the market would immediately revert to either prohibition (if enforcement capacity remained) or an unregulated free-for-all, leading to chaos, health crises, and a resurgence of illegal markets. The entire legal and commercial infrastructure would collapse.
% FOUNDING_PROBLEM: The founding problem was the failure of drug prohibition to eliminate drug use, its creation of vast illegal markets, associated crime and violence, and the public health harms from unregulated substances.
% FOUNDING_PROBLEM_CORROBORATION: Law enforcement agencies, public health organizations, and economic analysts outside the direct beneficiaries corroborate the persistence of illegal markets and associated harms under prohibition, providing the impetus for legalization. The debate is over the best solution, not the existence of the problem.
narrative_ontology:disappearance_verdict(substance_control_authority__legalization_reading, world_rearranges).
narrative_ontology:founding_problem_status(substance_control_authority__legalization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(substance_control_authority__legalization_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(substance_control_authority__legalization_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(substance_control_authority__legalization_reading_tests).
:- end_tests(substance_control_authority__legalization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.3) reflecting the costs of regulation, licensing fees, and taxation, which are borne by producers and consumers but are generally accepted as the price of a legal, safe market. Suppression is low (0.2) as it primarily targets illegal market actors, not consumers or legal businesses, and aims to reduce the need for coercive enforcement against users. Theater ratio is low (0.1) as the regulatory functions are genuinely active and serve their stated purpose of market control and public safety.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state agencies and legal businesses, this is a functional Rope, solving a complex coordination problem with reasonable costs. From the perspective of illegal market actors, it is a Snare, actively suppressing their operations and extracting their market share. Public health advocates may view it as a Tangled Rope, balancing benefits of safety with potential harms of increased access.
 *
 * DIRECTIONALITY LOGIC:
 *   State regulatory agencies and licensed producers/distributors are clear beneficiaries, gaining revenue and legitimate market access. Consumers benefit from safer products and legal access. Illegal market actors are the primary victims, as their business is directly undermined. The criminal justice system also benefits from reduced caseloads. The overall directionality is towards a net benefit for most actors within the legal framework, with extraction concentrated on the displaced illegal market.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legalization_vs_prohibition_efficacy,
    'Does the legalization reading effectively eliminate illegal markets and reduce drug-related crime more than the prohibition reading?',
    'Comparative empirical studies of jurisdictions that have adopted legalization versus those maintaining prohibition, focusing on market size, crime rates, and public safety metrics.',
    'If legalization proves more effective, it strengthens the claim of a functional Rope; if not, it suggests a higher hidden cost or a failure to fully displace the Snare of the illegal market.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legalization_vs_prohibition_efficacy, empirical, 'Empirical comparison of market and crime outcomes under legalization vs. prohibition.').

omega_variable(
    legalization_vs_harm_reduction_scope,
    'To what extent does the legalization reading adequately address public health harms (e.g., addiction, increased use) compared to a dedicated harm reduction reading?',
    'Longitudinal public health data tracking rates of use, addiction, and treatment seeking in legalized jurisdictions, compared to those with strong harm reduction policies.',
    'If legalization leads to significant unmitigated harms, it suggests a need for stronger public health components, potentially shifting towards a Tangled Rope if regulatory costs become insufficient to address these issues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legalization_vs_harm_reduction_scope, empirical, 'Assessment of public health outcomes under legalization versus harm reduction.').

omega_variable(
    legalization_reading_vs_prohibition_reading_structural_difference,
    'Is the legalization reading fundamentally distinct from the prohibition reading, or merely a different set of regulatory tools applied to the same underlying problem?',
    'Analysis of the core axioms and goals: legalization aims to integrate into commerce, prohibition to eliminate. The structural difference lies in the treatment of the substance itself as a commodity versus a contraband.',
    'If the readings are structurally distinct, they represent different constraints. If they are merely variations, it suggests a single constraint with different policy settings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legalization_reading_vs_prohibition_reading_structural_difference, conceptual, 'Conceptual distinction between legalization and prohibition as structural constraints.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(substance_control_authority__legalization_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subs_tr_t1970, substance_control_authority__legalization_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(subs_tr_t1990, substance_control_authority__legalization_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(subs_tr_t2010, substance_control_authority__legalization_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(subs_tr_t2024, substance_control_authority__legalization_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(subs_be_t1970, substance_control_authority__legalization_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(subs_be_t1990, substance_control_authority__legalization_reading, base_extractiveness, 1990, 0.15).
narrative_ontology:measurement(subs_be_t2010, substance_control_authority__legalization_reading, base_extractiveness, 2010, 0.25).
narrative_ontology:measurement(subs_be_t2024, substance_control_authority__legalization_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(subs_su_t1970, substance_control_authority__legalization_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(subs_su_t1990, substance_control_authority__legalization_reading, suppression_requirement, 1990, 0.15).
narrative_ontology:measurement(subs_su_t2010, substance_control_authority__legalization_reading, suppression_requirement, 2010, 0.18).
narrative_ontology:measurement(subs_su_t2024, substance_control_authority__legalization_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
