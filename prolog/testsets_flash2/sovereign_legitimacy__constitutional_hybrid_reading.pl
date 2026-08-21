% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__constitutional_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__constitutional_hybrid_reading, []).

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
 *   constraint_id: sovereign_legitimacy__constitutional_hybrid_reading
 *   human_readable: Constitutional Hybrid Sovereignty (Dual-Sourced Legitimacy)
 *   domain: political_philosophy/constitutional_theory/legitimacy_studies
 *
 * SUMMARY:
 *   This constraint describes the constitutional hybrid reading of sovereign
 *   legitimacy, where authority is dual-sourced: inherited ceremonial power
 *   (e.g., a monarch) and delegated political power (e.g., elected
 *   parliament). Constitutional law mediates the boundary, preventing either
 *   source from becoming absolute. This reading is a compromise, aiming for
 *   stability by accommodating historical tradition and modern democratic
 *   principles. It is one reading of the 'sovereign_legitimacy' kernel,
 *   distinct from purely monarchical or republican readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__constitutional_hybrid_reading, 0.35).
domain_priors:suppression_score(sovereign_legitimacy__constitutional_hybrid_reading, 0.45).
domain_priors:theater_ratio(sovereign_legitimacy__constitutional_hybrid_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(sovereign_legitimacy__constitutional_hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__constitutional_hybrid_reading, rope).
narrative_ontology:human_readable(sovereign_legitimacy__constitutional_hybrid_reading, "Constitutional Hybrid Sovereignty (Dual-Sourced Legitimacy)").
narrative_ontology:topic_domain(sovereign_legitimacy__constitutional_hybrid_reading, "political_philosophy/constitutional_theory/legitimacy_studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__constitutional_hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__constitutional_hybrid_reading, 'bd6c020c-c19e-4b74-a692-d0892506ec01').
narrative_ontology:cs_kernel_codification('bd6c020c-c19e-4b74-a692-d0892506ec01', formalized).
narrative_ontology:cs_authority_grounding('bd6c020c-c19e-4b74-a692-d0892506ec01', lineage).
narrative_ontology:cs_interpretation_layer_present('bd6c020c-c19e-4b74-a692-d0892506ec01').
narrative_ontology:cs_reading_relation('bd6c020c-c19e-4b74-a692-d0892506ec01', sovereign_legitimacy__monarchical_reading, coexists_with).
narrative_ontology:cs_reading_relation('bd6c020c-c19e-4b74-a692-d0892506ec01', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_axiom('bd6c020c-c19e-4b74-a692-d0892506ec01', foundational, dual_source_legitimacy).
narrative_ontology:cs_axiom_status(dual_source_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('bd6c020c-c19e-4b74-a692-d0892506ec01', dual_source_legitimacy, conventional).
narrative_ontology:cs_axiom('bd6c020c-c19e-4b74-a692-d0892506ec01', foundational, constitutional_mediation_supremacy).
narrative_ontology:cs_axiom_status(constitutional_mediation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('bd6c020c-c19e-4b74-a692-d0892506ec01', constitutional_mediation_supremacy, conventional).
narrative_ontology:cs_reference_frame('bd6c020c-c19e-4b74-a692-d0892506ec01', post_enlightenment_constitutional_settlement).
narrative_ontology:cs_drift_state('bd6c020c-c19e-4b74-a692-d0892506ec01', contemporary_global_democracy_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('bd6c020c-c19e-4b74-a692-d0892506ec01', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__constitutional_hybrid_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists).
narrative_ontology:constraint_victim(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__constitutional_hybrid_reading, citizenry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains symbolic and ceremonial authority, status, and income, but is constitutionally constrained from exercising direct political power. Benefits from the stability of the hybrid system which preserves their role.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, hereditary_monarch, beneficiary,
    institutional, generational, identity_locked, national).

% Exercise delegated political authority, making policy and governing. Benefit from the legitimacy provided by both popular consent and the inherited symbolic continuity, but are constrained by constitutional limits and the monarch's ceremonial role.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, elected_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Benefit from a stable system that balances tradition with modern democratic principles, avoiding the extremes of absolutism or revolutionary upheaval. Their consent is the ultimate source of delegated power, but they are bound by the constitutional framework.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, citizenry, beneficiary,
    organized, generational, constrained, national).

% Mediates the boundary between inherited and delegated authority through constitutional interpretation and precedent. Their rulings define the practical limits of each source of power, ensuring the hybrid system's coherence.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, constitutional_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Seek a return to absolute monarchical rule, viewing delegated authority as illegitimate. They are constrained by the constitutional framework that limits the monarch's power and are victims of the compromise that dilutes their preferred form of sovereignty.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, absolutist_monarchists, payer,
    powerless, generational, identity_locked, national).

% Advocate for a purely republican system, viewing inherited authority as an anachronism. They are constrained by the continued existence of the monarchy and are victims of the compromise that retains a non-elected head of state.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__constitutional_hybrid_reading, pure_republicans, payer,
    powerless, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transfer of power and ensures political stability by blending inherited symbolic legitimacy with delegated democratic authority, preventing radical shifts and maintaining continuity across generations.
% TRANSFER_FUNCTION: Transfers symbolic legitimacy and historical continuity from the hereditary monarch to the state, while transferring policy-making power and accountability from the citizenry to elected officials, all mediated by constitutional law.
% ABSENT_VOICES: Those advocating for either pure absolutism or pure republicanism are marginalized in the constitutional discourse, their arguments for singular legitimacy sources being incompatible with the hybrid's foundational compromise. They would argue for a simpler, 'purer' form of sovereignty.
% DISAPPEARANCE_RATIONALE: If the constitutional hybrid vanished, the nation would face a profound legitimacy crisis, forcing a choice between a purely monarchical or purely republican system, likely leading to significant political instability, constitutional upheaval, and potentially civil unrest as the two historical sources of authority would clash directly.
% FOUNDING_PROBLEM: To resolve historical conflicts between monarchical tradition and emerging democratic demands, preventing civil war or revolution by creating a stable system that accommodates both sources of legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the historical problem of reconciling monarchical and democratic claims. Constitutional scholars and public opinion polls attest that the problem of balancing tradition with modern governance remains a live concern, even if the specific threats have evolved.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__constitutional_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__constitutional_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__constitutional_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sovereign_legitimacy__constitutional_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__constitutional_hybrid_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).
:- end_tests(sovereign_legitimacy__constitutional_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the costs of compromise for those seeking 'purer' forms of government, but lower than either pure absolutism or the potential instability of pure republicanism without historical continuity. Suppression (0.45) is also moderate, as the system actively suppresses challenges to its hybrid nature from both extremes. Theater ratio (0.20) is low, as the ceremonial role of the monarch, while symbolic, is a genuine component of the system's legitimacy, not mere performance. The slight increase in extractiveness and suppression towards the end of the interval reflects renewed debates about the relevance of inherited authority in modern democracies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (monarch, elected officials, citizenry), the hybrid system is a stable and legitimate compromise. From the perspective of the victims (absolutists, pure republicans), it is an illegitimate constraint that prevents the realization of a 'true' form of sovereignty. The engine's classification will reflect this divergence based on the declared structural relationships and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The hereditary monarch and elected officials are beneficiaries, gaining status and power respectively from the system's stability. The citizenry also benefits from stability and balanced governance. Absolutist monarchists and pure republicans are victims, as their preferred forms of government are suppressed by the hybrid compromise. The constitutional judiciary acts as an agenda-setter, defining the boundaries of power.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    boundary_dispute_resolution,
    'How effectively does constitutional law mediate boundary disputes between inherited and delegated authority, particularly during periods of political stress?',
    'Analysis of historical constitutional crises and their resolutions, focusing on whether the hybrid system adapted or fractured under pressure.',
    'If mediation is consistently effective, the constraint''s stability and coordination function are stronger. If it frequently leads to crises or requires extra-constitutional interventions, the extractiveness and suppression might be higher than measured, reflecting the costs of maintaining an inherently unstable compromise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(boundary_dispute_resolution, empirical, 'Effectiveness of constitutional mediation in boundary disputes.').

omega_variable(
    legitimacy_source_weighting,
    'What is the relative weight of inherited vs. delegated authority in the public''s perception of legitimacy, and how does this shift over time?',
    'Longitudinal public opinion surveys, content analysis of political discourse, and historical studies of national identity formation.',
    'If delegated authority''s weight significantly increases, the inherited component might become more theatrical or even a source of instability, pushing the constraint towards a Snare or Piton. If inherited authority retains significant weight, the Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_source_weighting, empirical, 'Shifting public perception of legitimacy sources.').

omega_variable(
    hybrid_vs_pure_stability,
    'Is the constitutional hybrid inherently more stable than either a pure monarchical or pure republican system, or does its compromise introduce unique vulnerabilities?',
    'Comparative political science studies across different regime types, analyzing long-term stability, adaptability, and resilience to internal and external shocks.',
    'If the hybrid proves less stable, its coordination function is weaker, and the costs borne by ''victims'' (absolutists, republicans) are less justified by a genuine collective benefit, potentially reclassifying it towards a Tangled Rope or Snare. If more stable, the Rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_vs_pure_stability, conceptual, 'Comparative stability of hybrid vs. pure systems.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__constitutional_hybrid_reading, 1789, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1789, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1789, 0.1).
narrative_ontology:measurement(sove_tr_t1850, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1850, 0.15).
narrative_ontology:measurement(sove_tr_t1900, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(sove_tr_t1950, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(sove_tr_t2000, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(sove_tr_t2024, sovereign_legitimacy__constitutional_hybrid_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(sove_be_t1789, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1789, 0.45).
narrative_ontology:measurement(sove_be_t1850, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1850, 0.4).
narrative_ontology:measurement(sove_be_t1900, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(sove_be_t1950, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 1950, 0.35).
narrative_ontology:measurement(sove_be_t2000, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2000, 0.33).
narrative_ontology:measurement(sove_be_t2024, sovereign_legitimacy__constitutional_hybrid_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1789, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1789, 0.6).
narrative_ontology:measurement(sove_su_t1850, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1850, 0.55).
narrative_ontology:measurement(sove_su_t1900, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(sove_su_t1950, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 1950, 0.45).
narrative_ontology:measurement(sove_su_t2000, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2000, 0.42).
narrative_ontology:measurement(sove_su_t2024, sovereign_legitimacy__constitutional_hybrid_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__constitutional_hybrid_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'constitutional_hybrid_reading' of the 'sovereign_legitimacy' kernel, which also includes 'monarchical_reading' and 'republican_reading' as sibling constraints. Each represents a distinct structural claim about the source of legitimate authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
