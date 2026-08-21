% ============================================================================
% CONSTRAINT STORY: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, []).

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
 *   constraint_id: legitimacy_of_practice_standardization__dual_practice_equilibrium_reading
 *   human_readable: Dual Practice Legitimacy Equilibrium
 *   domain: political_history/institutional_change
 *
 * SUMMARY:
 *   This constraint describes a 'dual practice equilibrium' where legitimacy
 *   is partitioned: state authority governs public/administrative domains,
 *   while traditional authority governs private/ritual domains. This reading
 *   posits a stable, long-term coexistence without an expectation of
 *   convergence, where compliance is often strategic rather than fully
 *   internalized. It is one reading of the
 *   'legitimacy_of_practice_standardization' kernel, contrasting with
 *   readings that emphasize displacement or override.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.45).
domain_priors:suppression_score(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.55).
domain_priors:theater_ratio(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "Dual Practice Legitimacy Equilibrium").
narrative_ontology:topic_domain(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, "political_history/institutional_change").

domain_priors:requires_active_enforcement(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '6054a931-8fa6-461f-b242-3d6f0e30287a').
narrative_ontology:cs_kernel_codification('6054a931-8fa6-461f-b242-3d6f0e30287a', formalized).
narrative_ontology:cs_authority_grounding('6054a931-8fa6-461f-b242-3d6f0e30287a', distributed).
narrative_ontology:cs_reading_relation('6054a931-8fa6-461f-b242-3d6f0e30287a', legitimacy_of_practice_standardization__endogenous_displacement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6054a931-8fa6-461f-b242-3d6f0e30287a', legitimacy_of_practice_standardization__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('6054a931-8fa6-461f-b242-3d6f0e30287a', foundational, legitimacy_is_domain_specific).
narrative_ontology:cs_axiom_status(legitimacy_is_domain_specific, holdable).
narrative_ontology:cs_axiom_grounding('6054a931-8fa6-461f-b242-3d6f0e30287a', legitimacy_is_domain_specific, conventional).
narrative_ontology:cs_axiom('6054a931-8fa6-461f-b242-3d6f0e30287a', secondary, strategic_compliance_maintains_equilibrium).
narrative_ontology:cs_axiom_status(strategic_compliance_maintains_equilibrium, holdable).
narrative_ontology:cs_axiom_grounding('6054a931-8fa6-461f-b242-3d6f0e30287a', strategic_compliance_maintains_equilibrium, empirically_contingent).
narrative_ontology:cs_reference_frame('6054a931-8fa6-461f-b242-3d6f0e30287a', domain_partitioned_coexistence).
narrative_ontology:cs_drift_state('6054a931-8fa6-461f-b242-3d6f0e30287a', contemporary_globalization_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6054a931-8fa6-461f-b242-3d6f0e30287a', '').
narrative_ontology:cs_kernel_id(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, legitimacy_of_practice_standardization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_institutions).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities).
narrative_ontology:constraint_beneficiary(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_seeking_stability).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_reformers).
narrative_ontology:constraint_victim(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_purists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers and enforces public and administrative domains (e.g., taxation, education, law). Benefits from a stable, predictable system that avoids direct conflict with traditional practices, allowing for effective governance in its designated sphere.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, state_institutions, agenda_setter,
    institutional, generational, constrained, national).

% Maintains and enforces norms in private and ritual domains (e.g., family law, religious festivals, cultural customs). Benefits from the state's recognition of its sphere of influence, preserving cultural identity and social cohesion within its community.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_authorities, agenda_setter,
    organized, generational, constrained, local).

% Navigates both systems, strategically complying with state norms in public life and traditional norms in private. Benefits from the social order and predictability that the dual equilibrium provides, avoiding direct clashes of authority.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, citizens_seeking_stability, beneficiary,
    moderate, biographical, mobile, national).

% Advocates for a unified, state-centric system of legitimacy and practice. Bears the cost of maintaining dual systems, which they perceive as inefficient or hindering progress. Their efforts to unify are resisted by both state and traditional authorities who benefit from the equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, modernization_reformers, payer,
    organized, biographical, constrained, national).

% Advocates for a return to full traditional authority and practice, rejecting state encroachment. Bears the cost of state interference in traditional life and the perceived dilution of cultural purity. Their efforts to resist state norms are constrained by state power.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, traditional_purists, payer,
    organized, biographical, constrained, local).

% Analyzes the impact of dual practice legitimacy on development goals, human rights, and governance effectiveness. Their observations can influence policy recommendations but do not directly alter the equilibrium.
narrative_ontology:constraint_stakeholder(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, international_development_agencies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the coexistence of distinct legitimacy sources and practice standards within a single society by partitioning domains of authority, preventing direct conflict and allowing for functional specialization.
% TRANSFER_FUNCTION: Transfers social order and predictability to citizens by clearly delineating the spheres of state and traditional authority. It also transfers compliance (time, effort, cognitive load) from citizens to both state and traditional authorities, as they must navigate two distinct sets of norms.
% ABSENT_VOICES: Those who advocate for a single, unified system of legitimacy (either fully modern/state-centric or fully traditional) are structurally marginalized by this equilibrium, as their proposals would disrupt the established partition.
% DISAPPEARANCE_RATIONALE: If this dual equilibrium vanished overnight, the society would face immediate and widespread conflict over which authority system should govern which domain. This would lead to institutional collapse, social unrest, and a breakdown of predictable governance and cultural practices, as the foundational agreement on legitimacy partitioning would be gone.
% FOUNDING_PROBLEM: Societies undergoing modernization faced severe internal conflict between traditional norms and new state-imposed standards, threatening social cohesion, effective governance, and national unity.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists, anthropologists, and political scientists studying post-colonial and modernizing states widely document this historical problem and its ongoing manifestations, noting that the tension between traditional and modern authority often persists, making the dual equilibrium a continuous management challenge. Academic literature and historical records from independent scholars corroborate this.
narrative_ontology:disappearance_verdict(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).
:- end_tests(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it coordinates the coexistence of two distinct authority systems, preventing open conflict, but this coordination comes with asymmetric costs. State institutions and traditional authorities benefit from maintaining their respective spheres of influence and the overall stability. However, reformers and purists bear the costs of this compromise, as their desired unified systems are suppressed. Active enforcement is required by both state and traditional authorities to maintain the boundaries of their domains. Extractiveness is moderate, reflecting the ongoing costs of maintaining dual systems and the strategic nature of compliance, which implies some friction. Suppression is also moderate, as both authorities exert pressure within their domains, but neither fully collapses alternatives in the other's sphere. Theater ratio is low because both systems are genuinely functional in their designated domains.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state and traditional authorities, this equilibrium is a necessary and functional compromise that ensures social order. From the perspective of reformers and purists, it represents a persistent obstacle to their desired societal transformation, whether towards full modernization or full traditionalism. The engine's classification will reflect this divergence, showing benefits for those maintaining the equilibrium and extraction for those whose agendas are suppressed by it.
 *
 * DIRECTIONALITY LOGIC:
 *   State institutions and traditional authorities are beneficiaries and agenda-setters, as they define and enforce the boundaries of their respective domains and derive legitimacy from this arrangement. Citizens seeking stability are beneficiaries, gaining predictability. Modernization reformers and traditional purists are payers, as they bear the costs of the system's persistence, which prevents their preferred unified systems from emerging. International development agencies act as observers, analyzing the system without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equilibrium_stability_vs_drift,
    'Is this dual practice equilibrium truly stable, or is one authority (e.g., state) gradually displacing the other over a longer time horizon?',
    'Longitudinal ethnographic studies and institutional analysis tracking the expansion or contraction of each authority''s effective domain over several generations.',
    'If displacement is occurring, the constraint''s extractiveness and suppression might be higher for the losing authority, and its classification could drift towards a Snare or Tangled Rope for the displaced party. If truly stable, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equilibrium_stability_vs_drift, empirical, 'Assessing the long-term stability of the dual practice equilibrium.').

omega_variable(
    strategic_vs_internalized_compliance,
    'To what extent is compliance with the dual system purely strategic (avoiding penalties), versus internalized (seen as legitimate and natural by participants)?',
    'Sociological surveys and qualitative interviews exploring citizens'' attitudes, beliefs, and motivations for adhering to both state and traditional norms, particularly in ambiguous or contested domains.',
    'If compliance is largely internalized, the constraint''s effective suppression is lower than measured, and its coordination function is stronger, potentially shifting it closer to a Rope. If purely strategic, the current classification emphasizing extraction and suppression is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_internalized_compliance, empirical, 'Distinguishing strategic from internalized compliance in a dual legitimacy system.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this ''dual_practice_equilibrium_reading'' the most accurate framing of the ''legitimacy_of_practice_standardization'' kernel, or do sibling readings (endogenous_displacement, exogenous_override) better capture the underlying dynamics?',
    'Comparative historical analysis across multiple societies, evaluating which reading''s predictions (e.g., stable partition vs. convergence/override) are most consistently observed over time and across different contexts.',
    'If a sibling reading is more accurate, the entire structural analysis of this constraint would need re-evaluation, potentially leading to a different claimed_type, different beneficiaries/victims, and a different set of temporal dynamics (e.g., a Snare if exogenous override is dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity in the most appropriate reading of the legitimacy of practice standardization kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(legi_tr_t1960, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(legi_tr_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(legi_tr_t1980, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(legi_tr_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(legi_tr_t2000, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(legi_tr_t2010, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(legi_tr_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(legi_be_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(legi_be_t1960, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1960, 0.42).
narrative_ontology:measurement(legi_be_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1970, 0.43).
narrative_ontology:measurement(legi_be_t1980, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement(legi_be_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(legi_be_t2000, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(legi_be_t2010, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(legi_be_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t1950, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1950, 0.5).
narrative_ontology:measurement(legi_su_t1960, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1960, 0.52).
narrative_ontology:measurement(legi_su_t1970, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1970, 0.53).
narrative_ontology:measurement(legi_su_t1980, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1980, 0.54).
narrative_ontology:measurement(legi_su_t1990, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(legi_su_t2000, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(legi_su_t2010, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(legi_su_t2020, legitimacy_of_practice_standardization__dual_practice_equilibrium_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
