% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__exogenous_override_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__exogenous_override_reading
 *   human_readable: Exogenous Imposition Pathway for Commitment Displacement (Meiji Calendar/Dress)
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   This constraint describes the 'exogenous override' reading of commitment
 *   displacement, exemplified by the Meiji Restoration's top-down imposition
 *   of calendar and dress reforms. This reading posits that a strong state
 *   can directly displace existing commitments through coercive enforcement,
 *   without requiring a prior 'fringe adoption' phase. The reforms were not
 *   emergent from popular demand but were decreed and enforced, leading to
 *   rapid, widespread, but often reluctant, compliance. The M-set framework
 *   is incomplete without acknowledging this distinct pathway.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, 0.85).
domain_priors:suppression_score(imposition_pathway_kernel__exogenous_override_reading, 0.92).
domain_priors:theater_ratio(imposition_pathway_kernel__exogenous_override_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(imposition_pathway_kernel__exogenous_override_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__exogenous_override_reading, snare).
narrative_ontology:human_readable(imposition_pathway_kernel__exogenous_override_reading, "Exogenous Imposition Pathway for Commitment Displacement (Meiji Calendar/Dress)").
narrative_ontology:topic_domain(imposition_pathway_kernel__exogenous_override_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__exogenous_override_reading, '66a73e77-c3e3-434a-8086-f7bcbc611943').
narrative_ontology:cs_kernel_codification('66a73e77-c3e3-434a-8086-f7bcbc611943', formalized).
narrative_ontology:cs_authority_grounding('66a73e77-c3e3-434a-8086-f7bcbc611943', extraction).
narrative_ontology:cs_reading_relation('66a73e77-c3e3-434a-8086-f7bcbc611943', imposition_pathway_kernel__endogenous_climb_reading, forecloses).
narrative_ontology:cs_reading_relation('66a73e77-c3e3-434a-8086-f7bcbc611943', imposition_pathway_kernel__hybrid_cascade_reading, influences).
narrative_ontology:cs_axiom('66a73e77-c3e3-434a-8086-f7bcbc611943', foundational, state_capacity_enables_direct_override).
narrative_ontology:cs_axiom_status(state_capacity_enables_direct_override, holdable).
narrative_ontology:cs_axiom_grounding('66a73e77-c3e3-434a-8086-f7bcbc611943', state_capacity_enables_direct_override, empirically_contingent).
narrative_ontology:cs_axiom('66a73e77-c3e3-434a-8086-f7bcbc611943', foundational, fringe_adoption_not_prerequisite_for_displacement).
narrative_ontology:cs_axiom_status(fringe_adoption_not_prerequisite_for_displacement, holdable).
narrative_ontology:cs_axiom_grounding('66a73e77-c3e3-434a-8086-f7bcbc611943', fringe_adoption_not_prerequisite_for_displacement, empirically_contingent).
narrative_ontology:cs_reference_frame('66a73e77-c3e3-434a-8086-f7bcbc611943', strong_state_as_commitment_architect).
narrative_ontology:cs_drift_state('66a73e77-c3e3-434a-8086-f7bcbc611943', contemporary_historical_sociology, gap(stable, minor, true)).
narrative_ontology:cs_created_at('66a73e77-c3e3-434a-8086-f7bcbc611943', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__exogenous_override_reading, modernization_elites).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, traditional_social_groups).
narrative_ontology:constraint_victim(imposition_pathway_kernel__exogenous_override_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that decreed and enforced the calendar and dress reforms. It directly benefited from the symbolic and practical consolidation of state power and the projection of a modern image, which was seen as essential for national strength and international standing. It had full control over the implementation and suppression of dissent.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, meiji_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Intellectuals, bureaucrats, and industrialists who advocated for rapid Westernization and modernization. They gained social and political capital by aligning with the state's agenda and often served as local enforcers or exemplars of the new norms. Their benefits were primarily status and influence within the new order.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, modernization_elites, beneficiary,
    powerful, biographical, mobile, national).

% Rural populations, artisans, and conservative elements who adhered to traditional calendars, dress codes, and social customs. They bore the direct costs of forced compliance, social disruption, and the erosion of their cultural identity. Resistance was met with state coercion, and exit options were virtually nonexistent.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, traditional_social_groups, payer,
    powerless, biographical, trapped, local).

% Communities that had long-standing traditions tied to the old calendar (e.g., agricultural cycles, festivals) and dress codes. They experienced the disruption of their social fabric and economic practices. While some adapted, the imposition was a top-down override of their collective commitments, with compliance enforced by local authorities under state mandate.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__exogenous_override_reading, local_communities, payer,
    moderate, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To rapidly unify the nation under a single, modern temporal and symbolic framework, aligning Japan with international standards and projecting an image of strength and progress to both internal and external audiences.
% TRANSFER_FUNCTION: Transfers social and symbolic capital from traditional cultural practices to the modernizing state and its elites, by forcing compliance with new calendars and dress codes, thereby consolidating state authority and legitimacy.
% ABSENT_VOICES: Scholars and cultural preservationists who would argue for a more gradual, organic evolution of cultural norms, or for the value of maintaining traditional practices, were suppressed or marginalized by the state's modernization imperative. Their voices were not part of the policy-making process.
% DISAPPEARANCE_RATIONALE: If the Meiji state's capacity for top-down imposition had vanished, the calendar and dress reforms would not have taken hold as rapidly or universally. Traditional practices would have persisted much longer, and the process of cultural change would have been far more decentralized and contested, fundamentally altering the trajectory of Japanese modernization.
% FOUNDING_PROBLEM: The Meiji state perceived a need to rapidly modernize Japan to avoid Western colonization, requiring a unified national identity and a break from feudal traditions, including outdated calendars and dress that symbolized a 'backward' past.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists widely corroborate the Meiji state's perception of an existential threat and the strategic imperative for rapid modernization. While the specific reforms are historical, the underlying problem of national unity and international standing was genuinely live for the Meiji leadership, and the reforms were a direct response to it.
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__exogenous_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(imposition_pathway_kernel__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__exogenous_override_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the reforms imposed significant costs on traditional groups (cultural disruption, forced compliance) for the benefit of state consolidation and modernization elites. Suppression is very high, reflecting the direct coercive power of the Meiji state in enforcing these changes, with little tolerance for dissent. Theater ratio is low, as the state's actions were genuinely aimed at achieving its modernization goals, not merely performing. Resistance was present but largely ineffective against state power.
 *
 * PERSPECTIVAL GAP:
 *   From the state's perspective, these were necessary, efficient reforms for national survival and progress. From the perspective of traditional groups, it was a coercive imposition that eroded their way of life. This reading emphasizes the state's capacity to override existing commitments, a mechanism distinct from bottom-up emergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Meiji state apparatus and modernization elites were clear beneficiaries, gaining power, legitimacy, and status. Traditional social groups and local communities were the primary payers, bearing the costs of forced cultural change and social disruption. Their exit options were severely constrained or trapped due to the state's pervasive enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_evidence_for_fringe_absence,
    'Was there truly no meaningful fringe adoption of Western calendars or dress prior to the Meiji decrees, or were there unobserved micro-level adoptions that the state merely amplified?',
    'Detailed historical micro-studies of pre-Meiji social practices, diaries, and local records to detect any nascent adoption patterns that might have been overlooked by macro-historical accounts.',
    'If significant pre-decree fringe adoption is found, it would weaken the ''exogenous override'' claim, pushing the classification towards a ''hybrid cascade'' or even ''endogenous climb'' reading. If no such adoption is found, it strengthens the case for a distinct imposition pathway.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_evidence_for_fringe_absence, empirical, 'Assessing the true extent of pre-decree fringe adoption to validate the ''exogenous override'' premise.').

omega_variable(
    coercion_vs_legitimacy_shift,
    'To what extent was compliance driven purely by coercion, versus a rapid shift in perceived legitimacy of the state''s authority that made compliance seem inevitable or even desirable to some?',
    'Analysis of resistance patterns, defection rates, and post-enforcement social integration of the new norms. If compliance persisted without constant, overt coercion, it suggests a legitimacy shift; if it required continuous enforcement, it points to pure coercion.',
    'If legitimacy shift played a significant role, the ''suppression'' metric might be slightly overstated, and the constraint might have a subtle ''identity coordination'' component, even if initiated exogenously. If pure coercion, the ''snare'' classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_legitimacy_shift, conceptual, 'Distinguishing between coerced compliance and rapid legitimacy shifts in top-down impositions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__exogenous_override_reading, 1868, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t1868, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1868, 0.05).
narrative_ontology:measurement(impo_tr_t1875, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1875, 0.08).
narrative_ontology:measurement(impo_tr_t1882, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1882, 0.12).
narrative_ontology:measurement(impo_tr_t1890, imposition_pathway_kernel__exogenous_override_reading, theater_ratio, 1890, 0.1).

% Extraction over time
narrative_ontology:measurement(impo_be_t1868, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1868, 0.75).
narrative_ontology:measurement(impo_be_t1875, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1875, 0.82).
narrative_ontology:measurement(impo_be_t1882, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1882, 0.88).
narrative_ontology:measurement(impo_be_t1890, imposition_pathway_kernel__exogenous_override_reading, base_extractiveness, 1890, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t1868, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1868, 0.8).
narrative_ontology:measurement(impo_su_t1875, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1875, 0.9).
narrative_ontology:measurement(impo_su_t1882, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1882, 0.95).
narrative_ontology:measurement(impo_su_t1890, imposition_pathway_kernel__exogenous_override_reading, suppression_requirement, 1890, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__exogenous_override_reading, imposition_pathway_kernel__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'imposition_pathway_kernel', which explores how new commitments are established. This 'exogenous override' reading emphasizes top-down state capacity, contrasting with 'endogenous climb' and 'hybrid cascade' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
