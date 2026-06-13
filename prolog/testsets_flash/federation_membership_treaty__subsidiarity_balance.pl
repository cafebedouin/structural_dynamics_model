% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'subsidiarity balance' reading of a
 *   federation's membership treaty, specifically concerning free movement. It
 *   posits that while free movement is a fundamental right, it is not
 *   absolute and can be legitimately constrained by member states' national
 *   interests, provided such constraints are proportionate and do not
 *   eliminate the right entirely. This reading seeks a middle ground between
 *   full integration and full national sovereignty, leading to a graduated
 *   constraint structure where both unrestricted mobility and blanket
 *   restrictions are moderately suppressed.
 *
 * KEY AGENTS:
 *   - mobile_citizens: Primary beneficiary (moderate/mobile) — benefits from mobility, but subject to proportionate national interests.
 *   - member_states_with_labor_shortages: Beneficiary (institutional/constrained) — benefits from inflow of labor, but must respect proportionality.
 *   - member_states_with_high_unemployment: Victim (institutional/constrained) — bears costs of potential labor market disruption, but can impose proportionate restrictions.
 *   - citizens_in_high_demand_sectors: Victim (moderate/constrained) — may face restrictions if their sector is deemed a national interest, but retains mobility rights.
 *   - federal_judiciary: Agenda setter (institutional/analytical) — interprets and enforces the proportionality principle, balancing competing interests.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.4).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.5).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance Reading").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '470f981f-0509-4537-86cb-c9c7f1cf0b65').
narrative_ontology:cs_kernel_codification('470f981f-0509-4537-86cb-c9c7f1cf0b65', formalized).
narrative_ontology:cs_authority_grounding('470f981f-0509-4537-86cb-c9c7f1cf0b65', lineage).
narrative_ontology:cs_interpretation_layer_present('470f981f-0509-4537-86cb-c9c7f1cf0b65').
narrative_ontology:cs_reading_relation('470f981f-0509-4537-86cb-c9c7f1cf0b65', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('470f981f-0509-4537-86cb-c9c7f1cf0b65', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('470f981f-0509-4537-86cb-c9c7f1cf0b65', foundational, proportionality_is_foundational).
narrative_ontology:cs_axiom_status(proportionality_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('470f981f-0509-4537-86cb-c9c7f1cf0b65', proportionality_is_foundational, conventional).
narrative_ontology:cs_axiom('470f981f-0509-4537-86cb-c9c7f1cf0b65', foundational, legitimate_national_interests_exist).
narrative_ontology:cs_axiom_status(legitimate_national_interests_exist, holdable).
narrative_ontology:cs_axiom_grounding('470f981f-0509-4537-86cb-c9c7f1cf0b65', legitimate_national_interests_exist, empirically_contingent).
narrative_ontology:cs_reference_frame('470f981f-0509-4537-86cb-c9c7f1cf0b65', balanced_federal_integration).
narrative_ontology:cs_drift_state('470f981f-0509-4537-86cb-c9c7f1cf0b65', contemporary_migration_crises, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('470f981f-0509-4537-86cb-c9c7f1cf0b65', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_states_with_labor_shortages).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_with_high_unemployment).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, citizens_in_high_demand_sectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who benefit from the right to live and work in any member state, subject to proportionate national restrictions. They experience the constraint as a framework that enables their mobility while occasionally imposing friction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% States that benefit from the inflow of labor to fill economic gaps. They are coordinated into accepting mobile citizens but must justify any restrictions on national interest grounds.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_labor_shortages, beneficiary,
    institutional, generational, constrained, national).

% States that bear the potential costs of increased competition in their labor markets or strain on social services. They can impose proportionate restrictions to protect national interests, but these are subject to federal oversight.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_high_unemployment, payer,
    institutional, generational, constrained, national).

% Citizens whose mobility might be restricted if their sector is deemed a 'legitimate national interest' by a member state. They bear the cost of reduced mobility but retain the overall right to move.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, citizens_in_high_demand_sectors, payer,
    moderate, biographical, constrained, continental).

% The ultimate arbiter of the proportionality principle, interpreting the treaty and balancing free movement rights against national interests. Their rulings shape the practical application of the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_judiciary, agenda_setter,
    institutional, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To enable the free movement of persons within the federation while allowing member states to protect legitimate national interests through proportionate measures, thereby balancing integration with national autonomy.
% TRANSFER_FUNCTION: It transfers the right to unrestricted mobility from individual citizens to the collective federal framework, which then grants mobility rights back, subject to a proportionality test. It also transfers some sovereign control over borders and labor markets from member states to the federal judiciary, in exchange for a framework that manages migration flows.
% ABSENT_VOICES: Advocates for completely unrestricted movement (who would argue against any national interest restrictions) and advocates for absolute national sovereignty over borders (who would argue against any federal oversight) are structurally marginalized by this balancing framework. They would object to the compromises inherent in the proportionality test.
% DISAPPEARANCE_RATIONALE: If this balancing constraint vanished, the federation would either default to absolute free movement (straining national welfare systems and labor markets) or to absolute national sovereignty (fragmenting the common market and restricting individual rights). The current equilibrium would collapse, forcing a fundamental re-negotiation of federal principles.
% FOUNDING_PROBLEM: The founding problem was how to create a common market and shared identity within a federation that respected the distinct national interests and sovereignty of its member states, particularly regarding the movement of people.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as evidenced by ongoing debates and legal challenges regarding migration policy, labor market impacts, and social welfare provisions across the federation. Federal institutions and independent academic analyses corroborate that the tension between integration and national interest is a persistent challenge, not a solved problem.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates free movement (benefiting mobile citizens and states with labor shortages) while simultaneously extracting from states seeking to protect specific national interests and from citizens whose mobility might be restricted. The extractiveness (0.4) and suppression (0.5) are moderate, reflecting the balancing act. The 'requires_active_enforcement' is true as the proportionality tests and judicial oversight are crucial for its operation. Theater ratio is low (0.15) as the balancing function is actively performed, not merely ceremonial.
 *
 * PERSPECTIVAL GAP:
 *   Mobile citizens experience this as a beneficial, albeit sometimes friction-filled, coordination mechanism. Member states, depending on their economic situation (labor shortages vs. high unemployment), experience it as either a net benefit or a cost. The federal judiciary, as the agenda setter, views it as a necessary and legitimate balancing act to maintain the federation's integrity.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile citizens are beneficiaries (d=0.0-0.2) as their right to move is largely protected. Member states with labor shortages are also beneficiaries (d=0.1-0.3) as they gain labor. Member states with high unemployment are victims (d=0.7-0.9) as they face pressure on their labor markets. Citizens in high-demand sectors are victims (d=0.6-0.8) if their mobility is restricted. The federal judiciary, while enforcing, aims for a balanced outcome, so its directionality is closer to symmetric (d=0.4-0.6) from an analytical perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction from national interests) or a pure Snare (ignoring the genuine coordination of free movement). The 'subsidiarity balance' reading explicitly acknowledges both aspects, making it a Tangled Rope. If the balance were to consistently tip towards one side without genuine justification, it would drift towards a Snare (if integrationist) or a Piton (if national interests became purely theatrical).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''subsidiarity balance'' reading of the federation membership treaty, or is it a disguised ''integration primary'' or ''sovereignty primary'' reading?',
    'Analysis of judicial rulings and legislative actions over time: consistent application of proportionality tests and balancing of interests would corroborate this reading; consistent prioritization of either integration or sovereignty would indicate a different underlying reading.',
    'If it''s a disguised ''integration primary'' reading, extractiveness for member states seeking to restrict mobility would be higher, and suppression of national interests would be more severe. If ''sovereignty primary'', extractiveness for mobile citizens would be higher, and suppression of mobility rights would be more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing this reading from sibling interpretations of the federation membership treaty.').

omega_variable(
    proportionality_test_objectivity,
    'Is the ''proportionality test'' applied to national interests genuinely objective, or is it subject to political capture or judicial activism?',
    'Empirical study of proportionality test outcomes across diverse cases and political contexts, assessing consistency and predictability of application.',
    'If the test is consistently objective, the constraint functions as a legitimate balancing mechanism. If captured, it becomes a tool for either integrationist or sovereignist agendas, increasing extraction for the losing party.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(proportionality_test_objectivity, empirical, 'Assessing the objectivity of the proportionality test in balancing free movement and national interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__subsidiarity_balance, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('subsidiarity_balance') of the 'federation_membership_treaty' kernel. It differs from 'integration_primary' (which prioritizes free movement) and 'sovereignty_primary' (which prioritizes national control) by seeking a proportionate balance. Each reading constitutes a distinct constraint with its own ε and classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
