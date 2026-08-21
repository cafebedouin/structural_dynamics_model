% ============================================================================
% CONSTRAINT STORY: federation_membership__integration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__integration_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__integration_reading
 *   human_readable: Federation Membership as Irreversible Integration (Integration Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint story describes the 'integration reading' of federation
 *   membership, where membership implies irreversible integration,
 *   supranational authority is legitimate, and free movement is a
 *   constitutional right. This reading emphasizes the benefits of a unified
 *   market and shared governance, while acknowledging the costs borne by
 *   specific national and local actors. The claimed type is 'tangled_rope'
 *   because it genuinely coordinates (free movement, shared governance) but
 *   also involves significant, asymmetric extraction (labor displacement,
 *   loss of national border control).
 *
 * KEY AGENTS:
 *   - Supranational Institutions: Primary agenda-setter (institutional/arbitrage) — defines and enforces integration.
 *   - Mobile Citizens: Primary beneficiary (moderate/mobile) — gains from free movement.
 *   - Local Labor Markets: Primary victim (powerless/trapped) — bears costs of labor displacement.
 *   - National Border Authorities: Payer (institutional/constrained) — loses traditional control.
 *   - National Governments (Sovereignty Advocates): Excluded (powerful/constrained) — voices against integration are marginalized.
 *   - Analytical Observers: Observer (analytical/analytical) — studies the system's operation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__integration_reading, 0.78).
domain_priors:suppression_score(federation_membership__integration_reading, 0.7).
domain_priors:theater_ratio(federation_membership__integration_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership__integration_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership__integration_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__integration_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(federation_membership__integration_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__integration_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__integration_reading, "Federation Membership as Irreversible Integration (Integration Reading)").
narrative_ontology:topic_domain(federation_membership__integration_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__integration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__integration_reading, '96f1b9ea-7b81-43d9-803e-631147f9809f').
narrative_ontology:cs_kernel_codification('96f1b9ea-7b81-43d9-803e-631147f9809f', formalized).
narrative_ontology:cs_authority_grounding('96f1b9ea-7b81-43d9-803e-631147f9809f', lineage).
narrative_ontology:cs_interpretation_layer_present('96f1b9ea-7b81-43d9-803e-631147f9809f').
narrative_ontology:cs_reading_relation('96f1b9ea-7b81-43d9-803e-631147f9809f', federation_membership__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('96f1b9ea-7b81-43d9-803e-631147f9809f', foundational, supranational_law_supremacy).
narrative_ontology:cs_axiom_status(supranational_law_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('96f1b9ea-7b81-43d9-803e-631147f9809f', supranational_law_supremacy, conventional).
narrative_ontology:cs_axiom('96f1b9ea-7b81-43d9-803e-631147f9809f', foundational, free_movement_fundamental_right).
narrative_ontology:cs_axiom_status(free_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('96f1b9ea-7b81-43d9-803e-631147f9809f', free_movement_fundamental_right, deontological).
narrative_ontology:cs_reference_frame('96f1b9ea-7b81-43d9-803e-631147f9809f', ever_closer_union).
narrative_ontology:cs_drift_state('96f1b9ea-7b81-43d9-803e-631147f9809f', contemporary, gap(stable, minor, false)).
narrative_ontology:cs_created_at('96f1b9ea-7b81-43d9-803e-631147f9809f', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(federation_membership__integration_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:constraint_victim(federation_membership__integration_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__integration_reading, national_border_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions define and enforce the principles of irreversible integration, including free movement and the supremacy of supranational law. They benefit from expanded authority and a unified political-economic space.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, supranational_institutions, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Citizens who benefit from the constitutional right to free movement, allowing them to live, work, and study across member states without significant barriers. They gain economic and social opportunities.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, mobile_citizens, beneficiary,
    moderate, biographical, mobile, continental).

% Local economies and workers in regions experiencing significant influxes of mobile citizens. They bear the costs of increased competition for jobs, downward pressure on wages in some sectors, and strain on local public services, with limited ability to control these dynamics.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, local_labor_markets, payer,
    powerless, immediate, trapped, local).

% National agencies tasked with border control and immigration. Under the integration reading, their traditional authority to restrict movement is significantly curtailed, forcing them to adapt to supranational mandates and manage internal borders differently, often at increased cost or perceived loss of control.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_border_authorities, payer,
    institutional, biographical, constrained, national).

% Political factions and governments within member states that advocate for the retention or restoration of national sovereignty, particularly over borders and legal supremacy. They are structurally excluded from the core decision-making that defines the integration reading, despite representing significant domestic constituencies.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, national_governments_sovereignty_advocates, excluded,
    powerful, generational, constrained, national).

% Academics, policy analysts, and international organizations studying the long-term impacts of federal integration on governance, economics, and social cohesion. They provide independent analysis of the constraint's operation and its effects on various stakeholders.
narrative_ontology:constraint_stakeholder(federation_membership__integration_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__integration_reading, supranational_institutions).
narrative_ontology:fixing_cost_class(federation_membership__integration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To create a unified political and economic space, fostering peace, stability, and prosperity through the free movement of people, goods, services, and capital, and establishing a common legal framework.
% TRANSFER_FUNCTION: Transfers legislative and judicial authority from national to supranational institutions; transfers labor supply and demand across national borders; transfers a sense of collective identity from national to federal level.
% ABSENT_VOICES: Nationalist movements, protectionist labor unions, and local communities struggling with the social and economic impacts of rapid demographic shifts are often marginalized in the discourse that defines and legitimizes this reading of federation membership.
% DISAPPEARANCE_RATIONALE: If the principles of irreversible integration and supranational authority vanished, the entire federal structure would unravel. Internal borders would re-emerge, free movement would cease, and the common legal framework would collapse, leading to profound political, economic, and social reorganization.
% FOUNDING_PROBLEM: To prevent devastating inter-state conflicts, foster economic interdependence, and establish a shared commitment to democratic values and human rights in a post-war context.
% FOUNDING_PROBLEM_CORROBORATION: Supranational institutions and pro-integration academics assert that the founding problems of peace and prosperity remain live, requiring continued integration. Critics, including national governments and sovereignty advocates, argue that while peace is secured, new problems (e.g., democratic deficit, economic disparities) have emerged, making the original solution less relevant or even counterproductive. Independent historical and political analyses offer mixed corroboration, often highlighting the shift in the nature of challenges over time.
narrative_ontology:disappearance_verdict(federation_membership__integration_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__integration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__integration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership__integration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__integration_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__integration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__integration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__integration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.78) because the benefits of integration (e.g., for mobile citizens, supranational institutions) come at a significant cost to specific groups like local labor markets and national authorities, who experience reduced autonomy and economic pressure. Suppression (0.70) is substantial because national-level attempts to reassert border control or challenge supranational legal supremacy are actively resisted and deemed illegitimate by the integrationist framework. Theater ratio is low (0.15) as the integrationist project is actively pursued and enforced, with little performative maintenance of atrophied functions. Accessibility collapse (0.65) is moderate, as national alternatives to integration are suppressed but not entirely eliminated from political discourse. Resistance (0.55) is also moderate, reflecting ongoing political contestation from national-level actors.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of supranational institutions and mobile citizens, the constraint is a highly beneficial coordination mechanism. However, from the perspective of local labor markets and national border authorities, it functions as a highly extractive force, imposing costs and limiting autonomy. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a Rope-like structure and victims experiencing a Snare-like structure, despite the overall Tangled Rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Supranational institutions are clear beneficiaries, gaining authority and legitimacy from the integration process. Mobile citizens are also beneficiaries, directly leveraging the right to free movement. Local labor markets are victims, bearing the economic and social costs of increased competition and demographic shifts. National border authorities are also victims, as their traditional functions are curtailed and their mandates redefined by supranational law. National governments advocating for sovereignty are 'excluded' as their perspective is outside the legitimate frame of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (peace, prosperity through integration) is still considered live by its beneficiaries, preventing a clear mandatrophy resolution. However, the high and increasing extractiveness, coupled with significant resistance, suggests that while the original coordination function persists, it has become heavily layered with asymmetric extraction, preventing it from being a pure Rope. The 'contested' status of the founding problem further highlights this tension.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent structural feature, or is its classification entirely dependent on the ''integration_reading'' of the ''federation_membership'' kernel?',
    'Compare classification with the ''sovereignty_reading'' of the same kernel. If classifications diverge significantly, it confirms the reading-dependence of the constraint''s identity.',
    'If reading-dependent, the constraint''s effective classification is conditional on the dominant interpretive frame, highlighting the fragility of its perceived ''naturalness'' or ''inevitability''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'The constraint''s identity as a reading of a contested kernel.').

omega_variable(
    labor_market_impact_quantification,
    'What is the precise, disaggregated economic impact of free movement on local labor markets, distinguishing between different sectors and skill levels?',
    'Detailed econometric studies, longitudinal data analysis, and comparative case studies across diverse local economies within the federation.',
    'More precise quantification of labor market impacts would refine the ''extractiveness'' metric for local labor markets and could inform policy adjustments to mitigate negative effects, potentially shifting the constraint closer to a Rope for these stakeholders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_impact_quantification, empirical, 'Quantifying the economic costs borne by local labor markets due to free movement.').

omega_variable(
    sovereignty_transfer_irreversibility,
    'Is the transfer of sovereignty to supranational institutions truly irreversible, or can it be reclaimed by national entities under specific conditions?',
    'Analysis of constitutional law, treaty interpretation, and historical precedents regarding withdrawal or reassertion of national powers within federal structures.',
    'If reversibility is structurally possible, the ''suppression'' metric for national border authorities might be lower, and their ''exit_options'' might be less ''constrained'', potentially shifting their seat classification away from Snare-like characteristics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_transfer_irreversibility, conceptual, 'The structural irreversibility of sovereignty transfer in the integration reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__integration_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1950, federation_membership__integration_reading, theater_ratio, 1950, 0.05).
narrative_ontology:measurement(fede_tr_t1965, federation_membership__integration_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(fede_tr_t1980, federation_membership__integration_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(fede_tr_t1995, federation_membership__integration_reading, theater_ratio, 1995, 0.12).
narrative_ontology:measurement(fede_tr_t2010, federation_membership__integration_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fede_tr_t2025, federation_membership__integration_reading, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1950, federation_membership__integration_reading, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(fede_be_t1965, federation_membership__integration_reading, base_extractiveness, 1965, 0.45).
narrative_ontology:measurement(fede_be_t1980, federation_membership__integration_reading, base_extractiveness, 1980, 0.6).
narrative_ontology:measurement(fede_be_t1995, federation_membership__integration_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(fede_be_t2010, federation_membership__integration_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(fede_be_t2025, federation_membership__integration_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1950, federation_membership__integration_reading, suppression_requirement, 1950, 0.2).
narrative_ontology:measurement(fede_su_t1965, federation_membership__integration_reading, suppression_requirement, 1965, 0.35).
narrative_ontology:measurement(fede_su_t1980, federation_membership__integration_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(fede_su_t1995, federation_membership__integration_reading, suppression_requirement, 1995, 0.6).
narrative_ontology:measurement(fede_su_t2010, federation_membership__integration_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(fede_su_t2025, federation_membership__integration_reading, suppression_requirement, 2025, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__integration_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__integration_reading, federation_membership__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'integration_reading' of the 'federation_membership' kernel. It is structurally linked to the 'sovereignty_reading' as a competing interpretation of the same underlying commitment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
