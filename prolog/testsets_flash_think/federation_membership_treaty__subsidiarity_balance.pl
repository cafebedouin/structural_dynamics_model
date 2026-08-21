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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'subsidiarity balance' reading of a
 *   federation's membership treaty, where free movement is balanced against
 *   legitimate national interests. It is a Tangled Rope because it genuinely
 *   coordinates mobility while simultaneously extracting costs from both
 *   member states (loss of full sovereignty) and mobile citizens
 *   (restrictions on movement). The system requires active enforcement by
 *   federal institutions to maintain this delicate balance against constant
 *   political pressures from both integrationist and sovereignist factions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.55).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.6).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance Reading").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '95253d1b-8f58-41f9-9849-b00b019c7222').
narrative_ontology:cs_kernel_codification('95253d1b-8f58-41f9-9849-b00b019c7222', formalized).
narrative_ontology:cs_authority_grounding('95253d1b-8f58-41f9-9849-b00b019c7222', lineage).
narrative_ontology:cs_interpretation_layer_present('95253d1b-8f58-41f9-9849-b00b019c7222').
narrative_ontology:cs_reading_relation('95253d1b-8f58-41f9-9849-b00b019c7222', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('95253d1b-8f58-41f9-9849-b00b019c7222', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('95253d1b-8f58-41f9-9849-b00b019c7222', foundational, proportionality_principle).
narrative_ontology:cs_axiom_status(proportionality_principle, holdable).
narrative_ontology:cs_axiom_grounding('95253d1b-8f58-41f9-9849-b00b019c7222', proportionality_principle, conventional).
narrative_ontology:cs_axiom('95253d1b-8f58-41f9-9849-b00b019c7222', foundational, legitimate_national_interest_clause).
narrative_ontology:cs_axiom_status(legitimate_national_interest_clause, holdable).
narrative_ontology:cs_axiom_grounding('95253d1b-8f58-41f9-9849-b00b019c7222', legitimate_national_interest_clause, conventional).
narrative_ontology:cs_reference_frame('95253d1b-8f58-41f9-9849-b00b019c7222', founding_treaty_balance).
narrative_ontology:cs_drift_state('95253d1b-8f58-41f9-9849-b00b019c7222', contemporary_political_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('95253d1b-8f58-41f9-9849-b00b019c7222', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federal_institutions).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_citizens).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, national_governments).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, mobile_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for interpreting and enforcing the treaty, balancing the principle of free movement with legitimate national interests. They benefit from the stability and legitimacy of the federal framework.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Must accept the principle of free movement for citizens of other member states, but can impose proportional restrictions based on legitimate national interests (e.g., public health, security, social welfare burden). They bear the costs of integration and reduced sovereign control over borders.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states, payer,
    institutional, biographical, constrained, national).

% Benefit from the right to live and work in other member states, but face potential restrictions or administrative burdens based on national policies. They pay the cost of navigating varied national regulations.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_citizens, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, mobile_citizens, payer).

% Benefit from the ability to protect specific national interests (e.g., labor markets, social security systems) by imposing proportional restrictions on free movement, as allowed by the treaty. This allows them to manage domestic political pressures.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, national_governments, beneficiary,
    institutional, biographical, constrained, national).

% Advocates for broader and less restricted free movement, often challenging national restrictions as disproportionate. They monitor federal court rulings and push for more expansive interpretations of mobility rights.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, pro_integration_lobby, observer,
    organized, generational, mobile, continental).

% Advocates for stronger national control over borders and migration, often challenging federal interpretations that limit member state autonomy. They push for more expansive definitions of 'legitimate national interests'.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, pro_sovereignty_lobby, observer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages the movement of people across a federation's internal borders, ensuring a degree of mobility while allowing member states to address specific national concerns, thereby preventing both unrestricted flows and complete border closures.
% TRANSFER_FUNCTION: Transfers some sovereign control over borders from member states to the federal level, in exchange for managed mobility and the ability to impose proportional restrictions. It also transfers some mobility rights to citizens, but with potential costs of national-level restrictions.
% ABSENT_VOICES: Those advocating for either absolute free movement (no national restrictions) or absolute national sovereignty (full border control) are structurally excluded from the 'balance' framing, as their core premises are outside the negotiated compromise.
% DISAPPEARANCE_RATIONALE: If this framework vanished, the federation would face immediate crises: either a collapse into unrestricted movement (overwhelming national welfare systems and labor markets) or a return to hard borders (fragmenting the single market and undermining federal identity). The political and economic landscape would fundamentally reorganize.
% FOUNDING_PROBLEM: The founding problem was to create a common market and a sense of shared identity within a federation, while respecting the distinct national interests and social models of its member states, avoiding both a superstate and a loose confederation.
% FOUNDING_PROBLEM_CORROBORATION: Federal institutions and many member states attest that the problem of balancing integration with national interests remains live, citing ongoing debates over migration, social security, and labor market impacts. Independent legal scholars and political scientists corroborate that the tension is inherent to the federal structure and requires continuous management.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The moderate extractiveness (0.55) reflects the costs imposed on both sides to maintain the balance. Suppression (0.60) is necessary to prevent either extreme (unrestricted movement or blanket restrictions) from dominating. The low theater ratio (0.15) indicates that the legal and institutional mechanisms are genuinely functional, not merely performative. The slight fluctuations in extractiveness and suppression over time reflect the ongoing political contestation and judicial interpretations that continuously adjust the 'balance'.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of federal institutions, this constraint is a necessary and legitimate framework for managing a complex federal system. From the perspective of member states, it can feel like an imposition on sovereignty, while from the perspective of some mobile citizens, it can feel like an arbitrary limitation on fundamental rights. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Federal institutions are beneficiaries as they gain authority and stability from managing this balance. Mobile citizens are both beneficiaries (gaining mobility) and payers (facing restrictions). Member states are primarily payers, ceding some sovereignty, but national governments within them are beneficiaries as they retain some capacity to protect national interests. The 'balance' itself is the object of coordination and extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_definition_ambiguity,
    'What constitutes ''proportionality'' and ''legitimate national interest'' in specific policy domains (e.g., social welfare, public security, labor markets)?',
    'Consistent judicial precedent from federal courts, or clear legislative guidelines from federal bodies, establishing a stable interpretation across cases.',
    'If definitions remain ambiguous, the constraint''s effective extractiveness and suppression can fluctuate unpredictably, leading to perceived unfairness and increased resistance from affected parties. If clarified, it could stabilize the balance or shift it decisively towards one side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_definition_ambiguity, conceptual, 'Ambiguity in key legal terms that define the balance.').

omega_variable(
    reading_legitimacy_contest,
    'Is the ''subsidiarity_balance'' reading genuinely accepted as the authoritative interpretation of the treaty, or is it merely a temporary political compromise?',
    'Long-term stability of judicial rulings and consistent political consensus across member states, or a formal amendment to the treaty codifying this balance.',
    'If it''s a temporary compromise, the constraint is more fragile and susceptible to reclassification towards ''integration_primary'' (if federal power grows) or ''sovereignty_primary'' (if member states assert more control). If it''s genuinely authoritative, its stability as a Tangled Rope is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_legitimacy_contest, empirical, 'The underlying political and legal stability of this specific treaty reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.57).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 50, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.63).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'federation_membership_treaty' kernel, alongside 'integration_primary' and 'sovereignty_primary'. Each reading represents a distinct structural constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
