% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership (Sovereignty Reading): Conditional Treaty with National Border Control
 *   domain: political/federalism/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty_reading of the
 *   federation_membership kernel. It treats federation membership as a
 *   conditional treaty among sovereign states in which national governments
 *   retain legitimate authority to control internal borders and restrict
 *   labor market access. Free movement is framed as a negotiable policy
 *   concession rather than a constitutional right. The arrangement extracts
 *   from mobile citizens by making their cross-border mobility contingent on
 *   inter-state bargaining, while benefiting domestic labor markets and
 *   member state governments. The sibling integration_reading treats the same
 *   treaty structure as irreversible integration with supranational authority
 *   and constitutional free movement. The two readings produce structurally
 *   distinct constraints from the same kernel.
 *
 * KEY AGENTS:
 *   - member_state_governments: Agenda-setter (institutional/national) â retain and administer border controls under conditional treaty terms
 *   - local_labor_markets: Beneficiary (organized/national) â protected from cross-border labor competition
 *   - mobile_citizens: Payer (powerless/continental) â bear mobility restrictions and labor market exclusion
 *   - supranational_institutions: Observer (institutional/continental) â lack enforcement authority under sovereignty reading
 *   - human_rights_organizations: Excluded (organized/continental) â advocate for mobility rights outside treaty negotiation rooms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.78).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.75).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership (Sovereignty Reading): Conditional Treaty with National Border Control").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political/federalism/migration").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'f3d18329-bfb0-4c8a-89e2-43ee3450df10').
narrative_ontology:cs_kernel_codification('f3d18329-bfb0-4c8a-89e2-43ee3450df10', formalized).
narrative_ontology:cs_authority_grounding('f3d18329-bfb0-4c8a-89e2-43ee3450df10', lineage).
narrative_ontology:cs_interpretation_layer_present('f3d18329-bfb0-4c8a-89e2-43ee3450df10').
narrative_ontology:cs_reading_relation('f3d18329-bfb0-4c8a-89e2-43ee3450df10', federation_membership__integration_reading, forecloses).
narrative_ontology:cs_axiom('f3d18329-bfb0-4c8a-89e2-43ee3450df10', foundational, national_sovereignty_over_mobility).
narrative_ontology:cs_axiom_status(national_sovereignty_over_mobility, holdable).
narrative_ontology:cs_axiom_grounding('f3d18329-bfb0-4c8a-89e2-43ee3450df10', national_sovereignty_over_mobility, conventional).
narrative_ontology:cs_axiom('f3d18329-bfb0-4c8a-89e2-43ee3450df10', foundational, free_movement_as_concession).
narrative_ontology:cs_axiom_status(free_movement_as_concession, holdable).
narrative_ontology:cs_axiom_grounding('f3d18329-bfb0-4c8a-89e2-43ee3450df10', free_movement_as_concession, conventional).
narrative_ontology:cs_reference_frame('f3d18329-bfb0-4c8a-89e2-43ee3450df10', westphalian_sovereignty).
narrative_ontology:cs_drift_state('f3d18329-bfb0-4c8a-89e2-43ee3450df10', contemporary_federation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f3d18329-bfb0-4c8a-89e2-43ee3450df10', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, member_state_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Negotiate and administer federation treaty terms, retaining formal authority to impose border controls, emergency brakes on free movement, and labor market restrictions. They set the conditions under which mobility is permitted and collect political legitimacy from sovereign autonomy.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership__sovereignty_reading, member_state_governments, beneficiary).

% Domestic workers and incumbent employers whose wages and job security are shielded from cross-border competition by mobility restrictions. They organize to lobby for continued border controls and conditional treaty provisions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, national).

% Citizens of member states who seek residence or employment across internal borders but face quotas, waiting periods, or exclusionary rules. Their mobility is contingent on inter-governmental negotiations in which they have no direct seat.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    powerless, immediate, constrained, continental).

% Bodies tasked with treaty oversight and integration promotion that lack effective enforcement authority under the sovereignty reading. They issue advisory opinions and infringement notices that member states can ignore or delay.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, observer,
    institutional, generational, analytical, continental).

% Advocate for mobility as a citizenship right rather than a policy concession. They are structurally excluded from closed treaty negotiations where sovereignty concerns dominate bargaining positions.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, human_rights_organizations, excluded,
    organized, biographical, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates relations among sovereign states by providing a formal treaty framework for partial cooperationâtrade, security, or diplomacyâwithout requiring full political integration or surrender of border authority.
% TRANSFER_FUNCTION: Moves the authority to permit or deny cross-border residence and labor market access from individual mobile citizens to member state governments and domestic labor market incumbents.
% ABSENT_VOICES: Mobile citizens directly affected by mobility restrictions and supranational integration advocates are formally consulted but substantively excluded when treaty negotiations prioritize sovereignty over free movement.
% DISAPPEARANCE_RATIONALE: If the conditional treaty and national border legitimacy vanished overnight, internal borders would lose their federation-sanctioned justification, mobility would default to open or require explicit bilateral agreements, and domestic labor markets would face immediate cross-border competition. The federation would either collapse into fully sovereign states or be forced toward the integration reading.
% FOUNDING_PROBLEM: How to secure interstate cooperation among sovereign entities with divergent economic interests, labor market conditions, and social systems without imposing unrestricted migration or full political unification.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments and domestic labor unions attest the problem remains live, citing wage divergence and social system differences. Supranational institutions and mobile citizen advocates contest this framing from outside the beneficiary set. Comparative political economists note that interstate heterogeneity persists, but disagree on whether mobility restrictions are the necessary or legitimate response.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because mobility is structurally contingent on government discretion rather than guaranteed by citizenship. Suppression (0.75) reflects the active legal and physical border infrastructure required to maintain differential access. Theater ratio (0.45) captures the performative sovereignty rhetoric that layers atop functional extractionâceremonial border assertions that exceed the security necessity. Accessibility collapse (0.62) is moderate because illegal migration and alternative bilateral routes persist, though at elevated cost and risk. Resistance (0.55) is moderate: mobile citizens and pro-migration advocates contest the arrangement, but domestic labor market incumbents and nationalist coalitions provide countervailing support.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (member state governments) experiences the constraint as a necessary coordination mechanism preserving democratic legitimacy and social cohesion. The payer seat (mobile citizens) experiences the same structure as an arbitrary barrier to economic opportunity and personal autonomy. The engine computes this divergence from structural data: identical treaty text produces near-beneficiary directionality for states and near-target directionality for mobile citizens. Local labor markets occupy a mixed positionâbeneficiaries without administrative burdenâreceiving low directionality from the beneficiary declaration.
 *
 * DIRECTIONALITY LOGIC:
 *   Member state governments and local labor markets are declared beneficiaries, deriving low directionality (d near 0.0) and thus damped effective extraction; they are subsidized by the constraint. Mobile citizens are declared victims (payers), deriving high directionality (d near 1.0) and amplified effective extraction. The divergence is driven by the structural asymmetry in exit options: governments can renegotiate treaty terms or invoke emergency brakes, while mobile citizens face constrained or trapped mobility with no individual opt-out.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as tangled_rope rather than snare preserves the genuine coordination function that federation membership solvesâinterstate cooperation on trade, security, and diplomacyâwhile registering the asymmetric extraction embedded in the mobility regime. A pure snare reading would erase the coordination function and mislabel the treaty as cover for extraction alone; a pure rope reading would ignore the victim set. The mandatrophy-resistance of tangled_rope captures that the treaty persists partly because it coordinates states and partly because it extracts from mobile citizens, with active enforcement required to hold both functions simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_integration_reading_boundary,
    'Does the sovereignty reading foreclose the integration reading within a single legal framework, or can both readings coexist as live interpretive options held by different parties?',
    'Comparative constitutional analysis of federation treaty texts, court rulings on the revocability of membership, and the legal status of free movement clauses.',
    'If foreclosed, the two constraints are mutually exclusive and the engine should treat them as a contradiction pair; if coexistent, they are competing framings whose relative dominance depends on political power rather than logical structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_integration_reading_boundary, conceptual, 'Mutual exclusivity of sovereignty and integration readings').

omega_variable(
    mobility_restriction_enforcement_ambiguity,
    'Is the suppression of mobile citizens'' mobility primarily structural (border controls, legal quotas, work permits) or internalized (acceptance of national identity as legitimately mobility-limiting)?',
    'Comparative survey of mobile citizens'' perceptions of border legitimacy across member states; measurement of restriction persistence after formal barriers are temporarily lifted.',
    'If internalized, effective extraction exceeds structural measures and the constraint functions partly as identity coordination; if purely structural, extraction is bounded by enforcement capacity and reverts to conventional tangled rope dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mobility_restriction_enforcement_ambiguity, empirical, 'Structural vs internalized suppression of mobility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(fede_tr_t30, federation_membership__sovereignty_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(fede_tr_t40, federation_membership__sovereignty_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(fede_tr_t50, federation_membership__sovereignty_reading, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fede_be_t30, federation_membership__sovereignty_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(fede_be_t40, federation_membership__sovereignty_reading, base_extractiveness, 40, 0.74).
narrative_ontology:measurement(fede_be_t50, federation_membership__sovereignty_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(fede_su_t30, federation_membership__sovereignty_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(fede_su_t40, federation_membership__sovereignty_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(fede_su_t50, federation_membership__sovereignty_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% The federation_membership kernel decomposes into two structurally distinct constraints. The sovereignty reading (this file) and the integration reading (sibling file) share a treaty text but assign opposite directionalities to mobile citizens and opposite authority to supranational institutions. Their epsilon values differ because the referent constraint differs: one is a conditional sovereignty arrangement, the other a constitutional integration arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
