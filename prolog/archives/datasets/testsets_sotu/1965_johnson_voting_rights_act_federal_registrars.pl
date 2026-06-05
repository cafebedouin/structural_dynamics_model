% ============================================================================
% CONSTRAINT STORY: 1965_johnson_voting_rights_act_federal_registrars
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1965_johnson_voting_rights_act_federal_registrars, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: 1965_johnson_voting_rights_act_federal_registrars
 *   human_readable: Federal Voter Registration Authority (Voting Rights Act Section 4/9 Registrars)
 *   domain: governance/electoral_systems/civil_rights
 *
 * SUMMARY:
 *   The Voting Rights Act of 1965 empowered the federal government to deploy
 *   voter registration officials in states and counties with documented
 *   systematic discrimination in voting rights administration. This
 *   constraint represents a fundamental conflict between two forms of
 *   institutional authority: decentralized state administration that had been
 *   weaponized against African American voters, and centralized federal
 *   enforcement designed to correct that systemic abuse. The constraint
 *   exhibits all six classification types depending on observer position,
 *   making it a diagnostic exemplar for how institutional authority appears
 *   differently from positions of power and powerlessness. From the
 *   disenfranchised voter's perspective, federal registrars dissolve an
 *   extractive snare. From the political machine's perspective, federal
 *   authority is coercive displacement of gatekeeping power. From the federal
 *   government's perspective, registration is pure coordination: solving a
 *   multi-jurisdictional failure in a decentralized system. From the civil
 *   rights movement's perspective, federal registrars are a temporary
 *   scaffold with explicit sunset logic. The declining extractiveness over
 *   the measurement interval reflects historical reality: federal
 *   registration authority was most needed when local discrimination was most
 *   severe (ε=0.72 at t=0), gradually became less necessary as compliance
 *   improved (ε=0.38 at t=10), and would theoretically reach zero once
 *   complete compliance eliminated the need for federal oversight. The low
 *   theater ratio (0.15) reflects that federal registration is rule-based and
 *   direct—not performative like traditional local registration processes
 *   filled with discretionary barriers and aesthetic justifications.
 *
 * KEY AGENTS:
 *   - African American voters in the Deep South: Primary beneficiaries (powerless/trapped before VRA) — gain enforceable voting rights, transition from snare victim to constraint-freed participant
 *   - Disfranchised minorities broadly: Primary beneficiaries (powerless/trapped) — literacy test, poll tax, and other systematic barriers removed by federal authority
 *   - White Southern Democratic machines: Primary victims (organized/constrained) — lose gatekeeping control over voter registration, lose ability to manipulate electorate through registration discretion
 *   - State and local registration officials: Secondary victims (institutional/constrained) — lose registration authority to federal registrars, subject to external oversight
 *   - Civil rights movement and voting rights coalition: Organized beneficiaries (organized/constrained) — gain federal enforcement power, build political participation, maintain goal of temporary federal presence leading to state compliance
 *   - Federal executive authority: Institutional beneficiary (institutional/arbitrage) — gains enforcement authority, expands federal power, experiences constraint as coordination mechanism
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing a constructed institutional apparatus as a law of federalism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1965_johnson_voting_rights_act_federal_registrars, 0.38).
domain_priors:suppression_score(1965_johnson_voting_rights_act_federal_registrars, 0.85).
domain_priors:theater_ratio(1965_johnson_voting_rights_act_federal_registrars, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1965_johnson_voting_rights_act_federal_registrars, extractiveness, 0.38).
narrative_ontology:constraint_metric(1965_johnson_voting_rights_act_federal_registrars, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(1965_johnson_voting_rights_act_federal_registrars, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1965_johnson_voting_rights_act_federal_registrars, tangled_rope).
narrative_ontology:human_readable(1965_johnson_voting_rights_act_federal_registrars, "Federal Voter Registration Authority (Voting Rights Act Section 4/9 Registrars)").
narrative_ontology:topic_domain(1965_johnson_voting_rights_act_federal_registrars, "governance/electoral_systems/civil_rights").

domain_priors:requires_active_enforcement(1965_johnson_voting_rights_act_federal_registrars).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1965_johnson_voting_rights_act_federal_registrars, african_american_voters).
narrative_ontology:constraint_beneficiary(1965_johnson_voting_rights_act_federal_registrars, disfranchised_minorities).
narrative_ontology:constraint_victim(1965_johnson_voting_rights_act_federal_registrars, state_political_machines).
narrative_ontology:constraint_victim(1965_johnson_voting_rights_act_federal_registrars, local_registration_gatekeepers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED VOTER (SNARE) — Before federal registrars, African American voters in the Deep South face impossible barriers: literacy tests applied arbitrarily, grandfather clauses, poll taxes, and personal discretion by local registrars who reject applications without cause. No appeal mechanism exists within state jurisdiction. The voter is trapped by legal and physical force (intimidation at registration sites). Federal registrars remove the suppression mechanism by bypassing local gatekeeping entirely. From this perspective, federal registration appears as *escape from snare*, not as a new constraint. The constraint being analyzed is the *old* local system; federal registrars dissolve it.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LOCAL BLACK ORGANIZING COALITION (TANGLED ROPE) — Benefits from federal registration authority (removes local barriers, creates new voter base, builds political power). Pays costs through dependence on federal enforcement (must maintain relationships with federal registrars, federal authority may be withdrawn, external control limits autonomy). Genuine coordination function exists: federal registrars coordinate multi-jurisdictional enforcement against systematic discrimination. Asymmetric extraction: the coalition's long-term political power depends on federal presence but federal presence also constrains local political autonomy. Neither pure extraction nor pure coordination.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WHITE SOUTHERN POLITICAL MACHINE (SNARE) — Loses control over voter registration (gatekeeping power removed). Federal registrars are an external enforcement apparatus that cannot be captured or negotiated with by local power structures. The machine's extraction mechanism (controlling who votes through registration discretion) is dismantled. From the machine's perspective, federal authority appears as pure coercion with no coordination benefit. Suppression is applied *to* the machine (federal law, federal officers) with no alternative. High effective extraction on the machine because it faces maximum institutional loss.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: STATE GOVERNMENT AUTHORITY (TANGLED ROPE) — Loses registration authority (extraction runs outward from state). Gains some coordination benefits: federal registrars handle the operational burden of registration, reducing state administrative costs in covered jurisdictions. Federal enforcement also removes the state's ability to deny services, which creates stability (no longer bears reputational cost for systematic discrimination). Suppression is moderate: states retain other powers and can eventually exit by demonstrating compliance with VRA requirements. Active enforcement required: federal law explicitly overrides state discretion.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL EXECUTIVE AUTHORITY (ROPE) — Benefits from expanded authority and enforcement power. Experiences federal registration as a pure coordination mechanism: solving the multi-jurisdictional coordination failure created by systematic state-level discrimination. Federal registrars coordinate voting rights compliance across resistant states. Extraction from states/machines runs *toward* federal authority, but federal authority sees this as legitimate enforcement of constitutional requirements, not extraction. Exit options are maximal (federal authority can withdraw, can renegotiate, can shift resources). Theater is minimal: federal registration is direct, rule-based, with little performative content.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL RIGHTS MOVEMENT / VOTING RIGHTS COALITION (SCAFFOLD) — Sees federal registrars as a temporary enforcement mechanism with explicit sunset logic. The VRA is structured with periodic reauthorization requirements (5-year review cycles). The coalition's goal is not permanent federal administration but transformation of state-level institutions to the point where federal oversight becomes unnecessary. As voter registration norms change and state compliance increases, federal registrar presence should decline. Constraint has explicit endpoint: when covered jurisdictions demonstrate sustained compliance, federal authority withdraws. Low theater because compliance is objectively measurable (registration rates, demographic parity, appeal mechanisms).
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: FEDERAL REGISTRATION APPARATUS (PITON) — Long-term inertial view: once established, federal registration authority persists through bureaucratic momentum even after discrimination has formally declined. Registrars become a vestigial administrative layer. Theater increases over time as the original suppression mechanism (local discrimination) weakens but federal presence persists. The apparatus maintains itself through administrative routine rather than functional necessity. This represents degradation of the constraint: once vital enforcement mechanism becomes performative bureaucracy.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational scale, the VRA registrars represent an unavoidable structural feature of federal enforcement against systematic inequality: whenever a decentralized system produces coordinated discrimination, correction requires external authority. The constraint appears immutable—a necessary law of federalism under conditions of jurisdictional inequality. However, the structure declares beneficiaries and victims, which triggers FSM evaluation: this is not a natural law but a constructed institutional apparatus that benefits specific actors. The 'natural' framing naturalizes what is actually a contingent policy choice.
constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1965_johnson_voting_rights_act_federal_registrars_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1965_johnson_voting_rights_act_federal_registrars, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1965_johnson_voting_rights_act_federal_registrars, TR),
    TR >= 0.70.

:- end_tests(1965_johnson_voting_rights_act_federal_registrars_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38 final value): Moderate-low. The constraint involves significant power redistribution (from machines to voters) but is justified and structured as rights enforcement rather than pure extraction. The beneficiaries (disenfranchised voters) gain fundamental political rights. The victims (political machines) lose illegitimate gatekeeping power. Initial extractiveness was high (0.72) because the federal intervention was massive and disruptive to established power structures. Over the measurement interval, extractiveness declines as the constraint achieves its goal (discrimination reduces, voter registration rates converge across racial groups, compliance increases). The declining trajectory reflects the constraint's success: as the underlying discrimination problem is solved, federal registration authority becomes less necessary. Suppression (0.85): Very high. This measures the suppression mechanism being *corrected* by federal registrars—the original state-level suppression (literacy tests, poll taxes, intimidation, arbitrary denial) was approximately 0.95. Federal registrars reduce the suppression experienced by voters to near zero by removing local gatekeeping. Theater ratio (0.15): Very low. Federal registration is rule-based and direct. Applicants present evidence of identity and residence; federal registrars verify eligibility against objective criteria; registration is granted or denied based on law, not discretion. This contrasts sharply with local registration (which had high theater—elaborate justifications for denials, ritualized humiliation, performative literacy tests). The slight increase in theater over time (0.08 to 0.15) reflects bureaucratic routine accumulation as the constraint becomes institutional.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The disenfranchised voter sees federal registrars as liberating. The political machine sees them as coercive occupation. The federal government sees them as legitimate enforcement. The civil rights coalition sees them as temporary scaffolding toward state-level compliance. State government sees them as loss of sovereignty with coordination benefits (reduced administrative burden, removed from complicity in discrimination). The institutional apparatus sees its own eventual degradation (piton perspective). The civilizational observer risks naturalizing a contingent political choice as an immutable federal principle. No perspective is objectively 'correct'—they are all structurally accurate readings from their specific positions. The engine's false summit detector will identify the mountain classification as a false summit because the constraint declares beneficiaries and victims, revealing that the 'natural law of federalism' framing naturalizes what is actually a constructed institutional apparatus responding to specific historical injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the constraint: whether they benefit or bear costs, and whether they can exit or are trapped. Disenfranchised voters have d ≈ 0.95 (maximum targets—their baseline power is powerless, exit is trapped, they bear the full cost of the original suppression; federal registrars reduce their extraction). Political machines have d ≈ 0.90 (maximum targets of federal enforcement—they lose gatekeeping power without meaningful exit options). Federal authority has d ≈ 0.10 (beneficiary—gains enforcement power, arbitrage exit options). Civil rights coalition has d ≈ 0.40 (mixed—benefits from federal enforcement but constrained by dependence on federal presence). The tang of directionality is that different agents experience the same constraint in opposite directions: beneficiaries experience it as rightful enforcement; victims experience it as coercive loss. The framework makes this divergence measurable rather than hiding it under univocal framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in this constraint is resolved by recognizing that all six types are legitimate perspectival readings. The disenfranchised voter's snare (federal registrars dissolve an underlying extractive snare). The machine's snare (federal authority appears as pure coercion from the machine's position). The federal government's rope (pure coordination). The civil rights coalition's scaffold (temporary enforcement with sunset logic). The state government's tangled rope (loss of authority with some coordination benefits). The institutional apparatus's piton (eventual degradation to performative bureaucracy). The analytical observer's mountain (naturalization of contingent policy). All six readings are structurally correct from their respective positions. The constraint does not collapse into a single type—it resolves into a presheaf of perspectival readings that together constitute the full structural understanding. The engine's classification at the primary analytical level (tangled rope: ε=0.38, beneficiaries, victims, active enforcement) represents the canonical reading that best captures the mixed coordination (enforcing equal voting rights) and extraction (concentrating federal authority, removing state control) that characterizes the policy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    registrar_exit_timeline_ambiguity,
    'What timeline threshold demonstrates that systematic discrimination has been solved well enough for federal registrars to withdraw?',
    'Historical data on voter registration denial rates, demographic parity in registration, sustained compliance over multiple election cycles, spontaneous compliance without federal presence',
    'If threshold is too low: federal authority withdraws prematurely and discrimination recurs (constraint regresses to old snare). If threshold is too high: federal presence persists indefinitely and becomes piton (performative bureaucracy). Correct threshold marks true coordination success.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(registrar_exit_timeline_ambiguity, empirical, 'Threshold for determining when federal registration oversight can be withdrawn').

omega_variable(
    suppression_mechanism_persistence,
    'Is the suppression measured (0.85) purely structural (legal barriers, intimidation) or does it include internalized beliefs that persist after legal barriers fall?',
    'Post-registration measurement of voter turnout, political participation rates, and confidence in voting system after legal barriers removed; longitudinal tracking of communities transitioning from federal registrar presence to state authority',
    'If purely structural: removing federal registrars allows normal political participation. If partially internalized: voting participation rates may remain suppressed even after barriers fall, requiring longer federal presence or cultural shift mechanisms beyond registration reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_persistence, empirical, 'Whether suppression is purely structural or partially internalized').

omega_variable(
    extraction_direction_ambiguity,
    'Is federal authority extracting from states/machines (redistributing political power) or enforcing rights (correcting injustice)?',
    'Framing analysis: federal authority claims enforcement of constitutional rights; beneficiaries (disenfranchised voters) experience it as rights restoration; victims (political machines) experience it as loss of power. The same mechanism appears as either enforcement or extraction depending on baseline assumptions about legitimate authority.',
    'If enforcement framing is correct: tangled_rope classification is appropriate (legitimate enforcement with side effect of concentrated federal power). If extraction framing is correct: federal authority is overreach with civil rights as cover. Classification does not resolve this — it documents it as an omega.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_direction_ambiguity, conceptual, 'Whether federal registrars constitute legitimate rights enforcement or extractive power consolidation').

omega_variable(
    state_compliance_incentive_structure,
    'Do states have sufficient incentive to achieve compliance and earn federal registrar withdrawal, or do federal registrars become a permanent fixture that states learn to work around?',
    'Analysis of VRA compliance history: states that achieved full compliance and registrar withdrawal vs states that remained under federal oversight; examination of state legislative responses to federal registration requirements',
    'If states have incentive to comply: scaffold perspective is correct, sunset is real. If states settle into permanent non-compliance or work-around strategies: constraint becomes piton (performative federal presence alongside informal local power structures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_compliance_incentive_structure, empirical, 'Whether state compliance incentives make federal registrar withdrawal achievable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1965_johnson_voting_rights_act_federal_registrars, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vra_tr_t0, 1965_johnson_voting_rights_act_federal_registrars, theater_ratio, 0, 0.08).
narrative_ontology:measurement(vra_tr_t5, 1965_johnson_voting_rights_act_federal_registrars, theater_ratio, 5, 0.12).
narrative_ontology:measurement(vra_tr_t10, 1965_johnson_voting_rights_act_federal_registrars, theater_ratio, 10, 0.15).

% Extraction over time
narrative_ontology:measurement(vra_be_t0, 1965_johnson_voting_rights_act_federal_registrars, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(vra_be_t5, 1965_johnson_voting_rights_act_federal_registrars, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(vra_be_t10, 1965_johnson_voting_rights_act_federal_registrars, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1965_johnson_voting_rights_act_federal_registrars, enforcement_mechanism).
narrative_ontology:affects_constraint(1965_johnson_voting_rights_act_federal_registrars, jim_crow_voter_suppression_system).
narrative_ontology:affects_constraint(1965_johnson_voting_rights_act_federal_registrars, literacy_test_administration_discretion).
narrative_ontology:affects_constraint(1965_johnson_voting_rights_act_federal_registrars, preclearance_requirement_section_5).

% DUAL FORMULATION NOTE:
% Federal registrars under the VRA are downstream of systematic state-level voter suppression (literacy tests, poll taxes, grandfather clauses, arbitrary denial). This constraint story analyzes the federal registrar mechanism itself as a structural phenomenon. Upstream constraints involve the specific suppression mechanisms (literacy test administration, discretionary denial). The federal registrar constraint is the federal response apparatus. The preclearance requirement (Section 5) is a parallel federal enforcement mechanism operating at the legislative level rather than the registration level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1965_johnson_voting_rights_act_federal_registrars, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
