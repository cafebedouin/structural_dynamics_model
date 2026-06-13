% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_obligations__selective_solidarity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Tiered Free Movement and Contributory Welfare Access
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   The European Union (and other federal structures) faces a foundational
 *   tension: open labor mobility and fiscal welfare-state autonomy are in
 *   structural conflict when economically inactive migrants can access
 *   welfare. The selective-solidarity reading resolves this by tiering free
 *   movement rights and welfare access according to contribution history:
 *   employed workers with documented contributions retain full mobility and
 *   welfare access; economically inactive arrivals face welfare-access
 *   restrictions until they accumulate contribution history or obtain
 *   citizenship. This is presented as a fiscally necessary middle ground
 *   between pure mobility (integration_primary reading) and labor-market
 *   closure (member_sovereignty_primary reading). The reading contest is
 *   situated within the federation's legitimacy kernel — the foundational
 *   commitment to both mobility and welfare-state sovereignty — and different
 *   readings reinterpret which principle takes priority and how the conflict
 *   should be resolved.
 *
 * KEY AGENTS:
 *   - High-contribution workers: employed, mobile, welfare-unrestricted; the model migrants the constraint enables.
 *   - Economically inactive migrants: newly arrived, unemployed, students; structurally trapped between right-to-move and inability-to-survive.
 *   - Welfare-state fiscal gatekeepers: finance ministries, social security administrators; set thresholds and police contribution status.
 *   - Member-state treasuries: directly benefit from welfare-access closure; primary fiscal beneficiaries.
 *   - Migrant advocacy organizations: excluded from decision-making; contest the contribution-as-entitlement premise.
 *   - Federation-integrationist parties: observe the constraint as a retreat from membership equality; advocate decoupling movement from welfare.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.68).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.71).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Tiered Free Movement and Contributory Welfare Access").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '3a82a414-f525-4655-97ab-dce7823dc160').
narrative_ontology:cs_kernel_codification('3a82a414-f525-4655-97ab-dce7823dc160', formalized).
narrative_ontology:cs_authority_grounding('3a82a414-f525-4655-97ab-dce7823dc160', extraction).
narrative_ontology:cs_interpretation_layer_present('3a82a414-f525-4655-97ab-dce7823dc160').
narrative_ontology:cs_reading_relation('3a82a414-f525-4655-97ab-dce7823dc160', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('3a82a414-f525-4655-97ab-dce7823dc160', federation_membership_obligations__member_sovereignty_primary, influences).
narrative_ontology:cs_axiom('3a82a414-f525-4655-97ab-dce7823dc160', foundational, welfare_access_follows_contribution_not_citizenship).
narrative_ontology:cs_axiom_status(welfare_access_follows_contribution_not_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('3a82a414-f525-4655-97ab-dce7823dc160', welfare_access_follows_contribution_not_citizenship, instrumental).
narrative_ontology:cs_axiom('3a82a414-f525-4655-97ab-dce7823dc160', foundational, fiscal_solidarity_bounded_by_member_state_capacity).
narrative_ontology:cs_axiom_status(fiscal_solidarity_bounded_by_member_state_capacity, holdable).
narrative_ontology:cs_axiom_grounding('3a82a414-f525-4655-97ab-dce7823dc160', fiscal_solidarity_bounded_by_member_state_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('3a82a414-f525-4655-97ab-dce7823dc160', contribution_based_welfare_access).
narrative_ontology:cs_drift_state('3a82a414-f525-4655-97ab-dce7823dc160', contemporary_migrant_welfare_policy, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a82a414-f525-4655-97ab-dce7823dc160', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, high_contribution_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, welfare_state_fiscal_gatekeepers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, member_state_treasuries).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, precarious_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, low_contribution_history_arrivals).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_obligations__selective_solidarity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_obligations__selective_solidarity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness climbs from 0.48 to 0.68 over the interval, reflecting the ratcheting up of contribution-status definitions and tightening of welfare-access administration. Early on (t=0), the tiering is loose — contribution thresholds are permissive, documentation standards are informal, and economically inactive migrants can often still access key supports. By t=25, the system has hardened: documentation requirements have become strict, contribution definitions narrow, and welfare-access restrictions bite deeper. Suppression rises in tandem (0.52 to 0.71) because the constraint's persistence increasingly depends on active enforcement — excluding migrants from welfare systems, monitoring contribution status, rejecting claims — rather than on participant consent. Theater ratio stays moderate (0.28 to 0.42) because the administrative apparatus is real and necessary (welfare-system management requires genuine scrutiny), but a growing share of that apparatus serves enforcement of the tiering rather than identification of genuine need. The claim of tangled_rope is structurally warranted: the constraint coordinates a real federation problem (welfare + mobility collision) AND extracts substantially from economically inactive migrants (asymmetric benefits and costs) AND requires active enforcement to persist (without policing, the tiering collapses).
 *
 * PERSPECTIVAL GAP:
 *   From the welfare-gatekeeper and member-state seat, the constraint appears as necessary fiscal coordination — a solution to a real collective-action problem (unbounded welfare claims under free movement). From the economically inactive migrant seat, the same structure appears as extraction disguised as necessity — a way to access federation mobility rights while denying the welfare support required to use them. The engine computes these divergent classifications from the structural data: gatekeeper seats see lower effective extraction (they designed it as coordination, benefit from it) while victim seats see higher effective extraction (they cannot escape it, cannot use the mobility it permits). The authored metrics and the claimed type do not pre-adjudicate which seat's reading is correct — they stand independent of each other, and their divergence is what the corpus measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are named: high-contribution workers (clear gain from unrestricted mobility + welfare access), welfare-state fiscal gatekeepers (control thresholds, design the system), member-state treasuries (directly capture reduced welfare expenditure). Victims are named: economically-inactive migrants (denied welfare access, trapped despite formal mobility), precarious workers (cycling in and out of contribution status, bearing the administrative burden). The directionality of high-contribution workers derives from beneficiary-status + arbitrage-exit options → d near 0.2. The directionality of economically-inactive migrants derives from victim-status + trapped-exit → d near 0.85. Welfare gatekeepers sit at agenda_setter role with institutional power and analytical exit, but the constraint they enforce is federal law, not purely their design — their d is pulled between the agenda-setting (lowering d) and their powerlessness to actually change the constraint unilaterally (raising d) — derive d ~ 0.35-0.40 from this institutional constraint-within-constraint. Member-state treasuries are the primary extractors (their budgets benefit), but they are locked into the federal framework and domestic welfare compacts — institutional, analytical exit — d ~ 0.25-0.35.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem here is NOT a dead problem being performed — the problem of welfare-cost collision under free movement is demonstrably live. What is contested is the diagnosis and the solution. Integration-primary reading diagnoses the problem as member-state selfishness (closing welfare to migrants violates membership equality) and prescribes federation-level fiscal transfers. Member-sovereignty-primary reading diagnoses the problem as excessive labor mobility (destabilizing welfare systems) and prescribes labor-market closure. Selective-solidarity reading (this one) diagnoses it as a misalignment of rights and contributions and prescribes tiering. None of these is mandatrophy (constraint with dead founding problem). Rather, this is a reading contest where different diagnoses of the same live problem produce incompatible solutions. The theater-ratio climb (0.28 to 0.42) signals that administrative energy is increasingly spent on enforcement of tiering rather than on legitimate welfare assessment — a drift toward performance — but the founding-problem status remains live, so the constraint is not piton-class. It is a tangled_rope in reading-contest condition: coordination function (welfare + mobility) + extraction (tiering by contribution) + enforcement (policing of status) remain structurally coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contribution_definition_ambiguity,
    'What counts as ''contribution'' sufficient to unlock welfare access — formal employment only, or caregiving, education, volunteering, previous tax history? How is contribution history backdated, and what happens to migrations during economic recessions when employment contribution is impossible?',
    'Jurisdictional comparison: examine how different member states and federal systems actually define contribution thresholds in practice; audit the temporal stability of definitions across economic cycles; examine documented evidence of how categorically excluded groups (persons with disabilities, caregivers, early retirees) are treated.',
    'A narrow definition of contribution (formal employment only, recent accumulation) maximizes extraction against economically inactive migrants and justifies the constraint as necessary fiscal closure. A broad definition (caregiving, volunteering, historical credits) reduces extraction and shifts the reading closer to Rope or away from victim-hood characterization. The definition choice itself is not neutral — it embeds a theory of what contributions ''count'' as participation in the federation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contribution_definition_ambiguity, empirical, 'Whether contribution is narrowly employment-based or broadly defined; how much room the system leaves for non-market forms of participation.').

omega_variable(
    federation_level_fiscal_capacity,
    'Does the federation possess sufficient fiscal capacity (tax base, central budget) to implement welfare-access rights without member-state welfare collapse, or is member-state fiscal autonomy genuinely a binding constraint on welfare generosity?',
    'Fiscal federalism analysis: comparative study of fiscal capacity, transfer mechanisms, and budget stabilization across federal systems; scenario modeling of federation-level fiscal transfers sufficient to guarantee welfare access for all mobile persons regardless of contribution history.',
    'If federation-level fiscal capacity is available but untapped, the selective-solidarity reading is a choice by member states to withhold solidarity, not a necessity imposed by fiscal constraints — the constraint is then pure extraction, not coordination. If fiscal capacity is genuinely constrained, the tiering is a real coordination solution to a binding constraint. The founding-problem diagnosis hinges on this empirical question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federation_level_fiscal_capacity, empirical, 'Whether the federation could afford universal welfare access if it chose to, or whether member-state fiscal capacity is the actual binding constraint.').

omega_variable(
    kernel_reading_foreclosure_risk,
    'Do the axioms of this reading (welfare access follows contribution, not citizenship) logically foreclose the integration_primary reading (welfare access follows membership), or do the readings coexist as different resolutions of the same founding problem?',
    'Axiom analysis: explicit statement of the foundational normative claims of each reading; assessment of whether accepting this reading''s axioms requires rejecting the integration_primary axioms within a single coherent framework, or whether the readings represent genuinely incompatible commitments only when both are held simultaneously.',
    'If foreclosure is real (axioms are contradictory), the readings cannot coexist in a single federation — one framework must prevail. If coexistence is possible (axioms are different resolutions of an ambiguous kernel), the federation''s legitimacy crisis is political, not logical — different seats hold different readings, but no reading is internally incoherent. The classification of the sibling relationship (forecloses vs. coexists_with in cs_structure.reading_relations) hangs on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_risk, conceptual, 'Whether the selective-solidarity and integration-primary readings are logically incompatible or politically contested variants of the same kernel.').

omega_variable(
    administrative_cost_of_status_verification,
    'What is the administrative cost of enforcing contribution-status verification and welfare-access tiering, relative to the actual welfare cost saved? Is the bureaucracy itself parasitic on the constraint?',
    'Cost accounting: track the personnel, IT systems, and processing costs of welfare-eligibility verification; compare to the documented welfare expenditure prevented by the tiering; estimate administrative burden on affected migrants (time, legal fees, documentation gathering).',
    'If administrative costs exceed welfare savings, the constraint is theater — enforcement machinery consuming resources without net fiscal gain, possibly with negative utility from migrant perspective. If administrative costs are modest, the constraint''s fiscal coordination claim is stronger. A finding of administrative parasitism would shift the theater_ratio upward and the classification toward piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_of_status_verification, empirical, 'Whether the bureaucracy of contribution-status enforcement justifies itself through fiscal savings or is itself an extractive apparatus.').

omega_variable(
    suppression_internalization_in_migrants,
    'Among economically inactive migrants facing welfare-access restrictions, how much of the observed suppression is structural (formal bars preventing access to services) versus internalized (migrants believing they are not ''entitled'' to welfare, not attempting access even when legal pathways might exist)?',
    'Post-exit analysis: document migrants who leave the jurisdiction due to welfare restrictions; interview a sample about their understanding of entitlement and legal options; compare documented access attempts to available legal pathways; examine patterns of non-take-up of benefits legally available.',
    'If suppression is primarily structural, the constraint''s persistence depends on active enforcement machinery. If suppression is substantially internalized, migrants carry the constraint with them even when the formal barrier is removed — the legitimacy claim (''contribution determines entitlement'') has been internalized as a self-concept. Internalization would shift the classification toward higher effective suppression despite lower measured suppression, and toward a Snare characterization (extraction working through identity and belief, not just formal rules).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_in_migrants, empirical, 'Whether suppression operates through formal structural barriers or through internalized belief in contribution-based entitlement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(fede_tr_t5, federation_membership_obligations__selective_solidarity, theater_ratio, 5, 0.33).
narrative_ontology:measurement(fede_tr_t10, federation_membership_obligations__selective_solidarity, theater_ratio, 10, 0.37).
narrative_ontology:measurement(fede_tr_t15, federation_membership_obligations__selective_solidarity, theater_ratio, 15, 0.4).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.41).
narrative_ontology:measurement(fede_tr_t25, federation_membership_obligations__selective_solidarity, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fede_be_t5, federation_membership_obligations__selective_solidarity, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(fede_be_t10, federation_membership_obligations__selective_solidarity, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(fede_be_t15, federation_membership_obligations__selective_solidarity, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(fede_be_t25, federation_membership_obligations__selective_solidarity, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(fede_su_t5, federation_membership_obligations__selective_solidarity, suppression_requirement, 5, 0.59).
narrative_ontology:measurement(fede_su_t10, federation_membership_obligations__selective_solidarity, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fede_su_t15, federation_membership_obligations__selective_solidarity, suppression_requirement, 15, 0.69).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(fede_su_t25, federation_membership_obligations__selective_solidarity, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.18).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% Part of the federation_membership_obligations constraint family. The kernel is the foundational federation commitment to both labor mobility and social protection. Three readings decompose it: integration_primary (mobility and welfare are inseparable, federation principle dominates), member_sovereignty_primary (member states retain closure authority, member-state principle dominates), and selective_solidarity (this constraint — mobility is federated, welfare is tiered-by-contribution, principles are decoupled). Each reading has a distinct ε, beneficiary/victim structure, and cs_structure axioms. All three should be authored as separate constraint stories linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, institutional, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
