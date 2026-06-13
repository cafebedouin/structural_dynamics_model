% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance on Free Movement
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   Within a federal system (EU, Switzerland, Canada in different contexts),
 *   free movement of labor and persons is treated as a constitutive
 *   principle—but one that member states retain authority to bound through
 *   "legitimate national interests." The subsidiarity balance reading holds
 *   that movement rights are real and protected, but can be proportionally
 *   constrained by public health, public policy, welfare sustainability, and
 *   labor-market stability concerns. This is NOT the integration-primary
 *   reading (movement is the primary good; exceptions are narrow) and NOT the
 *   sovereignty-primary reading (states retain near-total discretion). It is
 *   a middle path: graduated structure where both movement and restriction
 *   are legitimate, and the boundary shifts with circumstances. The
 *   constraint's operation produces tangled extraction: genuine coordination
 *   (the federation exists, its benefits are real) is coupled with asymmetric
 *   extraction (immobile workers, welfare systems, and localized service
 *   providers bear the costs). The suppression metric reflects moderate
 *   enforcement in both directions—federation authorities suppress excessive
 *   member-state restrictions, member states suppress extremes of
 *   unrestricted movement.
 *
 * KEY AGENTS:
 *   - mobile_professionals: high-power beneficiaries of the movement right; low exit cost (arbitrage-grade) across federation jurisdictions
 *   - cross_border_employers: institutional beneficiaries; access to distributed labor supply reduces their hiring friction
 *   - immobile_domestic_workers: moderate-power targets; trapped in local markets facing competition from mobile workers
 *   - welfare_administrators: institutional targets; constrained by inability to condition benefits on residency; face fiscal pressure from selective migration
 *   - federation_authority: agenda-setter; adjudicates the proportionality boundary; enforces the constraint by invalidating state restrictions deemed excessive
 *   - member_state_governments: organized payers; retain formal sovereignty but its scope is graduated and judicially constrained
 *   - indigenous/historically_rooted_populations: excluded from the debate frame; would advocate for territorial control and group rights but are not centered in mobility doctrine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.58).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.52).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance on Free Movement").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, 'cbd632ef-5b39-4cfd-8b47-3842da1fde57').
narrative_ontology:cs_kernel_codification('cbd632ef-5b39-4cfd-8b47-3842da1fde57', formalized).
narrative_ontology:cs_authority_grounding('cbd632ef-5b39-4cfd-8b47-3842da1fde57', lineage).
narrative_ontology:cs_interpretation_layer_present('cbd632ef-5b39-4cfd-8b47-3842da1fde57').
narrative_ontology:cs_reading_relation('cbd632ef-5b39-4cfd-8b47-3842da1fde57', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('cbd632ef-5b39-4cfd-8b47-3842da1fde57', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('cbd632ef-5b39-4cfd-8b47-3842da1fde57', foundational, proportionality_boundary_is_substantive).
narrative_ontology:cs_axiom_status(proportionality_boundary_is_substantive, holdable).
narrative_ontology:cs_axiom_grounding('cbd632ef-5b39-4cfd-8b47-3842da1fde57', proportionality_boundary_is_substantive, deontological).
narrative_ontology:cs_axiom('cbd632ef-5b39-4cfd-8b47-3842da1fde57', foundational, federation_authority_legitimate_arbiter).
narrative_ontology:cs_axiom_status(federation_authority_legitimate_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('cbd632ef-5b39-4cfd-8b47-3842da1fde57', federation_authority_legitimate_arbiter, conventional).
narrative_ontology:cs_reference_frame('cbd632ef-5b39-4cfd-8b47-3842da1fde57', proportional_constraint_with_graduated_exceptions).
narrative_ontology:cs_drift_state('cbd632ef-5b39-4cfd-8b47-3842da1fde57', contemporary_welfare_fiscal_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cbd632ef-5b39-4cfd-8b47-3842da1fde57', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_professionals).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, cross_border_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_authority).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, immobile_domestic_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, welfare_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, localized_service_providers).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).

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
 *   Extractiveness is moderate-to-high (0.58 at interval end) because the constraint structurally transfers bargaining power from immobile workers and localized service providers to mobile, credentialed actors, yet the transfer is not absolute—proportionality doctrine constrains it and member states retain limited exception grounds. Suppression is moderate (0.52) because the constraint requires active enforcement in BOTH directions: federation courts must suppress excessive state restrictions (preventing backsliding to closed borders), and member states must suppress unrestricted movement that would destabilize welfare and labor-market policy. Theater is moderate (0.41) because proportionality doctrine is substantively applied—the boundary genuinely moves with economic conditions and demographic shifts—yet the doctrine also performs a legitimacy function, making restrictions seem principled and exceptions seem temporary rather than constituting an open-ended state veto. The measurement series tracks gradual rise in extractiveness and theater over the interval as member states learn to couch restrictions in proportionality language (the rhetoric of exception becomes more sophisticated, not because the principle weakens but because the domain learns to use it).
 *
 * PERSPECTIVAL GAP:
 *   From the federation_authority and mobile_professionals seats, the constraint is a Rope—genuine coordination that enables gains from integration while managing legitimate exceptions. From the immobile_domestic_workers and welfare_administrators seats, the constraint is substantially extractive—they lack the exit options of mobile actors and bear the costs of labor-market integration without the benefits. Member_state_governments occupy a dual position: they are formal payers (constrained by federation authority) but also beneficiaries (they retain some exception grounds and their citizens include mobile professionals). The engine computes per-seat types from the structural data; this perspectival gap should be observable in the divergence between those computations.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile professionals and cross_border_employers have low directionality (d ~ 0.2–0.35): they structurally benefit from the constraint, have high exit options (arbitrage-grade across jurisdictions), and face little suppression. Immobile_domestic_workers and localized_service_providers have high directionality (d ~ 0.65–0.85): they bear wage pressure, are trapped (cannot exercise the exit option the constraint guarantees), and face suppression from competitive pressure. Welfare_administrators sit at intermediate directionality (d ~ 0.55–0.70): they are institutional actors with formal authority but constrained in its exercise; they face fiscal pressure but retain some exception grounds. Federation_authority is analytical (d = 0.5 by convention). Member_state_governments are moderately high (d ~ 0.60): they are targets of federation court review and must defend restrictions under proportionality doctrine, even though they retain formal authority. No directionality overrides are needed; the structural derivation captures the real relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent economic fragmentation and military conflict through integration) is substantially solved—no member state is attempting unilateral exit or economic nationalism at the scale that motivated the constraint's founding. Yet the constraint persists and has grown in complexity (proportionality doctrine has accumulated case law, making exceptions harder to invoke successfully). The constraint has transformed from its founding purpose (preventing conflict) to managing distributional consequences (how to share the gains from integration without destabilizing localized communities and welfare systems). This is classic mandatrophy: the original mandate is dead but the constraint persists, now functioning as a vehicle for extraction from immobile populations. The fact that proportionality doctrine is SUBSTANTIVELY applied (it is not mere theater) prevents full classification as Piton—the constraint is not pure performance—but the rise in theater_ratio over the interval (0.25 → 0.41) signals increasing performativity as the doctrine becomes more rhetorically sophisticated.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_doctrine_as_cover,
    'Is proportionality doctrine substantively constraining state restrictions, or does it function primarily as a legitimacy cover for the federation authority''s mobility agenda?',
    'Empirical analysis of proportionality case law: do member states win exceptions and successfully defend restrictions at meaningful frequencies, or do they lose systematically? Do courts'' proportionality holdings track genuine trade-offs between movement rights and national interests, or do they perform deference while always favoring mobility?',
    'If doctrine is substantive, the measured suppression (0.52) reflects real bilateral enforcement. If it is cover, the constraint is closer to snare—apparent shared enforcement masks one-directional extraction favoring mobile actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_doctrine_as_cover, empirical, 'Whether proportionality doctrine substantively constrains federation authority or legitimizes one-sided mobility pressure.').

omega_variable(
    welfare_system_divergence_causality,
    'To what extent does free movement CAUSE welfare system divergence and fiscal pressure, versus exploiting pre-existing divergence?',
    'Counterfactual or quasi-experimental analysis: do welfare costs and fiscal pressure rise AFTER free movement is liberalized, or were they present before and are merely exposed by mobility? Do high-benefit jurisdictions show selective in-migration of welfare-dependent populations, or do pre-existing residents become welfare-dependent after mobility opens?',
    'If movement causes divergence, the extraction is real—the constraint transfers welfare risk from a federal insurance pool to localized systems. If it exploits pre-existing divergence, the extraction is somewhat artificial—the constraint makes visible a divergence that existed already.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(welfare_system_divergence_causality, empirical, 'Causal direction of relationship between free movement and welfare fiscal pressure.').

omega_variable(
    identity_lock_vs_choice_immobility,
    'To what extent are immobile_domestic_workers and localized_service_providers trapped by structural constraints (capital, language, family) versus by identity fusion (they define themselves as local, rooted, place-bound)?',
    'Ethnographic or interview research on worker decision-making: when mobility barriers are experimentally reduced (language training, credential recognition support), how many workers exercise the exit option versus remain in place? Do those who remain cite structural barriers or cultural/identity commitment?',
    'If identity-locked, the constraint''s suppression operates partly through internalized acceptance of immobility—post-exit suppression trajectories would show persistence even after physical barriers are removed. If structurally trapped, the suppression is primarily external and would decay as barriers are removed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_vs_choice_immobility, empirical, 'Identity-locked versus structurally-trapped mechanisms in the immobility of domestic workers.').

omega_variable(
    sibling_reading_empirical_separation,
    'Can the three sibling readings—integration_primary, sovereignty_primary, subsidiarity_balance—be empirically distinguished by the pattern of restrictions that actually get approved or rejected in proportionality doctrine?',
    'Comparative case-law analysis across jurisdictions or time periods: do restrictions on welfare access, labor-market protections, residency requirements show different approval rates or doctrinal thresholds across the three reading frameworks? Does one reading''s doctrine approve restrictions the others reject?',
    'If empirically separable, the readings instantiate genuinely different constraints with different ε values and victim/beneficiary structures. If indistinguishable, the readings are rhetorical variants of a single underlying constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_empirical_separation, empirical, 'Whether the three sibling readings produce distinguishable restriction patterns in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fede_tr_t5, federation_membership_treaty__subsidiarity_balance, theater_ratio, 5, 0.28).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.32).
narrative_ontology:measurement(fede_tr_t15, federation_membership_treaty__subsidiarity_balance, theater_ratio, 15, 0.36).
narrative_ontology:measurement(fede_tr_t25, federation_membership_treaty__subsidiarity_balance, theater_ratio, 25, 0.4).
narrative_ontology:measurement(fede_tr_t35, federation_membership_treaty__subsidiarity_balance, theater_ratio, 35, 0.41).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.41).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(fede_be_t25, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(fede_be_t35, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 35, 0.58).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 5, 0.38).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 15, 0.46).
narrative_ontology:measurement(fede_su_t25, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(fede_su_t35, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 35, 0.52).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.18).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% The federation_membership_treaty kernel instantiates three distinct constraint stories: integration_primary (high mobility, low suppression, beneficiary-heavy), sovereignty_primary (low mobility, high suppression, victim-heavy), and subsidiarity_balance (this constraint: moderate mobility with proportional exceptions, tangled extraction from both directions). Each reading has its own ε, beneficiary/victim structure, and type classification. The three constraints are linked by the shared kernel and by the fact that federation doctrine simultaneously embodies all three readings in different jurisdictional contexts or different policy domains (labor mobility may be integration-primary while welfare access is more sovereignty-primary). The sibling constraints are NOT alternative framings of one underlying constraint—they are genuinely different constraints with different structural properties that coexist as live positions within the federation's legal and political order.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_treaty__subsidiarity_balance, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
