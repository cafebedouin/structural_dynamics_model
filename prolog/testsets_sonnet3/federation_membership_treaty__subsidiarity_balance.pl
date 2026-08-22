% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Proportionality-Bounded Free Movement Regime
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint captures the subsidiarity-balance reading of the
 *   federation's free-movement kernel: mobility rights are real and
 *   structurally enforced, but member states retain a bounded,
 *   judicially-supervised capacity to invoke proportionality-tested
 *   restrictions where local labor markets, welfare systems, or public order
 *   face demonstrable strain. This is a graduated regime, not the
 *   integration-primary reading (where restrictions are presumptively
 *   illegitimate) or the sovereignty-primary reading (where movement is
 *   conditional on continuing state consent). The three readings are separate
 *   constraints sharing one kernel; ε, beneficiaries, and victims differ
 *   across them because the underlying legitimacy premise differs, not
 *   because of a shared measurement viewed from different angles.
 *
 * KEY AGENTS:
 *   - mobile_workers_with_transferable_skills: primary beneficiary of exercised mobility (moderate/mobile) — gains access, absorbs friction from proportionality carve-outs
 *   - host_state_low_wage_incumbent_workers: primary target of labor-market competition effects (powerless/trapped) — bears wage/housing pressure the safeguards are meant to check but often arrive late for
 *   - sending_state_essential_service_sectors: secondary target via skilled-labor depletion (moderate/constrained) — no symmetric safeguard exists for outflow
 *   - posted_workers_subject_to_wage_undercutting: distinct victim class (powerless/constrained) — equal-treatment floor exists on paper but enforcement is uneven
 *   - federation_court_and_commission: analytical agenda-setter (institutional/analytical) — administers the proportionality test case by case, holding substantial discretion over where the balance actually falls
 *   - member_state_governments: agenda-setter and payer (institutional/constrained) — negotiate safeguards under cross-pressure from domestic politics and reciprocal-access commitments
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.42).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.48).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Proportionality-Bounded Free Movement Regime").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '1f8b0edd-e79c-40fb-a63e-810839447ad0').
narrative_ontology:cs_kernel_codification('1f8b0edd-e79c-40fb-a63e-810839447ad0', formalized).
narrative_ontology:cs_authority_grounding('1f8b0edd-e79c-40fb-a63e-810839447ad0', expertise).
narrative_ontology:cs_interpretation_layer_present('1f8b0edd-e79c-40fb-a63e-810839447ad0').
narrative_ontology:cs_reading_relation('1f8b0edd-e79c-40fb-a63e-810839447ad0', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('1f8b0edd-e79c-40fb-a63e-810839447ad0', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('1f8b0edd-e79c-40fb-a63e-810839447ad0', foundational, restrictions_and_mobility_are_mutually_limiting_not_hierarchical).
narrative_ontology:cs_axiom_status(restrictions_and_mobility_are_mutually_limiting_not_hierarchical, holdable).
narrative_ontology:cs_axiom_grounding('1f8b0edd-e79c-40fb-a63e-810839447ad0', restrictions_and_mobility_are_mutually_limiting_not_hierarchical, conventional).
narrative_ontology:cs_axiom('1f8b0edd-e79c-40fb-a63e-810839447ad0', foundational, national_interest_justification_requires_case_specific_proportionality_review).
narrative_ontology:cs_axiom_status(national_interest_justification_requires_case_specific_proportionality_review, holdable).
narrative_ontology:cs_axiom_grounding('1f8b0edd-e79c-40fb-a63e-810839447ad0', national_interest_justification_requires_case_specific_proportionality_review, instrumental).
narrative_ontology:cs_reference_frame('1f8b0edd-e79c-40fb-a63e-810839447ad0', graduated_balance_at_treaty_founding).
narrative_ontology:cs_drift_state('1f8b0edd-e79c-40fb-a63e-810839447ad0', contemporary_post_enlargement_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1f8b0edd-e79c-40fb-a63e-810839447ad0', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers_with_transferable_skills).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, host_state_employers_in_shortage_sectors).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, sending_state_treasuries_via_remittances).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_low_wage_incumbent_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, sending_state_essential_service_sectors).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, posted_workers_subject_to_wage_undercutting).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cross borders to take jobs, using treaty-guaranteed mobility rights that are qualified by proportionality tests (residency conditions, social-benefit waiting periods, professional-qualification recognition delays). They gain wage and opportunity access but absorb the friction the proportionality carve-outs impose — recognition lags, benefit gaps, and occasional host-state restriction triggers tied to labor-market thresholds.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers_with_transferable_skills, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on the mobile labor pool to fill vacancies without bidding up wages as much as a closed market would require. They lobby to keep proportionality tests calibrated loosely enough that supply keeps flowing, and can relocate operations if restrictions tighten too far.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_employers_in_shortage_sectors, beneficiary,
    organized, generational, arbitrage, national).

% Compete directly with mobile-worker inflows for the same low-wage jobs and are the group the proportionality safeguards (residency conditions, emergency brakes) are nominally designed to protect. In practice the safeguards trigger late and partially, after wage and housing pressure has already been felt; they cannot relocate the way capital or higher-skill workers can.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_low_wage_incumbent_workers, payer,
    powerless, biographical, trapped, national).

% Lose nurses, doctors, engineers, and skilled tradespeople to outward mobility faster than domestic training can replace them. The treaty's proportionality framework permits sending states no equivalent brake on outflow — the balance is calibrated to host-state disruption, not origin-state depletion.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_state_essential_service_sectors, payer,
    moderate, generational, constrained, national).

% Are sent by employers registered in lower-wage member states to work temporarily in higher-wage states, nominally protected by equal-treatment proportionality rules but in practice paid according to origin-state wage and social-contribution schedules, undercutting local pay scales. Enforcement of the equal-treatment floor is inconsistent across jurisdictions and hard for individual workers to invoke.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, posted_workers_subject_to_wage_undercutting, payer,
    powerless, immediate, constrained, continental).

% Collect remittance inflows and reduced domestic unemployment costs as workers move abroad. This is a real fiscal benefit that offsets — without fully compensating for — the loss of skilled labor documented above.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_state_treasuries_via_remittances, beneficiary,
    institutional, generational, analytical, national).

% Adjudicate what counts as a 'legitimate national interest' sufficient to justify a restriction, and what counts as 'proportionate.' They administer the balancing test case by case, which gives them substantial discretion over where the line falls and makes the regime's actual bite depend heavily on litigated precedent rather than the treaty text alone.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_court_and_commission, agenda_setter,
    institutional, civilizational, analytical, continental).

% Negotiate and periodically renegotiate the safeguard clauses (emergency brakes, transitional periods, benefit residency conditions), balancing domestic political pressure over labor-market disruption against federation-level integration commitments and reciprocal access for their own citizens abroad.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer).

% Fall entirely outside the free-movement framework because it is defined by federation citizenship. They compete in the same informal and precarious labor niches the framework indirectly shapes but have no standing to invoke proportionality protections, favorable or restrictive, in either direction.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, undocumented_and_third_country_national_workers, excluded,
    powerless, immediate, trapped, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a continent-scale labor market where skills, qualifications, and workers move to where they are most productive, while giving member states a structured, judicially-supervised channel to invoke narrowly-tailored restrictions when local labor markets, welfare systems, or public order are demonstrably strained — rather than either unrestricted movement or unilateral border closure.
% TRANSFER_FUNCTION: Moves labor supply from lower-wage/higher-unemployment member states to higher-wage/labor-shortage member states, moves remittance income back to sending states, and moves wage and housing-market pressure onto host-state low-wage incumbents; moves skilled-labor capacity away from sending-state essential services without a symmetric compensating mechanism.
% ABSENT_VOICES: Third-country nationals and undocumented workers are structurally outside the proportionality framework altogether — the balance is struck entirely among federation citizens and member states, and non-citizens experience the labor-market effects with none of the standing to invoke either the mobility right or its safeguards. Sending-state essential-service sectors have limited voice at the federation level because the proportionality doctrine has developed almost entirely around host-state disruption claims, not origin-state depletion claims.
% DISAPPEARANCE_RATIONALE: If the proportionality-bounded regime vanished, member states would revert either to unilateral quota/permit systems (fragmenting the labor market and forcing employers back into national hiring pools) or to genuinely unconditional free movement (removing the safeguards that currently manage acute local disruption). Millions of current cross-border employment relationships, benefit entitlements, and qualification-recognition arrangements are built on the specific graduated structure; its removal would force renegotiation of nearly every bilateral labor arrangement within the federation.
% FOUNDING_PROBLEM: Early-stage federation integration produced two irreconcilable pressures: economically, a genuinely single market required labor to move like capital and goods; politically, member states with divergent wage levels, welfare generosity, and labor-market conditions would not ratify integration without a credible mechanism to prevent sudden, concentrated local disruption. The subsidiarity-balance framework was built to let both pressures operate simultaneously rather than resolving into one or the other.
% FOUNDING_PROBLEM_CORROBORATION: Federation courts and the Commission attest the balance remains necessary and functioning, citing case law where restrictions were upheld or struck down on the merits. Independent labor economists and host-state incumbent-worker advocacy groups attest that in practice the balance has drifted toward near-unconditional mobility because emergency-brake mechanisms are rarely invoked successfully and take years to adjudicate, while sending-state trade unions and rural health-service administrators attest the balance has never accounted for origin-state depletion at all — corroboration from outside the mobile-worker and employer beneficiary groups is mixed rather than confirming the framework's own self-description.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction (ε ≈ 0.42) is moderate, not low and not severe — the graduated structure genuinely limits how far either full liberalization or full closure can go, which is precisely what distinguishes this reading from its siblings. Suppression (≈0.48) is likewise moderate: the regime actively suppresses BOTH unrestricted mobility (via proportionality tests, residency conditions, emergency brakes) AND blanket national restrictions (via judicial review and treaty obligations) — this bidirectional suppression is the structural signature of subsidiarity_balance as opposed to either sibling. Theater ratio is comparatively low (0.28) because the balancing test produces real, litigated, case-by-case outcomes rather than purely symbolic compliance — though it is rising slowly as case law accretes procedural formality. Accessibility collapse is moderate-low (0.35): workable alternative regimes (fuller integration, fuller sovereignty) remain live political options being argued in real institutions, unlike a settled natural fact.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting court/commission seat, the arrangement looks like a genuinely balanced coordination mechanism doing exactly the graduated work it was designed for. From the host-state low-wage incumbent seat, the same mechanism looks like extraction with a slow, unreliable safety valve. From the sending-state essential-service seat, it looks like an arrangement that was never calibrated to their loss at all. The engine computing divergent seat classifications from these structural facts is the expected signature of a tangled_rope: real coordination function (continent-scale labor mobility) coexisting with asymmetric extraction (concentrated costs on low-wage incumbents and depleted sending-state sectors) requiring active enforcement (courts, emergency-brake procedures, equal-treatment monitoring) to hold.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile skilled workers and shortage-sector employers sit near the beneficiary end: they capture the coordination gains and can exploit or lobby around the safeguard machinery. Host-state low-wage incumbents and posted workers sit near the target end: they are structurally powerless, cannot relocate as easily as capital or skilled labor, and are the nominal protected class of a safeguard mechanism that in practice under-protects them. Sending-state essential-service sectors are a target class the framework was never built to protect, because 'legitimate national interest' has developed asymmetrically toward host-state disruption claims. Third-country nationals are excluded from the directionality calculus entirely — they are outside the beneficiary/victim structure of federation citizenship, which is itself a boundary condition of this specific reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling single-market integration with member-state consent to avoid disruptive labor shocks — remains partly live (labor-market divergence between member states persists) but the mechanism built to manage it has drifted: emergency brakes are invoked rarely and adjudicated slowly, while the coordination function (labor mobility itself) has strengthened. This is not simple mandatrophy (the founding problem is not dead), but it is asymmetric drift — the safeguard half of the balance has atrophied relative to the mobility half, which is exactly the kind of graduated, contestable state the subsidiarity_balance reading is built to hold open rather than resolve, unlike the sibling readings which would each resolve it in one direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_capture,
    'Does the federation court''s proportionality test, as actually adjudicated, track genuine local labor-market/welfare-system strain, or has it drifted toward validating whatever level of restriction or openness the litigating parties can afford to argue for at length?',
    'Systematic case-law analysis comparing invoked justifications, evidentiary standards required, and success rates across member states and time periods; compare against independent labor-market strain indicators (unemployment, wage compression, housing cost growth) at the time of each ruling.',
    'If the test tracks genuine strain, this reading is closer to a rope with moderate coordination cost; if it has drifted toward resource-driven litigation outcomes, the tangled_rope classification understates capture and the constraint drifts toward host-state-employer-favoring snare for incumbent workers specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_capture, empirical, 'Whether the proportionality doctrine as applied tracks its stated justification or has been captured by litigation-resourced parties.').

omega_variable(
    sending_state_asymmetry_origin,
    'Is the framework''s asymmetric attention to host-state disruption over sending-state depletion an intentional design choice reflecting where the founding negotiators located the political risk, or an unaddressed gap that has simply never been litigated because sending states lack standing or incentive to raise it?',
    'Review of treaty negotiating history and subsequent legislative debate for explicit discussion of brain-drain/depletion effects; absence of discussion across decades would support the unaddressed-gap reading.',
    'If intentional, sending-state essential-service depletion is a known and accepted cost of the coordination function (still a victim class, but a knowingly-traded-off one). If an unaddressed gap, it represents a genuine blind spot in the balance the subsidiarity_balance reading claims to strike, weakening the claim that the reading achieves proportionality across all affected parties rather than only host-state-visible ones.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sending_state_asymmetry_origin, conceptual, 'Whether sending-state depletion is a deliberate trade-off or an unaddressed asymmetry in the balance.').

omega_variable(
    kernel_framing_alternative_reading,
    'Is subsidiarity_balance a genuinely distinct structural reading of the kernel, or is it better understood as integration_primary in practice with sovereignty_primary rhetoric layered on top for domestic political legitimation?',
    'Compare the rate at which emergency-brake and restriction mechanisms actually succeed versus the rate at which mobility claims succeed, over a multi-decade window; a sustained, large asymmetry favoring mobility claims would suggest the ''balance'' framing is largely rhetorical cover for a de facto integration_primary regime.',
    'If the balance is rhetorical, this story''s claimed_type and metrics should more closely resemble the integration_primary sibling''s profile, and the subsidiarity_balance reading itself would be better classified as approaching piton (a genuine balancing function that has atrophied into justificatory theater over a still-operating mobility mechanism) rather than tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_alternative_reading, conceptual, 'Whether the balance is structurally real or primarily a legitimating narrative over a de facto integration-primary outcome; routes the committer contest between readings into an explicit uncertainty rather than resolving it by fiat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t4, federation_membership_treaty__subsidiarity_balance, theater_ratio, 4, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__subsidiarity_balance, theater_ratio, 8, 0.21).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__subsidiarity_balance, theater_ratio, 12, 0.23).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__subsidiarity_balance, theater_ratio, 16, 0.25).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__subsidiarity_balance, theater_ratio, 20, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fede_be_t4, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 8, 0.36).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 16, 0.4).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.41).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(fede_su_t4, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 4, 0.4).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.47).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_treaty__subsidiarity_balance, 0.12).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, sovereignty_primary).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the federation_membership_treaty kernel. integration_primary treats restrictions as presumptively illegitimate (lower ε, near-absent incumbent-worker victim class); sovereignty_primary treats movement as conditional on state consent (higher suppression of mobility itself, near-absent mobile-worker beneficiary class at full strength). This reading (subsidiarity_balance) holds a graduated middle structure with moderate, bidirectional suppression and a beneficiary/victim set that varies by policy domain. Each reading is authored as its own ε-invariant constraint; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
