% ============================================================================
% CONSTRAINT STORY: federation_membership_obligations__selective_solidarity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: federation_membership_obligations__selective_solidarity
 *   human_readable: Contribution-Tiered Free Movement and Welfare Access (Selective Solidarity Reading)
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint instantiates the 'selective solidarity' reading of the
 *   federation membership obligations kernel: free movement is retained as a
 *   formal right for all mobile citizens, but welfare access is bifurcated by
 *   contribution history and current economic activity status rather than by
 *   citizenship or bare residence. An employed mobile worker or one with a
 *   qualifying contribution record enjoys equal treatment; an economically
 *   inactive mobile citizen, a long-term jobseeker, or a worker with
 *   interrupted contributions faces habitual-residence tests, right-to-reside
 *   checks, and eligibility cliffs that can convert nominal free movement
 *   into practical destitution or forced return. This reading is neither the
 *   'integration_primary' reading (which would subordinate welfare boundaries
 *   to mobility rights wholesale) nor the 'member_sovereignty_primary'
 *   reading (which would let member states close welfare borders on
 *   sovereignty grounds); it threads between them by making the contributory
 *   record, not nationality, the sorting variable.
 *
 * KEY AGENTS:
 *   - host_state_treasuries: agenda_setter/beneficiary (institutional/analytical) — designs and enforces contributory eligibility tests, retains fiscal savings
 *   - employed_mobile_workers: beneficiary (moderate/mobile) — full rights follow employment status
 *   - sending_state_governments: beneficiary (institutional/analytical) — offloaded fiscal burden for inactive co-nationals
 *   - economically_inactive_mobile_citizens: payer (powerless/trapped) — exercise formal right to reside without corresponding welfare access
 *   - precariously_employed_mobile_workers: payer/beneficiary (powerless/constrained) — flickering eligibility tied to unstable employment
 *   - long_term_jobseekers: payer (powerless/trapped) — converted from worker-seeking status to inactive status by evidentiary cliffs
 *   - federation_court_and_commission: observer (institutional/analytical) — adjudicates the equal-treatment/contributory boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.52).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.48).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contribution-Tiered Free Movement and Welfare Access (Selective Solidarity Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '3442d5a8-80f5-4a6a-9efe-a2572f601998').
narrative_ontology:cs_kernel_codification('3442d5a8-80f5-4a6a-9efe-a2572f601998', formalized).
narrative_ontology:cs_authority_grounding('3442d5a8-80f5-4a6a-9efe-a2572f601998', extraction).
narrative_ontology:cs_interpretation_layer_present('3442d5a8-80f5-4a6a-9efe-a2572f601998').
narrative_ontology:cs_reading_relation('3442d5a8-80f5-4a6a-9efe-a2572f601998', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('3442d5a8-80f5-4a6a-9efe-a2572f601998', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('3442d5a8-80f5-4a6a-9efe-a2572f601998', foundational, welfare_entitlement_follows_contribution_not_citizenship).
narrative_ontology:cs_axiom_status(welfare_entitlement_follows_contribution_not_citizenship, holdable).
narrative_ontology:cs_axiom_grounding('3442d5a8-80f5-4a6a-9efe-a2572f601998', welfare_entitlement_follows_contribution_not_citizenship, conventional).
narrative_ontology:cs_axiom('3442d5a8-80f5-4a6a-9efe-a2572f601998', secondary, economic_activity_status_is_legitimate_sorting_criterion).
narrative_ontology:cs_axiom_status(economic_activity_status_is_legitimate_sorting_criterion, holdable).
narrative_ontology:cs_axiom_grounding('3442d5a8-80f5-4a6a-9efe-a2572f601998', economic_activity_status_is_legitimate_sorting_criterion, instrumental).
narrative_ontology:cs_reference_frame('3442d5a8-80f5-4a6a-9efe-a2572f601998', contributory_reciprocity_baseline).
narrative_ontology:cs_drift_state('3442d5a8-80f5-4a6a-9efe-a2572f601998', post_free_movement_litigation_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('3442d5a8-80f5-4a6a-9efe-a2572f601998', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, sending_state_governments).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, precariously_employed_mobile_workers).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, long_term_jobseekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, precariously_employed_mobile_workers).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer welfare eligibility rules that condition access on employment record, National Insurance-style contribution history, or habitual residence tests. They design and enforce the habitual residence and right-to-reside checks, and directly retain the fiscal savings from excluding economically inactive mobile citizens from non-contributory benefits.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_treasuries, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, host_state_treasuries, beneficiary).

% Workers who have moved across the federation and hold active employment or a sufficient contribution record. They retain full free movement rights, equal treatment in in-work benefits, and portability of accrued social insurance. Their exit options remain genuinely open because their employment status itself is the passport to continued access.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Countries of origin benefit when their economically inactive citizens are denied host-state welfare, since this removes any incentive for those citizens to emigrate purely for benefit access and keeps the fiscal burden of supporting non-working co-nationals on the sending state's own books (or forces return migration), which the sending state does not have to subsidize abroad.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, sending_state_governments, beneficiary,
    institutional, generational, analytical, continental).

% Retirees, jobseekers who have exhausted job-seeker status windows, and dependents who moved under free movement rights but lack a qualifying contribution record. They are formally free to reside but are excluded from non-contributory welfare (housing benefit, minimum income support), leaving them exposed to destitution or forced return migration despite having exercised a nominally universal right.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, biographical, trapped, continental).

% Workers in gig work, zero-hours contracts, or seasonal labor whose contribution record is thin or intermittent. They fall in and out of qualifying worker status, so their welfare access flickers on and off with employment status changes, and a single gap in contributions can trigger reassessment and loss of the rights their labor was supposed to secure.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, precariously_employed_mobile_workers, payer,
    powerless, immediate, constrained, continental).
narrative_ontology:stakeholder_secondary_role(federation_membership_obligations__selective_solidarity, precariously_employed_mobile_workers, beneficiary).

% Mobile citizens who moved to seek work in good faith but have not found qualifying employment within the host state's permitted search period. Their right to reside becomes conditional on demonstrating genuine prospect of employment, and failure to meet that evidentiary bar converts them into economically inactive status with restricted access, effectively converting a labor-market risk into a welfare-eligibility cliff.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, long_term_jobseekers, payer,
    powerless, biographical, trapped, continental).

% Native low-wage workers competing in the same labor market segments as employed mobile workers are not formally party to the free-movement/welfare bargain, yet bear downward wage pressure and service competition from it. Their distributional concerns are raised in domestic politics but are not part of the contributory-eligibility framework itself, which is negotiated between federation-level and member-state institutions without their direct seat.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_domestic_low_wage_workers, excluded,
    moderate, biographical, constrained, national).

% The federation-level judiciary and executive interpret and adjudicate the boundary between the free movement guarantee and the contributory welfare carve-outs, ruling on individual cases (habitual residence tests, sufficient resources requirements) and shaping how far member states may tier access without violating equal treatment principles.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federation_court_and_commission, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes the genuine coordination problem of free movement (letting labor move to where it is needed, and pooling social insurance contributions across the mobility area) from a redistribution problem (whether unconditional cash welfare should be portable to non-contributors), by tying welfare eligibility to a worker's contribution record rather than to citizenship or mere physical presence.
% TRANSFER_FUNCTION: Moves fiscal exposure away from host-state treasuries and away from sending states hosting inactive returnees, and moves risk onto individual mobile citizens whose contribution record is interrupted, incomplete, or nonexistent — the arrangement transfers the cost of economic inactivity from the collective welfare pool to the inactive individual's own resources or forced return.
% ABSENT_VOICES: Economically inactive mobile citizens and long-term jobseekers who fall through the contributory net rarely have organized representation in the treaty-level or legislative negotiations that set the eligibility thresholds; their situation is documented mainly through case law (individual litigants) and civil society reporting rather than through a seat at the negotiating table. Domestic low-wage workers who feel wage/service competition are also structurally outside this specific contributory bargain.
% DISAPPEARANCE_RATIONALE: If the contributory tiering vanished and welfare access reverted purely to residence/citizenship-blind universalism, host-state treasuries would face a materially different fiscal exposure (contested estimates of magnitude), sending states would lose the safety-valve function, and inactive mobile citizens would gain access overnight — but employed mobile workers and the free-movement coordination function itself would likely persist largely unchanged, since their entitlement already runs through contribution. Whether 'the world rearranges' therefore depends on which seat you ask: treasuries and inactive citizens say yes; employed workers and the coordination architecture say comparatively little changes for them.
% FOUNDING_PROBLEM: Free movement was designed to let labor flow to where it is productively used without triggering unsustainable fiscal transfers or 'welfare shopping' that would make member states resist deepening the mobility area; the contributory principle was built to let the coordination benefit (labor mobility) proceed while keeping the redistributive commitment bounded to those who have paid into the system.
% FOUNDING_PROBLEM_CORROBORATION: Host-state governments and the federation's court/commission institutions attest the founding problem (fiscal sustainability enabling continued political support for free movement) remains live and justifies the tiering. Independent migration researchers, ombudsman reports, and litigants documented in case law attest that the actual welfare-shopping problem the tiering was built to prevent was empirically small relative to the population excluded, and that the contributory carve-out now functions largely to manage domestic political anxiety about migration rather than to solve a live fiscal problem — this corroboration comes from outside the benefiting treasuries and sending states.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, contested).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is moderate (0.52 at interval end, up from 0.34) because the arrangement genuinely solves a coordination problem — mobility without unbounded fiscal transfer — but does so by shifting risk onto a specific, identifiable subgroup (economically inactive and precariously employed mobile citizens) rather than distributing it broadly. Suppression is comparably moderate (0.48): the constraint's persistence depends on active administrative machinery (habitual residence tests, sufficient-resources checks, genuine-prospect-of-employment evidentiary requirements) that has hardened over the interval as member states have tightened enforcement in response to political pressure. Theater ratio is modest but rising (0.28) — some of the administrative apparatus (elaborate documentation requirements, repeated re-assessment) increasingly performs vigilance against 'welfare shopping' that empirical study suggests was a comparatively small phenomenon, a genealogy gap the founding_problem corroboration surfaces directly. Accessibility collapse is moderate (0.4): the formal right of free movement remains open, so alternatives are not eliminated, only the welfare dimension is tiered — this distinguishes the constraint from a pure closure mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Host-state treasuries and sending-state governments sit near the beneficiary end: they retain fiscal control and offload risk without bearing the underlying human cost directly. Employed mobile workers are beneficiaries of the same structure that burdens their inactive counterparts — the SAME contribution-based sorting mechanism produces full rights for one group and restricted rights for the other, which is the structural signature of a tangled rope rather than a clean rope: coordination (labor mobility) and asymmetric extraction (risk-shifting onto inactive/precarious mobile citizens) run through the identical mechanism. Economically inactive mobile citizens and long-term jobseekers carry high directionality toward the target end: trapped exit (having relocated, return is costly and status-losing), and the constraint's cost falls disproportionately on them. Precariously employed mobile workers are the clearest dual-positioned seat — their d oscillates with their own employment status, which is exactly the mechanism the constraint uses to sort.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (bounding fiscal exposure to preserve political sustainability of free movement) is contested rather than clearly dead — host states and federation institutions maintain it is live, while independent research suggests the scale of the problem it addresses was always smaller than the population now excluded by the resulting machinery. Classifying this as tangled_rope rather than snare avoids two mislabeling errors: treating the entire arrangement as pure extraction (which would erase the genuine coordination value to employed mobile workers and the labor market) and treating it as pure coordination/rope (which would erase the asymmetric cost borne by inactive and precarious mobile citizens, who did not consent to bear a risk that the founding rationale increasingly cannot fully justify).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contribution_record_as_neutral_sorting_variable,
    'Is contribution history a neutral, fair sorting mechanism for welfare eligibility, or does it systematically reproduce structural disadvantage (informal work histories, care responsibilities, disability, age at migration) under a formally neutral label?',
    'Empirical study of who fails contributory thresholds: if failure correlates strongly with protected characteristics or structurally involuntary circumstances (care work, disability, late-career migration) rather than with discretionary non-participation, the ''neutral contribution'' framing is doing cover-story work.',
    'If contribution history substantially proxies for structural disadvantage, the tangled_rope classification understates extraction — the arrangement would more closely resemble a snare wearing a contributory-fairness justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contribution_record_as_neutral_sorting_variable, empirical, 'Whether contribution-based sorting is a neutral mechanism or a proxy for structural disadvantage.').

omega_variable(
    welfare_shopping_scale_dispute,
    'Was the scale of ''welfare shopping'' by economically inactive mobile citizens ever large enough to justify the administrative machinery built to prevent it, or was the founding problem always smaller than the response?',
    'Comparative fiscal analysis of pre-tiering versus post-tiering welfare expenditure attributable specifically to economically inactive mobile citizens, cross-checked against independent (non-treasury-commissioned) migration research.',
    'If the founding problem was always marginal, the founding_problem_status should be read closer to ''dead'' with the machinery persisting via political theater and inertia rather than live fiscal necessity — pushing the classification toward piton-adjacent territory over time even while the coordination function is real today.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_shopping_scale_dispute, empirical, 'Whether the fiscal problem the contributory tiering was built to solve was ever proportionate to the exclusionary machinery it produced.').

omega_variable(
    kernel_reading_stability,
    'Is the selective_solidarity reading a stable equilibrium, or is it a transitional compromise that will drift toward either integration_primary (courts expanding equal-treatment interpretation) or member_sovereignty_primary (member states tightening thresholds further)?',
    'Track federation-level court rulings and legislative amendments over time: expansion of equal-treatment exceptions signals drift toward integration_primary; further tightening of habitual-residence and genuine-prospect tests signals drift toward member_sovereignty_primary.',
    'If this reading is unstable and drifting, its current tangled_rope classification is a snapshot of a moving target rather than a settled structural fact — future re-authoring would need to track which sibling reading is absorbing this one''s territory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Whether selective_solidarity is a stable third reading or a way-station between the other two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.12).
narrative_ontology:measurement(fede_tr_t4, federation_membership_obligations__selective_solidarity, theater_ratio, 4, 0.15).
narrative_ontology:measurement(fede_tr_t8, federation_membership_obligations__selective_solidarity, theater_ratio, 8, 0.18).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.21).
narrative_ontology:measurement(fede_tr_t16, federation_membership_obligations__selective_solidarity, theater_ratio, 16, 0.24).
narrative_ontology:measurement(fede_tr_t20, federation_membership_obligations__selective_solidarity, theater_ratio, 20, 0.26).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(fede_be_t4, federation_membership_obligations__selective_solidarity, base_extractiveness, 4, 0.38).
narrative_ontology:measurement(fede_be_t8, federation_membership_obligations__selective_solidarity, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.46).
narrative_ontology:measurement(fede_be_t16, federation_membership_obligations__selective_solidarity, base_extractiveness, 16, 0.48).
narrative_ontology:measurement(fede_be_t20, federation_membership_obligations__selective_solidarity, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fede_su_t4, federation_membership_obligations__selective_solidarity, suppression_requirement, 4, 0.34).
narrative_ontology:measurement(fede_su_t8, federation_membership_obligations__selective_solidarity, suppression_requirement, 8, 0.38).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.41).
narrative_ontology:measurement(fede_su_t16, federation_membership_obligations__selective_solidarity, suppression_requirement, 16, 0.44).
narrative_ontology:measurement(fede_su_t20, federation_membership_obligations__selective_solidarity, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'free movement vs. welfare state' debate under the federation_membership_obligations kernel. integration_primary treats free movement as constitutive and subordinates welfare boundaries to it (lower extraction on mobile citizens, higher pressure on member-state fiscal autonomy). member_sovereignty_primary treats member-state closure authority as primary and conditions mobility on labor-market and welfare-system protection (higher extraction on mobile citizens generally, lower pressure on member-state fiscal autonomy). selective_solidarity (this story) occupies a distinct structural position: it neither subordinates welfare to mobility nor subordinates mobility to sovereignty, but sorts individuals by contribution status, producing a bifurcated population (employed beneficiaries vs. inactive/precarious victims) that neither sibling reading produces in the same way. Each story carries its own ε and its own stakeholder set; they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(federation_membership_obligations__selective_solidarity, powerless, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
