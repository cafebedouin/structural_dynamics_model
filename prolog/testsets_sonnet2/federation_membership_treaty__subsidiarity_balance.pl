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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   domain: political/economic/federalism
 *
 * SUMMARY:
 *   This story instantiates the subsidiarity_balance reading of the
 *   federation_membership_treaty kernel: free movement is treated as a real
 *   but bounded right, checked against member-state interests through a
 *   proportionality test rather than treated as either an absolute market
 *   freedom (integration_primary) or a consent-conditioned privilege states
 *   may withdraw (sovereignty_primary). Under this reading, the constraint is
 *   genuinely a tangled rope: it coordinates a continent-wide labor market
 *   and legal predictability function while simultaneously distributing costs
 *   asymmetrically onto host-state incumbent workers, welfare systems, and
 *   posted workers who lack effective exit. The court's case-by-case
 *   balancing is the active enforcement mechanism that keeps neither side's
 *   absolutist reading from prevailing.
 *
 * KEY AGENTS:
 *   - mobile_workers_in_shortage_sectors: primary beneficiary of guaranteed access, moderate power, mobile exit
 *   - host_state_low_wage_incumbent_workers: primary target bearing wage competition, powerless, trapped exit
 *   - federation_court_authority: agenda-setter administering the proportionality test itself
 *   - member_state_governments: dual-positioned rule-writer and rule-taker
 *   - unrepresented_third_country_migrant_workers: excluded from the framework entirely
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.42).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.38).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.42).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Proportionality-Bounded Free Movement Regime").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political/economic/federalism").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, 'acd0f070-1b60-4540-8dd2-d685b7819c5e').
narrative_ontology:cs_kernel_codification('acd0f070-1b60-4540-8dd2-d685b7819c5e', fixed_text).
narrative_ontology:cs_authority_grounding('acd0f070-1b60-4540-8dd2-d685b7819c5e', lineage).
narrative_ontology:cs_interpretation_layer_present('acd0f070-1b60-4540-8dd2-d685b7819c5e').
narrative_ontology:cs_reading_relation('acd0f070-1b60-4540-8dd2-d685b7819c5e', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('acd0f070-1b60-4540-8dd2-d685b7819c5e', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('acd0f070-1b60-4540-8dd2-d685b7819c5e', foundational, proportionality_as_binding_middle_ground).
narrative_ontology:cs_axiom_status(proportionality_as_binding_middle_ground, holdable).
narrative_ontology:cs_axiom_grounding('acd0f070-1b60-4540-8dd2-d685b7819c5e', proportionality_as_binding_middle_ground, conventional).
narrative_ontology:cs_axiom('acd0f070-1b60-4540-8dd2-d685b7819c5e', foundational, legitimate_national_interest_constrains_but_does_not_negate_mobility).
narrative_ontology:cs_axiom_status(legitimate_national_interest_constrains_but_does_not_negate_mobility, holdable).
narrative_ontology:cs_axiom_grounding('acd0f070-1b60-4540-8dd2-d685b7819c5e', legitimate_national_interest_constrains_but_does_not_negate_mobility, instrumental).
narrative_ontology:cs_reference_frame('acd0f070-1b60-4540-8dd2-d685b7819c5e', graduated_proportionality_equilibrium).
narrative_ontology:cs_drift_state('acd0f070-1b60-4540-8dd2-d685b7819c5e', post_enlargement_labor_mobility_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('acd0f070-1b60-4540-8dd2-d685b7819c5e', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_workers_in_shortage_sectors).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, host_state_employers).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, sending_state_treasuries_via_remittances).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, federation_court_authority).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_low_wage_incumbent_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, host_state_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, posted_workers_facing_wage_undercutting).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_state_governments).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, proportionality_doctrine).
narrative_ontology:constraint_vindicates(federation_membership_treaty__subsidiarity_balance, subsidiarity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Cross borders freely to fill labor shortages, protected by non-discrimination rules that a host state cannot override without meeting a proportionality test. Their mobility right is real but conditioned on the host state's residual justification space, so a sufficiently well-argued restriction can still narrow their access to specific benefits or sectors.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_workers_in_shortage_sectors, beneficiary,
    moderate, biographical, mobile, continental).

% Draw on a continent-wide labor pool without needing work-permit sponsorship, lowering hiring costs and filling positions incumbent workers avoid or price too high. They lobby to keep the proportionality threshold for restrictions high.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_employers, beneficiary,
    organized, biographical, mobile, national).

% Compete directly against inbound mobile labor for the same low-wage positions and see downward wage pressure in concentrated local labor markets. They can appeal to national political channels but cannot get blanket protection because any restriction the state proposes must clear proportionality review, which usually strikes down measures broader than narrowly targeted sectoral safeguards.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_low_wage_incumbent_workers, payer,
    powerless, biographical, trapped, national).

% Must extend certain benefits to mobile citizens under equal-treatment rules after qualifying periods, even where the state's own contribution-based design assumed a closed risk pool. It can impose habitual-residence tests or waiting periods, but only if these survive proportionality scrutiny, so its capacity to protect the fiscal base of the welfare system is structurally bounded rather than eliminated.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, host_state_welfare_systems, payer,
    institutional, generational, constrained, national).

% Are posted by home-state employers to work temporarily in a host state, often at home-state wage and social-contribution levels below the host state's norms. Enforcement of host-state minimum standards exists but is patchy across sectors, and the worker's own exit option runs through the posting employer, not through the host state's labor market.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, posted_workers_facing_wage_undercutting, payer,
    powerless, immediate, trapped, continental).

% Collect remittance inflows and reduced domestic unemployment costs when workers exit to seek opportunity elsewhere in the federation. This benefit is indirect and depends on outward mobility remaining largely unrestricted.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, sending_state_treasuries_via_remittances, beneficiary,
    institutional, generational, analytical, national).

% Adjudicates whether a given national restriction is a proportionate, non-discriminatory pursuit of a legitimate interest or an unjustified barrier. Its case-by-case balancing test is the actual mechanism that draws the constraint's boundary; it neither guarantees unrestricted movement nor blanket sovereignty, and its rulings accumulate into a body of doctrine that member states must anticipate.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_court_authority, agenda_setter,
    institutional, civilizational, analytical, continental).

% Retain the formal authority to legislate restrictions on public-policy, public-security, or public-health grounds, and to design welfare eligibility rules, but must draft every measure to survive proportionality review or risk it being struck down and the political cost that follows. They are simultaneously rule-writers and rule-takers under this regime.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_state_governments, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(federation_membership_treaty__subsidiarity_balance, member_state_governments, payer).

% Are not covered by the federation's internal free-movement guarantees at all, so the proportionality balance that protects federation citizens does not extend to them; they compete in the same low-wage labor markets without the mobility right or the equal-treatment protections that structure this constraint.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, unrepresented_third_country_migrant_workers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership_treaty__subsidiarity_balance, diffuse).
narrative_ontology:fixing_cost_class(federation_membership_treaty__subsidiarity_balance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable rule for when a member state may restrict a federation citizen's mobility or access to services: restrictions must be non-discriminatory, pursue a legitimate interest, and be no more restrictive than necessary. This lets workers, employers, and states plan around a known legal test instead of a patchwork of unilateral national vetoes.
% TRANSFER_FUNCTION: Moves labor-market access and welfare-system exposure from host-state incumbents and welfare systems toward mobile workers and the employers who hire them; moves adjudicatory authority over the boundary from national legislatures toward the federation court.
% ABSENT_VOICES: Third-country migrant workers are structurally outside this framework entirely and have no standing in the proportionality test that governs federation citizens; posted workers' home-state unions are often not party to host-state wage-floor enforcement discussions even though undercutting affects host-state incumbents most directly.
% DISAPPEARANCE_RATIONALE: If the proportionality framework vanished overnight, member states would either revert to unilateral quota and permit systems (sovereignty_primary drift) or the treaty's free-movement guarantee would become unconditional and unreviewable (integration_primary drift). Either direction reorganizes labor markets, welfare eligibility rules, and the court's docket; employers who rely on frictionless cross-border hiring and incumbent workers who rely on the residual national veto space would both experience an abrupt change in their bargaining position.
% FOUNDING_PROBLEM: Early federation treaties needed a rule that let free movement function as a genuine single-market feature without requiring member states to fully surrender control over their labor markets and welfare systems, given large disparities in wage levels and social-benefit design among members.
% FOUNDING_PROBLEM_CORROBORATION: Independent labor economists studying wage effects in border regions and national ombudsman offices handling posted-worker complaints both attest that the underlying tension — cross-border wage and welfare-system disparity — remains active decades into the treaty's operation; this is not merely asserted by the court or by employer associations who benefit from the current balance.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at a moderate 0.42 because the proportionality test genuinely limits how far any single actor can extract from another — a host state cannot impose a blanket ban, and mobile workers cannot claim unconditional access to every welfare benefit from day one. Suppression sits lower still (0.38) because the constraint's persistence rests on case-law adjudication and treaty ratification rather than raw coercive enforcement, though it is real: states that ignore adverse rulings face infringement proceedings. Accessibility collapse is moderate (0.35) — both unrestricted movement and blanket restriction remain formally foreclosed as extremes, but the graduated middle ground the court occupies is itself narrow and technical, collapsing lay alternatives to litigation. Resistance is comparatively high (0.55) because both incumbent-worker groups and integration-maximalist advocates continue to contest the boundary the court draws, keeping the doctrine under permanent political and legal pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the federation court's seat, this is coordination: a stable rule that lets a genuinely single labor market function without abolishing national policy space. From a host-state incumbent worker's seat, the same rule looks like enforced exposure to wage competition they have no political lever to fully block, because any state-level protective measure must clear a proportionality bar set by an institution they do not elect. The engine should register this as seat divergence rather than resolving it — that divergence IS the tangled-rope signature.
 *
 * DIRECTIONALITY LOGIC:
 *   Mobile workers and the employers who hire them sit near the beneficiary end: they get guaranteed, litigable access with only residual national exceptions. Host-state incumbent workers and welfare systems sit near the target end: they absorb wage and fiscal pressure and can only push back through a legal channel (proportionality litigation) that is deliberately calibrated to let genuine national interests through only in narrow, justified slices. Posted workers occupy an unusual position — nominally protected by the same free-movement logic, but practically closer to trapped, since their exit runs through their sending-state employer rather than through the host labor market directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reconciling single-market mobility with disparate national welfare and labor-market designs) remains live by the corroboration of independent economists and ombudsman offices, not merely the court's own account of its docket relevance. This distinguishes the constraint from mandatrophy: it has not persisted past its function, the function is still being actively litigated and recalibrated. Classifying it as tangled_rope rather than snare prevents mislabeling a genuine (if asymmetric) coordination structure as pure extraction; classifying it as tangled_rope rather than rope prevents ignoring the real, identifiable victims among incumbent and posted workers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_stability,
    'Does the federation court''s proportionality test represent a stable equilibrium, or is it drifting toward one of the two poles (near-unconditional mobility or near-unconditional state discretion) over successive rulings?',
    'Longitudinal coding of court rulings on free-movement restrictions over multiple decades, tracking the success rate of state justifications and the scope of exceptions actually upheld.',
    'If drifting toward integration_primary, this reading''s extraction profile would converge toward the low-ε mountain/rope end; if drifting toward sovereignty_primary, victim exposure for mobile workers would rise and the beneficiary set would shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_stability, empirical, 'Whether the subsidiarity balance is a stable middle reading or a transitional state migrating toward a sibling reading.').

omega_variable(
    who_bears_proportionality_litigation_cost,
    'Given that the proportionality test is adjudicated through litigation, does the practical cost of invoking the test fall disproportionately on the parties least able to bear it (individual posted workers, small host-state municipalities) compared to well-resourced employer associations and national governments?',
    'Comparative study of case filings by claimant type and resourcing, and settlement/withdrawal rates before judgment.',
    'If litigation cost is itself asymmetric, the constraint''s formal proportionality protection may be substantially less accessible to its nominal beneficiaries among posted and incumbent workers than the doctrine implies, raising effective suppression above the authored 0.38.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(who_bears_proportionality_litigation_cost, empirical, 'Whether access to the proportionality remedy is itself unequally distributed.').

omega_variable(
    kernel_framing_choice_under_determination,
    'Is the subsidiarity_balance framing itself a neutral description of the treaty''s operation, or is it the framing preferred by the federation court and centrist member-state governments precisely because it preserves their joint adjudicatory authority against both fully mobile labor advocates and fully sovereigntist states?',
    'Compare institutional position statements: does the court''s own jurisprudential self-description align with subsidiarity_balance more than with integration_primary or sovereignty_primary, and does that alignment correlate with the court''s own authority being maximized under this framing?',
    'If the subsidiarity_balance framing is itself the framing that most preserves the federation_court_authority''s institutional role, this reading''s claimed_type of tangled_rope should note the court itself as a beneficiary of the very doctrinal structure it administers — which is already reflected in the beneficiaries list, but the causal direction (does the court choose this framing because it benefits, or does it benefit because the framing is correct) remains open.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_under_determination, conceptual, 'Whether the subsidiarity_balance reading is authored from a genuinely independent vantage or reflects the interests of the institution most empowered by adjudicating it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t8, federation_membership_treaty__subsidiarity_balance, theater_ratio, 8, 0.21).
narrative_ontology:measurement(fede_tr_t16, federation_membership_treaty__subsidiarity_balance, theater_ratio, 16, 0.23).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__subsidiarity_balance, theater_ratio, 24, 0.25).
narrative_ontology:measurement(fede_tr_t32, federation_membership_treaty__subsidiarity_balance, theater_ratio, 32, 0.27).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(fede_be_t8, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 8, 0.33).
narrative_ontology:measurement(fede_be_t16, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(fede_be_t32, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(fede_su_t8, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 8, 0.32).
narrative_ontology:measurement(fede_su_t16, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 16, 0.34).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 24, 0.36).
narrative_ontology:measurement(fede_su_t32, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 32, 0.37).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the federation_membership_treaty kernel. integration_primary authors near-zero extraction for the standing free-movement arrangement (restrictions are the anomaly requiring justification); sovereignty_primary authors substantial extraction from mobile workers and sending states (state consent is the baseline, movement is the conditional grant); subsidiarity_balance (this story) authors moderate, graduated extraction distributed across both incumbent and mobile populations depending on policy domain, with the court's proportionality test as the active balancing mechanism. All three share the same treaty text as their kernel but diverge in claimed_type, ε, and beneficiary/victim sets because each reading treats a different party's baseline as the reference point.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
