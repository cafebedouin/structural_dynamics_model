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
    narrative_ontology:constraint_stakeholder/7,
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
 *   domain: political economy / federalism / migration policy / welfare state theory
 *
 * SUMMARY:
 *   This story instantiates the 'selective_solidarity' reading of the
 *   federation_membership_obligations kernel: free movement rights and
 *   welfare access are governed not by citizenship as such, nor by pure
 *   member-state closure authority, but by a contribution/activity-status
 *   gate applied uniformly across all mobile citizens. Employed movers
 *   receive full parity with nationals; economically inactive movers face
 *   residence tests, means assessments, and potential loss of entitlement.
 *   This reading treats the contributory principle as the operative logic of
 *   the regime as it actually functions today — a compromise structure
 *   distinct from both the integration-primary reading (which would treat
 *   mobility rights as trumping welfare boundaries) and the
 *   member-sovereignty-primary reading (which would treat welfare closure as
 *   the default, conditional exception being movement).
 *
 * KEY AGENTS:
 *   - employed_mobile_workers: primary beneficiary of the contributory gate — full parity while active
 *   - economically_inactive_mobile_citizens: primary payer — formal right without substantive access
 *   - host_state_treasuries: agenda-setter administering and enforcing the eligibility tests
 *   - sending_state_governments: excluded from host-state rulemaking despite bearing displaced costs
 *   - federal_court_and_commission: analytical observer adjudicating the contribution/activity boundary case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_obligations__selective_solidarity, 0.58).
domain_priors:suppression_score(federation_membership_obligations__selective_solidarity, 0.52).
domain_priors:theater_ratio(federation_membership_obligations__selective_solidarity, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, extractiveness, 0.58).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(federation_membership_obligations__selective_solidarity, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_obligations__selective_solidarity, tangled_rope).
narrative_ontology:human_readable(federation_membership_obligations__selective_solidarity, "Contribution-Tiered Free Movement and Welfare Access (Selective Solidarity Reading)").
narrative_ontology:topic_domain(federation_membership_obligations__selective_solidarity, "political economy / federalism / migration policy / welfare state theory").

domain_priors:requires_active_enforcement(federation_membership_obligations__selective_solidarity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_obligations__selective_solidarity, '7800da30-a0f4-4f3f-abd1-0fa44381ad0f').
narrative_ontology:cs_kernel_codification('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', formalized).
narrative_ontology:cs_authority_grounding('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', lineage).
narrative_ontology:cs_interpretation_layer_present('7800da30-a0f4-4f3f-abd1-0fa44381ad0f').
narrative_ontology:cs_reading_relation('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', federation_membership_obligations__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', federation_membership_obligations__member_sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', foundational, entitlement_tracks_contribution_not_status).
narrative_ontology:cs_axiom_status(entitlement_tracks_contribution_not_status, holdable).
narrative_ontology:cs_axiom_grounding('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', entitlement_tracks_contribution_not_status, conventional).
narrative_ontology:cs_axiom('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', secondary, activity_status_is_legitimate_differentiator).
narrative_ontology:cs_axiom_status(activity_status_is_legitimate_differentiator, holdable).
narrative_ontology:cs_axiom_grounding('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', activity_status_is_legitimate_differentiator, instrumental).
narrative_ontology:cs_reference_frame('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', worker_based_free_movement_founding_treaty).
narrative_ontology:cs_drift_state('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', post_citizenship_directive_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7800da30-a0f4-4f3f-abd1-0fa44381ad0f', '').
narrative_ontology:cs_kernel_id(federation_membership_obligations__selective_solidarity, federation_membership_obligations).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, host_state_treasuries).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, employed_mobile_workers).
narrative_ontology:constraint_beneficiary(federation_membership_obligations__selective_solidarity, national_welfare_constituencies).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, long_term_unemployed_migrants).
narrative_ontology:constraint_victim(federation_membership_obligations__selective_solidarity, mobile_workers_with_broken_contribution_records).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, contributory_principle_of_welfare_entitlement).
narrative_ontology:constraint_vindicates(federation_membership_obligations__selective_solidarity, activity_based_membership_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Workers who move to another member state and hold jobs or recent work records receive full access to residence rights and welfare benefits on the same basis as nationals. Their entitlement is secured by their contribution record, not their citizenship, so as long as they remain economically active they experience free movement as functioning smoothly.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, employed_mobile_workers, beneficiary,
    moderate, biographical, mobile, continental).

% Citizens of one member state residing in another without work, sufficient resources, or recent contribution history — students without means, early retirees, jobseekers past the initial search period, people caring for family. They can be denied means-tested welfare, subjected to residence reviews, or in some regimes face removal for becoming an 'unreasonable burden.' Their formal free-movement right exists but its substantive content (welfare access) is withheld based on activity status.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, economically_inactive_mobile_citizens, payer,
    powerless, biographical, constrained, continental).

% Migrants whose contribution record lapses after job loss face benefit sanctions, loss of residence status, or reversion to a precarious jobseeker classification. They are structurally caught: returning home may mean losing accrued pension contributions or facing worse domestic labor markets, while staying means navigating administrative gatekeeping without income support.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, long_term_unemployed_migrants, payer,
    powerless, biographical, trapped, continental).

% Workers with fragmented careers across multiple member states (gig work, seasonal labor, career breaks for caregiving) accumulate contribution histories that fall below thresholds in any single system, even though their aggregate lifetime contribution may be substantial. Aggregation rules exist but are administratively complex and unevenly applied, leaving these workers with degraded entitlement despite genuine economic participation.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, mobile_workers_with_broken_contribution_records, payer,
    moderate, biographical, constrained, continental).

% National welfare administrations design and enforce the habitual-residence and right-to-reside tests that gate benefit access. They collect political credit for 'protecting the welfare system from abuse' while receiving the labor-market and tax contributions of employed mobile workers without the offsetting cost of supporting inactive arrivals. They administer eligibility reviews and can tighten or loosen the contribution threshold.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, host_state_treasuries, beneficiary,
    institutional, generational, arbitrage, national).

% Domestic voters and welfare-dependent citizens benefit from the contributory gate because it limits perceived competition for a fixed welfare pool from newly arrived non-contributors, which sustains political support for the free movement regime overall by making it politically survivable.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, national_welfare_constituencies, beneficiary,
    organized, generational, arbitrage, national).

% Member states whose citizens emigrate for work bear the cost of educating and initially supporting workers who then contribute to another state's welfare and tax base, and whose returning long-term-unemployed citizens re-enter domestic welfare rolls without having built entitlement abroad. They have limited voice in host-state eligibility rulemaking despite bearing displaced costs.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, sending_state_governments, excluded,
    institutional, generational, constrained, continental).

% Adjudicates disputes over what counts as sufficient contribution, genuine work-seeking, or 'unreasonable burden,' shaping the boundary case by case. Sees the full pattern of tiering across member states and can push doctrine toward either tighter or looser contributory gating.
narrative_ontology:constraint_stakeholder(federation_membership_obligations__selective_solidarity, federal_court_and_commission, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of sustaining free movement politically: without some mechanism to prevent free-riding on host-state welfare systems by non-contributors, domestic constituencies would likely revolt against free movement itself, collapsing the broader mobility regime for everyone including contributors.
% TRANSFER_FUNCTION: Moves welfare eligibility and residence security from citizenship-based universal entitlement to activity-based earned entitlement — economically active movers receive what nationals receive, while inactive movers are shifted the cost of their own support (or pushed back toward their state of origin), and sending states absorb re-entry costs for returning non-contributors.
% ABSENT_VOICES: Sending-state governments and the economically inactive movers themselves have little say in how host states define 'sufficient resources' or 'genuine work-seeking' — these thresholds are set unilaterally by host administrations and reviewed by courts that weigh treaty-level mobility rights against national welfare autonomy, not by the affected movers directly.
% DISAPPEARANCE_RATIONALE: If contribution-based tiering vanished and full citizenship-equivalent welfare access applied to every mobile person regardless of activity status, host states would face acute pressure to either restrict free movement itself or fundamentally reform welfare financing; conversely if all tiering including basic residence conditions vanished into pure exclusion, mobile workers would lose the protections that make cross-border labor markets function. The tiering is load-bearing for the political sustainability of the wider free-movement architecture.
% FOUNDING_PROBLEM: Free movement of workers was designed to build an integrated labor market, but extending it to full welfare-state membership regardless of economic participation created acute political exposure — the 'welfare tourism' narrative threatened public support for free movement as such, especially in high-benefit host states.
% FOUNDING_PROBLEM_CORROBORATION: Host-state governments and welfare ministries attest the contributory principle remains necessary to preserve public support for free movement. Independent labor economists and migration researchers outside the host-state administrations note that empirical 'welfare tourism' rates are consistently low, suggesting the political problem the tiering solves is more about perceived threat and domestic electoral pressure than actual fiscal strain — corroboration for the founding problem's continued vitality comes almost entirely from the benefiting host administrations themselves.
narrative_ontology:disappearance_verdict(federation_membership_obligations__selective_solidarity, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_obligations__selective_solidarity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_obligations__selective_solidarity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(federation_membership_obligations__selective_solidarity, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_obligations__selective_solidarity, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is moderate-to-substantial (0.58 by interval end) because the tiering does not simply exclude non-contributors from a benefit they never had a claim to — it withdraws or withholds welfare access from citizens who hold a formal free-movement right, redirecting the cost of their support back onto themselves or onto sending states. Suppression is moderate (0.52): the mechanism operates mainly through administrative gatekeeping (residence tests, resource assessments) rather than coercive force, but the gatekeeping is backed by removal power in some regimes, which is real suppression. Theater ratio (0.31) reflects that a meaningful share of enforcement activity — habitual residence interviews, documentation requirements — serves political signaling ('protecting the system') beyond its administrative necessity, though the underlying eligibility function is real.
 *
 * PERSPECTIVAL GAP:
 *   From the employed-mobile-worker and host-treasury seats, the arrangement reads as coordinated fairness — contribute and you're treated like anyone else. From the economically-inactive and broken-record-worker seats, the identical rule structure reads as a right that exists on paper but is withdrawn precisely when needed most (job loss, career gaps, caregiving interruptions). The engine computes these as different seat-level classifications from the same structural data; the claimed_type (tangled_rope) reflects the analytical judgment that both readings are simultaneously true of the underlying arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Employed mobile workers and host-state treasuries/constituencies sit near the beneficiary end: workers get parity while active, and treasuries/constituencies get labor and tax contribution without matched welfare liability for the inactive. Economically inactive mobile citizens, long-term unemployed migrants, and workers with broken contribution records sit toward the target end: the same structure that grants full rights to the active withholds them from the inactive, and exit is constrained because return to the home state often forfeits accrued (but not yet vested) entitlements. Sending-state governments are excluded rather than coordinated — they bear cost without voice in the rules that generate it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sustaining political support for free movement against a welfare-tourism backlash) is contested rather than resolved: independent labor-market data suggests actual non-contributory welfare uptake by mobile citizens is low, meaning the tiering may now be defending against a largely perceived rather than actual fiscal threat. This is precisely the divergence a Tangled Rope classification is built to hold: there IS a genuine coordination function (sustaining the political viability of free movement as a whole) bundled with genuine asymmetric extraction (inactive movers and broken-record workers absorbing costs disproportionate to any actual system risk they pose) — collapsing this into either pure Rope (ignoring the victims) or pure Snare (ignoring the coordination logic that keeps the broader mobility regime alive) would mislabel the structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contribution_gate_vs_citizenship_gate,
    'Is the contributory principle genuinely distinct from a disguised nationality-based gate, given that host-state citizens rarely face equivalent activity-status review for their own welfare entitlement?',
    'Compare enforcement intensity and threshold strictness applied to mobile EU citizens under residence/resource tests versus the (near-absent) equivalent scrutiny applied to host-state nationals with similarly thin contribution records.',
    'If host nationals with equally thin contribution records face negligible equivalent scrutiny, the contributory principle functions asymmetrically as a nationality-proxy gate, which would push this reading structurally closer to member_sovereignty_primary than its own framing claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contribution_gate_vs_citizenship_gate, empirical, 'Whether contribution-based gating is nationality-neutral in practice or a proxy for it.').

omega_variable(
    aggregation_rule_functionality,
    'Do the cross-border contribution aggregation mechanisms (which are supposed to let fragmented careers count toward entitlement) function well enough in practice to make the contributory principle fair to genuinely mobile, precarious workers?',
    'Administrative data on aggregation claim approval/denial rates and processing times across member states, compared against a benchmark of single-state career contribution recognition rates.',
    'If aggregation is administratively unreliable, the contributory principle''s fairness claim (earn it anywhere, keep it everywhere) is largely theatrical, raising the effective extractiveness experienced by mobile precarious workers beyond what the formal rule implies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregation_rule_functionality, empirical, 'Whether cross-border contribution aggregation delivers on its coordination promise.').

omega_variable(
    reading_choice_under_determination,
    'Could the same body of case law and welfare-eligibility rules be equally well described under the member_sovereignty_primary reading (contribution as one national safeguard among several) rather than as a distinct ''selective solidarity'' principle?',
    'Compare which reading better predicts case outcomes at the margin: does the doctrine track contribution status consistently even where it conflicts with member-state preference for restriction, or does it collapse into member-state discretion whenever politically salient?',
    'If the doctrine reliably yields to member-state restriction pressure at the margins, this reading is less structurally distinct from member_sovereignty_primary than presented, and the two constraints may need tighter coupling in the network graph or a revised axiom set.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_choice_under_determination, conceptual, 'Whether selective_solidarity is a structurally distinct reading or a variant framing of member sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_obligations__selective_solidarity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_obligations__selective_solidarity, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fede_tr_t6, federation_membership_obligations__selective_solidarity, theater_ratio, 6, 0.21).
narrative_ontology:measurement(fede_tr_t12, federation_membership_obligations__selective_solidarity, theater_ratio, 12, 0.24).
narrative_ontology:measurement(fede_tr_t18, federation_membership_obligations__selective_solidarity, theater_ratio, 18, 0.27).
narrative_ontology:measurement(fede_tr_t24, federation_membership_obligations__selective_solidarity, theater_ratio, 24, 0.29).
narrative_ontology:measurement(fede_tr_t30, federation_membership_obligations__selective_solidarity, theater_ratio, 30, 0.31).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_obligations__selective_solidarity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fede_be_t6, federation_membership_obligations__selective_solidarity, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(fede_be_t12, federation_membership_obligations__selective_solidarity, base_extractiveness, 12, 0.49).
narrative_ontology:measurement(fede_be_t18, federation_membership_obligations__selective_solidarity, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(fede_be_t24, federation_membership_obligations__selective_solidarity, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(fede_be_t30, federation_membership_obligations__selective_solidarity, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_obligations__selective_solidarity, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(fede_su_t6, federation_membership_obligations__selective_solidarity, suppression_requirement, 6, 0.39).
narrative_ontology:measurement(fede_su_t12, federation_membership_obligations__selective_solidarity, suppression_requirement, 12, 0.44).
narrative_ontology:measurement(fede_su_t18, federation_membership_obligations__selective_solidarity, suppression_requirement, 18, 0.47).
narrative_ontology:measurement(fede_su_t24, federation_membership_obligations__selective_solidarity, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(fede_su_t30, federation_membership_obligations__selective_solidarity, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_obligations__selective_solidarity, resource_allocation).
narrative_ontology:boltzmann_floor_override(federation_membership_obligations__selective_solidarity, 0.12).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__integration_primary).
narrative_ontology:affects_constraint(federation_membership_obligations__selective_solidarity, federation_membership_obligations__member_sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language 'free movement and welfare access' debate under the federation_membership_obligations kernel, per the ε-invariance principle. integration_primary evaluates the same underlying arrangement by the lights of a reading that treats mobility as constitutive and finds high extraction in any welfare gating; member_sovereignty_primary evaluates it by the lights of a reading that treats national closure as default and finds low extraction in the same gating (or reads it as insufficiently protective); this reading (selective_solidarity) evaluates the arrangement as it structurally is — bifurcated by contribution/activity status — and finds a hybrid coordination/extraction structure. All three share the same underlying institutional facts but author different ε and different classifications because they hold different normative premises about what counts as legitimate closure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
