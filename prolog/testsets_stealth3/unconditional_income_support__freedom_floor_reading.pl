% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Universal Unconditional Income Floor (Freedom-Floor Reading)
 *   domain: political economy/social policy/welfare state
 *
 * SUMMARY:
 *   A universal, individual, unconditional periodic payment to every
 *   resident, funded by a progressively weighted tax base: this story
 *   instantiates the freedom-floor reading of the
 *   unconditional_income_support kernel, in which the payment is an
 *   autonomy-enabling floor that removes desperation from labor-market
 *   bargaining, eliminates welfare stigma by construction, and insures
 *   against market shocks. The colloquial label covers structurally distinct
 *   claims; per the epsilon-invariance principle this file authors ONE
 *   reading with one stable epsilon, and the sibling readings
 *   (dependency_trap_reading, universality_paradox_reading) are separate
 *   files linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - national_fiscal_authority: agenda setter
 *   (institutional/constrained) — funds, administers, sets the level; cannot
 *   retarget without dissolving the defining property - precarious_workers:
 *   primary beneficiary (moderate/constrained) — the floor prices desperation
 *   out of their bargaining - unpaid_caregivers: primary beneficiary
 *   (powerless/trapped) — only personal income, zero assessment burden -
 *   independent_artists: beneficiary (moderate/constrained) — subsistence
 *   between uncertain payoffs - abuse_victims_exiting_relationships:
 *   beneficiary in crisis (powerless/trapped, immediate horizon) — automatic
 *   individual payment as exit resource - small_business_owners: incidental
 *   beneficiary and contributor (organized/mobile) - rental_property_owners:
 *   incidental beneficiary via price pass-through (organized/arbitrage) -
 *   high_income_net_contributors: principal net contributor
 *   (powerful/arbitrage, global scope) - future_taxpayers: excluded seat
 *   (powerless/trapped) — inherits unfunded promises -
 *   comparative_policy_researchers: analytical observer — owns the evidence
 *   base both sides cite
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.26).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.18).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.26).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Universal Unconditional Income Floor (Freedom-Floor Reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political economy/social policy/welfare state").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d').
narrative_ontology:cs_kernel_codification('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', formalized).
narrative_ontology:cs_authority_grounding('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', expertise).
narrative_ontology:cs_interpretation_layer_present('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d').
narrative_ontology:cs_reading_relation('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', foundational, unconditionality_is_autonomy_precondition).
narrative_ontology:cs_axiom_status(unconditionality_is_autonomy_precondition, holdable).
narrative_ontology:cs_axiom_grounding('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', unconditionality_is_autonomy_precondition, deontological).
narrative_ontology:cs_axiom('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', foundational, universality_eliminate_takeup_stigma).
narrative_ontology:cs_axiom_status(universality_eliminate_takeup_stigma, holdable).
narrative_ontology:cs_axiom_grounding('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', universality_eliminate_takeup_stigma, empirically_contingent).
narrative_ontology:cs_reference_frame('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', autonomy_preserving_universal_floor).
narrative_ontology:cs_drift_state('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', contemporary_partial_implementation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f28e1405-49fb-4bb2-9b5b-f2fcb0cbdf1d', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, independent_artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims_exiting_relationships).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, small_business_owners).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, rental_property_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, small_business_owners).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, high_income_net_contributors).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, pilot_labor_supply_neutrality).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, stigma_as_targeting_artifact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Levies the taxes that fund the payment, operates the enrollment and disbursement infrastructure, and sets the benefit level through the annual budget process. Receives political credit for the program's popularity and carries the fiscal exposure when revenues fall. Once the payment is universal and individual, it cannot retarget or withdraw it without dissolving the arrangement's defining property, so its practical room to maneuver is narrower than its formal authority suggests.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, national_fiscal_authority, agenda_setter,
    institutional, generational, constrained, national).

% Work in hourly, gig, and short-contract jobs where wages and conditions are set under time pressure. Receive the same payment as everyone else and contribute taxes on earnings; for most in this group the payment exceeds their tax contribution. The payment gives them the option to refuse an abusive shift, wait out a bad job market, or retrain without losing housing. Leaving the tax-and-transfer system itself is not a realistic option.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, precarious_workers, payer).

% Raise children or care for elderly and disabled relatives full-time without payroll income. The payment is often their only personal money, and it arrives without applications, home visits, or proof of job search. Their care obligations tie them to specific households and locations, so their mobility is limited regardless of income.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, trapped, national).

% Produce work with long, uncertain payoff horizons: music, writing, visual art, independent research. The payment covers subsistence between commissions and lets them decline commercial work that would consume their practice. They pay taxes on occasional income and could, under enough pressure, take conventional employment; the floor is what makes declining that pressure sustainable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, independent_artists, beneficiary,
    moderate, biographical, constrained, national).

% Need money of their own, quickly and quietly, to leave a controlling partner. A payment that arrives automatically in their own name, with no application a partner could intercept or sabotage, functions as an exit resource. Their immediate-timeframe needs and continued entanglement with the household they are leaving keep them the least mobile group in the arrangement.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims_exiting_relationships, beneficiary,
    powerless, immediate, trapped, national).

% Run shops, farms, and trades selling into local demand. The payment raises their customers' baseline spending and lets them hire seasonally without becoming anyone's only lifeline. They also pay the taxes that fund it and lobby over the rate; unlike employees, they can restructure, relocate, or shift income between forms.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, small_business_owners, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, small_business_owners, payer).

% Own the housing stock that payment recipients rent. Where housing supply is tight, part of any broad cash increase surfaces as rent rather than tenant consumption. They did not design the arrangement and gain nothing from administering it, but they can reposition assets, adjust rents, and convert units faster than tenants can move.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, rental_property_owners, beneficiary,
    organized, biographical, arbitrage, national).

% Pay substantially more in taxes than they receive back in payments. Under this arrangement that net contribution is framed as the price of living in a jurisdiction where no one's labor is secured by desperation, a framing they may or may not accept. Their capital, credentials, and residency options span borders, so sustained perceived overpayment leaks out through tax migration and income structuring rather than open revolt.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, high_income_net_contributors, payer,
    powerful, generational, arbitrage, global).

% Will inherit whatever gap remains between promised payments and funded reserves in jurisdictions that finance the floor partly with debt. They attend no budget negotiation, hold no seat in the pilot evaluations, and cannot decline the fiscal legacy; their only present representatives are actuaries and opposition parties speaking on their behalf.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Run and evaluate the pilots, compare dividend and transfer programs across countries, and publish the labor-supply, wellbeing, and takeup findings that both defenders and opponents of the floor cite. They collect no payment from the arrangement and bear none of its costs; their stake is the integrity of the evidence base.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, comparative_policy_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__freedom_floor_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a baseline-security problem: it establishes, once and centrally, a subsistence income every resident holds as of right, so that survival is not indexed to accepting any particular job, staying in any particular household, or performing eligibility rituals. Universality is the coordination device: one rule, no assessments, no stigma differential, replacing a patchwork of means-tested programs each with its own bureaucracy and takeup penalty.
% TRANSFER_FUNCTION: Moves purchasing power from the general tax base, progressively weighted so net contributions concentrate among high earners, to every resident in equal, individual, unconditional payments; net, it transfers from net contributors to net recipients while paying everyone, and converts categorical welfare budgets into a single universal line.
% ABSENT_VOICES: Future taxpayers bear debt-financed variants without a seat (authored as excluded). Residents of poorer jurisdictions excluded by national borders would object that the floor is a national club good. Caseworkers and administrators of the means-tested programs the floor would absorb hold expertise the transition decommissions. Recipients of deeper targeted benefits would object if universality arrived at the cost of their supplement levels.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip caregivers, people leaving controlling households, and between-jobs workers of their only unconditional income; means-tested rolls would swell within months; low-wage bargaining would revert to take-it-or-leave-it terms; dividend-style economies would see immediate consumption contraction concentrated in exactly the households with the highest marginal propensity to spend.
% FOUNDING_PROBLEM: Industrial and post-industrial economies bind survival to employment, and the welfare states built to patch this used means-tested aid that stigmatizes takeup, traps recipients behind benefit cliffs, and systematically misses caregivers, informal workers, and people between jobs. The floor was built to guarantee subsistence without conditions, assessments, or stigma.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary coalition: OECD and ILO coverage-gap statistics attest that means-tested systems miss caregivers, informal workers, and the between-jobs; employer federations attest recruitment and retention problems tied to desperation-driven labor mismatch; and fiscal-conservative institutes that oppose the remedy nonetheless concede the underlying precarity in their own analyses. No outside party attests that the floor as designed fully solves the problem; that remains this reading's assertion.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.26, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-low (0.26 at interval end) because the arrangement's real cost is fiscal and progressive: it extracts purchasing power from net contributors, but the reading holds there is no behavioral extraction channel (labor-supply effects minimal per the Alaska/Kenya record) and no suppressed alternative. The temporal series shows gentle accumulation (0.12 to 0.26) as proposals scale from oil-funded dividends and donor-funded pilots toward tax-funded universality: delivery costs amortize with digitization while fiscal weight grows with coverage and aging. Suppression is low (0.18): the only compulsion is the general tax obligation shared with every public expenditure; receipt is passive and cannot be opted out of, but nothing enforces participation in any activity. Mechanistically the suppression is structural, not internalized, and the arrangement's distinctive move is removing the structural triggers (applications, assessments, categories) that generated internalized stigma under means-tested regimes; the residue fades with cohort turnover rather than being suppressed. Theater is low (0.14): the payments are the function; the slow rise tracks demonstration-project framing and branding rather than any growth in performative administration. Accessibility_collapse is moderate (0.38): workable alternatives persist (negative income tax, refined targeted aid, job guarantees) but the stigma-elimination property is hard for targeted designs to replicate, so the alternative space partially collapsed around the universal form. Resistance is moderate (0.42): fiscal-conservative opposition and taxpayer associations are real and organized, yet implemented versions enjoy broad approval. Claim and metrics are independent authored facts: the rope claim follows from the reading's structure (participants net beneficiaries, alternatives unsuppressed, coordination enabling voluntary participation), while the metrics describe observed operation; the engine computes per-seat types and any divergence is the datum. The coercion grid tells the same account at level resolution: individual and class coercive pressure falls sharply (desperation pricing out of bargaining), structural-level pressure is stable-to-mildly-rising (tax compulsion replaces market compulsion as the binding form), and organizational-level movement is mild. Rising fiscal weight alongside falling lived coercion is coherent, not contradictory: they are different observables of one arrangement maturing. Trajectories are monotonic; no cyclical dynamics, so no intermittent-reinforcement concern arises.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the beneficiary seats (caregivers, abuse victims, precarious workers) the arrangement is experienced as option-opening: the same payment that is trivial at the median is existential at the bottom decile. From the high_income_net_contributors seat the identical structure is a permanent net levy whose legitimating story they did not write; their exit is arbitrage (migration, structuring), not refusal, so their resistance registers as leakage rather than contest. From the national_fiscal_authority seat the arrangement is a maintenance burden with political upside: it cannot retarget without breaking universality, so its discretion is narrower than its formal power. Inter-institutionally, business associations experience the funding side while their members experience the demand side, splitting the organized seat's own alignment. Same-level lateral texture: precarious workers and independent artists sit at the same nominal beneficiary position with different exit textures — the worker's constraint is wage subordination, the artist's is income volatility — which is why both are authored constrained rather than trapped or mobile. No seat is identity_locked: the nearest case is caregivers, whose trapping is relational obligation rather than identity fusion, and the classification would not change if that frame broke.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the six beneficiary groups; the victims array is intentionally EMPTY because this reading claims a Pareto structure — no seat is authored as harmed. That emptiness is exactly why two overrides are needed. First, powerful (high_income_net_contributors): the structural derivation cannot see their net-payer position because the reading declines to declare victims, and the canonical fallback would misplace them near the beneficiary end merely because they receive the dividend too; 0.7 encodes substantial-target position tempered by dividend receipt, insurance value, and the systemic benefits they consume. Second, institutional (national_fiscal_authority): the authority is a conduit, not a collector — it administers the flow, absorbs fiscal risk, and gains legitimacy, which nets to roughly symmetric exposure; 0.5 prevents a fallback that would read administration as benefit. Rental_property_owners derive low directionality from their beneficiary declaration, correctly reflecting incidental rather than sponsored benefit; whether their capture grows is tracked by the rent_capture_leakage omega rather than pre-decided here.
 *
 * MANDATROPHY ANALYSIS:
 *   The rope claim protects the genuine coordination content — stigma-free universality, shock absorption, exit finance for caregivers and people leaving controlling households — from being flattened into pure fiscal extraction by the incentive-distortion critique; the coordination function (one rule replacing a patchwork of stigmatizing assessments) is real and load-bearing. Symmetrically, the omegas are the tripwires that would expose coordination decaying into asymmetric extraction: net_payer_seat_visibility tests the Pareto claim against lifecycle incidence, rent_capture_leakage tests whether the gains stay diffuse, labor_supply_scale_neutrality tests whether the voluntary-participation premise survives scale, and universality_durability_under_fiscal_stress tests whether the defining property survives its first real revenue shock. Mandatrophy is not declared: the founding problem is live and corroborated from outside the beneficiary coalition, so the arrangement has not outlived its function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_index,
    'This story instantiates the freedom_floor_reading of the unconditional_income_support kernel; would the same statutory arrangement, read through the dependency_trap_reading, yield a different victim set, a higher epsilon, and a different type?',
    'Cross-reading comparison once the sibling stories are compiled: hold the referent fixed (the same arrangement), compare authored beneficiary/victim structures and epsilon across readings; divergence then locates the dispute in the readings'' premises rather than in the arrangement.',
    'If the dependency-trap reading better fits observed net flows, this story''s no-victims Pareto claim fails and the arrangement reclassifies toward hybrid coordination/extraction with net contributors as victims; if this reading fits, the sibling''s incentive-distortion premise loses its structural footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_index, conceptual, 'Kernel membership: one arrangement, three readings; this file is the freedom-floor instantiation.').

omega_variable(
    labor_supply_scale_neutrality,
    'Do the minimal labor-supply effects observed in the Alaska dividend and in cash-transfer pilots persist at national, tax-funded scale?',
    'Scaled natural experiments: nationwide rollouts with staggered timing, or regional discontinuities in benefit level, measured against reservation-wage and hours-worked baselines.',
    'Large-scale withdrawal effects would raise effective extraction (recipients pay in forgone output), erode the voluntary-participation coordination claim, and push classification toward hybrid coordination/extraction; confirmation locks the rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_scale_neutrality, empirical, 'Whether pilot-scale labor-supply neutrality extrapolates to universal scale.').

omega_variable(
    net_payer_seat_visibility,
    'Is there a structurally distinct net-payer cohort whose cost the no-victims declaration leaves unmodeled?',
    'Lifecycle distributional incidence analysis of the funding package: if a defined cohort pays persistently more than it receives across the lifecycle and cannot offset through the dividend, it is a payer seat despite the Pareto framing.',
    'A confirmed persistent net-payer cohort forces a victims declaration, pushes the powerful seat''s directionality above the authored override, and moves the arrangement toward hybrid coordination/extraction; lifecycle-neutral incidence supports the no-victims claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_payer_seat_visibility, empirical, 'Whether progressive funding creates a durable loser cohort the Pareto claim conceals.').

omega_variable(
    rent_capture_leakage,
    'How much of the payment''s real value leaks to rental_property_owners through rent pass-through in supply-constrained housing markets?',
    'Event-study rent series around dividend disbursement dates and benefit-level changes, stratified by local housing-supply elasticity.',
    'High pass-through converts a share of the transfer into landlord income, shrinks recipients'' real gain, and would justify listing rental_property_owners as a primary rather than incidental beneficiary; low pass-through preserves the diffuse-gains claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rent_capture_leakage, empirical, 'Housing-market capture of universal cash transfers.').

omega_variable(
    universality_durability_under_fiscal_stress,
    'Does universality survive a fiscal crisis, or does the arrangement collapse into means-testing, destroying the stigma-elimination function this reading depends on?',
    'Observe the arrangement''s behavior under a realized revenue shock: whether adjustments preserve universality (level changes, funding rebalancing) or reintroduce conditions (cliffs, assessments, categories).',
    'Collapse into means-testing ends this constraint and births a different one (targeted aid with restored stigma and takeup penalties); resilience confirms the coordination function is robust rather than fair-weather.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universality_durability_under_fiscal_stress, empirical, 'Whether the floor''s defining property is crisis-robust.').

omega_variable(
    cs_framing_under_determination,
    'Is the kernel the statutory payment formula, or the pilot-evaluation practice that produces the evidence this reading''s authority rests on?',
    'Trace which entity adjudicates disputes in practice: legislatures amending formulas versus research communities ruling on evidence quality.',
    'Under the practice framing, authority_grounding shifts from expertise toward practice and drift reads as methodological rather than legislative; the reading''s authority classification changes though the arrangement''s type does not.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_under_determination, conceptual, 'Alternative kernel framings for the commitment-system classification.').

omega_variable(
    leveled_judgment_uncertainty,
    'The organizational-level rows of the coercion grid rest on thinner evidence than the individual and structural rows; how much of the organizational-level picture is conservative judgment rather than measurement?',
    'Firm-level studies of hiring behavior, wage-setting, and business-association lobbying before and after dividend introduction.',
    'If employer-side adaptation is larger than authored, the organizational rows understate residual coercive capacity and the individual-level relief is partly illusory; if smaller, the grid understates the floor''s bargaining-power effect.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(leveled_judgment_uncertainty, empirical, 'Evidence quality of organization-level grid judgments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ubi_freedom_floor_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.06).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t0, observed).
narrative_ontology:measurement(ubi_freedom_floor_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.08).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t8, observed).
narrative_ontology:measurement(ubi_freedom_floor_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t16, observed).
narrative_ontology:measurement(ubi_freedom_floor_tr_t24, unconditional_income_support__freedom_floor_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t24, observed).
narrative_ontology:measurement(ubi_freedom_floor_tr_t32, unconditional_income_support__freedom_floor_reading, theater_ratio, 32, 0.13).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t32, observed).
narrative_ontology:measurement(ubi_freedom_floor_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.14).
narrative_ontology:measurement_basis(ubi_freedom_floor_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(ubi_freedom_floor_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t0, observed).
narrative_ontology:measurement(ubi_freedom_floor_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.15).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t8, observed).
narrative_ontology:measurement(ubi_freedom_floor_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.18).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t16, observed).
narrative_ontology:measurement(ubi_freedom_floor_be_t24, unconditional_income_support__freedom_floor_reading, base_extractiveness, 24, 0.21).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t24, observed).
narrative_ontology:measurement(ubi_freedom_floor_be_t32, unconditional_income_support__freedom_floor_reading, base_extractiveness, 32, 0.24).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t32, observed).
narrative_ontology:measurement(ubi_freedom_floor_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.26).
narrative_ontology:measurement_basis(ubi_freedom_floor_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(ubi_freedom_floor_grid_01, unconditional_income_support__freedom_floor_reading, accessibility_collapse(class), 0, 0.5).
narrative_ontology:measurement(ubi_freedom_floor_grid_02, unconditional_income_support__freedom_floor_reading, accessibility_collapse(class), 40, 0.32).
narrative_ontology:measurement(ubi_freedom_floor_grid_03, unconditional_income_support__freedom_floor_reading, accessibility_collapse(individual), 0, 0.55).
narrative_ontology:measurement(ubi_freedom_floor_grid_04, unconditional_income_support__freedom_floor_reading, accessibility_collapse(individual), 40, 0.3).
narrative_ontology:measurement(ubi_freedom_floor_grid_05, unconditional_income_support__freedom_floor_reading, accessibility_collapse(organizational), 0, 0.35).
narrative_ontology:measurement(ubi_freedom_floor_grid_06, unconditional_income_support__freedom_floor_reading, accessibility_collapse(organizational), 40, 0.38).
narrative_ontology:measurement(ubi_freedom_floor_grid_07, unconditional_income_support__freedom_floor_reading, accessibility_collapse(structural), 0, 0.45).
narrative_ontology:measurement(ubi_freedom_floor_grid_08, unconditional_income_support__freedom_floor_reading, accessibility_collapse(structural), 40, 0.38).
narrative_ontology:measurement(ubi_freedom_floor_grid_09, unconditional_income_support__freedom_floor_reading, resistance(class), 0, 0.35).
narrative_ontology:measurement(ubi_freedom_floor_grid_10, unconditional_income_support__freedom_floor_reading, resistance(class), 40, 0.3).
narrative_ontology:measurement(ubi_freedom_floor_grid_11, unconditional_income_support__freedom_floor_reading, resistance(individual), 0, 0.1).
narrative_ontology:measurement(ubi_freedom_floor_grid_12, unconditional_income_support__freedom_floor_reading, resistance(individual), 40, 0.12).
narrative_ontology:measurement(ubi_freedom_floor_grid_13, unconditional_income_support__freedom_floor_reading, resistance(organizational), 0, 0.3).
narrative_ontology:measurement(ubi_freedom_floor_grid_14, unconditional_income_support__freedom_floor_reading, resistance(organizational), 40, 0.33).
narrative_ontology:measurement(ubi_freedom_floor_grid_15, unconditional_income_support__freedom_floor_reading, resistance(structural), 0, 0.4).
narrative_ontology:measurement(ubi_freedom_floor_grid_16, unconditional_income_support__freedom_floor_reading, resistance(structural), 40, 0.42).
narrative_ontology:measurement(ubi_freedom_floor_grid_17, unconditional_income_support__freedom_floor_reading, stakes_inflation(class), 0, 0.6).
narrative_ontology:measurement(ubi_freedom_floor_grid_18, unconditional_income_support__freedom_floor_reading, stakes_inflation(class), 40, 0.34).
narrative_ontology:measurement(ubi_freedom_floor_grid_19, unconditional_income_support__freedom_floor_reading, stakes_inflation(individual), 0, 0.7).
narrative_ontology:measurement(ubi_freedom_floor_grid_20, unconditional_income_support__freedom_floor_reading, stakes_inflation(individual), 40, 0.35).
narrative_ontology:measurement(ubi_freedom_floor_grid_21, unconditional_income_support__freedom_floor_reading, stakes_inflation(organizational), 0, 0.3).
narrative_ontology:measurement(ubi_freedom_floor_grid_22, unconditional_income_support__freedom_floor_reading, stakes_inflation(organizational), 40, 0.33).
narrative_ontology:measurement(ubi_freedom_floor_grid_23, unconditional_income_support__freedom_floor_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(ubi_freedom_floor_grid_24, unconditional_income_support__freedom_floor_reading, stakes_inflation(structural), 40, 0.58).
narrative_ontology:measurement(ubi_freedom_floor_grid_25, unconditional_income_support__freedom_floor_reading, suppression(class), 0, 0.4).
narrative_ontology:measurement(ubi_freedom_floor_grid_26, unconditional_income_support__freedom_floor_reading, suppression(class), 40, 0.22).
narrative_ontology:measurement(ubi_freedom_floor_grid_27, unconditional_income_support__freedom_floor_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(ubi_freedom_floor_grid_28, unconditional_income_support__freedom_floor_reading, suppression(individual), 40, 0.2).
narrative_ontology:measurement(ubi_freedom_floor_grid_29, unconditional_income_support__freedom_floor_reading, suppression(organizational), 0, 0.25).
narrative_ontology:measurement(ubi_freedom_floor_grid_30, unconditional_income_support__freedom_floor_reading, suppression(organizational), 40, 0.22).
narrative_ontology:measurement(ubi_freedom_floor_grid_31, unconditional_income_support__freedom_floor_reading, suppression(structural), 0, 0.25).
narrative_ontology:measurement(ubi_freedom_floor_grid_32, unconditional_income_support__freedom_floor_reading, suppression(structural), 40, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file is the freedom-floor reading: beneficiaries are those whose options the floor widens, no victims claimed, moderate epsilon, rope. The dependency_trap_reading authors the same statutory arrangement with net contributors and displaced targeted-aid recipients as victims and higher epsilon; the universality_paradox_reading models the political mechanics rather than the incidence. Upstream/downstream: this reading's pilot evidence (Alaska dividend, Kenya transfers, Finland experiment) is the empirical substrate both siblings engage, the dependency-trap reading contests it and the paradox reading consumes its cross-ideological appeal. Each family file links the other two via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, powerful, 0.7).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
