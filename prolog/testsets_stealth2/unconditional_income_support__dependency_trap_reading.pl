% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__dependency_trap_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support — Dependency Trap Reading
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'unconditional income support': the dependency_trap_reading, on which a
 *   universal flat payment operates as an incentive-distorting subsidy that
 *   rewards labor-market withdrawal, displaces targeted programs worth more
 *   than the grant to the working poor, and redistributes net resources
 *   upward to households that do not need them. Per the ε-invariance
 *   principle, ε here refers ONLY to the standing arrangement under contest —
 *   universal unconditional income support with targeted-program replacement,
 *   as this reading assesses it — never to the freedom-floor arrangement its
 *   sibling reading would install. The sibling readings
 *   (freedom_floor_reading, universality_paradox_reading) are separate
 *   constraint files linked through network.affects_constraints; their ε
 *   values differ and are not averaged here. Claim/metric independence:
 *   claimed_type=snare is asserted from structure (identifiable victims,
 *   extraction carried by a universal-solidarity presentation, active
 *   displacement of the targeted alternative); the metrics are authored
 *   descriptively and the engine computes per-seat classifications. KEY
 *   AGENTS (by structural relationship): - working_poor: primary target
 *   (powerless/trapped) — lose targeted-program value exceeding the flat
 *   grant - net_taxpayers: primary target (moderate/constrained) — bear the
 *   ~$1.4T annual net cost diffusely - middle_upper_income_recipients:
 *   primary beneficiary (organized/mobile) — net-positive after offsets -
 *   ubi_advocacy_coalition: secondary beneficiary (organized/identity_locked)
 *   — collects political capital from universality -
 *   federal_budget_authorities: agenda setter (institutional/arbitrage) —
 *   sets grant level and selects displaced programs -
 *   severely_disabled_benefit_recipients: excluded voice (powerless/trapped)
 *   - targeted_program_service_providers: excluded voice
 *   (moderate/constrained) - labor_economists: analytical observer — produce
 *   the employment-effect evidence every faction cites
 *
 * KEY AGENTS:
 *   - working_poor: primary target (powerless/trapped) — bears concentrated program-displacement losses
 *   - net_taxpayers: primary target (moderate/constrained) — bears the $1.4T net cost diffusely
 *   - middle_upper_income_recipients: primary beneficiary (organized/mobile) — nets positive after offsets
 *   - ubi_advocacy_coalition: secondary beneficiary (organized/identity_locked) — collects political capital from universality
 *   - federal_budget_authorities: agenda setter (institutional/arbitrage) — sets grant level, picks displaced programs
 *   - severely_disabled_benefit_recipients: excluded voice (powerless/trapped) — layered supports exceed any flat grant
 *   - targeted_program_service_providers: excluded voice (moderate/constrained)
 *   - labor_economists: analytical observer — produce the employment-effect evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.72).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.6).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support — Dependency Trap Reading").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '82acafcc-3203-4485-bb5b-1c5728a8140d').
narrative_ontology:cs_kernel_codification('82acafcc-3203-4485-bb5b-1c5728a8140d', distributed).
narrative_ontology:cs_authority_grounding('82acafcc-3203-4485-bb5b-1c5728a8140d', distributed).
narrative_ontology:cs_reading_relation('82acafcc-3203-4485-bb5b-1c5728a8140d', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('82acafcc-3203-4485-bb5b-1c5728a8140d', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('82acafcc-3203-4485-bb5b-1c5728a8140d', foundational, unconditional_transfers_distort_work_incentives).
narrative_ontology:cs_axiom_status(unconditional_transfers_distort_work_incentives, holdable).
narrative_ontology:cs_axiom_grounding('82acafcc-3203-4485-bb5b-1c5728a8140d', unconditional_transfers_distort_work_incentives, empirically_contingent).
narrative_ontology:cs_axiom('82acafcc-3203-4485-bb5b-1c5728a8140d', foundational, targeted_aid_dominates_universal_grants_for_the_poor).
narrative_ontology:cs_axiom_status(targeted_aid_dominates_universal_grants_for_the_poor, holdable).
narrative_ontology:cs_axiom_grounding('82acafcc-3203-4485-bb5b-1c5728a8140d', targeted_aid_dominates_universal_grants_for_the_poor, empirically_contingent).
narrative_ontology:cs_reference_frame('82acafcc-3203-4485-bb5b-1c5728a8140d', targeted_reciprocity_welfare_state).
narrative_ontology:cs_drift_state('82acafcc-3203-4485-bb5b-1c5728a8140d', post_stimulus_check_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('82acafcc-3203-4485-bb5b-1c5728a8140d', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, net_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, income_effect_labor_supply_hypothesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__dependency_trap_reading, public_choice_program_displacement_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold jobs that pay too little to escape means-tested programs: housing assistance, nutrition supplements, childcare subsidies, and earned-income tax credits stack to more than the flat universal payment. When the universal grant replaces those programs, they lose the difference. They cannot purchase political voice, and a second job or a move carries costs the flat grant does not cover.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor, payer,
    powerless, immediate, trapped, national).

% Cover the roughly $1.4 trillion annual net cost after financing offsets through federal taxation. Each household's share is small enough that organizing against it costs more than the share itself, so opposition stays diffuse; relocating abroad or restructuring income to avoid the tax is available only to a thin slice of filers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, net_taxpayers, payer,
    moderate, biographical, constrained, national).

% Receive the same flat payment as every other resident while paying a share of the taxes that finance it; under the offset package this arrangement assumes, most households in the upper-middle of the distribution come out ahead in net terms. They vote, donate, and defend continuation at the ballot box. Exit looks like ordinary geographic and financial mobility, which they possess in abundance.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients, payer).

% Think tanks, organizing networks, and philanthropic funders whose staff rosters, budgets, and public identity are built around advancing a universal payment. The universality frame is their fundraising pitch and coalition glue; a return to means-tested programs would dissolve the movement's reason to exist. Individual staffers could change careers; the organizations cannot rebrand without losing their base.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_advocacy_coalition, beneficiary,
    organized, generational, identity_locked, national).

% Congressional appropriators and the treasury and social-security bureaucracies set the grant amount, choose which means-tested programs are sunsetted to finance it, and administer the payment rolls. They gain a dramatically simpler benefit apparatus and discretion over the transition schedule; they can restructure the arrangement by statute and face elections on the outcome.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, federal_budget_authorities, agenda_setter,
    institutional, biographical, arbitrage, national).

% Depend on layered supports — disability insurance supplements, Medicaid waiver services, subsidized accessible housing — whose combined value runs far beyond any flat grant. They hold no seat in the fiscal negotiations that decide which programs survive; their objection, that cash cannot replace attendant care, reaches the debate only through advocacy intermediaries.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, severely_disabled_benefit_recipients, excluded,
    powerless, biographical, trapped, national).

% Nonprofit caseworkers, housing-first operators, and legal-aid clinics whose clients need coordinated services a cash transfer cannot substitute for. Their program funding is slated for consolidation into the universal grant; they object through comment letters and committee testimony but hold no vote on the financing package.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_program_service_providers, excluded,
    moderate, generational, constrained, national).

% Design and evaluate the income-maintenance experiments, from the 1970s negative income tax trials to recent city, national, and continental pilots, publishing employment and wellbeing estimates that every faction in the argument cites. They collect no transfer and pay none; their estimates move the political market for the arrangement.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, middle_upper_income_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation problem of means-tested welfare: dozens of programs with separate applications, eligibility cliffs, stigma, and low take-up are replaced by one simple unconditional payment that reaches everyone automatically and acts as a recession stabilizer without administrative latency.
% TRANSFER_FUNCTION: Moves approximately $1.4 trillion annually (net of financing offsets) from net federal taxpayers to the entire resident population in equal flat grants; relative to the targeted system it displaces, the net shift runs from the bottom deciles and the taxpaying middle toward the upper-middle of the distribution.
% ABSENT_VOICES: Severely disabled recipients and deep-poverty households with complex needs would object loudest — their stacked in-kind and supplemental supports exceed any flat grant and cannot be replaced by cash — but they hold no seat in fiscal negotiations; targeted-program service providers speak for them thinly through comment letters. Future generations bearing deferred financing costs are absent entirely: no one at the table represents them.
% DISAPPEARANCE_RATIONALE: Millions of households budget around the monthly payment; the social-security and treasury apparatus administering it would stand down; sunsetted means-tested programs would need years to rebuild casework capacity; and the advocacy coalition's institutional base would dissolve — the arrangements of every seated party depend on the payment continuing.
% FOUNDING_PROBLEM: Means-tested welfare was fragmented, stigmatized, and administratively burdensome: eligible households failed to enroll, benefit cliffs taxed additional work at effective rates above 100 percent, and program-by-program appropriation made support slow and volatile.
% FOUNDING_PROBLEM_CORROBORATION: Administrative-burden scholarship (Herd & Moynihan), GAO take-up audits, and OECD targeting-accuracy reviews — none of whose authors collect from universality — corroborate that the founding problem was real. Its status is disputed: this reading holds the replacement worsens the poverty problem it was built to solve, the freedom-floor reading holds it solves the dignity half, and no party outside the benefiting coalition attests that the problem is simply solved.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unconditional_income_support__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement's net incidence runs upward: after financing offsets, roughly $1.4 trillion annually moves from net taxpayers to the whole population, while the working poor — whose stacked means-tested benefits exceed the flat grant — absorb a net loss, compounded by measured employment reduction in large pilots (-3.2%, AEI meta-analysis). Suppression (0.60) is structural, not internalized: once universal checks flow, recipients defend them as a bloc while taxpayer opposition stays diffuse (each household's share sits below its cost of organizing — rational ignorance), and sunsetted targeted programs acquire restoration costs that grow with elapsed time. Theater (0.34, rising) tracks the widening gap between universality's solidaristic presentation ('everyone is in this together') and its regressive net incidence. Accessibility_collapse (0.45) is moderate: understanding the arrangement does not collapse alternatives — targeted programs demonstrably exist and work, which is precisely why their displacement must be legislated rather than assumed. Resistance (0.60) is real: fiscal conservatives, deficit hawks, and anti-poverty advocates contest it, though the natural coalition between diffuse taxpayers and program-dependent poor rarely forms because the two payer seats face opposite collective-action profiles — concentrated-but-voiceless versus dispersed-but-numerous. The measurement series share one grid (t=0..30, step 6) with all three metrics authored at every point; points t>=24 carry basis='projected' because the full-scale replacement arrangement has not been enacted — terminal values extrapolate from pilot and proposal evidence. Suppression is authored as a raw structural property; only extractiveness is scaled by directionality and scope downstream.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute divergent types from identical structural data. From the working_poor seat the arrangement is enforced dispossession: a grant smaller than what it replaced, with no exit. From the net_taxpayer seat it is a diffuse levy whose per-household cost is too small to organize against — experienced as background fiscal weather rather than extraction. From the middle_upper_income_recipient seat the same structure is a net subsidy it did not ask to need. From the ubi_advocacy_coalition seat it is a movement victory whose universality is the point. The federal_budget_authorities seat experiences administrative simplification — dozens of means-tested programs collapsing into one payment line. The engine computes these per-seat classifications from power, exit, and directional position; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. working_poor: declared victim, powerless, trapped — sits nearest the full-target end; their loss is concentrated and their exit set empty. net_taxpayers: declared victim but with diffuse per-capita stakes and constrained (not trapped) exit — high directionality with weak per-agent intensity. middle_upper_income_recipients: declared beneficiary (with secondary payer position), but they also fund a large share of the program through the same tax system that pays them, so they sit nearer symmetry than a pure beneficiary; the residual uncertainty about their net position by decile is routed to the net_incidence_by_decile omega rather than a directionality override, because overrides key on the coarse power atom and would misapply to the ubi_advocacy_coalition, which shares the organized atom and is a pure political-capital collector near the beneficiary pole. ubi_advocacy_coalition: beneficiary on the political-capital dimension, identity_locked exit — near the beneficiary end. federal_budget_authorities: administers and gains simplification plus transition discretion — low-to-mid directionality. No directionality_overrides are authored: the derivation chain from beneficiary/victim declarations plus exit atoms reproduces every seat's qualitative position, and the one genuinely ambiguous seat is handled by omega instead of a coarse-keyed override.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim guards against misreading universality's coordination surface (one simple payment, no stigma, automatic stabilizers — all real) as the whole structure: a rope classification would license the arrangement on its administrative virtues while ignoring who nets what. Equally, the reading resists the opposite error of declaring a dead mandate: the founding problem (fragmented, stigmatized, low-take-up means-testing) is corroborated as real by administrative-burden scholarship outside the benefiting parties, so founding_problem_status is 'contested', not 'dead' — this is a functioning machine whose function mix is disputed, not a zombie. The mismatch consumer reads status x verdict: contested x world_rearranges raises no capture/zombie flag, correctly. If the administrative problems were ever fully solved by other means while the universal payment persisted, status would flip to dead and the flag should fire; the temporal series (rising theater_ratio alongside rising extractiveness) is the early-warning channel for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint instantiates only the dependency_trap_reading of the unconditional_income_support kernel; what structural facts would change under the sibling readings?',
    'Adopting freedom_floor_reading re-authors the poor as primary beneficiaries and drops program-displacement victimhood (epsilon falls toward coordination-cost levels); adopting universality_paradox_reading holds fiscal convergence fixed and re-reads incidence as path-dependent rather than designed.',
    'Classification flips: freedom_floor computes rope/scaffold-like coordination; universality_paradox suspends the designed-extraction claim entirely. This file''s snare verdict is valid only within this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of a three-reading kernel; sibling readings are separate constraints.').

omega_variable(
    labor_supply_effect_magnitude,
    'Does the -3.2% employment effect measured in large 1970s-style income-maintenance experiments scale to a permanent national grant, or do modern smaller pilots'' null-to-small effects represent the true parameter?',
    'Long-duration, full-population pilots or credible natural experiments (Alaska Permanent Fund dividends, unconditional casino stipends) with pre-registered labor-supply endpoints.',
    'Small true effects gut the incentive-distortion leg of this reading; large effects confirm it and raise effective extraction on the working-poor seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effect_magnitude, empirical, 'Whether the pilot-measured employment effect generalizes to permanent national implementation.').

omega_variable(
    program_replacement_counterfactual,
    'Is the working poor''s net loss inherent to unconditional income support, or an artifact of financing packages that sunset targeted programs?',
    'Score proposals that fund the grant exclusively through new progressive revenue with statutory protection for existing programs; observe whether displacement recurs politically even where not legislated.',
    'If displacement is proposal-contingent, the victim structure narrows to taxpayers alone and the reading''s extraction profile drops sharply; if displacement recurs under statutory protection, it is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(program_replacement_counterfactual, conceptual, 'Whether program displacement is constitutive of the arrangement or incidental to particular financing designs.').

omega_variable(
    net_incidence_by_decile,
    'Which deciles are net beneficiaries after financing offsets — is the upward redistribution designed, or incidental to tax-choice?',
    'Distributional scoring of enacted financing packages against baseline tax incidence, published by neutral scorekeepers.',
    'Sufficiently progressive financing removes the ''redistributes upward'' leg and moves middle/upper recipients toward symmetry; regressive financing confirms the reading''s incidence claim and concentrates gain_flow further up the distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(net_incidence_by_decile, empirical, 'Net beneficiary position of middle/upper deciles under specific financing offsets.').

omega_variable(
    meta_analysis_selection_effects,
    'How much of the pooled employment-effect estimate reflects sponsor selection and publication bias, given that much of the meta-analytic base traces to a single ideological research program?',
    'Pre-registered replications across independent labs and systematic comparison of sponsored versus unsponsored subsamples of the experimental record.',
    'Bias correction recalibrates epsilon; the reading''s empirical warrant strengthens or weakens independently of its structural claims about displacement and incidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meta_analysis_selection_effects, empirical, 'Sponsorship and publication bias in the employment-effect evidence base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dep_trap_reading_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t0, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t6, unconditional_income_support__dependency_trap_reading, theater_ratio, 6, 0.17).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t6, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.21).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t12, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t18, unconditional_income_support__dependency_trap_reading, theater_ratio, 18, 0.26).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t18, observed).
narrative_ontology:measurement(dep_trap_reading_tr_t24, unconditional_income_support__dependency_trap_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t24, projected).
narrative_ontology:measurement(dep_trap_reading_tr_t30, unconditional_income_support__dependency_trap_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(dep_trap_reading_tr_t30, projected).

% Extraction over time
narrative_ontology:measurement(dep_trap_reading_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(dep_trap_reading_be_t0, observed).
narrative_ontology:measurement(dep_trap_reading_be_t6, unconditional_income_support__dependency_trap_reading, base_extractiveness, 6, 0.46).
narrative_ontology:measurement_basis(dep_trap_reading_be_t6, observed).
narrative_ontology:measurement(dep_trap_reading_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement_basis(dep_trap_reading_be_t12, observed).
narrative_ontology:measurement(dep_trap_reading_be_t18, unconditional_income_support__dependency_trap_reading, base_extractiveness, 18, 0.62).
narrative_ontology:measurement_basis(dep_trap_reading_be_t18, observed).
narrative_ontology:measurement(dep_trap_reading_be_t24, unconditional_income_support__dependency_trap_reading, base_extractiveness, 24, 0.68).
narrative_ontology:measurement_basis(dep_trap_reading_be_t24, projected).
narrative_ontology:measurement(dep_trap_reading_be_t30, unconditional_income_support__dependency_trap_reading, base_extractiveness, 30, 0.72).
narrative_ontology:measurement_basis(dep_trap_reading_be_t30, projected).

% Suppression requirement over time
narrative_ontology:measurement(dep_trap_reading_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(dep_trap_reading_su_t0, observed).
narrative_ontology:measurement(dep_trap_reading_su_t6, unconditional_income_support__dependency_trap_reading, suppression_requirement, 6, 0.36).
narrative_ontology:measurement_basis(dep_trap_reading_su_t6, observed).
narrative_ontology:measurement(dep_trap_reading_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(dep_trap_reading_su_t12, observed).
narrative_ontology:measurement(dep_trap_reading_su_t18, unconditional_income_support__dependency_trap_reading, suppression_requirement, 18, 0.48).
narrative_ontology:measurement_basis(dep_trap_reading_su_t18, observed).
narrative_ontology:measurement(dep_trap_reading_su_t24, unconditional_income_support__dependency_trap_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(dep_trap_reading_su_t24, projected).
narrative_ontology:measurement(dep_trap_reading_su_t30, unconditional_income_support__dependency_trap_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement_basis(dep_trap_reading_su_t30, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, universality_paradox_reading).

% DUAL FORMULATION NOTE:
% 'Unconditional income support' is a colloquial label covering at least three structurally distinct claims with different epsilon, beneficiary/victim sets, and classifications. This file authors the dependency_trap_reading (high epsilon, snare-shaped: victims = working poor + net taxpayers; beneficiaries = middle/upper recipients + advocacy coalition). freedom_floor_reading shares the referent arrangement but authors low epsilon and a rope/scaffold-shaped structure (poor as primary beneficiaries). universality_paradox_reading treats the fiscal outcome as invariant across implementation paths and suspends designed-extraction claims. Linked per the epsilon-invariance principle; no averaging across readings occurs in any file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
