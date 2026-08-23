% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__freedom_floor_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: income_support_conditionality__freedom_floor_reading
 *   human_readable: Unconditional Income Floor as Decommodifying Exit Option (Freedom-Floor Reading)
 *   domain: political_economy/social_policy/labor_economics
 *
 * SUMMARY:
 *   This file instantiates the freedom_floor_reading of the
 *   income_support_conditionality kernel: unconditional income support as a
 *   decommodifying floor that converts labor from a compelled sale into a
 *   consensual one. Under this reading, the standing arrangement — the
 *   unconditional deposit plus its progressive funding — is a coordination
 *   device on a shared exit option: any worker can refuse a coercive offer
 *   only because everyone is guaranteed subsistence without one. The epsilon
 *   referent is that standing arrangement assessed by this reading's own
 *   lights: low-to-moderate extraction, because the compulsory funding is the
 *   designed price of the coordination rather than rent layered on top of a
 *   service. The sibling readings (dependency_trap, wage_subsidy) are
 *   separate constraints in separate files, linked via
 *   network.affects_constraints; their structural deltas are recorded in the
 *   kernel_reading_instantiation omega, never folded into this
 *   classification. Claim/metric independence holds: claimed_type rope states
 *   this reading's structural account, and the metrics are authored as
 *   descriptive judgments about how the arrangement actually operates.
 *
 * KEY AGENTS:
 *   - low_wage_workers: primary beneficiary (moderate power / constrained exit) — receive the floor and the refusal capacity it funds
 *   - unpaid_caregivers: beneficiary (powerless / identity_locked) — care performed outside markets becomes independently resourced
 *   - precarious_gig_workers: beneficiary (moderate / constrained) — income smoothed across gig spells
 *   - employers_of_low_wage_labor: primary target (institutional / arbitrage) — lose the destitution fallback that previously priced labor
 *   - high_income_taxpayers: fiscal target (powerful / mobile) — bear the largest funding share while receiving the same flat deposit
 *   - national_tax_authorities: agenda setter (institutional / constrained) — legislate and administer the floor and its funding
 *   - undocumented_residents: excluded voice (powerless / trapped) — inside the covered economy, outside the floor
 *   - labor_economists: analytical observer (analytical / analytical) — measure incidence and labor-supply response from outside the transfer
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__freedom_floor_reading, 0.34).
domain_priors:suppression_score(income_support_conditionality__freedom_floor_reading, 0.22).
domain_priors:theater_ratio(income_support_conditionality__freedom_floor_reading, 0.14).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, extractiveness, 0.34).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, theater_ratio, 0.14).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(income_support_conditionality__freedom_floor_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__freedom_floor_reading, rope).
narrative_ontology:human_readable(income_support_conditionality__freedom_floor_reading, "Unconditional Income Floor as Decommodifying Exit Option (Freedom-Floor Reading)").
narrative_ontology:topic_domain(income_support_conditionality__freedom_floor_reading, "political_economy/social_policy/labor_economics").

domain_priors:requires_active_enforcement(income_support_conditionality__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__freedom_floor_reading, 'fb23fee8-65c0-495e-9b7b-0d6133d572bb').
narrative_ontology:cs_kernel_codification('fb23fee8-65c0-495e-9b7b-0d6133d572bb', distributed).
narrative_ontology:cs_authority_grounding('fb23fee8-65c0-495e-9b7b-0d6133d572bb', distributed).
narrative_ontology:cs_reading_relation('fb23fee8-65c0-495e-9b7b-0d6133d572bb', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb23fee8-65c0-495e-9b7b-0d6133d572bb', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('fb23fee8-65c0-495e-9b7b-0d6133d572bb', foundational, labor_non_coercion_requires_unconditional_exit_option).
narrative_ontology:cs_axiom_status(labor_non_coercion_requires_unconditional_exit_option, holdable).
narrative_ontology:cs_axiom_grounding('fb23fee8-65c0-495e-9b7b-0d6133d572bb', labor_non_coercion_requires_unconditional_exit_option, deontological).
narrative_ontology:cs_axiom('fb23fee8-65c0-495e-9b7b-0d6133d572bb', secondary, decommodification_raises_worker_reservation_wages).
narrative_ontology:cs_axiom_status(decommodification_raises_worker_reservation_wages, holdable).
narrative_ontology:cs_axiom_grounding('fb23fee8-65c0-495e-9b7b-0d6133d572bb', decommodification_raises_worker_reservation_wages, empirically_contingent).
narrative_ontology:cs_reference_frame('fb23fee8-65c0-495e-9b7b-0d6133d572bb', universal_unconditional_individual_cash_floor).
narrative_ontology:cs_drift_state('fb23fee8-65c0-495e-9b7b-0d6133d572bb', contemporary_partial_coverage_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb23fee8-65c0-495e-9b7b-0d6133d572bb', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__freedom_floor_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, precarious_gig_workers).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor).
narrative_ontology:constraint_victim(income_support_conditionality__freedom_floor_reading, high_income_taxpayers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__freedom_floor_reading, high_income_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work hourly and service jobs at the bottom of the wage distribution. The unconditional deposit arrives regardless of employment status, so turning down a demeaning or underpaid offer no longer means missing rent. They remain paycheck-adjacent: the floor covers subsistence, not comfort, and leaving the country would mean leaving the deposit behind.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, low_wage_workers, beneficiary,
    moderate, biographical, constrained, national).

% Raise children and look after elderly relatives outside paid employment. The deposit arrives in their own name, so remaining in a household no longer requires depending on a breadwinner's income or goodwill. Exiting caregiving itself is not realistic — the people they care for do not stop needing them.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, identity_locked, national).

% String together delivery, ride-hail, and short-contract work with irregular income. The floor smooths the gaps between gigs and lets them decline unsafe assignments or unpaid waiting time. Taxes on their earnings still come out, and the accounting burden of irregular income falls on them.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, precarious_gig_workers, beneficiary,
    moderate, immediate, constrained, national).

% Run retail, hospitality, logistics, and care firms staffed at the bottom of the wage market. Before the floor, a worker's alternative to accepting the terms offered was destitution, which kept wages and scheduling discipline cheap. Now every worker can walk away and eat anyway, so vacancies last longer and retention costs more. Large firms respond with automation and by relocating roles abroad; smaller ones mostly reprice.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, employers_of_low_wage_labor, payer,
    institutional, generational, arbitrage, continental).

% Pay the progressive rates that fund the deposit; their annual bill is the largest single share of the financing. They also receive the same flat deposit as everyone else, though their net position is deeply negative. Relocating residence or booking income in lower-tax jurisdictions is possible and some do it, at the cost of uprooting businesses and families.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, high_income_taxpayers, payer,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__freedom_floor_reading, high_income_taxpayers, beneficiary).

% Legislate the deposit level, set the funding schedule, and administer payments through the tax system. Adjusting generosity is an annual budget decision; abolishing the program outright would trigger immediate visible hardship and electoral backlash, so administration continues whichever coalition holds office.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, national_tax_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Live and work in the same industries the floor reshapes — agriculture, construction, domestic work — but are barred from receiving the deposit by residence rules. They face the repriced labor market without its protection and have no seat in the coalition that set the rules.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, undocumented_residents, excluded,
    powerless, immediate, trapped, national).

% Design and evaluate pilots, estimate labor-supply and wage-pass-through elasticities, and publish incidence studies. Their findings feed the political argument on every side; they collect no deposit and pay no special levy.
narrative_ontology:constraint_stakeholder(income_support_conditionality__freedom_floor_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__freedom_floor_reading, low_wage_workers).
narrative_ontology:fixing_cost_class(income_support_conditionality__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem that no individual worker can credibly refuse a coercive job offer alone: unilateral refusal means destitution, so refusal becomes available only when everyone is guaranteed a floor. The unconditional deposit mutualizes subsistence risk, converting exit from an individual gamble into a routinely available option.
% TRANSFER_FUNCTION: Moves purchasing power from the general tax base — concentrated on high earners and capital returns — to every resident individual as an unconditional cash floor; derivatively, it moves bargaining power from employers of low-wage labor to workers, repricing the worst jobs upward.
% ABSENT_VOICES: Undocumented residents living inside the covered economy but barred from the deposit would object to their exclusion; future cohorts inheriting the fiscal obligation were not seated when the funding formula was set; small employers without relocation options bear repricing costs while the arbitrage-capable large-firm voice dominates the employer side of the debate. All sit outside the legislative coalition that fixed coverage and funding.
% DISAPPEARANCE_RATIONALE: Overnight repeal would push caregivers, recent care leavers, and low-wage workers back into take-any-offer positions within weeks; bottom-quartile wages would fall toward desperation pricing, refusal capacity would vanish, and the labor market would reorganize around restored employer wage-setting power. Every suspension or lapse episode produces visible immediate hardship, demonstrating the dependence.
% FOUNDING_PROBLEM: Industrial labor markets left survival contingent on accepting whatever terms were on offer: unemployment meant destitution, so employers could price labor against starvation. Poor laws answered destitution punitively while deliberately preserving that pricing power. The floor was built to sever survival from unconditional labor-market submission.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: labor-history documentation of pre-floor destitution pricing; the monopsony-economics literature attesting that wage-setting power over low-wage workers persists wherever workers lack outside options; and employer-side testimony in wage-floor hearings acknowledging reliance on worker desperation. No corroborating source attests that the founding problem is solved.
narrative_ontology:disappearance_verdict(income_support_conditionality__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__freedom_floor_reading, 0.34, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__freedom_floor_reading_tests).
:- end_tests(income_support_conditionality__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.34 at interval end) because the arrangement's compulsory element — taxation — is the mechanism of the coordination itself, not a levy taken beyond function; the slow upward drift reflects demographic aging raising the fiscal weight per contributing worker, not rent-seeking accretion. Suppression is low (0.22): the deposit carries no behavioral conditions, and the only enforcement machinery is ordinary tax collection; suppression is authored as a raw structural property and is not scaled by power or scope — the engine owns any scaling of extractiveness. Theater is low (0.14): money moves every cycle and the function is directly performed; the mild rise tracks administrative reporting rituals accreting around a mature program. Accessibility collapse is low (0.28): the floor adds an option rather than closing any — work, saving, self-employment, and emigration all remain open, which is precisely what distinguishes this instantiation from the trap-structured sibling. Resistance is moderate (0.52): taxpayer and employer coalitions contest expansion continuously, and referendum-level attempts at abolition recur. The measurement series run on one shared time grid (points 0-36, roughly a four-decade program lifetime) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not tracked because the enforcement picture is static — tax collection neither ratchets nor decays over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary seats should compute differently. From the employer seat, the floor is an imposed cost that removes a pricing lever they did not experience as coercion; from the low-wage worker seat, the same structure is the first reliable outside option they have held; from the administrator seat it is a budget line with an electorate attached. High-income taxpayers occupy a genuinely dual position — largest funder and equal recipient — which the derivation must weigh against their strongly negative net position. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: low_wage_workers, unpaid_caregivers, and precarious_gig_workers sit near the full-beneficiary end (caregivers reinforced by identity_locked exit — their position inside the arrangement is fused with the caregiving role itself). employers_of_low_wage_labor are declared victims and sit near the target end; their arbitrage-grade exit (automation, offshoring) legitimately tempers the derived d rather than distorting it, because that leakage channel is real — it is exactly the channel through which the wage_subsidy sibling reading says the transfer is recaptured. high_income_taxpayers sit near the target end on the funding side with mobile exit providing modest damping. No directionality overrides are authored: the derivation from declarations plus exit options reproduces the reading's structural account without correction, and the two institutional-power agents (employers, tax authorities) sit at opposite ends, which an override keyed only by power atom could not distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — severing survival from unconditional labor-market submission — remains live wherever the floor is absent or thin, so mandatrophy is not resolved and no sunset applies: this is a steady-state coordination arrangement, not a transitional one. The classification discipline cuts both ways. Reading the arrangement as pure extraction (a taxpayer-funded giveaway) erases the coordination function that makes refusal collectively possible; reading it as pure gift erases the real compulsory cost borne by funders and the repricing cost borne by employers. The rope claim holds both in view. The drift risk this reading specifically tracks is conditionality creep: reintroducing work requirements would convert the shared exit option into a managed incentive scheme, moving the structure toward the dependency sibling's trap geometry. The theater series is the early-warning instrument for that drift — theatrical compliance activity rises when administration begins performing motivation management rather than moving money.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'This constraint is the freedom_floor_reading of kernel income_support_conditionality — what structural changes would instantiating the sibling readings produce?',
    'Cross-reading comparison: compile the sibling files and diff victim sets, gain-flow seats, and computed types. The disagreement is located in the incidence of unconditional cash — who captures the bargaining-power effect — and in the sign of the incentive effect of conditionality-free income.',
    'The dependency_trap_reading would place low_wage_workers in the victim set (skill atrophy, benefit lock) and classify toward snare; the wage_subsidy_reading would move the gain-flow seat to employers_of_low_wage_labor (wage suppression absorbs the transfer) and classify toward tangled_rope. This file''s rope classification stands or falls with the decommodification-incidence evidence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this story instantiates one of three readings of the income_support_conditionality kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    reservation_wage_pass_through,
    'Does the unconditional floor actually raise workers'' reservation wages and refusal capacity, or do employer counter-mechanisms (scheduling control, monopsony coordination, automation threats) absorb the bargaining shift?',
    'Pilot and dividend natural experiments (negative-income-tax trials, Alaska Permanent Fund dividends, the Finnish experiment) measuring job-refusal rates, quit rates, vacancy durations, and wage pass-through in the bottom wage deciles.',
    'Full absorption by employers collapses this reading into the wage_subsidy sibling''s structure — gain flow migrates to capital and the rope verdict degrades toward tangled_rope; durable refusal-capacity gains confirm the freedom-floor instantiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reservation_wage_pass_through, empirical, 'Whether the decommodification effect survives employer counter-adjustment.').

omega_variable(
    fiscal_incidence_regression_risk,
    'Who ultimately bears the funding burden — the progressive capital-and-high-earner taxation this reading assumes, or broad-based consumption funding that pushes net cost onto the floor''s own beneficiaries?',
    'Fiscal incidence analysis of the enacted funding statute: statutory versus effective incidence by income decile, including indirect price and rent effects.',
    'Regressive effective incidence would move low_wage_workers off the pure-beneficiary end of the directionality scale, raising their effective extraction and degrading the rope verdict toward tangled_rope; progressive incidence preserves the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_incidence_regression_risk, empirical, 'Effective incidence of the funding side determines whether nominal beneficiaries are net payers.').

omega_variable(
    coercive_offer_boundary,
    'Which labor-market offers count as coercive — does a floor that lets workers refuse destitution-level offers but not median offers create positive freedom, or merely a higher submission threshold?',
    'Conceptual clarification of the coercion boundary paired with revealed-preference data on offer-refusal behavior across floor levels; compare refusal elasticity at successive wage percentiles.',
    'A narrow coercion definition shrinks the measured decommodification benefit and pulls this reading toward the wage_subsidy sibling; a broad definition inflates it. The rope classification is stable only if the refusal margin is empirically substantive.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercive_offer_boundary, conceptual, 'Boundary of the coercive-work set that the floor frees workers to refuse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__freedom_floor_reading, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(inco_tr_t6, income_support_conditionality__freedom_floor_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__freedom_floor_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(inco_tr_t18, income_support_conditionality__freedom_floor_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(inco_tr_t24, income_support_conditionality__freedom_floor_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(inco_tr_t30, income_support_conditionality__freedom_floor_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement(inco_tr_t36, income_support_conditionality__freedom_floor_reading, theater_ratio, 36, 0.14).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__freedom_floor_reading, base_extractiveness, 0, 0.24).
narrative_ontology:measurement(inco_be_t6, income_support_conditionality__freedom_floor_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__freedom_floor_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(inco_be_t18, income_support_conditionality__freedom_floor_reading, base_extractiveness, 18, 0.29).
narrative_ontology:measurement(inco_be_t24, income_support_conditionality__freedom_floor_reading, base_extractiveness, 24, 0.31).
narrative_ontology:measurement(inco_be_t30, income_support_conditionality__freedom_floor_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(inco_be_t36, income_support_conditionality__freedom_floor_reading, base_extractiveness, 36, 0.34).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_conditionality__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__dependency_trap_reading).
narrative_ontology:affects_constraint(income_support_conditionality__freedom_floor_reading, income_support_conditionality__wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'unconditional income support' decomposes into three structurally distinct claims per the epsilon-invariance principle. This file instantiates the freedom_floor_reading (decommodifies labor; workers gain refusal capacity; employers lose coercive pricing power; coordination on a shared exit option). income_support_conditionality__dependency_trap_reading instantiates the erosion claim (recipients trapped, snare-flavored). income_support_conditionality__wage_subsidy_reading instantiates the incidence claim (capital captures the transfer via wage suppression, tangled-rope/snare-flavored). The epsilon values differ because the readings locate the arrangement's operative effect in different places; all three cite the same pilot evidence base, which is the coupling channel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
