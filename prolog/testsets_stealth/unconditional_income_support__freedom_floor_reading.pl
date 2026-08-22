% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor (freedom-floor reading)
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   A periodic cash payment is made to every adult resident, individually,
 *   with no means test, no work requirement, and no behavioral condition —
 *   funded through progressive taxation and, in the longest-standing case,
 *   resource rents. The payment arrives without an application interview, a
 *   caseworker, or a eligibility hearing: the administrative gatekeeping that
 *   conditional relief requires is absent by design. The standing
 *   arrangements this story is about are the ones that actually exist: the
 *   Alaska dividend operating since 1982, the Finnish national experiment of
 *   2017-2018 and its follow-ups, and multi-year unconditional cash trials in
 *   Kenya — together with their funding bases. The interval 0-40 maps
 *   approximately to 1985-2025. The epsilon referent is this standing
 *   arrangement assessed by the reading's own lights — the reading's endorsed
 *   full-scale design is NOT the referent, and no metric here describes that
 *   alternative. KEY AGENTS (by structural relationship): -
 *   precarious_workers: primary beneficiary seat (powerless/constrained) —
 *   the payment converts desperation-taking into refusable offers -
 *   unpaid_caregivers: primary beneficiary seat (powerless/trapped) — care
 *   work compensated by default instead of penalized - independent_artists:
 *   beneficiary seat (moderate/identity_locked) — vocation decoupled from
 *   survival income - domestic_abuse_survivors: primary beneficiary seat
 *   (powerless/trapped) — individual payment as exit resource from economic
 *   dependence - high_income_net_contributors: funding seat
 *   (powerful/arbitrage) — largest net wedge, partially returned via the
 *   universal payment - small_business_employers: dual seat
 *   (organized/mobile) — upward wage pressure against stabilized demand -
 *   national_treasury: agenda setter (institutional/mobile) — sets level,
 *   indexation, and funding base; administers disbursement -
 *   conditional_aid_apparatus: excluded seat (organized/constrained) — the
 *   gatekeeping profession the design eliminates by construction -
 *   welfare_policy_analysts: analytical observer — supplies the external
 *   evaluation base
 *
 * KEY AGENTS:
 *   - precarious_workers: primary beneficiary (powerless/constrained) — bears labor-market desperation the payment removes; cannot exit the tax-and-transfer system but gains refusal power within it
 *   - unpaid_caregivers: primary beneficiary (powerless/trapped) — bound by care obligations; the payment reaches them without requiring them to misrepresent availability for work
 *   - independent_artists: beneficiary (moderate/identity_locked) — vocation-fused; the payment changes the viability of staying, not the decision to stay
 *   - domestic_abuse_survivors: primary beneficiary (powerless/trapped) — the individual, unmonitored payment is the exit resource; the window for exit is crisis-bound
 *   - high_income_net_contributors: funding seat (powerful/arbitrage) — largest net outflow; holds insurance and externality arguments as compensation claim; holds tax-arbitrage exit
 *   - small_business_employers: dual payer/beneficiary (organized/mobile) — pays into the funding base, faces wage pressure, sells into stabilized demand
 *   - national_treasury: agenda setter (institutional/mobile) — legislates and administers; absorbs counter-cyclical fiscal exposure; can restructure at political cost
 *   - conditional_aid_apparatus: excluded (organized/constrained) — the means-testing profession whose discretion the design removes; would argue targeting reaches the desperate better
 *   - welfare_policy_analysts: analytical observer (analytical/analytical) — runs the Alaska, Finland, and Kenya evaluation base from outside the administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.28).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor (freedom-floor reading)").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(unconditional_income_support__freedom_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'd94dc194-0dc0-4c91-9d8e-f6a0c2ed0304').
narrative_ontology:cs_kernel_codification('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', formalized).
narrative_ontology:cs_authority_grounding('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', practice).
narrative_ontology:cs_interpretation_layer_present('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304').
narrative_ontology:cs_reading_relation('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', foundational, unconditionality_removes_market_coercion).
narrative_ontology:cs_axiom_status(unconditionality_removes_market_coercion, holdable).
narrative_ontology:cs_axiom_grounding('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', unconditionality_removes_market_coercion, empirically_contingent).
narrative_ontology:cs_axiom('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', secondary, conditional_relief_machinery_generates_stigma).
narrative_ontology:cs_axiom_status(conditional_relief_machinery_generates_stigma, holdable).
narrative_ontology:cs_axiom_grounding('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', conditional_relief_machinery_generates_stigma, empirically_contingent).
narrative_ontology:cs_reference_frame('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', universal_adequate_autonomy_floor).
narrative_ontology:cs_drift_state('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', post_pilot_evaluation_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d94dc194-0dc0-4c91-9d8e-f6a0c2ed0304', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, unpaid_caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, independent_artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, high_income_net_contributors).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, small_business_employers).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, independent_artists).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, high_income_net_contributors).
narrative_ontology:constraint_victim(unconditional_income_support__freedom_floor_reading, small_business_employers).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, autonomy_precondition_thesis).
narrative_ontology:constraint_vindicates(unconditional_income_support__freedom_floor_reading, voluntary_participation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Work unstable low-wage jobs with irregular hours and thin savings. The periodic unconditional payment arrives regardless of employment status, so declining a dangerous or demeaning shift no longer means missing rent. They continue to pay sales and payroll taxes into the funding base, so their net position is modestly positive. Leaving the national tax-and-transfer system is not realistic; what changes for them is the power to say no to individual employers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, constrained, national).

% Provide full-time care for children or aging relatives outside paid employment. Means-tested programs have historically treated them as work-ready and penalized the care they perform; an unconditional payment compensates care by default and arrives without an application interview. Their days are bound by care obligations they will not abandon, so their practical options run through the payment rather than around it.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, unpaid_caregivers, beneficiary,
    powerless, biographical, trapped, national).

% Earn volatile project income in fields with long unpaid apprenticeships. The payment covers the gap years that would otherwise force a choice between the vocation and survival work; they also pay taxes in their good years. Their attachment to the vocation is constitutive — they would not leave the field under any payment design — so what the payment changes is the viability of staying, not the decision to stay.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, independent_artists, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, independent_artists, payer).

% Face economic dependence on a partner or family member as the practical barrier to leaving. An individual payment, paid to each adult rather than to households, provides money an abuser cannot jointly claim or monitor through a caseworker. The window in which escape is possible is short and crisis-bound, which makes the timing and individuality of the payment matter more than its long-run size.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, domestic_abuse_survivors, beneficiary,
    powerless, immediate, trapped, national).

% Pay the largest net share of the funding through progressive income and capital taxation while receiving the same flat payment as everyone else — a substantial net outflow. They hold the insurance argument as their compensation claim: the payment protects their own downside in shocks, and a population that cannot be starved into any job competes less desperately for their labor and their neighborhoods. Income restructuring and relocation to lower-tax jurisdictions are live options, which limits how much of the funding burden is actually collectable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, high_income_net_contributors, payer,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, high_income_net_contributors, beneficiary).

% Contribute payroll and business taxes to the funding base while facing upward wage pressure as employees' ability to refuse bad offers rises. They also sell into a population with stabilized purchasing power and hire from a workforce that can afford training and health. Relocation, automation, and restructuring are available to them, so the wage pressure operates as a price signal rather than a bind.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, small_business_employers, payer,
    organized, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__freedom_floor_reading, small_business_employers, beneficiary).

% Legislates the payment's level, indexation, and funding base, and administers disbursement through the tax authority rather than a caseworker apparatus. It absorbs the counter-cyclical exposure: in recessions the payment's cost rises exactly as revenue falls. It can restructure or repeal the design at any budget cycle, at the political cost of taking a payment away from the entire electorate.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, national_treasury, agenda_setter,
    institutional, generational, mobile, national).

% The professional strata — caseworkers, means-testing administrators, targeted-aid organizations — whose gatekeeping role an unconditional design eliminates by construction. They are not seated in the design conversations, where unconditionality is framed as the removal of their discretion rather than as a decision affecting them. They would argue that dollars delivered to the non-needy are dollars not delivered to the desperate, and that professional discretion is how aid reaches the hardest cases.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, conditional_aid_apparatus, excluded,
    organized, biographical, constrained, national).

% Academic and institutional researchers who run the evaluation base: long-run dividend studies in Alaska, the Finnish experiment's follow-up analyses, multi-year cash-transfer trials in Kenya. They attest labor-supply, health, and relationship-exit effects from outside the program's administration, and their findings are the main external check on what the payment actually does.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, welfare_policy_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__freedom_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(unconditional_income_support__freedom_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools survival risk once, universally, instead of assessing need case by case: every resident's downside is insured by the same mechanism, and relief arrives without verification of deservingness. Removes the need to prove desperation before receiving subsistence.
% TRANSFER_FUNCTION: Moves purchasing power from net contributors — concentrated in higher-income households, funded through progressive taxation and in one standing case resource rents — to every adult resident equally as a periodic unconditional payment; the net flow runs downward, with the largest relative gains to those with the least market income.
% ABSENT_VOICES: The conditional-aid apparatus — caseworkers, means-testing administrators, targeted-aid organizations — would object that universality spends fiscal capacity on the non-needy and that professional discretion is how aid reaches the desperate; they are absent from the design conversation because the design's premise is the removal of their discretion. Residency-excluded groups (migrants without permanent status) would object to being outside the floor's coverage entirely.
% DISAPPEARANCE_RATIONALE: If the payment vanished overnight, refusal power would collapse back to desperation: caregivers would be forced into whatever work fits around care or into deeper dependence, abuse exits would close for anyone without independent savings, artists would abandon vocations for survival employment, and precarious workers would lose the ability to wait out a bad labor market. The conditional apparatus would re-expand to fill the gap, reinstating the surveillance and stigma the design removed. The labor market would re-coerce at the bottom within a payment cycle.
% FOUNDING_PROBLEM: Decoupling survival from two coupled machines: the labor market's desperation pricing (take any offer or go without) and the conditional-relief apparatus's surveillance-and-stigma gatekeeping (prove deservingness, submit to monitoring, accept the marked status). The design problem was a floor that reached care, vocation, and crisis without passing through either machine.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the academic evaluation base — University of Alaska Anchorage ISER dividend studies, the Finnish government's evaluation consortium for the 2017-18 experiment, and the multi-year Kenyan cash-transfer trials — attests both the persistence of desperation-driven labor acceptance and the measured effects of removing it. Poor-law and welfare-state historiography independently attests the stigma-and-surveillance machinery of conditional relief. Critics of the design, who dispute the cure, nonetheless concede the founding problem exists — the strongest available external corroboration of its liveness.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.28: the funding wedge is compulsory and concentrated on the upper tail, but the universal payment returns purchasing power to the same population, and the behavioral evidence this reading rests on (minimal labor-supply response in the Alaska and Kenya records) keeps the distortion cost low. Suppression 0.15: the arrangement's defining structural feature is the removal of conditional gates — no means test, no sanctions, no caseworker surveillance — so the residual compulsion is the tax itself, priced here as the cost of the insurance. Theater_ratio 0.15: the payment is the function; the ritual share is the annual check ceremony, pilot press cycles, and anniversary politics, which grow slowly as the payment becomes a political symbol. Accessibility_collapse 0.30: rival designs — means-tested aid, negative-income-tax forms, workfare — remain live, partly implemented policy options; the arrangement does not collapse them. Resistance 0.40: real funding fights exist (the Alaska formula battles of the mid-2010s), but universality diffuses opposition since nearly every seat also receives. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is engine-scaled. No suppression_requirement series is authored: enforcement capacity (tax collection plus disbursement) is stable across the interval, and a static enforcement picture belongs to the scalar, not to a manufactured trajectory. Both tracked series run on one shared six-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the beneficiary seats the arrangement is experienced as autonomy: the caregiver reads the payment as compensation finally arriving by default; the abuse survivor reads it as an exit resource no abuser can jointly claim; the precarious worker reads it as the difference between refusing and accepting a dangerous shift. From the funding seat the same structure is experienced as a compulsory wedge partially returned — the net contributor's position is genuinely dual, which is why the override pins that seat above symmetric rather than at the beneficiary end the no-victims declaration would otherwise imply. The treasury seat is pass-through: it raises and disburses, retaining only overhead, and bears the counter-cyclical exposure. The artist seat is identity-locked to the vocation, not to the arrangement — exit from the field is not a live option under any design, so that seat reads the payment purely as viability. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The four declared beneficiary seats derive low directionality from the beneficiary declarations — the payment subsidizes them, and for the trapped seats (caregivers, abuse survivors) the subsidy is deepest because their exit options are worst. No victim seat is declared: the reading's Pareto claim is that the funding wedge is compensated by insurance value and externality benefits even for its largest payers. Three overrides are needed precisely because the derivation cannot see funding burden in the absence of victim declarations: high_income_net_contributors (powerful) would derive near the beneficiary end, but they bear the largest net outflow — pinned at 0.60, above symmetric; small_business_employers (organized) pay into the base but sell into stabilized demand — pinned at 0.55; national_treasury (institutional) is pass-through administration — pinned at 0.50. Spatial scope is national for the standing arrangements, giving moderate scope amplification of the funding wedge.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: labor-market desperation, uncompensated care, vocation-destroying income volatility, and economically enforced relationship dependence all persist, and market shocks recur on schedule. The status is corroborated from outside the benefiting parties — the academic evaluation base (Alaska ISER dividend studies, the Finnish evaluation consortium, the Kenyan long-run trials) attests both the problem and the payment's measured effects, and even critics of the design concede the desperation and stigma it targets exist while disputing the cure. The rope classification prevents the two mislabelings this arrangement attracts: reading the transfers as pure subsidy-for-idleness ignores the live coordination function (risk pooled once, universally, instead of per-case means-tested assessment); reading them as self-sustaining ignores the compulsory funding machinery that requires active enforcement. The live mandatrophy risk runs toward decay rather than capture: if adequacy erodes while the payment persists, the arrangement drifts toward a theatrically maintained check whose floor function has atrophied — the theater_ratio series and the adequacy omega are the tracking instruments for exactly that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_location,
    'This constraint is one reading of the unconditional_income_support kernel (freedom_floor_reading). The sibling dependency_trap_reading reads the same transfers as incentive-distorting subsidy. Where exactly is the structural disagreement located, such that the two readings instantiate different constraints?',
    'Long-run panel evidence on the contested parameters: labor-supply and sector-participation elasticities from the Alaska dividend cohorts, the Finnish experiment follow-ups, and multi-year Kenyan cash trials, plus net-incidence tables showing whether any seat is a net loser. The sibling file authors the dependency-trap constraint with its own epsilon and victim set; this file does not hedge across the two.',
    'If behavioral distortion dominates and a net-loser seat is established, this reading''s rope classification fails toward the sibling''s structure; if the behavioral evidence stays minimal, the freedom-floor classification holds. The two files remain separate constraints either way — the disagreement is a parameter dispute between stories, not an observable-selection dial inside one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_location, empirical, 'Location of the structural disagreement between the freedom-floor and dependency-trap readings of the kernel.').

omega_variable(
    pareto_claim_net_contributor_position,
    'This reading claims no victim seat (Pareto improvement via autonomy). Is the net contributor''s compensation claim real — insurance value in shocks, reduced desperate competition for labor, care and health externalities — or is it cover for an uncompensated net outflow?',
    'Distributional incidence studies measuring each seat''s net position across the full funding-and-payment cycle, combined with willingness-to-accept estimates for the insurance and externality benefits; the Alaska funding fights provide a revealed-preference test of whether net contributors behave as compensated parties.',
    'If net contributors are uncompensated, a victim seat exists and the arrangement is coordination plus asymmetric extraction through the same tax-transfer structure — a hybrid, not a pure coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pareto_claim_net_contributor_position, empirical, 'Whether the no-victims Pareto claim survives incidence analysis.').

omega_variable(
    funding_source_ambiguity,
    'How much of the standing arrangement''s funding wedge falls on taxable activity versus resource rents? Rent-funded dividends (Alaska''s oil-funded payment) extract from a pool no one labored for; broad-tax-funded designs place a visible compulsory wedge on earned income.',
    'Decompose the standing arrangements by jurisdiction and funding instrument, and track pending legislation''s funding bases separately from its payment levels.',
    'Rent-funded variants sit near the low end of the extractiveness range; broad-tax variants carry a heavier wedge and sharpen the net-contributor seat''s position, changing the per-seat classifications the engine computes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_source_ambiguity, empirical, 'Funding-base composition of the standing arrangements and its effect on the funding wedge.').

omega_variable(
    adequacy_erosion_piton_risk,
    'If the payment''s real value erodes against subsistence costs while the payment itself persists, does the arrangement''s autonomy function atrophy into ritual maintenance of a check that no longer floors anything?',
    'Real-value series of the transfer against subsistence and median-cost benchmarks; the Alaska formula fights of the mid-2010s are the observed case of adequacy erosion resisted and partially accepted.',
    'Sustained erosion would push the standing arrangement toward an inertial, theatrically maintained remnant even under this reading''s own lights — the theater_ratio series is the early-warning indicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adequacy_erosion_piton_risk, empirical, 'Whether adequacy erosion converts the live floor into a maintained ritual.').

omega_variable(
    implementation_path_ambiguity,
    'This reading instantiates the flat, unconditional design: one payment per adult, no phase-out. Phase-out designs (negative-income-tax forms) embed implicit marginal tax rates in the withdrawal zone that reintroduce conditionality by stealth. Which implementation path does the standing arrangement actually take?',
    'Legislative design analysis of enacted and pending schemes: payment uniformity, withdrawal structure, and household versus individual payment units.',
    'Adoption of phase-out designs would create an effective-loser seat among withdrawal-zone earners, raise the extractiveness profile, and move the standing arrangement away from this reading''s reference frame without any change in headline generosity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(implementation_path_ambiguity, conceptual, 'Flat-versus-phase-out design ambiguity within this reading''s own reference frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uif_floor_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(uif_floor_tr_t0, observed).
narrative_ontology:measurement(uif_floor_tr_t8, unconditional_income_support__freedom_floor_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(uif_floor_tr_t8, observed).
narrative_ontology:measurement(uif_floor_tr_t16, unconditional_income_support__freedom_floor_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement_basis(uif_floor_tr_t16, observed).
narrative_ontology:measurement(uif_floor_tr_t24, unconditional_income_support__freedom_floor_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(uif_floor_tr_t24, observed).
narrative_ontology:measurement(uif_floor_tr_t32, unconditional_income_support__freedom_floor_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement_basis(uif_floor_tr_t32, observed).
narrative_ontology:measurement(uif_floor_tr_t40, unconditional_income_support__freedom_floor_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement_basis(uif_floor_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(uif_floor_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement_basis(uif_floor_be_t0, observed).
narrative_ontology:measurement(uif_floor_be_t8, unconditional_income_support__freedom_floor_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement_basis(uif_floor_be_t8, observed).
narrative_ontology:measurement(uif_floor_be_t16, unconditional_income_support__freedom_floor_reading, base_extractiveness, 16, 0.23).
narrative_ontology:measurement_basis(uif_floor_be_t16, observed).
narrative_ontology:measurement(uif_floor_be_t24, unconditional_income_support__freedom_floor_reading, base_extractiveness, 24, 0.25).
narrative_ontology:measurement_basis(uif_floor_be_t24, observed).
narrative_ontology:measurement(uif_floor_be_t32, unconditional_income_support__freedom_floor_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement_basis(uif_floor_be_t32, observed).
narrative_ontology:measurement(uif_floor_be_t40, unconditional_income_support__freedom_floor_reading, base_extractiveness, 40, 0.28).
narrative_ontology:measurement_basis(uif_floor_be_t40, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(unconditional_income_support__freedom_floor_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__dependency_trap_reading).
narrative_ontology:affects_constraint(unconditional_income_support__freedom_floor_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'unconditional income support' covers three structurally distinct claims about the same transfers, decomposed per the epsilon-invariance principle into a three-story kernel family. This file is the freedom-floor reading: no victim seat claimed, moderate extractiveness, rope. The dependency_trap_reading file instantiates the incentive-distortion claim (victim seat: net contributors and crowded-out targeted-aid recipients; higher extractiveness). The universality_paradox_reading file instantiates the implementation-path claim (the dispute located in design convergence rather than behavioral response). Each story links to the other two; the upstream freedom-floor reading supplies the cross-ideological appeal that the paradox reading analyzes, and the dependency-trap reading is the standing rival characterization whose empirical parameters this file's omegas track.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, powerful, 0.6).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, organized, 0.55).
constraint_indexing:directionality_override(unconditional_income_support__freedom_floor_reading, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
