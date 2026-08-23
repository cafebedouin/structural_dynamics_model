% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__dependency_trap_reading, []).

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
 *   constraint_id: income_support_conditionality__dependency_trap_reading
 *   human_readable: Unconditional Income Support — Dependency Trap Reading (Work-Incentive Erosion)
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   An unconditional income-support regime pays every resident a
 *   subsistence-level transfer regardless of work status, funded by
 *   compulsory taxation. This story authors the constraint as seen from the
 *   dependency-trap reading: the arrangement converts a temporary income
 *   floor into a durable exit from labor-force participation — with search
 *   pressure removed, recipients' skills, references, and networks decay each
 *   year outside employment, and re-entry wages fall accordingly — while the
 *   transfer rolls onward and the taxing machinery grows with it. The
 *   beneficiaries are the officeholders who convert the dependent
 *   constituency into reliable electoral support and the administrative
 *   apparatus whose budget scales with enrollment; the victims are the
 *   recipients themselves (compounding capability and lifetime-earnings
 *   losses) and the taxpayers who fund transfers the reading classes as
 *   non-productive. The epsilon referent is the standing arrangement under
 *   contest — the universal unconditional transfer regime — assessed by this
 *   reading's own lights; the sibling readings over the same arrangement are
 *   separate constraint stories, not averaged here. The claim/metric gap is
 *   deliberate: the arrangement is CLAIMED as snare from this seat while the
 *   metrics independently describe its operation; the engine measures any
 *   divergence.
 *
 * KEY AGENTS:
 *   - - political_officeholders: Agenda setter (institutional/arbitrage) — sets transfer levels and defends expansion; collects electoral allegiance from the dependent constituency
 *   - - welfare_administrative_bureaucracy: Secondary beneficiary (organized/constrained) — staffs disbursement; budget and headcount scale with enrollment
 *   - - ubi_transfer_recipients: Primary target (powerless/trapped) — receive the transfer; bear compounding skill decay and falling re-entry wages
 *   - - general_taxpayers: Primary target (moderate/constrained) — fund the transfer compulsorily; no opt-out
 *   - - future_taxpayers: Excluded voice (powerless/trapped) — inherit accumulated fiscal commitments; no seat today
 *   - - labor_economists: Analytical observer (analytical/analytical) — estimate labor-supply responses from natural experiments; no vote
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, 0.72).
domain_priors:suppression_score(income_support_conditionality__dependency_trap_reading, 0.6).
domain_priors:theater_ratio(income_support_conditionality__dependency_trap_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_conditionality__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__dependency_trap_reading, snare).
narrative_ontology:human_readable(income_support_conditionality__dependency_trap_reading, "Unconditional Income Support — Dependency Trap Reading (Work-Incentive Erosion)").
narrative_ontology:topic_domain(income_support_conditionality__dependency_trap_reading, "economic/political/social").

domain_priors:requires_active_enforcement(income_support_conditionality__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__dependency_trap_reading, 'b24bb7a4-6910-478d-b579-3e24772c0ad1').
narrative_ontology:cs_kernel_codification('b24bb7a4-6910-478d-b579-3e24772c0ad1', distributed).
narrative_ontology:cs_authority_grounding('b24bb7a4-6910-478d-b579-3e24772c0ad1', distributed).
narrative_ontology:cs_reading_relation('b24bb7a4-6910-478d-b579-3e24772c0ad1', income_support_conditionality__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('b24bb7a4-6910-478d-b579-3e24772c0ad1', income_support_conditionality__wage_subsidy_reading, coexists_with).
narrative_ontology:cs_axiom('b24bb7a4-6910-478d-b579-3e24772c0ad1', foundational, unconditional_transfers_erode_work_capacity).
narrative_ontology:cs_axiom_status(unconditional_transfers_erode_work_capacity, holdable).
narrative_ontology:cs_axiom_grounding('b24bb7a4-6910-478d-b579-3e24772c0ad1', unconditional_transfers_erode_work_capacity, empirically_contingent).
narrative_ontology:cs_axiom('b24bb7a4-6910-478d-b579-3e24772c0ad1', foundational, labor_participation_constitutes_flourishing).
narrative_ontology:cs_axiom_status(labor_participation_constitutes_flourishing, holdable).
narrative_ontology:cs_axiom_grounding('b24bb7a4-6910-478d-b579-3e24772c0ad1', labor_participation_constitutes_flourishing, deontological).
narrative_ontology:cs_reference_frame('b24bb7a4-6910-478d-b579-3e24772c0ad1', labor_participation_social_contract).
narrative_ontology:cs_drift_state('b24bb7a4-6910-478d-b579-3e24772c0ad1', contemporary_universal_expansion_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b24bb7a4-6910-478d-b579-3e24772c0ad1', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__dependency_trap_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, political_officeholders).
narrative_ontology:constraint_beneficiary(income_support_conditionality__dependency_trap_reading, welfare_administrative_bureaucracy).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, ubi_transfer_recipients).
narrative_ontology:constraint_victim(income_support_conditionality__dependency_trap_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set transfer levels, eligibility breadth, and program framing; campaign on protecting and expanding the payment; assemble the legislative coalitions that sustain it. The constituency receiving the transfer reliably supports them at the polls, and proposals to attach work conditions to the payment draw concentrated opposition from that constituency. They fund the program from general revenue they do not personally depend on, and can pivot to other platforms or offices if the politics turn.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, political_officeholders, agenda_setter,
    institutional, biographical, arbitrage, national).

% Staff the disbursement machinery: enrollment, payment processing, fraud screening, outreach. Agency headcount and budget scale with program size and enrollment, giving the agency a durable institutional interest in broad coverage, and its technical memoranda shape what expansions reach the legislative agenda. Career paths inside the agency are tied to the program's continuation; movement to unrelated portfolios is rare.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, welfare_administrative_bureaucracy, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_conditionality__dependency_trap_reading, welfare_administrative_bureaucracy, agenda_setter).

% Receive a monthly payment sufficient for subsistence regardless of work status. With the acute pressure to accept available work removed, job search thins; each year outside formal employment erodes skills, references, and networks, and employers discount the resulting gaps, lowering the wages on offer at re-entry. The payment covers necessities but not the assets or retraining that would fund a return to credential-bearing work, so the practical route off the program runs through accepting entry-level wages after capabilities have already depreciated. Individually they have no leverage over program design; their collective weight registers mainly as defensive electoral mobilization against conditionality.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, ubi_transfer_recipients, payer,
    powerless, immediate, trapped, national).

% Fund the transfer through payroll and general taxation. They receive no direct service in exchange beyond the argued stability dividend, and their tax share rises with enrollment and with each expansion. Opting out individually is not possible; reducing the aggregate burden requires winning national political contests against a constituency that mobilizes defensively around the payment. Emigration or shadow-economy exit carries costs most households cannot absorb.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, general_taxpayers, payer,
    moderate, biographical, constrained, national).

% Will inherit whatever fiscal obligations and program commitments accumulate during the current period. They have no vote, no lobby, and no seat in the legislative coalitions that set transfer levels today; their interests surface only indirectly, through deficit projections and actuarial warnings issued by third parties.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, future_taxpayers, excluded,
    powerless, generational, trapped, national).

% Estimate labor-supply responses to unconditional transfers using natural experiments — Alaska's dividend, GiveDirectly trials, negative income tax pilots, lottery-winner panels — and publish elasticity and skill-decay estimates. Their findings feed legislative testimony and reform proposals on all sides of the conditionality dispute, but they hold no vote and bear none of the program's costs or benefits.
narrative_ontology:constraint_stakeholder(income_support_conditionality__dependency_trap_reading, labor_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__dependency_trap_reading, political_officeholders).
narrative_ontology:fixing_cost_class(income_support_conditionality__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal consumption floor decoupled from employment status: survival is pooled society-wide rather than contingent on any particular job, and the elimination of means-testing removes the administrative overhead and take-up gaps of targeted programs.
% TRANSFER_FUNCTION: Moves taxed income from employed households to every resident irrespective of work status; secondarily moves electoral allegiance toward the officeholders who maintain the payment, and moves years of labor-force participation out of the formal economy as receipt duration lengthens.
% ABSENT_VOICES: Future taxpayers would object to the accumulated fiscal commitments but have no seat — they register only through third-party actuarial warnings. The pre-transfer trajectories of current recipients were altered without a consent point at which the lifetime cost of extended non-participation was priced. Entry-level employers facing thinning applicant pools observe the labor-supply effect but hold no formal seat in program design. All three stand outside the legislative coalition that sustains the arrangement.
% DISAPPEARANCE_RATIONALE: If the unconditional transfer and its taxing machinery vanished overnight, recipient households would lose their subsistence floor immediately and re-enter job search en masse at depressed wages; the administrative agencies built around disbursement would dissolve or shrink; the electoral coalitions organized around defending the payment would lose their binding issue; and the fiscal share currently absorbed by taxpayers would release back to households or alternative spending. Every named seat's situation reorganizes — nothing about the current arrangement survives its removal unchanged.
% FOUNDING_PROBLEM: Mass destitution when wage labor fails: industrial downturns, technological displacement, and old-age poverty before pension systems left people outside work with no income and no floor, and earlier means-tested relief reached them late, stigmatized, and partially.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians' documentation of pre-welfare-era destitution and the failure of early means-tested relief corroborates the founding problem's reality from outside the benefiting parties, as do OECD and ILO income-security assessments of unprotected populations. On the status question the parties divide along the kernel's fault line: program defenders attest the protective need is live (persistent unemployment and displacement risk), while labor economists publishing disincentive findings and taxpayer-side reform coalitions attest that the arrangement's operative function has shifted toward retention and patronage. No source outside the benefiting parties attests that the original protective function remains the dominant one.
narrative_ontology:disappearance_verdict(income_support_conditionality__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_conditionality__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__dependency_trap_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because it runs on two channels at once: recipients surrender skill formation and lifetime-earnings trajectory, and taxpayers surrender taxed income funding transfers this reading classes as non-productive; both channels scale with enrollment and duration. Suppression (0.60) is temporal-compounding rather than legal-barrier suppression: there is no means-test cliff to trip, but each year outside employment raises the effective cost of return through skill decay, reference-network attrition, and employer discounting of résumé gaps, while taxpayer exit is blocked by compulsory taxation. Theater ratio (0.42) reflects a growing share of activity devoted to performing compassion — pilot announcements, dignity framing, expansion ceremonies — relative to the protective function the founding problem specified. Accessibility collapse is moderate (0.45): work, relocation, and informal insurance remain nominally available but degrade with duration of receipt, so alternatives persist yet weaken. Resistance (0.60) is real and recurring: workfare reform movements, taxpayer coalitions, and conditionality legislation repeatedly contest the arrangement. The measurement series run on one shared time grid (t = 0, 10, 20, 30, 40, 50) with every tracked metric authored at every point; the trajectories are monotonic ratchets, not cycles — expansion begets enrollment duration, duration begets dependence, dependence begets defensive electoral mobilization, which begets further expansion.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute very different types from the same structural data. From the officeholder seat the arrangement is compassionate governance it designed, funds from general revenue it does not depend on, and defends against conditionality amendments; from the recipient seat the same payment is a floor that quietly repriced its future labor at ever-lower wages; from the taxpayer seat it is a compulsory levy with no service rendered in exchange. The bureaucracy seat sits near the beneficiary pole with constrained exit — careers are welded to the agency. The engine computes this divergence from the power/exit asymmetries (institutional-arbitrage setters versus powerless-trapped and moderate-constrained payers); the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: political_officeholders and welfare_administrative_bureaucracy sit near the subsidized end (low d, low or negative effective extraction). Victim declarations drive the opposite pole: general_taxpayers bear the fiscal channel with constrained exit (high d). ubi_transfer_recipients are the subtle case — cash flows TO them, but this reading's structural claim is that the cash is the retention mechanism: it purchases the non-participation whose costs (skill decay, scarred trajectories) the recipients themselves bear, with trapped exit pushing them toward the full-target end despite the material inflow. The derivation from victim declaration plus trapped exit produces that high-d placement directly, so no directionality override is needed. Future taxpayers are excluded rather than seated — they feed the absent-voices answer, not the directionality arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting survival when wage labor fails — has not simply died: destitution risk persists, so the arrangement cannot be dismissed as a zombie on the dead-mandate test alone. But the status is contested, not live: this reading holds the protective function has been substantially superseded by a patronage-and-retention function, while defenders hold the protective need is as urgent as ever. The classification machinery prevents two mislabelings symmetrically. Against the rope mislabel: declaring the victim set and the enforcement data blocks the safety-net coordination story from computing as pure coordination — the same structure that pools risk also retains recipients and levies taxpayers, and the tangled-rope gate would demand naming who is coordinated and who pays. Against the piton mislabel: the gains demonstrably accrue to named seats (officeholder electoral returns, bureaucratic budgets), so the diffuse-gains cell — the piton signature — does not obtain; a captured arrangement stays captured under either cost class, and the prohibitive cost-to-fix (withdrawing transfers from a mobilized dependent constituency) explains persistence without invoking inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_multiplicity,
    'This file instantiates only the dependency_trap_reading of the income_support_conditionality kernel; the freedom_floor_reading and wage_subsidy_reading instantiate structurally different constraints over the same transfer arrangement. Which reading governs the arrangement''s classification?',
    'Side-by-side compilation of the three reading-files over the identical structural referent (an unconditional transfer regime), comparing computed types, victim sets, and epsilon values across readings.',
    'Under the freedom_floor_reading, recipients flip from victims to beneficiaries and epsilon collapses toward the coordination floor; under the wage_subsidy_reading, employers enter the beneficiary set while the taxpayer channel persists. The snare verdict holds only within this reading''s structural commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_multiplicity, conceptual, 'One kernel, three readings: this constraint is the dependency-trap instantiation only; sibling readings are separate files with different victim sets.').

omega_variable(
    labor_supply_response_sign,
    'Does unconditional transfer receipt actually reduce employment and accelerate skill decay, and by how much?',
    'Natural experiments — Alaska permanent fund dividends, GiveDirectly trials, negative income tax pilots, lottery-winner panels — measuring employment, hours, and skill proxies over multi-year horizons.',
    'A null or small labor-supply response dissolves the trap mechanism and removes the recipient victim channel, collapsing this reading''s structure toward the freedom-floor sibling; a large, cumulative response confirms trapped exit and sustains the snare profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_response_sign, empirical, 'The empirical crux on which this reading stands or falls: the sign and magnitude of the labor-supply response to unconditional transfers.').

omega_variable(
    exit_cost_compounding,
    'Is the re-entry cost of transfer-supported non-employment cumulative and persistent (scarring), or transient?',
    'Longitudinal wage trajectories after multi-year non-employment spells among transfer recipients versus matched controls, tracking skill proxies, reference networks, and offered wages at re-entry.',
    'Persistent scarring locks recipients'' exit options at trapped and sustains high effective extraction; transient costs downgrade exit to constrained and pull the computed type toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_compounding, empirical, 'Whether the exit-cost mechanism compounds over duration of receipt or washes out.').

omega_variable(
    non_work_normative_valence,
    'Is time outside employment a capability loss borne by the recipient (harm), or acquired freedom to refuse coercive work (benefit)?',
    'Not resolvable by data alone: it turns on whether flourishing is indexed to labor-market participation (this reading''s axiom) or to the option value of refusal (the freedom-floor sibling''s axiom). Resolvable only at the level of framework choice between readings.',
    'Determines whether recipients sit in the victim set at all; flipping the valence flips this reading''s central victim channel and reverses the directionality of the recipient seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_work_normative_valence, preference, 'The normative dispute over the valence of non-work that separates this reading from its freedom-floor sibling.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__dependency_trap_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dependency_trap_tr_t0, income_support_conditionality__dependency_trap_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dependency_trap_tr_t10, income_support_conditionality__dependency_trap_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(dependency_trap_tr_t20, income_support_conditionality__dependency_trap_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement(dependency_trap_tr_t30, income_support_conditionality__dependency_trap_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(dependency_trap_tr_t40, income_support_conditionality__dependency_trap_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(dependency_trap_tr_t50, income_support_conditionality__dependency_trap_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(dependency_trap_be_t0, income_support_conditionality__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dependency_trap_be_t10, income_support_conditionality__dependency_trap_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dependency_trap_be_t20, income_support_conditionality__dependency_trap_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(dependency_trap_be_t30, income_support_conditionality__dependency_trap_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(dependency_trap_be_t40, income_support_conditionality__dependency_trap_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(dependency_trap_be_t50, income_support_conditionality__dependency_trap_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(dependency_trap_su_t0, income_support_conditionality__dependency_trap_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dependency_trap_su_t10, income_support_conditionality__dependency_trap_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(dependency_trap_su_t20, income_support_conditionality__dependency_trap_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(dependency_trap_su_t30, income_support_conditionality__dependency_trap_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(dependency_trap_su_t40, income_support_conditionality__dependency_trap_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(dependency_trap_su_t50, income_support_conditionality__dependency_trap_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__dependency_trap_reading, wage_subsidy_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'unconditional income support' covers three structurally distinct claims, decomposed per the epsilon-invariance principle into a three-story constraint family sharing one referent (the unconditional transfer regime) but not one epsilon: dependency_trap_reading (this file — recipients and taxpayers as victims, snare), freedom_floor_reading (recipients as beneficiaries, near-zero extraction), and wage_subsidy_reading (employers as beneficiaries, taxpayers as victims). The files link via affects_constraints. The upstream empirical layer — the labor-supply natural-experiment literature — feeds all three: this reading cites disincentive findings as confirming evidence, while the freedom-floor sibling cites null-result findings as confirming evidence; the family shares a referent, not a verdict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
