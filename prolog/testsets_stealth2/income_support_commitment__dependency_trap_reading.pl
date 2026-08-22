% ============================================================================
% CONSTRAINT STORY: income_support_commitment__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__dependency_trap_reading, []).

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
 *   constraint_id: income_support_commitment__dependency_trap_reading
 *   human_readable: Unconditional Income Support — Dependency-Trap Reading
 *   domain: political economy / social policy / welfare state theory
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the dependency_trap_reading — of the
 *   income_support_commitment kernel: the standing statutory and practical
 *   commitment to unconditional income support. Per Rule 1, the contest is
 *   NOT described inside the constraint: the referent of epsilon is the
 *   standing arrangement of unconditional income support as the
 *   dependency-trap reading assesses it (never the arrangement this reading
 *   would prefer, and never averaged across readings). The reading holds that
 *   the arrangement carries a genuine coordination function (an unconditional
 *   floor that prevents destitution and stabilizes demand) while
 *   simultaneously extracting along two asymmetries: purchasing power moves
 *   from productive taxpayers to non-participation, and continued collection
 *   erodes recipients' own re-entry capability — the trap. KEY AGENTS (by
 *   structural relationship): - nonparticipating_recipients: primary
 *   beneficiary (organized/constrained) — collects the transfer with no work
 *   condition - skill_atrophying_recipients: concealed target
 *   (powerless/trapped) — collects while re-entry options decay -
 *   working_taxpayers: primary target (organized/constrained) — finances the
 *   transfer - welfare_bureaucracy: administrator-collector
 *   (institutional/mobile) - welfare_state_legislators:
 *   agenda-setter-collector (institutional/mobile) -
 *   working_poor_nonrecipients: excluded voice (powerless/constrained) -
 *   labour_economists: analytical observer (analytical/analytical). Sibling
 *   readings are separate constraint files linked through
 *   network.affects_constraints; their content enters this file only through
 *   omega variables and kernel_context.
 *
 * KEY AGENTS:
 *   - nonparticipating_recipients: primary beneficiary (organized/constrained) — collects the unconditional transfer; job-search intensity declines with collection duration
 *   - skill_atrophying_recipients: concealed target (powerless/trapped) — collects while skills, references, and networks depreciate; the reading's signature vantage
 *   - working_taxpayers: primary target (organized/constrained) — bears the tax levy financing the floor; no opt-out short of emigration
 *   - welfare_bureaucracy: agenda-setter with secondary beneficiary position (institutional/mobile) — staffing and budgets scale with program size
 *   - welfare_state_legislators: agenda-setter with secondary beneficiary position (institutional/mobile) — harvest concentrated electoral support from recipient constituencies
 *   - working_poor_nonrecipients: excluded (powerless/constrained) — equally poor, fully employed, receives nothing; no seat in program governance
 *   - labour_economists: analytical observer (analytical/analytical) — designs pilots and panels measuring labor supply and duration dependence from outside the program budget
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, 0.62).
domain_priors:suppression_score(income_support_commitment__dependency_trap_reading, 0.57).
domain_priors:theater_ratio(income_support_commitment__dependency_trap_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(income_support_commitment__dependency_trap_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__dependency_trap_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__dependency_trap_reading, "Unconditional Income Support — Dependency-Trap Reading").
narrative_ontology:topic_domain(income_support_commitment__dependency_trap_reading, "political economy / social policy / welfare state theory").

domain_priors:requires_active_enforcement(income_support_commitment__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__dependency_trap_reading, '0985195a-2a81-4fbd-acbb-bd09c2f260d0').
narrative_ontology:cs_kernel_codification('0985195a-2a81-4fbd-acbb-bd09c2f260d0', formalized).
narrative_ontology:cs_authority_grounding('0985195a-2a81-4fbd-acbb-bd09c2f260d0', lineage).
narrative_ontology:cs_interpretation_layer_present('0985195a-2a81-4fbd-acbb-bd09c2f260d0').
narrative_ontology:cs_reading_relation('0985195a-2a81-4fbd-acbb-bd09c2f260d0', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('0985195a-2a81-4fbd-acbb-bd09c2f260d0', income_support_commitment__targeting_efficiency_reading, influences).
narrative_ontology:cs_axiom('0985195a-2a81-4fbd-acbb-bd09c2f260d0', foundational, labor_force_attachment_preserves_capability).
narrative_ontology:cs_axiom_status(labor_force_attachment_preserves_capability, holdable).
narrative_ontology:cs_axiom_grounding('0985195a-2a81-4fbd-acbb-bd09c2f260d0', labor_force_attachment_preserves_capability, empirically_contingent).
narrative_ontology:cs_axiom('0985195a-2a81-4fbd-acbb-bd09c2f260d0', secondary, transfer_expansion_ratchet_self_reinforcing).
narrative_ontology:cs_axiom_status(transfer_expansion_ratchet_self_reinforcing, holdable).
narrative_ontology:cs_axiom_grounding('0985195a-2a81-4fbd-acbb-bd09c2f260d0', transfer_expansion_ratchet_self_reinforcing, empirically_contingent).
narrative_ontology:cs_reference_frame('0985195a-2a81-4fbd-acbb-bd09c2f260d0', transitional_floor_preserving_work_attachment).
narrative_ontology:cs_drift_state('0985195a-2a81-4fbd-acbb-bd09c2f260d0', post_pandemic_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0985195a-2a81-4fbd-acbb-bd09c2f260d0', '').
narrative_ontology:cs_kernel_id(income_support_commitment__dependency_trap_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, nonparticipating_recipients).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, working_taxpayers).
narrative_ontology:constraint_victim(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, welfare_bureaucracy).
narrative_ontology:constraint_beneficiary(income_support_commitment__dependency_trap_reading, welfare_state_legislators).
narrative_ontology:constraint_vindicates(income_support_commitment__dependency_trap_reading, social_right_to_income_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional periodic payment sufficient to cover basic costs, with no work-status condition attached. Some combine the payment with occasional or informal earnings; job-search intensity declines the longer collection continues. Leaving the arrangement means forfeiting the floor entirely, so the practical alternative to collecting is full reliance on market earnings at whatever wage the current labor market offers. As a geographically concentrated constituency they vote at measurable rates and are visibly courted during expansion debates.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, nonparticipating_recipients, beneficiary,
    organized, biographical, constrained, national).

% Collect the same payment after job loss or early labor market exit and intend to return to work. Each year out of employment narrows their current skills, references, and networks, so the wage offers available on re-entry fall below the ones they left. The payment keeps them housed and fed while their re-entry options deteriorate; stopping collection does not restore the lost attachments. They are spoken for in policy debate chiefly by organizations committed to the program's continuation rather than its reform.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, skill_atrophying_recipients, beneficiary).

% Pay the payroll and income levies that finance the payment. They cannot decline the levy, and escaping it means leaving jobs, housing, and family networks behind. Their returns from the arrangement are indirect at most — macroeconomic stabilization, less visible destitution — while the direct financial flow runs outward from their households. They organize episodically around reform campaigns and work requirements, with recurring but incomplete political traction.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_taxpayers, payer,
    organized, biographical, constrained, national).

% Administers payment processing, identity verification, fraud control, and reporting for the program. Agency staffing, budgets, and career ladders scale with program size and complexity. Staff move between programs and ministries carrying administrative careers with them, and the agency's institutional memory and procedural ownership give it decisive weight in how any reform would actually be implemented.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_bureaucracy, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, welfare_bureaucracy, beneficiary).

% Vote the program's budget, set payment levels, and decide expansions or consolidations on electoral cycles. Recipient households concentrate geographically and vote at measurable rates, so widening the payment builds identifiable electoral support while trimming it mobilizes opposition without producing visible gratitude. Careers continue through ministerial rotation and lobbying after office, in circles where program scale counts as accomplishment.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, welfare_state_legislators, agenda_setter,
    institutional, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__dependency_trap_reading, welfare_state_legislators, beneficiary).

% Work full schedules at low wages and receive nothing from the arrangement beyond what their own taxes indirectly channel to others. They are not organized around this program, hold no seat in its administration, and their objection — that the same public money rewards non-participation while their participation buys them little — reaches policy debate mainly through third parties and advocacy intermediaries.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, working_poor_nonrecipients, excluded,
    powerless, biographical, constrained, national).

% Design and analyze pilot programs, natural experiments, and longitudinal panels measuring labor supply, skill depreciation, and duration dependence under unconditional transfers. They publish in journals and advise ministries from outside the program's budget, and their findings cut both for and against the arrangement depending on the evidence — which makes them the nearest thing the arrangement has to a disinterested auditor.
narrative_ontology:constraint_stakeholder(income_support_commitment__dependency_trap_reading, labour_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__dependency_trap_reading, nonparticipating_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an income floor that arrives without work or need tests: it solves the coordination problems of income volatility, administrative exclusion from means-tested systems, and poverty gaps at eligibility boundaries, by paying everyone and financing the floor from the general tax base.
% TRANSFER_FUNCTION: Moves purchasing power from employed and asset-holding households, via broad taxation, to all resident households regardless of work status; the net flow concentrates on households with little market income, including those that reduce or exit labor supply.
% ABSENT_VOICES: Working-poor households that receive nothing are not seated anywhere in the program's governance; future taxpayers who will service the accumulated fiscal commitments are constitutionally absent from every decision that accumulates them; low-wage employers facing thinned labor supply act through industry associations rather than program channels; and skill-atrophying recipients are voiced chiefly by organizations committed to the program's continuation, not its reform.
% DISAPPEARANCE_RATIONALE: Overnight removal would strip the income floor from tens of millions of households, flood low-wage labor markets with urgent job-seekers, and overwhelm local charities and municipal relief within weeks, forcing emergency re-legislation; the fiscal side would unwind gradually, but the household side of the world rearranges immediately.
% FOUNDING_PROBLEM: Mass industrial destitution that poor-law and charitable systems handled punitively and unreliably: the founding aim was subsistence guaranteed as a matter of right, independent of employer, family, or local parish discretion.
% FOUNDING_PROBLEM_CORROBORATION: Social and economic historians corroborate the founding problem from outside any benefiting party, using pre-transfer-era mortality and destitution records and poor-law archives. On status: statistical agencies and cross-national poverty research attest that severe material deprivation persists at the margins, supporting a live residual problem; labour-economic studies of benefit duration and reservation wages, published outside the program's budget, attest effects the founders did not target. No source outside the beneficiary coalition attests that the arrangement in its current unconditional form remains the right instrument — that attestation comes only from within it.
narrative_ontology:disappearance_verdict(income_support_commitment__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__dependency_trap_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__dependency_trap_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__dependency_trap_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__dependency_trap_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claim and metrics are authored independently. CLAIMED TYPE: tangled_rope, because this reading's own structure contains both halves — a real coordination function (unconditional floor, no means-test exclusion, automatic stabilization) AND asymmetric extraction through the same structure (productive-to-non-productive transfer plus capability erosion in the recipient class itself), held in place by active enforcement (compulsory taxation, statutory entitlement, administrative machinery). METRICS, authored descriptively: extractiveness 0.62 — substantial asymmetric transfer, tempered by the real floor function even this reading concedes; suppression 0.57 — participation is formally voluntary, but the taxpayer levy is compulsory and the recipient's exit capacity decays with duration, so the arrangement holds its shape through a mix of legal compulsion and eroded alternatives rather than consent alone; theater_ratio 0.34 — real money moves (the functional core), but a growing share of activity is legitimation: pilots maintained past their evidentiary purpose, activation schemes that measure compliance rather than re-employment; accessibility_collapse 0.45 — work, targeted aid, charity, and migration remain live alternatives, so understanding the arrangement does not collapse the choice set; resistance 0.60 — sustained taxpayer politics, work-requirement legislation, and recurring reform movements. Suppression is authored as a raw structural property and is deliberately NOT reconciled to extractiveness; only extractiveness gets scaled by directionality and scope downstream. The temporal series run on one shared six-point grid (T=0..30) with all three metrics authored at every point; all three rise together — expanding generosity raises the transfer volume (extractiveness), the legitimation apparatus grows faster than evaluated outcomes (theater), and fiscal-political entrenchment raises the cost of exit from the arrangement for taxpayers (suppression_requirement).
 *
 * PERSPECTIVAL GAP:
 *   Four seats compute four different constraints from the same statutes. From the working_taxpayer seat the arrangement computes as enforced extraction with no domestic exit. From the nonparticipating_recipient seat it computes as unconditional subsidy — the beneficiary pole, damped extraction, possibly net subsidy once directionality scaling runs. From the institutional seats it computes as an administrable portfolio returning electoral support and staffing scale. From the skill_atrophying seat — the reading's signature vantage — it computes as a floor that pays for the loss of one's own re-entry options: income received, capability destroyed, exit narrowing yearly. The engine derives this divergence from power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. nonparticipating_recipients sit at the beneficiary pole (declared beneficiary, organized but exit-constrained — leaving means forfeiting the floor). working_taxpayers sit near the target pole (declared victim, compulsory levy, constrained exit). skill_atrophying_recipients are declared victims and carry secondary beneficiary position: the derivation weights the victim declaration, placing them high-d, which matches the reading's claim that their net position is negative — they pay in capability what they receive in cash. OVERRIDE: one explicit entry for the institutional power atom (d=0.22). Rationale: the structural derivation treats agenda-setters as neutral coordinators near the symmetric midpoint, but this reading holds both institutional seats collect concrete rents — legislators harvest concentrated electoral support from recipient constituencies, and the bureaucracy's budgets and career ladders scale with program size — placing both nearer the beneficiary pole than any neutral-coordinator default would. The override corrects the derivation for exactly this reading-specific rent structure; it is not a substitute for the beneficiary/victim declarations, which stand first.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — destitution handled punitively and unreliably by poor-law and charitable systems — retains a live residual (severe material deprivation persists at the margins), so this is not a clean dead-mandate zombie; the mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges and no zombie flag fires, but the contested genealogy routes scrutiny to the political-ratchet omega. Classification discipline: calling this a snare would erase the genuine floor function that even this reading concedes — real money prevents real destitution, and no single seat captures all the gains (receipt concentrates on recipients, but the arrangement is not coercion-first); calling it a rope would erase the asymmetric capability extraction the reading places at its center. Tangled_rope preserves both halves and forces the enforcement requirement into the open. Mandatrophy resolution here is the reading's core charge: an arrangement built as a floor has, on this reading, outlived its transitional warrant — permanence where bridge was intended — while its defenders read the same permanence as the founding problem simply continuing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (dependency_trap_reading) of the income_support_commitment kernel; would instantiating the freedom_floor_reading instead restructure the beneficiary/victim map and collapse measured extraction?',
    'Compile the sibling reading-stories (freedom_floor_reading, targeting_efficiency_reading) over the same structural facts and compare computed per-seat classifications; the disagreement resolves only at the reading-selection level, not inside this file.',
    'Under the freedom_floor reading, recipients become autonomy-gainers rather than dependents, taxpayers become purchasers of a public good rather than victims, and effective extraction falls toward coordination-cost floors; this file''s tangled_rope verdict would not survive the swap.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame omega: reading-contest over the income_support_commitment kernel; this file authors only the dependency-trap instantiation.').

omega_variable(
    labor_supply_effect_magnitude,
    'How large and how durable are the labor-supply reduction and skill-depreciation effects of unconditional transfers on recipients?',
    'Long-horizon randomized trials and administrative-panel studies measuring employment, earnings trajectories, and skill proxies under unconditional transfers (multi-year basic-income trials, benefit-spell duration-dependence models).',
    'Small transient effects push this reading''s epsilon down toward the freedom_floor reading''s coordination-cost estimate; large persistent effects push epsilon up and harden the trap structure toward a snare-flavored assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_supply_effect_magnitude, empirical, 'Load-bearing empirical uncertainty inside the dependency-trap reading.').

omega_variable(
    trap_design_contingency,
    'Is the dependence dynamic intrinsic to unconditionality itself, or an artifact of design parameters such as interaction with means-tests, withdrawal rates, and the absence of activation services?',
    'Cross-program comparison holding generosity constant: universal dividends without cliffs, unconditional floors wrapped in activation services, and means-tested conditional regimes.',
    'If contingent, the arrangement is a parameter-fixable hybrid (reform dissolves the trap while keeping the floor); if intrinsic, the extraction is structural and the freedom_floor reading loses its empirical footing entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trap_design_contingency, empirical, 'Whether the trap is intrinsic to unconditionality or to specific design parameters.').

omega_variable(
    political_ratchet_operation,
    'Does the electoral feedback loop this reading posits — recipient constituencies rewarding expansion — actually operate, or do beneficiary coalitions remain too diffuse to drive a ratchet?',
    'Roll-call and referendum analysis linking recipient density to expansion votes; historical tracing of program-growth episodes against coalition strength.',
    'If the ratchet does not operate, the arrangement''s persistence needs another explanation (administrative inertia, crisis-driven expansion), weakening this reading''s persistence account and its agenda-setter rent claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_ratchet_operation, empirical, 'Whether the posited political ratchet is real.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__dependency_trap_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__dependency_trap_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t6, income_support_commitment__dependency_trap_reading, theater_ratio, 6, 0.21).
narrative_ontology:measurement_basis(inco_tr_t6, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__dependency_trap_reading, theater_ratio, 12, 0.24).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t18, income_support_commitment__dependency_trap_reading, theater_ratio, 18, 0.27).
narrative_ontology:measurement_basis(inco_tr_t18, observed).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__dependency_trap_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(inco_tr_t24, observed).
narrative_ontology:measurement(inco_tr_t30, income_support_commitment__dependency_trap_reading, theater_ratio, 30, 0.34).
narrative_ontology:measurement_basis(inco_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__dependency_trap_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t6, income_support_commitment__dependency_trap_reading, base_extractiveness, 6, 0.49).
narrative_ontology:measurement_basis(inco_be_t6, observed).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__dependency_trap_reading, base_extractiveness, 12, 0.53).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t18, income_support_commitment__dependency_trap_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement_basis(inco_be_t18, observed).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__dependency_trap_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement_basis(inco_be_t24, observed).
narrative_ontology:measurement(inco_be_t30, income_support_commitment__dependency_trap_reading, base_extractiveness, 30, 0.62).
narrative_ontology:measurement_basis(inco_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__dependency_trap_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t6, income_support_commitment__dependency_trap_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement_basis(inco_su_t6, observed).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__dependency_trap_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t18, income_support_commitment__dependency_trap_reading, suppression_requirement, 18, 0.51).
narrative_ontology:measurement_basis(inco_su_t18, observed).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__dependency_trap_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(inco_su_t24, observed).
narrative_ontology:measurement(inco_su_t30, income_support_commitment__dependency_trap_reading, suppression_requirement, 30, 0.57).
narrative_ontology:measurement_basis(inco_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__dependency_trap_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__dependency_trap_reading, income_support_commitment__targeting_efficiency_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the income_support_commitment kernel decomposes into three reading-stories — dependency_trap_reading (this file), freedom_floor_reading, and targeting_efficiency_reading. Same statutory referent, different epsilon: this file authors epsilon 0.62 for the standing arrangement (capability extraction plus taxpayer burden dominate the floor function); freedom_floor authors epsilon near the coordination floor (autonomy purchase dominates); targeting_efficiency authors epsilon keyed to misallocation and leakage rather than dependence. Links run bidirectionally through affects_constraints; no upstream/downstream ordering is asserted — the readings compete rather than compose.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__dependency_trap_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
