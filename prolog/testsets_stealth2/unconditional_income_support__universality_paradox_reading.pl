% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__universality_paradox_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__universality_paradox_reading, []).

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
 *   constraint_id: unconditional_income_support__universality_paradox_reading
 *   human_readable: Unconditional Income Support as Cross-Ideological Trojan Horse (Universality Paradox Reading)
 *   domain: political economy/social policy/welfare state theory
 *
 * SUMMARY:
 *   Unconditional income support — negative income taxes, universal
 *   dividends, refundable credits, pilot payments — attracts sponsors from
 *   incompatible ideological camps, and this story authors the reading on
 *   which that appeal is the point: the vehicle functions as a Trojan horse
 *   whose constructive ambiguity lets libertarians, progressives, and
 *   administrators each describe the same payment schedule as their own
 *   proposal. Taxing-back research finds distributional outcomes converge
 *   across implementation paths at equal budget, so the visible design war is
 *   substantially performative while the durable products are coalition
 *   credit for political entrepreneurs, rhetorical flexibility for policy
 *   designers, budget diversion away from need-calibrated targeted programs,
 *   and the defeat of stable evaluation. CONSTRAINT FAMILY: this is one of
 *   three readings of the kernel unconditional_income_support. The
 *   freedom_floor reading authors the same vehicles as autonomy
 *   infrastructure (lower epsilon, victims concentrated among labor-market
 *   coercers); the dependency_trap reading authors them as
 *   incentive-distorting subsidy (higher epsilon, taxpayers as victims,
 *   recipients flipped toward beneficiary). Epsilon differs across the family
 *   because each reading assesses the same referent — the standing vehicles
 *   as operated — by its own lights; this reading's lights emphasize
 *   political form over material flow, yielding low-but-nonzero epsilon. The
 *   referent is the standing arrangement, never an endorsed alternative: this
 *   reading is diagnostic rather than advocative. Claim and metrics are
 *   authored independently — the tangled_rope claim states what this reading
 *   takes the structure to be; the metrics describe observed operation.
 *
 * KEY AGENTS:
 *   - political_entrepreneurs: primary beneficiary and agenda setter (organized/mobile) — collects coalition credit by marketing one schedule under incompatible descriptions
 *   - policy_designers: secondary beneficiary (institutional/mobile) — collects advisory influence and career capital by supplying each description on demand
 *   - targeted_program_recipients: primary target (powerless/trapped) — absorbs consolidation and cuts justified by universality talk; flat payments rarely match assessed need
 *   - independent_policy_evaluators: secondary target (moderate/constrained) — scoring is defeated by the vehicle's shifting description
 *   - universal_dividend_households: coordinated participant (organized/constrained) — receives the payment; net position similar across rival designs
 *   - net_contributor_taxpayers: funding seat (moderate/constrained) — finances the net transfer; material stake in design detail limited by convergence
 *   - comparative_welfare_researchers: analytical observer — documents the cross-design equivalence the debate routes around
 *   - transparent_targeting_advocates: excluded — would force the question of who loses; sidelined because precision dissolves the coalition
 *   - minimal_state_purists: excluded — would force the substitution question; kept at the edge of the negotiation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__universality_paradox_reading, 0.36).
domain_priors:suppression_score(unconditional_income_support__universality_paradox_reading, 0.48).
domain_priors:theater_ratio(unconditional_income_support__universality_paradox_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, extractiveness, 0.36).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unconditional_income_support__universality_paradox_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__universality_paradox_reading, tangled_rope).
narrative_ontology:human_readable(unconditional_income_support__universality_paradox_reading, "Unconditional Income Support as Cross-Ideological Trojan Horse (Universality Paradox Reading)").
narrative_ontology:topic_domain(unconditional_income_support__universality_paradox_reading, "political economy/social policy/welfare state theory").

domain_priors:requires_active_enforcement(unconditional_income_support__universality_paradox_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__universality_paradox_reading, '8e999910-2f63-400d-a611-40df0e07b8ce').
narrative_ontology:cs_kernel_codification('8e999910-2f63-400d-a611-40df0e07b8ce', distributed).
narrative_ontology:cs_authority_grounding('8e999910-2f63-400d-a611-40df0e07b8ce', distributed).
narrative_ontology:cs_reading_relation('8e999910-2f63-400d-a611-40df0e07b8ce', unconditional_income_support__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('8e999910-2f63-400d-a611-40df0e07b8ce', unconditional_income_support__dependency_trap_reading, influences).
narrative_ontology:cs_axiom('8e999910-2f63-400d-a611-40df0e07b8ce', foundational, implementation_paths_normatively_incompatible).
narrative_ontology:cs_axiom_status(implementation_paths_normatively_incompatible, holdable).
narrative_ontology:cs_axiom_grounding('8e999910-2f63-400d-a611-40df0e07b8ce', implementation_paths_normatively_incompatible, deontological).
narrative_ontology:cs_axiom('8e999910-2f63-400d-a611-40df0e07b8ce', foundational, fiscal_convergence_renders_design_debate_performative).
narrative_ontology:cs_axiom_status(fiscal_convergence_renders_design_debate_performative, holdable).
narrative_ontology:cs_axiom_grounding('8e999910-2f63-400d-a611-40df0e07b8ce', fiscal_convergence_renders_design_debate_performative, empirically_contingent).
narrative_ontology:cs_axiom('8e999910-2f63-400d-a611-40df0e07b8ce', secondary, ambiguity_is_load_bearing_not_incidental).
narrative_ontology:cs_axiom_status(ambiguity_is_load_bearing_not_incidental, holdable).
narrative_ontology:cs_axiom_grounding('8e999910-2f63-400d-a611-40df0e07b8ce', ambiguity_is_load_bearing_not_incidental, instrumental).
narrative_ontology:cs_reference_frame('8e999910-2f63-400d-a611-40df0e07b8ce', constructive_ambiguity_coalition_vehicle).
narrative_ontology:cs_drift_state('8e999910-2f63-400d-a611-40df0e07b8ce', contemporary_post_pandemic_transfer_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8e999910-2f63-400d-a611-40df0e07b8ce', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__universality_paradox_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, policy_designers).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, independent_policy_evaluators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__universality_paradox_reading, universal_dividend_households).
narrative_ontology:constraint_victim(unconditional_income_support__universality_paradox_reading, net_contributor_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legislators, movement founders, and campaign operatives who carry the proposal across partisan lines. They market the same payment schedule to libertarian audiences as a replacement for bureaucratic welfare, to progressive audiences as a stigma-free floor, and to administrative audiences as a simplification — without committing to any one description. What flows to them: endorsements, donor networks, media ownership of the issue, and agenda control in coalition negotiations. Exit looks like pivoting to adjacent issues; their coalition assets are portable.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, political_entrepreneurs, agenda_setter,
    organized, biographical, mobile, national).

% Economists and technocratic staff in ministries, budget offices, and think tanks who draft the payment formulas and phase-out schedules. The same schedule can be described as a negative income tax, a demogrant with surtax, or a consolidated refundable credit, and they supply each description on request. What flows to them: advisory influence, publication and career capital, and continued relevance across administrations. They also draft the enabling legislation, so they share in setting the technical agenda. Exit looks like movement between governments, universities, and multilateral institutions.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, policy_designers, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__universality_paradox_reading, policy_designers, agenda_setter).

% Households across the income distribution who receive the periodic payment. Lower-income households keep most of it; higher-income households see most of it returned through taxation or reduced credits. The payment arrives without application, means testing, or stigma. What flows to them: cash and simplified dealings with the state. Exit is not meaningful — participation comes through ordinary residence and tax liability.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, universal_dividend_households, beneficiary,
    organized, biographical, constrained, national).

% Households and firms above the clawback threshold whose taxes finance the net transfer. Under the convergence finding their net contribution is broadly similar whichever implementation path is chosen, so their material stake in the design debate is limited; their exposure runs to the aggregate tax level rather than to design detail. Exit options are the ordinary ones — relocation and tax planning — realistically available mainly at the top of this group.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, net_contributor_taxpayers, payer,
    moderate, biographical, constrained, national).

% Households relying on means-tested programs — disability support, housing vouchers, food assistance — calibrated to assessed need. When universal payments are adopted or proposed, consolidation arguments follow: overlapping targeted programs are folded in, capped, or cut on the grounds that the universal payment now covers the need. Because a flat payment rarely matches an individual household's assessed need, many of these households come out behind. Exit from the situation is limited by the same need that qualified them for the targeted programs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, targeted_program_recipients, payer,
    powerless, immediate, trapped, national).

% Budget offices, statistical agencies, and academic evaluators tasked with scoring who gains and loses. The vehicle's deliberately flexible description defeats stable scoring: each faction quotes the description that flatters its case, and evaluation output is absorbed into whichever narrative is running. Professional effort flows out; what flows back is diminished purchase — findings settle nothing because the object of evaluation keeps changing shape. Exit means leaving public-finance work altogether.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, independent_policy_evaluators, payer,
    moderate, generational, constrained, national).

% Academic researchers comparing negative income tax experiments, dividend programs, refundable credit expansions, and pilots across jurisdictions. They document that distributional outcomes converge across implementation paths once tax-back schedules are held budget-neutral, and they publish the equivalence results the political debate routes around. They hold no stake in adoption and see the full structure from outside.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, comparative_welfare_researchers, observer,
    analytical, generational, analytical, global).

% Anti-poverty lawyers, caseworker associations, and scholars who insist any redistribution name its losers and who defend need-calibrated support. They would force the question the coalition survives by not asking — which design, who loses what — but they are structurally sidelined: their precision threatens every faction's preferred description, so they are consulted late or not at all.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, transparent_targeting_advocates, excluded,
    moderate, generational, constrained, national).

% Factional advocates who would accept a payment only as an explicit, dollar-for-dollar replacement for existing programs. They would expose that the vehicle's universal framing permits spending to grow rather than substitute, but their candor breaks the cross-ideological coalition, so they are kept at the edge of the negotiation.
narrative_ontology:constraint_stakeholder(unconditional_income_support__universality_paradox_reading, minimal_state_purists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__universality_paradox_reading, political_entrepreneurs).
narrative_ontology:fixing_cost_class(unconditional_income_support__universality_paradox_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a coalition-formation problem: it lets mutually distrustful factions vote for the same fiscal instrument without first resolving their disagreement about what the instrument is for. Constructive ambiguity converts an unsolvable design dispute into a passable bill; the coordination achieved is agreement on the vehicle, not on its purpose.
% TRANSFER_FUNCTION: Materially, moves purchasing power from the broad tax base to the bottom of the income distribution through a flat payment with progressive clawback — an amount that varies little across rival implementation paths at equal budget. Institutionally, moves budget share from need-calibrated targeted programs toward the universal vehicle. Politically, moves agenda control and coalition credit to whoever frames the vehicle, and moves evaluative clarity away from everyone.
% ABSENT_VOICES: Transparent-targeting advocates and minimal-state purists would each force the design question — who loses, what substitutes for what — and both are structurally sidelined because precision dissolves the coalition. Targeted-program recipients are present as statistics but not as negotiators. Future cohorts bearing the consolidated fiscal structure are absent entirely.
% DISAPPEARANCE_RATIONALE: If the vehicle vanished overnight, the coalition built on it dissolves: factions revert to incompatible pure proposals — dollar-for-dollar replacement, defended targeted expansion, pure demogrant — most fail, and the fiscal settlement that eventually emerges differs materially. Targeted-program budgets, clawback thresholds, and recipient benefit levels would all rearrange. The parties dispute which settlement is better, not whether the landscape changes.
% FOUNDING_PROBLEM: Mid-to-late twentieth century welfare states faced a legitimacy and design crisis: means-tested programs were stigmatized and fragmented, imposed high effective marginal tax rates that trapped recipients, and leaked through take-up gaps — while universal proposals were attacked as wasteful transfers to the non-needy. Reformers sought a single legible instrument that could command support across ideological lines where neither pure targeting nor pure universalism could pass alone.
% FOUNDING_PROBLEM_CORROBORATION: Poverty researchers and administrative-burden studies outside the sponsoring coalitions continue to document stigma, take-up gaps, and benefit cliffs; recipient advocacy organizations attest the traps from lived experience; budget-office reports attest the fragmentation. None of these corroborating seats benefits from the vehicle's persistence — the corroboration establishes that the founding problems persist, not that the vehicle solves them.
narrative_ontology:disappearance_verdict(unconditional_income_support__universality_paradox_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__universality_paradox_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__universality_paradox_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__universality_paradox_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__universality_paradox_reading, 0.36, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__universality_paradox_reading_tests).
:- end_tests(unconditional_income_support__universality_paradox_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.36) because the reading's own lights hold that material outcomes converge: at equal budget, rival designs deliver similar net transfers, so the extraction above coordination cost is political rather than fiscal — coalition rents, targeted-budget diversion, and epistemic defeat. Suppression (0.48) is authored as a raw structural property, unscaled by power or scope: the vehicle persists by keeping the design question off the table — procedural gatekeeping, framing capture of evaluation, sidelining of precision advocates — not by participant preference. Theater ratio (0.55) exceeds half because the observable activity (branding wars, pilot hype, duelling white papers) is predominantly framing rather than fiscal-content determination, though the transfer machinery itself is real. Accessibility collapse is low-moderate (0.40): understanding the convergence does not collapse the alternatives — paradoxically it preserves them, since no design dominates and choice among them becomes symbolic. Resistance (0.52) reflects active defense of targeted programs and purist factions on both flanks. The measurement series run on one shared grid (t=0..50) with all three metrics authored at every point; trajectories are monotonic — no oscillation is modeled, and the base_properties values describe the interval-end state. Coalition note: the principal threat to the vehicle's stability is recipient-side coalition formation, which the flat payment's breadth partially pre-empts by giving every income stratum a stake in continuation.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute differently. From the entrepreneur seat the vehicle is a coordination achievement they built and can exit at will — coordination-forward. From the targeted-recipient seat the same structure operates as enforced loss: trapped, need-calibrated benefits traded for a flat payment that under-covers them — extraction-forward. The evaluator seat experiences epistemic extraction: effort in, no settling power out. The researcher seat sees near-equivalent fiscal mathematics — close to neutral structure. The engine derives these divergences from the declared power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for political_entrepreneurs and policy_designers — the vehicle subsidizes their careers, and their mobile exits dampen any residual cost they bear. universal_dividend_households derive low-to-mid directionality: genuine receipt, diffuse financing. net_contributor_taxpayers derive mid-to-high directionality: they fund the net transfer with constrained exit, though convergence caps their design-specific exposure. targeted_program_recipients derive the highest directionality: trapped exit plus lost need-calibrated benefits. independent_policy_evaluators derive high directionality through the payer declaration with constrained exit — their cost is professional efficacy. No directionality overrides are authored: the derivation chain from declarations, power, and exit produces the correct relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding mandate — solving stigma, benefit cliffs, and fragmentation with one legible instrument — remains live, but the vehicle's operative function has shifted toward coalition maintenance, which the founding problem's persistence corroborates from outside the beneficiary set. The tangled_rope classification blocks two mislabels: a pure-snare reading would miss the real coordination (transfers flow, and stigma-free receipt is real for the universal component); a pure-rope reading would miss the asymmetric extraction (targeted budgets divert, evaluation is defeated, and the gains concentrate on seats with mobile exits). Mandatrophy is not declared resolved because the machine still works — the risk it carries is forward drift toward inertial performance if fiscal convergence ever renders the transfers themselves negligible and only the framing remains; the theater_ratio series is the monitor for that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading — the universality-paradox reading — of the unconditional_income_support kernel. Would the freedom_floor or dependency_trap readings of the same policy vehicles assign different beneficiaries, victims, and extraction levels?',
    'Author and compare the sibling stories directly: hold the referent fixed (the same standing vehicles as operated), vary only the reading, and diff the beneficiary/victim sets and epsilon values across the family.',
    'If the freedom_floor reading governs, the victim set shrinks toward labor-market coercers and epsilon falls toward coordination cost; if the dependency_trap reading governs, taxpayers become primary victims and recipients flip toward beneficiary, raising epsilon. Classification of the same statute is reading-relative by construction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three declared readings of the unconditional-income-support kernel.').

omega_variable(
    fiscal_convergence_robustness,
    'Do distributional outcomes actually converge across implementation paths at equal budget, as the taxing-back literature holds, or do designs diverge materially for realistic populations and behavioral responses?',
    'Budget-neutral microsimulation of negative income tax, universal payment with clawback, and expanded refundable credits on the same population and behavioral assumptions, with sensitivity analysis on labor-supply elasticities and take-up rates.',
    'If outcomes diverge substantially, the reading''s low-extraction premise fails — design choice redistributes materially, epsilon rises, and the vehicle looks more purely extractive; if convergence holds robustly, the visible design debate is confirmed as substantially performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_convergence_robustness, empirical, 'Load-bearing empirical claim of this reading: cross-design fiscal equivalence.').

omega_variable(
    strategic_vs_emergent_ambiguity,
    'Is the vehicle''s ambiguity strategically engineered — drafters rejecting precise specification when it was available — or an emergent byproduct of ordinary coalition bargaining?',
    'Process-trace the legislative history: locate moments where a precise specification was on the table and rejected, and map who benefited from each rejection.',
    'Strategic ambiguity attributes the suppression of coherent evaluation to identifiable agenda setters and strengthens the active-enforcement reading; emergent ambiguity diffuses agency and softens the classification toward friction-bearing coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strategic_vs_emergent_ambiguity, empirical, 'Whether the ambiguity that defines this reading is designed or accidental.').

omega_variable(
    universality_cut_causation,
    'Does universality rhetoric actually cause retrenchment of targeted programs, or do targeted budgets decline for independent fiscal reasons that universality talk merely accompanies?',
    'Cross-jurisdiction difference-in-differences on targeted-program budgets surrounding universal-payment adoptions, controlling for fiscal stress and partisan composition.',
    'If causation holds, targeted_program_recipients are genuine victims and the asymmetric-extraction half of the structure stands; if not, the victim set narrows to evaluative harms and epsilon falls further toward the pure-transfer floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_cut_causation, empirical, 'Whether the targeted-program harm is caused by the vehicle or merely coincident with it.').

omega_variable(
    epistemic_harm_standing,
    'Can defeated evaluation and degraded ideological clarity ground victimhood for a seat, or is that harm too diffuse and preference-dependent to count structurally?',
    'Normative-framework decision: adopt or reject epistemic standing in the classification''s harm ontology, then re-derive the victim set from the ruling.',
    'If epistemic harms count, independent_policy_evaluators remain victims and suppression includes the agenda-control mechanism; if not, victims reduce to targeted_program_recipients and measured extraction drops accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_harm_standing, preference, 'Whether the reading''s non-material victim (ideological clarity) maps to a countable seat.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__universality_paradox_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uips_universality_paradox_tr_t0, unconditional_income_support__universality_paradox_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(uips_universality_paradox_tr_t10, unconditional_income_support__universality_paradox_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(uips_universality_paradox_tr_t20, unconditional_income_support__universality_paradox_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(uips_universality_paradox_tr_t30, unconditional_income_support__universality_paradox_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(uips_universality_paradox_tr_t40, unconditional_income_support__universality_paradox_reading, theater_ratio, 40, 0.49).
narrative_ontology:measurement(uips_universality_paradox_tr_t50, unconditional_income_support__universality_paradox_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(uips_universality_paradox_be_t0, unconditional_income_support__universality_paradox_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(uips_universality_paradox_be_t10, unconditional_income_support__universality_paradox_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(uips_universality_paradox_be_t20, unconditional_income_support__universality_paradox_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement(uips_universality_paradox_be_t30, unconditional_income_support__universality_paradox_reading, base_extractiveness, 30, 0.33).
narrative_ontology:measurement(uips_universality_paradox_be_t40, unconditional_income_support__universality_paradox_reading, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(uips_universality_paradox_be_t50, unconditional_income_support__universality_paradox_reading, base_extractiveness, 50, 0.36).

% Suppression requirement over time
narrative_ontology:measurement(uips_universality_paradox_su_t0, unconditional_income_support__universality_paradox_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(uips_universality_paradox_su_t10, unconditional_income_support__universality_paradox_reading, suppression_requirement, 10, 0.34).
narrative_ontology:measurement(uips_universality_paradox_su_t20, unconditional_income_support__universality_paradox_reading, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(uips_universality_paradox_su_t30, unconditional_income_support__universality_paradox_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(uips_universality_paradox_su_t40, unconditional_income_support__universality_paradox_reading, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(uips_universality_paradox_su_t50, unconditional_income_support__universality_paradox_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__universality_paradox_reading, identity_coordination).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__universality_paradox_reading, unconditional_income_support__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per epsilon-invariance: the colloquial label 'unconditional income support / basic income' covers three structurally distinct claims — autonomy infrastructure (freedom_floor_reading), incentive distortion (dependency_trap_reading), and coalition-sustaining political ambiguity (this file). Each member authors its own epsilon, beneficiary/victim sets, and type over the same referent (the standing vehicles as operated). They are linked here because the upstream empirical result — taxing-back distributional equivalence — feeds this reading's central claim, and this reading's ambiguity critique in turn changes the legitimacy conditions under which the sibling readings are marketed. Orphaning any member would hide the reading-relativity of the classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
