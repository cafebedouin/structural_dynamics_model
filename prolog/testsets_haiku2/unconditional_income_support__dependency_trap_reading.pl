% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__dependency_trap_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: unconditional_income_support__dependency_trap_reading
 *   human_readable: Unconditional Income Support as Dependency Trap (Incentive-Distorting Subsidy Reading)
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint instantiates one reading of contested unconditional
 *   income support: the dependency-trap reading frames UBI as an
 *   incentive-distorting subsidy that rewards idleness, crowds out targeted
 *   assistance superior to the universal floor, and redistributes upward to
 *   non-needy populations while extracting from working poor and taxpayers.
 *   The kernel is the commitment to universalizing income support; this
 *   reading emphasizes the extractive effects of that universalization
 *   (substitution, upward redistribution, employment loss). Sibling readings
 *   instantiate different constraints: the freedom-floor reading frames UBI
 *   as autonomy-enabling and labor-market liberating; the
 *   universality-paradox reading frames the political ambiguity itself as the
 *   constraint. This story embodies ONLY the dependency-trap reading's
 *   structural claims—high extraction from working poor via program
 *   replacement, transfer to non-needy, active enforcement of universality
 *   against means-testing. The other readings are separate constraints with
 *   different epsilon, different victims, different beneficiary structures.
 *
 * KEY AGENTS:
 *   - upper_middle_class_unneedy_recipients: Direct beneficiaries of universal transfers; no enforcement burden on them
 *   - working_poor_program_losers: Victims extracted from via program substitution; trapped, powerless, face net loss of $800-2500/month
 *   - taxpayers: Organized payers; constrained exit; bear $1.4 trillion annual fiscal cost
 *   - ubi_political_advocates: Agenda-setters who design universality, enforce against means-testing, collect political capital from the framing
 *   - program_administrators: Institutional beneficiaries from consolidation and administrative overhead
 *   - labor_market_employers: Indirect beneficiaries from reduced wage pressure; employment drops -3.2% in pilots
 *   - research_economists_critical_tradition: Excluded from design conversations despite producing contradictory empirical evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__dependency_trap_reading, 0.82).
domain_priors:suppression_score(unconditional_income_support__dependency_trap_reading, 0.71).
domain_priors:theater_ratio(unconditional_income_support__dependency_trap_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(unconditional_income_support__dependency_trap_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__dependency_trap_reading, snare).
narrative_ontology:human_readable(unconditional_income_support__dependency_trap_reading, "Unconditional Income Support as Dependency Trap (Incentive-Distorting Subsidy Reading)").
narrative_ontology:topic_domain(unconditional_income_support__dependency_trap_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(unconditional_income_support__dependency_trap_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__dependency_trap_reading, '0914565d-e36b-43b9-9946-37711c80e0ce').
narrative_ontology:cs_kernel_codification('0914565d-e36b-43b9-9946-37711c80e0ce', formalized).
narrative_ontology:cs_authority_grounding('0914565d-e36b-43b9-9946-37711c80e0ce', extraction).
narrative_ontology:cs_interpretation_layer_present('0914565d-e36b-43b9-9946-37711c80e0ce').
narrative_ontology:cs_reading_relation('0914565d-e36b-43b9-9946-37711c80e0ce', unconditional_income_support__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('0914565d-e36b-43b9-9946-37711c80e0ce', unconditional_income_support__universality_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('0914565d-e36b-43b9-9946-37711c80e0ce', foundational, universality_masks_extraction).
narrative_ontology:cs_axiom_status(universality_masks_extraction, holdable).
narrative_ontology:cs_axiom_grounding('0914565d-e36b-43b9-9946-37711c80e0ce', universality_masks_extraction, empirically_contingent).
narrative_ontology:cs_axiom('0914565d-e36b-43b9-9946-37711c80e0ce', foundational, program_substitution_inevitable_under_fiscal_constraint).
narrative_ontology:cs_axiom_status(program_substitution_inevitable_under_fiscal_constraint, holdable).
narrative_ontology:cs_axiom_grounding('0914565d-e36b-43b9-9946-37711c80e0ce', program_substitution_inevitable_under_fiscal_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('0914565d-e36b-43b9-9946-37711c80e0ce', targeted_assistance_regime).
narrative_ontology:cs_drift_state('0914565d-e36b-43b9-9946-37711c80e0ce', contemporary_ubi_advocacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0914565d-e36b-43b9-9946-37711c80e0ce', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__dependency_trap_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, upper_middle_class_unneedy_recipients).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, ubi_political_advocates).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, program_administrators).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, working_poor_program_losers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, taxpayers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, targeted_assistance_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unconditional_income_support__dependency_trap_reading, labor_market_employers).
narrative_ontology:constraint_victim(unconditional_income_support__dependency_trap_reading, low_wage_workers_still_employed).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional monthly transfers ($1000-2000 depending on pilot design) despite sufficient existing income. Their participation in the program is presented as universality's strength but constitutes pure transfer to non-needy populations. They exercise full exit options: they can accept or refuse; accepting is simply advantageous. No enforcement needed at their seat.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, upper_middle_class_unneedy_recipients, beneficiary,
    powerful, generational, arbitrage, national).

% Lose targeted assistance programs (housing subsidies, food assistance, childcare vouchers, medical coverage) worth $1800-3500/month that were replaced or defunded to finance UBI. Their UBI payment ($1000-2000/month) does not substitute; they face net loss of support. They cannot exit: they depend on income support entirely and have no alternative sources. The constraint extracts by substituting inferior, universal transfers for superior, targeted ones.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, working_poor_program_losers, payer,
    powerless, biographical, trapped, national).

% Bear the fiscal cost of universalizing the program: estimated $1.4 trillion annually after consolidating replaced programs, or $4200/household/year in additional tax. The constraint enforces this transfer through tax policy and budget reallocation. Exit options are limited: tax avoidance is illegal; exit to lower-tax jurisdictions requires migration; political voice is diluted across millions of payers.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, taxpayers, payer,
    organized, biographical, constrained, national).

% Populations dependent on targeted programs (SSDI recipients, elderly on Supplemental Security Income, disabled workers, families in transition) face program cuts or consolidation into UBI as it scales. The program either explicitly replaces targeted assistance or indirectly crowds it out via fiscal constraints. They cannot exit dependence and face the most extreme extraction: loss of support built around their specific needs.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, targeted_assistance_populations, payer,
    powerless, biographical, trapped, national).

% Set the policy agenda for UBI implementation: design universality, defend against means-testing, frame the program as liberation rather than substitution. They collect political capital, foundation funding, and policy influence from the universality framing. They enforce the universality constraint by resisting means-testing or program-specific design that would segment beneficiaries.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, ubi_political_advocates, agenda_setter,
    institutional, generational, arbitrage, national).

% Manage the UBI rollout, design implementation rules, control eligibility verification and payment systems. They benefit from the program's administrative overhead and political durability; consolidating multiple targeted programs into UBI creates employment and institutional stability. Their role is both administrator and partial beneficiary.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, program_administrators, beneficiary,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(unconditional_income_support__dependency_trap_reading, program_administrators, agenda_setter).

% Benefit indirectly from reduced labor-market pressure: unconditional income support reduces desperation-driven job-seeking, lowering wage pressure on low-wage work. Pilots show -3.2% employment reduction (AEI meta-analysis), meaning fewer workers competing for jobs. Employers can maintain lower wage floors. They do not enforce the constraint directly but benefit from its employment effects.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, labor_market_employers, beneficiary,
    powerful, biographical, arbitrage, national).

% Those who remain employed face wage suppression as labor supply shrinks and employers adjust hiring down rather than raising wages. Some portion of the extraction from taxpayers is captured by employers as rent, not passed to workers. Exit options are limited: they must work to eat, cannot exit labor market participation, and face reduced bargaining power as employment shrinks.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, low_wage_workers_still_employed, payer,
    powerless, biographical, constrained, national).

% Economists who have produced employment-negative findings (AEI meta-analysis, IFS studies, OECD wage-elasticity work) are marginalized in the policy conversation. Their empirical findings contradict the universality framing and are treated as hostile or insufficiently welfarist. They are structurally excluded from implementation design conversations, which are dominated by advocates.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, research_economists_critical_tradition, excluded,
    moderate, biographical, constrained, national).

% Compares UBI implementation across jurisdictions, tracks pilot outcomes, documents actual program substitution patterns, and measures employment and poverty effects. They take testimony from all other seats, analyze pilot data, and can produce evidence that would shift the constraint's perceived type or beneficiary structure.
narrative_ontology:constraint_stakeholder(unconditional_income_support__dependency_trap_reading, observer_comparative_welfare_systems, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unconditional_income_support__dependency_trap_reading, upper_middle_class_unneedy_recipients).
narrative_ontology:fixing_cost_class(unconditional_income_support__dependency_trap_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replacing fragmented, stigmatizing, means-tested welfare with a single, streamlined transfer mechanism that reduces administrative overhead and eliminates eligibility verification delays. The stated coordination problem: welfare bureaucracy is Byzantine and excludes eligible populations through complexity.
% TRANSFER_FUNCTION: Moves money from taxpayers (organized labor, corporate tax base, high-income earners) to all residents above some age threshold, with intended effect of reducing poverty and market-labor coercion. The constraint also transfers from working-poor program-dependent populations to non-needy recipients and employers via substitution and wage suppression.
% ABSENT_VOICES: Targeted-assistance populations (disabled, elderly, families with specific needs) who would argue that their programs should be *expanded*, not consolidated into a universal floor below current support. Research economists documenting employment loss are excluded from implementation planning. Competing policy approaches (means-tested top-up, job guarantees, targeted sectoral support) are not represented in the design conversation.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared and prior targeted programs were restored, working-poor populations would recover $1800-3500/month in support, taxpayer burden would decrease by $1.4 trillion annually, employment in low-wage sectors would increase, and the political advocates' policy agenda would lose its flagship program. The economy would reorganize around the prior targeted structure.
% FOUNDING_PROBLEM: Welfare stigma, bureaucratic exclusion, and means-test traps that discourage work and create administrative friction; also, labor-market desperation and worker powerlessness in wage negotiation.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates attest the problem is live and severe. Labor economists and targeted-assistance advocates attest the founding problem exists but argue targeted programs with better design address it more efficiently. Empirical evidence (pilot employment outcomes) from outside the advocacy community shows -3.2% employment in large pilots, contradicting the 'desperation reduction' framing. No consensus corroboration exists; the founding problem definition itself is contested.
narrative_ontology:disappearance_verdict(unconditional_income_support__dependency_trap_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__dependency_trap_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__dependency_trap_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unconditional_income_support__dependency_trap_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__dependency_trap_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is high (0.82) and rising over the interval because the constraint's core function is to substitute inferior universal transfers for superior targeted ones, concentrating the efficiency loss on already-vulnerable populations. The substitution mechanism is deliberate: universality is defended as ideologically superior to means-testing, but the structural effect is extraction via program replacement. Suppression is high (0.71) because enforcement focuses on maintaining universality against means-testing and on excluding critical research from implementation design—the constraint's persistence depends on blocking alternative policy designs and marginalizing contradictory empirical findings. Theater ratio rises from 0.30 to 0.48 over the interval as pilot employment losses accumulate (AEI: -3.2%) but are reframed as acceptable (incentive effects, work disincentives repackaged as liberation) rather than acknowledged as design failures. Measurements are authored at one shared grid—every metric is valued at every time point. The rising trajectory for extractiveness and suppression reflects the accumulation of pilot data, hardening of the policy architecture, and entrenchment of beneficiary interests in the program.
 *
 * PERSPECTIVAL GAP:
 *   From the upper-middle-class recipient and advocate seats, UBI is liberation and efficiency; from the working-poor and taxpayer seats, it is extraction and program cannibalization. The engine will compute different types at different seats: beneficiaries may experience coordination; payers experience snare. The authored claim (snare) reflects the constraint as experienced from the extraction seats. The metrics (high extractiveness, high suppression) support the snare claim. Advocates would author the same kernel as rope or scaffold; they would author lower extractiveness, lower suppression, and declare beneficiaries as the entire population (not just non-needy recipients). That is their reading, their constraint, their story. This story is the dependency-trap reading only.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: upper-middle-class recipients start at d≈0.0 (pure beneficiary, arbitrage exit, no enforcement burden—full subsidy). Working poor start at d≈1.0 (pure target, trapped exit, net loss via substitution, enforcement burden on them to accept the worse deal). Taxpayers at d≈0.7 (organized power gives them more exit options than working poor, but exit is still constrained—they cannot fully avoid tax). Advocates/administrators at d≈0.1 (institutional power, beneficiary roles, they set the constraint). No directionality overrides needed; the structural data produces appropriate d values. The constraint's persistence depends on keeping working poor in the trapped state and advocates in the beneficiary state; this asymmetry is the snare's core.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (welfare stigma, desperation) is contested in status. The freedom-floor reading claims it is live; the dependency-trap reading asserts it is overstated and that the cure (program substitution) is worse than the disease (existing means-test friction). The disappearance verdict is world_rearranges—if the constraint vanished, prior targeted programs would be restored, working-poor populations would recover support, employment would increase. The mismatch (founding_problem_status=contested, disappearance_verdict=world_rearranges) signals a zombie mandate: the program persists despite contested justification because beneficiaries (non-needy recipients, advocates, employers) have organized around it. Mandatrophy is the core of the snare: the extraction persists not because it solves a founding problem but because the beneficiary coalition has locked in the universality principle as irreversible. This distinguishes snare from temporary scaffold—scaffolds have declared sunsets; this constraint has no exit mechanism built in.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    program_substitution_vs_additive_funding,
    'Is UBI implemented by consolidating and replacing targeted programs (the dependency-trap assumption), or is it funded additively on top of existing targeted assistance?',
    'Audit of pilot program structures and subsequent policy design. If UBI is funded additively and targeted programs are preserved, extraction from working poor disappears and the constraint reclassifies. If substitution is confirmed, extraction is confirmed.',
    'If funded additively: extractiveness drops to ~0.35, victims list shrinks to taxpayers only, snare becomes rope or scaffold. If substitution is structural: extractiveness holds at 0.82+, snare classification holds, mandatrophy signal persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(program_substitution_vs_additive_funding, empirical, 'Whether UBI replaces or supplements targeted assistance.').

omega_variable(
    employment_effect_causality_and_magnitude,
    'Is the -3.2% employment reduction caused by genuine incentive effects (reduced labor-supply desperation, voluntary exit), or by demand-side effects (employers hiring fewer workers given reduced labor-market pressure), or measurement artifact from pilot design?',
    'Distinguish employment supply vs. demand effects via randomized-control-trial design isolating income effects; track wage effects in control cohorts; scale pilots to natural-experiment size to rule out noise.',
    'If supply-side (workers choosing idleness): supports dependency-trap framing and high extractiveness. If demand-side (employers exploiting reduced supply): still extracts from working poor (via wage suppression) but the mechanism differs. If noise: extractiveness plausibly overstated, theater ratio rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_effect_causality_and_magnitude, empirical, 'Causal mechanism of employment reduction in UBI pilots.').

omega_variable(
    universality_enforcement_as_ideological_constraint,
    'Is the commitment to universality (refusing means-testing, including non-needy recipients) ideological self-justification, or does it serve a structural function that would be lost if means-testing were adopted?',
    'Compare outcomes and political durability under means-tested vs. universal designs; examine advocacy literature for explicit defense of universality vs. pragma-justified accounts; track whether means-tested alternatives are considered in policy design.',
    'If purely ideological: universality is the suppression mechanism itself—it persists because beneficiaries and advocates have locked it in. If functionally justified: extractiveness might be partly legitimate coordination cost. If contested: an omega documenting the reading-dependence persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(universality_enforcement_as_ideological_constraint, conceptual, 'Whether universality is ideological commitmentsor structurally necessary.').

omega_variable(
    redistribution_direction_measurement,
    'Net-net, does unconditional income support redistribute downward to the poor, or upward to the non-needy, or sideways between deciles?',
    'Comprehensive fiscal incidence analysis comparing pre- and post-UBI income distributions, net of all tax and transfer changes, across full population distribution. Disentangle vertical (rich-to-poor) from horizontal (same-income-class redistribution) and perverse (upward-to-non-needy) flows.',
    'Strong downward redistribution: extractiveness overstated, type might reclassify toward rope. Upward or sideways redistribution: supports snare classification and confirms ''redistributes upward to non-needy'' claim. Mixed pattern: omega documents the reading-dependence of redistribution evaluation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(redistribution_direction_measurement, empirical, 'Direction and magnitude of net redistribution under UBI.').

omega_variable(
    kernel_reading_dependence,
    'Is UBI fundamentally a constraint whose type depends on which reading is adopted (which kernel interpretation is operative), or is the type objectively determinable from metrics independent of reading?',
    'Measure epsilon under both freedom-floor and dependency-trap interpretations: same empirical facts (program replacement, employment loss, upward transfers) but valued differently (efficiency loss vs. liberation, incentive effect vs. work disincentive). If epsilon diverges, the constraint is reading-dependent; if constant, the reading is commentary, not structural.',
    'If reading-dependent: this story is one valid interpretation; freedom-floor reading is another; universality-paradox reading is a third. Classification is per-reading. If objective: one reading is correct and others are empirically false. The engine should compute per-seat types and compare to authored claims; if they align within the dependency-trap frame but diverge under other frames, reading-dependence is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_dependence, conceptual, 'Whether UBI constraint type is reading-dependent or objective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__dependency_trap_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unco_tr_t0, unconditional_income_support__dependency_trap_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(unco_tr_t0, projected).
narrative_ontology:measurement(unco_tr_t3, unconditional_income_support__dependency_trap_reading, theater_ratio, 3, 0.37).
narrative_ontology:measurement_basis(unco_tr_t3, observed).
narrative_ontology:measurement(unco_tr_t6, unconditional_income_support__dependency_trap_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement_basis(unco_tr_t6, observed).
narrative_ontology:measurement(unco_tr_t12, unconditional_income_support__dependency_trap_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement_basis(unco_tr_t12, observed).
narrative_ontology:measurement(unco_tr_t18, unconditional_income_support__dependency_trap_reading, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(unco_tr_t18, observed).
narrative_ontology:measurement(unco_tr_t25, unconditional_income_support__dependency_trap_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(unco_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(unco_be_t0, unconditional_income_support__dependency_trap_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(unco_be_t0, projected).
narrative_ontology:measurement(unco_be_t3, unconditional_income_support__dependency_trap_reading, base_extractiveness, 3, 0.71).
narrative_ontology:measurement_basis(unco_be_t3, observed).
narrative_ontology:measurement(unco_be_t6, unconditional_income_support__dependency_trap_reading, base_extractiveness, 6, 0.74).
narrative_ontology:measurement_basis(unco_be_t6, observed).
narrative_ontology:measurement(unco_be_t12, unconditional_income_support__dependency_trap_reading, base_extractiveness, 12, 0.79).
narrative_ontology:measurement_basis(unco_be_t12, observed).
narrative_ontology:measurement(unco_be_t18, unconditional_income_support__dependency_trap_reading, base_extractiveness, 18, 0.81).
narrative_ontology:measurement_basis(unco_be_t18, observed).
narrative_ontology:measurement(unco_be_t25, unconditional_income_support__dependency_trap_reading, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(unco_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(unco_su_t0, unconditional_income_support__dependency_trap_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(unco_su_t0, projected).
narrative_ontology:measurement(unco_su_t3, unconditional_income_support__dependency_trap_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement_basis(unco_su_t3, observed).
narrative_ontology:measurement(unco_su_t6, unconditional_income_support__dependency_trap_reading, suppression_requirement, 6, 0.64).
narrative_ontology:measurement_basis(unco_su_t6, observed).
narrative_ontology:measurement(unco_su_t12, unconditional_income_support__dependency_trap_reading, suppression_requirement, 12, 0.69).
narrative_ontology:measurement_basis(unco_su_t12, observed).
narrative_ontology:measurement(unco_su_t18, unconditional_income_support__dependency_trap_reading, suppression_requirement, 18, 0.7).
narrative_ontology:measurement_basis(unco_su_t18, observed).
narrative_ontology:measurement(unco_su_t25, unconditional_income_support__dependency_trap_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(unco_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__dependency_trap_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unconditional_income_support__dependency_trap_reading, 0.18).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__freedom_floor_reading).
narrative_ontology:affects_constraint(unconditional_income_support__dependency_trap_reading, unconditional_income_support__universality_paradox_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the unconditional_income_support kernel. The dependency_trap_reading frames UBI as extractive substitution and incentive-distortion; the freedom_floor_reading frames UBI as autonomy-enabling and labor-market-liberating; the universality_paradox_reading frames the political incoherence as the constraint itself. All three are valid interpretations of the same policy arrangement, but they instantiate different constraint types with different epsilon values and different beneficiary/victim structures. The network links all three as siblings in the same constraint family. Epsilon values differ because each reading assesses the standing arrangement (UBI policy as operative) from different normative premises: the dependency-trap reading values program replacement and employment loss as extraction; the freedom-floor reading values the same facts as liberation; the universality-paradox reading treats the reading-choice itself as the constraint. The structural data (who benefits, who pays, what flows from whom to whom) is the same across all three stories; the classification diverges because the readings' axioms diverge.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
