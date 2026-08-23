% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Universal Income Swap Assessed by the Targeting-Efficiency Reading
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the targeting_efficiency_reading —
 *   of the contested kernel 'public income support commitment.' The
 *   arrangement under assessment (this reading's ε referent) is the
 *   universal-distribution swap: a single flat annual grant issued to every
 *   resident, financed substantially by consolidating and cannibalizing the
 *   existing stack of means-tested programs (housing assistance, food
 *   assistance, childcare subsidies, earnings-linked tax credits). Through
 *   this reading's own lights the swap presents a coordination surface — one
 *   payment, no paperwork, no stigma, no cliffs — wrapped around an
 *   asymmetric transfer reversal: the exemplar Queens parent whose calibrated
 *   package totals $31,100 falls to a $12,000 flat grant (−$19,100), while
 *   low-need households gain checks sized independently of need. The poor are
 *   simultaneously the arrangement's nominal beneficiaries (they receive the
 *   grant) and its actual targets (they lose the most). Claim and metrics are
 *   independent authored facts: the claimed_type records this reading's
 *   structural verdict on the swap; the metrics record its descriptive
 *   operation. This reading's own endorsed arrangement — the calibrated
 *   means-tested baseline — is a DIFFERENT constraint carried by sibling
 *   stories; its ε is not authored here. KEY AGENTS (by structural
 *   relationship): - deep_need_targeted_recipients: Primary target
 *   (powerless/trapped) — loses the largest share of support under the swap -
 *   disabled_high_needs_recipients: Secondary target (moderate/trapped) —
 *   categorical supplements folded into a median-sized grant -
 *   middle_income_universal_recipients: Primary beneficiary
 *   (organized/mobile) — net gainers from flat checks -
 *   universal_payment_administration: Agenda setter (institutional/arbitrage)
 *   — runs the unified system and defends uniformity -
 *   unstably_housed_deep_poverty_households: Excluded voice
 *   (powerless/trapped) — deepest need, least representation -
 *   low_income_working_adults: Dual-positioned (constrained) — gains the flat
 *   grant, loses work-linked credits - anti_poverty_policy_analysts:
 *   Analytical observer (analytical/analytical) — publishes the gain/loss
 *   ledgers
 *
 * KEY AGENTS:
 *   - deep_need_targeted_recipients: Primary target (powerless/trapped) — bears the largest absolute loss under the swap
 *   - disabled_high_needs_recipients: Secondary target (moderate/trapped) — condition-scaled costs become invisible to a flat formula
 *   - middle_income_universal_recipients: Primary beneficiary (organized/mobile) — the majority constituency that receives and defends the checks
 *   - universal_payment_administration: Agenda setter (institutional/arbitrage) — administers the unified system; its continuity interest aligns it near the beneficiary end
 *   - unstably_housed_deep_poverty_households: Excluded voice (powerless/trapped) — deepest needs, effectively absent from the design conversation
 *   - low_income_working_adults: Dual-positioned payer/beneficiary (constrained) — flat grant replaces work-linked credits; net position varies by household composition
 *   - anti_poverty_policy_analysts: Analytical observer (analytical/analytical) — documents the transfer reversal without holding a stake in the flows
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.75).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.7).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Universal Income Swap Assessed by the Targeting-Efficiency Reading").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "economic/political/social").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, '230486f8-8bb9-4662-813f-ddb92c102c9e').
narrative_ontology:cs_kernel_codification('230486f8-8bb9-4662-813f-ddb92c102c9e', distributed).
narrative_ontology:cs_authority_grounding('230486f8-8bb9-4662-813f-ddb92c102c9e', distributed).
narrative_ontology:cs_reading_relation('230486f8-8bb9-4662-813f-ddb92c102c9e', income_support_commitment__freedom_floor_reading, coexists_with).
narrative_ontology:cs_reading_relation('230486f8-8bb9-4662-813f-ddb92c102c9e', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('230486f8-8bb9-4662-813f-ddb92c102c9e', foundational, support_must_track_demonstrated_need).
narrative_ontology:cs_axiom_status(support_must_track_demonstrated_need, holdable).
narrative_ontology:cs_axiom_grounding('230486f8-8bb9-4662-813f-ddb92c102c9e', support_must_track_demonstrated_need, instrumental).
narrative_ontology:cs_axiom('230486f8-8bb9-4662-813f-ddb92c102c9e', foundational, need_assessment_is_administratively_feasible).
narrative_ontology:cs_axiom_status(need_assessment_is_administratively_feasible, holdable).
narrative_ontology:cs_axiom_grounding('230486f8-8bb9-4662-813f-ddb92c102c9e', need_assessment_is_administratively_feasible, empirically_contingent).
narrative_ontology:cs_reference_frame('230486f8-8bb9-4662-813f-ddb92c102c9e', calibrated_demonstrated_need_baseline).
narrative_ontology:cs_drift_state('230486f8-8bb9-4662-813f-ddb92c102c9e', universal_swap_legislative_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('230486f8-8bb9-4662-813f-ddb92c102c9e', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_income_universal_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, deep_need_targeted_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, disabled_high_needs_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, low_income_working_adults).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, low_income_working_adults).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, flat_grant_sufficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A single parent in a high-cost metro borough currently draws a stacked package of housing assistance, food assistance, childcare subsidy, and earnings-linked tax credits totaling about $31,100 a year, calibrated to local rents and the ages of her children. Under the swap she receives a flat annual grant of about $12,000 with no adjustment for her rent burden or childcare hours. Leaving is not realistic: her housing assistance is tied to her unit and region, her childcare slot and support network are local, and her wages cover less than her costs anywhere nearby.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, deep_need_targeted_recipients, payer,
    powerless, immediate, trapped, local).

% People whose disabilities generate costs — personal care, equipment, accessible transport, medication — that scale with condition severity rather than with income alone. They currently receive categorical supplements layered onto income support precisely because flat formulas miss these costs. The swap folds those supplements into a uniform payment sized to the median recipient, leaving the highest-cost conditions uncovered. Organized advocacy networks speak for them, but individual recipients have little bargaining power and no practical exit from the benefit system.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, disabled_high_needs_recipients, payer,
    moderate, biographical, trapped, national).

% Low-wage workers who currently receive earnings-linked credits that rise with each hour worked. The flat grant pays the same amount at zero hours as at full time, so for many household compositions the swap trades a work-linked benefit stream for a smaller unconditional one. Some gain, some lose, depending on hours, dependents, and state supplements; they can adjust work effort at the margin but cannot escape the trade itself.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, low_income_working_adults, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, low_income_working_adults, beneficiary).

% Households above the poverty line who receive the flat grant and, under partial-clawback financing, keep most of it after tax. Whatever else the arrangement does, it writes them a check, and as a voting majority they constitute the constituency that keeps it in place. Their benefit is portable and survives relocation, job change, and changes in circumstance.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_income_universal_recipients, beneficiary,
    organized, biographical, mobile, national).

% The federal payment agency that issues the uniform grant, absorbed the dismantled eligibility offices, and maintains the payment rolls. Its budget and headcount depend on the unified system continuing. It opposes reinstatement of categorical programs, manages the annual universality defense, and can reorganize its own operations freely if the design changes.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, universal_payment_administration, agenda_setter,
    institutional, generational, arbitrage, national).

% People cycling through shelters, doubled-up couches, and motels with no stable address. They have the deepest needs and the least contact with any consultation process: no reliable address for enrollment notices or comment periods, low voter registration, high mobility. Design documents assume they can be reached; mostly they cannot.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, unstably_housed_deep_poverty_households, excluded,
    powerless, immediate, trapped, local).

% Researchers and advocacy economists who model benefit stacks household-by-household and publish the before-and-after ledgers. They attend hearings, submit regulatory comments, and maintain the public record of who gains and who loses under each swap variant. They hold no stake in the payment flows themselves.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, anti_poverty_policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, middle_income_universal_recipients).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the fragmentation problem of the means-tested patchwork: dozens of programs with separate applications, eligibility offices, recertification cycles, take-up failures, and benefit cliffs are replaced by one uniform payment with no eligibility determination at all.
% TRANSFER_FUNCTION: Moves purchasing power from circumstance-calibrated deep-needs households to the entire population: the exemplar stacked household's $31,100 becomes a $12,000 flat grant, and the freed roughly $19,100 spreads as uniform checks to households regardless of need — net flows run from high-need to low-need households relative to the targeted baseline.
% ABSENT_VOICES: Unstably housed deep-poverty households — the deepest-need seat — are effectively absent: no fixed address for enrollment or comment periods, lowest voter registration and turnout, highest mobility. Severely disabled people unable to navigate consultation are similarly missing. Their objection — that cost-calibration is not stigma, and that flat grants cannot meet scaled needs — appears nowhere in the design record except as second-hand testimony from advocates.
% DISAPPEARANCE_RATIONALE: If the swap vanished overnight, payment would stop for tens of millions of households, the dismantled eligibility infrastructure would need reassembly or replacement, state and municipal budgets would absorb stranded costs, and the cannibalized categorical programs would face immediate restoration pressure — the income-support landscape would visibly reorganize within months.
% FOUNDING_PROBLEM: The targeted patchwork's documented failures: severe under-take-up of benefits by eligible households, administrative burden falling hardest on those least equipped to bear it, benefit cliffs that penalize work, and the stigma of means-tested claiming.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the universalist benefiting coalition: administrative-burden scholarship and take-up audits document the founding failures directly (eligibility-churn loss rates, non-take-up surveys, cliff-effect studies), and government accountability reviews attest them institutionally. Those same sources, notably, dispute the remedy — their finding is that take-up failure argues for easier targeting, not for abolishing the target — so the founding problem is attested while the swap's solution to it is contested.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.75, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   All tracked metrics run on one shared seven-point grid (T=0..24) — one time grid per story, every metric authored at every point. Early points carry basis=observed (documented pilot programs, legislative scoring exercises, published benefit-stack ledgers, take-up research); late points carry basis=projected (authored expectations for the implementation phase, flagged as such). Extractiveness ends at 0.75: at full swap, roughly sixty percent of the exemplar household's support disappears while the freed funds spread across the whole population — a steep transfer gradient against the deepest need. Suppression ends at 0.70: the swap's persistence depends on active political enforcement — majority-beneficiary lock-in, dismantled eligibility infrastructure that cannot be cheaply reassembled, and framing that recasts need-assessment itself as stigma. Theater_ratio ends at 0.46: the simplicity and dignity arguments are not empty (fragmentation, take-up failure, and cliff effects are real, documented problems), but a growing share of the arrangement's public activity defends uniformity as an end in itself rather than delivering payment mechanics. The suppression_requirement series is authored deliberately rather than left static: this story specifically traces enforcement-capacity buildup — anti-retargeting machinery, administrative consolidation, constituency defense — not merely extraction drift, and the rising trajectory models that hardening. Resistance 0.55 reflects real but minority-positioned opposition (analyst testimony, disability advocacy, some legislators). Accessibility_collapse 0.60 reflects that retargeting remains formally possible but collapses politically once checks reach majorities.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute divergent classifications from identical structure. The deep-need payer seats (powerless, trapped, near-full-target directionality) experience the swap as enforced extraction with no exit. The middle-income beneficiary seat (organized, mobile) experiences genuine coordination value — one simple payment — and subsidization, computing something coordination-like. The administration seat sits near the beneficiary end through institutional continuity interest: it neither pays the transfer nor receives it, but its budget and mandate depend on the arrangement holding. The analyst seat sees the full ledger and holds no stake in either side. Coalition potential: the victim class is large but diffuse — geographically scattered, low-turnout, administratively hard to reach — so its theoretical coalition power is unrealized; the arrangement's durability depends partly on exactly this mobilization failure, which is why the powerless victim atoms do not translate into effective resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: middle_income_universal_recipients (organized, mobile, national scope) derive directionality near the beneficiary pole — the flat checks subsidize them, and their exit options are excellent. Victims: deep_need_targeted_recipients, disabled_high_needs_recipients, and low_income_working_adults derive near the target pole; trapped exit options push the first two to the full-target end. Low-income working adults sit somewhat nearer the symmetric middle — they receive the grant and lose the work-linked credits — which is why their stakeholder entry carries a secondary beneficiary role alongside the payer role. No directionality_overrides are needed: the beneficiary/victim declarations plus the exit atoms reproduce the true relationships. One derivation nuance noted here: the administration seat derives low-to-moderate directionality through agenda-setting continuity interest rather than transfer receipts — its stake is institutional, not pecuniary — so the engine should not expect it to register as a transfer collector.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline cuts both ways here. Without the victim and receipt-surface declarations, the swap's genuine simplification achievements would let it masquerade as pure coordination — one payment, real overhead reduction, no obvious villain. Declaring the victims and naming the receipt seat forces the asymmetry into the open: someone is coordinated AND someone pays through the same structure, and the paying seat is the neediest. Conversely, the analysis prevents overgeneralization in the other direction: 'income support' as a topic contains multiple structurally distinct constraints, and this reading's favorable assessment of the targeted baseline belongs to the sibling stories, not to this one — condemning all income-support arrangements as one extractive mass would erase the family structure the kernel decomposition establishes. No sunset clause exists and none is claimed: the arrangement is designed as a permanent steady state, so scaffold framing is unavailable to it. The founding problem (fragmentation, take-up failure) is not yet obsolete — it is corroborated by administrative-burden research from outside the benefiting coalition — so piton framing is premature; the mandate is contested, not dead.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the targeting-efficiency reading of the income_support_commitment kernel; the freedom_floor_reading and dependency_trap_reading instantiate different constraints with different ε and different victim sets. Which reading governs a jurisdiction''s actual arrangement?',
    'Statutory design outcomes — whether replacement legislation preserves categorical supplements, whether a floor is layered beneath targeted programs — reveal which reading''s constraint is operative; cross-jurisdiction comparison of implemented designs.',
    'The classification here applies to the universal swap under this reading only. Under the freedom-floor reading the same legislative episode computes with different beneficiary/victim structure and a materially different ε; folding the readings together would violate ε-invariance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading among three of a contested kernel; sibling readings are separate constraint stories.').

omega_variable(
    cannibalization_completeness,
    'Does the modeled swap fully replace stacked targeted benefits with the flat grant (full cannibalization, −$19,100 for the exemplar household), or do residual categorical programs survive alongside it?',
    'Text of replacement legislation and benefit-stack simulation against current administrative caseload data.',
    'Full replacement sustains the high-extraction snare profile; partial stacking redistributes losses and moves the computed classification toward a tangled-rope shape in which genuine simplification benefit coexists with the remaining extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cannibalization_completeness, empirical, 'Scope of targeted-program elimination determines victim magnitude.').

omega_variable(
    political_ratchet_durability,
    'Once flat grants reach a majority-voter constituency, how reversible is the swap — does majority-beneficiary lock-in make retargeting politically impossible?',
    'Natural experiments in which universal programs were retargeted or clawed back retroactively (child-benefit clawbacks, energy-rebate phase-outs); longitudinal polling of beneficiary constituencies facing proposed retargeting.',
    'Durable lock-in raises effective suppression toward structural permanence and hardens the snare classification; demonstrated reversibility lowers it and opens a transitional, scaffold-like reading of the arrangement''s politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_ratchet_durability, empirical, 'Durability of the majority-beneficiary ratchet that defends uniformity.').

omega_variable(
    targeting_exclusion_error_mirror,
    'Does this reading''s endorsed targeted baseline itself systematically exclude eligible households (take-up failure, administrative burden, recertification churn), such that the reading''s favorable assessment of its own preferred arrangement is overstated?',
    'Take-up-rate administrative data, audit studies of eligibility screening, and churning/loss rates at recertification.',
    'Resolving this changes the reading''s comparative judgment and feeds the sibling stories'' ε — it does not change this story''s ε, whose referent remains the standing universal-swap arrangement. High exclusion-error findings strengthen the freedom-floor sibling reading''s case.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_exclusion_error_mirror, empirical, 'Mirror-image uncertainty about the endorsed alternative; kept out of this story''s ε by the referent rule.').

omega_variable(
    geographic_cost_flatness,
    'How much of the measured extraction is driven by flat-grant indifference to geographic cost variation (metro rents, childcare market prices) versus the universalist design per se?',
    'Regional price-parity adjustments to benefit-adequacy benchmarks; simulation of the exemplar household''s loss under a cost-indexed grant.',
    'If geography dominates the loss, a cost-indexed universal variant would shed much of the extraction and compute closer to rope/scaffold; the flat variant retains the snare profile regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geographic_cost_flatness, empirical, 'Separates design-driven extraction from geography-driven inadequacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t4, income_support_commitment__targeting_efficiency_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement_basis(inco_tr_t4, observed).
narrative_ontology:measurement(inco_tr_t8, income_support_commitment__targeting_efficiency_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(inco_tr_t8, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_commitment__targeting_efficiency_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t16, income_support_commitment__targeting_efficiency_reading, theater_ratio, 16, 0.41).
narrative_ontology:measurement_basis(inco_tr_t16, projected).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement_basis(inco_tr_t20, projected).
narrative_ontology:measurement(inco_tr_t24, income_support_commitment__targeting_efficiency_reading, theater_ratio, 24, 0.46).
narrative_ontology:measurement_basis(inco_tr_t24, projected).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t4, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 4, 0.55).
narrative_ontology:measurement_basis(inco_be_t4, observed).
narrative_ontology:measurement(inco_be_t8, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement_basis(inco_be_t8, observed).
narrative_ontology:measurement(inco_be_t12, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t16, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 16, 0.72).
narrative_ontology:measurement_basis(inco_be_t16, projected).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.74).
narrative_ontology:measurement_basis(inco_be_t20, projected).
narrative_ontology:measurement(inco_be_t24, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 24, 0.75).
narrative_ontology:measurement_basis(inco_be_t24, projected).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t4, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(inco_su_t4, observed).
narrative_ontology:measurement(inco_su_t8, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement_basis(inco_su_t8, observed).
narrative_ontology:measurement(inco_su_t12, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 12, 0.55).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t16, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(inco_su_t16, projected).
narrative_ontology:measurement(inco_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(inco_su_t20, projected).
narrative_ontology:measurement(inco_su_t24, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement_basis(inco_su_t24, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% 'How should income support be delivered?' is a colloquial label covering at least three structurally distinct constraints sharing the kernel income_support_commitment. This file authors the targeting_efficiency_reading, whose ε referent is the universal-distribution swap assessed by the need-tracking premise (snare; victims = deep-need and disabled recipients; receipt seat = the broad middle-income constituency). The sibling files author the freedom_floor_reading (referent: conditional/means-tested arrangements as barriers to autonomy) and the dependency_trap_reading (referent: unconditional support as skill-atrophying dependence), each with its own ε, beneficiary/victim structure, and classification. Edges run from this reading to both siblings because the dependency-trap and freedom-floor readings supply the empirical premises that the targeting reading's opponents and allies respectively deploy in legislative argument — mutual influence channels, not derivations. The ε values differ across the family because the readings locate the standing arrangement's extraction differently; that disagreement IS the kernel contest, and folding it into a single story would violate ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
