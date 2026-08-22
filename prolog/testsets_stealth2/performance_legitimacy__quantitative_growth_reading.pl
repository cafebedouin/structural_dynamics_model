% ============================================================================
% CONSTRAINT STORY: performance_legitimacy__quantitative_growth_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_performance_legitimacy__quantitative_growth_reading, []).

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
 *   constraint_id: performance_legitimacy__quantitative_growth_reading
 *   human_readable: GDP Growth-Rate Performance Legitimacy (Quantitative Growth Reading)
 *   domain: political economy/development planning/state capitalism
 *
 * SUMMARY:
 *   A state-capitalist developmental regime grounds its political legitimacy
 *   in the GDP growth rate: the number is the standing public promise that
 *   expansion continues and jobs follow, and the binding standard by which
 *   the governing apparatus judges itself. This file instantiates the
 *   quantitative_growth_reading of the performance_legitimacy kernel; the
 *   claim and metrics below are authored for this reading only, and the
 *   sibling readings (qualitative development, techno-nationalist, livelihood
 *   security) are separate constraints with their own beneficiary structures
 *   and are not averaged here. Structurally the arrangement is a genuine
 *   coordination device wrapped around an asymmetric transfer: one legible
 *   number aligns credit allocation, land conversion, cadre promotion, and
 *   industrial policy across thousands of jurisdictions, while administered
 *   deposit rates, capital controls, and thin social spending move household
 *   savings and consumption share into an investment channel whose output
 *   flows disproportionately to the industrial-export complex and to the
 *   officials promoted on the number. The epsilon referent is the standing
 *   growth-target arrangement as this reading assesses it, never any
 *   successor formula. Claim and metrics are authored independently: the
 *   claimed type records the structure believed true (tangled_rope); the
 *   metrics record the operation believed descriptively accurate; the engine
 *   computes per-seat classifications from the structural data and any
 *   divergence from the claim is the measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - national_leadership: agenda-setter (institutional / identity_locked) — sets the targets, controls the credit and fiscal levers, and is itself bound by the formula its authority runs on
 *   - industrial_export_complex: primary beneficiary (institutional / constrained) — receives administered credit, cheap land, and overcapacity tolerance; its expansion is the raw material of the number
 *   - local_government_officials: dual beneficiary/payer (organized / identity_locked) — promoted on the growth number; inherit its debts and cleanup burdens in the late interval
 *   - state_banking_sector: intermediary beneficiary (institutional / constrained) — earns administered spread income intermediating the household-to-borrower transfer
 *   - household_savers: primary payer (powerless / constrained) — below-market deposit returns, suppressed consumption share, capital controls closing the exit
 *   - future_generations: deferred payer (powerless / trapped) — bear the debt, environmental, and demographic costs booked forward to hold the number
 *   - overseas_export_consumers: external beneficiary (moderate / mobile) — capture the consumer surplus of suppressed factor costs; their mobility disciplines the export complex
 *   - alternative_framing_advocates: excluded (moderate / constrained) — livelihood- and quality-first voices outside the target-setting conversation
 *   - comparative_political_economists: analytical observer (analytical / analytical) — see the full structure without holding a seat in it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, 0.68).
domain_priors:suppression_score(performance_legitimacy__quantitative_growth_reading, 0.62).
domain_priors:theater_ratio(performance_legitimacy__quantitative_growth_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(performance_legitimacy__quantitative_growth_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(performance_legitimacy__quantitative_growth_reading, tangled_rope).
narrative_ontology:human_readable(performance_legitimacy__quantitative_growth_reading, "GDP Growth-Rate Performance Legitimacy (Quantitative Growth Reading)").
narrative_ontology:topic_domain(performance_legitimacy__quantitative_growth_reading, "political economy/development planning/state capitalism").

domain_priors:requires_active_enforcement(performance_legitimacy__quantitative_growth_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(performance_legitimacy__quantitative_growth_reading, 'bb990f89-4f06-411a-9efa-448c53929361').
narrative_ontology:cs_kernel_codification('bb990f89-4f06-411a-9efa-448c53929361', formalized).
narrative_ontology:cs_authority_grounding('bb990f89-4f06-411a-9efa-448c53929361', practice).
narrative_ontology:cs_interpretation_layer_present('bb990f89-4f06-411a-9efa-448c53929361').
narrative_ontology:cs_reading_relation('bb990f89-4f06-411a-9efa-448c53929361', performance_legitimacy__qualitative_development_reading, influences).
narrative_ontology:cs_reading_relation('bb990f89-4f06-411a-9efa-448c53929361', performance_legitimacy__livelihood_security_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb990f89-4f06-411a-9efa-448c53929361', performance_legitimacy__techno_nationalist_reading, coexists_with).
narrative_ontology:cs_axiom('bb990f89-4f06-411a-9efa-448c53929361', foundational, quantitative_growth_demonstrates_competence).
narrative_ontology:cs_axiom_status(quantitative_growth_demonstrates_competence, holdable).
narrative_ontology:cs_axiom_grounding('bb990f89-4f06-411a-9efa-448c53929361', quantitative_growth_demonstrates_competence, empirically_contingent).
narrative_ontology:cs_axiom('bb990f89-4f06-411a-9efa-448c53929361', foundational, growth_rate_precedence_over_composition).
narrative_ontology:cs_axiom_status(growth_rate_precedence_over_composition, holdable).
narrative_ontology:cs_axiom_grounding('bb990f89-4f06-411a-9efa-448c53929361', growth_rate_precedence_over_composition, empirically_contingent).
narrative_ontology:cs_axiom('bb990f89-4f06-411a-9efa-448c53929361', secondary, employment_follows_aggregate_rate).
narrative_ontology:cs_axiom_status(employment_follows_aggregate_rate, holdable).
narrative_ontology:cs_axiom_grounding('bb990f89-4f06-411a-9efa-448c53929361', employment_follows_aggregate_rate, empirically_contingent).
narrative_ontology:cs_reference_frame('bb990f89-4f06-411a-9efa-448c53929361', sustained_high_speed_expansion_norm).
narrative_ontology:cs_drift_state('bb990f89-4f06-411a-9efa-448c53929361', structural_slowdown_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('bb990f89-4f06-411a-9efa-448c53929361', '').
narrative_ontology:cs_kernel_id(performance_legitimacy__quantitative_growth_reading, performance_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, state_banking_sector).
narrative_ontology:constraint_beneficiary(performance_legitimacy__quantitative_growth_reading, overseas_export_consumers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, household_savers).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(performance_legitimacy__quantitative_growth_reading, local_government_officials).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, growth_first_development_doctrine).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, investment_led_expansion_model).
narrative_ontology:constraint_vindicates(performance_legitimacy__quantitative_growth_reading, aggregate_rate_employment_linkage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the growth targets in five-year plans and annual work conferences, evaluates the provincial apparatus against them, and controls the credit and fiscal levers used to hit them. The growth number is simultaneously the regime's public promise and the standard by which its own competence is judged; pivoting to a different legitimacy formula would require dismantling the evaluation system its own authority runs on. Collects legitimacy and political stability when targets are met; absorbs crisis pressure when they are not.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, national_leadership, agenda_setter,
    institutional, generational, identity_locked, national).

% State-owned and private export manufacturers, heavy industry, developers, and their supply chains receive administered-rate credit, land at below-market prices, export infrastructure, and tolerance for overcapacity. Their expansion is the raw material the growth number is made of; without the investment channel a large share would face insolvency at market rates. Exit would mean competing for capital at market cost against the subsidy stream that currently sustains them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, industrial_export_complex, beneficiary,
    institutional, biographical, constrained, global).

% Provincial and municipal cadres are promoted, rotated, or sidelined on growth performance. Hitting targets brings promotion and access to land-finance revenue; missing them ends careers. In the later interval they also inherit the cleanup: financing-vehicle debt service, ghost-project maintenance, and environmental remediation fall on the same offices that booked the growth. Exit is not available; the cadre system is their entire career structure.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, local_government_officials, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(performance_legitimacy__quantitative_growth_reading, local_government_officials, payer).

% State commercial banks gather household deposits at administered rates and lend at policy-directed margins, earning guaranteed spread income while balance sheets expand with the investment drive. They intermediate the transfer but do not set it; their solvency depends on the borrower base the growth machine sustains.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, state_banking_sector, beneficiary,
    institutional, biographical, constrained, national).

% Households hold the savings that fund the investment drive and receive below-market returns on deposits, an implicit annual transfer to borrowers. They also carry the consumption-share suppression: social spending stayed thin relative to growth, so precautionary saving stayed high. Moving savings abroad is restricted by capital controls; the main domestic alternative, property, routes back through the same banks and the same arrangement.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, household_savers, payer,
    powerless, biographical, constrained, national).

% Bear the deferred costs booked to sustain the growth number: local-government and state-enterprise debt service, degraded land, air, and water, the demographic costs of growth-first policy choices, and the overcapacity write-downs. They are present in the arrangement only as its residual claimants; no seat in the current negotiation represents them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, future_generations, payer,
    powerless, generational, trapped, national).

% Import the output of the export machine at prices that embed suppressed factor costs: administered credit, unpriced environmental externalities, and labor without independent bargaining. They capture the consumer surplus of the arrangement and can shift sourcing when relative prices move, which is precisely the discipline that keeps the export complex tolerating thin margins.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, overseas_export_consumers, beneficiary,
    moderate, immediate, mobile, global).

% Welfare economists, demographers, environmental agencies, and livelihood-first policy advocates argue for metrics of directly experienced welfare over the aggregate rate. They publish inside the system but sit outside target-setting: the plan and cadre-evaluation machinery does not include their metric, and advocacy that reads as delegitimizing the growth number carries career and discourse risk. Their exit is marginalization or emigration.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, alternative_framing_advocates, excluded,
    moderate, generational, constrained, national).

% Study performance-legitimacy regimes comparatively: how growth-based mandates are established, how they behave under slowdown, and what replaces them. They take the full structure in view (the coordination achievement, the transfer mechanics, the deferred costs) without holding a seat in any of them.
narrative_ontology:constraint_stakeholder(performance_legitimacy__quantitative_growth_reading, comparative_political_economists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(performance_legitimacy__quantitative_growth_reading, industrial_export_complex).
narrative_ontology:fixing_cost_class(performance_legitimacy__quantitative_growth_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the alignment problem of a continental developmental state: a single legible number coordinates credit allocation, land conversion, cadre promotion, and industrial policy across thousands of jurisdictions whose effort is not directly observable, and gives household savers a state-guaranteed destination for capital formation.
% TRANSFER_FUNCTION: Moves household savings (via administered deposit rates and capital controls) and land and fiscal revenues into investment in industry, infrastructure, and property; moves promotion, rents, and market share toward officials and enterprises that hit the number; moves consumption share and environmental capacity away from households and the commons.
% ABSENT_VOICES: Livelihood-first and quality-first framers sit outside target-setting; household savers have no seat in credit-allocation decisions; future generations hold no seat anywhere in the present negotiation; the unemployed in lagging regions, for whom the aggregate number does not convert into work, are counted in the denominator of the promise but not in its making.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand the evaluation system that orders the entire official career structure, leave administered credit without its allocation criterion, force immediate repricing of deposits and loans, and confront the regime with the question its growth number has been answering — what demonstrates competence? — with no successor formula in place. The developmental apparatus would have to renegotiate its own incentives in real time.
% FOUNDING_PROBLEM: Post-crisis mass poverty and an enormous, poorly-instrumented bureaucracy: the regime needed a legible, hard-to-fake-at-the-center metric that could mobilize household savings into capital formation, align millions of officials, and demonstrate governing competence to a population with living memory of economic failure.
% FOUNDING_PROBLEM_CORROBORATION: International development institutions and independent economists outside the beneficiary set corroborate that the founding problem was real and substantially addressed; the poverty record is externally audited. Demographers, environmental researchers, and household-income analysts, also outside the beneficiary set, corroborate that the metric now misallocates relative to its original problem, supporting the shifted-function reading. No corroborating source outside the benefiting parties attests that the raw growth rate remains the necessary legitimacy metric.
narrative_ontology:disappearance_verdict(performance_legitimacy__quantitative_growth_reading, world_rearranges).
narrative_ontology:founding_problem_status(performance_legitimacy__quantitative_growth_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(performance_legitimacy__quantitative_growth_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(performance_legitimacy__quantitative_growth_reading, 'none', 1).
narrative_ontology:epsilon_provenance(performance_legitimacy__quantitative_growth_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(performance_legitimacy__quantitative_growth_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(performance_legitimacy__quantitative_growth_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(performance_legitimacy__quantitative_growth_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.68: the transfer is large and persistent — administered deposit rates below market clear a multi-point-of-GDP annual transfer from households to borrowers, consumption share is structurally suppressed by thin social insurance, and the debt and environmental bills are booked forward. Suppression 0.62 is authored as a raw structural property and is not scaled by power or scope: the constraint persists by cadre discipline, credit-allocation control, and discourse management of framings that would delegitimize the growth number, not by participant preference. Theater 0.42: a real and rising share of activity is performative — local statistical inflation, vanity projects booked as output, deflator management — but much of the investment is physically real even when misallocated, so the ratio stays well below piton territory. Accessibility_collapse 0.45: alternatives have not collapsed — livelihood and quality framings exist, are gaining official rhetorical space, and officials can game or reinterpret the metric; the constraint forecloses less than a snare, more than a clean rope. Resistance 0.5: demographers, environmental agencies, household-income researchers, and slowdown-pessimist officials push back continuously without breaking the arrangement. All three tracked series run on ONE shared grid (T=0,8,16,24,32,40) with a value authored at every point for every metric. suppression_requirement is authored deliberately: the story's traced dynamic is enforcement-capacity change — as structural growth decelerates, maintaining the growth-legitimacy narrative requires hardening machinery (statistical verification campaigns, tightened evaluation, discourse control of slowdown talk), a rising enforcement trajectory rather than a static picture.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership seat the constraint is a self-imposed discipline that works: it aligned the apparatus and delivered the record poverty reduction, and its computed type should come out coordination-flavored. From the household-saver seat the same structure is a transfer machine with no exit, and should compute extraction-flavored at high chi. From the local-official seat it is both the career ladder and the debt trap — the same metric that promotes at mid-interval buries at late interval. Overseas consumers experience only the cheap-output side. The engine computes these per-seat classifications from the structural data; the divergence between seats is the measurement, not a defect to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations put the export complex, the banks, and overseas consumers near the beneficiary end (low d); the export complex's constrained exit keeps it from the arbitrage end despite its benefit position, while overseas consumers' mobility sits them nearest it. Victim declarations put household_savers at high d — their constrained exit (capital controls, a property channel that loops back into the arrangement) pushes them toward the full-target end — and future_generations at the structural maximum (trapped, no seat). The leadership appears in neither list: it both collects legitimacy from the formula and bears the delivery obligation, sitting near symmetric (d near 0.5); the canonical fallback for its power atom is roughly right, so no directionality override is authored. Local officials carry a dual declaration (beneficiary with secondary payer): promotion gains push d down, cleanup burdens push it up, landing them mid-low. Suppression is a raw structural property, unscaled; only extractiveness is scaled by directionality and scope in the engine's computation, and the export complex's global scope slightly amplifies its effective extraction relative to a domestic-only beneficiary.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. Reading the growth formula as pure rope ignores the financial-repression transfer and the deferred debt and environmental bill — the coordination is real but asymmetric, and it takes active enforcement (cadre evaluation, credit control, discourse management) to hold. Reading it as pure snare erases the genuine coordination achievement: the largest poverty-reduction mobilization on record ran through this structure, and the apparatus-alignment problem it solved was real. Tangled rope holds both. On mandatrophy: the founding problem (post-crisis poverty, bureaucratic legibility) is partly solved but not dead — the apparatus-alignment function persists even as the poverty function fades, so the mandate has not simply outlived its function; it has partially shifted function. The contested founding_problem_status paired with a world_rearranges disappearance verdict records exactly that partial shift: the world still rearranges around the growth number even though the problem it was built to solve has changed shape, which is why mandatrophy is not declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This story instantiates one reading (quantitative_growth) of the performance_legitimacy kernel. Would a sibling reading applied to the same standing arrangement produce a structurally different constraint — different beneficiary and victim sets and a different epsilon?',
    'Author the sibling readings as separate stories over the same referent and compare their structural derivations and epsilon values. The disagreement is located in which observable counts as the demonstration of legitimacy: the aggregate rate (this reading), directly experienced welfare (livelihood), transformation quality (qualitative), or strategic-industry capability (techno-nationalist).',
    'Under the livelihood reading household_savers move toward partial beneficiary (welfare delivery is the metric they are owed) and epsilon falls; under this reading they are payers and epsilon is high. Classification of the same arrangement is reading-relative by design; the corpus needs all four files to measure the contest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Kernel-reading underdetermination: which observable demonstrates legitimacy.').

omega_variable(
    financial_repression_transfer_magnitude,
    'How large is the annual household-to-borrower transfer implied by administered deposit rates below market-clearing levels, relative to household income?',
    'Interest-rate counterfactual analysis: reprice household deposits at a market benchmark and measure the implied annual transfer; cross-check against household income share of GDP trends over the interval.',
    'A transfer of several GDP points annually sustains high epsilon and the payer status of household_savers; a near-zero transfer would move the arrangement toward rope and soften the victim declaration.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(financial_repression_transfer_magnitude, empirical, 'Size of the financial-repression transfer at the core of the extraction claim.').

omega_variable(
    growth_statistics_authenticity,
    'What share of measured growth is real activity versus statistical manufacture — local number inflation, deflator management, vanity projects booked as output?',
    'Independent physical cross-checks of official GDP: satellite night-luminance, electricity consumption, rail freight, and tax-receipt growth against reported growth, by province and period.',
    'Higher manufacture raises theater_ratio above the authored 0.42 and pushes the late-interval type toward piton or snare flavor; authentic growth keeps the tangled_rope reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(growth_statistics_authenticity, empirical, 'Real-versus-performative composition of the measured growth number.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative legitimacy framings structural (career and discourse machinery) or internalized (officials and citizens genuinely hold growth-equals-legitimacy as belief)?',
    'Post-pivot trajectory analysis: where enforcement relaxes (retired officials, regions reweighted away from GDP, open survey data), does growth-first belief persist? Persistent belief after barrier removal indicates partial internalization.',
    'An internalized component means suppression outlives the enforcement machinery and the pivot to sibling framings is slower than structural change implies; a purely structural mechanism means the arrangement could reframe quickly if the center reweighted the evaluation criteria.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression of alternative framings.').

omega_variable(
    legitimacy_substitution_under_slowdown,
    'As structural growth decelerates, does political legitimacy actually transfer to sibling framings (livelihood delivery, nationalist achievement), or does the growth formula retain binding force over the apparatus?',
    'Longitudinal legitimacy surveys crossed with observed cadre-evaluation criteria: if evaluation weights shift in practice, not merely rhetoric, toward welfare and quality metrics, substitution is real; if the rate remains the operative target, the formula retains binding force.',
    'If binding force persists under slowdown, suppression_requirement and theater_ratio keep rising on the measured trajectory and the constraint hardens; if substitution is real, the constraint decays toward piton — maintained performatively while a successor formula takes over.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_substitution_under_slowdown, empirical, 'Whether the growth formula survives its own slowdown or yields to sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(performance_legitimacy__quantitative_growth_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perf_tr_t0, performance_legitimacy__quantitative_growth_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(perf_tr_t8, performance_legitimacy__quantitative_growth_reading, theater_ratio, 8, 0.21).
narrative_ontology:measurement(perf_tr_t16, performance_legitimacy__quantitative_growth_reading, theater_ratio, 16, 0.27).
narrative_ontology:measurement(perf_tr_t24, performance_legitimacy__quantitative_growth_reading, theater_ratio, 24, 0.33).
narrative_ontology:measurement(perf_tr_t32, performance_legitimacy__quantitative_growth_reading, theater_ratio, 32, 0.38).
narrative_ontology:measurement(perf_tr_t40, performance_legitimacy__quantitative_growth_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(perf_be_t0, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(perf_be_t8, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(perf_be_t16, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(perf_be_t24, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(perf_be_t32, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(perf_be_t40, performance_legitimacy__quantitative_growth_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perf_su_t0, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(perf_su_t8, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(perf_su_t16, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 16, 0.51).
narrative_ontology:measurement(perf_su_t24, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 24, 0.56).
narrative_ontology:measurement(perf_su_t32, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 32, 0.6).
narrative_ontology:measurement(perf_su_t40, performance_legitimacy__quantitative_growth_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(performance_legitimacy__quantitative_growth_reading, resource_allocation).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__qualitative_development_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__techno_nationalist_reading).
narrative_ontology:affects_constraint(performance_legitimacy__quantitative_growth_reading, performance_legitimacy__livelihood_security_reading).

% DUAL FORMULATION NOTE:
% 'Performance legitimacy' as a colloquial label decomposes into four structurally distinct constraints — the four readings of the kernel — each with its own epsilon, beneficiary structure, and classification; this file is the quantitative_growth_reading. It sits upstream of the siblings in resource terms: the growth machine this reading mandates built the fiscal and industrial base the qualitative and livelihood agendas draw on, and its cadre-evaluation machinery still sets the operating environment in which the sibling readings compete. The sibling files link back to this one; the family is linked through affects_constraints in every member.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
