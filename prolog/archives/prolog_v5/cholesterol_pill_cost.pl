% ============================================================================
% CONSTRAINT STORY: cholesterol_pill_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cholesterol_pill_cost, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: cholesterol_pill_cost
 *   human_readable: Cost of Patented Cholesterol Medication
 *   domain: economic/healthcare
 *
 * SUMMARY:
 *   The patented cholesterol medication constraint exemplifies the structural
 *   tension between pharmaceutical innovation incentives and healthcare
 *   access. The drug demonstrates genuine clinical efficacy in reducing
 *   cardiovascular risk, but patent protection enables monopoly pricing that
 *   renders treatment unaffordable for uninsured and underinsured
 *   populations. The constraint operates across multiple institutional
 *   levels: individual patients face binary choice (pay or forgo), insurance
 *   companies face negotiating pressure from both manufacturers and members,
 *   healthcare systems face fiscal pressure, and governments face policy
 *   choices between innovation incentives and access mandates. The
 *   extractiveness has increased over the measurement interval (0.35 → 0.58)
 *   as the manufacturer consolidates market position and pricing power —
 *   initial post-launch pricing was more competitive due to alternative
 *   treatments, but as clinical superiority became established, pricing
 *   escalated. Theater ratio increased more modestly (0.32 → 0.48) as the
 *   justificatory discourse shifted from 'innovation funding' to 'market
 *   value of superior efficacy,' creating more performative economic framing.
 *
 * KEY AGENTS:
 *   - Uninsured Cardiac Patients: Primary victim (powerless/trapped) — bear full cost with zero negotiating power; face binary choice of payment or health risk
 *   - Insured Patients with High Copays: Secondary victim (moderate/constrained) — nominal coverage negated by manufacturer pricing; exit to alternatives is constrained
 *   - Insurance Companies and Healthcare Systems: Intermediate actor (organized/constrained) — benefit from preventive efficacy (coordination) but extracted from via pricing power; constrained by competitive coverage pressure
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — captures monopoly rent through patent protection; experiences constraint as innovation funding mechanism
 *   - Government Healthcare Authorities: Powerful actor (powerful/mobile) — have regulatory tools but constrained by concerns about innovation disincentives; must balance access and incentives
 *   - IP System: Institutional infrastructure (institutional/arbitrage) — maintains patent enforcement through legal architecture; benefits from licensing and litigation activity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cholesterol_pill_cost, 0.58).
domain_priors:suppression_score(cholesterol_pill_cost, 0.68).
domain_priors:theater_ratio(cholesterol_pill_cost, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cholesterol_pill_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(cholesterol_pill_cost, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cholesterol_pill_cost, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cholesterol_pill_cost, snare).
narrative_ontology:human_readable(cholesterol_pill_cost, "Cost of Patented Cholesterol Medication").
narrative_ontology:topic_domain(cholesterol_pill_cost, "economic/healthcare").

domain_priors:requires_active_enforcement(cholesterol_pill_cost).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, pharmaceutical_manufacturer).
narrative_ontology:constraint_victim(cholesterol_pill_cost, uninsured_patients).
narrative_ontology:constraint_victim(cholesterol_pill_cost, insurance_copay_burden).
narrative_ontology:constraint_victim(cholesterol_pill_cost, healthcare_system_cost_pressure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINSURED CARDIAC PATIENT (SNARE) — Cannot exit the constraint. Faces binary choice: pay monopoly price for life-saving medication or forgo treatment. No substitute exists due to patent protection. The patient bears full extraction with zero negotiating power. Maximum suppression through absence of alternatives.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSURED PATIENT WITH HIGH COPAY (SNARE) — Nominally has insurance but manufacturer pricing forces prohibitive copay tiers. Exit options are severely constrained: switch to inferior drug (generic competitor), risk health deterioration, or exhaust financial resources. Extraction persists despite ostensible coverage.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSURANCE COMPANIES / HEALTHCARE SYSTEMS (TANGLED ROPE) — Experience mixed dynamics. Coordination function: the medication prevents more costly downstream interventions (cardiac events, hospitalizations). Extraction function: manufacturer uses patent leverage and prior exclusivity to extract monopoly rent through pricing power. Insurers can negotiate but face collective action problem — refusing coverage loses market share to competitors who do cover. Constrained exit (must offer competitive coverage) but real organizational power.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Experiences the constraint as pure coordination: patent protection enables recouping R&D investment and funding future drug discovery. From the manufacturer's perspective, the pricing reflects the value of the innovation and the coordination function (funding a pipeline of cardiac medications). Exit option is arbitrage: can reallocate capital to other therapeutic areas if pricing becomes untenable. Primary beneficiary with full organizational power.
constraint_indexing:constraint_classification(cholesterol_pill_cost, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOVERNMENT HEALTHCARE AUTHORITY (TANGLED ROPE) — Faces genuine hybrid constraint. Coordination function: patent system incentivizes drug discovery. Extraction function: pricing power creates fiscal pressure on public healthcare budgets, forcing rationing decisions or cost-shifting to patients. Government has some exit options (price regulation, compulsory licensing, international sourcing) but deploying them risks future innovation disincentives. Mobile but strategically constrained by longer-term system effects.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: INTELLECTUAL PROPERTY SYSTEM (PITON) — The patent system persists as an enforcement mechanism despite evidence that fixed-term monopoly pricing creates severe access barriers. Originally designed to coordinate innovation incentives, the system now operates with high theater: patent extension negotiations, pay-for-delay agreements, and regulatory capture substitute for genuine R&D differentiation. The system maintains itself through institutional inertia and legal complexity rather than functional performance. Theater ratio reflects the performative legal architecture surrounding drug pricing.
constraint_indexing:constraint_classification(cholesterol_pill_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ECONOMIC EFFICIENCY VIEW (FALSE SUMMIT) — Risks naturalizing the constraint as an inevitable law of pharmaceutical economics: innovation requires incentives, incentives require pricing power, pricing power necessarily creates access barriers. However, this perspective fails the natural law gate — the structural data shows the constraint is contingent on policy choices (patent term, licensing rules, regulatory discretion), not immutable economic law. Alternative systems (prize funds, public manufacturing, tiered pricing) demonstrate the arrangement is malleable. False summit classification reveals naturalization.
constraint_indexing:constraint_classification(cholesterol_pill_cost, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cholesterol_pill_cost_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cholesterol_pill_cost, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cholesterol_pill_cost, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cholesterol_pill_cost, TR),
    TR >= 0.70.

:- end_tests(cholesterol_pill_cost_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The manufacturer captures monopoly pricing during the patent period, extracting consumer surplus from patients and healthcare systems. This is not total extraction (0.70+) because the medication provides genuine health value and the patent is time-limited — the constraint is designed to eventually terminate. The 0.58 value reflects that this is asymmetric capture of legitimate innovation returns, not predatory extraction with zero coordination function. Suppression (0.68): Moderate-high. Patients face severe barriers to alternatives — the drug is clinically superior, alternatives either inferior or expensive, and switching requires physician authorization. Patent enforcement prevents generic competition during the monopoly period. However, suppression is not total (>0.80) because international price variation and eventual patent expiration create off-ramps. Theater ratio (0.48): Moderate. The economic justification for pricing contains real elements (R&D cost recovery, innovation incentive) but also performative elements (regulatory capture, extension strategies, pay-for-delay agreements). The 0.48 reflects that the coordination narrative is substantive but increasingly mixed with rent-seeking theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Uninsured patients see pure extraction with no coordination benefit (Snare). Manufacturers see innovation funding mechanism with zero extraction (Rope). Insurance companies see genuine mix: the medication prevents downstream costs (coordination benefit) but arrives at prohibitive price (extraction force) — hence Tangled Rope. Governments see longer-term structural constraint: patent incentivizes innovation today but creates access barriers that may harm tomorrow's health system efficiency — hence also Tangled Rope but with different causal emphasis. The IP system sees its own degraded function (Piton): patent enforcement persists but increasingly via pay-for-delay and regulatory extension rather than genuine R&D competition. The analytical observer risks seeing this as an inevitable trade-off (Mountain: access always trades against innovation) but the structural data reveals this as false summit — policy choices (patent term, compulsory licensing authority, prize fund alternatives) are contingent, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality varies sharply by agent due to their different structural positions. Uninsured patients experience d ≈ 0.95 (full target, trapped exit) → maximum f(d) ≈ 1.42 → experience high χ despite moderate base extractiveness. Insured patients experience d ≈ 0.75 (primarily target, constrained exit) → f(d) ≈ 1.15 → moderate-high χ. Insurance companies experience d ≈ 0.45 (mixed beneficiary/target, organized exit) → f(d) ≈ 0.50 → χ scales down significantly. Manufacturer experiences d ≈ 0.05 (beneficiary, arbitrage exit) → f(d) ≈ -0.12 → negative χ (they perceive subsidy, not extraction). Government authorities experience d ≈ 0.50 (balanced, mobile exit) → f(d) ≈ 0.65 → moderate χ. The scope modifier σ(S)=1.0 (national scope) applies uniformly. This derivation chain explains why the same constraint classifies as snare (trappped victims), tangled_rope (organized intermediate actors), and rope (beneficiary manufacturer).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT TYPE CLASSIFICATION INSTABILITY: The pharmaceutical constraint demonstrates why extractiveness thresholds matter. At ε=0.58, this qualifies as a Snare (ε ≥ 0.46, suppression ≥ 0.60). But the classification is structurally unstable if any of three factors change: (1) If generic competition timeline moves forward (patent expiration accelerates), extractiveness declines below 0.46 → reclassifies as Tangled Rope. (2) If manufacturer price concessions or tiered pricing expands (suppression drops below 0.60), classification shifts to Rope despite high ε. (3) If innovation rate slows without pricing increase (theater rises further as justification decays), classification might shift toward Piton despite nominally high extraction. The Snare classification is accurate at the current measurement point, but the constraint's identity is not stable across time or counterfactual scenarios. This instability is informative: it suggests the underlying structural relationship is genuinely contested — whether this 'should' be pure innovation incentive (Rope) or pure monopoly exploitation (Snare) depends on empirical claims about alternatives and necessity that omega variables make explicit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_rent_insufficiency,
    'What share of pharmaceutical profits derives from genuine innovation vs. regulatory capture (patent extension, pay-for-delay, FDA exclusivity)?',
    'Comparative analysis of drug efficacy improvements vs. pricing increases over 10-year periods; patent litigation and regulatory outcome data; international pricing variation for identical molecules',
    'If genuine innovation dominates: patent system functions as coordination mechanism (Rope). If capture dominates: system functions as extraction (Snare/Piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_rent_insufficiency, empirical, 'Share of profit from innovation vs. regulatory capture').

omega_variable(
    alternative_funding_viability,
    'Could public funding, prize systems, or tiered pricing models sustain equivalent innovation rates in cholesterol therapeutics?',
    'Comparative analysis of government-funded drug development timelines and outcomes; international case studies (India, Australia, Canada public manufacturing programs); economic modeling of alternative incentive structures',
    'If viable: patent monopoly is revealed as contingent policy choice (constraint reclassifies as Tangled Rope/Scaffold with sunset). If non-viable: innovation argument strengthens (Rope classification holds).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_funding_viability, empirical, 'Viability of non-patent funding for drug development').

omega_variable(
    access_barrier_necessity,
    'Is high domestic pricing in wealthy markets necessary to fund development, or does global patent enforcement and international price differentiation already enable lower domestic pricing without loss of incentive?',
    'Economic analysis of R&D cost models; comparison with tiered international pricing strategies already in use (European reference pricing, Indian generic markets); pharmaceutical company financial disclosures on cost allocation',
    'If differentiation is viable: domestic monopoly pricing is unjustified (constraint reclassifies as pure Snare for wealthy markets). If global pricing must be uniform: extraction is inherent to the coordination model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(access_barrier_necessity, empirical, 'Necessity of high domestic pricing for innovation funding').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cholesterol_pill_cost, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chol_tr_t0, cholesterol_pill_cost, theater_ratio, 0, 0.32).
narrative_ontology:measurement(chol_tr_t5, cholesterol_pill_cost, theater_ratio, 5, 0.4).
narrative_ontology:measurement(chol_tr_t10, cholesterol_pill_cost, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(chol_be_t0, cholesterol_pill_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chol_be_t5, cholesterol_pill_cost, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(chol_be_t10, cholesterol_pill_cost, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cholesterol_pill_cost, resource_allocation).
narrative_ontology:affects_constraint(cholesterol_pill_cost, insulin_pricing_cliff).
narrative_ontology:affects_constraint(cholesterol_pill_cost, generic_drug_market_entry).
narrative_ontology:affects_constraint(cholesterol_pill_cost, healthcare_financing_rationing).

% DUAL FORMULATION NOTE:
% The cholesterol medication constraint is downstream of broader pharmaceutical patent policy but upstream of specific healthcare access outcomes. The network reflects that pricing power in one drug class creates systemic pressure on insurance pools, which affect subsequent rationing decisions in other therapeutic areas.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cholesterol_pill_cost, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
