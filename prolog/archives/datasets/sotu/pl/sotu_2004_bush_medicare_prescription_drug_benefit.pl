% ============================================================================
% CONSTRAINT STORY: sotu_2004_bush_medicare_prescription_drug_benefit
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2004_bush_medicare_prescription_drug_benefit, []).

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
 *   constraint_id: sotu_2004_bush_medicare_prescription_drug_benefit
 *   human_readable: Medicare Prescription Drug Benefit Expansion (2003-2006)
 *   domain: healthcare/entitlement_policy
 *
 * SUMMARY:
 *   The Medicare Prescription Drug, Improvement, and Modernization Act (2003)
 *   expanded Medicare to cover outpatient prescription drugs beginning in
 *   2006, creating a new entitlement worth approximately $400 billion over
 *   the first decade. The constraint exhibits genuine tangled-rope structure:
 *   it solves a real coordination problem (seniors' inability to afford
 *   essential medications) while simultaneously creating extractive
 *   mechanisms (manufacturer pricing power, intermediary rents, trust fund
 *   burden). The expansion directed federal resources to primary
 *   beneficiaries (seniors with medication needs) and indirect beneficiaries
 *   (pharmaceutical manufacturers gaining increased demand and reduced price
 *   negotiation). Costs were borne by general federal taxpayers (through tax
 *   increases and general revenue), Medicare beneficiaries (through higher
 *   premiums), and Medicare Trust Fund solvency (through increased outlays
 *   and insolvency risk). The constraint's evolution reveals rising
 *   extractiveness and theater as prescription drug costs outpaced inflation
 *   and as regulatory capture mechanisms (prohibition on price negotiation,
 *   intermediary profit extraction) deepened over time. The policy
 *   simultaneously embodies genuine coordination (solving medication access
 *   barriers) and significant extraction (absorbing costs that might have
 *   been addressed through price negotiation or international reference
 *   pricing).
 *
 * KEY AGENTS:
 *   - Senior Citizens with High Medication Needs: Primary beneficiary (moderate/constrained) — gain access to affordable medications; net benefit despite constrained alternatives
 *   - Pharmaceutical Manufacturers: Indirect beneficiary (institutional/arbitrage) — expand market demand and reduce price sensitivity; bear moderate extraction through reimbursement regulation
 *   - Federal Taxpayers (Working Age): Cost bearer (moderate/constrained) — fund expansion through higher taxes and general revenue; experience snare-like asymmetric cost allocation
 *   - Medicare Trust Fund: Fiscal victim (powerful/constrained) — constrained by political inability to control costs; experiences generational extraction risk
 *   - Public Health Advocates: Organized critics (organized/constrained) — see temporary scaffold with sunset logic (eventual price negotiation reform); constrained by political realities
 *   - Health Insurance Intermediaries: Institutional actors (institutional/arbitrage) — create administrative overhead with modest coordination function; persist through institutional inertia (piton)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choice as immutable healthcare necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2004_bush_medicare_prescription_drug_benefit, 0.52).
domain_priors:suppression_score(sotu_2004_bush_medicare_prescription_drug_benefit, 0.35).
domain_priors:theater_ratio(sotu_2004_bush_medicare_prescription_drug_benefit, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2004_bush_medicare_prescription_drug_benefit, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_2004_bush_medicare_prescription_drug_benefit, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_2004_bush_medicare_prescription_drug_benefit, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2004_bush_medicare_prescription_drug_benefit, tangled_rope).
narrative_ontology:human_readable(sotu_2004_bush_medicare_prescription_drug_benefit, "Medicare Prescription Drug Benefit Expansion (2003-2006)").
narrative_ontology:topic_domain(sotu_2004_bush_medicare_prescription_drug_benefit, "healthcare/entitlement_policy").

domain_priors:requires_active_enforcement(sotu_2004_bush_medicare_prescription_drug_benefit).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2004_bush_medicare_prescription_drug_benefit, senior_citizens_with_medication_needs).
narrative_ontology:constraint_beneficiary(sotu_2004_bush_medicare_prescription_drug_benefit, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(sotu_2004_bush_medicare_prescription_drug_benefit, health_insurance_intermediaries).
narrative_ontology:constraint_victim(sotu_2004_bush_medicare_prescription_drug_benefit, general_federal_taxpayers).
narrative_ontology:constraint_victim(sotu_2004_bush_medicare_prescription_drug_benefit, medicare_trust_fund_solvency).
narrative_ontology:constraint_victim(sotu_2004_bush_medicare_prescription_drug_benefit, future_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENIOR CITIZEN WITH HIGH MEDICATION NEEDS (ROPE) — Experiences the constraint as genuine coordination solving a real collective action problem: prescription drug costs create a coordination failure for elderly agents with limited income and no individual bargaining power against pharmaceutical manufacturers. The benefit reduces this friction. Exit is constrained (could migrate to countries with price controls, or reduce medication compliance) but expensive. The senior perceives genuine net benefit — the constraint solves the coordination problem of accessing essential medications. The theater ratio is low from this perspective because the benefit delivers tangible outcomes.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: WORKING-AGE FEDERAL TAXPAYER (SNARE) — Bears the extraction cost (higher payroll taxes, general revenue increases) with minimal coordination benefit. Constrained by citizenship and tax obligations. The extraction is moderate but durable — payroll tax increases to support Medicare Part D are structural and difficult to reverse. Exit requires emigration or tax evasion. The working-age taxpayer experiences asymmetric cost allocation without corresponding benefit — genuine snare structure at biographical timescale.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL MANUFACTURER (TANGLED ROPE) — Experiences the constraint as coordination (expanded market demand, reduced consumer price sensitivity, increased negotiation complexity) combined with significant extraction (regulation of reimbursement rates, price controls, formulary restrictions). The manufacturer can arbitrage by adjusting pricing, supply, and R&D strategy in response to policy changes. High experienced extraction because government negotiation leverage is increasing, but also genuine coordination benefits (reliable demand, insurance pathway, reduced bad-debt write-offs). The constraint requires active enforcement of reimbursement rules and formulary compliance.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICARE TRUST FUND SOLVENCY (SNARE) — The trust fund is constrained (cannot opt out of financing Part D, cannot adjust contribution rates independently, faces insolvency risk from prescription drug cost growth). Experienced extraction is severe because prescription drug spending growth outpaces general inflation and revenue growth. The trust fund has no exit option and no negotiating power to constrain costs — manufacturers set prices, beneficiaries demand access, and the trust fund bears the cost asymmetrically. Powerful agent (federal government) but constrained by political inability to cut benefits or raise taxes sufficiently. The snare is structural despite the nominal power level because the political constraints are insurmountable.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PUBLIC HEALTH ADVOCATE COALITION (SCAFFOLD) — Organized agents (Medicare patient advocates, state health departments, consumer groups) see the benefit as a temporary scaffold addressing an immediate coordination failure (medication access for poor seniors) but with built-in sunset logic: the constraint creates incentives for future price reforms (formulary negotiation, government price-setting, international reference pricing). The constraint is high-theater because the benefit expands coverage while explicitly prohibiting Medicare from negotiating drug prices (negotiation prohibition was a manufacturer-favorable compromise). Exit is constrained (advocates want price reforms, not benefit withdrawal) but the sunset is structural — cost growth will eventually force renegotiation. Theater ratio reflects the compromise: the benefit is performative (solves access without addressing pricing power), but the underlying coordination problem is real.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: HEALTH INSURANCE INTERMEDIARIES (PITON) — The constraint creates administrative overhead that appears to serve coordination but increasingly serves theater: private Medicare Advantage plans, pharmacy benefit managers, and insurance intermediaries manage Part D coverage while capturing rents through administrative margins. The coordination function (connecting beneficiaries to prescriptions) is largely performative because the real work (payment processing, formulary management) is orthogonal to the substantive coordination problem (access). The intermediaries have arbitrage options (adjust margins, change plan design, consolidate) but persist in roles that generate extraction overhead without corresponding value. The constraint persists through institutional inertia rather than functional necessity — it survives because the beneficiaries, manufacturers, and intermediaries all benefit from the current structure despite its high theater ratio.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED NECESSITY (MOUNTAIN) — From a civilizational perspective, the constraint can appear as an immutable law of modern healthcare: aging populations require comprehensive medication access; pharmaceutical innovation requires investment incentives; some form of social insurance is necessary for universal coverage; therefore expanded Medicare coverage is natural and inevitable. This perspective naturalizes what is actually a contingent policy choice — the expansion could have taken alternative forms (government price negotiation, international reference pricing, means-tested coverage, pharmaceutical price controls). The engine's false summit detection should flag this as naturalization of a political arrangement rather than natural law.
constraint_indexing:constraint_classification(sotu_2004_bush_medicare_prescription_drug_benefit, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2004_bush_medicare_prescription_drug_benefit_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2004_bush_medicare_prescription_drug_benefit, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2004_bush_medicare_prescription_drug_benefit, TR),
    TR >= 0.70.

:- end_tests(sotu_2004_bush_medicare_prescription_drug_benefit_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint's base extraction starts at 0.28 in 2003 (genuine coordination benefit, limited scope) and rises to 0.52 by 2009 as costs exceed projections and become asymmetrically borne by taxpayers and trust fund. The rise reflects accumulating extraction: initial expansion appeared to solve coordination problem; subsequent cost growth revealed underlying structural asymmetry (beneficiaries enjoy expanded access while costs are externalized to trust fund and future beneficiaries). Suppression (0.35): Moderate. Seniors have constrained but non-zero exit options (medication compliance reduction, international sourcing, manufacturer assistance programs). Taxpayers have higher suppression (citizenship obligation, tax enforcement) but ultimately constrained. The constraint does not rely on maximum coercion — exit is possible at cost, not impossible. Theater ratio (0.48): Moderate. Initial implementation theater is low (genuine benefit delivery to seniors), but theater increases as the constraint evolves and cost-control performance proves weak. Administrative overhead (intermediaries, formulary management, cost-sharing complexity) grows relative to actual coordination function. By 2009, theater ratio approaches 0.5 as performance-to-overhead ratio declines.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is widest between the senior beneficiary (Rope classification, low experienced extraction, genuine coordination function) and the Medicare Trust Fund (Snare classification, maximum experienced extraction, no exit capacity). Both are truthful observations of the same constraint from different structural positions. The gap reveals that the constraint genuinely solves a coordination problem (medication access) while simultaneously externalizing costs (trust fund burden) to agents who cannot exit. The analytical observer's mountain classification is a false summit — it naturalizes what is actually a contingent policy choice (the expansion could have included price negotiation authority, means-testing, benefit limitations, or alternative financing mechanisms). The false summit occurs when the observer conflates 'aging populations need medications' (genuine coordination problem) with 'the specific structure of Medicare Part D is natural' (contingent policy).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) vary across perspectives based on structural position relative to extraction flow. Senior beneficiaries with constrained exit options experience moderate d (0.35-0.45) — they benefit from the constraint but are partially trapped by medical necessity and limited alternatives. Taxpayers bear asymmetric costs and experience high d (0.75-0.85) — the constraint extracts from them without direct reciprocal benefit. Pharmaceutical manufacturers experience moderate d (0.40-0.50) — they benefit from expanded demand but face regulatory constraints on pricing. The Medicare Trust Fund experiences extremely high d (0.90+) because it is constrained by political inability to control costs and legal obligation to pay — the fullest expression of extraction. The analytical observer experiences canonical d (0.73) representing analytical distance and incomplete structural visibility. These d-values feed the sigmoid f(d) to produce experienced chi (effective extractiveness) for each perspective. Beneficiaries experience negative or low chi; cost-bearers experience high chi; the trust fund experiences maximum chi.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is resolved by recognizing that it is genuinely both coordination AND extraction, depending on perspective. The senior beneficiary's Rope classification is truthful — the constraint solves real coordination problems (medication access, bargaining power imbalance). The taxpayer's Snare classification is equally truthful — the constraint extracts disproportionate costs. The constraint is not mislabeled as pure extraction (Snare) when it contains genuine coordination; nor is it mislabeled as pure coordination (Rope) when it contains significant extraction. The tangled-rope classification at the institutional/immediate perspective (pharmaceutical manufacturers) correctly identifies the hybrid structure: the constraint coordinates drug supply and demand while extracting regulatory rent and pricing power. The constraint's classification across perspectives forms a presheaf: different agents legitimately perceive different types because they occupy different structural positions relative to the extraction and coordination flows. The mandate is resolved by accepting that the classification IS the multiplicity of truthful observations, not by reducing to a single type. The widest perspectival gap (beneficiary vs. trust fund) reveals the contingent policy choices embedded in the structure: the constraint could have been designed to avoid trust fund burden (means-tested, price-controlled, time-limited) without losing the coordination benefit. The gap shows that some extraction is a consequence of the specific policy design (negotiation prohibition, open-ended benefit), not an inherent feature of medication access coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharmaceutical_pricing_mechanism,
    'Is the high cost of Part D extraction driven by fundamental pharmaceutical economics (R&D requirements, patent monopolies, clinical complexity) or by regulatory capture (manufacturer pricing power, absence of government negotiation, prohibition on price controls)?',
    'Comparative analysis: Part D cost growth vs. international pricing (Canada, Australia, UK, Germany); correlation between negotiation legality and price differentials; analysis of R&D productivity given patent protection and price controls in other countries',
    'If fundamental: extraction is natural constraint (mountain-adjacent). If regulatory capture: extraction is contingent policy choice (snare/tangled rope). Cost reduction path and classification change depending on resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pharmaceutical_pricing_mechanism, empirical, 'Whether high Part D costs reflect pharmaceutical economics or regulatory capture').

omega_variable(
    senior_bargaining_power,
    'Are seniors genuinely unable to exit high-cost medications (trapped by medical necessity) or constrained by financial barriers that could be addressed through voucher systems, price negotiation, or international sourcing?',
    'Analysis of medication necessity vs. price-sensitivity; tracking of seniors who reduce compliance due to cost vs. those who maintain compliance through savings programs; evaluation of alternative financing mechanisms',
    'If trapped: seniors genuinely benefit from coordination (rope classification more accurate). If constrained: alternative mechanisms could provide benefit with lower extraction (reframes the constraint as snare masquerading as coordination). Directionality d-value and classification change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(senior_bargaining_power, empirical, 'Whether seniors are trapped by medical necessity or constrained by financial barriers').

omega_variable(
    trust_fund_insolvency_causation,
    'Is Part D the primary driver of Medicare Trust Fund insolvency risk, or is insolvency driven by broader healthcare cost inflation and aging demographics independent of prescription drug coverage?',
    'Actuarial analysis: Part D contribution to projected trust fund depletion; counterfactual modeling of trust fund trajectory without Part D; correlation between Part D cost growth and overall Medicare spending growth',
    'If Part D is primary driver: the constraint bears concentrated responsibility for trust fund harm (snare classification for trust fund perspective is accurate). If Part D is secondary: the constraint is one component of broader structural forces (classification shifts toward rope/scaffold hybrid). Extraction attribution changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trust_fund_insolvency_causation, empirical, 'Whether Part D is primary driver of Medicare Trust Fund insolvency').

omega_variable(
    beneficiary_access_vs_fairness,
    'Does the constraint genuinely expand access to essential medications for seniors who would otherwise forgo treatment, or does it primarily provide financial transfers to seniors who would access medications anyway through alternative pathways (out-of-pocket savings, manufacturer assistance programs, state programs)?',
    'Comparison of medication compliance before and after Part D; analysis of beneficiary population (means-test correlation with actual access barriers); tracking of medication types covered (essential vs. discretionary); evaluation of displacement (does Part D funding simply replace private/state/manufacturer programs)?',
    'If genuine access expansion: coordination function is strong (rope classification more accurate, extraction justified as cost of coordination). If primarily transfer: coordination function is weaker (snare/piton classification more accurate, extraction is less justified). Mandatrophy resolution changes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_access_vs_fairness, empirical, 'Whether Part D genuinely expands access or primarily transfers resources').

omega_variable(
    negotiation_prohibition_optimality,
    'Is the explicit prohibition on Medicare negotiating drug prices a necessary condition for pharmaceutical innovation incentives, or a regulatory capture outcome that inflates prices without corresponding innovation benefit?',
    'Analysis of pharmaceutical R&D productivity in countries with price negotiation (France, Germany) vs. without (US); correlation between price levels and innovation rates; mechanism design analysis of optimal patent protection vs. price control trade-off',
    'If necessary: suppression value decreases (exit constraint is justified). If capture: suppression value should increase (negotiation prohibition artificially constrains exit). Classification and directionality change. Determines whether Part D is Rope (justified compression of alternatives) or Snare (artificial extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(negotiation_prohibition_optimality, conceptual, 'Whether price negotiation prohibition is necessary for innovation or regulatory capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2004_bush_medicare_prescription_drug_benefit, 2003, 2009).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mpdp_theater_2003, sotu_2004_bush_medicare_prescription_drug_benefit, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mpdp_theater_2006, sotu_2004_bush_medicare_prescription_drug_benefit, theater_ratio, 3, 0.42).
narrative_ontology:measurement(mpdp_theater_2009, sotu_2004_bush_medicare_prescription_drug_benefit, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(mpdp_extract_2003, sotu_2004_bush_medicare_prescription_drug_benefit, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mpdp_extract_2006, sotu_2004_bush_medicare_prescription_drug_benefit, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(mpdp_extract_2009, sotu_2004_bush_medicare_prescription_drug_benefit, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2004_bush_medicare_prescription_drug_benefit, resource_allocation).
narrative_ontology:affects_constraint(sotu_2004_bush_medicare_prescription_drug_benefit, medicare_trust_fund_solvency).
narrative_ontology:affects_constraint(sotu_2004_bush_medicare_prescription_drug_benefit, pharmaceutical_pricing_power).
narrative_ontology:affects_constraint(sotu_2004_bush_medicare_prescription_drug_benefit, healthcare_cost_inflation).

% DUAL FORMULATION NOTE:
% The prescription drug benefit expansion decomposes into multiple structurally distinct constraints: (1) medication_access_coordination (ε ≈ 0.20, Rope) — the genuine coordination problem of connecting seniors to prescriptions; (2) pharmaceutical_pricing_extraction (ε ≈ 0.65, Snare) — manufacturer pricing power enabled by negotiation prohibition; (3) trust_fund_burden (ε ≈ 0.70, Snare) — cost externalization to future beneficiaries and taxpayers. These three constraints are linked via network effects: the benefit expansion solves (1) while simultaneously enabling (2) and creating (3). The unified 'Medicare Part D' story aggregates these distinct mechanisms into a single tangled-rope narrative; decomposition would clarify which mechanisms are genuinely problematic (pharmaceutical pricing) vs. genuinely beneficial (medication access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2004_bush_medicare_prescription_drug_benefit, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
