% ============================================================================
% CONSTRAINT STORY: cholesterol_pill_cost
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: economic/healthcare/pharmaceutical
 *
 * SUMMARY:
 *   Patented cholesterol medications exemplify the core tension in
 *   pharmaceutical intellectual property: patents provide crucial innovation
 *   incentives by allowing manufacturers to recoup R&D investments through
 *   temporary monopoly pricing, but this same mechanism creates severe access
 *   barriers for low-income patients who cannot afford the monopoly price and
 *   have no substitute (switching to generic alternatives is impossible, and
 *   clinical necessity makes non-treatment a poor option). The constraint
 *   exhibits distinct structures depending on perspective. For manufacturers,
 *   the patent is a legitimate coordination mechanism solving the
 *   underinvestment problem of drug development. For healthcare payers, it
 *   represents mixed coordination (incentivizing R&D) and extraction (raising
 *   total treatment costs). For low-income patients, it is a pure snare: they
 *   are trapped by medical need, cannot exit through generic alternatives,
 *   and face suppression in the form of active patent enforcement. The
 *   extractiveness has increased over the interval (0.35 to 0.58) as the
 *   manufacturer has extended patent life through evergreening strategies
 *   (reformulations, combination therapies) and price increases have outpaced
 *   inflation. The theater ratio remains low (0.45) because the constraint
 *   functions mechanically — patents are not maintained through performative
 *   institutional ritual but through straightforward legal enforcement.
 *
 * KEY AGENTS:
 *   - Low-Income Patients: Primary victim (powerless/trapped) — medical need creates inescapable demand; patent excludes generics; financial barriers force medication denial or other necessity trade-offs
 *   - Uninsured/Underinsured Patients: Primary victim (moderate/constrained) — insurance formulary restrictions and high copayments create barriers; can substitute with lower-efficacy alternatives but at clinical cost
 *   - Healthcare Systems (Payers): Secondary actor (organized/mobile) — experience mixed coordination and extraction; can negotiate prices, lobby for policy change, but are constrained by clinical obligation and benefit manager contracts
 *   - Pharmaceutical Manufacturer: Primary beneficiary (institutional/arbitrage) — captures monopoly rents from patent protection; sees constraint as enabling innovation; has high exit options (can focus R&D on other conditions)
 *   - Patent System Institution: Institutional actor (institutional/arbitrage) — maintains legal enforcement mechanism; increasingly theater-like as evergreening strategies extend exclusivity beyond genuine innovation
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees extraction through global lens where innovation benefits are concentrated in high-income markets while access barriers affect low-income populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cholesterol_pill_cost, 0.58).
domain_priors:suppression_score(cholesterol_pill_cost, 0.68).
domain_priors:theater_ratio(cholesterol_pill_cost, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cholesterol_pill_cost, extractiveness, 0.58).
narrative_ontology:constraint_metric(cholesterol_pill_cost, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cholesterol_pill_cost, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cholesterol_pill_cost, snare).
narrative_ontology:human_readable(cholesterol_pill_cost, "Cost of Patented Cholesterol Medication").
narrative_ontology:topic_domain(cholesterol_pill_cost, "economic/healthcare/pharmaceutical").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cholesterol_pill_cost, pharmaceutical_manufacturer).
narrative_ontology:constraint_victim(cholesterol_pill_cost, low_income_patients).
narrative_ontology:constraint_victim(cholesterol_pill_cost, uninsured_patients).
narrative_ontology:constraint_victim(cholesterol_pill_cost, healthcare_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENT (SNARE) — Cannot afford the patented medication; faces choice between medication and other necessities. No exit option: patent protection prevents generic alternatives, insurance coverage is denied, and switching to alternative treatments means accepting higher cardiovascular risk. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INSURED PATIENT (SNARE) — Faces high copayments or formulary restrictions. Can switch medications but at clinical cost (switching to lower-efficacy alternatives). Patent excludes generics, and insurance tier placement is dictated by pharmacy benefit manager contracts (themselves often influenced by manufacturer rebates). d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.64.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HEALTHCARE SYSTEM/PAYER (TANGLED ROPE) — Experiences mixed coordination and extraction. Coordination function: patent incentivizes R&D investment in novel cholesterol therapeutics that might not exist under generic competition. Extraction function: high drug prices increase overall treatment costs, strain budgets, and force formulary restrictions that limit patient access. Healthcare systems have some mobility (can negotiate prices, substitute alternative therapies, lobby for patent reform) but are constrained by clinical obligation to offer effective treatments and contractual lock-in with pharmacy benefit managers. d≈0.52, f(d)≈0.67, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(cholesterol_pill_cost, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: PHARMACEUTICAL MANUFACTURER (ROPE) — Primary beneficiary. Patent protection solves a coordination problem: without exclusivity, firms would underinvest in drug development (market would be flooded with generics immediately upon approval, destroying R&D incentives). The manufacturer sees the constraint as enabling innovation coordination — their price-setting power is the legitimate return on R&D risk capital. High exit options: can shift R&D focus to other therapeutic areas, license patents internationally, extend patent life through regulatory strategies (evergreening). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(cholesterol_pill_cost, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT SYSTEM INSTITUTION (PITON) — Theater ratio = 0.45 (below piton threshold, but this is a piton from institutional inertia perspective). The patent system's primary function was to incentivize innovation under 20th-century conditions (high R&D costs, long drug development timelines, weak copycat mechanisms). Modern conditions have degraded this function: (1) biologics and manufacturing complexity already provide 10-15 years of de facto exclusivity before biosimilars appear; (2) regulatory data exclusivity provides additional protection layers; (3) evergreening strategies (minor reformulations, combination therapies) extend exclusivity beyond patent expiration without new innovation. The institutional theater persists because alternatives (prize systems, open-source drug development, government R&D funding) have not fully matured. Suppression is high (0.68) — patient access is actively restricted by legal enforcement. d≈0.10, f(d)≈-0.06, σ=1.2 → χ≈-0.03.
constraint_indexing:constraint_classification(cholesterol_pill_cost, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational timescale viewing global access, this is a snare: patent protection creates a structural asymmetry where pharmaceutical manufacturers extract rent from patients in high-income countries to fund innovation, while patients in low-income countries have limited access at any price. The system cannot be exited (patients cannot choose to not need statins; they cannot become unpatentable), and the suppression is structural (patent law enforcement is active and global). d≈0.88, f(d)≈1.35, σ=1.2 → χ≈0.93.
constraint_indexing:constraint_classification(cholesterol_pill_cost, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): Moderate-high. The medication's efficacy is genuine and valuable, but the price is set well above manufacturing and distribution cost, generating substantial economic surplus capture by the manufacturer. The ε is not higher (0.70+) because the innovation is real — the R&D investment does require some monopoly protection to justify. However, the 20-year patent term often extends beyond the period needed to recover development costs, and evergreening strategies further increase the rent-extraction period. Suppression (0.68): High. Patent law actively prevents generic entry, and pricing creates financial barriers to access. Patients cannot legally produce or import generics; they cannot modify the medication to escape patent claims; they cannot choose not to need the medication. The constraint is enforced through law, not just market mechanisms. Theater ratio (0.45): Low-moderate. The constraint functions mechanistically — patents are straightforward legal tools, not maintained through institutional theater. However, the claim that patents 'incentivize innovation' is partly performative; much cholesterol drug development is now incremental (me-too reformulations) rather than breakthrough, yet the patent system treats all claims equally. The theater ratio is not higher because the mechanism is transparent, but it is not zero because the innovation narrative masks incremental optimization as breakthrough research.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. The manufacturer sees a coordination solution (Rope) — enabling innovation that would not occur under competition. Low-income patients see a snare (Snare) — pure extraction with no escape. Healthcare systems see a hybrid (Tangled Rope) — the system enables innovation but extracts unsustainable costs. The patent system itself sees its function degrading (Piton) — evergreening extends exclusivity beyond innovation into pure rent-seeking. The analytical observer, viewing global access, sees extraction (Snare) — innovation subsidized by high-income patients and denied to low-income populations. The perspectival gap reveals that no single type captures the full structure; the constraint is legitimately coordination-enabling for innovation while simultaneously being legitimate-rent-extracting from patients.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-income patients: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. No exit: medical need, patent blocks generics, financial barriers are absolute. Insured patients: Victim + constrained → d≈0.75, f(d)≈1.10. High extraction but not maximal; can switch to alternatives (at clinical cost), have some insurance cushioning. Healthcare systems: Victim + organized + mobile → d≈0.52, f(d)≈0.67. Moderate extraction; can negotiate prices, substitute therapies, lobby for policy change. Manufacturer: Beneficiary + institutional + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; can set prices, extend patents, shift R&D focus. Patent system: Institutional + arbitrage → d≈0.10, f(d)≈-0.06. Net beneficiary (maintains legal monopoly rents); but piton classification comes from theater degradation, not from positive d. Analytical observer: d≈0.88, f(d)≈1.35. High extraction from global perspective; innovation concentrated in high-income markets, access barriers affect low-income globally.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint simultaneously serves coordination (innovation incentive) and extraction (monopoly rent capture). The manufacturer's legitimate claim that patents incentivize R&D is true at the margin — without patent protection, some fraction of current innovation would not occur. However, the empirical fraction is uncertain (omega_1: how much R&D actually depends on patent rents?), and current patent terms and evergreening practices substantially exceed the minimum needed for incentive. The constraint resolves from Rope (pure coordination) to Tangled Rope (mixed) to Snare (pure extraction) as one moves from manufacturer perspective to healthcare system to patient perspective. The mandatrophy is not 'is this coordination or extraction?' but 'at what point does the coordination function degrade into pure rent-seeking?' The theater ratio (0.45) is low because the constraint is enforced transparently (patent law), not hidden behind institutional ritual. This transparency is actually a liability for Tangled Rope classification — without theater obscuring the extraction mechanism, the constraint appears more nakedly extractive than it actually is. The resolution is honest: this is a Snare from the patient perspective and will remain so unless the patent term is shortened, generic entry barriers are lowered, or alternative incentive structures (prize funds, government R&D) substitute for monopoly rents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    r_and_d_incentive_dependence,
    'How much of current cholesterol drug R&D genuinely depends on patent-based revenue capture versus alternative incentive structures (government contracts, advance market commitments, prize funds)?',
    'Comparative analysis of R&D spending and molecule discovery rates under different patent regimes (e.g., US pre/post Bayh-Dole, countries with compulsory licensing, public drug development programs)',
    'If R&D is highly dependent (>70% of innovation driven by patent rents): patent extraction is justified coordination overhead. If weakly dependent (<30%): current patent system is pure extraction with institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r_and_d_incentive_dependence, empirical, 'Dependence of cholesterol drug R&D on patent-based revenue').

omega_variable(
    clinical_necessity_threshold,
    'At what patient income/insurance level does the medication transition from ''beneficial optional improvement'' to ''medically necessary to prevent death''?',
    'Longitudinal health outcome data comparing patients with and without access; cost-benefit analysis of medication efficacy gain versus out-of-pocket cost burden',
    'If threshold is high (only severe familial hypercholesterolemia): medication is elective, suppression is lower. If threshold is low (all high-risk patients): medication is essential, suppression is high (0.68 confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(clinical_necessity_threshold, empirical, 'Clinical necessity threshold for cholesterol medication access').

omega_variable(
    generic_entry_timeline_counterfactual,
    'If the patent expired tomorrow, how quickly would generic equivalents reduce the price, and at what price would manufacturing reach equilibrium?',
    'Historical precedent from other statin patents (atorvastatin, rosuvastatin, pravastatin patent expirations); manufacturing cost analysis; generic market entry timelines',
    'If generics reduce price 80-90% within 1 year: current patent generates rents well above innovation cost recovery. If reduced only 20-30%: manufacturing/distribution has high legitimate costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generic_entry_timeline_counterfactual, empirical, 'Generic entry price trajectory and equilibrium').

omega_variable(
    international_pharmaceutical_arbitrage,
    'How much of the global market extraction is enabled by price discrimination (different prices in different countries) versus a single global monopoly price?',
    'International price comparison data for the same molecule; analysis of parallel importation barriers; assessment of gray-market generic access in low-income countries',
    'If arbitrage is significant (3-5x price differences): suppression is partially enabled by legal trade restrictions (additional extraction vector). If prices are globally similar: suppression is extraction via scarcity, not discrimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_pharmaceutical_arbitrage, empirical, 'International price discrimination and arbitrage barriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cholesterol_pill_cost, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chol_tr_t0, cholesterol_pill_cost, theater_ratio, 0, 0.38).
narrative_ontology:measurement(chol_tr_t10, cholesterol_pill_cost, theater_ratio, 10, 0.41).
narrative_ontology:measurement(chol_tr_t20, cholesterol_pill_cost, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(chol_be_t0, cholesterol_pill_cost, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chol_be_t10, cholesterol_pill_cost, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(chol_be_t20, cholesterol_pill_cost, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cholesterol_pill_cost, resource_allocation).
narrative_ontology:affects_constraint(cholesterol_pill_cost, insulin_patent_pricing).
narrative_ontology:affects_constraint(cholesterol_pill_cost, rare_disease_drug_access).
narrative_ontology:affects_constraint(cholesterol_pill_cost, generic_drug_entry_barriers).

% DUAL FORMULATION NOTE:
% The cholesterol medication cost constraint is one member of a pharmaceutical pricing family. Upstream constraints include generic entry barriers (structural/legal); downstream constraints include specific disease categories (insulin, rare diseases) where the same patent mechanism operates with different patient vulnerability profiles. The network reflects causal dependency: patent mechanism affects all downstream drug prices, and legal barriers to generic entry are the common structural element.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
