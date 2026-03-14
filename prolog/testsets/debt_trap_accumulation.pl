% ============================================================================
% CONSTRAINT STORY: debt_trap_accumulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_trap_accumulation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: debt_trap_accumulation
 *   human_readable: Debt Trap Accumulation
 *   domain: economic/financial
 *
 * SUMMARY:
 *   Debt trap accumulation is a structural constraint in which creditors
 *   extract wealth from borrowers through compounding interest, unequal
 *   bargaining power, and suppression mechanisms (wage garnishment, credit
 *   destruction, asset seizure, institutional tracking) that prevent exit.
 *   The constraint is intentionally designed: predatory lending targets
 *   borrowers below income thresholds where debt service becomes impossible
 *   without sacrificing subsistence needs. Interest compounds faster than
 *   borrower income grows, creating permanent indebtedness. Suppression
 *   operates through legal enforcement systems that treat default as a crime
 *   (in some jurisdictions literally — debt-prison frameworks) and
 *   social/institutional mechanisms that make exit costlier than compliance.
 *   The constraint appears as a pure snare (Snare) from the perspective of
 *   powerless, trapped borrowers; as an extractive coordination mechanism
 *   (Tangled Rope) from moderate borrowers with some mobility; as pure
 *   coordination (Rope) from creditor institutions; and as a structural
 *   poverty function (Snare) from civilizational scope. Theater ratio (0.35)
 *   is relatively low — debt traps are not sustained through performative
 *   mechanisms but through material enforcement.
 *
 * KEY AGENTS:
 *   - Borrowers with inadequate income (powerless/trapped): Primary victims — structurally unable to service debt while meeting subsistence needs; experience maximum suppression through wage garnishment and asset seizure
 *   - Precarious workers (moderate/constrained): Secondary victims — income volatility makes debt service unpredictable; high barriers to exit through institutional employment tracking and credit scoring
 *   - Economically vulnerable populations (powerless/trapped): Structural victims — systemic exclusion from traditional credit creates dependency on predatory lenders (payday loans, subprime mortgages, title loans)
 *   - Creditors and debt servicers (institutional/arbitrage): Primary beneficiaries — extract through interest compounding and institutional enforcement mechanisms; operate within legal frameworks that externalize suppression
 *   - Debt securitizers (institutional/arbitrage): Secondary beneficiaries — pool debt into financial instruments, concentrating extraction and obscuring accountability; have options to sell portfolios or diversify
 *   - Debtors' coalitions (organized/constrained): Secondary agents — organized collective action creates partial exit options but faces legal/social repression; demonstrate that organization is possible at cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_trap_accumulation, 0.68).
domain_priors:suppression_score(debt_trap_accumulation, 0.75).
domain_priors:theater_ratio(debt_trap_accumulation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_trap_accumulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(debt_trap_accumulation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(debt_trap_accumulation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_trap_accumulation, snare).
narrative_ontology:human_readable(debt_trap_accumulation, "Debt Trap Accumulation").
narrative_ontology:topic_domain(debt_trap_accumulation, "economic/financial").

domain_priors:requires_active_enforcement(debt_trap_accumulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_trap_accumulation, creditors).
narrative_ontology:constraint_beneficiary(debt_trap_accumulation, debt_securitizers).
narrative_ontology:constraint_victim(debt_trap_accumulation, borrowers_with_inadequate_income).
narrative_ontology:constraint_victim(debt_trap_accumulation, precarious_workers).
narrative_ontology:constraint_victim(debt_trap_accumulation, economically_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED BORROWER (SNARE) — Structurally immobilized by debt obligations exceeding escape capacity. Income insufficient to service debt while meeting subsistence needs. No collateral to liquidate. Unable to default without catastrophic consequences (bankruptcy stigma, asset seizure, wage garnishment). Experiences maximum extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(debt_trap_accumulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRAINED BORROWER / INTERGENERATIONAL (SNARE) — At generational time horizon, sees debt patterns inherited from parents or forced onto children. Structural barriers include inadequate wage growth relative to living costs, limited educational access (blocked by debt), geographic immobility (tied to employment for wage garnishment avoidance). High suppression through institutional mechanisms (employment verification, credit scoring, asset tracking). Exit costlier than immediate default but nominally possible. Still experiences snare classification due to suppression magnitude and multi-generational lock-in.
constraint_indexing:constraint_classification(debt_trap_accumulation, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCIALLY LITERATE BORROWER WITH ALTERNATIVES (TANGLED ROPE) — Mobile exit options through bankruptcy discharge, debt negotiation, or income diversification (gig economy, relocation). Benefits from legitimate credit access for housing/education. Experiences coordination function (credit system allocates capital) alongside asymmetric extraction (interest compounds faster than earned income). Suppression is surmountable at moderate cost (credit score impact, legal fees). Effective extraction significant but not maximal.
constraint_indexing:constraint_classification(debt_trap_accumulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: CREDITOR INSTITUTION (ROPE) — Experiences the constraint as pure coordination: managing loan portfolios, pricing risk, regulatory compliance. Has arbitrage options (securitization, sale of debt, portfolio diversification). Benefits from the constraint's extraction mechanisms without direct enforcement costs. Suppression is externalized onto borrowers; creditor operates within legal frameworks. Perceives debt accumulation as market-clearing mechanism rather than extraction.
constraint_indexing:constraint_classification(debt_trap_accumulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DEBTORS' COALITION (TANGLED ROPE) — Organized collective action (strike debt campaigns, debt jubilee movements) has real but constrained power. Can negotiate partial relief, policy changes, or collective default mechanisms. Suppression persists through legal frameworks and social stigma but is partially surmountable through political organizing. Both coordination and extraction occur: collective action is coordinated response, but the constraint itself extracts from the organized group through repressive legal actions, debt assignment to collection agencies, reputation systems.
constraint_indexing:constraint_classification(debt_trap_accumulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From civilizational scope, debt trap accumulation is a structural mechanism for converting temporary income insufficiency into permanent poverty and intergenerational transmission of disadvantage. The constraint's extractiveness (0.68) is below the immediate snare threshold (0.66 chi) but suppression (0.75) is extreme. The mechanism relies on making borrowers structurally powerless through compounding interest, reduced mobility, and institutional barriers. Theater ratio (0.35) is low — this is not performative, it is material extraction.
constraint_indexing:constraint_classification(debt_trap_accumulation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_trap_accumulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_trap_accumulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_trap_accumulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_trap_accumulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(debt_trap_accumulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts through three mechanisms: (1) compounding interest that grows faster than income, (2) suppression that prevents exit and forces compliance, (3) asymmetric information (lender knows risk; borrower does not). The measurement trajectory (0.42 → 0.68 over 10 years) reflects how debt accumulates and borrowers become progressively more trapped — the initial debt obligation is manageable (0.42 extractiveness) but compounds into immobility (0.68). This trajectory captures the mechanism: it is not predatory at origination but becomes extractive through time. Suppression (0.75): High. Material barriers to exit include wage garnishment (direct income capture), asset seizure (eliminates emergency reserves), credit score destruction (blocks alternative credit), geographic immobility (employment tracking prevents relocation), and in some contexts literal imprisonment for debt. These mechanisms are legally enforced and institutionalized. Theater ratio (0.35): Low. Debt traps are not sustained through narrative, performance, or ideological cover. The mechanism is straightforward: borrow, fail to repay on schedule, accrue interest, attempt to repay, fall further behind. The apparent 'reasonableness' of interest charges and the legality of enforcement provide some cover story, but the core mechanism is transparent material extraction. The theater ratio is not higher because the constraint does not rely on pretense — it relies on legal authority and material enforcement.
 *
 * PERSPECTIVAL GAP:
 *   Creditors perceive Rope (pure coordination); trapped borrowers perceive Snare (pure extraction); constrained borrowers perceive Tangled Rope (mixed); organized borrowers perceive Tangled Rope with sunset potential (scaffold for policy change). This gap is not a misunderstanding but a genuine structural difference in how the constraint operates for each actor. The creditor institution truly is allocating credit (coordination function) even as it extracts (asymmetric distribution of benefit). The trapped borrower truly cannot exit (snare condition) even though other agents experience choices (rope condition for creditors). The perspectival gap is the diagnostic — it shows that the constraint is extractive, not that some perspectives are wrong.
 *
 * DIRECTIONALITY LOGIC:
 *   Directional d values flow from beneficiary/victim declarations and exit options. Trapped borrowers (victims + trapped exit) derive d ≈ 0.95, producing f(d) ≈ 1.42 and chi ≈ 0.68 × 1.42 × 1.0 ≈ 0.96 (effective extraction near-maximal). Creditor institutions (beneficiaries + arbitrage exit) derive d ≈ 0.05, producing f(d) ≈ -0.12 and chi ≈ 0.68 × (-0.12) × 1.0 ≈ -0.08 (negative effective extraction — they perceive benefit, not burden). Constrained moderate borrowers (victims + constrained exit) derive d ≈ 0.75, producing f(d) ≈ 1.15 and chi ≈ 0.68 × 1.15 × 1.0 ≈ 0.78 (high effective extraction, less extreme than trapped). The directionality derivation explains why creditors see Rope while borrowers see Snare — they are measuring different d values through the same constraint structure.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is clearly Snare from the victim perspective. Extractiveness (0.68), suppression (0.75), and chi (≥0.66) all exceed snare thresholds. The mandatrophy — the ambiguity between coordination and extraction — is resolved by noting that the constraint genuinely provides coordination benefits (credit allocation, risk pricing, portfolio management) but concentrates those benefits on creditors while concentrating extraction on borrowers. This is not a single constraint misclassified as two types; it is two structures in one: a coordination system for creditors (Rope) and an extraction system for borrowers (Snare). The entire system is Snare at the civilizational level because the coordination function is secondary to the extraction function — the system could allocate credit without the suppression mechanisms, but those mechanisms exist because extraction is the primary goal. The beneficiary/victim declarations and the directional derivation resolve the mandatrophy: the creditor sees Rope, the borrower sees Snare, the system is Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsistence_threshold_ambiguity,
    'Below what income level do debt obligations become structurally impossible to service alongside subsistence needs?',
    'Time-budget analysis: ratio of debt service + living expenses vs. available income; cross-national comparison of bankruptcy trigger points',
    'If threshold is high (low income ratio): more borrowers classified as trapped (snare perspective dominant). If threshold is low: more borrowers appear constrained rather than trapped (tangled rope perspective viable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsistence_threshold_ambiguity, empirical, 'Income threshold distinguishing trapped from constrained debt obligations').

omega_variable(
    interest_compounding_extraction_proportion,
    'What fraction of a borrower''s debt burden is attributable to interest compounding vs original principal?',
    'Loan-level amortization analysis across product types (credit cards, payday loans, subprime mortgages, student loans); correlation of interest magnitude with borrower income level',
    'If interest > 60% of total repayment: extraction mechanism is clearly superlinear (snare). If interest < 30%: mechanism appears more like standard coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interest_compounding_extraction_proportion, empirical, 'Proportion of debt attributable to compounding interest').

omega_variable(
    default_consequence_severity_distribution,
    'Are default consequences (wage garnishment, asset seizure, credit destruction, imprisonment in debt-prison contexts) applied uniformly or concentrated on powerless agents?',
    'Enforcement pattern analysis: prosecution/collection rates by borrower income level, attorney representation, geographic jurisdiction; comparison of institutional vs individual default treatment',
    'If concentrated on powerless: confirms snare classification through selective suppression. If distributed: extraction mechanism is more symmetric, moving toward tangled rope or rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(default_consequence_severity_distribution, empirical, 'Distributional pattern of default enforcement consequences').

omega_variable(
    debt_securitization_extraction_opacity,
    'To what extent does securitization of debt (pooling into financial instruments, sale to institutional investors) obscure the extractive mechanism from borrowers and creditors alike?',
    'Trace capital flow from borrower payment through servicer, investor, and financial instruments; identify where extraction accumulates and who captures it; measure information asymmetry between borrowers and actual beneficial owners',
    'If high opacity: creditors are also constrained agents within a larger extraction system (institutional snare). If low opacity: creditors are clear beneficiaries (institutional rope or tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_securitization_extraction_opacity, empirical, 'Opacity of debt securitization processes and true extraction concentration').

omega_variable(
    behavioral_lock_in_mechanisms,
    'Are borrowers trapped by material barriers (legal, economic) or by internalized beliefs (financial illiteracy, shame, identity as debtor)?',
    'Comparative analysis: debt-load outcomes for borrowers with identical income but different information/counseling access; exit rates after identity-shifting interventions (bankruptcy counseling, financial therapy); geographic jurisdiction effects',
    'If primarily material barriers: trapped exit classification is accurate (snare). If significant behavioral lock-in: some borrowers are identity_locked (perceive constraint as unchangeable from within identity frame) — classification shifts to rope from identity_locked perspective at biographical time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_lock_in_mechanisms, empirical, 'Relative contribution of material vs behavioral trapping mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_trap_accumulation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_tr_t0, debt_trap_accumulation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(debt_tr_t5, debt_trap_accumulation, theater_ratio, 5, 0.3).
narrative_ontology:measurement(debt_tr_t10, debt_trap_accumulation, theater_ratio, 10, 0.35).
narrative_ontology:measurement(debt_tr_t8, debt_trap_accumulation, theater_ratio, 8, 0.33).

% Extraction over time
narrative_ontology:measurement(debt_be_t0, debt_trap_accumulation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(debt_be_t5, debt_trap_accumulation, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(debt_be_t10, debt_trap_accumulation, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(debt_be_t8, debt_trap_accumulation, base_extractiveness, 8, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_trap_accumulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(debt_trap_accumulation, 0.18).
narrative_ontology:affects_constraint(debt_trap_accumulation, credit_score_punishment).
narrative_ontology:affects_constraint(debt_trap_accumulation, wage_garnishment_mechanisms).
narrative_ontology:affects_constraint(debt_trap_accumulation, predatory_lending_cycles).
narrative_ontology:affects_constraint(debt_trap_accumulation, intergenerational_poverty_transmission).

% DUAL FORMULATION NOTE:
% Debt trap accumulation is upstream of multiple downstream constraints: credit scoring punishment (individual borrower consequences), wage garnishment (institutional enforcement), predatory lending cycles (systemic reproduction), and intergenerational poverty transmission (family-level reproduction). Each downstream constraint has its own extractiveness value reflecting its structural specificity, but all depend on the debt trap mechanism as their foundation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_trap_accumulation, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
