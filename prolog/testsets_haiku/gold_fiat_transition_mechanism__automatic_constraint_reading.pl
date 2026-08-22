% ============================================================================
% CONSTRAINT STORY: gold_fiat_transition_mechanism__automatic_constraint_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gold_fiat_transition_mechanism__automatic_constraint_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: gold_fiat_transition_mechanism__automatic_constraint_reading
 *   human_readable: Gold-to-Fiat Transition: Automatic Constraint Elimination
 *   domain: economic/political
 *
 * SUMMARY:
 *   The gold standard operated as an automatic, decentralized constraint on
 *   monetary expansion: money creation was mechanically limited by physical
 *   gold reserves, and any attempt to exceed reserves triggered automatic
 *   rebalancing through gold flows and balance-of-payments discipline. This
 *   reading frames the transition to fiat money as the elimination of that
 *   automatic constraint and its replacement with discretionary central bank
 *   authority. The constraint changed from material (gold-physics-enforced)
 *   to institutional (law-and-enforcement dependent). Beneficiary: monetary
 *   authorities and reserve-currency issuers gained discretion. Victim:
 *   creditors lost the veto power that gold redemption provided. The measure
 *   of extraction is the shift from passive mechanism to active authority
 *   wielded asymmetrically.
 *
 * KEY AGENTS:
 *   - monetary_authorities: central banks and treasuries; gained discretion; institutional power
 *   - creditor_class: bondholders, savers, rentiers; lost automatic veto; powerful but constrained
 *   - domestic_debtors: governments and corporations; benefited from inflation-driven debt reduction
 *   - reserve_currency_issuers: the US and allies; gained seigniorage and monetary hegemony
 *   - gold_standard_disciplinarians: economists and policy thinkers; lost structural argument for discipline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72).
domain_priors:suppression_score(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.58).
domain_priors:theater_ratio(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(gold_fiat_transition_mechanism__automatic_constraint_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gold_fiat_transition_mechanism__automatic_constraint_reading, tangled_rope).
narrative_ontology:human_readable(gold_fiat_transition_mechanism__automatic_constraint_reading, "Gold-to-Fiat Transition: Automatic Constraint Elimination").
narrative_ontology:topic_domain(gold_fiat_transition_mechanism__automatic_constraint_reading, "economic/political").

domain_priors:requires_active_enforcement(gold_fiat_transition_mechanism__automatic_constraint_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gold_fiat_transition_mechanism__automatic_constraint_reading, '32875b21-dccf-4307-8ada-9c879a6f65eb').
narrative_ontology:cs_kernel_codification('32875b21-dccf-4307-8ada-9c879a6f65eb', distributed).
narrative_ontology:cs_authority_grounding('32875b21-dccf-4307-8ada-9c879a6f65eb', extraction).
narrative_ontology:cs_interpretation_layer_present('32875b21-dccf-4307-8ada-9c879a6f65eb').
narrative_ontology:cs_reading_relation('32875b21-dccf-4307-8ada-9c879a6f65eb', gold_fiat_transition_mechanism__creditor_discipline_reading, coexists_with).
narrative_ontology:cs_reading_relation('32875b21-dccf-4307-8ada-9c879a6f65eb', gold_fiat_transition_mechanism__composite_overdetermination_reading, influences).
narrative_ontology:cs_axiom('32875b21-dccf-4307-8ada-9c879a6f65eb', foundational, monetary_constraint_automaticity_essential).
narrative_ontology:cs_axiom_status(monetary_constraint_automaticity_essential, overridden).
narrative_ontology:cs_axiom_grounding('32875b21-dccf-4307-8ada-9c879a6f65eb', monetary_constraint_automaticity_essential, empirically_contingent).
narrative_ontology:cs_axiom('32875b21-dccf-4307-8ada-9c879a6f65eb', foundational, discretionary_authority_extraction_dominant).
narrative_ontology:cs_axiom_status(discretionary_authority_extraction_dominant, holdable).
narrative_ontology:cs_axiom_grounding('32875b21-dccf-4307-8ada-9c879a6f65eb', discretionary_authority_extraction_dominant, instrumental).
narrative_ontology:cs_reference_frame('32875b21-dccf-4307-8ada-9c879a6f65eb', gold_standard_constraint_regime).
narrative_ontology:cs_drift_state('32875b21-dccf-4307-8ada-9c879a6f65eb', contemporary_post_1971_fiat_hegemony, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('32875b21-dccf-4307-8ada-9c879a6f65eb', '').
narrative_ontology:cs_kernel_id(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_standard_disciplinarians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, domestic_debtors).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions_wage_workers).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, exporters_trade_surplus_nations).
narrative_ontology:constraint_beneficiary(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_issuers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions_wage_workers).
narrative_ontology:constraint_victim(gold_fiat_transition_mechanism__automatic_constraint_reading, savers_rentiers).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, central_bank_discretion_doctrine).
narrative_ontology:constraint_vindicates(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_policy_flexibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Central banks and government treasuries that operated under gold standard constraints now possess discretionary authority to expand money supply without reserve backing. They set interest rates, manage inflation, conduct quantitative easing, and engineer currency devaluation for competitive advantage. They justify the transition as enabling counter-cyclical policy and economic stimulus; they frame the gold standard as deflationary rigidity. They directly benefit from the discretion the transition granted.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Holders of government bonds, international creditors, savers expecting currency stability, and rentiers whose wealth depends on real value of fixed-income assets. Under the gold standard, they possessed a veto: if a government spent beyond its reserves, creditors could demand gold redemption and force fiscal discipline. The transition eliminated this automatic constraint, replacing it with discretionary monetary expansion that erodes real asset values through inflation. They retain legal claims but lost the physical backing that made claims enforceable.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, creditor_class, payer,
    powerful, generational, constrained, global).

% Governments and corporations that carry debt denominated in their own currency. The transition allows the monetary authority to inflate away the real burden of their obligations. Nominal debt stays fixed while inflation erodes its value. They benefit from monetary discretion they could not have accessed under gold-standard constraints.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, domestic_debtors, beneficiary,
    organized, biographical, constrained, national).

% Wage earners whose purchasing power depends on whether nominal wage growth exceeds inflation. The transition created inflationary pressure that eroded real wages for workers without cost-of-living adjustment mechanisms, especially in the 1970s. Some workers benefited from full employment policies enabled by monetary discretion; many bore the hidden tax of inflation on savings and fixed wages. Their position is genuinely mixed and bifurcated by bargaining power.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions_wage_workers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, labor_unions_wage_workers, beneficiary).

% Nations that ran trade surpluses could now accumulate fiat reserves instead of gold, then deploy those reserves to finance their own deficits or purchase real assets abroad. The transition enabled currency manipulation and competitive devaluation as tools of trade advantage. Surplus nations benefited from the discretion to set exchange rates rather than being constrained by automatic gold-flow discipline.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, exporters_trade_surplus_nations, beneficiary,
    powerful, generational, mobile, global).

% The United States and (later) other reserve-currency-issuing nations gained the power to run persistent deficits, finance government spending through seigniorage, and impose monetary discipline on other economies through reserve-currency hegemony. The dollar's role as international reserve was decoupled from gold backing, enabling the US to export inflation and absorb real resources from the rest of the world.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_issuers, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(gold_fiat_transition_mechanism__automatic_constraint_reading, reserve_currency_issuers, beneficiary).

% Economists, policymakers, and institutional defenders of the gold standard who argued it provided automatic, impartial, and depoliticized constraint on monetary expansion. They lost the structural argument that constraints were 'natural' and became advocates in a political contest rather than administrators of an inevitable rule. The transition forced them to defend discipline through discretionary central bank orthodoxy rather than mechanical law.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_standard_disciplinarians, payer,
    analytical, generational, analytical, universal).

% Individuals whose wealth consists of cash savings, bonds, and fixed-income assets. The transition exposed them to inflation erosion without the automatic hedge the gold standard provided (redemption at fixed parity). They cannot exit into gold without explicit permission; they bear the purchasing-power risk the transition created.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, savers_rentiers, payer,
    moderate, civilizational, constrained, national).

% The IMF, World Bank, and other multilateral agencies emerged to manage the transition's destabilization. They inherited the role of enforcing fiscal and monetary discipline that the gold standard had automated, now through conditional lending and policy conditionality. They observe and referee the constraint's operation.
narrative_ontology:constraint_stakeholder(gold_fiat_transition_mechanism__automatic_constraint_reading, international_financial_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gold_fiat_transition_mechanism__automatic_constraint_reading, monetary_authorities).
narrative_ontology:fixing_cost_class(gold_fiat_transition_mechanism__automatic_constraint_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Eliminates the coordination problem of verifying claims to base money through physical reserves. Instead of each creditor evaluating a currency's gold backing, a single central authority certifies fiat value through monopoly coinage and central bank operations. Solves the trust problem: 'Is this currency redeemable?' becomes 'Does the central bank issue it and enforce legal tender status?'
% TRANSFER_FUNCTION: Transfers seigniorage—the real wealth gain from creating money without backing—from creditors (who lose protection against debasement) to monetary authorities and beneficiary debtors (who gain discretion). The initial transfer is from the gold-standard creditor veto power to central bank discretion; the ongoing transfer is from savers/creditors to debtors/money-issuers through inflation.
% ABSENT_VOICES: Gold-standard economists and metallist theorists who were right about the constraint elimination but lacked political power to prevent it; international creditors in the global South who lost hard-currency discipline without gaining reserve-currency seigniorage; future generations bearing inflation externalities of current monetary expansion.
% DISAPPEARANCE_RATIONALE: If this transition were reversed—if gold-standard discipline were restored tomorrow—central banks would lose the primary tool of counter-cyclical policy, credit-expansion would collapse, fixed-income asset values would spike, debtors (especially governments) would face immediate default risk, and the entire post-1971 financial system (built on the assumption of fiat discretion) would be forced to restructure. The constraint's elimination is foundational to contemporary monetary operations.
% FOUNDING_PROBLEM: The gold standard created deflationary rigidity during depressions: money supply was mechanically constrained by gold reserves, so central banks could not expand credit to offset falling demand. The Great Depression demonstrated that this automatic constraint prevented rather than enabled monetary policy responses. The founding problem was: How to enable central banks to inject liquidity during crises without waiting for gold flows?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by monetary historians (Keynes, Temin, Eichengreen) outside the benefiting parties, and by the 2008 financial crisis itself—where central banks deployed quantitative easing, a tool wholly unavailable under gold-standard constraints. The problem remains live because the tension between discretion (enabling stabilization) and discipline (limiting debasement) is unresolved.
narrative_ontology:disappearance_verdict(gold_fiat_transition_mechanism__automatic_constraint_reading, world_rearranges).
narrative_ontology:founding_problem_status(gold_fiat_transition_mechanism__automatic_constraint_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gold_fiat_transition_mechanism__automatic_constraint_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gold_fiat_transition_mechanism__automatic_constraint_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gold_fiat_transition_mechanism__automatic_constraint_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.45→0.72) because the constraint eliminated an automatic, decentralized protective mechanism (gold redemption) and replaced it with discretionary authority that overwhelmingly benefits issuers over creditors. The transition was not immediate in its effects—extractiveness stayed moderate in the 1950s-60s (when Bretton Woods still shadowed the system) and rose sharply in the 1970s (stagflation, abandonment of price stability norms) before stabilizing in the 1980s-90s (when credibility mechanisms and inflation targeting replaced gold as the disciplinary narrative). Suppression is moderate-high (0.58) because the constraint's persistence requires active enforcement: central banks must prevent alternative currencies, enforce legal tender status, and suppress gold-redemption claims. Without ongoing legal enforcement, creditors would organize around gold or commodity baskets. Theater is modest but rising (0.12→0.41) because central banks increasingly justify discretion through technical expertise (inflation targeting, forward guidance) rather than mechanical necessity—the 'independence' narrative performs authority in place of the gold standard's self-evident constraint.
 *
 * PERSPECTIVAL GAP:
 *   The monetary authority seat computes a rope or scaffold reading: the transition solved a real coordination failure (the deflationary trap) and continues to solve it through discretionary policy. The creditor seat computes a snare reading: the constraint changed from protective to extractive, replacing mechanical discipline with administrative discretion. The labor seat splits—workers with cost-of-living adjustment benefit; fixed-wage workers suffer from inflation erosion. The reserve-currency-issuer seat computes a tangled rope with seigniorage siphoning: genuine coordination in some dimensions (stable medium of exchange, counter-cyclical policy), pure extraction in others (inflation tax, competitive devaluation). The engine computes these divergences from the structural data; the authored claim does not adjudicate.
 *
 * DIRECTIONALITY LOGIC:
 *   Monetary authorities moved from constrained (d near 0.2 under gold standard, passive mechanism) to agenda-setting (d near 1.0 post-transition, active discretion). Creditors moved from protected beneficiaries (d near 0.1, gold redemption as automatic hedge) to targets (d near 0.8, erosion risk). Domestic debtors moved from victims (d near 0.7, deflation burden) to beneficiaries (d near 0.2, inflation-driven debt reduction). The transition's structural signature is the reversal of directionality for capital-holding seats: creditors went from beneficiary to payer, debtors from payer to beneficiary. This is not a measurement error—it is the core claim of the automatic_constraint_reading: the constraint's type changed, so the seats' positions inverted.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (deflationary rigidity preventing monetary stabilization) is live, not dead. Central banks invoked it during the 2008 crisis when they deployed quantitative easing, demonstrating that discretion remained functionally necessary. However, the measurement series shows theater rising while extractiveness plateaus: central banks increasingly justify operations through technical expertise (inflation models, transmission channels) rather than necessity, and they face growing pressure to manage distributional effects (who bears inflation, who captures seigniorage gains). This is not mandatrophy—the constraint's purpose hasn't atrophied—but drift toward theater: the original emergency (Great Depression, stagflation) recedes, and the constraint persists more as an institutional power arrangement than a functional necessity. The constraint is not piton-grade (beneficiaries still defend it actively), but theater-ratio growth signals erosion of the founding problem's perceived urgency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automatic_constraint_elimination_scope,
    'Was the gold standard constraint truly automatic and mechanical, or did it depend on continuous government choices to enforce gold parity and prevent monetary expansion?',
    'Historical analysis of failed gold standard enforcements (China 1930s, France pre-1926): if parity collapsed due to political choice rather than mechanical exhaustion, the constraint was never fully automatic. Comparative analysis of how different governments maintained or abandoned the peg despite economic pressure.',
    'If the constraint was already discretionary-in-structure but disguised-as-mechanical, the transition was not from material to institutional but from concealed discretion to explicit discretion. This would lower the reading''s ε (extractiveness would be attributed less to constraint elimination, more to unmasking pre-existing choice). If the constraint was genuinely mechanical, ε stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automatic_constraint_elimination_scope, empirical, 'Whether the gold standard''s constraint was truly automatic or disguised discretion').

omega_variable(
    beneficiary_persistence_mechanism,
    'Why did monetary authorities choose to maintain fiat-currency discipline (low inflation, central bank independence) for decades after the gold standard was eliminated, if the transition was purely extractive?',
    'Genealogy of central bank independence and inflation targeting: trace whether these were internally chosen credibility mechanisms or externally imposed (via creditor pressure, international conditionality, labor bargaining). If imposed, they are suppression artifacts; if chosen, they suggest genuine coordination benefits.',
    'If discipline was creditor-imposed, extractiveness was genuinely reversed (creditors retained veto despite losing gold). If discipline was chosen, extractiveness was lower than authored (the constraint coordinated monetary stability, not pure extraction). This cuts into whether the beneficiary/victim framing is complete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_persistence_mechanism, empirical, 'Whether fiat discipline was extracted or chosen, and what that implies for the constraint''s extractiveness').

omega_variable(
    kernel_reading_framing_choice,
    'Is this the automatic_constraint_reading because the mechanical constraint swap is genuinely the primary causal driver of the transition''s distributional effects, or because this framing narrates the constraint elimination as a benefit to those who study monetary policy?',
    'Comparative analysis of constraint families: if the creditor_discipline_reading and composite_overdetermination_reading produce different ε values and victim/beneficiary sets on the same historical data, the choice of reading has nontrivial consequences. Test whether the automatic_constraint_reading''s beneficiary set (monetary_authorities) would have gained discretion absent the specific causal mechanism this reading privileges.',
    'High impact: if reading choice is functionally arbitrary among defensible framings, ε is partially a narrative choice rather than a structural fact. This cuts at the ε-invariance principle (OQ-26). This omega documents that risk and invites post-generation testing of reading independence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_framing_choice, conceptual, 'Whether reading choice (automatic_constraint vs. sibling readings) is causally grounded or narratively contingent').

omega_variable(
    suppression_vs_internalization_ambiguity,
    'Is the suppression measured in this constraint (0.58) structural (external enforcement of legal tender, suppression of gold redemption, exclusion of rival currencies) or internalized (savers and creditors accept fiat despite erosion risk, having internalized the constraint''s inevitability)?',
    'Post-exit trajectory: if creditors developed exit mechanisms (commodities, crypto, offshore hard-currency hoarding) and the suppression required increased external force, suppression is primarily structural. If creditors continue to hold fiat despite availability of alternatives, suppression has become internalized.',
    'If internalized, effective suppression is higher than the structural measure suggests—the constraint carries with it a belief system that persists even after external barriers weaken. This would support Snare classification from some seats. If structural, the constraint remains Tangled Rope: coordination with enforced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_vs_internalization_ambiguity, empirical, 'Whether suppression of alternative currencies is structural or internalized in creditor behavior').

omega_variable(
    seigniorage_distribution_empirical_claim,
    'Who actually captured the seigniorage gains from fiat money creation? Was it monetary authorities (the authored beneficiary) or did benefits diffuse to domestic debtors, exporters, and savers differently than the constraint story assumes?',
    'Distribute net present value of seigniorage across seat-categories (domestic government, central bank operating surplus, real wage changes, asset price inflation, export competitiveness gains). Trace whose net wealth actually increased.',
    'If seigniorage was diffusely captured or unevenly distributed, the beneficiary set is incomplete or misdeclared. If gains concentrated where authored (monetary authorities), the constraint''s beneficiary structure is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(seigniorage_distribution_empirical_claim, empirical, 'Empirical distribution of seigniorage gains across seats versus the authored beneficiary/victim structure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gold_fiat_transition_mechanism__automatic_constraint_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gold_tr_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(gold_tr_t0, observed).
narrative_ontology:measurement(gold_tr_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(gold_tr_t5, observed).
narrative_ontology:measurement(gold_tr_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(gold_tr_t10, observed).
narrative_ontology:measurement(gold_tr_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement_basis(gold_tr_t15, observed).
narrative_ontology:measurement(gold_tr_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 25, 0.39).
narrative_ontology:measurement_basis(gold_tr_t25, observed).
narrative_ontology:measurement(gold_tr_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement_basis(gold_tr_t35, observed).
narrative_ontology:measurement(gold_tr_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(gold_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gold_be_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gold_be_t0, observed).
narrative_ontology:measurement(gold_be_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(gold_be_t5, observed).
narrative_ontology:measurement(gold_be_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(gold_be_t10, observed).
narrative_ontology:measurement(gold_be_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement_basis(gold_be_t15, observed).
narrative_ontology:measurement(gold_be_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement_basis(gold_be_t25, observed).
narrative_ontology:measurement(gold_be_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 35, 0.72).
narrative_ontology:measurement_basis(gold_be_t35, observed).
narrative_ontology:measurement(gold_be_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(gold_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gold_su_t0, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(gold_su_t0, observed).
narrative_ontology:measurement(gold_su_t5, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 5, 0.44).
narrative_ontology:measurement_basis(gold_su_t5, observed).
narrative_ontology:measurement(gold_su_t10, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement_basis(gold_su_t10, observed).
narrative_ontology:measurement(gold_su_t15, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(gold_su_t15, observed).
narrative_ontology:measurement(gold_su_t25, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 25, 0.58).
narrative_ontology:measurement_basis(gold_su_t25, observed).
narrative_ontology:measurement(gold_su_t35, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 35, 0.58).
narrative_ontology:measurement_basis(gold_su_t35, observed).
narrative_ontology:measurement(gold_su_t50, gold_fiat_transition_mechanism__automatic_constraint_reading, suppression_requirement, 50, 0.58).
narrative_ontology:measurement_basis(gold_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gold_fiat_transition_mechanism__automatic_constraint_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(gold_fiat_transition_mechanism__automatic_constraint_reading, 0.18).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__creditor_discipline_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, gold_fiat_transition_mechanism__composite_overdetermination_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, bretton_woods_peg_system__reserve_currency_reading).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, central_bank_inflation_targeting_constraint).
narrative_ontology:affects_constraint(gold_fiat_transition_mechanism__automatic_constraint_reading, legal_tender_enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading (automatic_constraint_reading) of the contested kernel gold_fiat_transition_mechanism. The kernel has three sibling readings: (1) creditor_discipline_reading frames the transition as creditor-veto elimination enabling debtor-nation flexibility; (2) composite_overdetermination_reading denies a single causal swap, attributing the transition to independent convergence of telecommunications, labor bargaining, and legal tender maturation. All three readings share the same historical referent (1944-1971 Bretton Woods era, 1971-present post-gold period) but instantiate different constraints with different ε, beneficiary/victim structures, and causal chains. The automatic_constraint_reading privileges the mechanical constraint swap as primary; the siblings distribute causality differently. The constraint family is linked via network.affects_constraints; each story is self-contained and ε-invariant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gold_fiat_transition_mechanism__automatic_constraint_reading, powerful, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
