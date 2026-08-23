% ============================================================================
% CONSTRAINT STORY: dollar_gold_convertibility__policy_flexible_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dollar_gold_convertibility__policy_flexible_reading, []).

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
 *   constraint_id: dollar_gold_convertibility__policy_flexible_reading
 *   human_readable: Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Stabilization (Policy-Flexible Reading)
 *   domain: economic/international-monetary
 *
 * SUMMARY:
 *   Under this reading, the Bretton Woods redemption undertaking is real but
 *   conditional: the United States honors dollar-for-gold conversion at $35
 *   per ounce for official holders except where domestic employment, price
 *   stability, or war finance requires otherwise. In practice the condition
 *   swallowed the rule. As US deficits widened through the 1960s, Washington
 *   defended the parity with capital controls, allied pressure, and pooled
 *   gold interventions while continuing to issue dollars, and in August 1971
 *   it suspended redemption outright, bundling the suspension with a domestic
 *   wage-price freeze and an import surcharge. The burden of the
 *   arrangement's endgame fell on the official and private holders of the
 *   accumulating dollar claims, whose redemption right proved contingent on
 *   the issuer's convenience. Family note (epsilon decomposition, not
 *   averaging): the same Articles text decomposes into three epsilon-distinct
 *   stories — this policy-flexible reading (burden shifts to external
 *   creditors as devaluation risk; the issuer regains autonomy), the
 *   strict_convertibility_reading (the pledge binds the issuer, whose
 *   autonomy bears the cost), and the triffin_structural_reading (the design
 *   itself is the defect, with diffuse systemic cost). This file authors
 *   epsilon only for the conditional-obligation arrangement as this reading
 *   assesses it.
 *
 * KEY AGENTS:
 *   - us_federal_government: agenda-setting sovereign issuer (institutional/arbitrage) — administers the gold window, decides case by case when home-country stabilization overrides the parity, ultimately suspends redemption, and collects the deficit-financing gains throughout
 *   - foreign_central_banks: principal official creditor seat (organized/trapped) — hold working dollar reserves with a nominal right to redeem at $35 per ounce; redeeming in size would destroy the parity and write down the very reserves they hold
 *   - surplus_economies_germany_japan: powerful creditor governments (powerful/constrained) — pressed to revalue or absorb inflation rather than demand gold; Germany revalues twice at exporter cost, Japan accumulates rather than revalue
 *   - foreign_exporters_accepting_dollars: commercial creditor seat (moderate/constrained) — invoice and carry dollar receivables and absorb the purchasing-power loss when the parity moves or the window closes
 *   - us_importing_sectors: domestic beneficiary seat (moderate/mobile) — buy against an undervalued dollar, gain cheaper inputs and goods, pay nothing into the redemption machinery
 *   - developing_imf_members: small member states (moderate/constrained) — import stability and liquidity from the arrangement while holding no seat where parity and suspension decisions are made
 *   - triffin_school_economists: analytical observers (analytical/analytical) — publish the arithmetic connecting reserve growth to redemption erosion from outside official councils; consulted and set aside in turn
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, 0.68).
domain_priors:suppression_score(dollar_gold_convertibility__policy_flexible_reading, 0.64).
domain_priors:theater_ratio(dollar_gold_convertibility__policy_flexible_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dollar_gold_convertibility__policy_flexible_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dollar_gold_convertibility__policy_flexible_reading, tangled_rope).
narrative_ontology:human_readable(dollar_gold_convertibility__policy_flexible_reading, "Dollar-Gold Convertibility as Conditional Obligation Subordinate to Domestic Stabilization (Policy-Flexible Reading)").
narrative_ontology:topic_domain(dollar_gold_convertibility__policy_flexible_reading, "economic/international-monetary").

domain_priors:requires_active_enforcement(dollar_gold_convertibility__policy_flexible_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dollar_gold_convertibility__policy_flexible_reading, 'e6445393-5c50-4fe5-9d77-54e93a0728cb').
narrative_ontology:cs_kernel_codification('e6445393-5c50-4fe5-9d77-54e93a0728cb', formalized).
narrative_ontology:cs_authority_grounding('e6445393-5c50-4fe5-9d77-54e93a0728cb', practice).
narrative_ontology:cs_interpretation_layer_present('e6445393-5c50-4fe5-9d77-54e93a0728cb').
narrative_ontology:cs_reading_relation('e6445393-5c50-4fe5-9d77-54e93a0728cb', dollar_gold_convertibility__strict_convertibility_reading, forecloses).
narrative_ontology:cs_reading_relation('e6445393-5c50-4fe5-9d77-54e93a0728cb', dollar_gold_convertibility__triffin_structural_reading, coexists_with).
narrative_ontology:cs_axiom('e6445393-5c50-4fe5-9d77-54e93a0728cb', foundational, domestic_stability_primacy).
narrative_ontology:cs_axiom_status(domestic_stability_primacy, holdable).
narrative_ontology:cs_axiom_grounding('e6445393-5c50-4fe5-9d77-54e93a0728cb', domestic_stability_primacy, conventional).
narrative_ontology:cs_axiom('e6445393-5c50-4fe5-9d77-54e93a0728cb', secondary, creditor_risk_absorption_norm).
narrative_ontology:cs_axiom_status(creditor_risk_absorption_norm, holdable).
narrative_ontology:cs_axiom_grounding('e6445393-5c50-4fe5-9d77-54e93a0728cb', creditor_risk_absorption_norm, instrumental).
narrative_ontology:cs_reference_frame('e6445393-5c50-4fe5-9d77-54e93a0728cb', consultative_conditional_parity).
narrative_ontology:cs_drift_state('e6445393-5c50-4fe5-9d77-54e93a0728cb', august_1971_suspension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e6445393-5c50-4fe5-9d77-54e93a0728cb', '').
narrative_ontology:cs_kernel_id(dollar_gold_convertibility__policy_flexible_reading, dollar_gold_convertibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_federal_government).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, us_importing_sectors).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, foreign_exporters_accepting_dollars).
narrative_ontology:constraint_victim(dollar_gold_convertibility__policy_flexible_reading, surplus_economies_germany_japan).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dollar_gold_convertibility__policy_flexible_reading, developing_imf_members).
narrative_ontology:constraint_vindicates(dollar_gold_convertibility__policy_flexible_reading, sovereign_monetary_discretion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the reserve currency and operates the gold window from the Treasury. Declares and defends the $35 parity while running budget deficits for war and social programs, and decides case by case whether employment and price goals at home take precedence over honoring redemption requests. Deploys capital controls, suasion over allied central banks, and finally a wage-price freeze and import surcharge alongside suspending redemption in August 1971. Finances its deficits in its own currency for the entire span, and its electoral calendar is what the domestic-priority condition protects.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_federal_government, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, us_federal_government, beneficiary).

% Hold the bulk of their reserves as dollar balances built up through intervention and trade settlement. They possess a formal right to redeem dollars for gold at $35 per ounce, but redeeming in size would exhaust US gold stock, break the parity, and write down the very reserves they hold while rupturing the export markets their economies depend on. They intervene to defend a parity they privately doubt, staff the London Gold Pool, and absorb successive Washington assurances until the window closes and their balances reprice.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_central_banks, payer,
    organized, generational, trapped, global).

% Run persistent trade surpluses and accumulate dollars faster than they wish. They face Washington's pressure to revalue their currencies or expand domestic demand rather than convert dollars to gold. Germany revalues twice and absorbs the exporter backlash at home; Japan resists revaluation and keeps accumulating. Their realistic choices run between unwanted paper holdings and a self-imposed loss of competitiveness.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, surplus_economies_germany_japan, payer,
    powerful, generational, constrained, national).

% Invoice exports in dollars and carry dollar receivables between settlements. When the parity moves or the window shuts, the purchasing power of those receivables falls; re-invoicing wholesale is not realistic because buyers and trade finance remain dollar-centered, so they absorb the repricing as a cost of doing business.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, foreign_exporters_accepting_dollars, payer,
    moderate, biographical, constrained, global).

% Buy foreign manufactures and materials priced against an undervalued dollar and sell into a home market shielded from deflationary adjustment. They obtain cheaper inputs and consumer goods across the whole span of the arrangement and contribute nothing to the redemption machinery that underwrites their sourcing.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, us_importing_sectors, beneficiary,
    moderate, biographical, mobile, national).

% Import exchange-rate stability and dollar liquidity through membership and borrow against the order the major economies maintain. They hold no seat where parity and suspension decisions are made, learn outcomes after the fact, and receive both the stability and the inflation the issuing country exports.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, developing_imf_members, beneficiary,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(dollar_gold_convertibility__policy_flexible_reading, developing_imf_members, excluded).

% Work outside official councils, publishing the arithmetic that shows reserve growth requires the issuer to run deficits which in turn erode confidence in redemption. They testify before Congress and international study groups, supply the vocabulary officials use to describe the strains, and are consulted and set aside in turn.
narrative_ontology:constraint_stakeholder(dollar_gold_convertibility__policy_flexible_reading, triffin_school_economists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dollar_gold_convertibility__policy_flexible_reading, us_federal_government).
narrative_ontology:fixing_cost_class(dollar_gold_convertibility__policy_flexible_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplied the non-communist world with a single anchor currency and settlement asset: parities pegged to a dollar redeemable in gold gave traders and governments predictable exchange rates and an internationally acceptable reserve, letting world trade grow without gold scarcity rationing it.
% TRANSFER_FUNCTION: Moved real goods, services, and assets from surplus economies to the United States in exchange for dollar claims whose redemption value depended entirely on the issuer's continued willingness and ability to convert; when conversion ended, the accumulated claims repriced downward and the loss landed on the holders.
% ABSENT_VOICES: Private dollar holders, small IMF members, and commodity exporters had no seat at the G10 tables where parity defense was arranged; creditor-country legislatures learned of commitments after they were made; the 1971 suspension was announced without prior consultation with the very institutions the Articles named for that purpose.
% DISAPPEARANCE_RATIONALE: Overnight disappearance unhinges every parity pegged to the dollar decades ahead of schedule, strips trade finance of its settlement asset, forces each surplus country into immediate bilateral adjustment, and removes the channel that automatically financed US deficits — the postwar trading order reorganizes around whichever reserve arrangements the creditor bloc can improvise.
% FOUNDING_PROBLEM: Between 1914 and 1944 the classical gold standard collapsed and its interwar reconstruction failed amid competitive devaluation, protection, and depression; the founders sought exchange-rate stability plus adequate international liquidity without the old standard's deflationary discipline.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the US beneficiary set by European central bank archives (Bundesbank and Banque de France records), the Fund's own commissioned histories, and monetary scholars who accepted the founding diagnosis while rejecting the solution's durability. By interval end several creditor central banks went further and attested in writing that the founding problem no longer justified the arrangement's asymmetries — corroboration both of the problem's original reality and of its fading justificatory force.
narrative_ontology:disappearance_verdict(dollar_gold_convertibility__policy_flexible_reading, world_rearranges).
narrative_ontology:founding_problem_status(dollar_gold_convertibility__policy_flexible_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dollar_gold_convertibility__policy_flexible_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dollar_gold_convertibility__policy_flexible_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dollar_gold_convertibility__policy_flexible_reading, 0.68, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dollar_gold_convertibility__policy_flexible_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dollar_gold_convertibility__policy_flexible_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.68: by interval end the arrangement transfers real resources from surplus economies to the issuer and terminates with the accumulated claims repriced downward on their holders — the reading's own lights place the burden squarely on external creditors. Suppression (raw structural property, unscaled by power or scope) is 0.64: persistence depended on actively discouraging redemption — Gold Pool operations, the Interest Equalization Tax, capital controls, pressure on allied central banks, and finally the coercive package accompanying suspension — not on voluntary assent. Theater rises from 0.12 to 0.52 as defense of the parity becomes increasingly performative: communiques reaffirming a parity officials privately expect to abandon, interventions managing optics more than flows (Goodhart drift preceding termination). Accessibility_collapse 0.48: exits existed in principle (revaluation, invoicing shifts, SDR substitution) but each imposed costs on the exiting creditor, so alternatives persisted only partially. Resistance 0.55: the French conversion policy, German revaluations, congressional testimony, and the eventual refusal of the creditor bloc constitute real, sustained opposition. The three series share one grid (points 0, 2, 5, 8, 11, 13 on a 1958-1971 annual scale) with every metric authored at every point. Dynamics are a monotonic ratchet, not a cycle: each dollar crisis left enforcement machinery heavier and the parity weaker, so no oscillation phase needs dating.
 *
 * PERSPECTIVAL GAP:
 *   From the issuer's seat the arrangement reads as legitimate sovereign flexibility — a parity honored in good faith except when domestic stabilization forbids, which is what a democracy owes its own voters first. From the trapped official creditor seats the identical structure reads as unilateral risk placement: a redemption right honored exactly until honoring it costs the redeemer nothing and suspending it costs the holder everything. Same-power creditor governments diverge laterally: Germany purchased partial exit twice through revaluation and accepted exporter retaliation; Japan, facing identical pressure, held the line and kept accumulating — the difference is constraint-specific (export dependence and security ties), not global standing. Analytical seats split by reading allegiance rather than interest. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The issuer seat combines declared beneficiary position with arbitrage-grade exit (it can redefine or terminate the undertaking at others' expense), driving its derived directionality toward the beneficiary pole and damping or inverting effective extraction on it. The official creditor seat declares borne-cost position with trapped exit — redemption in size is self-destructive — placing it near the full-target pole with amplified effective extraction. Surplus-economy governments and commercial exporters declare borne-cost position with constrained exit: high directionality, moderately amplified. The importing-sector seat is a declared beneficiary with mobile exit, near the subsidy pole. Developing members sit mildly beneficiary-side but voiceless. Global spatial scope scales effective extraction upward modestly for the extracted seats. No directionality overrides are authored: the beneficiary/borne-cost declarations plus exit atoms already separate every seat the derivation needs to distinguish.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — interwar competitive devaluation and the collapse of internationally acceptable money — was real and is corroborated from outside the beneficiary set. By 1969-1971 its justificatory force had faded for the creditor parties, hence founding_problem_status contested rather than dead: the parties dispute whether the anchor still earned its asymmetries. Classification discipline cuts both ways here. Naming the genuine coordination core (predictable parities, settlement liquidity, reconstruction finance) blocks a pure-extraction mislabel; naming the asymmetric conditionality and its enforced persistence blocks a pure-coordination mislabel. The arrangement did not decay into inertial performance — the issuer terminated it while still collecting — so no piton signature applies: gains remained concentrated on the named capturer, and the cost-to-fix for the only actor who could fix it (full redemption would have demanded domestic deflation) was prohibitive. The R5 mismatch consumer reads status=contested against verdict=world_rearranges and fires no zombie flag; the rising theater series documents the performative drift that preceded termination instead of a post-functional afterlife.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bindingness_of_article_iv,
    'Does Article IV''s redemption undertaking bind the issuing government as enforceable legal obligation (strict_convertibility_reading), or operate as a conditional undertaking exercisable at the issuer''s domestic discretion (this reading)?',
    'Drafting history of the 1944 Articles, the legal opinions exchanged among G10 treasuries between 1960 and 1971, and the retrospective treatment of the obligation in the later amendment that legitimated floating.',
    'If binding, this story''s borne-cost set collapses and the strict reading''s story carries the classification; if conditional, official dollar holders remain the borne-cost seat as authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bindingness_of_article_iv, conceptual, 'Locates the kernel''s core disagreement: bindingness versus conditionality of the parity pledge.').

omega_variable(
    inherent_vs_manageable_design,
    'Is the conditional arrangement sustainably manageable by issuer discretion, or does the arithmetic that reserve growth requires issuer deficits which in turn erode redemption confidence make breakdown inevitable regardless of management?',
    'Counterfactual policy analysis of slower reserve growth, SDR substitution, or earlier parity adjustment; archival records of the Gold Pool and Working Group 3 deliberations.',
    'If manageable, this reading''s persistence reflects viable hybrid design; if inevitable, the measured burden is a transit phase of a structural flaw and the triffin_structural_reading sibling absorbs the explanation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_manageable_design, conceptual, 'Manageability contest with the triffin_structural_reading sibling.').

omega_variable(
    creditor_coalition_exit_feasibility,
    'Could the major creditor central banks have credibly coordinated mass redemption to force issuer discipline, or was joint exit infeasible given each holder''s individual exposure to parity collapse?',
    'Archival reconstruction of the 1965-1969 Franco-German consultations and Gold Pool dissolution bargaining; analysis of reserve-composition thresholds at which joint action becomes individually rational.',
    'Feasible coalition exit lowers the trapped character of the creditor seats and reduces effective extraction; infeasibility confirms the trap and supports the upper end of the extraction series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creditor_coalition_exit_feasibility, empirical, 'Whether the borne-cost seats held usable coalition leverage despite individual exposure.').

omega_variable(
    good_faith_conditionality,
    'Did exercises of the domestic-priority condition stay within the consultative procedure the Articles prescribe (prior Fund consultation, negotiated parity change), or become unilateral opportunism by 1971?',
    'Sequencing of the August 1971 decisions against the Articles'' consultation requirements; contemporaneous Treasury and Fund correspondence on notice and negotiation.',
    'Consultative exercise supports the hybrid reading with genuine coordination residue intact; unilateral exercise marks the operation as opportunistic at interval end and raises effective suppression on the creditor seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(good_faith_conditionality, empirical, 'Whether conditionality was exercised procedurally or opportunistically at the margin.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dollar_gold_convertibility__policy_flexible_reading, 0, 13).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doll_tr_t0, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(doll_tr_t0, observed).
narrative_ontology:measurement(doll_tr_t2, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 2, 0.15).
narrative_ontology:measurement_basis(doll_tr_t2, observed).
narrative_ontology:measurement(doll_tr_t5, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(doll_tr_t5, observed).
narrative_ontology:measurement(doll_tr_t8, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(doll_tr_t8, observed).
narrative_ontology:measurement(doll_tr_t11, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 11, 0.44).
narrative_ontology:measurement_basis(doll_tr_t11, observed).
narrative_ontology:measurement(doll_tr_t13, dollar_gold_convertibility__policy_flexible_reading, theater_ratio, 13, 0.52).
narrative_ontology:measurement_basis(doll_tr_t13, observed).

% Extraction over time
narrative_ontology:measurement(doll_be_t0, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(doll_be_t0, observed).
narrative_ontology:measurement(doll_be_t2, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 2, 0.36).
narrative_ontology:measurement_basis(doll_be_t2, observed).
narrative_ontology:measurement(doll_be_t5, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(doll_be_t5, observed).
narrative_ontology:measurement(doll_be_t8, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(doll_be_t8, observed).
narrative_ontology:measurement(doll_be_t11, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 11, 0.62).
narrative_ontology:measurement_basis(doll_be_t11, observed).
narrative_ontology:measurement(doll_be_t13, dollar_gold_convertibility__policy_flexible_reading, base_extractiveness, 13, 0.68).
narrative_ontology:measurement_basis(doll_be_t13, observed).

% Suppression requirement over time
narrative_ontology:measurement(doll_su_t0, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement_basis(doll_su_t0, observed).
narrative_ontology:measurement(doll_su_t2, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 2, 0.27).
narrative_ontology:measurement_basis(doll_su_t2, observed).
narrative_ontology:measurement(doll_su_t5, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement_basis(doll_su_t5, observed).
narrative_ontology:measurement(doll_su_t8, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement_basis(doll_su_t8, observed).
narrative_ontology:measurement(doll_su_t11, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 11, 0.59).
narrative_ontology:measurement_basis(doll_su_t11, observed).
narrative_ontology:measurement(doll_su_t13, dollar_gold_convertibility__policy_flexible_reading, suppression_requirement, 13, 0.64).
narrative_ontology:measurement_basis(doll_su_t13, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dollar_gold_convertibility__policy_flexible_reading, resource_allocation).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, strict_convertibility_reading).
narrative_ontology:affects_constraint(dollar_gold_convertibility__policy_flexible_reading, triffin_structural_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'dollar-gold convertibility' covers three structurally distinct claims sharing one codified text. The strict reading (upstream, legalist) supplies the legitimacy conditions this flexible reading relaxes; the flexible reading's operational record is the primary evidentiary input the triffin reading (downstream) cites for inherent unsustainability. Epsilon differs across the family by construction — issuer-bound (strict), creditor-bound (this file), system-bound (triffin) — so each story carries its own stable epsilon and the files link via affects_constraints rather than sharing one measurement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
