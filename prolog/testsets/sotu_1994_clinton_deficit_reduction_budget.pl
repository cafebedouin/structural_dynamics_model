% ============================================================================
% CONSTRAINT STORY: sotu_1994_clinton_deficit_reduction_budget
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1994_clinton_deficit_reduction_budget, []).

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
 *   constraint_id: sotu_1994_clinton_deficit_reduction_budget
 *   human_readable: Deficit Reduction Through Progressive Taxation and Spending Cuts (1993-1994)
 *   domain: economics/fiscal_policy
 *
 * SUMMARY:
 *   The Omnibus Budget Reconciliation Act of 1993 (Clinton administration)
 *   enacted a structural fiscal constraint that combined progressive income
 *   taxation (targeting the top 1.2% of earners) with $255 billion in
 *   spending cuts distributed across 340+ budget items. The constraint serves
 *   a dual function: redistributing deficit reduction burden away from middle
 *   and working-class households (who are protected from direct tax
 *   increases) onto high-income earners and corporations, while
 *   simultaneously establishing fiscal discipline as the foundation for
 *   economic growth. This architecture makes the constraint an exemplar of
 *   Tangled Rope structure — genuine coordination function (deficit reduction
 *   enabling macroeconomic stability) layered over asymmetric extraction
 *   (concentrated burden on top earners). The constraint's classification
 *   varies sharply across perspectives: top earners experience it as a snare
 *   (constrained exit, pure extraction); middle-class households experience
 *   it as rope (protected, benefiting from stability); organized fiscal
 *   advocates see a scaffold with an implicit sunset (temporary sacrifice for
 *   long-term health); and the analytical observer risks naturalizing a
 *   contingent institutional choice as economic necessity. The theater
 *   ratio's increase over time reflects the degradation of the constraint as
 *   fiscal discipline narrative persists despite re-emergence of deficits in
 *   subsequent decades, indicating Piton characteristics developing.
 *
 * KEY AGENTS:
 *   - Top 1.2% earners (powerful/constrained): Primary victims of tax increases; face material exit costs but some structural mobility
 *   - Middle and working-class households (moderate/mobile): Primary beneficiaries; protected from direct taxation; benefit from deficit reduction coordination
 *   - Federal fiscal authority (institutional/arbitrage): Primary enforcer; coordinates deficit reduction while capturing increased revenues
 *   - Future fiscal sustainability advocates (organized/constrained): Secondary beneficiaries; organized around long-term fiscal health; see sunset logic
 *   - Capital and professional class (powerful/mobile at generational horizon): Secondary victims; face global exit options; generational perspective produces different classification
 *   - Congressional Budget Office and economic advisors (analytical/analytical): Observers measuring constraint effectiveness; at risk of naturalizing contingent choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1994_clinton_deficit_reduction_budget, 0.52).
domain_priors:suppression_score(sotu_1994_clinton_deficit_reduction_budget, 0.48).
domain_priors:theater_ratio(sotu_1994_clinton_deficit_reduction_budget, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1994_clinton_deficit_reduction_budget, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1994_clinton_deficit_reduction_budget, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1994_clinton_deficit_reduction_budget, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1994_clinton_deficit_reduction_budget, tangled_rope).
narrative_ontology:human_readable(sotu_1994_clinton_deficit_reduction_budget, "Deficit Reduction Through Progressive Taxation and Spending Cuts (1993-1994)").
narrative_ontology:topic_domain(sotu_1994_clinton_deficit_reduction_budget, "economics/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1994_clinton_deficit_reduction_budget).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1994_clinton_deficit_reduction_budget, middle_and_working_class_households).
narrative_ontology:constraint_beneficiary(sotu_1994_clinton_deficit_reduction_budget, fiscal_stability_constituency).
narrative_ontology:constraint_victim(sotu_1994_clinton_deficit_reduction_budget, top_income_earners).
narrative_ontology:constraint_victim(sotu_1994_clinton_deficit_reduction_budget, high_earning_professionals).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TOP EARNERS (SNARE) — High-income earners face binding tax increases with limited exit options within the U.S. fiscal domain. While theoretically mobile (could relocate, restructure income), the cost barriers are substantial — tax residence change, business relocation, income reorganization all carry execution costs. The constraint appears as pure extraction: tax rates rise, benefits accrue to fiscal stability and deficit reduction (not to the payers), and suppression operates through institutional authority (IRS enforcement, withholding mechanisms). No coordination benefit flows to the targeted group itself; the extraction is asymmetric.
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE/WORKING-CLASS HOUSEHOLDS (ROPE) — Protected from immediate tax increases; experience the constraint primarily as coordination infrastructure. The deficit reduction narrative requires widespread fiscal discipline — spending cuts across 340+ budget items enforce constraints on government spending growth, which stabilizes the macroeconomic environment benefiting employment and wage stability. The households benefit from both the protection (progressive structure) and the coordination function (deficit reduction enables lower long-term interest rates, economic growth). Exit options are mobile — they can adjust consumption, savings, and labor supply in response to fiscal conditions — but the constraint itself provides coordination value.
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL AUTHORITY (TANGLED ROPE) — The federal government simultaneously coordinates macroeconomic stability (deficit reduction as foundation for growth) and extracts resources through progressive taxation. The government benefits from increased revenues and restored credibility with bond markets; it also coordinates the broader economic environment by establishing fiscal discipline. Active enforcement is required: IRS administration, withholding mechanisms, audit and compliance infrastructure. The constraint exhibits both genuine coordination function (deficit reduction enabling lower rates, growth) and asymmetric extraction (concentrating burden on top earners). This is the canonical tangled rope structure: real coordination overlay on extraction.
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE FISCAL SUSTAINABILITY ADVOCATES (SCAFFOLD) — Organized agents (deficit hawks, budget reformers, long-term fiscal planners) see this constraint as temporary coordination infrastructure with an implicit sunset. The deficit reduction mechanism is designed to be phased out as the deficit declines: if successful, the tax increases should become unnecessary once the deficit is eliminated or substantially reduced. The suppression is tolerated because the sunset is visible — the constraint is framed as 'temporary sacrifice for long-term health,' which enables acceptance of the burden. Low effective extraction because the constraint has an exit path: if deficits decline, tax rates could theoretically decline (though political will for tax cuts varies).
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE TAX MYTHOLOGY (PITON) — Over civilizational time, the constraint degrades as the political narrative ('temporary deficit reduction') becomes detached from fiscal reality. Tax increases remain in place while deficits re-emerge (as they did in the 2000s and 2010s); the constraint becomes a standing revenue mechanism maintained through institutional inertia rather than active deficit reduction. The progressive framing persists theatrically as a policy rationale even when the underlying fiscal discipline dissolves. Theater ratio is high because the constraint's primary function (deficit reduction coordination) is gradually replaced by pure revenue extraction, yet the narrative structure remains intact.
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GLOBAL CAPITAL / PROFESSIONALS (GENERATIONAL HORIZON) — At a generational time horizon with global scope, high earners face genuine exit options: international relocation, income shifting to lower-tax jurisdictions, professional migration to Canada or other OECD countries. The constraint exhibits hybrid structure — both extraction (the tax burden is asymmetric and substantial) and coordination (the deficit reduction environment affects everyone's long-term prosperity). Generational exit options are real (second-generation relocation, establishment of professional practice in alternative jurisdictions). This perspective produces tangled rope: the coordination function is global (stable U.S. fiscal policy benefits international trade and investment) while the extraction is national (U.S. residents bear the tax burden).
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (NATURAL LAW VIEW) — From a civilizational analytical perspective, deficit reduction through progressive taxation appears as an immutable structural necessity: any government running persistent deficits must eventually either reduce spending or increase revenues (or both). The 'choice' between these is constrained by economic reality — growth cannot indefinitely substitute for fiscal discipline, and borrowing has limits. This perspective risks naturalizing what is actually a contingent institutional design: the specific choice of progressive taxation and selective spending cuts is not inherent to deficit reduction itself. This is a false summit candidate — the structural data (beneficiaries, victims, enforcement requirements) contradicts the mountain classification.
constraint_indexing:constraint_classification(sotu_1994_clinton_deficit_reduction_budget, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1994_clinton_deficit_reduction_budget_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1994_clinton_deficit_reduction_budget, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1994_clinton_deficit_reduction_budget, TR),
    TR >= 0.70.

:- end_tests(sotu_1994_clinton_deficit_reduction_budget_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint concentrates substantial fiscal burden (approximately $63 billion in new revenues from top-earner taxation over five years, plus $255 billion in spending cuts) on a narrow beneficiary group (top 1.2%). The extraction is measurable, asymmetric, and enforced through institutional mechanisms. However, extraction is not maximal (0.66+) because: (1) spending cuts are distributed, not concentrated; (2) the rationale includes genuine macroeconomic benefits (lower deficits → lower interest rates → broader growth) that accrue to victims as well as beneficiaries; (3) escape options exist (income shifting, relocation, restructuring), reducing the suppression below snare-range levels. Suppression (0.48): Moderate. The constraint operates through binding institutional enforcement (IRS, tax withholding, compliance mechanisms) but is not total — high earners retain meaningful escape routes (international relocation, business restructuring, tax avoidance strategies). The binding is structural (cannot simply ignore federal tax law) but not absolute (can incur costs to evade or exit). Theater ratio (0.55): Moderate. The constraint begins with high functional content — genuine deficit reduction mechanism, real fiscal discipline enforcement, legitimate macroeconomic coordination. Over time (per measurements), theater increases as the narrative of 'temporary deficit reduction' becomes detached from fiscal reality (deficits re-emerge, but tax structure persists), indicating piton-like degradation. The constraint's theater is not purely performative at inception but becomes increasingly theatrical as function decays.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits striking perspectival variation. Top earners (Snare, powerful/constrained) perceive maximum extraction with minimal coordination benefit — the constraint appears as pure burden transfer. Middle-class households (Rope, moderate/mobile) perceive coordination benefit and protection — the constraint appears as infrastructure for shared prosperity. The fiscal authority (Tangled Rope, institutional/arbitrage) perceives both genuine coordination function (deficit reduction) and extraction revenue. Organized fiscal advocates (Scaffold, organized/constrained) perceive a temporary problem with sunset logic — they tolerate suppression because exit is visible. The piton perspective (institutional/arbitrage at civilizational scale) perceives degradation — the functional constraint becomes theatrical as deficits re-emerge. The analytical observer (mountain perspective) risks naturalizing what is actually a contingent institutional design: the 'necessity' of deficit reduction through progressive taxation is not a law of nature but a specific policy choice that alternative fiscal architectures could address differently. The false summit is precisely this risk — the constraint appears inevitable because deficit reduction is real, masking that the mechanism (who pays, how much) is contingent.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural relationship to the extraction flow and exit capacity. Top earners (powerful/constrained) derive d ≈ 0.70 (victims + constrained exit → high target designation) → f(d) ≈ 1.10, producing high experienced extraction chi despite only moderate base ε. Middle-class households (moderate/mobile) derive d ≈ 0.35 (beneficiaries + mobile exit → lower target designation) → f(d) ≈ 0.45, producing low or negative chi despite moderate base ε. The scope modifier σ(S=national) = 1.0 means chi for this constraint equals ε × f(d) without additional scope scaling — the constraint is primarily national in operation, not amplified by global spread. The institutional enforcer (arbitrage exit) derives d ≈ 0.00 → f(d) ≈ -0.12, producing negative chi (institutional beneficiary position). The powerful/mobile agents at generational scope derive d ≈ 0.55 (victim designation but meaningful exit optionality) → f(d) ≈ 0.75, placing them in the moderate extraction range despite powerful status, because their exit options are real (international relocation, professional migration) but costly enough to require generational timescale.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint avoids mandatrophy through structural clarity about both its coordination function and its extraction function. The beneficiary group (middle/working-class households, fiscal stability advocates) genuinely benefits from deficit reduction coordination — lower future interest rates, more stable growth environment, reduced crowding out of private investment. The victim group (top earners) bears genuine costs through progressive taxation. Active enforcement is required (IRS administration, withholding, audit). The constraint simultaneously coordinates (deficit reduction environment benefits everyone long-term) and extracts (top earners fund a disproportionate share). This dual structure prevents the constraint from collapsing into either pure-rope (no asymmetric extraction) or pure-snare (no coordination benefit). The risk is piton degradation: as deficits re-emerge in subsequent years, the coordination rationale weakens while the extraction mechanism persists, making the constraint increasingly theatrical. The omega variables address this risk — tracking actual deficit reduction permanence, measuring whether growth attribution is fiscal or exogenous, and questioning whether deficit reduction is a necessity (mountain) or contingent policy (tangled rope).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deficit_reduction_permanence,
    'Is the deficit reduction achieved through this constraint temporary (budget mechanics) or permanent (structural fiscal discipline)?',
    'Multi-decade tracking of deficit levels post-1993; isolation of fiscal change due to this constraint vs. changes due to revenue growth, economic cycles, and subsequent legislation',
    'If temporary: constraint should reclassify toward Scaffold (sunset is real, even if long). If permanent: constraint is foundational to long-term fiscal architecture (Tangled Rope confirmed). If deficit re-emerges despite constraint: constraint degrades to Piton (theatrical maintenance of progressive framing despite fiscal failure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deficit_reduction_permanence, empirical, 'Whether deficit reduction is permanent or temporary').

omega_variable(
    progressive_tax_incidence_distribution,
    'What is the true incidence of the progressive tax increase? Does it fall primarily on the declared 1.2% earners, or do secondary effects (business investment, employment, capital relocation) distribute burden more widely?',
    'Dynamic incidence analysis tracking after-tax income distribution, wage changes for non-top earners, employment effects, and capital structure adjustments over 5-10 years post-implementation',
    'If narrow incidence (falls on top 1.2%): Snare/Rope classification is accurate; extraction is concentrated. If broad incidence (distributed via employment/wage effects): beneficiary group expands; Tangled Rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(progressive_tax_incidence_distribution, empirical, 'True distribution of tax incidence across income groups').

omega_variable(
    spending_cut_effectiveness,
    'Do the 340+ spending cuts represent genuine deficit reduction or primarily transfer burden across budget items (accounting reshuffling vs. real spending decline)?',
    'Program-level tracking of actual expenditures vs. baseline projections; isolation of cuts producing real service reduction vs. cuts producing deferrals or transfers to future years',
    'If genuine: constraint involves real resource reallocation (both extraction and coordination). If accounting shuffling: constraint is partly theatrical (theater ratio should increase); Piton classification becomes viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(spending_cut_effectiveness, empirical, 'Whether spending cuts are genuine or accounting transfers').

omega_variable(
    coalition_power_emergence,
    'Do high earners coordinate as a political coalition to resist the constraint, potentially upgrading from Powerful to Organized?',
    'Tracking of lobbying effort, political spending, and policy influence by top-earner groups (Wall Street, hedge funds, professional associations) in subsequent years; comparison of influence relative to middle-class voter coalitions',
    'If coordination emerges: top-earner perspective shifts from Snare toward Tangled Rope or even Rope (if they capture sunset logic). If no coordination: Snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_emergence, empirical, 'Emergence of high-earner political coalition to resist constraint').

omega_variable(
    macroeconomic_growth_attribution,
    'Does the deficit reduction (via this constraint) cause or enable the 1990s economic growth, or is growth endogenous (technology, globalization, demographics) independent of fiscal policy?',
    'Counterfactual macroeconomic modeling: comparison of actual growth trajectory vs. simulated trajectory without deficit reduction; isolation of fiscal contributions from monetary policy, tech acceleration, and demographic factors',
    'If fiscal causation confirmed: constraint''s coordination function is real and substantial; Tangled Rope/Rope perspectives justified. If growth is independent: constraint appears primarily extractive; Snare/Scaffold perspectives gain weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(macroeconomic_growth_attribution, conceptual, 'Attribution of 1990s growth to deficit reduction vs. exogenous factors').

omega_variable(
    false_summit_natural_law,
    'Is deficit reduction through progressive taxation an immutable structural necessity (mountain), or a contingent institutional choice subject to political reversal?',
    'Comparative institutional analysis: examination of alternative fiscal architectures (consumption tax, wealth tax, corporate tax, deficit tolerance) used by comparable nations; analysis of political reversals of this constraint in subsequent administrations',
    'If necessary: mountain classification is defensible. If contingent: mountain is a false summit; Tangled Rope is the accurate classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether deficit reduction is a natural law or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1994_clinton_deficit_reduction_budget, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clint94_tr_t0, sotu_1994_clinton_deficit_reduction_budget, theater_ratio, 0, 0.42).
narrative_ontology:measurement(clint94_tr_t2, sotu_1994_clinton_deficit_reduction_budget, theater_ratio, 2, 0.48).
narrative_ontology:measurement(clint94_tr_t5, sotu_1994_clinton_deficit_reduction_budget, theater_ratio, 5, 0.55).
narrative_ontology:measurement(clint94_tr_t8, sotu_1994_clinton_deficit_reduction_budget, theater_ratio, 8, 0.62).

% Extraction over time
narrative_ontology:measurement(clint94_be_t0, sotu_1994_clinton_deficit_reduction_budget, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(clint94_be_t2, sotu_1994_clinton_deficit_reduction_budget, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(clint94_be_t5, sotu_1994_clinton_deficit_reduction_budget, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(clint94_be_t8, sotu_1994_clinton_deficit_reduction_budget, base_extractiveness, 8, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1994_clinton_deficit_reduction_budget, resource_allocation).
narrative_ontology:affects_constraint(sotu_1994_clinton_deficit_reduction_budget, 1990s_economic_growth_attribution).
narrative_ontology:affects_constraint(sotu_1994_clinton_deficit_reduction_budget, bond_market_credibility_restoration).
narrative_ontology:affects_constraint(sotu_1994_clinton_deficit_reduction_budget, interest_rate_transmission_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is structurally distinct from general progressive taxation systems and from deficit reduction mechanisms in other eras. Its specific formulation (concentrating burden on top 1.2%, distributing spending cuts across 340+ items) produces unique perspectival profile. Related constraints tracking long-term deficit trajectory and theater-ratio degradation should be evaluated separately via network propagation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1994_clinton_deficit_reduction_budget, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
