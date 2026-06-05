% ============================================================================
% CONSTRAINT STORY: greshams_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_greshams_law, []).

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
 *   constraint_id: greshams_law
 *   human_readable: Gresham's Law (Bad money drives out good)
 *   domain: economic/monetary
 *
 * SUMMARY:
 *   Gresham's Law describes a structural tension in commodity money systems
 *   where two forms of circulating money with identical legal face value but
 *   different intrinsic metallic content create a rational incentive
 *   asymmetry: holders hoard the money with higher intrinsic value (good
 *   money) and spend the debased coinage (bad money), eventually removing the
 *   good money from circulation entirely. This constraint operates across
 *   centuries of monetary history and has shaped the debasement cycles of
 *   medieval and early modern states. The mechanism combines elements of
 *   coordination (legal tender standardization solves transaction costs) and
 *   extraction (seigniorage transfers real wealth from merchants and savers
 *   to monetary authorities through debasement). The constraint's
 *   classification depends critically on the observer's structural position:
 *   the sovereign views debasement as immediate fiscal necessity and
 *   legitimate monetary authority; the merchant or saver views it as forced
 *   wealth transfer; historians increasingly view the 'law' as a piton—a
 *   principle maintained in textbooks and historical narratives despite
 *   modern fiat systems' failure to exhibit the predicted behavior.
 *
 * KEY AGENTS:
 *   - Sovereign or Monetary Authority: Primary beneficiary (institutional/arbitrage) — captures seigniorage revenue through debasement mandates
 *   - Merchants and Small Traders: Primary victims (powerless/trapped) — forced to accept debased coins at legal face value; bear inflation cost
 *   - Hoarders and Savers: Secondary victims (moderate/constrained) — rationally withdraw good money, accelerating collapse; trapped between accepting debasement or losing purchasing power
 *   - Merchant Guilds or Trade Coalitions: Organized pressure actors (organized/constrained) — depend on monetary system but can coordinate pressure for currency stability
 *   - Legitimate Commerce: Abstract victim (powerless/trapped) — monetary instability increases transaction costs and uncertainty; lacks agency to resist
 *   - Economic Historians and Textbook Tradition: Institutional inertia (institutional/arbitrage) — perpetuate Gresham's Law as explanatory principle despite its failure in modern systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(greshams_law, 0.38).
domain_priors:suppression_score(greshams_law, 0.52).
domain_priors:theater_ratio(greshams_law, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(greshams_law, extractiveness, 0.38).
narrative_ontology:constraint_metric(greshams_law, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(greshams_law, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(greshams_law, tangled_rope).
narrative_ontology:human_readable(greshams_law, "Gresham's Law (Bad money drives out good)").
narrative_ontology:topic_domain(greshams_law, "economic/monetary").

domain_priors:requires_active_enforcement(greshams_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(greshams_law, debasers_and_counterfeiters).
narrative_ontology:constraint_beneficiary(greshams_law, monetary_authorities_controlling_supply).
narrative_ontology:constraint_victim(greshams_law, savers_and_hoarders).
narrative_ontology:constraint_victim(greshams_law, legitimate_commerce).
narrative_ontology:constraint_victim(greshams_law, currency_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MERCHANT/SMALL TRADER (SNARE) — Forced to accept debased coinage at legal face value. Cannot refuse; cannot exit the monetary system. Bears full cost of inflation. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.52.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: HOARDER/SAVER (SNARE) — Rationally withdraws good money from circulation to preserve value, but this rational individual choice accelerates the collective collapse. Trapped between accepting debased money or losing purchasing power. d≈0.88, f(d)≈1.28, σ=0.9 → χ≈0.48.
constraint_indexing:constraint_classification(greshams_law, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MONETARY AUTHORITY/SOVEREIGN (ROPE) — Benefits from immediate revenue through debasement (seigniorage). Experiences the constraint as a coordination mechanism: decree fiat value, mandate acceptance, solve immediate fiscal crises. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.04. Net beneficiary; sees Gresham's Law as an enabling rule, not an extraction.
constraint_indexing:constraint_classification(greshams_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MERCHANT GUILD/TRADE COALITION (TANGLED ROPE) — Organized enough to pressure authorities for currency stability and legal tender reforms, but constrained by dependence on monetary system. Both benefits from coordination (legal tender simplifies transactions) and bears extraction (seigniorage tax on commerce). d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(greshams_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL ARCHIVE (PITON) — Gresham's Law is invoked ritualistically in monetary textbooks as an explanatory principle, but modern fiat money systems do NOT obey it (paper dollars do not disappear when debased; they circulate at nominal value). The 'law' persists in pedagogy through theatrical invocation despite empirical failure in the systems it purports to describe. theater_ratio≈0.75 ≥ 0.70. The constraint is maintained by institutional inertia: economists still cite it as if it holds universally.
constraint_indexing:constraint_classification(greshams_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / BEHAVIORAL EQUILIBRIUM VIEW (MOUNTAIN) — From a civilizational perspective, Gresham's Law appears as a structural fact of rational behavior under legal tender laws: given identical nominal value but different intrinsic values, and given legal enforcement of acceptance at face value, hoarding the higher-value coin is inevitable. This is not a law of nature but a consequence of rational incentive alignment. However, the structural data (ε=0.38, suppression=0.52, theater=0.35, requires_active_enforcement=true) contradicts the mountain classification — the 'law' only holds under specific institutional conditions (legal tender mandate, commodity money circulation). When conditions change (fiat money, floating exchange rates), the mechanism breaks. This is a false summit.
constraint_indexing:constraint_classification(greshams_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(greshams_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(greshams_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(greshams_law, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(greshams_law, TR),
    TR >= 0.70.

:- end_tests(greshams_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Debasement is a real wealth transfer from holders to authorities, but the extraction is not maximal (≥0.46) because: (1) authorities often debase in response to genuine fiscal crises (wars, infrastructure); (2) merchants can partially pass costs forward; (3) arbitrage mechanisms (currency exchange, commodity hoarding) provide limited exits. The trajectory from 0.22 to 0.38 reflects escalating debasement cycles in historical periods before currency stabilization. Suppression (0.52): Moderate-high. Legal tender mandates prevent refusal; no alternative monetary system is available. But suppression is not total because: (1) parallel barter networks and commodity hoarding exist; (2) merchants can switch to foreign currency or ledger credit; (3) goldsmiths and banks eventually provide alternatives. Theater ratio (0.35): Low-moderate, and increasing slightly (0.20→0.35). Modern textbook invocations of Gresham's Law are increasingly theatrical (fiat systems do not obey it), but historical Gresham's Law itself was highly functional in commodity money systems — the mechanism was real, not performative. The increase reflects the growing gap between the law's historical validity and its modern applicability.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival divide between sovereigns and merchants. The sovereign sees Gresham's Law as a coordination mechanism (Rope): legal tender creates fungibility and simplifies transactions in an empire or kingdom. Debasement is an immediate fiscal tool for war, infrastructure, or emergency spending. The merchant sees extraction (Snare): forced acceptance of depreciated currency is a hidden tax. The analytical observer risks seeing a Mountain (immutable law of rational behavior), but the omega variables reveal this as a false summit — Gresham's Law holds only under specific institutional conditions (legal tender monopoly, commodity money). Modern fiat systems, where all circulating money has identical intrinsic value (zero), do not exhibit the predicted behavior. The piton perspective captures this degradation: the law persists in textbook pedagogy (theater_ratio=0.75 in the historical archive) despite empirical failure in contemporary monetary systems.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign/Monetary Authority: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Experiences debasement as enabling policy, not extraction. Merchants/Traders: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction for powerless agents. No exit from the monetary system; forced to accept debased coins. Hoarders/Savers: Victim + constrained → d≈0.88, f(d)≈1.28. High extraction. Rational individual choice (hoarding) accelerates collective collapse (currency disappearance). Merchant Guild (organized): Beneficiary (partial) + victim (partial) + constrained → d≈0.62, f(d)≈0.85. Mixed: guild benefits from legal tender coordination but bears seigniorage cost. Can pressure for stabilization. Historical Archive (piton): institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification comes from theater gate, not high chi; the institutional invocation benefits from textbook authority despite empirical inapplicability. Analytical Observer: Mountain classification risks d≈0.50 (symmetric natural law view), but the false summit detector catches this — the 'law' is contingent on legal tender mandates, not immutable.
 *
 * MANDATROPHY ANALYSIS:
 *   Gresham's Law resolves mandatrophy by distinguishing institutional conditions from behavioral universals. The 'law' is NOT a mountain (immutable natural law) — it is contingent on the legal tender mandate. In systems without legal tender enforcement (parallel currencies, early medieval barter networks, modern cryptocurrency markets), good money does NOT necessarily disappear; instead, it circulates in premium form or in parallel networks. The law is NOT pure coordination (rope) because debasement transfers real wealth to authorities; it IS a tangled rope: the legal tender mechanism coordinates transactions but extraction (seigniorage) layers on top. The modern piton perspective is diagnostically important: in fiat systems where all money has identical intrinsic value, Gresham's Law becomes inapplicable, yet it persists in textbook discussion (theater_ratio increasing). This piton status indicates institutional degradation — the law has lost empirical power but retains rhetorical authority. The constraint's classification would collapse to 'rope' if: (a) debasement were actually used only in genuine fiscal emergencies, and (b) fiat systems exhibited stable purchasing power. But historical evidence shows repeated episodes of discretionary debasement and modern monetary expansion shows seigniorage as persistent, confirming tangled_rope structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_tender_vs_rational_choice,
    'Is Gresham''s Law a consequence of legal tender mandates, or a deeper feature of rational valuation?',
    'Historical analysis of currency systems with and without legal tender laws; observation of parallel money systems (commodity vs fiat) in the same jurisdiction; behavioral economics experiments on exchange decisions under different legal frameworks',
    'If mandate-dependent: Gresham''s Law is a Tangled Rope (coordination + extraction via fiat). If rational-choice fundamental: Mountain (immutable behavior). Classification swings from tangled_rope to mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_tender_vs_rational_choice, conceptual, 'Whether Gresham''s Law reflects legal mandates or fundamental rational behavior').

omega_variable(
    fiat_money_applicability,
    'Why does Gresham''s Law fail to predict behavior in modern fiat money systems where all circulating notes have identical intrinsic value (zero)?',
    'Empirical observation: fiat currencies do not disappear from circulation when debased through monetary expansion. The law''s empirical failure in the systems it was originally formulated to describe.',
    'If fiat non-applicability is fundamental: Gresham''s Law is a piton (maintained through textbook inertia, not real function). If there is a fiat analog (asset hoarding, capital flight): law generalizes and remains operative in modified form.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiat_money_applicability, empirical, 'Whether Gresham''s Law applies to fiat currency systems').

omega_variable(
    extraction_vs_coordination_boundary,
    'Does seigniorage through debasement constitute legitimate monetary coordination (sovereign funding mechanism) or exploitative extraction from the monetary commons?',
    'Analysis of fiscal necessity vs. discretionary spending; comparison of debasement rates to legitimate state functions funded; historical correlation between debasement cycles and internal conflict or external threat',
    'If legitimate coordination: Rope classification from sovereign perspective. If exploitative: Snare. This omega determines whether the sovereign perspective sees the constraint as beneficial policy or coercive extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, preference, 'Whether seigniorage represents legitimate funding or exploitative extraction').

omega_variable(
    parallel_currency_dynamics,
    'In jurisdictions with parallel currencies (legal tender + commodity hoards + barter networks), do secondary currencies obey Gresham''s Law or does Gresham''s Law only hold for primary legal tender systems?',
    'Historical study of periods with currency competition (pre-unified currency states, underground economies during hyperinflation, modern cryptocurrency emergence); observation of which money dominates different transaction types',
    'If Gresham''s Law is specific to legal tender monopolies: Constraint is institutional (Tangled Rope/Snare under mandate). If law holds across currency types: More fundamental principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parallel_currency_dynamics, empirical, 'Whether Gresham''s Law applies to parallel currency systems').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(greshams_law, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gresh_tr_t0, greshams_law, theater_ratio, 0, 0.2).
narrative_ontology:measurement(gresh_tr_t3, greshams_law, theater_ratio, 3, 0.28).
narrative_ontology:measurement(gresh_tr_t6, greshams_law, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(gresh_be_t0, greshams_law, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(gresh_be_t3, greshams_law, base_extractiveness, 3, 0.3).
narrative_ontology:measurement(gresh_be_t6, greshams_law, base_extractiveness, 6, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(greshams_law, resource_allocation).
narrative_ontology:affects_constraint(greshams_law, monetary_expansion).
narrative_ontology:affects_constraint(greshams_law, currency_debasement_cycle).
narrative_ontology:affects_constraint(greshams_law, legal_tender_mandate).
narrative_ontology:affects_constraint(greshams_law, commodity_hoarding).

% DUAL FORMULATION NOTE:
% Gresham's Law can be decomposed into two structurally distinct constraints: (1) the coordination problem of standardizing multiple commodity monies (legal tender coordination), and (2) the extraction mechanism of seigniorage through debasement. This story treats them as a unified tangled rope. The upstream constraints (legal_tender_mandate, monetary_expansion) have their own ε values; this story addresses the intersection effect.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(greshams_law, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
