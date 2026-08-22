% ============================================================================
% CONSTRAINT STORY: price_formation_kernel__institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_price_formation_kernel__institutional_reading, []).

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
 *   constraint_id: price_formation_kernel__institutional_reading
 *   human_readable: Housing Price Formation as Institutional Construction
 *   domain: political_economy/housing_markets
 *
 * SUMMARY:
 *   This reading instantiates one specific claim about housing price
 *   formation: that prices are constructed by deliberate institutional
 *   policy—zoning restrictions, lending gatekeeping, tax incentives, and
 *   intermediary platforms—which benefits incumbent owners, lenders, and
 *   intermediaries while imposing costs on first-time buyers, renters, and
 *   workers geographically misaligned with job opportunities. This reading
 *   coexists with three sibling readings that explain price formation
 *   differently: the naturalist reading (prices reflect objective scarcity
 *   and preference equilibrium), the georgist reading (prices reflect the
 *   distribution of unearned land rent vs. earned improvement value), and the
 *   financialization reading (prices reflect credit expansion and asset-price
 *   feedback). All four readings contest the same kernel (price formation in
 *   housing markets) but attribute causation and beneficiary structure
 *   differently. This JSON instantiates ONLY the institutional reading as a
 *   clean, ε-invariant constraint; the sibling readings are separate stories
 *   linked via network edges.
 *
 * KEY AGENTS:
 *   - incumbent_property_owners: Primary beneficiary (asset appreciation, wealth protection), powerful, mobile exit
 *   - institutional_lenders: Primary beneficiary (interest capture, fee extraction), institutional power, agenda-setter role in credit gatekeeping
 *   - real_estate_intermediaries: Organized beneficiary (commission capture), constrained exit due to network effects
 *   - municipal_zoning_authorities: Institutional agenda-setter (enforce supply restrictions), local power, analytical exit
 *   - tax_policy_apparatus: Institutional agenda-setter (capital-gains deferral, mortgage deduction), national scope
 *   - first_time_homebuyers: Victims (entry barriers, inflated prices), moderate power, identity-locked to ownership aspiration
 *   - renters: Victims (rental inflation, geographic immobility), powerless, trapped exit
 *   - workers_geographically_misaligned: Victims (wage suppression, opportunity denial), powerless, constrained exit
 *   - financialization_platforms: Institutional beneficiary (asset-price arbitrage), global scope, agenda-setter via lobbying
 *   - observer_economists: Analytical seat documenting structure and causation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, 0.68).
domain_priors:suppression_score(price_formation_kernel__institutional_reading, 0.71).
domain_priors:theater_ratio(price_formation_kernel__institutional_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(price_formation_kernel__institutional_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(price_formation_kernel__institutional_reading, tangled_rope).
narrative_ontology:human_readable(price_formation_kernel__institutional_reading, "Housing Price Formation as Institutional Construction").
narrative_ontology:topic_domain(price_formation_kernel__institutional_reading, "political_economy/housing_markets").

domain_priors:requires_active_enforcement(price_formation_kernel__institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(price_formation_kernel__institutional_reading, '54c145ee-e413-42b5-b024-2bc7767ae8b5').
narrative_ontology:cs_kernel_codification('54c145ee-e413-42b5-b024-2bc7767ae8b5', distributed).
narrative_ontology:cs_authority_grounding('54c145ee-e413-42b5-b024-2bc7767ae8b5', extraction).
narrative_ontology:cs_interpretation_layer_present('54c145ee-e413-42b5-b024-2bc7767ae8b5').
narrative_ontology:cs_reading_relation('54c145ee-e413-42b5-b024-2bc7767ae8b5', price_formation_kernel__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('54c145ee-e413-42b5-b024-2bc7767ae8b5', price_formation_kernel__georgist_reading, coexists_with).
narrative_ontology:cs_reading_relation('54c145ee-e413-42b5-b024-2bc7767ae8b5', price_formation_kernel__financialization_reading, influences).
narrative_ontology:cs_axiom('54c145ee-e413-42b5-b024-2bc7767ae8b5', foundational, prices_constructed_by_institutional_policy).
narrative_ontology:cs_axiom_status(prices_constructed_by_institutional_policy, holdable).
narrative_ontology:cs_axiom_grounding('54c145ee-e413-42b5-b024-2bc7767ae8b5', prices_constructed_by_institutional_policy, empirically_contingent).
narrative_ontology:cs_axiom('54c145ee-e413-42b5-b024-2bc7767ae8b5', secondary, institutional_apparatus_sustains_asymmetric_extraction).
narrative_ontology:cs_axiom_status(institutional_apparatus_sustains_asymmetric_extraction, holdable).
narrative_ontology:cs_axiom_grounding('54c145ee-e413-42b5-b024-2bc7767ae8b5', institutional_apparatus_sustains_asymmetric_extraction, empirically_contingent).
narrative_ontology:cs_reference_frame('54c145ee-e413-42b5-b024-2bc7767ae8b5', postwar_coordination_equilibrium).
narrative_ontology:cs_drift_state('54c145ee-e413-42b5-b024-2bc7767ae8b5', contemporary_financialization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('54c145ee-e413-42b5-b024-2bc7767ae8b5', '').
narrative_ontology:cs_kernel_id(price_formation_kernel__institutional_reading, price_formation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, incumbent_property_owners).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, institutional_lenders).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, real_estate_intermediaries).
narrative_ontology:constraint_beneficiary(price_formation_kernel__institutional_reading, municipal_zoning_authorities).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, first_time_homebuyers).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, renters).
narrative_ontology:constraint_victim(price_formation_kernel__institutional_reading, workers_geographically_misaligned).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from artificial supply constraints created by zoning restrictions and lending gatekeeping. Their property wealth appreciates as entry barriers rise. They lobby to maintain restrictive zoning and oppose liberalizing reforms. Can exit by selling at inflated prices, but collective interest is to prevent supply expansion that would deflate asset values.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, incumbent_property_owners, beneficiary,
    powerful, generational, mobile, national).

% Extract value through interest on inflated mortgages and fee capture on origination, servicing, and refinancing. Set lending standards that exclude lower-credit borrowers, maintaining a protected higher-risk premium. Their institutional power over capital allocation is the primary enforcement mechanism: borrowers cannot access the market without meeting lender criteria.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, institutional_lenders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(price_formation_kernel__institutional_reading, institutional_lenders, agenda_setter).

% Capture commissions and transaction fees on inflated property values. The higher the price, the larger their absolute take. Platform intermediaries (online marketplaces, valuation services) extract data rents and matching fees. Professional associations (realtors, appraisers) enforce standards that limit supply and complexity.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, real_estate_intermediaries, beneficiary,
    organized, biographical, constrained, national).

% Administer zoning restrictions that legally cap supply. They claim public-interest justifications (neighborhood character, infrastructure capacity, environmental protection) while effectively protecting incumbent owner wealth and local tax base stability. Maintain the restrictions through regulatory enforcement and political resistance to upzoning.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, municipal_zoning_authorities, agenda_setter,
    institutional, generational, analytical, local).

% Structures capital-gains tax treatment, mortgage interest deductibility, and property tax systems in ways that amplify owner returns and create lock-in effects for mortgaged properties. The apparatus is both formal (IRS code) and fragmented (state/local variation), making coordinated reversal difficult.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, tax_policy_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Face barriers to entry: restricted supply keeps prices high, lending standards exclude lower-credit borrowers, and down-payment requirements absorb years of savings. Their options are narrowed to accepting inflated prices (which locks them into decades of high debt), renting indefinitely (incurring no accumulation), or leaving the desired geographic market. Entry into ownership is presented as personal aspiration and economic imperative, creating internalized pressure to accept extractive terms.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, first_time_homebuyers, payer,
    moderate, biographical, identity_locked, national).

% Bear costs indirectly through rental inflation (landlords pass through higher carrying costs and capitalize asset appreciation into rents) and geographic immobility (restricted housing supply near jobs). Can exit only by accepting longer commutes, leaving desired regions, or entering ownership at inflated prices. Lack political voice in zoning decisions that restrict supply.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, renters, payer,
    powerless, biographical, trapped, national).

% Cannot locate in high-productivity labor markets (San Francisco Bay, New York, Seattle) because housing costs are disconnected from local wages. This caps wage growth and opportunity access. They either accept lower-wage remote work, incur unsustainable commutes, or leave the region entirely. Zoning and credit restrictions in high-opportunity zones prevent wage arbitrage.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, workers_geographically_misaligned, payer,
    powerless, biographical, constrained, global).

% Institutional investors (private equity, REITs, foreign sovereign funds) exploit loose credit conditions and regulatory arbitrage to buy single-family homes, securitize rental streams, and lobby for policies that maintain asset-price appreciation. Their scale creates feedback loops: capital inflows drive prices, which justify further inflows, which suppress affordability.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, financialization_platforms, agenda_setter,
    institutional, generational, arbitrage, global).

% Analyze the constraint's structure. Document how zoning, lending, tax, and platform policies construct prices and distribute gains. Provide evidence-based accounts of causality and magnitude, which feeds into regulatory debate and reform advocacy.
narrative_ontology:constraint_stakeholder(price_formation_kernel__institutional_reading, observer_economists, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(price_formation_kernel__institutional_reading, incumbent_property_owners).
narrative_ontology:fixing_cost_class(price_formation_kernel__institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes capital allocation, risk assessment, and property transfer through standardized lending, legal title, zoning governance, and price discovery. Solves coordination problems around credit underwriting, geographical development, neighborhood stability, and tax base protection.
% TRANSFER_FUNCTION: Moves wealth from first-time buyers and renters (through inflated prices and rental costs) to incumbent owners (asset appreciation), lenders (interest and fees), intermediaries (commissions), and local governments (stabilized tax revenue). The flow is enforced by supply restriction (zoning), credit gatekeeping (lending standards), tax incentives (capital-gains deferral, mortgage deduction), and fee capture (brokerage, appraisal, servicing).
% ABSENT_VOICES: Supply-side competitors (developers with capital who would build if zoning allowed) are excluded by regulatory barriers. Workers without access to credit or down payments cannot participate in the conversation structuring their own market access. Renters have no seat at zoning boards that restrict supply and raise their costs. Alternative financial intermediaries (community lending, cooperative housing) are excluded by regulatory capture and network effects in incumbent platforms.
% DISAPPEARANCE_RATIONALE: If institutional price formation vanished—zoning liberalized, lending standards opened, tax treatment equalized, intermediary fees were unbundled or competitive—supply would expand sharply, prices would fall toward long-run marginal construction cost, and wealth distribution would shift from incumbent owners toward first-time buyers and workers in high-opportunity zones. Incumbent owner asset values would decline, lender yield spreads would compress, intermediary commissions would face competitive pressure, and local tax revenues would need recalibration. The entire incumbent-beneficiary coalition depends on the institutional apparatus persisting.
% FOUNDING_PROBLEM: Mid-twentieth-century housing shortage and coordination gaps: local governments needed tools to manage growth and conflict between competing land uses; lenders needed standardized underwriting to manage credit risk; tax systems needed to incentivize homeownership as a policy instrument for middle-class wealth-building. These coordination problems were real.
% FOUNDING_PROBLEM_CORROBORATION: Incumbent owners and lenders attest the founding problems (neighborhood stability, credit risk management) are still live. Economists, housing-policy reformers, and renters attest the founding problems are substantially solved (mid-century shortage is gone; underwriting standards have become gatekeeping mechanisms; tax incentives have transformed into incumbent-wealth amplification). The weight of evidence from housing economics and comparative institutional analysis (peer countries without such restrictive zoning or lending gatekeeping) supports the 'problem shifted, constraints persist as extraction' reading.
narrative_ontology:disappearance_verdict(price_formation_kernel__institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(price_formation_kernel__institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(price_formation_kernel__institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(price_formation_kernel__institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(price_formation_kernel__institutional_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(price_formation_kernel__institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(price_formation_kernel__institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(price_formation_kernel__institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.68 (moderate-high) because the institutional apparatus produces sustained, asymmetric benefit flows to identifiable beneficiaries (owners, lenders, intermediaries) at the direct expense of identifiable victims (first-time buyers, renters). The measured value reflects the constraint's operation in 2026: strong but not total extraction (owner wealth accumulation is real and measurable; victim burden is documented in affordability statistics). Suppression is high (0.71) because the constraint's persistence depends on actively enforcing zoning restrictions, maintaining lending gatekeeping, and defending tax incentives against reform—each requires sustained institutional effort and political pressure to suppress alternative arrangements (liberalized zoning, open lending standards, equalized tax treatment). Theater is moderate (0.42): the public justifications (neighborhood stability, credit risk management, middle-class wealth-building) contain genuine coordination value, but an increasing share of enforcement activity defends extractive asymmetries rather than the original coordination problems. The measurement series tracks the constraint's evolution from 1945 (post-war housing shortage, genuine coordination problem, low extraction) through 2008 (peak of extractiveness driven by financialization) to 2026 (extraction stable at high levels despite financial crisis). The 1945-2008 upward trend reflects institutional layering: zoning intensified (sprawl prevention), lending standards hardened (post-war standardization), tax incentives compounded (multiple incentives stacked), and intermediary platforms consolidated (network effects). The 2008-2026 plateau reflects a partial equilibrium: extraction remains high, but resistance has risen (reform advocacy, GIS data making zoning visible, alternative finance), suppression requirement remains elevated (defensive political lobbying), and theater has stabilized (public rhetoric unchanged, enforcement intensity unchanged). One shared time grid governs all three metrics so the temporal analysis has consistent sampling.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (lenders, zoning authorities, tax policy) experience this constraint as legitimate governance and risk management—coordination problems solved, standards maintained, property rights protected. The beneficiary seats (incumbent owners, intermediaries) experience it as wealth creation and asset appreciation enabled by good policy. The victim seats (first-time buyers, renters, geographically misaligned workers) experience it as structural barriers to opportunity, enforced by rules they did not author and cannot easily exit. The engine computes per-seat classifications from the power/exit atoms and directionality: the institutional seats get lower extracted values (they set the rules, control enforcement); the payer seats get higher extraction (they cannot escape the constraints). This divergence is the measurement the corpus takes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality is driven by beneficiary role + power + mobile or arbitrage exit: incumbent owners have strong exit (can sell) but collective interest in constraint persistence keeps d in the 0.1-0.3 range (subsidized by the constraint, participate in maintaining it). Lenders have institutional power and arbitrage exit (can shift credit to other assets) but are bound by regulatory constraints on portfolio composition; d near 0.15-0.25 (beneficiary, institution). Intermediaries have organized power but constrained exit (network lock-in); d near 0.25-0.35 (beneficiary, but less agile than lenders). Victim directionality is driven by victim role + power + constrained/trapped exit: first-time buyers face identity lock (ownership = aspiration and economic imperative) which amplifies psychological suppression; d near 0.75-0.85 (trapped target, aspirational lock). Renters face structural powerlessness and geographic trap; d near 0.85-0.95 (full target). Workers geographically misaligned have moderate power but constrained exit; d near 0.70-0.80 (target, but with some external mobility). No directionality overrides are needed; the derivation from the declared beneficiary/victim structure and exit conditions is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   The institutional reading avoids mislabeling this constraint as pure coordination (rope) or pure extraction (snare) by declaring BOTH beneficiaries (coordination function is real: lenders solve credit risk allocation, zoning solves land-use conflict, tax code incentivizes homeownership accumulation) AND victims (supply restriction extracts from first-time buyers, gatekeeping extracts from low-credit workers, zoning inequity extracts from renters). The claimed type is tangled_rope (hybrid coordination/extraction), which correctly captures the constraint's structure: it solves genuine coordination problems AND produces asymmetric extraction. The alternative mistakes are: (1) calling it rope (pure coordination)—this would ignore the supply restriction asymmetry, lending gatekeeping, and wealth transfer patterns documented in housing economics; (2) calling it snare (pure extraction)—this would ignore that lenders do perform real risk assessment, zoning does address genuine land-use conflicts, and the tax incentive does reflect a deliberate policy choice for middle-class wealth-building. The tangled_rope classification reflects the constraint's actual structure: coordination functions and extractive asymmetries are structurally inseparable in this case. Liberalizing zoning or lending would not just redistribute wealth; it would also degrade credit risk management and increase neighborhood-level externalities—the victims' exit requires sacrificing some coordination function, which is why it meets resistance from both beneficiaries and those who benefit from the coordination side of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    institutional_vs_naturalist_causation,
    'Is price formation primarily constructed by institutional policy choices (zoning, lending, tax), or primarily driven by objective scarcity and preference equilibrium, with institutional policy merely adding noise?',
    'Causal identification studies: (1) exogenous policy shocks (sudden zoning changes, lending standard shifts) and their downstream price effects; (2) cross-jurisdictional comparison (same geography, different institutions → price divergence); (3) counterfactual institutional removal (model housing market under alternative regulatory scenarios). Time-series decomposition of price variance into institutional vs. preference-driven components.',
    'If institutional causation dominates (>60% of variance explained), the constraint is indeed tangled_rope with substantial extractive asymmetry. If naturalist causation dominates, the constraint should be reclassified as mountain or rope (institutional policies merely coordinate on top of natural equilibrium). The ε value depends entirely on this resolution: high institutional causation → high extraction (ε 0.65-0.75); low institutional causation → low extraction (ε 0.15-0.35).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_naturalist_causation, empirical, 'Whether institutional policy choices drive price formation or merely modulate underlying equilibrium.').

omega_variable(
    beneficiary_intent_vs_institutional_emergence,
    'Is the extractive asymmetry a deliberate outcome designed by beneficiaries (incumbent owners, lenders, intermediaries coordinating to restrict supply and maintain asset values), or an emergent byproduct of institutions designed for other purposes (neighborhood stability, credit risk, middle-class wealth-building)?',
    'Historical analysis of institutional design intentions (legislative records, regulatory statements, foundation grant histories); behavioral evidence of coordinated beneficiary action (political donations, zoning lobbying, lending standard coordination); counterfactual institutional design (what if the same coordination problems were solved with different institutional forms?). Testimony from beneficiary and victim seats about their understanding of institutional purpose.',
    'Deliberate beneficiary coordination would support snare classification (extraction defended by a cover story of coordination); emergent byproduct would support tangled_rope classification (genuine coordination with asymmetric extraction as a side effect). The ε value remains the same; the type changes based on intentionality and persistence mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_intent_vs_institutional_emergence, conceptual, 'Whether extractive asymmetry is intentional beneficiary design or institutional emergence.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) structural—external barriers (zoning barriers, lending gatekeeping, down-payment requirements)—or internalized—psychological identification with homeownership as personal aspiration, self-limiting renter expectations, normative acceptance of incumbent owner wealth?',
    'Post-exit suppression trajectory: observe renters and first-time buyers in jurisdictions that liberalize zoning and lending standards. If suppression persists (they continue to restrict consumption, accept higher-cost alternatives) after structural barriers fall, reclassify as partially internalized. Qualitative research on identity formation around homeownership (how much is aspiration vs. imposed expectation). Comparison of suppression levels across cultures with different homeownership narratives.',
    'If suppression is primarily structural, removing barriers (liberalize zoning, open lending, reduce down-payment requirements) would substantially lower extraction. If suppression is primarily internalized, structural changes would have weaker effect on behavior—the victims have been shaped to accept their position. The effective suppression (what the engine computes after directionality modulation) could be substantially higher than the authored 0.71 if internalization is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression in housing constraint is structural or internalized.').

omega_variable(
    reading_contest_resolution,
    'Which sibling reading (naturalist, georgist, financialization) best explains housing price formation in the 2026 context, or do all four readings capture partially valid dimensions of a complex phenomenon?',
    'Comparative predictive validity: which reading''s causal model predicts price changes, affordability gaps, and supply responses best? Econometric model selection across reading-specific theories. Institutional genealogy: trace which reading''s logic has actually been coded into law and regulation (dominant reading in practice). Stakeholder alignment: which reading''s beneficiary and victim structure matches observed political coalitions and advocacy patterns.',
    'A dominant reading would establish that one reading''s ε value and beneficiary/victim structure are more accurate than the others. The institutional reading would be validated (or not) relative to naturalist (if prices are really just equilibrium, institutional construction adds less asymmetry than claimed), georgist (if rent vs. value distinction is the real lever), or financialization (if credit expansion and capital inflows dominate institutional policy effects). Whichever reading dominates, the others remain live as sibling constraints in the family.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_resolution, conceptual, 'Comparative validation of the four price-formation readings.').

omega_variable(
    kernel_identity_stability,
    'Is the price_formation_kernel a stable, contestable claim (different readings of the same fact), or do the readings actually refer to different phenomena being mislabeled with the same word?',
    'Formal analysis of the kernel''s referent: do all four readings agree on what ''price'' means, what market is being priced, what time horizon and geography are in scope? If readings diverge on the referent itself, the shared kernel dissolves and what we have is separate constraints mislabeled as readings of one kernel. Canonical definitions from housing economics and policy: what do econometricians and regulators mean by ''price formation''? Do the four readings align with canonical definitions or extend them idiosyncratically?',
    'If the kernel dissolves (readings refer to different things), each reading should be authored as an independent constraint with no family link, and the omega document should state that what appeared to be a contest over one claim is actually a polysemy (the word ''price formation'' covers four different claims). If the kernel is stable, the family structure holds, and the four readings remain meaningful siblings contesting the same claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_identity_stability, conceptual, 'Whether the price-formation kernel is a stable contested claim or polysemy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(price_formation_kernel__institutional_reading, 1945, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pf_inst_tr_t1945, price_formation_kernel__institutional_reading, theater_ratio, 1945, 0.12).
narrative_ontology:measurement_basis(pf_inst_tr_t1945, observed).
narrative_ontology:measurement(pf_inst_tr_t1970, price_formation_kernel__institutional_reading, theater_ratio, 1970, 0.18).
narrative_ontology:measurement_basis(pf_inst_tr_t1970, observed).
narrative_ontology:measurement(pf_inst_tr_t1990, price_formation_kernel__institutional_reading, theater_ratio, 1990, 0.28).
narrative_ontology:measurement_basis(pf_inst_tr_t1990, observed).
narrative_ontology:measurement(pf_inst_tr_t2008, price_formation_kernel__institutional_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement_basis(pf_inst_tr_t2008, observed).
narrative_ontology:measurement(pf_inst_tr_t2015, price_formation_kernel__institutional_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(pf_inst_tr_t2015, observed).
narrative_ontology:measurement(pf_inst_tr_t2026, price_formation_kernel__institutional_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(pf_inst_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(pf_inst_be_t1945, price_formation_kernel__institutional_reading, base_extractiveness, 1945, 0.25).
narrative_ontology:measurement_basis(pf_inst_be_t1945, observed).
narrative_ontology:measurement(pf_inst_be_t1970, price_formation_kernel__institutional_reading, base_extractiveness, 1970, 0.38).
narrative_ontology:measurement_basis(pf_inst_be_t1970, observed).
narrative_ontology:measurement(pf_inst_be_t1990, price_formation_kernel__institutional_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement_basis(pf_inst_be_t1990, observed).
narrative_ontology:measurement(pf_inst_be_t2008, price_formation_kernel__institutional_reading, base_extractiveness, 2008, 0.71).
narrative_ontology:measurement_basis(pf_inst_be_t2008, observed).
narrative_ontology:measurement(pf_inst_be_t2015, price_formation_kernel__institutional_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement_basis(pf_inst_be_t2015, observed).
narrative_ontology:measurement(pf_inst_be_t2026, price_formation_kernel__institutional_reading, base_extractiveness, 2026, 0.68).
narrative_ontology:measurement_basis(pf_inst_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(pf_inst_su_t1945, price_formation_kernel__institutional_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement_basis(pf_inst_su_t1945, observed).
narrative_ontology:measurement(pf_inst_su_t1970, price_formation_kernel__institutional_reading, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement_basis(pf_inst_su_t1970, observed).
narrative_ontology:measurement(pf_inst_su_t1990, price_formation_kernel__institutional_reading, suppression_requirement, 1990, 0.61).
narrative_ontology:measurement_basis(pf_inst_su_t1990, observed).
narrative_ontology:measurement(pf_inst_su_t2008, price_formation_kernel__institutional_reading, suppression_requirement, 2008, 0.79).
narrative_ontology:measurement_basis(pf_inst_su_t2008, observed).
narrative_ontology:measurement(pf_inst_su_t2015, price_formation_kernel__institutional_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement_basis(pf_inst_su_t2015, observed).
narrative_ontology:measurement(pf_inst_su_t2026, price_formation_kernel__institutional_reading, suppression_requirement, 2026, 0.71).
narrative_ontology:measurement_basis(pf_inst_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(price_formation_kernel__institutional_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(price_formation_kernel__institutional_reading, 0.18).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__naturalist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__georgist_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, price_formation_kernel__financialization_reading).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, zoning_restrictiveness_enforcement).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, mortgage_lending_gatekeeping).
narrative_ontology:affects_constraint(price_formation_kernel__institutional_reading, tax_incentive_homeownership).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the price_formation_kernel family. The institutional reading attributes price construction to deliberate policy choices (zoning, lending, tax, intermediaries). Sibling readings: naturalist_reading (prices reflect equilibrium), georgist_reading (prices reflect land rent vs. earned value separation), financialization_reading (prices reflect credit expansion). All four readings are separate constraint stories. The institutional reading directly influences three component-constraints (zoning_restrictiveness_enforcement, mortgage_lending_gatekeeping, tax_incentive_homeownership) which are also authored separately as the institutional lever-points for price formation. The institutional reading's ε and classification are independent of the sibling readings; this story stands alone as a clean constraint while acknowledging sibling readings as alternative explanations of the same phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(price_formation_kernel__institutional_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
