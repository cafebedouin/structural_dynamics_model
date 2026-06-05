% ============================================================================
% CONSTRAINT STORY: franchisee_corporate_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_franchisee_corporate_squeeze, []).

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
 *   constraint_id: franchisee_corporate_squeeze
 *   human_readable: Franchisee Corporate Squeeze
 *   domain: economic/franchise_systems
 *
 * SUMMARY:
 *   The franchise business model presents as a coordination solution to a
 *   capital and operational problem: corporations gain rapid expansion
 *   without balance-sheet risk; entrepreneurs gain branded business models
 *   with lower startup risk than independent ventures. However, once
 *   franchisees are locked into agreements with sunk costs (buildout,
 *   training, brand-specific equipment, local market reputation), franchisors
 *   gain asymmetric power to extract wealth through successive mechanisms:
 *   royalty increases (marketed as 'system improvements'), mandatory product
 *   purchases at inflated prices (marketed as 'supply chain coordination'),
 *   marketing fee inflation (marketed as 'brand protection'), technology fee
 *   imposition (marketed as 'system modernization'), and unfavorable renewal
 *   terms (marketed as 'performance standards'). The constraint exhibits
 *   genuine coordination function in the system's initial phase — franchisees
 *   do receive real brand value, training, and supply chain benefits. But as
 *   franchisees accumulate sunk costs and become dependent on the business
 *   for livelihood, the franchisor's power to extract through fee increases,
 *   mandatory purchases, and renewal threat accelerates. The extractiveness
 *   trajectory (0.32→0.58) reflects this dynamic: the constraint begins as
 *   near-pure coordination (Rope), transitions through hybrid territory as
 *   sunk costs accumulate (Tangled Rope), and approaches Snare as franchisees
 *   mature and face renewal negotiations from positions of weakness.
 *
 * KEY AGENTS:
 *   - Franchisee Operators: Primary victims (powerless/trapped or moderate/constrained) — invest life savings; face non-negotiable fee escalation; limited exit options; experience maximum extraction at biographical time horizon
 *   - Franchisor Corporation: Primary beneficiary (institutional/arbitrage) — captures capital accumulation through franchisee sweat equity; maintains contractual veto over business decisions; exits friction-free through refranchising or consolidation
 *   - Franchisee Association: Organized secondary actor (organized/constrained) — trade groups and class-action coalitions that generate partial counter-power through collective bargaining and litigation
 *   - Regulatory Framework: Institutional actor (institutional/constrained) — state franchise disclosure laws and relationship protections provide performative oversight with weak enforcement
 *   - Local Market Competition: Victim collective (powerless/trapped) — franchisees locked into high-cost structures undercut by competing systems, creating local market concentration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(franchisee_corporate_squeeze, 0.58).
domain_priors:suppression_score(franchisee_corporate_squeeze, 0.68).
domain_priors:theater_ratio(franchisee_corporate_squeeze, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, extractiveness, 0.58).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(franchisee_corporate_squeeze, tangled_rope).
narrative_ontology:human_readable(franchisee_corporate_squeeze, "Franchisee Corporate Squeeze").
narrative_ontology:topic_domain(franchisee_corporate_squeeze, "economic/franchise_systems").

domain_priors:requires_active_enforcement(franchisee_corporate_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(franchisee_corporate_squeeze, franchisor_corporation).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, franchisee_operators).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, local_market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED FRANCHISEE (SNARE) — Franchisee has invested life savings in buildout, training, and brand equity. Exit requires selling the business at distressed prices (franchisor often controls buyer approval), walking away from sunk costs, or litigating contracts with asymmetric legal resources. Royalty increases, mandatory product purchases at inflated prices, and technology fees are non-negotiable. No coordination benefit perceived — only extraction.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTRAINED MULTI-UNIT OPERATOR (TANGLED ROPE) — Larger franchisees operating multiple units have some negotiating leverage and can absorb fee increases by optimizing operations. Still constrained by contract terms and renewal risk, but genuine coordination benefits exist: brand management, supply chain leverage, marketing coordination. Extraction is real but coexists with coordination function.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FRANCHISOR CORPORATION (ROPE) — The franchisor experiences the system as pure coordination: franchisees provide capital and operational labor; the franchisor provides brand, systems, and scaling logistics. The corporation has full exit optionality — can restructure, divest, refranchise, or consolidate. Perceives compliance as beneficial coordination, not coercion.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FRANCHISEE ASSOCIATION (TANGLED ROPE) — Organized franchisee groups (trade associations, class-action coalitions) have demonstrated capacity to negotiate terms, publicize abusive fee structures, and bring litigation. Still constrained by asymmetric legal resources and franchisor leverage, but organized power generates some exit optionality and genuine coordination benefit through collective bargaining.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — State franchise disclosure laws (Item 19 financials, franchise relationship laws) create a performative apparatus that legitimizes franchise systems while remaining largely unenforceable at scale. Regulations require disclosure and establish renewal/termination protections, but enforcement is reactive, underresourced, and rarely deters the most extractive practices. The regulatory structure persists through institutional inertia and franchisee reliance on imperfect legal remedies.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, information asymmetry and principal-agent misalignment in franchise relationships may appear as inherent features of scaled business models: corporations must monitor franchisees, incentive alignment requires performance penalties, and market dynamics reward systems with lowest franchisee returns. This perspective risks naturalizing extraction as inevitable, but the structural data reveals identifiable beneficiaries extracting through deliberate policy choices, not immutable law.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(franchisee_corporate_squeeze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(franchisee_corporate_squeeze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(franchisee_corporate_squeeze, TR),
    TR >= 0.70.

:- end_tests(franchisee_corporate_squeeze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The franchisor extracts wealth through multiple channels that accumulate over the franchisee's lifetime in the system: royalties (typically 3-7% of gross revenue), mandatory product purchases (10-20% markup above market), marketing fund fees (1-3% of revenue, often without franchisee input on spending), technology fees (recently imposed, averaging $500-2000/month), and renewal negotiation leverage (franchisor can refuse renewal, forcing sale at depressed prices). However, the extractiveness is not maximal (0.66+) because some fee components correlate with genuine coordination benefits (brand management, supply chain efficiency, marketing reach). The franchisee experiences real value from these services in the system's early phases; extraction becomes apparent only in the mature phase when renewal renegotiation occurs. Base extractiveness reflects this: the system extracts significantly but not ruthlessly. Suppression (0.68): High. Multiple mechanisms prevent franchisee exit: (1) Sunk costs — franchise buildout costs $250K-2M+, plus training, equipment, local market investment; (2) Non-transferability — franchisor approval required for owner transfers, and approval is often denied or conditional on onerous terms; (3) Legal asymmetry — franchisor's contract terms are non-negotiable adhesion contracts with litigation costs that dwarf franchisee resources; (4) Market concentration — in many categories (quick-service restaurants, fitness, home services), a small number of franchisors dominate, leaving limited competitive alternatives; (5) Psychological lock — franchisees often develop identity fusion with the brand, internalizing franchisor's framing of extraction as necessary investment. Theater ratio (0.52): Moderate-high. The franchise system maintains substantial performative apparatus: brand standards and compliance audits (many serve franchisee benefit but create theater of 'quality control'), technology platforms and upgrade requirements (mix of genuine coordination and obsolescence-driven replacement), and marketing fund governance (franchisees ostensibly have input through advisory councils, but franchisor retains unilateral budget authority and spending discretion). Theater has increased over the measurement interval as technology fees and digitization mandates accumulated without clear franchisee benefit transparency.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharply divergent perspectives based on agent power and exit capacity. The trapped single-unit franchisee (powerless/biographical/trapped) experiences a Snare: non-negotiable fee escalation with no exit option except business failure or distressed sale. The multi-unit operator (moderate/biographical/constrained) experiences Tangled Rope: some negotiating leverage, ability to absorb costs through optimization, genuine coordination benefits in marketing and supply chain. The franchisor (institutional/immediate/arbitrage) experiences Rope: the system purely coordinates — franchisees provide capital and labor, franchisor provides brand and systems. The organized franchisee association (organized/generational/constrained) experiences Tangled Rope with negotiating power: has demonstrated capacity for litigation and media leverage that moderates franchisor extraction. The regulatory framework (institutional/biographical/constrained) experiences Piton: franchise disclosure laws create a legitimizing ritual (Item 19 financial statements, relationship termination protections) that feels like regulation but remains largely unenforced, persisting through institutional inertia. The analytical observer at civilizational scope risks Mountain: 'Information asymmetry and principal-agent misalignment are inherent to scaled business models.' But the structural data reveals this as false summit — the extraction mechanism is deliberate (fee structures, renewal terms, mandatory purchasing) and benefits identifiable agents (the franchisor), not an immutable law of economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective follows from the agent's structural position relative to the extraction flow. The franchisee victim (trapped exit) derives d≈0.92 from victim status + trapped exit → high f(d) → high chi. The franchisor beneficiary (arbitrage exit) derives d≈0.08 from beneficiary status + arbitrage exit → negative f(d) → negative chi (experienced as subsidy). The regulatory framework (constrained, neither beneficiary nor victim) derives d from moderate power + constrained exit, but the piton classification overrides via theater gate rather than chi threshold. The multi-unit operator (moderate power, constrained exit) derives d≈0.65 from mixed victim-beneficiary status — constrained exit keeps d elevated but benefits from coordination reduce it below trapped level. The analytical observer derives canonical d≈0.73 (analytical context), producing chi that highlights the false summit: the mountain classification from this perspective is belied by the structural beneficiary declaration.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by showing that Tangled Rope classification (ε=0.58, suppression=0.68, requires_active_enforcement=true, beneficiaries + victims + enforcement present) is structurally correct: the franchise system coordinates supply, brand, and marketing functions while simultaneously extracting wealth through fee escalation and renewal leverage. The mandatrophy dissolves when the analytical observer acknowledges that 'efficiency' in the franchise model accumulates toward the franchisor not because it is inherent to the model, but because the franchisor structures fees, renewal terms, and mandatory purchasing to extract surpluses generated by franchisee labor and capital. The system is neither pure coordination (Rope) nor pure extraction (Snare); it is hybrid. The tragedy is that franchisees often cannot perceive or organize against the extraction while they are identity-locked to the brand. The mandatrophy is resolved by recognizing that from the trapped franchisee's biological time horizon, the constraint is experientially indistinguishable from a Snare, even though from the system's structural perspective it is Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchisor_reinvestment_allocation,
    'What proportion of franchisor fee revenue returns to franchisee-serving infrastructure vs franchisor profit extraction?',
    'Franchisor financial disclosure; allocation of royalty revenue to marketing fund, supply chain, R&D, corporate overhead; comparison across comparable franchise systems',
    'If <40% reinvested: extraction classification confirmed. If >60% reinvested: constraint reclassifies toward Rope (coordination emphasis). If 40-60% split: Tangled Rope classification stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchisor_reinvestment_allocation, empirical, 'Reinvestment of franchisor fees in franchisee infrastructure vs corporate profit').

omega_variable(
    mandatory_supply_pricing_justification,
    'Do mandatory product purchases represent genuine supply chain coordination benefits, or pure price extraction above market rates?',
    'Competitive market pricing analysis; comparison of franchisor-mandated supplier prices vs open-market alternatives for identical products; franchisee cost analysis with and without mandatory purchasing',
    'If prices ≤ market rates: coordination function genuine, extraction is coordination cost, tangled_rope stable. If prices >20% above market: extraction mechanism confirmed independent of supply chain claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_supply_pricing_justification, empirical, 'Whether mandatory supply pricing represents coordination or pure extraction').

omega_variable(
    renewal_term_power_asymmetry,
    'Do franchise renewal terms systematically favor franchisor renegotiation power, creating a bilateral bargaining structure where franchisees have no genuine alternative?',
    'Comparative analysis of renewal terms across franchise systems; franchisee renewal acceptance rates; correlation between unit age and fee increases; litigation patterns over renewal disputes',
    'If renewal refusal leads to immediate business termination: trapped exit classification confirmed. If renewals are genuinely negotiable: constrained classification more accurate. If franchisor offers perpetual terms: suppression drops.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewal_term_power_asymmetry, empirical, 'Whether renewal terms create structural power imbalance favoring franchisor').

omega_variable(
    identity_lock_franchise_identity,
    'Do franchisees become identity-locked to the franchise brand, internalizing the franchisor''s framing of extraction as brand investment?',
    'Franchisee narratives and exit interviews; analysis of how franchisees frame mandatory fees (as brand protection vs extraction); correlation between identity fusion and acceptance of unfavorable renewal terms; post-exit franchisee reflections on perception shift',
    'If identity-locked: biographical time horizon produces Rope classification despite trapped exit status — the franchisee''s own frame prevents perception of extraction. If purely constrained: biographical time classifies as Snare. Identity-lock detection reveals that exit barriers are partially cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_franchise_identity, conceptual, 'Whether franchisee identity is fused with the franchise brand, preventing exit perception').

omega_variable(
    franchisor_market_power_concentration,
    'Does franchisor control over supply chains, technology platforms, and brand certification create structural monopoly power, or do competitive alternatives limit extraction capacity?',
    'Market concentration analysis in specific franchise sectors; availability of competing franchise systems in the same category; franchisee ability to source alternative suppliers; technology lock-in severity',
    'If franchisor monopoly in category: suppression increases (trapped classification confirmed). If competitive alternatives exist: exit_options shift from trapped to constrained; classification moderates toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchisor_market_power_concentration, empirical, 'Franchisor market power and availability of competitive alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(franchisee_corporate_squeeze, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcs_tr_t0, franchisee_corporate_squeeze, theater_ratio, 0, 0.38).
narrative_ontology:measurement(fcs_tr_t5, franchisee_corporate_squeeze, theater_ratio, 5, 0.48).
narrative_ontology:measurement(fcs_tr_t10, franchisee_corporate_squeeze, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(fcs_be_t0, franchisee_corporate_squeeze, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(fcs_be_t5, franchisee_corporate_squeeze, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fcs_be_t10, franchisee_corporate_squeeze, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(fcs_su_t0, franchisee_corporate_squeeze, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(fcs_su_t5, franchisee_corporate_squeeze, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(fcs_su_t10, franchisee_corporate_squeeze, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(franchisee_corporate_squeeze, resource_allocation).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, small_business_debt_accumulation).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, brand_switching_cost_lock_in).

% DUAL FORMULATION NOTE:
% Franchise squeeze is downstream of the franchise system architecture itself. The upstream constraint (franchise_coordination_structure) instantiates the hybrid coordination-extraction model; this story examines how that structure's asymmetries materialize in extractive fee escalation and renewal leverage. Related constraints: small_business_debt_accumulation tracks the franchisee's sunk-cost dynamics; brand_switching_cost_lock_in examines the cognitive lock created by brand identity fusion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(franchisee_corporate_squeeze, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
