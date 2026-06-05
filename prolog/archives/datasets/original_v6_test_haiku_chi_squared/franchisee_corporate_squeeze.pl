% ============================================================================
% CONSTRAINT STORY: franchisee_corporate_squeeze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: Franchise Agreement Squeeze
 *   domain: economic/labor/regulatory
 *
 * SUMMARY:
 *   The franchise business model presents itself as a coordination mechanism:
 *   the franchisor provides brand, training, supply chain, and marketing
 *   support; the franchisee provides local knowledge, customer relationships,
 *   and operational management. In practice, the franchise agreement has
 *   evolved into a mechanism for wealth extraction from individual
 *   owner-operators to corporate franchisors. The asymmetry is structural:
 *   franchisees make large capital investments ($250k-$2M+) with limited
 *   collateral or recourse; franchisors control pricing, operations, supply
 *   costs, renewal terms, and termination conditions. Franchisees cannot
 *   credibly exit without losing invested capital. The theater ratio reflects
 *   that franchise disclosure documents (FDD) and uniform franchise offer
 *   circulars (UFOC) create an appearance of transparency and fair dealing
 *   that is substantially performative — key profit extraction mechanisms
 *   (supply chain markups, vendor rebates, forced marketing spending) remain
 *   opaque or contractually mandatory despite regulatory disclosure
 *   requirements. The constraint exhibits all six types from different
 *   structural positions, making it a high-extractiveness exemplar of
 *   institutional capture and dependent labor disguised as entrepreneurship.
 *
 * KEY AGENTS:
 *   - Corporate Franchisor: Primary beneficiary (institutional/arbitrage) — extracts through franchising fees, royalties, supply markups, and vendor rebates while controlling brand and supply chain
 *   - Individual Franchisees: Primary victims (powerless/trapped) — bear investment risk, operational burden, and extraction while having minimal control over costs or renewal
 *   - Multi-Unit Franchisees: Secondary actor (moderate/constrained) — have some bargaining power and operational autonomy but remain extraction targets
 *   - Franchisee Association: Organized victim (organized/constrained) — attempts collective action but constrained by fear of retaliation and individual franchisee vulnerability
 *   - Regulatory Enforcer (FTC, State AGs): Institutional observer (organized/mobile) — tasked with enforcement but constrained by burden of proof, franchisee fear of retaliation, and industry lobbying
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes franchise model as scaled extraction mechanism converting entrepreneurship into dependent wage-equivalence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(franchisee_corporate_squeeze, 0.58).
domain_priors:suppression_score(franchisee_corporate_squeeze, 0.68).
domain_priors:theater_ratio(franchisee_corporate_squeeze, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, extractiveness, 0.58).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(franchisee_corporate_squeeze, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(franchisee_corporate_squeeze, tangled_rope).
narrative_ontology:human_readable(franchisee_corporate_squeeze, "Franchise Agreement Squeeze").
narrative_ontology:topic_domain(franchisee_corporate_squeeze, "economic/labor/regulatory").

domain_priors:requires_active_enforcement(franchisee_corporate_squeeze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(franchisee_corporate_squeeze, corporate_franchisor).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, individual_franchisees).
narrative_ontology:constraint_victim(franchisee_corporate_squeeze, franchise_ecosystem_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRANCHISEE OWNER-OPERATOR (SNARE) — Trapped by sunk capital investment, contractual lock-in, and dependence on brand/supply chain. Cannot exit without catastrophic loss. Franchisor controls pricing, operations, marketing, supply costs, and renewal terms. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CORPORATE FRANCHISOR (ROPE) — Benefits from franchisee network while bearing minimal operational risk. Views franchise system as coordination mechanism: franchisees handle local operations, franchisor handles brand and supply. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary with low overhead.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTI-UNIT FRANCHISEE (TANGLED ROPE) — Controls multiple locations and has some operational autonomy and bargaining power, but still constrained by franchise agreement and supply dependencies. Benefits from brand recognition and support; exploited through cost controls and pricing. d≈0.62, f(d)≈0.85, σ=1.0 → χ≈0.49.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: FRANCHISEE ASSOCIATION (PITON) — Organized collective of franchisees attempting to counterbalance franchisor power through advocacy and collective action, but increasingly ineffective as individual franchisees fear retaliation or non-renewal. Theater ratio=0.61 reflects ceremonial negotiations with limited real enforcement. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY ENFORCER (TANGLED ROPE) — State regulators (FTC, state AGs) see the franchise system as coordinating local distribution but also recognize extraction through hidden fees, supply gouging, and termination threats. Enforcement is constrained by franchisee fear of retaliation, burden of proof, and interstate complexity. d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From the vantage of economic justice and systemic extraction, the franchise model is a scaled mechanism for converting individual entrepreneurship into dependent wage-equivalence with asset risk shifted entirely to franchisees. This is pure extraction dressed as partnership. d≈0.88, f(d)≈1.35, σ=1.2 → χ≈0.92.
constraint_indexing:constraint_classification(franchisee_corporate_squeeze, snare,
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
    constraint_indexing:constraint_classification(franchisee_corporate_squeeze, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate. The franchisor extracts through multiple channels: initial franchise fee (0-40k), ongoing royalties (5-8% of gross revenue), marketing fund contributions (2-5%), and supply chain markups (often 20-40% above open-market cost). The weighted average extraction from franchisee net revenue is significant but varies by franchise system. The value reflects that some franchisors manage genuine coordination (brand value, supply efficiency) alongside extraction. Suppression (0.68): High. Franchisees face multiple barriers to exit or collective action: (1) sunk capital investment creates path dependency; (2) franchise agreements include non-compete clauses and termination-without-cause provisions; (3) fear of retaliation (non-renewal, reduced support, termination) constrains collective organizing; (4) information asymmetry (franchisor knows system-wide profitability; franchisees see only their own unit); (5) regulatory oversight is weak due to industry capture and burden-of-proof requirements. Theater ratio (0.61): Moderate-high. Franchise disclosure documents create appearance of transparency, but key profit mechanisms are obscured: supply chain markups are wrapped in 'quality control' language; required marketing spending is framed as 'brand investment'; termination clauses are presented as mutual rather than unilateral franchisor power. The performative content has increased as the industry has become more sophisticated in regulatory compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The franchisee and the franchisor experience structurally different constraints from the same agreement. For the franchisee, the constraint is a snare: exit is impossible, costs are franchisor-controlled, and extraction is inevitable. For the franchisor, the constraint is a rope: coordination benefits (scalable brand distribution, local knowledge integration) with manageable overhead. The multi-unit franchisee sees tangled rope (some power, some constraint). The franchisee association sees piton (organized, but increasingly ineffective). The regulatory enforcer sees tangled rope (trying to enforce fairness while constrained by industry capture). The analytical observer sees snare (pure extraction disguised as partnership). The perspectival gap reveals the core mandatrophy: the franchisor genuinely benefits from coordination (scaling brand requires franchisee local operations), but the franchise agreement structure allows the coordination benefit to be captured entirely by the franchisor while shifting all risk and extraction costs to franchisees.
 *
 * DIRECTIONALITY LOGIC:
 *   Corporate Franchisor: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Individual Franchisees: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction — cannot exit without catastrophic loss. Sunk capital ($250k-$2M+) and brand dependence create structural trap. Multi-Unit Franchisee: Victim + constrained (some autonomy, but contractually constrained) → d≈0.62, f(d)≈0.85. Significant extraction but not maximal; multiple units provide some bargaining power. Franchisee Association: Victim + constrained (organized, but members fear retaliation) → d≈0.55, f(d)≈0.75. Organization exists but is constrained by individual members' vulnerability to franchisor retaliation. Regulatory Enforcer: Organized + mobile (can move enforcement priority but constrained by jurisdiction and industry lobby) → d≈0.48, f(d)≈0.60. Low-moderate effective extraction from franchisor perspective; enforcement power exists but is constrained. Analytical Observer: analytical → d≈0.88, f(d)≈1.35. High extraction when viewed systemically as conversion of entrepreneurship into dependent labor.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The franchise constraint resolves the mandatrophy by decomposing two structurally distinct agreements: (1) **Genuine coordination franchise**: Franchisor provides meaningful training, operational support, supply efficiency, and marketing; franchisee provides local knowledge and customer relationships. Extraction rate <0.35 (low fees, transparent pricing, high franchisee profitability >70%). Classification: Rope. (2) **Extraction franchise**: Franchisor provides brand name and supply chain that franchisee is contractually forbidden to use elsewhere; extraction occurs through opaque supply markups, mandatory spending, and termination threats. Extraction rate >0.50 (high hidden costs, low franchisee profitability <40%). Classification: Snare or Tangled Rope depending on franchisor's level of active operational control. The current constraint story (ε=0.58) reflects the empirical distribution: most major franchise systems exhibit extraction dominance with coordination theater. The mandatrophy is resolved by recognizing that the franchise agreement is not inherently extractive — its extractiveness depends on the franchisor's pricing transparency, supply chain fairness, operational support quality, and contract symmetry. When these align with genuine coordination, the constraint approaches Rope. When they become mechanisms for hidden extraction, the constraint becomes Snare. The industry's trajectory over 30 years (0.35→0.58) shows increasing extractiveness as franchisors have learned to structure hidden profit mechanisms while maintaining compliance theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    franchise_economic_viability,
    'What fraction of franchisees achieve net profitability (excluding sunk capital amortization) after franchisor fees, supply costs, and marketing obligations?',
    'Longitudinal economic survey of franchise cohorts; franchise disclosure document (FDD) analysis against actual franchisee financial outcomes; IRS Schedule C audit correlation',
    'If <30% profitable: extraction classification (Snare) dominates. If 50-70% profitable: Tangled Rope holds (mixed coordination and extraction). If >80% profitable: Rope hypothesis confirmed (pure coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(franchise_economic_viability, empirical, 'Profitability rate of franchisees net of all franchisor-controlled costs').

omega_variable(
    franchisor_supply_markup_mechanism,
    'Does the franchisor profit primarily through franchise fees and royalties, or through supply chain markups and vendor rebates that are hidden from franchisee financial transparency?',
    'Franchisor financial disclosure (10-K SEC filings); supply contract analysis; franchisee cost audit comparing franchisor mandated suppliers vs open-market equivalents',
    'If supply profits exceed royalties: hidden extraction mechanism confirmed (Snare gate satisfied). If transparent royalty model dominates: extraction is overt but consensual (Rope-compatible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(franchisor_supply_markup_mechanism, empirical, 'Whether extraction occurs through transparent royalties or hidden supply chain markups').

omega_variable(
    exit_option_feasibility,
    'What are the actual legal and financial costs for a franchisee to exit the franchise system (lease termination, equipment resale, lost brand customer base)?',
    'Franchisee lawsuit discovery (damages calculations); franchise exit case law; franchisee cohort tracking post-termination financial recovery',
    'If exit costs >90% of invested capital: exit is effectively impossible, supporting d≈0.95 (trapped). If exit costs 40-60%: exit is constrained but possible, supporting d≈0.70 (constrained).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility, empirical, 'True economic cost of franchise exit as percentage of invested capital').

omega_variable(
    regulatory_capture_depth,
    'To what extent does the franchising industry capture regulatory bodies through lobbying, agency revolving doors, and industry-friendly legislation?',
    'Lobbying expenditure tracking (OpenSecrets, state lobbying filings); revolving-door analysis (FTC/state AG employment histories); legislative voting correlation with franchise lobby campaign funding',
    'If capture is strong: regulatory perspective (Tangled Rope) overstates enforcement capacity; actual constraint is closer to Snare. If capture is weak: regulatory capacity to enforce disclosure and fair dealing is real, supporting Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree to which franchising industry captures regulatory enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(franchisee_corporate_squeeze, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fcs_tr_t0, franchisee_corporate_squeeze, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fcs_tr_t15, franchisee_corporate_squeeze, theater_ratio, 15, 0.54).
narrative_ontology:measurement(fcs_tr_t30, franchisee_corporate_squeeze, theater_ratio, 30, 0.61).

% Extraction over time
narrative_ontology:measurement(fcs_be_t0, franchisee_corporate_squeeze, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fcs_be_t15, franchisee_corporate_squeeze, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(fcs_be_t30, franchisee_corporate_squeeze, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(franchisee_corporate_squeeze, resource_allocation).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, supply_chain_monopoly_control).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, franchisee_labor_misclassification).
narrative_ontology:affects_constraint(franchisee_corporate_squeeze, brand_value_appropriation).

% DUAL FORMULATION NOTE:
% The franchise agreement squeeze is downstream of franchisor market concentration (supply chain monopoly) and institutional asymmetry (franchisee classification as independent contractor despite operational control). The upstream constraints establish the structural preconditions for extraction; this constraint models the direct extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(franchisee_corporate_squeeze, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
