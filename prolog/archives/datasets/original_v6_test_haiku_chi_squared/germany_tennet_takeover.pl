% ============================================================================
% CONSTRAINT STORY: germany_tennet_takeover
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_germany_tennet_takeover, []).

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
 *   constraint_id: germany_tennet_takeover
 *   human_readable: German Government Stake in TenneT Germany
 *   domain: economic/political
 *
 * SUMMARY:
 *   The German government's acquisition of a controlling stake in TenneT
 *   Germany represents a critical case study in the mandatrophy between
 *   coordination and extraction in energy infrastructure. TenneT Germany
 *   operates approximately 40% of Germany's high-voltage electricity
 *   transmission network and is essential to managing the renewable energy
 *   transition by coordinating distributed wind and solar generation with
 *   grid demand. The government justified the acquisition primarily on energy
 *   security grounds — preventing foreign (particularly Dutch, given TenneT's
 *   parent company Kema Holding) or other external actors from controlling
 *   critical infrastructure during a period of energy vulnerability. However,
 *   the transaction exhibits simultaneous coordination and extraction
 *   properties: the government claims to solve the collective action problem
 *   of grid reliability during the energy transition (coordination) while
 *   simultaneously excluding competing bidders and enabling cost-shifting to
 *   ratepayers through political mechanisms rather than market mechanisms
 *   (extraction). The constraint's theater ratio (0.62) reflects the gap
 *   between the official security narrative and the actual mechanism: much of
 *   the 'energy security' argument is performed through rhetoric rather than
 *   demonstrated through operational improvements, while ratepayer costs rise
 *   under the banner of national security.
 *
 * KEY AGENTS:
 *   - German Government / Federal Ministry for Economics: Primary beneficiary (institutional/arbitrage) — gains strategic control over grid, prevents foreign ownership, coordinates renewable integration, extracts via ratepayer cost-shifting
 *   - Grid Ratepayers (household and small business consumers): Primary victim (powerless/trapped) — bear transmission costs under energy security rationale with no exit option; costs may rise due to government investment financing
 *   - Competing Bidders (foreign pension funds, Canadian, Norwegian, Australian infrastructure investors): Secondary victim (organized/constrained) — excluded from infrastructure ownership through state pre-emption; face sunk costs and reputational damage; can exit but face barriers
 *   - Industrial Energy Consumers: Mixed actor (moderate/constrained) — benefit from coordinated grid planning and renewable integration; constrained by transmission costs and government industrial policy prioritization
 *   - EU Competition Authorities: Mediating actor (organized/constrained) — treat state ownership as temporary measure justified by energy transition; constrained by member state sovereignty but enforce competition rules
 *   - Private Grid Operator Precedent: Historical actor — previous private operation (though constrained by regulation) provides counterfactual for comparison; performance metrics enable assessment of whether state ownership improves coordination or merely redistributes extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(germany_tennet_takeover, 0.38).
domain_priors:suppression_score(germany_tennet_takeover, 0.48).
domain_priors:theater_ratio(germany_tennet_takeover, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(germany_tennet_takeover, extractiveness, 0.38).
narrative_ontology:constraint_metric(germany_tennet_takeover, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(germany_tennet_takeover, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(germany_tennet_takeover, tangled_rope).
narrative_ontology:human_readable(germany_tennet_takeover, "German Government Stake in TenneT Germany").
narrative_ontology:topic_domain(germany_tennet_takeover, "economic/political").

domain_priors:requires_active_enforcement(germany_tennet_takeover).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, german_government).
narrative_ontology:constraint_beneficiary(germany_tennet_takeover, energy_security_state).
narrative_ontology:constraint_victim(germany_tennet_takeover, competing_bidders).
narrative_ontology:constraint_victim(germany_tennet_takeover, grid_ratepayers).
narrative_ontology:constraint_victim(germany_tennet_takeover, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRID RATEPAYER (SNARE) — Trapped in the German electricity market with no exit from paying transmission fees. Government ownership legitimizes cost-shifting to ratepayers under national security rationale. Cannot exit or organize effective resistance. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING BIDDERS (SNARE) — Foreign investment consortia (including CalPERS, Ontario Teachers' Pension Plan) excluded from ownership of critical infrastructure through state pre-emption. Can exit by divesting but face reputational and legal barriers. Strategic extraction via exclusion. d≈0.85, f(d)≈1.20, σ=1.1 → χ≈0.50.
constraint_indexing:constraint_classification(germany_tennet_takeover, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INDUSTRIAL ENERGY CONSUMERS (TANGLED ROPE) — Benefit from grid reliability and security of supply (coordination function) but constrained by transmission costs and government industrial policy. Government ownership coordinates grid investment but enables cost allocation asymmetries. d≈0.62, f(d)≈0.82, σ=1.0 → χ≈0.31.
constraint_indexing:constraint_classification(germany_tennet_takeover, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: GERMAN GOVERNMENT (ROPE) — Benefits from grid control, energy security coordination, and prevention of foreign ownership leveraging. Experiences the constraint as a coordination mechanism: owning TenneT enables synchronized renewable integration and decarbonization planning. d≈0.12, f(d)≈0.08, σ=1.0 → χ≈0.03. Net institutional beneficiary.
constraint_indexing:constraint_classification(germany_tennet_takeover, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: EU COMPETITION AUTHORITIES (SCAFFOLD) — Treat government ownership as temporary coordination mechanism justified by energy transition urgency, with sunset logic contingent on completion of green infrastructure. Constrained by member state sovereignty but organized through Brussels rule-making. d≈0.48, f(d)≈0.62, σ=1.1 → χ≈0.26. Theater ratio in EU compliance: 0.58 (significant but declining as rules clarify).
constraint_indexing:constraint_classification(germany_tennet_takeover, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: MARKET LIBERAL IDEOLOGY (PITON) — Neoliberal orthodoxy that grid privatization maximizes efficiency persists as intellectual framework despite contradictory evidence (infrastructure underinvestment, profit-driven cost-cutting). Government ownership is theoretically anathema but practically necessary; the market narrative is maintained performatively while state control expands. theater_ratio=0.62 reflects the contradiction. d≈0.08, f(d)≈0.02, σ=1.0 → χ≈0.01.
constraint_indexing:constraint_classification(germany_tennet_takeover, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational view, some degree of public control over energy infrastructure is immutable given thermodynamic/resource constraints: centralized grids require coordinated dispatch, and the coordination problem has inherent commons properties that market mechanisms cannot resolve without state enforcement. However, the structural data (ε=0.38, suppression=0.48, theater=0.62) contradicts mountain classification — the engine will identify this as a false summit, revealing that 'infrastructure is natural monopoly' naturalizes what is contingent policy design.
constraint_indexing:constraint_classification(germany_tennet_takeover, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(germany_tennet_takeover_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(germany_tennet_takeover, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(germany_tennet_takeover, TR),
    TR >= 0.70.

:- end_tests(germany_tennet_takeover_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The government acquisition extracts through multiple mechanisms: (1) foreign bidder exclusion via state pre-emption (strategic rent); (2) ratepayer cost-shifting — government ownership enables political mechanisms to raise transmission fees justified by infrastructure security (0.22 → 0.38 over interval); (3) industrial favoritism through differential grid access policies. However, extractiveness is not high (not >0.46) because the government genuinely coordinates renewable integration, which is a legitimate public good that would require significant state involvement regardless. The extraction is real but layered atop genuine coordination. Suppression (0.48): Moderate. Significant barriers prevent market alternatives: foreign ownership is legislatively prohibited; ratepayers have no exit from the grid; industrial consumers have limited negotiating power. However, suppression is not total — German regulatory framework remains relatively transparent, EU competition oversight exists, and public discourse is active. Theater ratio (0.62): Moderate-high. The 'energy security' framing performs much of the justification work without demonstrated operational improvement. Previous private operator performance was acceptable; government promises of superior coordination are aspirational rather than proven. However, theater is not very high (not >0.70) because the grid coordination problem is genuinely complex and some performative elements (security certifications, resilience planning) have real substance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits sharp perspectival gaps reflecting the fundamental tension between state control justified as coordination versus state control that enables extraction. The grid ratepayer sees pure extraction (Snare) — they are trapped in the payment system with no market exit and face costs rising under security pretexts. The competing bidders see strategic extraction (Snare) — they are deliberately excluded through sovereign power, not market competition. The German government sees pure coordination (Rope) — managing renewable integration and grid stability are genuine public goods that benefit all parties including ratepayers. Industrial consumers see hybrid (Tangled Rope) — they benefit from grid coordination but are constrained by cost allocation and prioritization policies. The EU observer sees a temporary scaffold — state ownership is justified by energy transition urgency with assumed sunset. The market ideology observer sees a degraded piton — neoliberal orthodoxy persists in rhetoric while state control expands. The civilizational observer risks seeing a natural law (Mountain) — public control over energy infrastructure is immutable — but the structural data reveals this as a false summit: the coordination actually required is less total control than political control enables.
 *
 * DIRECTIONALITY LOGIC:
 *   German government: Beneficiary + arbitrage → d≈0.12, f(d)≈0.08. Net institutional beneficiary; has capacity to exit (could sell stake) but chooses strategic control. Low directionality because beneficiary status is clear and exit is available to the institution. Grid ratepayers: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction directionality; trapped in payment system with no feasible exit; cannot organize effective resistance to cost increases justified by security. Competing bidders: Victim + constrained → d≈0.85, f(d)≈1.20. High extraction directionality; excluded by sovereign action; can exit by divesting but face transaction costs and reputational barriers. Industrial consumers: Mixed (beneficiary from coordination + victim from cost allocation) + constrained → d≈0.62, f(d)≈0.82. Moderate extraction; benefit from grid coordination but constrained by political cost allocation. EU authorities: Neutral observer with organized power + constrained exit → d≈0.48, f(d)≈0.62. Moderate directionality reflecting their mediating role — not primary beneficiary or victim, but constrained by sovereignty principles.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The tangled_rope classification resolves the tension by acknowledging that BOTH coordination and extraction are genuine structural features. The government's claim is partially true: renewable grid coordination does require integrated planning and investment that a fragmented market cannot provide. The extraction is also true: ratepayers bear costs through political mechanisms that offer no market exit or feedback, and competing bidders are deliberately excluded through sovereign power. The mandatrophy emerges from treating this as a binary choice (is it coordination or extraction?) when the structure exhibits both simultaneously with different distributions across agents. The grid ratepayer perspective (Snare) shows the extraction most clearly. The government perspective (Rope) shows the coordination most clearly. Neither is false. The tangled_rope classification allows both to be true: there is a genuine coordination function (renewable integration, grid stability) AND there is genuine asymmetric extraction (ratepayer cost-shifting, bidder exclusion). The theater ratio (0.62) reveals where the mandatrophy is performed most heavily: in the 'energy security' rhetoric that justifies extraction as necessity. The omegas — particularly the ratepayer cost incidence and competitive bidder alternatives — are the data points that would resolve whether the coordination genuinely requires this level of extraction or whether extraction is concealed within coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    energy_security_threshold,
    'What constitutes genuine energy security risk versus political justification for state ownership expansion?',
    'Comparative analysis of grid vulnerability to foreign ownership vs. vulnerability to political directive capture; historical cases of foreign infrastructure ownership in EU (UK water companies, French energy); stress-test scenarios for coordinated foreign divestment.',
    'If genuine security risk: government ownership is primarily coordinative (Rope from government perspective). If political justification: ownership is primarily extractive (Snare from bidder perspective). Classification gap indicates unresolved ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_security_threshold, empirical, 'Whether energy security risk is genuine or politically inflated').

omega_variable(
    ratepayer_cost_incidence,
    'Will government ownership result in lower, equivalent, or higher transmission costs for end consumers compared to private ownership?',
    'Cost modeling comparison: government investment cost of capital vs. private cost of capital; principal-agent analysis of ratepayer billing under state vs. private operator; international comparison (Denmark, France, Austria public ownership models).',
    'If costs lower or equivalent: coordination narrative holds (Rope/Tangled Rope). If costs higher: extraction is confirmed (Snare from ratepayer perspective, Tangled Rope is correct classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ratepayer_cost_incidence, empirical, 'Whether government ownership reduces or increases ratepayer costs').

omega_variable(
    sunset_exit_condition,
    'Does the government stake include a explicit or implicit sunset clause tied to renewable transition completion, or is it indefinite state control?',
    'Examination of government acquisition agreement, legislative language, and public commitments; timeline assessment for 80% renewable grid operation; analysis of political feasibility of privatization after state build-up.',
    'If sunset is real and credible: Scaffold classification holds with genuine coordination function. If indefinite: constraint is Tangled Rope with no exit path (victims trapped), bordering on Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_exit_condition, empirical, 'Whether government stake has a real or indefinite sunset clause').

omega_variable(
    competitive_bidder_alternatives,
    'Could the foreign bidders have operated TenneT as effectively or more effectively than the government, or does state ownership genuinely improve grid coordination?',
    'Comparative governance analysis: operational metrics (uptime, cost efficiency, investment speed) for TenneT under previous operator vs. peers; investor proposals vs. government plan; interviews with operators in comparable markets (UK, France, Netherlands).',
    'If bidders were competent: exclusion is pure extraction (Snare). If government superior: exclusion serves coordination (Rope/Tangled Rope). Gap between rhetoric and governance indicates mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_bidder_alternatives, empirical, 'Whether state ownership operationally improves on private alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(germany_tennet_takeover, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tennet_tr_t0, germany_tennet_takeover, theater_ratio, 0, 0.35).
narrative_ontology:measurement(tennet_tr_t2, germany_tennet_takeover, theater_ratio, 2, 0.5).
narrative_ontology:measurement(tennet_tr_t5, germany_tennet_takeover, theater_ratio, 5, 0.62).

% Extraction over time
narrative_ontology:measurement(tennet_be_t0, germany_tennet_takeover, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(tennet_be_t2, germany_tennet_takeover, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(tennet_be_t5, germany_tennet_takeover, base_extractiveness, 5, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(germany_tennet_takeover, global_infrastructure).
narrative_ontology:affects_constraint(germany_tennet_takeover, eu_energy_security_policy).
narrative_ontology:affects_constraint(germany_tennet_takeover, renewable_transition_grid_coordination).
narrative_ontology:affects_constraint(germany_tennet_takeover, foreign_critical_infrastructure_acquisition).

% DUAL FORMULATION NOTE:
% German TenneT takeover decomposes into three structural constraints: (1) the technical grid coordination problem (renewable integration) — primarily Rope, low ε; (2) the strategic infrastructure exclusion mechanism (preventing foreign ownership) — primarily Snare, moderate ε; (3) the ratepayer cost allocation under security justification — primarily Tangled Rope, moderate ε. This story treats the aggregate as tangled_rope (ε=0.38); sibling stories at higher technical resolution could decompose coordination and extraction components separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(germany_tennet_takeover, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
