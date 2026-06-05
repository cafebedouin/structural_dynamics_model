% ============================================================================
% CONSTRAINT STORY: india_nuclear_liability_act_2010
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_india_nuclear_liability_act_2010, []).

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
 *   constraint_id: india_nuclear_liability_act_2010
 *   human_readable: India's Civil Liability for Nuclear Damage Act of 2010
 *   domain: economic/political
 *
 * SUMMARY:
 *   India's Civil Liability for Nuclear Damage Act of 2010 establishes a
 *   legal framework for managing liability in nuclear accidents, implementing
 *   a liability cap of ₹1,500 crore (approximately $180 million USD at 2010
 *   exchange rates). The Act channels all liability through the reactor
 *   operator and insulates foreign suppliers from direct exposure — a
 *   critical requirement of the US-India nuclear deal framework that enabled
 *   India's nuclear expansion. The constraint exhibits classic Tangled Rope
 *   characteristics: it solves a genuine coordination problem (enabling
 *   financing and insurance products for reactor operators) while
 *   simultaneously extracting from affected populations by capping
 *   compensation far below actuarial accident risk. The structure reflects a
 *   political bargain: India gains access to foreign nuclear technology and
 *   reactor suppliers gain market entry; in exchange, India's public absorbs
 *   tail-risk exposure that would otherwise be uninsurable. The theater ratio
 *   has increased from 0.35 (2010, when the Act was novel and served a real
 *   coordination function) to 0.55 (2024), reflecting growing recognition
 *   that the liability framework is performative rather than functional — the
 *   caps are maintained through legal ritual, not because they represent
 *   actual risk allocation. The constraint is downstream of India's strategic
 *   energy independence goals and upstream of specific reactor construction
 *   permits; it also affects international supply chain agreements, financing
 *   mechanisms, and public health policy.
 *
 * KEY AGENTS:
 *   - Nuclear Reactor Operators (NTPC, NPCIL): Primary institutional beneficiaries (arbitrage exit) — gain liability caps and financing certainty
 *   - Foreign Suppliers (Westinghouse, Areva, GE): Institutional beneficiaries (arbitrage exit) — insulated from liability exposure; enabled market entry
 *   - Affected Public Populations (residential/agricultural communities): Primary victims (powerless/trapped) — face radiation exposure risk with capped compensation
 *   - Cleanup Workers and Emergency Responders: Secondary victims (moderate/constrained) — bear occupational exposure with asymmetric compensation
 *   - Nuclear Regulatory Authority and Government of India: Institutional actor (organized/constrained) — experiences dual function (enables policy goal; absorbs tail risk)
 *   - Civil Society and Environmental Advocates: Organized agents (organized/constrained) — pushing for sunset through amendment or phase-out
 *   - International Nuclear Liability Standards Regime: Institutional inertia mechanism (powerful/mobile) — maintains performative compliance theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_nuclear_liability_act_2010, 0.52).
domain_priors:suppression_score(india_nuclear_liability_act_2010, 0.68).
domain_priors:theater_ratio(india_nuclear_liability_act_2010, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, extractiveness, 0.52).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(india_nuclear_liability_act_2010, tangled_rope).
narrative_ontology:human_readable(india_nuclear_liability_act_2010, "India's Civil Liability for Nuclear Damage Act of 2010").
narrative_ontology:topic_domain(india_nuclear_liability_act_2010, "economic/political").

domain_priors:requires_active_enforcement(india_nuclear_liability_act_2010).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, nuclear_reactor_operators).
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, foreign_suppliers).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, affected_public_populations).
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, cleanup_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: AFFECTED PUBLIC POPULATIONS (SNARE) — Residents in nuclear facility proximity cannot exit the radiation exposure risk. Liability caps at ₹1,500 crore (approximately $180M) limit total compensation regardless of actual damages, creating a massive gap between potential losses and recoverable amounts. For mass casualty events (similar to Fukushima or Chernobyl), per-victim compensation would be negligible. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.58.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CLEANUP WORKERS AND EMERGENCY RESPONDERS (TANGLED ROPE) — Constrained by occupational duty and limited exit options; bear direct exposure risk. The Act provides some occupational liability frameworks but compensation structures are asymmetric. They benefit from structured liability frameworks (better than no legal standing) but bear disproportionate risk. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.52.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: NUCLEAR REACTOR OPERATORS (ROPE) — Primary beneficiaries. Liability is capped and channeled through the operator, who can purchase insurance and externalize tail risks. The operator experiences the Act as coordination for standardizing liability procedures, enabling reactor construction financing and insurance products. Arbitrage comes from regulatory certainty and ability to model worst-case scenarios into capital costs. d≈0.08, f(d)≈-0.11, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOREIGN SUPPLIERS (ROPE) — The Act channels all liability through the reactor operator, insulating suppliers from direct exposure. This was a key requirement of the US-India nuclear deal framework. Suppliers experience the constraint as a coordination mechanism enabling market entry into India's nuclear sector. d≈0.10, f(d)≈-0.10, σ=1.2 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NUCLEAR REGULATORY AUTHORITY AND GOVERNMENT (TANGLED ROPE) — India's regulatory apparatus experiences dual extraction and coordination function. The Act enables nuclear energy expansion (government policy goal) while protecting the state treasury from unlimited liability. But the government is also the primary beneficiary if the nuclear expansion succeeds, and bears long-term environmental remediation costs that exceed the ₹1,500 crore cap in a major accident. Constrained by inability to exit commitment to nuclear energy expansion. d≈0.55, f(d)≈0.76, σ=1.0 → χ≈0.40.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INTERNATIONAL LIABILITY STANDARDS REGIME (PITON) — India's Act mimics the Vienna Convention on Civil Liability for Nuclear Damage and related international frameworks, but with liability caps far below actual worst-case scenarios (₹1,500 crore vs. estimated $100B+ for a major event). The international framework persists through institutional inertia — it served a function in 1960s to enable nuclear energy adoption, but its caps have not scaled with inflation or accident severity. theater_ratio=0.55 reflects that the Act maintains formal compliance theater with international norms while materially failing to cover realistic damage. d≈0.48, f(d)≈0.62, σ=1.2 → χ≈0.41.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CIVIL SOCIETY AND ENVIRONMENTAL ADVOCATES (SCAFFOLD) — Organized groups see the Act as a temporary inadequate framework driving the sunset of a particular model of nuclear expansion in India. Their strategy is to push for amendment (raising caps to actuarial levels) or to phase out reactor types with highest accident risk. The constraint has a structural sunset: as climate pressure mounts, renewable energy alternatives mature, and the political cost of uncompensated accident risk becomes visible (social media, litigation), either the caps must rise or the expansion stalls. d≈0.65, f(d)≈1.01, σ=1.0 → χ≈0.52. Sunset rationale: technological and policy alternatives mature over 15-25 years, making the liability constraint's extraction mechanism obsolete.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN TRAP) — The analyst might argue that nuclear liability caps are inherent to how energy systems work: you cannot insure black-swan catastrophes, caps are therefore a physical/economic law. However, the base properties (ε=0.52, suppression=0.68, theater=0.55) reveal this as a false summit. The structure is a contingent institutional choice (other countries use unlimited liability, others use public compensation funds), not a law of nature. The engine detects this as a false summit through the mandatrophy gate.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_nuclear_liability_act_2010_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(india_nuclear_liability_act_2010, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(india_nuclear_liability_act_2010, TR),
    TR >= 0.70.

:- end_tests(india_nuclear_liability_act_2010_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The liability cap at ₹1,500 crore represents a massive gap between realistic accident costs (estimated $50B-$200B for a major event) and recoverable compensation. For a catastrophic event, per-victim compensation would be $1,000-$10,000 — orders of magnitude below actual medical, resettlement, and agricultural losses. The cap extracts by capping the operator's (and ultimately the public's) maximum obligation. However, extractiveness is not 0.85 (snare level) because: (1) the cap applies equally to all parties and was ratified by Parliament, not imposed unilaterally; (2) operator profit margins are constrained by cost of capital for insuring their ₹1,500 crore exposure; (3) alternative energy sources exist (renewable, coal), creating some countervailing power. Suppression (0.68): High. Affected populations cannot exit proximity to nuclear facilities. The political-legal apparatus suppresses alternatives by channeling liability through a single operator, preventing individual claims against suppliers or the state. Information asymmetry is high — most affected populations lack understanding of actual accident risks. Theater ratio (0.55): Moderate-rising. The Act maintains compliance with international norms (Vienna Convention language), but the actual risk allocation is theater: the liability cap is maintained through legal ritual, not because it reflects actuarial reality. The ratio rose over the interval as the political discussion matured and the inadequacy became more visible.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental perspectival gap is between the beneficiary's coordination experience and the victim's extraction experience. The reactor operator experiences the Act as coordination for financing: the cap enables insurance products, debt structuring, and capital deployment. The beneficiary sees a legitimate market mechanism. The affected public experiences pure extraction: a cap on their recoverable losses that bears no relationship to actual harm. The civil society coalition sees a temporary inadequacy driving systemic change — their perspective is that the constraint has a sunset. The regulatory authority experiences mixed extraction and coordination: the Act enables their policy goal (nuclear expansion) while shifting tail-risk to the public treasury and affected populations. The international standards regime sees itself as maintaining neutral liability frameworks, but masks asymmetric outcomes by maintaining formal equivalence with countries that use different (often more protective) liability structures.
 *
 * DIRECTIONALITY LOGIC:
 *   Reactor operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. Foreign suppliers: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.10. Net beneficiary. Affected populations: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction — cannot exit, no countervailing power. Cleanup workers: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction but not maximal — occupational duty required but some exit paths exist (career changes). Regulatory authority: Mixed (both beneficiary via policy goal and victim via long-term state liability) + constrained → d≈0.55, f(d)≈0.76. Moderate extraction. Civil society: Organized + constrained → d≈0.65, f(d)≈1.01. Moderate-high extraction but coalition has agency and sees sunset path. International regime: Institutional + mobile → d≈0.48, f(d)≈0.62. Moderate extraction; theater-based persistence.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by showing that the Tangled Rope classification correctly captures the hybrid structure. This is NOT a pure extraction mechanism (Snare) because: (1) there is a genuine coordination function (enabling financing and international technology transfer); (2) the beneficiaries are constrained by their own exposure (operators still face ₹1,500 crore in losses, which is material); (3) the cap was ratified through democratic processes, not imposed by force. This is NOT pure coordination (Rope) because: (1) the public bears asymmetric tail-risk exposure (affected populations have no voice in the bargain); (2) the beneficiaries (operators and suppliers) structured the deal specifically to externalize risk; (3) suppression is high (can't exit, low information, one-sided contract terms). The Tangled Rope classification prevents false mislabeling of the constraint as either pure coordination (which would miss the extraction from affected populations) or pure extraction (which would miss that reactor operators genuinely face ₹1,500 crore in liability and that financing genuinely requires the cap). The scaffold perspective (civil society see a sunset) is structural: as renewable energy costs decline and political pressure mounts, the extraction mechanism becomes unsustainable. The piton perspective (international standards maintain theater) reveals the constraint's institutional persistence mechanism: formal compliance with Vienna Convention language masks the asymmetric outcomes. The false mountain perspective (analyst naturalizes caps as inherent) is caught by the mandatrophy gate — caps are not laws of nature but contingent political choices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    actual_accident_severity,
    'What is the true probability distribution of nuclear accident costs in Indian geography and reactor fleet composition?',
    'Probabilistic risk assessment calibrated to Indian seismic zones, cooling water availability, and specific reactor designs; comparison with Fukushima/Chernobyl scenarios adapted to Indian infrastructure',
    'If P(cost > ₹1,500 crore) > 0.5: liability cap is structurally unsustainable. If < 0.1: cap may be adequate and the constraint is primarily coordination. Actual estimates suggest >70% probability for major accidents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(actual_accident_severity, empirical, 'True distribution of nuclear accident costs in Indian context').

omega_variable(
    supplier_responsibility_shift,
    'Will international pressure or trade agreements force supplier liability re-exposure, fundamentally restructuring the constraint?',
    'Monitoring US-India nuclear deal language, EU liability requirements, WTO dispute history; tracking supplier warranty clauses and recall patterns in nuclear sector',
    'If suppliers re-accept liability: beneficiary position inverts, extraction mechanism evaporates, constraint becomes Rope. If status quo persists: beneficiary protection is durable, constraint remains Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supplier_responsibility_shift, conceptual, 'Whether international pressure will shift liability back to suppliers').

omega_variable(
    renewable_energy_competitiveness,
    'At what levelized cost of energy (LCOE) does Indian renewable deployment make the liability constraint politically unsustainable?',
    'Tracking solar/wind LCOE vs. nuclear capital and operating costs; monitoring parliamentary debates on nuclear expansion; analyzing policy pivot toward renewables in 5-year plans',
    'If renewables reach cost parity within 10 years: scaffold sunset becomes inevitable, constraint shifts toward Piton (theater maintenance of obsolete regime). If nuclear remains cost-competitive: constraint persists as Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(renewable_energy_competitiveness, empirical, 'Energy cost competitiveness timeline for renewable alternatives').

omega_variable(
    accident_litigation_precedent,
    'Will a significant accident (or near-miss litigation) force reinterpretation of liability caps as unconstitutional or unconscionable?',
    'Monitoring Indian Supreme Court doctrine on fundamental rights to safe environment; tracking analogous liability cap challenges in other jurisdictions; analyzing victim compensation fund adequacy post-incident',
    'If yes: constraint collapses through legal challenge, becomes Piton (theater of a struck-down law). If no: constraint persists in force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accident_litigation_precedent, preference, 'Constitutional or legal challenge to liability caps').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(india_nuclear_liability_act_2010, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indnuc_tr_t0, india_nuclear_liability_act_2010, theater_ratio, 0, 0.35).
narrative_ontology:measurement(indnuc_tr_t5, india_nuclear_liability_act_2010, theater_ratio, 5, 0.48).
narrative_ontology:measurement(indnuc_tr_t10, india_nuclear_liability_act_2010, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(indnuc_be_t0, india_nuclear_liability_act_2010, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indnuc_be_t5, india_nuclear_liability_act_2010, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(indnuc_be_t10, india_nuclear_liability_act_2010, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(india_nuclear_liability_act_2010, enforcement_mechanism).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, us_india_nuclear_deal).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, indian_energy_security_goal).
narrative_ontology:affects_constraint(india_nuclear_liability_act_2010, foreign_direct_investment_regulations_india).

% DUAL FORMULATION NOTE:
% This constraint is downstream of India's strategic energy independence goals and the US-India nuclear deal framework. The liability cap structure is upstream of specific reactor permits and financing arrangements. Decomposed from the broader 'India nuclear energy policy' because the liability mechanism has distinct structural properties: it is primarily a transfer mechanism (from public to operators/suppliers) rather than a capacity-building or technology-access constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(india_nuclear_liability_act_2010, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
