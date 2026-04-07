% ============================================================================
% CONSTRAINT STORY: us_china_tech_export_controls
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_china_tech_export_controls, []).

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
 *   constraint_id: us_china_tech_export_controls
 *   human_readable: US-China Technology Export Controls Regime
 *   domain: geopolitical_economic/trade_security
 *
 * SUMMARY:
 *   US-China technology export controls represent a comprehensive regime
 *   restricting semiconductor manufacturing equipment, advanced chip design
 *   tools, and foundry access to Chinese entities. Implemented through the
 *   Bureau of Industry and Security (BIS) Foreign Direct Product Rule, Entity
 *   Lists, and increasingly through allied coordination, the constraint
 *   operates across multiple institutional levels: bilateral US-China
 *   relations, trilateral alignment with Taiwan and South Korea, broader
 *   alliance architecture, and international commercial semiconductor
 *   markets. The regime exhibits high extractiveness (0.68) because it
 *   deliberately restricts China's access to technological capability,
 *   creating asymmetric advantage for US and allied firms. Suppression is
 *   high (0.72) due to comprehensive enforcement mechanisms, criminal
 *   penalties for violations, and international compliance requirements.
 *   Theater ratio (0.58) reflects that some enforcement activity is
 *   performative security ritual (control list designation authority,
 *   bureaucratic compliance audits) while much directly functions to exclude
 *   adversary capability development. The constraint generates the full
 *   perspectival range: pure snare from the Chinese semiconductor industry's
 *   view (no legitimate exit), tangled rope from US semiconductor companies'
 *   experience (market protection mixed with revenue loss and capital
 *   reallocation burden), rope from the defense industrial base (pure benefit
 *   with arbitrage options), and mountain-like claims about technological
 *   inevitability that obscure the institutional choice to enforce controls.
 *   The extractiveness trajectory shows acceleration from 2021-2026, driven
 *   by successive rounds of CHIPS Act, advanced node restrictions, and entity
 *   list expansions.
 *
 * KEY AGENTS:
 *   - Chinese Semiconductor Manufacturers: Primary victim (powerless/trapped) — comprehensive exclusion from advanced node design tools and manufacturing equipment; zero legitimate exit pathway
 *   - US Defense Industrial Base: Primary beneficiary (institutional/arbitrage) — receives sustained protected markets and security classification advantages; maximum arbitrage capacity
 *   - US Semiconductor Companies: Secondary beneficiary with constraints (powerful/mobile) — benefit from China market exclusion but bear reshoring costs and dual-design requirements
 *   - Allied Technology Ecosystems (TSMC, Samsung, European fabs): Mixed position (moderate/constrained) — gain security alignment and preferential access but constrained by compliance requirements and vulnerability to policy changes
 *   - Global Supply Chain Actors: Moderate position (organized/constrained) — experience scaffold logic with generational sunset as alternative technologies mature
 *   - Export Control Bureaucracy: Institutional maintainer (institutional/arbitrage) — preserves control authority through procedural legitimacy; sees own processes as performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_china_tech_export_controls, 0.68).
domain_priors:suppression_score(us_china_tech_export_controls, 0.72).
domain_priors:theater_ratio(us_china_tech_export_controls, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_china_tech_export_controls, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_china_tech_export_controls, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(us_china_tech_export_controls, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_china_tech_export_controls, tangled_rope).
narrative_ontology:human_readable(us_china_tech_export_controls, "US-China Technology Export Controls Regime").
narrative_ontology:topic_domain(us_china_tech_export_controls, "geopolitical_economic/trade_security").

domain_priors:requires_active_enforcement(us_china_tech_export_controls).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_china_tech_export_controls, us_defense_industrial_base).
narrative_ontology:constraint_beneficiary(us_china_tech_export_controls, us_semiconductor_companies).
narrative_ontology:constraint_beneficiary(us_china_tech_export_controls, allied_tech_ecosystems).
narrative_ontology:constraint_victim(us_china_tech_export_controls, chinese_semiconductor_industry).
narrative_ontology:constraint_victim(us_china_tech_export_controls, global_supply_chain_efficiency).
narrative_ontology:constraint_victim(us_china_tech_export_controls, dual_use_technology_research).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CHINESE SEMICONDUCTOR MANUFACTURERS (SNARE) — Trapped by comprehensive export restrictions on advanced chip design tools, manufacturing equipment, and process nodes. No legitimate exit pathway exists within the constraint; manufacturer access to leading-edge technology requires either circumventing controls (illegal) or accepting technological obsolescence. Maximum experienced extraction with minimal coordination benefit — the regime exists to exclude this actor from capability development.
constraint_indexing:constraint_classification(us_china_tech_export_controls, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED ECOSYSTEMS (TANGLED ROPE) — European and East Asian allies experience both coordination benefits (security alignment, preferential access to US tech) and extraction costs (restricted autonomy in tech policy, required compliance with US designation changes, vulnerability to weaponization of controls). High suppression through compliance requirements; moderate exit cost if breaking with US policy.
constraint_indexing:constraint_classification(us_china_tech_export_controls, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: US DEFENSE INDUSTRIAL BASE (ROPE) — Primary beneficiary with near-total arbitrage capacity. Receives protected market access, sustained funding, and security classification as competitive advantage. Experiences the constraint as pure coordination: controlling adversary access to advanced technology enables US military-industrial dominance. Minimal extraction because the regime's extraction runs toward this actor.
constraint_indexing:constraint_classification(us_china_tech_export_controls, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US SEMICONDUCTOR COMPANIES (TANGLED ROPE) — Powerful but constrained by controls that reduce addressable markets and force regionalization of supply chains. Benefit from China market exclusion (competition reduction) but bear costs of reduced revenue, forced reshoring investments, and dual-design requirements. Mobile exit capacity (can lobby or relocate) constrains suppression but does not eliminate extraction.
constraint_indexing:constraint_classification(us_china_tech_export_controls, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: GLOBAL SUPPLY CHAIN COALITION (SCAFFOLD) — Organized actors (semiconductor manufacturers, logistics firms, equipment vendors) perceive controls as a temporary coordination problem with a sunset clause implicit in technology evolution: advanced semiconductor design will eventually be replicated or superseded, and controls become obsolete when the technology gap narrows or new architectures emerge. Theater ratio reflects genuine coordination function (security alignment) declining over generational timescales as alternatives mature.
constraint_indexing:constraint_classification(us_china_tech_export_controls, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECHNICAL FEASIBILITY VIEW (MOUNTAIN) — From a civilizational perspective, technological progress is unidirectional: advanced semiconductor design knowledge, once demonstrated, cannot be un-known. Controls can only delay, not prevent, capability diffusion. The constraint appears as a natural law of technological development — attempting to freeze advantage is futile. However, this naturalization obscures the political economy: the actual constraint is an institutional choice to enforce controls, not a law of physics.
constraint_indexing:constraint_classification(us_china_tech_export_controls, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: EXPORT CONTROL BUREAUCRACY (PITON) — Regulatory apparatus (BIS, State Department, military-industrial coordination bodies) maintains control designation authority through institutional momentum. Theater ratio (0.58) reflects that much bureaucratic activity is performative: control list updates, compliance audits, entity designations persist through routine institutional procedures despite uncertainty about marginal effectiveness. The system maintains itself through procedural legitimacy rather than demonstrated impact.
constraint_indexing:constraint_classification(us_china_tech_export_controls, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_china_tech_export_controls_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_china_tech_export_controls, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_china_tech_export_controls, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_china_tech_export_controls, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_china_tech_export_controls, TR),
    TR >= 0.70.

:- end_tests(us_china_tech_export_controls_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime deliberately restricts China's access to leading-edge semiconductor capability, creating substantial asymmetric advantage for US and allied firms. The extraction is not total (some Chinese indigenous capability exists, circumvention mechanisms provide partial access) but is severe enough to warrant high extractiveness. The metric reflects the primary function of the constraint: advantage concentration. Suppression (0.72): High. Enforcement mechanisms include criminal penalties for violations, international compliance requirements, ongoing entity list updates, and allied coordination. However, suppression is not absolute because sophisticated smuggling networks, subsidiary design work, and gradual technology diffusion erode enforcement. Theater ratio (0.58): Moderate-High. Significant bureaucratic activity (entity designation procedures, control list curation, compliance audits) is performative — the regulatory ritual maintains authority rather than materially advancing capability exclusion. But core enforcement (equipment interdiction, foundry access restrictions) has genuine functional content. The metric reflects mixed motives: security rationale combined with bureaucratic self-preservation. Trajectory analysis (0-9 interval): Extractiveness rises from 0.45 to 0.71, reflecting successive restriction rounds. Theater ratio rises from 0.42 to 0.63, indicating expanding performative bureaucracy alongside genuine enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exemplifies how institutional control mechanisms produce radically different experienced realities for positioned agents. From the Chinese semiconductor industry's perspective, the constraint is a snare with no exit — comprehensive exclusion that forces either circumvention (illegal, risky) or technological obsolescence (economically catastrophic). From the US defense industrial base's perspective, the same constraint is rope — a coordination mechanism that concentrates technological advantage and secures supply chains. From US semiconductor companies, it's tangled rope — competition reduction benefits mixed with capital reallocation burdens. From allied ecosystems, it's tangled rope but inverted — security alignment benefits mixed with compliance burdens. From the export control bureaucracy, it's piton — procedural legitimacy maintaining institutional authority despite uncertainty about marginal effectiveness. From the civilizational analytical view, it risks appearing as mountain — 'technological progress is inevitable, controls cannot block diffusion' — but this is a false summit that naturalizes the institutional choice to enforce controls. The perspectival range reveals that the constraint is not a natural law or inevitable outcome but a deliberate extraction regime producing asymmetric advantage, sustained through enforcement, and gradually undermined by technological substitution and circumvention mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective derives from the structural position relative to the extraction flow. Chinese manufacturers: victim + trapped = maximum d (close to 1.0), producing maximum f(d) and maximum experienced χ. US defense industry: beneficiary + arbitrage = minimum d (close to 0.0), producing negative f(d) and net benefit (negative χ). US semiconductor companies: mixed beneficiary-victim + mobile = moderate d (0.4-0.6), producing moderate χ. Allied ecosystems: mixed + constrained = moderate-high d (0.55-0.70) due to constrained exit. Export control bureaucracy: beneficiary + arbitrage = low d. The pipeline derives d from these structural parameters; the sigmoid f(d) converts d to effective power modifier; χ = ε × f(d) × σ(S) computes experienced extractiveness. Scope modifiers matter: Chinese manufacturers experience the constraint at global scope (σ=1.2), amplifying χ. Allied ecosystems experience it at continental scope (σ=0.9), damping χ relative to global scale.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint exhibits both genuine coordination benefits and asymmetric extraction, satisfying the mandatrophy gates. Coordination function: security alignment, supply chain concentration, allied technology ecosystem coordination, dual-use technology risk management. These are real, positive-sum benefits for US and allied actors. Asymmetric extraction: Chinese capability exclusion, forced reshoring, reduced market access for affected actors, compliance burdens on allies. These are real, asymmetric costs. The regime's extractiveness rises over the interval (0.45→0.71) as restrictions expand, consistent with Tangled Rope degradation toward Snare. Theater ratio rises (0.42→0.63) as bureaucratic procedures expand, consistent with performative enforcement growing. The constraint is neither pure coordination (there is substantial asymmetric extraction) nor pure extraction (there is genuine security-aligned coordination). The Tangled Rope classification resolves the mandatrophy by acknowledging both functions coexist: the regime genuinely coordinates allied technology advantage while deliberately extracting from designated adversaries. Mandatrophy is resolved at high extractiveness (0.68 > 0.70 gate approached) through explicit declaration that the asymmetric extraction mechanism is intentional policy, not a byproduct of coordination failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_substitution_timeline,
    'At what point do alternative chip architectures or manufacturing techniques render current export controls technologically obsolete?',
    'Longitudinal tracking of Chinese indigenous semiconductor capability; analysis of alternative technologies (chiplet design, extreme-UV manufacturing, quantum architectures); capability gap narrowing metrics',
    'If timeline < 10 years: constraints become Scaffold (temporary). If > 20 years: constraints remain Snare for duration. If substitution is fundamentally impossible: mountain classification gains traction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technological_substitution_timeline, empirical, 'Timeline for technological substitution of controlled architectures').

omega_variable(
    transnational_smuggling_efficacy,
    'How effective are smuggling and circumvention mechanisms (shell companies, transshipment through third countries, reverse engineering) in undermining export controls?',
    'Detection rate analysis; cost-benefit modeling of enforcement vs circumvention; tracking of illicit semiconductor shipment interdictions; capability gains from circumvented materials',
    'If smuggling enables 40%+ capability access: constraints are Tangled Rope or Snare with moderate suppression. If < 10%: constraints function as intended Snare with high suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transnational_smuggling_efficacy, empirical, 'Effectiveness of transnational circumvention mechanisms').

omega_variable(
    allied_defection_risk,
    'How brittle is the allied coalition enforcing controls if China offers economic incentives for defection?',
    'Trade flow analysis; preference revelation through bilateral negotiation behavior; historical precedent of technology export violations by allied states; sanctions resistance modeling',
    'If coalition is stable: suppression remains high, constraints function as Snare for China. If brittle: constraints degrade to Piton (performative) or Rope (coordination with enforcement failures).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(allied_defection_risk, empirical, 'Stability of international coalition enforcing controls').

omega_variable(
    dual_use_misclassification_rate,
    'What fraction of controlled items have legitimate civilian applications, and are they over-restricted due to security theater?',
    'Classification accuracy audit; comparison of control scope to actual dual-use risk; economic impact assessment of over-restriction; technology transfer analysis',
    'If over-restriction > 50%: theater ratio should be higher (0.70+), classification shifts toward Piton. If misclassification < 20%: theater ratio justified, Tangled Rope confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_misclassification_rate, empirical, 'Rate of dual-use misclassification in export control lists').

omega_variable(
    semiconductor_fab_relocation_reversibility,
    'Are forced relocations of semiconductor fabrication (CHIPS Act incentives, supply chain diversification) reversible or do they create path-dependent industrial lock-in?',
    'Cost analysis of relocation reversal; geographic fixed capital durability; workforce specialization clustering; comparative advantage persistence',
    'If irreversible: constraints become mountain-adjacent (once structural change occurs, reversal becomes structurally impossible). If reversible: constraints remain Tangled Rope with mobile exit capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semiconductor_fab_relocation_reversibility, empirical, 'Reversibility of semiconductor fab geographic lock-in').

omega_variable(
    security_classification_scope_creep,
    'Is the scope of ''security-relevant'' technology expanding over time, converting controls from targeted snare into universal suppression mechanism?',
    'Entity list expansion rate; control list category proliferation; justification coherence analysis; threat definition mission creep',
    'If scope_creep is high: suppression metric should increase over measurement interval, theater_ratio rises (performative expansion). If scope stable: extracted values validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_classification_scope_creep, empirical, 'Temporal expansion of security-relevant technology definitions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_china_tech_export_controls, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usctec_tr_t0, us_china_tech_export_controls, theater_ratio, 0, 0.42).
narrative_ontology:measurement(usctec_tr_t3, us_china_tech_export_controls, theater_ratio, 3, 0.52).
narrative_ontology:measurement(usctec_tr_t6, us_china_tech_export_controls, theater_ratio, 6, 0.58).
narrative_ontology:measurement(usctec_tr_t9, us_china_tech_export_controls, theater_ratio, 9, 0.63).

% Extraction over time
narrative_ontology:measurement(usctec_be_t0, us_china_tech_export_controls, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usctec_be_t3, us_china_tech_export_controls, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(usctec_be_t6, us_china_tech_export_controls, base_extractiveness, 6, 0.68).
narrative_ontology:measurement(usctec_be_t9, us_china_tech_export_controls, base_extractiveness, 9, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_china_tech_export_controls, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_china_tech_export_controls, 0.18).
narrative_ontology:affects_constraint(us_china_tech_export_controls, semiconductor_supply_chain_concentration).
narrative_ontology:affects_constraint(us_china_tech_export_controls, us_taiwan_security_commitment).
narrative_ontology:affects_constraint(us_china_tech_export_controls, chinese_indigenous_semiconductor_development).
narrative_ontology:affects_constraint(us_china_tech_export_controls, allied_technology_autonomy).
narrative_ontology:affects_constraint(us_china_tech_export_controls, dual_use_research_restrictions).

% DUAL FORMULATION NOTE:
% US-China tech export controls form a constraint family with upstream constraints (US-Taiwan security commitment, China technological progress capability gap) and downstream constraints (semiconductor supply chain concentration, allied autonomy in tech policy). The export controls represent the institutional mechanism translating geopolitical commitment into market structure. The family spans timescales from immediate (control list updates) to civilizational (technology diffusion dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_china_tech_export_controls, powerful, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
