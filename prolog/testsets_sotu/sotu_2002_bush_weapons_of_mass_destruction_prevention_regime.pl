% ============================================================================
% CONSTRAINT STORY: sotu_2002_bush_weapons_of_mass_destruction_prevention_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, []).

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
 *   constraint_id: sotu_2002_bush_weapons_of_mass_destruction_prevention_regime
 *   human_readable: International WMD Prevention Coordination Regime (SOTU 2002)
 *   domain: regulatory/security/geopolitics
 *
 * SUMMARY:
 *   The international WMD prevention regime, articulated in the 2002 State of
 *   the Union address, represents a multilateral institutional mechanism
 *   designed to coordinate enforcement of nonproliferation norms across
 *   advanced industrial states and their security allies. The constraint
 *   establishes shared obligations to deny WMD-related materials,
 *   technologies, and expertise to states and nonstate actors designated as
 *   hostile threats. Structurally, the regime demonstrates the core feature
 *   of Tangled Rope: it provides genuine coordination benefits
 *   (burden-sharing on intelligence, aligned strategic deterrence,
 *   standardized export controls) while embedding asymmetric extraction
 *   (sanctioned regimes face total embargo; allied states retain technology
 *   access; beneficiary states capture strategic advantage from technology
 *   freeze). The constraint exhibits rising theater over time (0.35 → 0.55)
 *   as UN inspection rituals become increasingly disconnected from actual
 *   verification capacity, and rising extractiveness (0.38 → 0.58) as
 *   enforcement mechanisms expand beyond original WMD mandate into broader
 *   geopolitical surveillance and economic control. The regime's legitimacy
 *   depends on performing universal nonproliferation commitment while
 *   selectively enforcing against adversaries and exempting allies — a
 *   structural contradiction that generates the piton perspective (degraded
 *   institutional form maintained through inertia).
 *
 * KEY AGENTS:
 *   - Great Power Coalition (US, UK, France, Russia, China as veto holders): Institutional beneficiaries (arbitrage exit) — establish enforcement norms that freeze technological advantage in their favor
 *   - Targeted Hostile Regimes (Iraq, Iran, DPRK, Syria): Primary victims (powerless/trapped) — face comprehensive embargo with no legitimate exit; bear existential security costs
 *   - Dual-Use Technology Exporters (advanced industrial firms in coalition states): Moderate victims (constrained exit) — benefit from eliminated competition but constrained by export control liability
 *   - Scientific Expertise Communities (physicists, chemists, engineers): Organized victims (constrained exit) — regulated by classification restrictions and monitoring; benefit from enhanced security-clearance access
 *   - Verification Bodies (IAEA, UN inspectors, export control agencies): Institutional actors with ambiguous position — maintain performative function while actual verification capacity is limited
 *   - Emerging Verification Technology Sector: Powerful beneficiaries (mobile exit) — develop advanced nonproliferation technology; see regime as temporary scaffold
 *   - Nonstate Actors (terrorists, criminal networks): Implicit targets — regime addresses state-level proliferation but has limited effect on nonstate acquisition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, 0.58).
domain_priors:suppression_score(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, 0.68).
domain_priors:theater_ratio(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, tangled_rope).
narrative_ontology:human_readable(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, "International WMD Prevention Coordination Regime (SOTU 2002)").
narrative_ontology:topic_domain(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, "regulatory/security/geopolitics").

domain_priors:requires_active_enforcement(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, security_conscious_great_powers).
narrative_ontology:constraint_beneficiary(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, advanced_industrial_states).
narrative_ontology:constraint_victim(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, targeted_hostile_regimes).
narrative_ontology:constraint_victim(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, dual_use_technology_exporters).
narrative_ontology:constraint_victim(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, scientific_expertise_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGETED HOSTILE REGIME (SNARE) — Faces comprehensive embargo on materials, technology, and expertise with no legitimate exit path. The regime cannot access the international technology ecosystem without abandonment of strategic objectives. Suppression is maximum: economic isolation, diplomatic exclusion, military threat. No coordination benefit — pure extraction with existential stakes. Trapped by the constraint structure itself; exit requires regime change or capitulation.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DUAL-USE TECHNOLOGY EXPORT SECTOR (TANGLED ROPE) — Benefits from expanded control authority (eliminates low-margin competitors in sanctioned countries) while bearing costs of compliance infrastructure, inspection regimes, and restricted export markets. Constrained by legal export control liability and career/corporate risk. Genuine coordination function exists: the regime prevents proliferation arms races and establishes shared standards. But asymmetric extraction embedded: profitable dual-use exporters to allied states retain market access while competitors face legal exposure.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCIENTIFIC EXPERTISE COMMUNITY (TANGLED ROPE) — Organized by discipline and nation. Benefits from enhanced funding for dual-use research oversight, security clearances that enable access to classified research, and international collaboration frameworks that exclude competitors from sanctioned states. Constrained by publish-or-perish norms conflicting with classification restrictions, export control liability for training foreign scientists, and monitoring of international conferences. Genuine coordination: preventing knowledge transfer to hostile actors. Asymmetric extraction: knowledge flows remain open among allied states while restricted for adversaries.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: GREAT POWER COALITION (ROPE) — Institutional actors with arbitrage exit (can defect from enforcement, shift enforcement leverage, renegotiate terms). Experience the constraint as coordination: establishes shared enforcement obligations, burden-sharing on intelligence and interdiction, aligned strategic objectives. Net beneficiaries — the coalition establishes nonproliferation norms that freeze technological gaps in their favor. Effective extraction runs toward non-coalition states, not toward coalition members.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: EMERGING VERIFICATION TECHNOLOGY SECTOR (SCAFFOLD) — Powerful agents (inspection bodies, export verification firms, nonproliferation research institutions) see the regime as a temporary scaffolding enabling development of advanced verification technologies (remote sensing, supply-chain tracking, biological agent detection). Mobile exit: can pivot to civilian applications or exit markets entirely. Theater ratio is moderate (0.55) because inspection rituals generate compliance theater, but underlying verification capacity is genuinely functional and improving. Sunset logic: as verification technology matures and proliferation risk shifts toward non-state actors and biological agents, the state-centric embargo regime will require redesign.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UN SECURITY COUNCIL / INTERNATIONAL TREATY BODIES (PITON) — Institutional frameworks (NPT, IAEA, CWC, BWC) that originally functioned to coordinate nonproliferation now operate with substantial performative content: inspections theater, toothless enforcement votes vetoed by permanent members, treaty obligations violated without consequence (Iraq, Iran, DPRK). Theater ratio is embedded in the institutional design. The bodies persist through inertia and legitimacy franchise, not functional verification. Arbitrage: powerful states negotiate selective enforcement (e.g., allied nuclear programs uncontested, adversary programs condemned).
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / DIFFUSION-OF-WEAPONS VIEW (MOUNTAIN) — Civilizational timescale viewing WMD prevention as an immutable structural constraint on international order. Once certain technologies exist and knowledge is codified, no regime can prevent their eventual diffusion through time. The constraint's apparent success (fewer state WMD programs) is treated as a natural law of material scarcity or technical difficulty. However, the structural data reveals this as a false summit: the apparent success reflects enforcement capacity and coalition enforcement will (contingent geopolitical factors), not physical/logical impossibility. Counter-example: Iran and DPRK developed programs despite decades of embargo, showing the constraint's extractive mechanism depends on enforcement not law.
constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, TR),
    TR >= 0.70.

:- end_tests(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regime extracts significant costs from targeted regimes (comprehensive economic isolation, technology denial, military threat) while benefiting coalition members. The extractiveness value reflects that the constraint is not maximum (Snare-level extraction would be 0.70+) because some dual-use technology still leaks through secondary markets and allied states negotiate selective exemptions. The coalition's enforcement is imperfect and selective. Suppression (0.68): High. Sanctions regimes create comprehensive barriers to material acquisition, expertise transfer, and market access. Scientific travel is monitored; supply chains are tracked; export licenses are required. However, suppression is not maximum (0.85+) because secondary markets, neutral countries, and insider defection provide partial circumvention channels. The A.Q. Khan network demonstrates that expert networks can route around formal embargoes. Theater ratio (0.55): Moderate-high. UN inspections (IAEA, UNMOVIC in Iraq) perform verification theater: site visits, document reviews, inspector briefings to Security Council. The theater increases over time as inspection effectiveness declines relative to concealment sophistication. Post-2003, verification theater becomes primary function as Iraq War demonstrates that actual WMD status was disputed despite years of inspections.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence from a unified structural phenomenon. The targeted regime sees comprehensive extraction (Snare): no legitimate path to technology access, economic isolation, existential military threat. The great power coalition sees coordination (Rope): burden-sharing on enforcement, aligned strategic objectives, technology freeze that preserves their advantage. The dual-use export sector sees mixed constraints (Tangled Rope): reduced competition from sanctioned-state exporters but increased compliance costs and liability exposure. The scientific community sees organizational constraints (Tangled Rope): enhanced security funding and international collaboration frameworks offset by classification restrictions and travel monitoring. The verification bodies see performative functionality (Piton): inspection rituals maintain legitimacy but actual verification capacity is limited. The emerging verification technology sector sees temporary scaffolding (Scaffold): regime creates demand for advanced detection and monitoring systems; sunset logic applies as technology matures and threat shifts to nonstate actors. The analytical observer risks treating the constraint as an immutable law (Mountain): WMD proliferation is an inherent structural feature of international anarchy — but the structural data reveals contingent geopolitical enforcement, not physical inevitability.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality derivation chain maps agent structural position to experienced extractiveness. Targeted regimes (powerless/trapped) experience d ≈ 0.92 (full extraction target): no exit options, maximum suppression. Coalition member states (institutional/arbitrage) experience d ≈ 0.08 (full beneficiary): can exit enforcement selectively, capture technology advantage. Dual-use exporters (moderate/constrained) experience d ≈ 0.65 (mixed): benefits from eliminated competition but constrained by liability. Scientists (organized/constrained) experience d ≈ 0.58 (moderate): benefits from security clearance access but constrained by publication and travel restrictions. Verification bodies (institutional/arbitrage) experience d ≈ 0.50 (symmetric): genuine coordination function but also theater maintenance. The perspectival gaps emerge from these differentiated d values: high-d agents (targeted regimes) see maximum extraction (Snare); low-d agents (coalition members) see coordination (Rope); moderate-d agents see mixed dynamics (Tangled Rope).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's classification depends critically on the observer's relationship to enforcement and benefit flow. The Tangled Rope classification at the claimed_type level reflects the analytical consensus: the regime has genuine coordination function (burden-sharing, aligned deterrence) AND asymmetric extraction (technology advantage frozen in coalition favor, sanctions applied selectively to adversaries not allies). The Snare perspective from targeted regimes is their structural reality — they face maximum extraction with no coordination benefit. The Rope perspective from great powers is their structural reality — they capture coordination benefits with minimal extraction cost. The Scaffold perspective from verification technology sector is their structural reality — temporary regime creating demand for advancing capability. The Piton perspective from UN bodies is their structural reality — performative inspection rituals maintaining institutional legitimacy. The Mountain perspective from civilizational analytical view is a false summit: WMD proliferation appears immutable from a distance, but the constraint's apparent success reflects enforcement capacity and geopolitical will, not physical law. The mandatrophy resolves not by choosing one type but by recognizing that all six readings are structurally valid from their respective positions within the constraint's operational topology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_vs_pretext_ambiguity,
    'Does the verification regime genuinely prevent WMD development, or does it primarily provide pretext for regime change targeting hostile powers while exempting allied ones?',
    'Comparative analysis: verification outcomes for allied states with WMD programs (Israel, France, UK, Pakistan, India) vs. sanctioned states (Iraq 1991-2003, Iran, DPRK). If applied evenly, verification constraint is genuine. If systematically exempting allies, verification is theater masking geopolitical extraction.',
    'If genuine: Tangled Rope classification sustained. If pretext: Snare classification more accurate for all perspectives — the regime extracts strategic advantage for coalition through selective enforcement of claimed nonproliferation norm.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(verification_vs_pretext_ambiguity, empirical, 'Whether verification regime is functional or theater for regime change targeting').

omega_variable(
    dual_use_technology_leakage_rate,
    'What fraction of dual-use materials and expertise actually reaches sanctioned regimes despite embargo, and through what channels?',
    'Intelligence community assessment; interdiction success rates; reverse-engineering analysis of sanctioned regimes'' procurement networks (e.g., A.Q. Khan network, Iranian procurement front companies). Higher leakage indicates suppression is overstated.',
    'If leakage > 40%: suppression metric should be downgraded to ~0.45 (medium-high); constraint reclassifies toward Tangled Rope from multiple perspectives. If leakage < 10%: suppression confirmed; Snare classification for targeted regimes is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_technology_leakage_rate, empirical, 'Actual leakage rate of dual-use materials and expertise despite embargo').

omega_variable(
    surveillance_authority_expansion_intent,
    'Is the expanded surveillance authority (intelligence sharing, export monitoring, supply-chain tracking) deployed strictly for WMD prevention, or does it expand beyond original mandate into general geopolitical intelligence collection?',
    'Declassified surveillance program scope creep analysis; mission expansion of joint intelligence bodies (e.g., NSG, MTCR, Australia Group); documented use of export control apparatus for economic espionage or political leverage unrelated to WMD.',
    'If scope creep present: extractiveness should increase to 0.65+ (high extraction masked by WMD prevention framing). If strictly contained: extractiveness confirmed at 0.58. Affects whether security-conscious states experience the constraint as pure coordination (Rope) or as mixed (Tangled Rope with hidden extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_authority_expansion_intent, empirical, 'Whether surveillance authority expands beyond WMD prevention mandate').

omega_variable(
    effectiveness_on_nonstate_actors,
    'Does state-centric embargo regime effectively prevent nonstate actors (terrorist groups, criminal networks) from acquiring WMD materials and expertise?',
    'Comparative risk assessment: probability of state-level WMD acquisition vs. nonstate actor WMD acquisition under the regime. Historical incidents of nonstate WMD attempts (e.g., Aum Shinrikyo sarin, 2001 anthrax letters). If nonstate actors can circumvent the regime, the constraint''s framing as ''WMD prevention'' is overstated.',
    'If nonstate vulnerability is substantial: the constraint prevents state proliferation (partially) but fails at asymmetric threat (higher-leverage target). Reclassifies the regime as addressing wrong threat actor. May suggest Piton classification (performative state-centric focus masking nonstate proliferation risk).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(effectiveness_on_nonstate_actors, empirical, 'Regime effectiveness against nonstate WMD acquisition').

omega_variable(
    coalition_unity_maintenance,
    'Can the coalition sustain shared enforcement commitment over decades as geopolitical alignments shift and economic incentives pull toward sanctions violation?',
    'Historical tracking of sanctions violations by coalition members (e.g., Iraq sanctions erosion 1998-2003; Iran sanctions fracturing post-JCPOA). Measurement of coalition defection rate under economic pressure (dual-use exports to sanctioned states by nominal coalition members).',
    'If coalition unity degrades: the constraint''s extractiveness increases (enforcement costs rise, credibility declines). Theater ratio may increase (performative enforcement rituals mask actual leakage). May reclassify from Rope to Tangled Rope or Piton as enforcement becomes theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_unity_maintenance, empirical, 'Long-term sustainability of coalition enforcement commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wmd_tr_t0, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(wmd_tr_t5, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, theater_ratio, 5, 0.45).
narrative_ontology:measurement(wmd_tr_t10, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(wmd_be_t0, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wmd_be_t5, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(wmd_be_t10, sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, international_sanctions_regime_economic).
narrative_ontology:affects_constraint(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, export_control_technology_denial).
narrative_ontology:affects_constraint(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, intelligence_sharing_surveillance_expansion).

% DUAL FORMULATION NOTE:
% The WMD prevention regime is a composite constraint family. This story models the macro-level institutional coordination mechanism. Downstream constraints include: (1) export control systems that enforce technology denial (higher extractiveness, more Snare-like from exporter perspective); (2) sanctions regimes that generate economic coercion (separate ε calculation reflecting distribution of pain); (3) intelligence sharing frameworks that expand surveillance authority beyond WMD prevention (distinct extractiveness reflecting scope creep). Each downstream constraint has its own perspectives and beneficiary/victim declarations, linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, institutional, 0.08).
constraint_indexing:directionality_override(sotu_2002_bush_weapons_of_mass_destruction_prevention_regime, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
