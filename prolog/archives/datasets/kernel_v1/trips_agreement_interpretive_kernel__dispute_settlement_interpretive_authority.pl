% ============================================================================
% CONSTRAINT STORY: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trips_dispute_settlement_authority, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority
 *   human_readable: WTO Dispute Settlement Panel Interpretive Authority over TRIPS
 *   domain: international_trade_law/intellectual_property/public_health
 *
 * SUMMARY:
 *   The WTO dispute settlement system's interpretive authority over TRIPS
 *   text is contested across three structural readings: the
 *   dispute-settlement-authority reading (this constraint), the
 *   strong-exclusivity reading (emphasizing patent monopoly as economic
 *   growth engine), and the public-health-flexibility reading (emphasizing
 *   health access safeguards embedded in TRIPS Articles 30-32). This story
 *   instantiates the dispute-settlement reading — the meta-constraint that
 *   interpretive power flows through WTO panels, binding member states
 *   through enforcement via trade retaliation. The kernel is the TRIPS text
 *   itself, a formally codified international agreement whose meaning is
 *   claimed by competing interpreters. This reading treats panel rulings as
 *   the legitimate source of TRIPS meaning. The structural data reveal that
 *   this reading's institutional dominance has been contested and is
 *   currently destabilizing: the Appellate Body's functional collapse
 *   (2017-2020) removed appellate constraint on panels, and panels have
 *   increasingly locked in capital-intensive interpretations through
 *   precedent. The theater ratio shows degradation since 2015 (panels
 *   operating without appellate oversight) followed by recent recovery
 *   (alternative multilateral frameworks and bilateral negotiations
 *   re-introducing interpretive flexibility). The extractiveness trajectory
 *   shows steady rise (1995-2020) as panel authority hardened, then decline
 *   (2020-2024) as developing nations and health systems found exit routes
 *   and alternative forums.
 *
 * KEY AGENTS:
 *   - WTO Dispute Settlement Panels: Institutional interpreter (institutional/arbitrage) — hold binding authority over TRIPS text meaning through panel rulings, enforced via trade retaliation
 *   - Appellate Body: Formal institutional check (institutional/arbitrage-degraded) — designed to review panel interpretations; functionally collapsed since 2017 US blocking of judge appointments
 *   - Capital-Intensive Pharmaceutical Firms: Primary beneficiary (institutional/arbitrage) — lobby for panel interpretations emphasizing patent monopoly enforcement; benefit from binding authority as coordination mechanism for investment
 *   - Wealthy WTO Member States (US, EU): Dual structural position — facilitate panel appointments favorable to capital-intensive readings; exercise retaliatory capacity to enforce capital-intensive panel rulings; institutional power allows exit through bilateral deals
 *   - Developing Nations and Generic Manufacturers: Primary victims (powerless-to-moderate/trapped-to-constrained) — face panel lock-in of capital-intensive interpretations; trade retaliation threat prevents exit despite public health arguments
 *   - Public Health Systems: Powerless victim (powerless/trapped) — bear cost of patent monopoly prices locked in by panel interpretations; no exit capacity and no representation in panel appointments
 *   - WTO Multilateral Balance Architecture: Institutional actor (organized/constrained) — designed to balance competing interests across TRIPS, labor, environment; increasingly substituted by bilateral power dynamics as panels harden readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.58).
domain_priors:suppression_score(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.62).
domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, extractiveness, 0.58).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope).
narrative_ontology:human_readable(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "WTO Dispute Settlement Panel Interpretive Authority over TRIPS").
narrative_ontology:topic_domain(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, "international_trade_law/intellectual_property/public_health").

domain_priors:requires_active_enforcement(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, '1ec28966-65ed-469f-b07f-5a5d2cb24c32').
narrative_ontology:cs_kernel_codification('1ec28966-65ed-469f-b07f-5a5d2cb24c32', formalized).
narrative_ontology:cs_authority_grounding('1ec28966-65ed-469f-b07f-5a5d2cb24c32', extraction).
narrative_ontology:cs_interpretation_layer_present('1ec28966-65ed-469f-b07f-5a5d2cb24c32').
narrative_ontology:cs_reading_relation('1ec28966-65ed-469f-b07f-5a5d2cb24c32', trips_agreement_strong_exclusivity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1ec28966-65ed-469f-b07f-5a5d2cb24c32', trips_agreement_public_health_flexibility_reading, coexists_with).
narrative_ontology:cs_axiom('1ec28966-65ed-469f-b07f-5a5d2cb24c32', foundational, binding_panel_authority_legitimate).
narrative_ontology:cs_axiom_status(binding_panel_authority_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1ec28966-65ed-469f-b07f-5a5d2cb24c32', binding_panel_authority_legitimate, conventional).
narrative_ontology:cs_axiom('1ec28966-65ed-469f-b07f-5a5d2cb24c32', foundational, trade_retaliation_enforcement_necessity).
narrative_ontology:cs_axiom_status(trade_retaliation_enforcement_necessity, holdable).
narrative_ontology:cs_axiom_grounding('1ec28966-65ed-469f-b07f-5a5d2cb24c32', trade_retaliation_enforcement_necessity, instrumental).
narrative_ontology:cs_reference_frame('1ec28966-65ed-469f-b07f-5a5d2cb24c32', multilateral_binding_interpretation).
narrative_ontology:cs_drift_state('1ec28966-65ed-469f-b07f-5a5d2cb24c32', post_appellate_body_collapse_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1ec28966-65ed-469f-b07f-5a5d2cb24c32', '').
narrative_ontology:cs_kernel_id(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_interpretive_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, capital_intensive_pharmaceutical_firms).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, dispute_settlement_institutional_authority).
narrative_ontology:constraint_beneficiary(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wealthy_wto_member_states).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, public_health_systems).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, generic_drug_manufacturers).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, developing_nations).
narrative_ontology:constraint_victim(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_multilateral_balance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING NATION HEALTH SYSTEM (SNARE) — WTO panel rulings lock in patent monopoly interpretations that prevent affordable generic drug access. Exit is impossible: withdrawing from TRIPS incurs trade sanctions; internal generic production faces panel injunction. Zero degrees of freedom. Maximum extraction from perspective of those bearing cost (health access, mortality from treatable diseases).
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERIC MANUFACTURER COALITION (TANGLED ROPE) — Panel rulings constrain but do not eliminate generic production routes (compulsory licensing, differential tiering strategies). Exit cost is high (trade retaliation exposure) but not prohibitive. Benefits from dispute certainty and rule-of-law framing, even when rulings disadvantage generics. Mixed extraction — some coordination function (settling trade disputes) alongside asymmetric cost imposition.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DISPUTE SETTLEMENT PANEL SYSTEM (ROPE) — Panels experience the constraint as pure coordination: producing binding interpretations reduces uncertainty, enables trade predictability, and centralizes authority. Exit options are maximal (panels can produce divergent interpretations; the appellate collapse opened this path). Net beneficiary of the system — institutional authority and legitimacy flow from dispute resolution function.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL-INTENSIVE PHARMACEUTICAL INDUSTRY (ROPE) — Panels operationalize patent monopoly as coordination mechanism: standardized TRIPS interpretation enables predictable markets, reduces investment risk, creates enforceable intellectual property across jurisdictions. Effective extraction is scaled by panel authority (f(d) approaches beneficiary floor as arbitrage options increase). Net beneficiary with high exit capacity (can lobby for panel reinterpretation, exit via bilateral trade deals).
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: APPELLATE BODY INSTITUTIONAL STRUCTURE (PITON) — The Appellate Body (formally functional until 2020, structurally dysfunctional since 2017 blocking of appointments) represents a degraded coordination mechanism. Theater ratio is high: the Body's legitimacy derives from appellate review function, but that function has atrophied due to deliberate institutional sabotage (US blocking appellate judge appointments). The structure persists through inertia and diplomatic fiction despite functional collapse — panels now operate as final arbiters without appellate constraint.
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: WTO MULTILATERAL BALANCE ARCHITECTURE (TANGLED ROPE) — The dispute settlement system was designed to coordinate competing interests across issue domains (market access, health, labor). Panel authority creates genuine coordination by forcing bilateral negotiation through a multilateral frame. But panels increasingly lock in capital-intensive readings of TRIPS over public-health readings, substituting bilateral power dynamics (US retaliatory capacity, EU trade leverage) for multilateral balance. Mixed coordination (stabilizing disputes) and extraction (privileging capital-intensive interpretations).
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW FRAMING (MOUNTAIN) — From a civilizational view, binding dispute settlement is inherent to any trade system: contracts always require interpretation, and interpretation always requires authority. The panel system appears as an immutable feature of international law itself. However, false summit detection applies: the 'binding authority through panels' is not a natural law but a specific institutional design choice with identifiable beneficiaries (capital-intensive firms, wealthy nations with large retaliatory capacity).
constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, TR),
    TR >= 0.70.

:- end_tests(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Panel authority creates genuine coordination (reducing uncertainty about TRIPS meaning) but operationalizes capital-intensive extraction through precedent and enforcement. The extractiveness value reflects the asymmetric cost imposition on health systems and generic manufacturers — their structural vulnerability (high suppression, no exit) produces high experienced extraction despite the coordinating function panels provide to beneficiaries. The temporal trajectory shows accumulation (0.38 to 0.68 over 1995-2020) as panels locked in precedent, then partial decline (0.68 to 0.58 by 2024) as health systems found alternative forums. Suppression (0.62): Moderate-high. Trade retaliation threat is credible and enforced (real sanctions against India, Brazil, Thailand for public health overrides). But suppression is not total: developing nations can lobby within panels (constrained exit), pursue compulsory licensing under Article 31 (albeit under panel challenge), and increasingly exit to alternative forums (MERCOSUR agreements, African Union frameworks, bilateral deals). Theater ratio (0.48 rising to 0.72 then falling to 0.48): Shows institutional degradation and recovery. Rise reflects appellate collapse — panels operated as final arbiters without appellate review, generating opaque precedent (high theater). Fall reflects recent pluralization of interpretive sites — developing nations using panel challenges as negotiating theater rather than as binding authority venues, shifting real interpretive power to bilateral and regional forums.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Panels and beneficiaries see Rope — coordination through binding authority reduces transaction costs and enables predictable investment. Health systems see Snare — panel authority locks in capital-intensive readings with no exit capacity. Generic manufacturers see Tangled Rope — mixed coordination (dispute certainty) and extraction (capital-intensive bias). The Appellate Body sees Piton — the appellate oversight function has atrophied due to deliberate institutional sabotage (US blocking), leaving theater (legitimacy through appellate process) without function (actual appellate review). The multilateral architecture sees Tangled Rope — genuine coordination of competing interests alongside extraction through bilateral power substitution. The analytical observer's mountain (binding dispute settlement as natural law of trade) is a false summit: the 'binding authority through panels' is a specific institutional design with identifiable beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is computed from structural position — power level, exit options, beneficiary/victim status. Panels and pharmaceutical firms are beneficiaries with institutional power and arbitrage options (exit through bilateral deals, influence over panel appointments), yielding low d → negative effective extraction (they experience the system as coordination). Health systems are victims with powerless status and trapped options (exit incurs sanctions, alternatives don't escape TRIPS), yielding high d → high effective extraction (maximum experienced cost). Generic manufacturers are victims with moderate power and constrained options (can lobby within panels, pursue limited alternatives), yielding moderate d → moderate effective extraction. The Appellate Body has degraded from institutional to organized status with constrained exit due to US blocking, shifting its directionality and producing Piton classification. The multilateral architecture occupies organized/constrained position: genuine coordination function (balancing competing interests) but increasingly subordinate to bilateral power dynamics (US-EU leverage over developing nations).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is the gap between the formal claim (panel interpretations are neutral application of TRIPS law) and the structural reality (panel rulings lock in capital-intensive readings through appointment composition and precedent effects). The constraint resolves the mandatrophy through perspectival differentiation: from the beneficiary's view (pharmaceutical firms, wealthy nations), the system genuinely coordinates investment uncertainty; from the victim's view (health systems), it extracts through authority lock-in; from the institutional check's view (Appellate Body), it has become performative theater without function. No single type erases the mandatrophy — the presheaf of perspectives IS the answer. The false summit reveals that 'binding dispute settlement' is not an immutable feature of trade law but a specific institutional choice with reversible beneficiary structures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    panel_neutrality_vs_beneficiary_alignment,
    'Do WTO dispute panels genuinely apply neutral legal reasoning to TRIPS interpretation, or do panel appointments and composition systematically favor capital-intensive readings that align with US and EU pharmaceutical lobbying?',
    'Comparative analysis of panel composition (nationality, prior employment, professional networks) correlated with ruling patterns; causal inference from appointment timing to decision direction; international law scholarship on judicial independence in WTO',
    'If genuinely neutral: panels are coordination mechanisms (Rope from all perspectives). If systematically biased: panels are extraction mechanisms disguised as neutral law (constraint reclassifies to Snare from health-system and developing-nation perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(panel_neutrality_vs_beneficiary_alignment, empirical, 'Whether panel composition and rulings reflect neutral interpretation or systematic beneficiary alignment').

omega_variable(
    appellate_body_collapse_causality,
    'Did the US block Appellate Body judge appointments to collapse the multilateral system and preserve bilateral negotiating leverage, or for principled disagreement about appellate scope?',
    'Documentary evidence (trade representative statements, diplomatic cables); correlation of blocking timing with specific appellate decisions unfavorable to US interests; counterfactual analysis of what positions the US had adopted in prior appellate contexts',
    'If deliberate institutional sabotage: panels operate as bilateral power arbitration (constraint reclassifies from Rope to Tangled Rope at multilateral level, Snare from developing nations). If principled disagreement: appellate collapse is a legitimate dispute over institutional design, not extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_collapse_causality, empirical, 'Causality of Appellate Body institutional collapse').

omega_variable(
    public_health_flexibility_enforceability,
    'Can TRIPS Article 30-31-32 public health flexibilities (compulsory licensing, parallel importation, research exemptions) be sustained through panels once challenged by capital-intensive firms, or do panels consistently override them?',
    'Historical review of panel rulings on public health challenges to TRIPS enforcement (India generics cases, Thailand compulsory licensing disputes, Brazil ARV program); measure panel success rate for public health vs IP rights arguments; longitudinal trend analysis',
    'If flexibilities are enforceable: Tangled Rope classification holds (genuine coordination with trade-offs). If systematically overridden: constraint reclassifies to Snare with capital-intensive firm as sole beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_health_flexibility_enforceability, empirical, 'Whether WTO panels enforce public health flexibilities or systematically override them').

omega_variable(
    kernel_reading_status_ambiguity,
    'Is the ''dispute settlement interpretive authority'' reading currently institutionally dominant, or is the ''public health flexibility'' reading still live despite panel precedent? What markers distinguish active kernel contest from settled reading?',
    'Measure institutional signaling: WTO member statements, pharmaceutical lobby advocacy direction, developing nation coalition statements, NGO litigation strategy; track whether developing nations are still petitioning panels for public health readings or have shifted to alternative forums (WHO, regional trade agreements)',
    'If dispute settlement reading is institutionally dominant: panels have settled the kernel toward capital-intensive interpretation (Snare from health perspective). If kernel contest remains live: sibling readings retain structural salience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status_ambiguity, empirical, 'Institutional dominance status of dispute settlement interpretive reading versus sibling readings').

omega_variable(
    alternative_forum_substitution,
    'Are developing nations and health systems exiting the WTO dispute system and relying instead on regional trade agreements (African Union, MERCOSUR), bilateral arrangements, or WHO framework agreements for public health exceptions?',
    'Longitudinal mapping of dispute forum selection: WTO panels vs regional arbitration vs bilateral negotiation; analysis of pharmaceutical access outcomes under each forum; tracking of patent enforcement gaps and generic production under alternative frameworks',
    'If substitution is occurring: the WTO dispute system''s effective scope is narrowing (hidden exit reducing measured suppression). Constraint may be reclassifying as local or regional rather than global.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_forum_substitution, empirical, 'Extent of exit from WTO dispute settlement to alternative forums').

omega_variable(
    committer_frame_kernel_contest,
    'Which sibling reading is institutionally ascending and which is declining? Has the dispute-settlement-authority reading locked in one interpretation of TRIPS over others, or are all three readings still in contest within the WTO system?',
    'Track panel ruling patterns over time: frequency of capital-intensive vs public-health interpretations; measure institutional signaling by WTO bodies, member states, and stakeholder coalitions; assess whether appellate collapse has hardened the dispute-settlement reading into institutional dominance',
    'If dispute-settlement reading is locked in: it has foreclosed or substantially constrained the public-health reading within the multilateral framework. If kernel contest remains live: all readings are still defended by institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_frame_kernel_contest, empirical, 'Institutional dominance and kernel contest dynamics among TRIPS interpretive readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trips_theater_1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 0, 0.35).
narrative_ontology:measurement(trips_theater_2002, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 7, 0.42).
narrative_ontology:measurement(trips_theater_2009, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 14, 0.48).
narrative_ontology:measurement(trips_theater_2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 20, 0.65).
narrative_ontology:measurement(trips_theater_2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 25, 0.72).
narrative_ontology:measurement(trips_theater_2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, theater_ratio, 29, 0.48).

% Extraction over time
narrative_ontology:measurement(trips_extractiveness_1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(trips_extractiveness_2002, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 7, 0.52).
narrative_ontology:measurement(trips_extractiveness_2009, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 14, 0.63).
narrative_ontology:measurement(trips_extractiveness_2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(trips_extractiveness_2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(trips_extractiveness_2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, base_extractiveness, 29, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(trips_suppression_1995, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(trips_suppression_2002, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 7, 0.55).
narrative_ontology:measurement(trips_suppression_2009, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 14, 0.65).
narrative_ontology:measurement(trips_suppression_2015, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(trips_suppression_2020, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 25, 0.78).
narrative_ontology:measurement(trips_suppression_2024, trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, suppression_requirement, 29, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_strong_exclusivity_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, trips_agreement_public_health_flexibility_reading).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, pharmaceutical_patent_monopoly_enforcement).
narrative_ontology:affects_constraint(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, wto_appellate_body_institutional_collapse).

% DUAL FORMULATION NOTE:
% The dispute-settlement-authority reading is the meta-constraint that channels how substantive TRIPS readings (strong exclusivity vs public health flexibility) are operationalized. The constraint family consists of three sibling constraint stories: (1) dispute-settlement-interpretive-authority (this file) — the institutional process through which readings are locked in; (2) strong-exclusivity-reading — the substantive meaning emphasizing patent monopoly; (3) public-health-flexibility-reading — the substantive meaning emphasizing health access safeguards. Each has distinct epsilon, beneficiary/victim structures, and perspectives. The dispute-settlement reading's extractiveness reflects the institutional mechanism's ability to lock in readings through precedent and enforcement; the sibling readings' extractiveness reflects the substantive meaning itself (capital-intensive vs health-centered allocations). All three are linked through network.affects_constraints because the institutional process (dispute settlement) determines which substantive reading becomes institutionally dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trips_agreement_interpretive_kernel__dispute_settlement_interpretive_authority, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
