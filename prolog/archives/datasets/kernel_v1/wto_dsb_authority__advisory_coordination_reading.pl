% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB Authority: Advisory Coordination Reading
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   The WTO Dispute Settlement Body (DSB) operates under contested authority:
 *   the Marrakesh Agreement (article 3.2) describes panel and Appellate Body
 *   determinations as 'recommendations,' yet the enforcement mechanism
 *   (Dispute Settlement Understanding, articles 22–23) permits
 *   countermeasures when states do not comply. This reading instantiates the
 *   'advisory coordination' interpretation: DSB findings provide expert
 *   analysis to facilitate negotiated settlements; member states retain
 *   ultimate policy discretion. Compliance is voluntary but enforced through
 *   bilateral power dynamics and reputational costs. This constraint exhibits
 *   structural asymmetry: large trading states with retaliation capacity
 *   experience the DSB as a negotiation tool (Rope from their perspective);
 *   small developing states without retaliation capacity experience it as
 *   binding extraction dressed in advisory language (Snare from their
 *   perspective). The advisory framing legitimizes asymmetric outcomes by
 *   avoiding direct assertion of institutional authority. The constraint's
 *   extractiveness (0.35) reflects moderate asymmetry — genuine coordination
 *   function exists (dispute resolution is valuable), but extraction is
 *   embedded in power-dependent compliance. Theater has increased over the
 *   interval (0.35 → 0.58) as the Appellate Body crisis (2017–present) has
 *   exposed the institutional fiction: without appellate review, panel
 *   findings lose procedural legitimacy and become pure power-determined
 *   outcomes, increasing the perceived artificiality of the 'recommendation'
 *   framing.
 *
 * KEY AGENTS:
 *   - Small Developing States (Respondent): Primary victims (powerless/trapped) — face binding DSB pressure without retaliation capacity; experience advisory framing as institutional cover for enforcement of large-state preferences.
 *   - Large Trading States (Complainant): Primary beneficiaries (organized/arbitrage) — use DSB as negotiation leverage; can extract concessions by initiating disputes; experience advisory framing as flexibility and discretion.
 *   - WTO Institution: Secondary actor (institutional/constrained) — maintains dispute mechanism authority and member-state legitimacy; benefits from dispute initiation fees and institutional relevance; faces extraction from institutional contestation (AB crisis).
 *   - Developed Trading Sectors (Capital-Intensive): Secondary beneficiary (organized/arbitrage) — benefit from favorable DSB findings that protect intellectual property, investment protections, and market access preferences.
 *   - Policy Space Sovereignty (Abstract): Secondary victim (powerless/trapped) — abstract collective good; bears cost of DSB interpretations that narrow policy space; cannot exit or organize.
 *   - Reform Coalition: Organized actor (organized/constrained) — negotiating next-generation dispute settlement rules; treats advisory framing as transitional framework with negotiated exit pathway (AB restoration, enforcement modernization).
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing power-asymmetric advisory framing as inherent to international law's structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.35).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.42).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB Authority: Advisory Coordination Reading").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__advisory_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, 'f393c4b4-e92d-466d-b6d2-976ad89769c9').
narrative_ontology:cs_kernel_codification('f393c4b4-e92d-466d-b6d2-976ad89769c9', formalized).
narrative_ontology:cs_authority_grounding('f393c4b4-e92d-466d-b6d2-976ad89769c9', lineage).
narrative_ontology:cs_interpretation_layer_present('f393c4b4-e92d-466d-b6d2-976ad89769c9').
narrative_ontology:cs_reading_relation('f393c4b4-e92d-466d-b6d2-976ad89769c9', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('f393c4b4-e92d-466d-b6d2-976ad89769c9', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('f393c4b4-e92d-466d-b6d2-976ad89769c9', foundational, member_state_ultimate_discretion_preserved).
narrative_ontology:cs_axiom_status(member_state_ultimate_discretion_preserved, holdable).
narrative_ontology:cs_axiom_grounding('f393c4b4-e92d-466d-b6d2-976ad89769c9', member_state_ultimate_discretion_preserved, conventional).
narrative_ontology:cs_axiom('f393c4b4-e92d-466d-b6d2-976ad89769c9', foundational, advisory_findings_facilitate_settlement).
narrative_ontology:cs_axiom_status(advisory_findings_facilitate_settlement, holdable).
narrative_ontology:cs_axiom_grounding('f393c4b4-e92d-466d-b6d2-976ad89769c9', advisory_findings_facilitate_settlement, instrumental).
narrative_ontology:cs_reference_frame('f393c4b4-e92d-466d-b6d2-976ad89769c9', marrakesh_negotiated_consensus_framework).
narrative_ontology:cs_drift_state('f393c4b4-e92d-466d-b6d2-976ad89769c9', appellate_body_crisis_contemporary, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f393c4b4-e92d-466d-b6d2-976ad89769c9', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, developed_trading_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, capital_intensive_sectors).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, dispute_initiating_parties).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, small_developing_states).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, policy_space_sovereignty).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, trade_rule_predictability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL DEVELOPING STATE AS RESPONDENT (SNARE) — Faces WTO DSB rulings presented as 'advisory' but enforced through countermeasures if ignored. Cannot exit: withdrawal incurs reputational and market access costs. Cannot organize: asymmetric power means smaller states cannot threaten reciprocal retaliation. The advisory framing reduces legitimacy pressure on the DSB while maintaining extraction through bilateral power dynamics.
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LARGE TRADING STATE AS COMPLAINANT (ROPE) — Uses DSB mechanism as a negotiation tool. Initiates disputes to extract concessions; advisory ruling gives leverage without the blunt instrument of unilateral action. Can ignore adverse rulings and accept countermeasures if beneficial. The constraint coordinates dispute resolution while benefiting this actor through asymmetric information and power in bilateral post-ruling negotiation.
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WTO INSTITUTION (TANGLED ROPE) — Maintains its authority and budget justification through the dispute mechanism, yet must coordinate legitimate trade governance. The advisory framing protects institutional autonomy (member states retain ultimate discretion) while preserving the DSB's perceived legitimacy. WTO coordinates rule-making; it also extracts institutional benefit from dispute initiation fees and member-state compliance pressure. Faces extraction from the system itself: if the advisory framing is too weak, compliance evaporates and institutional relevance declines.
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REFORM COALITION (SCAFFOLD) — Organized actors (smaller states, development advocates, institutional reformers) treat the advisory framing as a negotiation platform with a built-in sunset: the Appellate Body (AB) crisis (2017–present) and ongoing WTO reform negotiations show that member states are actively reconstituting the dispute settlement authority structure. The advisory reading has explicit temporal boundedness — it is a transitional framework during institutional contestation, with negotiated reform (e.g., AB restoration, appellate capacity, enforcement rules) generating the exit pathway.
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INSTITUTIONAL PITON (LEGALIST FICTION) — The Marrakesh Agreement's ambiguity about DSB authority (article 3.2 on 'recommendations' vs. enforceability through countermeasures and potential unilateral action) has degraded into a purely theatrical staging of neutrality. Panel reports are treated as advisory by those who benefit from ignoring them (rich states with retaliation capacity) and binding by those who cannot afford countermeasures (small states). The fiction is maintained through ritual — written findings, appellate procedures, compliance reviews — that gives the appearance of impartial adjudication while actual power determines outcomes. Piton classification: high theater (0.58), low institutional purity (beneficiaries benefit through power asymmetry, not through rule clarity).
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal scope, the tension between member-state sovereignty and institutional adjudication is an irreducible feature of any international legal order that lacks supranational enforcement power. No binding dispute mechanism can survive without member-state consent; consent-based authority cannot compel compliance without coercion. This perspective naturalizes the advisory framing as inherent to the structure of international law itself. However, this reading risks concealing the contingent institutional arrangements — the advisory framing benefits states with retaliation capacity and extracts from those without.
constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wto_dsb_authority__advisory_coordination_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, TR),
    TR >= 0.70.

:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The advisory framing legitimizes DSB authority while preserving member-state discretion, reducing the institutional coercion required to enforce compliance. However, extractiveness is not minimal because compliance is enforced through countermeasures and power dynamics, not purely through legitimacy or coordination benefit. Large states extract concessions through dispute leverage; small states extract institutional benefits through predictable rules. The base value reflects genuine coordination (dispute resolution is valuable) with embedded power asymmetry. Suppression (0.42): Moderate-high. Barriers to non-compliance include potential retaliation, reputational cost, and risk of counter-disputes. However, suppression is not maximal because advisory framing nominally preserves state discretion and provides negotiation exit (states can negotiate compliance reduction or side agreements). Theater ratio (0.58): Moderate-high and rising. The DSB procedure (written panels, appellate review, compliance monitoring) creates substantive legitimacy, but the advisory characterization and the Appellate Body crisis have exposed the underlying power dynamics. Panel reports are treated as authoritative by weak states and ignored by strong states; appellate review was supposed to ensure consistency, but its collapse (2017–) has revealed that the legitimacy rested on procedural theater rather than institutional depth. Measurements show theater increasing over time as the gap between formal authority and actual enforcement widens.
 *
 * PERSPECTIVAL GAP:
 *   The reading produces stark perspectival asymmetry. Large trading states (organized/arbitrage) classify the constraint as Rope: the DSB coordinates dispute resolution while preserving state autonomy to negotiate compliance. Small developing states (powerless/trapped) classify it as Snare: the advisory framing masks binding pressure to comply with outcomes determined by large-state preferences and retaliation capacity. The WTO institution itself (institutional/constrained) experiences Tangled Rope: it benefits from the dispute mechanism (institutional budget, member legitimacy, dispute fees) while facing extraction from the advisory framing (reduced authority to compel compliance, Appellate Body contestation). The reform coalition (organized/constrained) experiences Scaffold: the advisory framework is transitional; reform negotiations are building alternative dispute settlement authority (restored AB, appellate capacity, enhanced enforcement) with an explicit sunset logic. The piton perspective recognizes that the advisory framing has become primarily theatrical: it signals impartiality while actual outcomes flow from power asymmetry. The analytical observer risks the false summit: naturalizing power-asymmetric advisory as inherent to international law's structure rather than recognizing it as a contingent institutional choice that benefits states with enforcement capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Large states (beneficiaries with arbitrage options) experience low d, producing negative or minimal effective extraction (χ ≈ 0.15–0.25). Small states (victims with trapped exit) experience high d, producing high effective extraction (χ ≈ 0.55–0.75). The WTO institution (beneficiary but with constrained exit due to member-state contestation) experiences moderate d with moderate extraction. The reform coalition (organized victim-adjacent actors with constrained but active exit pathway) experiences moderate d offset by coalition power (organized atom), producing lower experienced extraction despite victim status. The analytical observer (analytical/analytical) experiences canonical d ≈ 0.73, producing χ ≈ 0.40–0.50. The perspectival gap in directionality is the core diagnostic: the same structural property (advisory framing) produces different d values depending on the agent's power level and exit capacity, revealing that the constraint's 'neutrality' is power-dependent.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not fully resolve mandatrophy, but diagnostic evidence clarifies the structural tensions. Extractiveness (0.35) places the constraint in the Tangled Rope band (0.30–0.60) where genuine coordination coexists with asymmetric extraction. The rising theater_ratio (0.35 → 0.58) and suppression_requirement (0.38 → 0.42) over the interval show that the advisory framing's legitimacy has eroded as the Appellate Body crisis has exposed institutional fragility. Perspectives classified as Snare (small state), Rope (large state), and Piton (institutional theater) are all empirically supported by observed behavior. The mandatrophy is not resolved because the reading itself is contested: different state coalitions and institutional actors disagree on whether the advisory framing accurately describes DSB authority or covers power asymmetry. The omega variables identify the irreducible uncertainties: (1) whether 'advisory' is functionally accurate or institutional fiction; (2) whether the reading forecloses or coexists with the binding reading; (3) whether it influences the judicial activism reading; (4) power-dependent variance in experienced extractiveness; (5) impact of AB vacancy; (6) asymmetric reading selection by powerful states; (7) whether advisory framing is natural law or false summit. These uncertainties cannot be fully resolved within the advisory reading itself — they require empirical measurement and comparative institutional analysis. The framework routes them through the omega apparatus for ongoing investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_framing_semantics,
    'Does ''advisory'' accurately describe the DSB''s functional role, or is it a legitimacy cover story masking de facto binding authority enforced through asymmetric power?',
    'Empirical compliance data: measure compliance rates by state power level; analyze post-ruling bilateral negotiations to identify whether DSB findings anchor bargaining or are ignored; document patterns of retaliation threat and capitulation',
    'If advisory is accurate: constraint is primarily Rope (coordination with limited extraction). If advisory is fiction: constraint is primarily Snare (binding through power asymmetry with minimal legitimacy). Current evidence suggests reading-dependent interpretation — large states treat it as advisory, small states treat it as binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(advisory_framing_semantics, empirical, 'Whether DSB''s advisory characterization is functionally accurate or institutional fiction').

omega_variable(
    binding_referee_foreclose,
    'Does the advisory coordination reading logically foreclose the binding referee reading (WTO DSB as court with compulsory jurisdiction)?',
    'Legal doctrine analysis: examine whether affirming advisory authority (member-state discretion preserved) is compatible with asserting binding compulsory jurisdiction. Test through hypothetical: can a single framework hold both ''recommendations are advisory'' AND ''panel reports are binding absent state opt-out''?',
    'If forecloses: the two readings are mutually exclusive within a single institutional framework; only one can be correct. If coexists_with: readings compete across different state preferences and institutional roles, but both remain live positions. Current WTO reform debate suggests coexistence — different reform proposals instantiate different readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_referee_foreclose, conceptual, 'Logical relationship between advisory and binding readings of DSB authority').

omega_variable(
    judicial_activism_downstream_pressure,
    'Does the advisory coordination reading create structural downstream pressure on the judicial activism reading (Appellate Body expanding substantive review scope)?',
    'Institutional history: trace whether advisory framing enables or constrains AB scope expansion; examine whether DSB panels have responded to advisory characterization by narrowing or broadening their interpretive mandate; analyze reform debates to see whether advisory advocates oppose AB activism',
    'If influences: advisory reading restricts AB scope by emphasizing recommendations rather than precedent; activism reading requires stronger institutional authority to justify expansive interpretation. If no pressure: readings operate independently. Current AB crisis (2017–present) suggests substantial pressure — advisory advocates cite AB activism as institutional overreach; AB defenders cite advisory framing as insufficient authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_activism_downstream_pressure, empirical, 'Whether advisory framing constrains Appellate Body interpretive scope').

omega_variable(
    extractiveness_measurement_variance,
    'Does the experienced extractiveness of the DSB ruling change depending on whether the respondent state has retaliation capacity?',
    'Comparative case analysis: measure extractiveness for small-state respondents (0.65–0.85 experienced extraction: high suppression, no escape) vs. large-state respondents (0.20–0.40 experienced extraction: advisory framing provides negotiation space). If variance exceeds 0.40, constraint is fundamentally asymmetric and the ''advisory'' characterization obscures power-dependent enforcement.',
    'If high variance: the constraint is not uniform across agents; same structural property (advisory framing) produces different effects depending on power level. Small states experience binding extraction; large states experience advisory coordination. Suggests constraint family decomposition: separate advisory_coordination (large-state perspective) from power_asymmetry_snare (small-state perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extractiveness_measurement_variance, empirical, 'Power-dependent variance in experienced extractiveness of DSB rulings').

omega_variable(
    appellate_body_vacancy_impact,
    'Does the 2017 AB vacancy (DS dispute appellate mechanism collapsed) change the baseline extractiveness of the advisory framing by removing the institutional filter that legitimized DSB findings?',
    'Temporal measurement: compare pre-2017 and post-2017 compliance rates and bilateral negotiation outcomes; examine whether respondent states cite AB absence when rejecting panel findings; measure institutional authority drift in the interim appellate mechanism and substitute dispute resolution (WTO reform negotiations, external arbitration)',
    'If substantial impact: the advisory framing''s legitimacy depends on appellate review; without it, extractiveness increases (enforcement through power alone) or decreases (enforcement through norm becomes weaker). Suggests measurement-dependent constraint classification: advisory_coordination (with AB) vs. institutional_vacuum_extraction (post-AB collapse).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_vacancy_impact, empirical, 'Impact of Appellate Body vacancy on extractiveness of advisory framing').

omega_variable(
    reading_selection_asymmetry,
    'Can a single WTO member state simultaneously hold the advisory reading for ruling outcomes favorable to itself and the binding reading for outcomes adverse to itself?',
    'Institutional practice analysis: examine state statements, compliance decisions, and bilateral negotiation rhetoric; identify whether states invoke advisoriness as grounds for non-compliance when adverse and invoke binding force when favorable. Document whether major states (US, EU, China) apply consistent or inconsistent reading selection.',
    'If asymmetry is widespread: readings are not stable institutional positions but instrumental framings deployed strategically by powerful actors. The constraint is pure Snare (extraction through framing manipulation) rather than Tangled Rope (hybrid coordination-extraction with stable reading). If readings are stable across outcome: constraint classification is robust.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_asymmetry, empirical, 'Whether states apply reading selection asymmetrically based on ruling favorability').

omega_variable(
    false_summit_advisory_naturalization,
    'Is the advisory framing a natural feature of international law''s enforcement limits, or a constructed institutional choice that benefits states with retaliation capacity?',
    'Comparative legal analysis: examine whether advisory authority is necessary for international adjudication, or whether alternative designs (compulsory jurisdiction, binding arbitration, supranational enforcement) are available and adopted elsewhere. Test whether the ''advisory is inevitable'' claim rests on legal necessity or on specific institutional choices.',
    'If natural law: mountain classification is correct; advisory framing is irreducible. If constructed: advisory framing is a false summit that naturalizes power asymmetry; constraint should reclassify toward Snare from analytical perspective. Current evidence suggests false summit: regional trade courts (EU Court of Justice, Andean Tribunal, CEMAC Court of Justice) operate with binding authority, showing that ''advisory'' is institutional choice, not necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_advisory_naturalization, conceptual, 'Whether advisory framing is natural law or constructed institutional choice benefiting powerful states').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsb_advisory_theater_t0, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dsb_advisory_theater_t15, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(dsb_advisory_theater_t30, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(dsb_advisory_extract_t0, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(dsb_advisory_extract_t15, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 15, 0.32).
narrative_ontology:measurement(dsb_advisory_extract_t30, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(dsb_advisory_suppress_t0, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(dsb_advisory_suppress_t15, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 15, 0.4).
narrative_ontology:measurement(dsb_advisory_suppress_t30, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 30, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_ab_appellate_authority_crisis).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, trade_rule_predictability_asymmetry).

% DUAL FORMULATION NOTE:
% The WTO DSB authority kernel decomposes into three constraint stories reflecting three distinct institutional readings. advisory_coordination_reading (this story): low compliance pressure, advisory framing, negotiation-centered, ε≈0.35. binding_referee_reading: institutional authority framing, compulsory compliance, rule-of-law emphasis, ε≈0.28–0.32. judicial_activism_reading: Appellate Body scope expansion, constitutional-level interpretation, institutional overreach risk, ε≈0.42–0.52. These are not three measurements of one constraint; they are three structurally distinct constraints produced by different readings of the same kernel. The advisory reading influences the other two by defining the baseline from which deviation is measured: the binding reading asserts stronger authority; the activism reading extends interpretive scope. The Appellate Body crisis (2017–present) is a downstream constraint that affects all three readings by destabilizing the procedural legitimacy on which the advisory and binding readings depend.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__advisory_coordination_reading, organized, 0.25).
constraint_indexing:directionality_override(wto_dsb_authority__advisory_coordination_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
