% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Complementarity: Hybrid Universal-Sovereign Authority
 *   domain: international_law/institutional_authority/treaty_interpretation
 *
 * SUMMARY:
 *   The Rome Statute's complementarity mechanism embodies a fundamental
 *   tension in international law: the aspiration to universal accountability
 *   for mass atrocities (genocide, crimes against humanity, war crimes)
 *   versus the institutional reality of state sovereignty as the organizing
 *   principle of international order. Complementarity locates primary
 *   jurisdiction in states ('positive complementarity') with ICC exercising
 *   residual jurisdiction only when states are 'unwilling or unable' to
 *   prosecute. This reading instantiates the hybrid model: neither pure
 *   universalism (ICC as ultimate authority) nor pure sovereigntism (states
 *   as exclusive authority), but a layered structure where universal
 *   aspiration meets practical deference to state primacy. The constraint is
 *   Tangled Rope because complementarity serves a genuine coordination
 *   function (allowing states to retain primacy while building an
 *   accountability backstop) while simultaneously enabling extraction
 *   (powerful states can evade jurisdiction through non-cooperation or
 *   selective enforcement, and victims in non-cooperating states face maximum
 *   entrapment). The theater ratio (0.58) reflects that the Rome Statute's
 *   preamble declares universal justice commitments while actual enforcement
 *   privileges state sovereignty: the performative aspect has increased over
 *   time as the gap between universal aspiration and selective enforcement
 *   has become visible.
 *
 * KEY AGENTS:
 *   - Victims in Non-Cooperating States: Primary victims (powerless/trapped) — face maximum extraction when state refuses ICC cooperation; no exit path or appeal mechanism
 *   - Human Rights Coalitions & Civil Society: Secondary actors (organized/constrained) — experience coordination benefit (ICC norm-setting, accountability backstop) alongside constraint (complementarity limits ICC reach); constrained exit via regional alternatives
 *   - ICC as Institutional Authority: Primary beneficiary (institutional/arbitrage) — benefits from treaty framework guaranteeing deference; maintains universal authority claim while deferring enforcement burden to states
 *   - State Parties (Compliant States): Secondary beneficiary (institutional/arbitrage) — see complementarity as coordination (sovereignty protected, burden-sharing via ICC); low extraction, high coordination value
 *   - Non-Party States & Selective Cooperators: Strategic actors (powerful/mobile) — enjoy extraction benefits from non-party status or selective cooperation; high mobility constrains actual extraction despite formal primacy
 *   - Universal Justice Aspiration: Institutional victim (institutional/arbitrage, viewed theatrically) — declared in preamble but operationally subordinated to state deference; theater ratio increasing over time
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the hybrid tension as immutable structural feature rather than contingent institutional design choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.52).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Complementarity: Hybrid Universal-Sovereign Authority").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/institutional_authority/treaty_interpretation").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'a6a97491-5761-489c-b57d-3c57fb5d0490').
narrative_ontology:cs_kernel_codification('a6a97491-5761-489c-b57d-3c57fb5d0490', formalized).
narrative_ontology:cs_authority_grounding('a6a97491-5761-489c-b57d-3c57fb5d0490', extraction).
narrative_ontology:cs_interpretation_layer_present('a6a97491-5761-489c-b57d-3c57fb5d0490').
narrative_ontology:cs_reading_relation('a6a97491-5761-489c-b57d-3c57fb5d0490', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6a97491-5761-489c-b57d-3c57fb5d0490', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('a6a97491-5761-489c-b57d-3c57fb5d0490', foundational, complementarity_deference_principle).
narrative_ontology:cs_axiom_status(complementarity_deference_principle, holdable).
narrative_ontology:cs_axiom_grounding('a6a97491-5761-489c-b57d-3c57fb5d0490', complementarity_deference_principle, conventional).
narrative_ontology:cs_axiom('a6a97491-5761-489c-b57d-3c57fb5d0490', foundational, hybrid_authority_legitimacy).
narrative_ontology:cs_axiom_status(hybrid_authority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('a6a97491-5761-489c-b57d-3c57fb5d0490', hybrid_authority_legitimacy, instrumental).
narrative_ontology:cs_reference_frame('a6a97491-5761-489c-b57d-3c57fb5d0490', rome_statute_article_17_complementarity).
narrative_ontology:cs_drift_state('a6a97491-5761-489c-b57d-3c57fb5d0490', contemporary_selective_enforcement_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a6a97491-5761-489c-b57d-3c57fb5d0490', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_institutional_authority).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_sovereignty_preservers).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_justice_aspiration).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_non_cooperating_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VICTIMS IN NON-COOPERATING STATES (SNARE) — Face maximum entrapment when their state refuses ICC cooperation or denies jurisdiction. No appeal mechanism exists; no exit path. The complementarity mechanism ensures ICC defers to state sovereignty even when the state is the perpetrator. Extraction runs maximum: universal jurisdiction aspiration exists but is operationally foreclosed for this agent.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: HUMAN RIGHTS COALITIONS & CIVIL SOCIETY (TANGLED ROPE) — Experience genuine coordination benefit (ICC as accountability backstop, norm-setting for universal justice) alongside constraint (complementarity deference limits ICC reach). High resource requirements for advocacy; modest enforcement capacity; real but constrained exit (shifting jurisdiction to regional bodies, but global reach is lost). Mixed classification: coordination through universal norm-building, extraction via procedural entrenchment of state primacy.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ICC AS INSTITUTIONAL AUTHORITY (ROPE) — Benefits from treaty framework that guarantees deference via complementarity. ICC exercises residual universal authority (can prosecute when state is unwilling or unable) while avoiding friction with sovereign states. The institutional arrangement serves ICC: low political friction, maintained treaty legitimacy, secure budgetary and jurisdictional foundation. Net beneficiary through arbitrage — ICC can claim universal authority while deferring enforcement burden to states.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE PARTIES - COMPLIANT STATES (ROPE) — See complementarity as pure coordination: their sovereignty is protected (state retains primary jurisdiction), and they benefit from burden-sharing (ICC handles cases when state capacity is exceeded). Low extraction, genuine coordination function. Exit option: withdraw from treaty, but arbitrage option is more valuable (maintain treaty relationship while preserving state primacy).
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-PARTY STATES & SELECTIVE COOPERATORS (TANGLED ROPE) — Navigate hybrid authority structure strategically. Non-party states enjoy extraction benefits (no ICC jurisdiction over nationals absent Security Council referral) while claiming alignment with universal justice aspirations. Selective cooperators (parties who cooperate on cases against adversaries but not allies) experience mixed extraction and coordination. High mobility (can leave treaty, can selectively enforce) but constrained by international reputation costs and pressure from civil society coalitions.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: UNIVERSAL JUSTICE ASPIRATION - INSTITUTIONAL PERFORMANCE (PITON) — The Rome Statute's preamble declares commitment to universal accountability for mass atrocities. Complementarity operationally reverses this: actual practice privileges state deference over universal jurisdiction. The aspiration persists (in treaty text, rhetoric, norm-setting) but is increasingly theatrical — enforcement requires state cooperation, which is selectively granted. Theater ratio high (universal accountability declared; selective actual enforcement). Piton classification: the performative maintenance of a degraded institution, inertially preserved by path-dependency rather than functional necessity.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER - NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, the tension between universal justice and state sovereignty is presented as an immutable structural feature of international law: nation-states cannot fully cede jurisdiction because statehood is constituted through sovereignty. This view naturalizes complementarity as an inevitable compromise between incompatible commitments. However, this classification is a false summit: the apparent immutability conceals the contingent institutional choices (treaty design, enforcement mechanism selection, state power asymmetries) that could be restructured. The engine's FSM detector will flag this perspective as naturalizing contingent arrangements.
constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rome_statute_jurisdiction__hybrid_complementarity_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, TR),
    TR >= 0.70.

:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The Rome Statute creates asymmetric power distribution favoring state parties (especially powerful states) and the ICC institutional structure. Complementarity deference ensures primary jurisdiction rests with states, limiting ICC enforcement; non-cooperating states face zero enforcement consequences; victims' access to justice depends on their state's willingness and capacity. The extractiveness has increased over time (0.38 → 0.52) as the selectivity gap has become visible: powerful states enjoy non-party status or selective cooperation, while weaker states face pressure to cooperate and comply. Suppression (0.48): Moderate. Multiple barriers limit universal jurisdiction: complementarity doctrine itself (primary jurisdiction deference), state non-cooperation (no enforcement mechanism absent UNSC referral for non-parties), resource constraints in ICC, political pressure from powerful states, and victim/witness intimidation in conflict zones. Suppression has increased (0.35 → 0.48) as the enforcement barriers have hardened. Theater ratio (0.58): Moderate-high. The Rome Statute's preamble declares commitment to universal accountability and 'lasting peace and security' through justice. Actual practice privileges state sovereignty: ICC cases proceed only when states cooperate or UNSC refers. The gap between universal aspiration (rhetoric) and selective enforcement (practice) has widened, increasing theater. The hybrid reading emphasizes that complementarity is neither purely performative nor purely functional — it serves real coordination goals while enabling real extraction.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is maximal and instructive. Victims and ICC occupy opposite positions in the directionality spectrum: victims derive d ≈ 0.95 (maximum target status); ICC derives d ≈ 0.18 (maximum beneficiary status). From victims' perspective, complementarity is pure extraction (Snare). From ICC's perspective, it is pure coordination (Rope). The analytical observer risks seeing the gap as evidence of a natural law (the inevitable tension between universal aspiration and state sovereignty is immutable), but the structural data shows the gap is contingent: it depends on treaty design choices (complementarity clause), enforcement mechanisms (state cooperation required), and power asymmetries (powerful states enjoy non-party status). If treaty design shifted (e.g., ICC prosecutor could initiate cases without state cooperation, or universal jurisdiction primacy replaced complementarity), the classification landscape would restructure. The mountain classification is a false summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation of directionality (d) from structural position yields the classification gap across perspectives. Victims in non-cooperating states face maximum structural extraction (state is perpetrator, refuses cooperation, ICC defers) — derived d ≈ 0.95 (trapped + victim status + no exit) → high f(d) → high χ → Snare experienced. Compliant state parties derive d ≈ 0.15 (institutional power + arbitrage exit + beneficiary status via sovereignty preservation) → low f(d) → low χ → Rope experienced. ICC derives d ≈ 0.18 (institutional power + arbitrage exit + beneficiary status via deference structure) → low f(d) → negative χ → Rope experienced. Organized civil society derives d ≈ 0.58 (organized power + constrained exit + mixed victim/beneficiary status: victim of limited enforcement, beneficiary of norm-setting) → moderate f(d) ≈ 0.75 → moderate χ → Tangled Rope experienced. The directionality chain is stable and well-derived from structural data; no overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-LEVEL RESOLUTION: The hybrid complementarity reading resolves mandatrophy by declaring that the Rome Statute instantiates a legitimate but asymmetric institutional structure. Neither universalism (ICC as supreme) nor sovereigntism (states as exclusive) is structurally tenable given the empirical reality of state power and institutional capacity. Complementarity is the institutional compromise that operationalizes coexistence: states retain primary jurisdiction (satisfying sovereigntist concerns) while ICC retains residual universal authority (satisfying universalist aspirations). The constraint is Tangled Rope at the modal perspective (organized civil society) because the trade-off is real and asymmetric: genuine coordination exists (states gain burden-sharing, ICC gains legitimacy) but extraction also exists (powerful states extract immunity, victims extract zero justice in non-cooperating states). The mandatrophy is resolved not by collapsing the tension but by precisely specifying which agents experience which costs and benefits. This reading is structurally distinct from pure universalism (which denies sovereigntist constraints) and pure sovereigntism (which denies universal aspirations).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_as_genuine_coordination_vs_sovereignty_protection_cover,
    'Does complementarity function as genuine coordination between universal justice and state autonomy, or primarily as a sovereignty-protection mechanism disguised as coordination?',
    'Longitudinal case analysis: track ICC prosecutions where complementarity deference occurred; assess whether deferral served coordination goals (state capacity building, norm development) or protected state interests regardless of accountability capacity. Measure cooperation rates by state power level.',
    'If coordination: classification shifts toward Rope for all perspectives (genuine mutual benefit). If sovereignty cover: classification remains Tangled Rope/Snare (asymmetric extraction). Determines whether the constraint is structurally sustainable or will collapse when universal justice expectations unmet.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complementarity_as_genuine_coordination_vs_sovereignty_protection_cover, empirical, 'Whether complementarity is genuine coordination or sovereignty protection mechanism').

omega_variable(
    selectivity_bias_in_state_cooperation,
    'Do states cooperate with ICC based on capacity to prosecute (complementarity logic) or based on geopolitical alignment with ICC membership and permanent members'' interests?',
    'Mapping of ICC prosecution patterns against state capacity metrics (judicial infrastructure, rule-of-law indicators) and against geopolitical alignment with permanent UNSC members and treaty adherents. Identify prosecutions that occur against major-power allies and compare to non-prosecutions against non-parties or adversaries.',
    'If capacity-driven: complementarity is functionally legitimate (respects state autonomy while enabling accountability). If alignment-driven: complementarity masks selective enforcement based on geopolitical power (extraction increases, classification shifts toward Snare from victims'' perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(selectivity_bias_in_state_cooperation, empirical, 'Selectivity bias: state cooperation based on capacity vs. geopolitical alignment').

omega_variable(
    reading_contest_kernel_stability,
    'Which reading of the Rome Statute''s jurisdiction clause is most defensible within contemporary international law authority structures: universalist (universal justice primacy), sovereigntist (state primacy), or hybrid complementarity (coexistence with deference)?',
    'Analysis of treaty text (Rome Statute Article 17 complementarity clause), travaux préparatoires, ICJ advisory opinions, ICC case law interpreting complementarity, state practice in cooperation/non-cooperation, and emerging doctrine on universal jurisdiction.',
    'If universalist defended: the hybrid reading''s axiom of ''coexistence via deference'' becomes untenable; universal aspiration could restructure complementarity toward primacy. If sovereigntist defended: hybrid reading becomes the progressive compromise position. If hybrid defended: the current institutional arrangement has deeper legitimacy than either pole acknowledges.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contest_kernel_stability, conceptual, 'Textual and doctrinal defensibility of each reading of Rome Statute jurisdiction').

omega_variable(
    institutional_capture_by_powerful_states,
    'Is ICC institutional authority genuinely constrained by complementarity principle, or is complementarity invoked selectively to protect powerful non-parties and strategic allies from prosecution?',
    'Analysis of cases dropped or not initiated due to complementarity deference; correlation between state power, permanent UNSC seat, and rate of ICC non-intervention. Comparison of complementarity application to powerful states vs. weak states.',
    'If genuine principle: complementarity is legitimate institutional design (Rope/Tangled Rope classification sustained). If selective invocation: ICC is captured by powerful-state interests (classification shifts to Snare for victims; piton degradation of universal authority confirmed). Determines whether hybrid reading can sustain legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_by_powerful_states, empirical, 'Selectivity of complementarity deference as institutional capture mechanism').

omega_variable(
    complementarity_sunset_or_permanent_structural_condition,
    'Is complementarity an interim mechanism awaiting state capacity development and norm maturation (sunset trajectory), or a permanent structural principle defining international law''s hybrid authority model?',
    'Trend analysis: ICC prosecutions initiated over time against state capacity indicators. Analysis of state ratification and cooperation commitments. Assessment of whether complementarity principle is moving toward universal primacy (institutional drift) or remaining locked (structural stability). Expert opinion from international law authorities on trajectory.',
    'If sunset: complementarity is Scaffold (temporary mechanism with enforcement and built-in trigger for phase transition). If permanent: Tangled Rope or sustained Piton (depends on whether functionality persists). Determines regime stability and whether victims'' entrapment is temporary or structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_sunset_or_permanent_structural_condition, empirical, 'Whether complementarity is sunset mechanism or permanent structural principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rsjc_theater_t0_adoption, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rsjc_theater_t5_early_cases, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 5, 0.52).
narrative_ontology:measurement(rsjc_theater_t10_arab_spring_era, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(rsjc_extractiveness_t0_adoption_1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rsjc_extractiveness_t5_early_cases_2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rsjc_extractiveness_t10_arab_spring_era_2015, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 10, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(rsjc_suppression_t0_adoption, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rsjc_suppression_t5_early_cases, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 5, 0.42).
narrative_ontology:measurement(rsjc_suppression_t10_arab_spring_era, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court_institutional_capture).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universal_jurisdiction_state_non_cooperation).

% DUAL FORMULATION NOTE:
% The Rome Statute jurisdiction kernel decomposes into three structurally distinct constraint stories with different ε values and different authority groundings. Universalist reading emphasizes universal aspiration (lower ε, coordination-dominant). Sovereigntist reading emphasizes state primacy (lower ε, consent-based). Hybrid complementarity reading emphasizes coexistence via deference (moderate ε, 0.52, both coordination and extraction present). These are not observable-dependent variations on one constraint — they are genuinely different authority framings with different institutional implications. Network links connect the readings via their shared kernel (Article 17) and their structural interdependencies: universalist and sovereigntist readings coexist across different parties' commitments; hybrid reading influences both by establishing the operational status quo.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
