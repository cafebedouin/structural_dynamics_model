% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_collapse, []).

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
 *   constraint_id: institutional_legitimacy_collapse
 *   human_readable: Institutional Legitimacy Collapse
 *   domain: political/institutional/social
 *
 * SUMMARY:
 *   Institutional legitimacy collapse occurs when the gap between an
 *   institution's rhetorical claims (its promised function and procedural
 *   legitimacy) and its actual delivered outcomes becomes undeniable. The
 *   collapse is not instantaneous but occurs through a cyclical process:
 *   captured elites extract rents, service quality degrades, theater
 *   investment increases to maintain appearance, dependent populations
 *   experience declining benefits while enforcement remains constant,
 *   organized reformers mobilize, partial reforms are co-opted as theater,
 *   and extraction resumes. The constraint exhibits all six classification
 *   types from different structural positions, revealing that 'legitimacy
 *   collapse' is not a single phenomenon but a multiplex of overlapping
 *   extraction, coordination failure, and institutional degradation
 *   mechanisms. The theater_ratio (0.81) reflects that institutional activity
 *   is predominantly performative—maintaining the appearance of legitimacy
 *   through rituals, symbols, and rhetorical reframing—rather than delivering
 *   genuine benefit. The extractiveness (0.58) shows moderate-high but not
 *   total capture; some institutional functions persist, but an increasing
 *   share of resources flows to capture mechanisms rather than benefit
 *   delivery.
 *
 * KEY AGENTS:
 *   - Dependent Populations: Primary victims (powerless/trapped) — welfare recipients, public service users, patients, students, citizens — bear declining benefits while enforcement mechanisms remain constant
 *   - Institutional Incumbents: Primary beneficiaries (institutional/arbitrage) — leadership, ownership, extractive networks — capture institutional rents and can exit to capture successor institutions
 *   - Organizational Middle Class: Secondary victims (moderate/constrained) — civil servants, professional managers, organizational loyalists — absorb legitimacy debt through cognitive dissonance and identity fusion
 *   - Transnational Elite: Secondary beneficiaries (powerful/mobile) — multi-jurisdictional actors — benefit from regulatory arbitrage and can externalize collapse costs to less-mobile populations
 *   - Institutional Reform Coalition: Organized agents (organized/mobile) — NGOs, civil society, transparency advocates — perceive institutional renewal as solvable problem with sunset pathways
 *   - Legitimacy Theater Apparatus: Institutional actor (institutional/arbitrage) — communications, public relations, ceremonial functions — maintains appearance of legitimacy through increased performative investment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional capture as inherent organizational aging
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_collapse, 0.58).
domain_priors:suppression_score(institutional_legitimacy_collapse, 0.68).
domain_priors:theater_ratio(institutional_legitimacy_collapse, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_legitimacy_collapse, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_collapse, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_collapse, "Institutional Legitimacy Collapse").
narrative_ontology:topic_domain(institutional_legitimacy_collapse, "political/institutional/social").

domain_priors:requires_active_enforcement(institutional_legitimacy_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_collapse, institutional_incumbents).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_collapse, capture_beneficiaries).
narrative_ontology:constraint_victim(institutional_legitimacy_collapse, dependent_populations).
narrative_ontology:constraint_victim(institutional_legitimacy_collapse, legitimacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT POPULATION (SNARE) — Trapped within institutional systems (welfare, healthcare, education, legal) with no viable alternatives. As legitimacy collapses, the institution's performative capacity to coerce compliance increases while its capacity to deliver genuine benefit declines. Maximum experienced extraction: the population bears costs through degraded services while the institutional apparatus maintains control through force and administrative overhead.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZATIONAL MIDDLE CLASS (TANGLED ROPE) — Career professionals, civil servants, and mid-level managers benefit from institutional stability and are locked in by accumulated credentials and sunk costs. They also bear costs through cognitive dissonance between their professional duty and visible institutional dysfunction. Exit is constrained by credential non-portability and identity fusion with professional role. The hybrid classification captures both the coordination function (they keep the system running) and asymmetric extraction (they absorb legitimacy debt).
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL INCUMBENT (ROPE) — Leadership and ownership benefit from institutional continuity and capture mechanisms. They experience the constraint as pure coordination: maintaining the appearance of legitimacy enables continued extraction. Exit options are arbitrage — they can relocate capital, shift to new jurisdictions, or capture successor institutions. Net beneficiary experiencing coordination benefit.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL REFORM COALITION (SCAFFOLD) — Organized agents (NGOs, civil society, reform movements, international bodies) perceive the legitimacy crisis as a temporary coordination failure with structural solution pathways. They have agency and see exit mechanisms: institutional renewal through accountability measures, decentralization, transparency, and participatory redesign. Suppression is high but declining as awareness spreads. Sunset logic applies: if reforms succeed, the need for external scaffolding dissolves.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGITIMACY THEATER (PITON) — The institutional apparatus maintains performative legitimacy through rituals, symbols, and rhetorical reframing (elections, transparency reports, public consultations, awards ceremonies) while material institutional function atrophies. Theater ratio (0.81) reflects that most institutional activity is maintenance of appearance rather than delivery of function. The constraint persists through inertia: institutions maintain the performance because alternatives would require explicit admission of failure. Degraded institutional form.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TRANSNATIONAL ELITE (TANGLED ROPE) — Powerful actors with genuine geographic and jurisdictional mobility experience the constraint as both coordinated benefit-sharing (global networks, regulatory arbitrage across jurisdictions) and asymmetric extraction (they externalize institutional collapse costs to less-mobile populations). They benefit from legitimacy collapse in some jurisdictions while maintaining insulated networks in others. Mixed experience: significant agency and exit mobility alongside capacity to extract from those without it.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, some legitimacy erosion appears inherent to institutional aging: all complex organizations eventually accumulate dysfunction, all systems require periodic renewal, all hierarchies face periodic delegitimation crises. This perspective risks naturalizing contingent institutional arrangements as immutable features of human organization. The false summit detector will flag this if structural data shows the collapse is driven by contingent capture rather than inherent temporal decay.
constraint_indexing:constraint_classification(institutional_legitimacy_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_collapse, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint demonstrates asymmetric resource flows toward capture beneficiaries and enforcement overhead while legitimate institutional functions persist but degrade. The measurement trajectory shows extraction accumulation over the interval (0.32→0.58), indicating that capture mechanisms layer onto coordination functions rather than replacing them entirely. The value reflects that institutional capture is not total theft but rather accumulated rent-extraction that crowds out legitimate benefit delivery. Suppression (0.68): High. Dependent populations face multiple barriers to exit: institutional services are often legally mandated (education, court system, benefits administration), practical alternatives are absent or accessible only to the wealthy, and exit information is controlled by the institution itself. Organized populations (reform coalitions) face suppression through co-optation, captured oversight bodies, and the institution's control of its own renewal processes. Theater_ratio (0.81): Very high and increasing. The institution's activity over the interval shifts from mixed function-performance (theater_ratio=0.45 at T=0) to predominantly performative (0.81 at T=15). The trajectory shows that as extraction accumulates and legitimate service delivery declines, institutional investment increasingly concentrates on appearance maintenance: public relations, ceremonial functions, transparency reports that obscure rather than reveal, performance metrics that measure output rather than outcome. The high theater ratio indicates that most institutional resources flow to legitimacy management rather than actual function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint manifests radically different classifications from different structural positions. The dependent population perceives Snare—pure extraction with no coordination benefit and no exit. The middle class perceives Tangled Rope—both genuine coordination function (they perform legitimate institutional work) and asymmetric extraction (they absorb legitimacy cost). The institutional incumbent perceives Rope—coordination of resource flows that benefit them. The transnational elite perceives Tangled Rope but with genuine mobility—they experience extraction locally but can externalize costs globally. The reform coalition perceives Scaffold—a temporary problem with structural solution pathways and sunset logic. The legitimacy theater apparatus perceives Piton—their role is degraded (performing legitimacy rather than delivering function) but persists through inertia. The analytical observer risks perceiving Mountain—naturalizing institutional aging as an immutable feature of organization. The perspectival gap reveals that 'legitimacy collapse' cannot be classified as a single type; it is a presheaf of overlapping constraints experienced differently depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from structural position relative to extraction flows. Dependent populations with trapped exit face maximum d (~0.95) producing maximum f(d)—they are pure targets. Institutional incumbents with arbitrage exit face minimum d (~0.05) producing negative f(d)—they are pure beneficiaries. The middle class (moderate power, constrained exit) derives medium-high d (~0.65) reflecting that they are partial targets; they benefit from institutional stability but bear costs through cognitive dissonance. The transnational elite (powerful, mobile) face medium d (~0.50) reflecting symmetric positioning—they benefit from institutional function where they interface with it but can exit to less-dysfunctional systems elsewhere. The reform coalition (organized, mobile) faces medium d (~0.45) reflecting that they have agency and exit capacity but bear some cost through inefficiency and time investment in reform efforts. The theater apparatus (institutional, arbitrage) faces very low d (~0.10)—their role benefits from institutional continuation. The divergence in d values across perspectives produces the wide classification range (Snare through Mountain), which is diagnostically appropriate: the constraint genuinely distributes costs and benefits asymmetrically across the institutional landscape.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially resolved but remains contested. The classification as Tangled Rope at the aggregate level appears justified by the coexistence of genuine coordination functions (the middle class performs real institutional work, some services are still delivered, the system still partially functions) alongside asymmetric extraction (rents flow upward, dependent populations experience declining benefits, enforcement persists despite service degradation). However, the classification edges toward Snare as the theater_ratio rises (0.81) and the dependent population's experienced extraction approaches maximum. The risk of mandatrophy is that the 'tangled' framing could naturalize extraction as inherent to coordination, when in fact the tangling is contingent on capture accumulation. The resolution mechanism is empirical: track whether institutional renewal (reform coalition success) reduces both theater_ratio and extractiveness simultaneously, confirming that the tangling was indeed contingent rather than structural. If reforms fail despite mobilization (reform coalition capture), the constraint's classification should degrade toward pure Snare across all perspectives, indicating that the coordination function was illusory and only extraction remained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_erosion_mechanism,
    'Is observed legitimacy collapse driven by inherent institutional aging or by contingent capture and extraction accumulation?',
    'Comparative institutional analysis: contrast collapse trajectories across similar institutions with different governance models and capture vulnerability. Identify whether collapse correlates with capture metrics (regulatory agency revolving door intensity, lobbying concentration, wealth extraction rates) or universal aging metrics (institutional age, complexity measures).',
    'If capture-driven: classification shifts toward Snare and Tangled Rope perspectives; reform pathways exist. If aging-driven: mountain classification gains credibility; institutional renewal becomes inevitable structural feature rather than contingent policy choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_erosion_mechanism, empirical, 'Whether legitimacy collapse is driven by capture or inherent aging').

omega_variable(
    theater_effectiveness_feedback,
    'Does legitimacy theater (performative institutional activity) delay actual collapse or accelerate it by masking underlying dysfunction?',
    'Longitudinal measurement of institutional performance against theater investment: does increased spending on public legitimacy management (communications, events, reports) correlate with stability or instability? Track institutional collapses preceded by theater investment surges.',
    'If theater delays collapse: Piton classification accurate; high theater ratio is structural stabilizer. If theater accelerates collapse: theater becomes Snare mechanism (deceiving populations while capture intensifies); theater measurement becomes risk indicator rather than stability indicator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_effectiveness_feedback, empirical, 'Whether legitimacy theater delays or accelerates institutional collapse').

omega_variable(
    reform_coalition_capacity,
    'Do organized reform movements (civil society, NGOs, accountability mechanisms) have genuine structural capacity to implement institutional renewal or are they themselves captured/coopted?',
    'Analysis of reform coalition outcomes: track implemented reforms, measure policy adoption rates, assess whether adopted reforms address root capture mechanisms or merely performatively update theater. Identify instances of reform capture (superficial changes masking ongoing extraction).',
    'If genuine capacity: Scaffold perspective confirmed; sunset logic is real. If captured: Scaffold is aspirational rather than structural; reform movements function as legitimacy theater (Piton upgrade). Classification remains Tangled Rope but with darker prognosis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_coalition_capacity, empirical, 'Whether reform coalitions have genuine capacity for institutional renewal').

omega_variable(
    identity_lock_institutional_middle,
    'Is the organizational middle class trapped by constrained exit options (credential non-portability, career costs) or identity-locked (professional identity fused with institution)?',
    'Post-exit career trajectory analysis: track middle-class organizational actors who leave collapsing institutions. Measure: employment outcomes (constrained vs mobile career paths), psychological distress (identity disorientation vs practical adjustment challenges), willingness to rejoin similar institutions.',
    'If constrained exit: Tangled Rope classification stands; actors perceive and resist extraction. If identity-locked: actors perceive extraction as unchangeable even when barriers are surmountable; classification shifts toward Piton (theater acceptance) from the middle-class perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_institutional_middle, empirical, 'Whether middle-class organizational actors face constrained or identity-locked exit').

omega_variable(
    transnational_elite_externality_costs,
    'Can transnational elites actually externalize institutional collapse costs indefinitely or do cascading failures in lower-tier jurisdictions eventually threaten higher-tier sanctuary systems?',
    'Network analysis of institutional coupling: map which elite activities depend on legitimacy and functioning of lower-tier institutions (supply chains, customer bases, regulatory cooperation). Model failure cascade scenarios to identify critical coupling points where elite externalization breaks.',
    'If indefinite externalization possible: elite perspective is genuinely mixed (real mobility). If coupling is tight: elite mobility is temporary; Tangled Rope experience intensifies as cascade effects reach sanctuary jurisdictions. Classification may degrade from elite perspective to Snare if interdependencies are underestimated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transnational_elite_externality_costs, empirical, 'Whether elite externalization of collapse costs is sustainable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_collapse, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ilc_tr_t0, institutional_legitimacy_collapse, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ilc_tr_t5, institutional_legitimacy_collapse, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ilc_tr_t10, institutional_legitimacy_collapse, theater_ratio, 10, 0.75).
narrative_ontology:measurement(ilc_tr_t15, institutional_legitimacy_collapse, theater_ratio, 15, 0.81).

% Extraction over time
narrative_ontology:measurement(ilc_be_t0, institutional_legitimacy_collapse, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ilc_be_t5, institutional_legitimacy_collapse, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(ilc_be_t10, institutional_legitimacy_collapse, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(ilc_be_t15, institutional_legitimacy_collapse, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_collapse, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(institutional_legitimacy_collapse, 0.12).
narrative_ontology:affects_constraint(institutional_legitimacy_collapse, regulatory_capture).
narrative_ontology:affects_constraint(institutional_legitimacy_collapse, public_service_degradation).
narrative_ontology:affects_constraint(institutional_legitimacy_collapse, political_legitimacy_erosion).

% DUAL FORMULATION NOTE:
% Institutional legitimacy collapse is upstream of specific capture mechanisms (regulatory capture, bureaucratic rent-seeking) and downstream of structural factors (wealth inequality, information asymmetry). This story represents the macro-level constraint that emerges when multiple micro-level capture mechanisms accumulate. The network links show dependency: legitimacy collapse affects regulatory capture intensity (as legitimacy declines, capture mechanisms must intensify to maintain extraction); public service degradation is both cause and effect of legitimacy collapse; political legitimacy erosion is the visible symptom of institutional legitimacy collapse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
