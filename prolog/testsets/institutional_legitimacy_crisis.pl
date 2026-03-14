% ============================================================================
% CONSTRAINT STORY: institutional_legitimacy_crisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_legitimacy_crisis, []).

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
 *   constraint_id: institutional_legitimacy_crisis
 *   human_readable: Institutional Legitimacy Crisis
 *   domain: institutional_governance/political_economy
 *
 * SUMMARY:
 *   An institutional legitimacy crisis occurs when the institutional
 *   structures ostensibly designed to serve a constituency progressively
 *   reveal themselves as mechanisms for extracting resources, attention,
 *   compliance, and consent from that constituency while delivering
 *   diminishing substantive value in return. The constraint exhibits all six
 *   DR types from different perspectives, making it a diagnostic exemplar for
 *   multi-level institutional dynamics. The same structural phenomenon — the
 *   gap between institutional accountability claims and institutional
 *   performance outcomes — appears as an immutable law of governance
 *   (mountain), a coordination mechanism managing complexity (rope), a mixed
 *   coordination-extraction hybrid (tangled_rope), a temporary problem being
 *   solved by reform movements (scaffold), a degraded legitimacy ritual
 *   (piton), or pure extraction (snare), depending on the observer's
 *   structural position and power level. The constraint's theater_ratio
 *   (0.78) reflects that formal legitimacy rituals — public consultations,
 *   transparency reports, accountability hearings, participatory processes —
 *   are substantially performative: they create the appearance of
 *   constituency voice and institutional responsiveness without shifting
 *   substantive decision-making authority or resource allocation. The
 *   progression from theater_ratio 0.55 to 0.78 indicates that performative
 *   legitimacy theater has intensified as structural legitimacy has eroded,
 *   generating the characteristic Piton signature: theatrical maintenance of
 *   a degraded function.
 *
 * KEY AGENTS:
 *   - Institutional Constituency: Primary victim (powerless/trapped) — citizens dependent on institutional services; face maximal extraction with no exit options
 *   - Institutional Leadership: Primary beneficiary (institutional/arbitrage) — captures legitimacy flow, narrative authority, resource control; experiences constraint as coordination mechanism
 *   - Intermediate Institutional Actors: Secondary victim (moderate/constrained) — mid-level bureaucrats, professionals, administrators face ethical compromise and career risk; also benefit from institutional stability
 *   - Reform Movements and Accountability Networks: Organized agents (organized/constrained) — transparency advocates, NGOs, distributed accountability mechanisms building alternative institutional structures with sunset logic
 *   - Institutional Performance Theater: Institutional actor (institutional/arbitrage) — formal legitimacy rituals; maintains appearance of accountability while real power remains concentrated
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable laws of governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_legitimacy_crisis, 0.58).
domain_priors:suppression_score(institutional_legitimacy_crisis, 0.65).
domain_priors:theater_ratio(institutional_legitimacy_crisis, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_legitimacy_crisis, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_legitimacy_crisis, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_legitimacy_crisis, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_legitimacy_crisis, tangled_rope).
narrative_ontology:human_readable(institutional_legitimacy_crisis, "Institutional Legitimacy Crisis").
narrative_ontology:topic_domain(institutional_legitimacy_crisis, "institutional_governance/political_economy").

domain_priors:requires_active_enforcement(institutional_legitimacy_crisis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_legitimacy_crisis, institutional_leadership).
narrative_ontology:constraint_beneficiary(institutional_legitimacy_crisis, regulatory_capture_beneficiaries).
narrative_ontology:constraint_victim(institutional_legitimacy_crisis, institutional_constituency).
narrative_ontology:constraint_victim(institutional_legitimacy_crisis, public_trust_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INSTITUTIONAL CONSTITUENCY (SNARE) — Citizens dependent on institutional services face maximal extraction with no exit. Cannot withdraw consent, relocate beyond jurisdiction, or access alternatives. Suppression is structural: legal dependency, resource concentration, information asymmetries. Experiences pure extraction mechanism.
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERMEDIATE INSTITUTIONAL ACTORS (TANGLED ROPE) — Mid-level bureaucrats, local administrators, frontline professionals benefit from institutional stability and career structure while bearing extraction through compliance costs, ethical compromise, and career risk of dissent. Some genuine coordination function (institutional continuity, service delivery) exists alongside asymmetric extraction (upward accountability, rule compliance).
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL LEADERSHIP (ROPE) — Leadership perceives constraint as coordination mechanism: managing legitimacy narrative, distributing symbolic authority, coordinating narrative consistency across institution. Experiences arbitrage options: can shift messaging, reallocate blame, coordinate institutional response. Net beneficiary of legitimacy flow.
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORM MOVEMENTS AND ACCOUNTABILITY NETWORKS (SCAFFOLD) — Organized reform agents (NGOs, transparency advocates, distributed accountability mechanisms) see legitimacy crisis as temporary problem with structured exit path: institutional reform, participatory governance, distributed legitimacy verification can reduce crisis extractiveness. Has sunset clause: alternative legitimacy structures can be built out.
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL PERFORMANCE THEATER (PITON) — Formal legitimacy rituals (public consultations, transparency reports, accountability hearings) persist as degraded mechanism: theaters of legitimacy that maintain appearance of consent without substantive participation. Real legitimacy-building function has atrophied; constraint maintained through inertia and historical expectation. Theater ratio highest from this perspective.
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From universalizing analytical context, some legitimacy gap is inherent to institutional governance: any system with power differentials generates legitimacy crises as inevitable feature of social organization. This perspective risks naturalizing what is actually a contingent institutional arrangement as immutable feature of governance.
constraint_indexing:constraint_classification(institutional_legitimacy_crisis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_legitimacy_crisis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_legitimacy_crisis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_legitimacy_crisis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_legitimacy_crisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_legitimacy_crisis, TR),
    TR >= 0.70.

:- end_tests(institutional_legitimacy_crisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Institutional leadership captures legitimacy, resources, and authority during crisis, but extraction is not as severe as pure predation because some genuine coordination function (institutional continuity, service provision, governance structure) persists. The extraction is embedded within coordination, not replacing it. Suppression (0.65): High. Significant barriers to exit include legal dependency on institutional services, resource concentration, epistemic barriers (limited information about alternatives), and identity fusion (constituency identity tied to institutional role). Suppression is rising as crisis deepens because desperation increases institutional dependence. Theater ratio (0.78): High and rising. Formal legitimacy rituals have intensified as structural legitimacy eroded. Public consultations, transparency reports, accountability processes create appearance of constituency voice without shifting substantive authority. The progression from 0.55 to 0.78 shows Goodhart drift: as legitimacy became scarce, institutions invested in theater to maintain appearance. Claimed type (Tangled Rope): Institutional coordination function is genuine (governance requires coordination), but is hybrid with asymmetric extraction (leadership captures disproportionate benefit, constituency bears compliance cost and legitimacy burden).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the structural asymmetry of the legitimacy extraction mechanism. Leadership and constituency are seeing fundamentally different constraints: leadership sees coordination (rope/managing complexity), constituency sees extraction (snare/no exit). The gap between these perspectives is diagnostic of the constraint's hybrid nature (tangled_rope) — both perspectives are correct about their own structural position. The gap is not resolvable by argument or negotiation at the leadership/constituency level because the positions are structurally asymmetric. Reform movements and accountability networks represent the only perspective that sees a structural path beyond the asymmetry (scaffold) — by building alternative legitimacy verification mechanisms, they create exit options that don't exist within the existing institutional frame.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the legitimacy extraction mechanism. Institutional leadership has low d (0.15-0.20: beneficiary with arbitrage options) — benefits from extraction flow, can reallocate narrative, shift blame, coordinate response. Constituency has high d (0.90-0.95: victim with trapped exit) — bears extraction through compromised institutional performance, cannot exit, has no arbitrage options. Intermediate actors have moderate d (0.55-0.65: partially victim through compliance costs and ethical compromise, but also benefit from institutional role and stability). Reform agents have moderate d (0.50-0.60: organized victims with constrained but structured exit pathways through institutional reform). The f(d) sigmoid maps these d values to experienced extractiveness chi: institutional leadership experiences low chi (negative, net benefit); constituency experiences high chi (maximum extraction); intermediate and reform agents experience moderate chi reflecting their mixed position.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC EXEMPLAR: This constraint resolves mandatrophy by showing that the six types reflect genuine structural asymmetries rather than subjective interpretations. The mandatrophy is not 'which type is correct?' but 'what structural reform would align perspectives toward rope or scaffold?' (a) If the institutional leadership perspective could be moved from rope (net beneficiary) toward tangled_rope or snare (bearing extraction costs), the constraint would compress toward fewer types — this would require institutional reforms that force leadership to internalize legitimacy costs. (b) If the constituency perspective could be moved from snare (trapped) toward constrained or mobile (exit options), the constraint would decompose — this would require building viable alternative institutional structures. (c) If the theater_ratio could decline from 0.78 toward 0.40 (genuine legitimacy function restored), the piton classification would shift upward toward rope. The analytical observer's mountain is a false summit — the constraint is not a law of nature but a contingent institutional arrangement amenable to structural reform.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_measurement_basis,
    'Is the measured legitimacy crisis structural (institutional performance genuinely degraded) or observational (measurement methodology changed, revealing pre-existing extraction)?',
    'Longitudinal measurement of constituency satisfaction, behavioral compliance, voluntary participation rates across stable methodology; comparison with historical data using same measurement protocol',
    'If structural: the crisis is real institutional degradation requiring reform. If observational: the extraction mechanism was always present but newly visible; classification shifts from crisis-period Snare to persistent Snare with changed detection threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_measurement_basis, empirical, 'Whether legitimacy crisis is structural institutional degradation or observational measurement artifact').

omega_variable(
    identity_locked_institutional_actor,
    'Are institutional actors (particularly mid-level) constrained by material barriers to exit or identity-fused with institutional role such that exit is identity-annihilating?',
    'Post-exit trajectory analysis: do intermediate institutional actors who leave during crisis demonstrate cognitive reframing and identity shift, or do they carry institutional identity and compliance patterns into new contexts?',
    'If identity_locked: the extraction mechanism operates partly through internalized identity, making suppression partly internal; constraint survives actor turnover and persists in new institutional contexts. If constrained: barriers to exit are primarily material (career risk, pension, reputation damage); constraint could be broken by external structural change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_institutional_actor, empirical, 'Whether institutional actors are identity-locked or materially constrained').

omega_variable(
    constituency_exit_option_availability,
    'Are exit options for institutional constituency genuinely trapped or merely identity_locked — do functionally viable alternatives exist but are cognitively unavailable?',
    'Audit of actual alternative institutional structures and their accessibility; assessment of whether constituency awareness/information barriers or material barriers are primary constraint on exit exercise',
    'If trapped: pure material barriers; crisis severity is maximum. If identity_locked: constituency has internalized institutional dependence and inevitability; could cognitively exit through identity reframe but materially trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constituency_exit_option_availability, empirical, 'Whether constituency faces material traps or cognitive identity lock').

omega_variable(
    leadership_extraction_intentionality,
    'Does leadership extraction flow from deliberate strategy or from institutional logic/incentive structure that leadership experiences as constraint rather than as tool?',
    'Leadership testimony, decision-making documentation, institutional incentive structure analysis; comparison of rhetoric (what leadership claims to intend) with structural outcomes',
    'If deliberate strategy: classification firm toward upper-range extractiveness. If structural inevitability experienced by leadership: leadership may be trapped by institutional role (identity_locked), making it a case of multi-level identity-locked coordination failure, not intentional snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_extraction_intentionality, empirical, 'Whether leadership extraction is deliberate or structurally inevitable').

omega_variable(
    reform_sunset_mechanism_viability,
    'Are the distributed accountability and participatory governance mechanisms actually building institutional alternatives, or are they performative channels for managed dissent?',
    'Track institutional power migration: do reforms actually shift decision-making authority, resource allocation, or accountability chains? Or do formal reforms coexist with unchanged informal power structures?',
    'If building viable alternatives: scaffold classification confirmed; legitimacy crisis has genuine sunset path. If performative: reform movements are themselves pitons — theater channels that maintain appearance of change without substantive power shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_sunset_mechanism_viability, empirical, 'Whether reform mechanisms are building viable alternatives or are performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_legitimacy_crisis, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inst_tr_t0, institutional_legitimacy_crisis, theater_ratio, 0, 0.55).
narrative_ontology:measurement(inst_tr_t3, institutional_legitimacy_crisis, theater_ratio, 3, 0.68).
narrative_ontology:measurement(inst_tr_t6, institutional_legitimacy_crisis, theater_ratio, 6, 0.78).

% Extraction over time
narrative_ontology:measurement(inst_be_t0, institutional_legitimacy_crisis, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(inst_be_t3, institutional_legitimacy_crisis, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(inst_be_t6, institutional_legitimacy_crisis, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_legitimacy_crisis, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_legitimacy_crisis, public_trust_epistemic_commons).
narrative_ontology:affects_constraint(institutional_legitimacy_crisis, regulatory_capture_institutional_drift).
narrative_ontology:affects_constraint(institutional_legitimacy_crisis, constituency_voice_suppression).

% DUAL FORMULATION NOTE:
% Institutional legitimacy crisis is upstream of specific institutional failures (regulatory capture, suppression of constituency voice, epistemic commons degradation). Each downstream constraint has its own extractiveness value reflecting the domain-specific extraction mechanism; the legitimacy crisis represents the meta-level constraint through which those mechanisms operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_legitimacy_crisis, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
