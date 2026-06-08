% ============================================================================
% CONSTRAINT STORY: infrastructure_maintenance_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_maintenance_regime, []).

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
 *   constraint_id: infrastructure_maintenance_regime
 *   human_readable: Dutch Flood-Control Maintenance Regime (Post-1953)
 *   domain: disaster_preparedness/institutional_memory/infrastructure_governance
 *
 * SUMMARY:
 *   The Dutch flood-control maintenance regime represents a canonical
 *   institutional piton: a constraint that arose from genuine coordination
 *   necessity (post-1953 flood disaster) but has atrophied functionally while
 *   being maintained theatrically through ceremonial compliance. The regime
 *   exhibits classic drift-to-failure mechanics — generational knowledge
 *   loss, suppression of costly standards upgrades in response to climate
 *   data, and increasing theater-to-function ratio — masked by persistent
 *   institutional performance of competence. The constraint's trajectory from
 *   rope (1953-1970: genuine coordination and knowledge transfer) through
 *   tangled_rope (1970-1990: coordination with increasing extraction as costs
 *   rise) to piton (1990-present: theater-only maintenance, function
 *   atrophied, institutional inertia primary driver) reveals how
 *   catastrophe-driven institutional memory can degrade across
 *   non-catastrophe generations. The measurement data shows: theater_ratio
 *   rising from 0.22 (1953, high functional content) to 0.68 (2023, high
 *   ceremonial content), with most increase occurring 1953-1993 (rapid
 *   atrophy across the first cohort turnover) then plateauing. This suggests
 *   the piton entered stable degradation state around 1993 — further decay
 *   has slowed because what remained to degrade is minimal. Suppression
 *   requirement (institutional effort to maintain the fiction of competence)
 *   shows parallel rise, indicating that keeping the regime alive requires
 *   increasingly active suppression of alternatives (delta adaptation,
 *   managed retreat) and questions (drill effectiveness, climate
 *   integration). The regime now extracts value primarily through
 *   institutional maintenance costs while suppressing awareness of its
 *   degraded state.
 *
 * KEY AGENTS:
 *   - Population at Risk: Powerless/trapped (powerless/biographical/trapped/national) — Located in floodplains, dependent on dike system, cannot audit competence, bears catastrophic cost if regime fails
 *   - Dike Maintenance Bureaucracy: Institutional beneficiary (institutional/generational/constrained/national) — Captures budget allocation, employment, and mandate justification; benefits from regime's inertia
 *   - Engineering Profession: Institutional beneficiary (institutional/immediate/arbitrage/regional) — Captures contract work, professional legitimacy, and certification authority; has exit and mobility options
 *   - Water Board Districts: Institutional actor (institutional/biographical/constrained/national) — Embedded operatives performing ceremonial compliance; know the regime is degraded but constrained by law and budget
 *   - Climate Science Community: Organized challenger (organized/generational/constrained/global) — Provides coordination function (updated standards) but victimized by suppression of their data in standards upgrade
 *   - Delta Adaptation Paradigm: Organized alternative (organized/civilizational/mobile/global) — Emerging movement proposing sunset of traditional regime; has exit paths and alternatives in view
 *   - Analytical Observer: Civilizational position (analytical/civilizational/analytical/universal) — Risks naturalizing institutional contingency as inevitable organizational decay
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_maintenance_regime, 0.38).
domain_priors:suppression_score(infrastructure_maintenance_regime, 0.62).
domain_priors:theater_ratio(infrastructure_maintenance_regime, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, extractiveness, 0.38).
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(infrastructure_maintenance_regime, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_maintenance_regime, piton).
narrative_ontology:human_readable(infrastructure_maintenance_regime, "Dutch Flood-Control Maintenance Regime (Post-1953)").
narrative_ontology:topic_domain(infrastructure_maintenance_regime, "disaster_preparedness/institutional_memory/infrastructure_governance").

domain_priors:requires_active_enforcement(infrastructure_maintenance_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_maintenance_regime, dike_maintenance_bureaucracy).
narrative_ontology:constraint_beneficiary(infrastructure_maintenance_regime, engineering_profession).
narrative_ontology:constraint_victim(infrastructure_maintenance_regime, population_at_risk).
narrative_ontology:constraint_victim(infrastructure_maintenance_regime, fiscal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULATION AT RISK (SNARE) — Structurally trapped by geography. No exit option from the floodplain. Bears full cost if maintenance regime fails. Cannot audit actual competence, cannot demand rehearsal, cannot exit the jurisdiction. Maximum extraction: the regime suppresses awareness of its own degradation while maintaining the population's dependence on its claimed protection.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DIKE MAINTENANCE BUREAUCRACY (PITON) — Constrained by statutory mandate and fiscal budget. Maintains ceremonial compliance with post-1953 standards (drills, inspections, reports) but has lost contact with the functional knowledge required to execute those standards under actual catastrophe conditions. High theater ratio: the regime performs competence through routine rather than demonstrating it. Atrophied function (actual emergency response capacity) maintained theatrically (annual drills) to justify continued institutional existence and budget allocation.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENGINEERING PROFESSION (ROPE) — Benefits from the maintenance regime through employment, contract opportunities, and professional legitimacy. Experiences the constraint as coordination: dike standards, inspection protocols, and professional certification enable large-scale coordination of flood prevention across multiple municipalities. Engineers can arbitrage by moving between jurisdictions, consulting firms, academic positions. Net beneficiary with significant agency.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: CLIMATE SCIENCE COMMUNITY (TANGLED ROPE) — Provides the coordination function: rising sea levels and intensified precipitation patterns require updated maintenance standards. But the regime suppresses the implications of climate science data, maintaining 1953-era design standards while sea level rises. The climate science community is both integrated into the regime (their data is cited in official reports) and victimized by it (their warnings are deflated by the regime's inertial resistance to costly upgrades). Mixed coordination-extraction with active enforcement of outdated standards.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DELTA ADAPTATION PARADIGM (SCAFFOLD) — Emerging organizational movement (water boards, climate adaptation initiatives, international delta partnerships) proposing a sunset for the traditional maintenance regime. Delta adaptation sees the current regime as a transitional constraint meant to be replaced by dynamic adaptive management, nature-based solutions, and managed retreat in high-risk zones. Has sunset logic built in: as adaptation infrastructure matures, the traditional dike-centric regime becomes obsolete. Organized agents have exit paths and alternatives.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: WATER BOARD DISTRICT (PITON) — Individual water boards perform the ceremonial maintenance routine (annual dike walks, inspection reports, drill exercises) as required by law. They are embedded in the regime and constrained by fiscal and jurisdictional limits. They experience the regime as degraded — they know the drills are largely performative and the inspection capacity is insufficient for actual emergency response — but continue the ritual because it maintains the legal fiction of competence and justifies budget allocation. Theater ratio high at this level.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the decay of institutional memory across generational boundaries is an inherent property of human organizations: knowledge held in the minds of a cohort cannot transfer to the next generation without explicit transmission mechanisms, and those mechanisms degrade naturally over time as institutions prioritize routine over renewal. This perspective sees the regime's atrophy as an unavoidable consequence of organizational aging, not a contingent institutional failure. However, the structural data contradicts this reading — the regime exhibits classic false-summit characteristics: it has identifiable beneficiaries (the bureaucracy, the profession), it suppresses alternatives (delta adaptation, managed retreat), and it maintains itself through theatrical compliance rather than actual competence. The 'inherent to human nature' framing naturalizes what is actually a contestable institutional arrangement.
constraint_indexing:constraint_classification(infrastructure_maintenance_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_maintenance_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_maintenance_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_maintenance_regime, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_maintenance_regime, TR),
    TR >= 0.70.

:- end_tests(infrastructure_maintenance_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38, measured 2023): Moderate. The regime extracts through multiple mechanisms: (1) cost suppression — avoiding expensive climate adaptation by maintaining outdated standards, (2) institutional maintenance — the bureaucracy survives on fiscal allocation justified by the regime's nominal function, (3) knowledge suppression — alternatives (delta adaptation, managed retreat) are suppressed to protect the regime's necessity. The value is moderate rather than high because the extraction is partly masked by institutional theater — the regime's beneficiaries (bureaucracy, profession) capture real benefits, but much of the extraction flows to cost avoidance rather than direct appropriation. Suppression (0.62, measured 2023): Moderate-high. The population is suppressed through: information asymmetry (they cannot audit actual competence), geographic trap (no exit from floodplains), and institutional claims of protection (the regime's fiction that competence is maintained). The profession and bureaucracy are constrained by legal mandate and fiscal limits — they cannot easily exit or propose radical alternatives. Climate science is suppressed through institutional absorption (its data is cited but not integrated into standards). Delta adaptation is suppressed through budget competition and institutional resistance to regime obsolescence. Theater ratio (0.68, measured 2023): High. The regime's primary function is now ceremonial: annual dike walks, inspection reports, emergency drills, and budget justifications. The functional content (actual emergency response capacity under catastrophic conditions) is minimal and unmeasured. The regime maintains appearance of competence through ritual while its capacity to execute actual emergency response has atrophied — this is the piton signature.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the regime's beneficiaries and its victims is substantial and structurally rooted: (1) The bureaucracy sees rope (coordination of flood prevention) and piton (knows the ritual is largely performative but must maintain it). (2) The profession sees rope (legitimate professional coordination and employment). (3) The population at risk sees snare (trapped, dependent on a degraded system, suppressed from auditing its actual state). (4) The water boards see piton (performing ceremonial compliance while knowing competence is atrophied). (5) The climate science community sees tangled_rope (their coordination function is needed but suppressed). (6) The delta adaptation movement sees scaffold (this regime is temporary, being replaced by better alternatives). (7) The analytical observer sees mountain (naturalized aging of institutional memory) until the structural data reveals the false summit. The gap arises because the regime's beneficiaries have institutional incentives and exit options that insulate them from awareness of its degradation, while its victims (the population at risk) have neither incentives nor exit options to question the regime's theater. The beneficiaries experience the regime as functional coordination because it delivers to them (budget, employment, contracts); the victims experience it as false protection because it suppresses the actual state of their risk.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) computation derives from the agent's relationship to the extraction flow: beneficiaries experience low d (extraction flows toward them), victims experience high d (extraction flows from them). The population at risk occupies high d (trapped/powerless/national) — they are targets of suppression and cost avoidance. The dike maintenance bureaucracy occupies low d (institutional/constrained/arbitrage) — they are beneficiaries of budget allocation and mandate justification. The engineering profession occupies low d (institutional/immediate/arbitrage) — they benefit from contract work and certification authority, with exit options. The water board district occupies moderate-high d (institutional/biographical/constrained) — they experience mixed coordination (genuine function in emergency response) and extraction (enforcement of low-cost maintenance standards that suppress climate adaptation). The climate science community occupies high d (organized/generational/constrained) — they provide the coordination function but are victimized by suppression of their data. The delta adaptation movement occupies low d (organized/civilizational/mobile) — they have exit paths and alternatives. The effective extraction (χ) is amplified for trapped agents (population at risk) and damped for mobile agents (profession, delta adaptation). The analytical observer's attempted mountain classification falls to the false-summit detector because beneficiaries are declared and the naturalness claim is falsified by the suppression of alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The regime's founding mandate — coordinate flood prevention following the 1953 catastrophe — has outlived its functional form. The original function was coordination across generational knowledge transfer: encode 1953 lessons, train new cohorts, maintain competence, and prepare for future events. This mandate was live and necessary 1953-1970. By 1990-2000, the mandate had become a historical artifact: the institutional apparatus continued (drills, inspections, standards) but the actual coordination it aimed at (maintaining emergency response competence across generational boundaries) had degraded substantially. The regime persists now as institutional inertia (piton) — the bureaucracy maintains it because the mandate justifies budget allocation; the profession maintains it because it provides contracts; the water boards maintain it because law requires it. The population at risk is suppressed from recognizing the mandate has expired because the regime's theater (annual dike walks, official reports) claims ongoing competence. The remediation pathway is not repair of the existing regime but its sunset and replacement by the delta adaptation paradigm, which explicitly contains sunset logic and transition mechanisms. The false-summit mountain perspective naturalizes this as inevitable organizational aging ('competence always decays'); the structural reading reveals it as a contestable institutional arrangement with identifiable beneficiaries and suppressors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    competence_atrophy_mechanism,
    'What is the primary mechanism of competence loss — generational knowledge turnover, or active suppression of new standards due to cost and institutional inertia?',
    'Oral history interviews with retired and current water board personnel; analysis of training curriculum changes over decades; documentation of proposals for standards upgrades and their rejection pathways',
    'If primarily generational: the regime is a rope-type knowledge transfer problem fixable by better documentation and training. If primarily institutional suppression: the regime is a snare-type extraction mechanism using knowledge decay as cover for cost avoidance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_atrophy_mechanism, empirical, 'Competence loss due to generational turnover vs. institutional suppression').

omega_variable(
    climate_science_integration_status,
    'Are climate-driven changes to sea level and precipitation being formally incorporated into dike design standards, or are they documented but suppressed by cost-benefit arguments?',
    'Audit of official design standards vs. climate projection data cited in the same documents; cost-benefit analysis comparing 1953-era design maintenance to climate-adapted design implementation; timeline of standards revision proposals and their status',
    'If integrated: the regime is adapting and the scaffold perspective is real — slow but functional transition. If suppressed: the regime is actively trading population safety for maintenance cost, making it a snare with deliberate knowledge suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(climate_science_integration_status, empirical, 'Whether climate science is integrated into design standards or suppressed').

omega_variable(
    drill_effectiveness_measurement,
    'Do annual dike walks and emergency response drills demonstrate actual emergency response capacity, or are they ceremonial compliance with low correlation to actual performance under catastrophic conditions?',
    'Tabletop exercises with realistic parameters (unprecedented rainfall, multiple dike breaches, generational-lag conditions); comparison of drill performance metrics to post-hoc analysis of historical near-miss events; competence assessment by retired personnel who experienced 1953 and current personnel',
    'If drills are effective: the regime is functional rope-type coordination, theater ratio is lower than 0.68. If drills are ceremonial: the regime is a piton, theater ratio is confirmed at high levels, and failure risk is untracked.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_effectiveness_measurement, empirical, 'Whether emergency drills demonstrate actual response capacity or are ceremonial').

omega_variable(
    false_summit_institutional_mandate,
    'Is the post-1953 maintenance regime a genuine natural law of flood management, or a contingent institutional arrangement that naturalizes its own necessity to prevent costly alternatives from being considered?',
    'Comparative analysis: which flood-management systems without generational-continuity hazards (e.g., Japan, Denmark, Singapore) adopt similar ceremonial maintenance vs. which adopt dynamic adaptive approaches; cost-benefit analysis of regime continuity vs. managed retreat in high-risk zones; analysis of doctrinal claims (what academics and policymakers cite as ''proven'' flood management standards) vs. empirical performance data',
    'If truly inherent: mountain classification stands. If contingent and beneficiary-serving: engine''s false-summit detector will reclassify to tangled_rope or snare, revealing the naturalness claim as institutional mythology.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_institutional_mandate, conceptual, 'Natural law vs. contingent institutional arrangement with beneficiaries').

omega_variable(
    generational_knowledge_transfer_protocol,
    'What proportion of 1953-generation knowledge about emergency response, dike breach behavior, and triage decisions was explicitly codified vs. held tacitly and lost in generational transition?',
    'Archive analysis of post-1953 documentation (reports, training materials, protocol updates); interviews with personnel who trained under 1953-generation mentors; simulation performance comparison between knowledge-documented cohorts and knowledge-transition-gap cohorts',
    'If high codification: knowledge loss is minimal and the regime''s decay is primarily maintenance-cost avoidance. If low codification: tacit knowledge loss is severe and represents a structural vulnerability independent of institutional intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_knowledge_transfer_protocol, empirical, 'Proportion of knowledge explicitly codified vs. lost in generational transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_maintenance_regime, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_theater_1953, infrastructure_maintenance_regime, theater_ratio, 0, 0.22).
narrative_ontology:measurement(infra_theater_1963, infrastructure_maintenance_regime, theater_ratio, 10, 0.38).
narrative_ontology:measurement(infra_theater_1978, infrastructure_maintenance_regime, theater_ratio, 25, 0.54).
narrative_ontology:measurement(infra_theater_1993, infrastructure_maintenance_regime, theater_ratio, 40, 0.65).
narrative_ontology:measurement(infra_theater_2003, infrastructure_maintenance_regime, theater_ratio, 50, 0.68).
narrative_ontology:measurement(infra_theater_2023, infrastructure_maintenance_regime, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(infra_extract_1953, infrastructure_maintenance_regime, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(infra_extract_1963, infrastructure_maintenance_regime, base_extractiveness, 10, 0.18).
narrative_ontology:measurement(infra_extract_1978, infrastructure_maintenance_regime, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(infra_extract_1993, infrastructure_maintenance_regime, base_extractiveness, 40, 0.35).
narrative_ontology:measurement(infra_extract_2003, infrastructure_maintenance_regime, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(infra_extract_2023, infrastructure_maintenance_regime, base_extractiveness, 70, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(infra_supp_1953, infrastructure_maintenance_regime, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(infra_supp_1968, infrastructure_maintenance_regime, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(infra_supp_1983, infrastructure_maintenance_regime, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(infra_supp_2003, infrastructure_maintenance_regime, suppression_requirement, 50, 0.62).
narrative_ontology:measurement(infra_supp_2023, infrastructure_maintenance_regime, suppression_requirement, 70, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_maintenance_regime, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(infrastructure_maintenance_regime, 0.12).
narrative_ontology:affects_constraint(infrastructure_maintenance_regime, climate_data_suppression_in_standards).
narrative_ontology:affects_constraint(infrastructure_maintenance_regime, generational_knowledge_transfer_failure).
narrative_ontology:affects_constraint(infrastructure_maintenance_regime, delta_adaptation_paradigm).

% DUAL FORMULATION NOTE:
% The infrastructure maintenance regime can be decomposed into three structurally distinct constraints: (1) the coordination mechanism (water boards coordinating dike maintenance across jurisdictions — genuine rope), (2) the knowledge transfer mechanism (generational transmission of emergency response competence — degraded from rope to piton), (3) the standards update mechanism (integrating climate science into design standards — tangled_rope with climate science as victim). This story focuses on the combined regime; the component constraints are downstream constraints linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_maintenance_regime, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
