% ============================================================================
% CONSTRAINT STORY: isolated_team_psychological_cohesion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_isolated_team_psychological_cohesion, []).

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
 *   constraint_id: isolated_team_psychological_cohesion
 *   human_readable: Isolated Team Psychological Cohesion
 *   domain: organizational_psychology/group_dynamics
 *
 * SUMMARY:
 *   Isolated team psychological cohesion is a constraint that emerges when
 *   groups are structurally separated from larger social systems and develop
 *   internal bonding mechanisms to maintain function and collective identity.
 *   The constraint exhibits characteristics of both coordination (teams
 *   genuinely need internal cohesion to function under stress and separation)
 *   and extraction (leadership benefits from cohesion-induced compliance,
 *   psychological dependency reduces exit costs, and normative pressure
 *   suppresses individual autonomy). This story examines isolated teams
 *   across diverse contexts: remote research stations, military units, space
 *   missions, offshore platforms, and other environments where geographic or
 *   operational separation creates forced interdependence. The constraint's
 *   extractiveness has increased over the measurement interval (0.35 → 0.58)
 *   as theater ratio has risen (0.35 → 0.64), indicating that performative
 *   cohesion rituals have increasingly substituted for functional
 *   coordination. Early in isolation, bonding serves genuine coordination
 *   needs; over time, the constraint becomes increasingly theatrical as
 *   leadership uses cohesion mechanisms for control rather than function.
 *
 * KEY AGENTS:
 *   - Individual Team Members: Primary victims (powerless/trapped) — psychologically dependent on group approval; socially isolated from external reference groups; face exit barriers both material and psychological
 *   - Leadership Hierarchy: Primary beneficiary (institutional/arbitrage) — captures compliance benefits, reduces monitoring costs, maintains control through psychological mechanisms; possesses exit options and geographic/organizational mobility
 *   - Mid-Level Coordinators: Secondary victims/beneficiaries (moderate/constrained) — bear coordination labor but receive status and information access; experience mixed extraction and benefit; constrained by career dependence
 *   - Parent Organization: Institutional stakeholder (institutional/constrained) — operationally dependent on team cohesion but shares liability risk if cohesion mechanisms become destructive; constrained by duty of care frameworks
 *   - Organizational Culture Industry: Organized institutional actor (organized/mobile) — maintains and profits from cohesion discourse; promotes team-building interventions; benefits from naturalizing cohesion as essential and requiring professional expertise
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent psychological mechanisms as universal laws of group dynamics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(isolated_team_psychological_cohesion, 0.58).
domain_priors:suppression_score(isolated_team_psychological_cohesion, 0.68).
domain_priors:theater_ratio(isolated_team_psychological_cohesion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(isolated_team_psychological_cohesion, extractiveness, 0.58).
narrative_ontology:constraint_metric(isolated_team_psychological_cohesion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(isolated_team_psychological_cohesion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(isolated_team_psychological_cohesion, tangled_rope).
narrative_ontology:human_readable(isolated_team_psychological_cohesion, "Isolated Team Psychological Cohesion").
narrative_ontology:topic_domain(isolated_team_psychological_cohesion, "organizational_psychology/group_dynamics").

domain_priors:requires_active_enforcement(isolated_team_psychological_cohesion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(isolated_team_psychological_cohesion, leadership_hierarchy).
narrative_ontology:constraint_victim(isolated_team_psychological_cohesion, individual_team_members).
narrative_ontology:constraint_victim(isolated_team_psychological_cohesion, collective_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL TEAM MEMBER (SNARE) — Trapped by psychological dependency on group approval, social isolation from outside reference groups, and economic necessity. Exit costs include loss of community, identity dissolution, and material survival concerns. No external exit mechanism; internal psychological bonds are primary suppression mechanism. Maximum extraction experienced.
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MID-LEVEL TEAM COORDINATOR (TANGLED ROPE) — Constrained by career dependence and social pressure, but also benefits from leadership role and information access. Coordination function is genuine (team scheduling, communication relay); extraction is asymmetric (labor without control). Experiences both cohesion benefits and subordination costs.
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEADERSHIP STRUCTURE (ROPE) — Benefits from psychological cohesion as a coordination tool. Sees the constraint as solving a genuine collective action problem: isolated teams need internal coordination mechanisms to function under stress and separation. Leadership experiences low extraction cost — they possess exit options (relocation, organizational mobility) and capture disproportionate benefits (control, resource allocation, identity prestige).
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PARENT ORGANIZATION (TANGLED ROPE) — Institutionally constrained by operational necessity and liability frameworks. Coordination function: teams must cohere to function in isolation. Extraction: organization benefits from cohesion-induced compliance, reducing need for external monitoring. Has enforcement capacity but also shares risk if cohesion mechanisms become psychologically destructive.
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZATIONAL CULTURE INDUSTRY (PITON) — Team-building consultants, cohesion workshops, and psychological assessment tools form an institutional ecosystem that maintains discourse around 'team cohesion' as essential. The theater ratio reflects that much organizational cohesion work is performative (team retreats, bonding exercises) rather than functionally necessary. Organized actors in this space benefit from maintaining the belief that cohesion requires professional intervention, even as the primary function has degraded or been replaced by informal coordination.
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a sufficiently abstract perspective, psychological cohesion in isolated groups appears as a universal requirement of human organization: any group separated from larger social systems must develop internal bonds to survive. This perspective naturalizes what are actually contingent psychological mechanisms (attachment, loyalty, shared threat perception) as inevitable laws of group dynamics. However, the base properties contradict this naturalization — the constraint is enforced, not emergent; suppression is high, not minimal; theater is substantial, not absent.
constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isolated_team_psychological_cohesion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isolated_team_psychological_cohesion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(isolated_team_psychological_cohesion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(isolated_team_psychological_cohesion, TR),
    TR >= 0.70.

:- end_tests(isolated_team_psychological_cohesion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint begins with legitimate coordination function (teams need internal cohesion under isolation) but increasingly serves leadership control. The measured value reflects that genuine coordination (perhaps 0.25-0.35 at t=0) has been overlaid with extractive mechanisms (theater rising to 0.64 by t=8). By interval end, approximately 40% is functional coordination, 60% is performative control or psychological dependency engineering. Suppression (0.68): High. Individual team members face multiple suppression layers: (1) geographic isolation reducing external reference groups; (2) economic/material dependency on team resources; (3) information control by leadership; (4) psychological mechanisms (attachment, shared threat perception) that become self-reinforcing. Exit barriers are both material (geographic, financial) and psychological (identity fusion with team identity). Theater ratio (0.64): Moderate-high. Cohesion rituals (team meetings, bonding exercises, shared symbols, loyalty ceremonies) are partially functional (real information sharing, genuine relationship maintenance) but increasingly theatrical (performative displays of loyalty, ritualized conflict resolution, mandatory participation in bonding activities). The rise in theater over time indicates Goodhart drift: cohesion metrics (attendance at rituals, verbal expressions of solidarity) become targets, substituting for actual functional coordination.
 *
 * PERSPECTIVAL GAP:
 *   Leadership perceives Rope (coordination mechanism); members perceive Snare (extraction mechanism). This gap is the canonical signal of successful extraction masquerading as coordination. The parent organization occupies an intermediate position (Tangled Rope) — operationally dependent on cohesion but increasingly aware of its extractive dynamics. The analytical observer's mountain classification is a false summit: the naturalizing narrative ('human groups always develop cohesion under isolation') obscures the specific engineered mechanisms (selective information control, facilitated attachment, normalized dependency) that create the extraction. The piton perspective (culture industry) reveals that cohesion itself is becoming theater — the industry profits from justifying cohesion as essential while the actual functional requirements have stabilized or declined.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position relative to the extraction flow. Leadership (institutional/arbitrage) has low d ≈ 0.10-0.15: they are beneficiaries with high exit mobility, so f(d) is negative, producing negative effective extraction (they capture more than they pay). Individual members (powerless/trapped) have high d ≈ 0.92: they are victims with no exit options, so f(d) ≈ 1.42, producing maximum experienced extraction. Mid-level coordinators (moderate/constrained) have d ≈ 0.65: they are both victims and partial beneficiaries with exit barriers, producing f(d) ≈ 1.00, moderate extraction. The parent organization (institutional/constrained) has d ≈ 0.45: they are structurally dependent on the cohesion mechanism but increasingly constrained by liability frameworks, producing f(d) ≈ 0.50. The culture industry (organized/mobile) has d ≈ 0.35: they profit from the constraint but have exit options, producing f(d) ≈ 0.35. The analytical observer (analytical/analytical) has canonical d ≈ 0.73, producing f(d) ≈ 1.15. These d values feed into the chi formula with scope modifier σ(local=0.8 at perspective 1, regional=0.9 at perspective 2-4, global=1.2 for the industry perspective). The direction of benefit flow is from individual members → leadership hierarchy, mediated through the organization and reinforced by the culture industry.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint satisfies the tangled rope gate by possessing three required elements: (1) genuine coordination function — isolated teams require internal communication and collective decision-making to operate under geographic/operational separation; (2) asymmetric extraction — leadership captures disproportionate benefits (control, resource allocation, prestige) while members bear disproportionate costs (psychological dependency, exit barriers); (3) active enforcement — the constraint is deliberately engineered and maintained through organizational structures, communication controls, and psychological mechanisms. The mandatrophy is resolved by recognizing that the coordination function is real but degrading (theater rising from 0.35 to 0.64) and the extraction is increasing (base_extractiveness rising from 0.35 to 0.58). The constraint is not becoming pure extraction (Snare) because the coordination function never fully disappears — isolated teams genuinely need communication mechanisms. But it is becoming increasingly extractive as leadership uses cohesion-building mechanisms beyond their functional necessity, and the theater ratio indicates that performative ritual is substituting for actual functional coordination. The mandatrophy dissolves the false choice between 'cohesion is purely necessary coordination' and 'cohesion is purely exploitative control' — it is legitimately both, and the ratio between them is shifting over time in the direction of extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'Is the suppression mechanism primarily structural (physical isolation, economic dependency) or primarily internalized (identity fusion with the group)?',
    'Post-isolation longitudinal tracking: Do team members retain cohesion-derived psychological patterns after isolation ends and external reference groups become accessible? If patterns persist, suppression is internalized; if they dissipate, suppression was structural.',
    'If internalized: the constraint''s effective suppression is higher than the measured value suggests — the binding persists after barrier removal. Classification of individual members shifts from trapped (structural) toward identity_locked (cognitive). If structural: isolation itself is the suppression; removing isolation removes the constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether team cohesion suppression is structural or internalized').

omega_variable(
    functional_necessity_threshold,
    'What minimum level of cohesion is genuinely required for isolated team function, versus what level is leadership-imposed for control?',
    'Comparative analysis of high-function isolated teams with minimal bonding rituals vs. low-function teams with intensive cohesion programs. Measurement of operational outcomes (task completion, safety, resource efficiency) as function of measured cohesion level.',
    'If functional necessity is low (ε < 0.20): cohesion is primarily extraction mechanism, classification shifts toward Snare for all non-leadership perspectives. If functional necessity is high (ε > 0.50): coordination function is genuine, Tangled Rope classification justified. Current estimate (ε = 0.58) assumes mixed functional/extractive content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_necessity_threshold, empirical, 'Functional necessity threshold for team cohesion').

omega_variable(
    exit_option_authenticity,
    'Do team members actually have exit options (constrained or mobile), or is their situation genuinely trapped? Are exit options merely theoretical (geographic/economic barriers prevent actual departure)?',
    'Survey data on exit attempts and actual barriers. Comparison of stated exit costs with realized costs for members who did leave. Analysis of opportunity cost of exit (employment alternatives, relocation feasibility) within geographic and sectoral constraints.',
    'If exits are genuinely constrained: current classification (constrained → moderate extraction) is accurate. If exits are theoretical but practically impossible: all member perspectives shift from constrained/mobile to trapped, extractiveness increases to 0.70+, classification becomes Snare across all non-leadership perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_authenticity, empirical, 'Whether team member exit options are genuine or theoretical').

omega_variable(
    leadership_enforcement_intentionality,
    'Is the cohesion-building structure (shared rituals, psychological dependency, isolation from outside contact) deliberately engineered for control, or an incidental consequence of isolation?',
    'Document analysis: organizational communication, training materials, leadership guidance on team management. Comparative case analysis: teams where leadership actively discourages external contact vs. teams where external contact is incidental to geographic isolation.',
    'If deliberate: requires_active_enforcement = true is justified, Tangled Rope is correct classification. If incidental: extraction is unintended consequence, extractiveness value may be overstated, constraint may degrade toward Rope. If incidental but leadership exploits it: worst case — Snare with plausible deniability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_enforcement_intentionality, empirical, 'Whether cohesion engineering is deliberately enforced or incidental').

omega_variable(
    psychological_harm_reversibility,
    'Are psychological patterns induced by cohesion constraints (trauma bonding, anxious attachment, identity fusion) reversible after team dispersal, or do they constitute permanent modifications to individual psychology?',
    'Longitudinal mental health tracking post-isolation; comparison with baseline pre-isolation psychological assessments; clinical evaluation for PTSD, complex trauma, attachment disorder persistence.',
    'If reversible: constraint is classified by its active phase (Snare/Tangled Rope while active). If irreversible: suppression was not merely behavioral manipulation but neuropsychological restructuring — extractiveness should be rated higher (0.65+), constraint potentially reclassified as Snare even from moderate perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_harm_reversibility, empirical, 'Reversibility of psychological patterns induced by cohesion constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(isolated_team_psychological_cohesion, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itpc_tr_t0, isolated_team_psychological_cohesion, theater_ratio, 0, 0.35).
narrative_ontology:measurement(itpc_tr_t2, isolated_team_psychological_cohesion, theater_ratio, 2, 0.5).
narrative_ontology:measurement(itpc_tr_t4, isolated_team_psychological_cohesion, theater_ratio, 4, 0.62).
narrative_ontology:measurement(itpc_tr_t6, isolated_team_psychological_cohesion, theater_ratio, 6, 0.68).
narrative_ontology:measurement(itpc_tr_t8, isolated_team_psychological_cohesion, theater_ratio, 8, 0.64).

% Extraction over time
narrative_ontology:measurement(itpc_be_t0, isolated_team_psychological_cohesion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(itpc_be_t2, isolated_team_psychological_cohesion, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(itpc_be_t4, isolated_team_psychological_cohesion, base_extractiveness, 4, 0.54).
narrative_ontology:measurement(itpc_be_t6, isolated_team_psychological_cohesion, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(itpc_be_t8, isolated_team_psychological_cohesion, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(isolated_team_psychological_cohesion, attachment_coordination).
narrative_ontology:boltzmann_floor_override(isolated_team_psychological_cohesion, 0.12).
narrative_ontology:affects_constraint(isolated_team_psychological_cohesion, organizational_information_control).
narrative_ontology:affects_constraint(isolated_team_psychological_cohesion, leadership_authority_legitimacy).

% DUAL FORMULATION NOTE:
% Team cohesion decomposes into structurally distinct constraints in different contexts. (1) Coordination cohesion (ε ≈ 0.25, Rope) — genuine functional bonding required for remote team operations; (2) Psychological extraction cohesion (ε ≈ 0.72, Snare) — deliberate engineering of dependency for control. This story integrates both and measures the shift over time. Upstream constraints: geographic isolation, organizational separation, external communication barriers. Downstream constraints: organizational information control (leadership leverage derived from cohesion monopoly), leadership authority legitimacy (cohesion-derived authority becomes generalized organizational legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(isolated_team_psychological_cohesion, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
