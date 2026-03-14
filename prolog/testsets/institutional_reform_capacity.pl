% ============================================================================
% CONSTRAINT STORY: institutional_reform_capacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_reform_capacity, []).

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
 *   constraint_id: institutional_reform_capacity
 *   human_readable: Institutional Reform Capacity Constraint
 *   domain: institutional_governance/organizational_dynamics
 *
 * SUMMARY:
 *   Institutional reform capacity describes the structural constraint on
 *   organizations' ability to adapt, transform, or correct course in response
 *   to internal dysfunction or external demand for change. This constraint
 *   operates across all organizational types — government agencies,
 *   corporations, nonprofits, educational institutions — and exhibits a
 *   characteristic pattern: initial reform pressure triggers performative
 *   response (task forces, consultations, new committees) that creates the
 *   appearance of change while preserving incumbent power and extracting
 *   resources from reform constituencies. The constraint is both a
 *   coordination problem (institutions must adapt to survive) and an
 *   extraction mechanism (incumbents benefit from blocking substantive reform
 *   while appearing responsive). The theater ratio rises over time as reform
 *   constituencies learn to expect performative responses and incumbents
 *   refine their ritualization of change processes. The extractiveness metric
 *   rises as suppression deepens — procedural complexity increases, reform
 *   costs rise, and the gap between reform promises and outcomes widens. This
 *   is a tangled rope constraint: genuine coordination function
 *   (institutional survival requires adaptation) coexists with asymmetric
 *   extraction (incumbents capture reform benefits while shifting costs onto
 *   reformers).
 *
 * KEY AGENTS:
 *   - Trapped Reform Constituency: Powerless/trapped agents (citizens, activists, reform advocates) — seek institutional change but cannot exit; bear full suppression cost
 *   - Mid-Level Reformer: Moderate/constrained institutional actors (mid-management, professional staff, internal change agents) — experience both coordination function and extraction; face career risk
 *   - Reform-Sponsoring Institution: Institutional actor with arbitrage exit (external funders, international organizations, oversight bodies) — benefit from reform but can shift resources elsewhere
 *   - International Reform Network: Organized global actors with mobile exit (transnational networks, technical assistance providers, standards bodies) — see reform bottleneck as solvable through structured support
 *   - Incumbency Defense System: Institutional power holders (senior executives, entrenched bureaucrats, formal authority holders) — benefit from maintaining status quo while appearing responsive
 *   - Analytical Observer: Civilizational perspective (organizational theorists, institutional analysts) — risks naturalizing contingent power structures as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_reform_capacity, 0.58).
domain_priors:suppression_score(institutional_reform_capacity, 0.65).
domain_priors:theater_ratio(institutional_reform_capacity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_reform_capacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_reform_capacity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_reform_capacity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_reform_capacity, tangled_rope).
narrative_ontology:human_readable(institutional_reform_capacity, "Institutional Reform Capacity Constraint").
narrative_ontology:topic_domain(institutional_reform_capacity, "institutional_governance/organizational_dynamics").

domain_priors:requires_active_enforcement(institutional_reform_capacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_reform_capacity, incumbent_power_holders).
narrative_ontology:constraint_beneficiary(institutional_reform_capacity, entrenched_bureaucratic_interests).
narrative_ontology:constraint_victim(institutional_reform_capacity, reform_constituencies).
narrative_ontology:constraint_victim(institutional_reform_capacity, institutional_renewal_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED REFORM CONSTITUENCY (SNARE) — Citizens, activists, and reform advocates bear the cost of institutional stagnation with no exit option. Trapped within geographic and citizenship boundaries; cannot leave the institution they seek to reform. Suppression operates through resource asymmetry, media capture, and procedural complexity. Maximum experienced extraction for powerless agents in trapped exit mode.
constraint_indexing:constraint_classification(institutional_reform_capacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-LEVEL REFORMER (TANGLED ROPE) — Bureaucratic reformers, mid-level managers, and progressive internal actors experience genuine coordination function: institutional adaptation is necessary for long-term survival. But they also face extraction: resistance from senior cadres, career risk, and institutional antibodies that suppress reform initiatives. Constrained exit — can leave the institution but at significant cost. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(institutional_reform_capacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REFORM-SPONSORING INSTITUTION (ROPE) — External funders, international organizations, or leadership bodies that sponsor reform experience it as pure coordination. They benefit from institutional adaptation and have arbitrage exit: can shift funding to other institutions if reform fails. Experiences the constraint as manageable coordination, not extraction. Low effective extraction due to arbitrage options.
constraint_indexing:constraint_classification(institutional_reform_capacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL REFORM NETWORK (SCAFFOLD) — Transnational networks of reform practitioners, technical assistance providers, and international standards bodies see the bottleneck as solvable through structured support with a sunset. The constraint is temporary: as capacity and norms shift, the need for external scaffolding declines. Organized agents with mobile exit options; see clear time-bound pathway out.
constraint_indexing:constraint_classification(institutional_reform_capacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INCUMBENCY DEFENSE RITUAL (PITON) — Senior power holders maintain performative reform processes (task forces, consultations, advisory committees) that preserve the appearance of openness while preventing substantive change. Theater ratio is high: ritual engagement without institutional transformation. The process persists through inertia because alternatives haven't displaced it, not because it functions. Arbitrage exit for incumbents — they can exit into alternative positions if the institution becomes unstable.
constraint_indexing:constraint_classification(institutional_reform_capacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the broadest analytical frame, institutional inertia appears as an immutable law of organizational dynamics: all institutions accumulate resistance to change proportional to their size and age. This perspective risks naturalizing what are actually contingent power structures and extractive mechanisms as inherent organizational physics. The engine's false summit detector should identify this classification as risk of naturalization.
constraint_indexing:constraint_classification(institutional_reform_capacity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_reform_capacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_reform_capacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_reform_capacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_reform_capacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_reform_capacity, TR),
    TR >= 0.70.

:- end_tests(institutional_reform_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts from reform constituencies through multiple mechanisms: time and resource costs of engaging in reform processes that do not deliver change, opportunity costs of pursuing failed internal reform vs. alternative strategies, and demoralization from repeated cycles of promise-and-disappointment. The extraction is not maximal (0.72+) because some institutions do reform, external pressure does create change, and scaffolding support does sometimes overcome inertia. The measurement trajectory (0.38 → 0.58 over 15 years) reflects deepening extraction as reform constituencies internalize that the process is increasingly performative. Suppression (0.65): High. Multiple suppression mechanisms operate: procedural barriers (complexity, time requirements, resource asymmetry), cognitive barriers (narratives naturalizing inertia as organizational law), institutional antibodies (selective enforcement against reformers, career penalties for dissent), and structural barriers (power concentrated in hands of those benefiting from status quo). Suppression is high enough that exit options are genuinely constrained for powerless agents and identity-locked for many mid-level reformers. Theater ratio (0.68): High and rising. Reform processes in mature institutions become increasingly performative: consultations that don't inform decisions, task forces that produce reports that are shelved, participatory committees whose recommendations are overruled, and advisory processes whose outcomes are predetermined. The rise from 0.35 to 0.68 reflects that as constituencies learn to recognize performance, institutions must elaborate ritual more extensively to maintain the appearance of responsiveness.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival diversity. The reform-sponsoring institution sees a coordination problem (Rope) — they benefit from institutional adaptation and have exit options. The international reform network sees a solvable temporary problem with sunset (Scaffold) — external scaffolding builds internal capacity. The incumbent power holders experience performative change processes (Piton) — ritual engagement without substantive transformation. Mid-level reformers see mixed coordination and extraction (Tangled Rope) — the institution must adapt but they bear the cost of adaptation while incumbents capture benefits. The trapped reform constituency sees pure extraction (Snare) — they provide political demand and take morale damage while nothing changes. The civilizational analytical observer risks seeing immutable organizational law (Mountain) — institutional inertia as inherent to all complex organizations. The perspectival gap emerges from different structural positions relative to the extraction flow: agents with arbitrage options (can exit to better institutions) see coordination; agents with trapped options (cannot exit) see extraction; agents with organized alternatives (can build parallel structures) see temporary scaffolding.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values reflect agents' structural positions relative to this specific constraint. Powerless trapped reformers derive d ≈ 0.95 (full targets) from victim status + trapped exit, experiencing maximum f(d) ≈ 1.42. Mid-level reformers derive d ≈ 0.55 from moderate power + constrained exit + mixed victim/beneficiary status (they suffer from reform failure but also have some institutional investment), experiencing f(d) ≈ 0.75. Reform sponsors derive d ≈ 0.15 from institutional power + arbitrage exit + beneficiary status (they benefit from institutional stability), experiencing f(d) ≈ -0.01 (negative effective extraction). Incumbent power holders derive d ≈ 0.05 from institutional power + arbitrage exit + primary beneficiary status, experiencing f(d) ≈ -0.12. The analytical observer at civilizational scope derives d ≈ 0.72 (canonical), experiencing f(d) ≈ 1.15 (moderate-high). The directionality gradient explains perspectival gaps: what appears as coordination problem to beneficiaries appears as extraction trap to powerless agents.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that the classification depends entirely on structural position. There is no single 'true' classification — the presheaf over observation positions is the answer. The tangled rope claim is validated by the presence of BOTH genuine coordination function (institutions must adapt) and BOTH beneficiaries (incumbents) and victims (reformers). The satisfaction of all three Tangled Rope gates (beneficiaries declared, victims declared, requires_active_enforcement=true) confirms the classification. The false summit (mountain classification) at the analytical context reveals a real diagnostic: institutional inertia is NOT an immutable law but a system of power relationships that can be restructured. The scaffold classification at the global-organized context is also validated: international reform scaffolding DOES create sunset effects when capacity-building succeeds. The piton classification reveals that performative reform processes are not coordination failures but a specific extraction mechanism — the ritual of responsiveness that maintains legitimacy without changing power. The snare classification at the powerless/trapped position is the ground truth for reform constituencies: they are locked in with no exit and bearing full suppression cost. The constraint's mandatrophy is resolved by recognizing that all six classifications are structurally correct from their respective positions — the true constraint story is the presheaf, not any single cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reform_success_metrics_ambiguity,
    'What observable evidence distinguishes genuine institutional reform from performative change theater?',
    'Longitudinal behavioral tracking of institutional outputs post-reform: do downstream decisions change, or do outputs remain identical despite new rhetoric? Measurement of decision-maker power distribution before/after reform processes.',
    'If theater masquerades successfully: extractiveness is actually higher than measured (genuine reform prevention hidden behind rhetoric). If reform achieves substantive change: extractiveness declines and constraint transitions toward rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_success_metrics_ambiguity, empirical, 'Distinguishing genuine reform from performative institutional change').

omega_variable(
    incumbency_leverage_mechanism,
    'To what degree do incumbents exploit reform processes to consolidate power by coopting and channeling reform demand?',
    'Comparative analysis of reform outcomes across institutions: does reform process increase or decrease incumbent control? Do new policies strengthen incumbent enforcement mechanisms or weaken them?',
    'If incumbents systematically co-opt reform: suppression score is understated; constraint should reclassify toward snare at broader scopes. If some reform efforts break incumbency lock: extraction is lower than measured.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incumbency_leverage_mechanism, empirical, 'Extent of incumbency co-optation of reform processes').

omega_variable(
    structural_vs_cultural_barriers,
    'Are the primary barriers to reform structural (power distribution, resource control, formal authority) or cultural (institutional identity, narrative legitimacy, identity fusion)?',
    'Decomposition analysis: test whether removing formal barriers (new legislation, external mandate) produces reform without cultural shift, and vice versa. Identify which barrier type shows greater resistance across multiple institutions.',
    'If primarily structural: reform requires power redistribution (high conflict, time-intensive). If primarily cultural: reform requires narrative reframing (faster but fragile to incumbency counter-narratives). Mixed barriers require dual intervention with different timelines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_cultural_barriers, conceptual, 'Whether reform barriers are primarily structural or cultural').

omega_variable(
    exit_option_asymmetry,
    'Do reform constituencies face genuine resource barriers to exit (trapped), or primarily identity fusion with the institution (identity_locked)?',
    'Post-exit longitudinal study: for reformers who leave the institution, do suppression mechanisms persist? If reformers carry internal suppression post-exit, the binding was identity-based; if suppression releases, it was structural.',
    'If identity_locked: constraint operates through cognitive capture; reform requires identity reframing in addition to structural change. If trapped: reform requires lowering material barriers. Different intervention approaches needed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_asymmetry, empirical, 'Whether constituency barriers are material or identity-based').

omega_variable(
    reform_coalition_critical_mass,
    'What threshold of organized internal support is necessary to overcome incumbency resistance and produce substantive reform?',
    'Comparative institutional analysis: track reform success rates against percentage of middle management and professional staff actively supporting change. Identify critical mass threshold.',
    'If low threshold (< 20%): reform is more likely, constraint is weaker. If high threshold (> 50%): reform rarely succeeds, constraint is stronger. Determines whether powerless agents can coalesce into organized reform force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_coalition_critical_mass, empirical, 'Critical mass threshold for successful institutional reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_reform_capacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irc_tr_t0, institutional_reform_capacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(irc_tr_t5, institutional_reform_capacity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(irc_tr_t10, institutional_reform_capacity, theater_ratio, 10, 0.68).
narrative_ontology:measurement(irc_tr_t15, institutional_reform_capacity, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(irc_be_t0, institutional_reform_capacity, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(irc_be_t5, institutional_reform_capacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(irc_be_t10, institutional_reform_capacity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(irc_be_t15, institutional_reform_capacity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_reform_capacity, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_reform_capacity, institutional_inertia).
narrative_ontology:affects_constraint(institutional_reform_capacity, policy_implementation_gap).
narrative_ontology:affects_constraint(institutional_reform_capacity, leadership_succession_bottleneck).

% DUAL FORMULATION NOTE:
% Institutional reform capacity is decomposed into three related constraints: institutional_inertia (the structural resistance to change, ε≈0.15, Mountain), policy_implementation_gap (the execution bottleneck after formal reform decisions, ε≈0.52, Tangled Rope), and leadership_succession_bottleneck (the power transfer constraint, ε≈0.48, Tangled Rope). This story addresses the general reform capacity bottleneck that encompasses all three. Each specialized constraint has its own extraction mechanisms and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_reform_capacity, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
