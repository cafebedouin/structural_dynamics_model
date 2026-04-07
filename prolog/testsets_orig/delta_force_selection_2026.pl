% ============================================================================
% CONSTRAINT STORY: delta_force_selection_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_delta_force_selection_2026, []).

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
 *   constraint_id: delta_force_selection_2026
 *   human_readable: Delta Force (1st SFOD-D) Selection & Assessment
 *   domain: military/special_operations
 *
 * SUMMARY:
 *   Delta Force (1st Special Forces Operational Detachment-Delta) selection
 *   represents one of the most extreme exclusionary pressures in military
 *   institutions. The selection process filters ~200 officer and NCO
 *   candidates annually to produce ~20 successful completions — a 90% failure
 *   rate maintained deliberately to ensure only the most exceptional
 *   personnel enter the unit. This constraint exhibits the core structure of
 *   a snare: trapped aspirants with no viable exit options bear massive costs
 *   (18+ months of intensive evaluation, physical injury risk, career
 *   opportunity damage) while Delta Force and its institutional supporting
 *   structure extract validation, prestige, and operational capability from
 *   the selection mechanism. The extraction is partially justified by genuine
 *   operational requirements (Delta conducts some of the most dangerous
 *   missions in military operations) but is also partly sustained by
 *   institutional inertia, mystique maintenance, and gatekeeping of elite
 *   career advancement. The constraint's theater_ratio (0.38) reflects that
 *   selection remains substantially functional — not primarily performative —
 *   distinguishing it from piton-class degradation. However, components of
 *   selection have drifted toward ritual (extreme psychological deprivation,
 *   arbitrary fitness metrics) as operational requirements have changed while
 *   selection protocols have remained stable.
 *
 * KEY AGENTS:
 *   - Candidate Pool (Trapped Aspirants): Primary victims (powerless/trapped) — commit 18+ months with ~90% failure rate; bear injury risk and career opportunity cost
 *   - Broader Officer Corps: Secondary victims (moderate/constrained) — suffer brain drain as ambitious personnel pursue Delta selection; face reduced career advancement if selection fails
 *   - Delta Force Institutional Capacity: Primary beneficiary (institutional/arbitrage) — extracts operational capability, personnel quality assurance, institutional validation
 *   - Training Command / Selection Administration: Secondary institutional beneficiary (organized/constrained) — manages selection apparatus; benefits from justification of special operations hierarchy; constrained by military bureaucracy
 *   - Reformed Selection Advocates: Organized reformers (organized/mobile) — perceive sunset pathway through competency-based assessment and technological modernization; possess policy influence but limited control
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements as immutable military requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(delta_force_selection_2026, 0.68).
domain_priors:suppression_score(delta_force_selection_2026, 0.82).
domain_priors:theater_ratio(delta_force_selection_2026, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(delta_force_selection_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(delta_force_selection_2026, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(delta_force_selection_2026, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(delta_force_selection_2026, snare).
narrative_ontology:human_readable(delta_force_selection_2026, "Delta Force (1st SFOD-D) Selection & Assessment").
narrative_ontology:topic_domain(delta_force_selection_2026, "military/special_operations").

domain_priors:requires_active_enforcement(delta_force_selection_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(delta_force_selection_2026, delta_force_institutional_capacity).
narrative_ontology:constraint_beneficiary(delta_force_selection_2026, operational_readiness_gatekeeping).
narrative_ontology:constraint_victim(delta_force_selection_2026, candidate_pool_trapped_aspirants).
narrative_ontology:constraint_victim(delta_force_selection_2026, military_career_opportunity_closure).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED ASPIRANT (SNARE) — Candidate commits 18+ months to selection with no guarantee of success. Failure closes doors: high physical risk of injury, reputational damage within military, career opportunity cost. Exit options are illusory — withdrawing signals weakness; cycling through selection multiple times entails compounding career damage. No viable alternative path to special operations leadership. Maximum extraction from a structurally powerless agent.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: BROADER OFFICER CORPS (SNARE) — Selection filters extract the highest-performing officers and NCOs from the general military population. Career incentives channel ambitious personnel toward Delta, knowing failure is likely. Success produces brain drain from conventional units; failure produces frustrated mid-career officers with reduced advancement prospects. Extraction is significant but not as total as the trapped aspirant — officers have some alternative career paths, but selection still narrows their options.
constraint_indexing:constraint_classification(delta_force_selection_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DELTA INSTITUTIONAL CAPACITY (ROPE) — Selection process generates enormous coordination benefits: it identifies personnel capable of autonomous decision-making under extreme uncertainty, filters for psychological resilience, and builds cohesion through shared ordeal. Delta benefits directly from the selection process as a validation mechanism for its own institutional quality. The constraint is experienced as essential coordination, not extraction — Delta sees selection as a legitimate functional requirement. The institutional actor has arbitrage options (recruitment from other nations' special forces, training pipeline acceleration) but uses extreme selection as the preferred mechanism.
constraint_indexing:constraint_classification(delta_force_selection_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SELECTION ADMINISTRATION (TANGLED ROPE) — Training Command administers selection and experiences it as a hybrid mechanism. It solves a genuine coordination problem: identifying personnel suited to extreme operational environments. But it also extracts from the candidate pool: generates data, maintains institutional justification for special operations hierarchy, controls access to elite career advancement. The administration is constrained (bound by military bureaucracy and funding constraints) but also benefits from maintaining high selectivity. This is a secondary institutional actor with a secondary perspective — included to show intra-institutional extraction.
constraint_indexing:constraint_classification(delta_force_selection_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REFORMED SELECTION ADVOCATES (SCAFFOLD) — Reformers argue selection should emphasize demonstrated operational performance, leadership track record, and psychological assessment over arbitrary exclusion metrics (e.g., height/weight standards, arbitrary fitness thresholds). They see selection as a temporary institutional arrangement that COULD be replaced by more inclusive assessment tied to genuine operational requirements. This is a scaffold perspective because it identifies a sunset path: replacing extreme physical exclusion with competency-based evaluation as technology (exoskeletons, remote operations) reduces correlation between extreme physicality and mission success. The reform movement has agency (institutional backing in personnel policy discussions) and sees an exit path (gradual metric modernization).
constraint_indexing:constraint_classification(delta_force_selection_2026, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL INERTIA VIEW (PITON) — From a historical/analytical perspective, Delta's selection process retains many elements optimized for Cold War-era operations (extreme distance running, deliberate psychological deprivation, isolation stress) that have attenuated relevance to contemporary special operations (drone coordination, cyber integration, distributed teams). The selection ritual persists through institutional inertia: it maintains Delta's mystique, justifies defense spending, and provides institutional continuity. The theater_ratio (0.38) reflects that selection is still highly functional — not primarily performative — but the piton classification highlights that SOME components have drifted from functional necessity to ritual maintenance (e.g., the Land Navigation course's extreme difficulty has minimal correlation with modern task requirements but enormous symbolic weight).
constraint_indexing:constraint_classification(delta_force_selection_2026, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(delta_force_selection_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(delta_force_selection_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(delta_force_selection_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(delta_force_selection_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(delta_force_selection_2026, TR),
    TR >= 0.70.

:- end_tests(delta_force_selection_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The selection process extracts significant costs from aspirants (18+ months, injury risk, opportunity cost, psychological strain, career damage if failed) while providing concentrated benefits to Delta and military institutional hierarchy. The value reflects moderate-to-high extraction — not as extreme as pure predatory snares (0.80+) because some extraction is justified by genuine operational requirements, but well above coordination-only mechanisms. Suppression (0.82): Very high. Multiple barriers prevent exit: institutional pressure (withdrawing signals weakness), career consequences (failed selection damages advancement prospects), reputational costs (within military culture, failure is stigmatized), and limited alternative pathways to special operations leadership. Candidates are locked into the selection trajectory once they enter. Theater ratio (0.38): Moderate-low. Selection retains substantial functional content — extreme physical testing correlates with operational capability in certain mission sets (direct action, direct assault, high-threat hostage rescue). However, some components (arbitrary psychological deprivation, extreme isolation stress, distance running thresholds) have degraded functional necessity while maintaining ritual importance. The theater_ratio has risen slightly over the measurement interval (0.32→0.38) as technological advances (exoskeletons, remote systems, drone integration) have reduced the correlation between extreme physicality and operational success, while selection protocols have remained stable.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival divergence. The trapped aspirant sees pure extraction (Snare) — they are locked in with no exit, bearing maximal costs, for benefits accruing to Delta institutional. Delta sees coordination necessity (Rope) — selection is genuinely required to identify operators capable of extreme autonomous decision-making. The Training Command sees a hybrid (Tangled Rope) — they must solve coordination (finding capable personnel) but also extract (maintain gatekeeping authority). Reformed selection advocates see a temporary problem with a modernizable solution (Scaffold) — competency-based assessment and technological integration offer a sunset path. The broader officer corps sees extraction with some agency (Tangled Rope) — they suffer but retain choice. The analyst sees possible institutional inertia (Piton) — some selection components have drifted toward ritual maintenance. The perspectives should produce a Snare consensus from the victim side (aspirants + officer corps constrained), Rope from Delta institutional, and mixed types from secondary actors.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position relative to extraction flow. Trapped aspirants (powerless/trapped) have d ≈ 0.95 — they are maximal targets bearing extraction. The broader officer corps (moderate/constrained) has d ≈ 0.70 — they suffer extraction through brain drain and career constraints but retain some exit options (acceptance of conventional career path, lateral move to other special operations branches). Delta institutional (institutional/arbitrage) has d ≈ 0.10 — massive beneficiary with exit options (could recruit from other nations, accelerate training pipelines). Training Command (organized/constrained) has d ≈ 0.45 — mixed position, both institutional enforcer and constrained actor. Reformed advocates (organized/mobile) have d ≈ 0.60 — they perceive the extraction and have agency to challenge it, but are constrained by military hierarchy. The analytical observer (analytical/analytical) has d ≈ 0.72 — detached but alert to possible naturalization of contingent arrangements. The variation in directionality produces perspectival gaps: trapped aspirants experience maximum snare; Delta experiences necessary coordination; reformers experience temporary dysfunction with fixable causes.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70): The constraint resolves mandatrophy by decomposing the dual claim (Is selection a necessary coordination mechanism OR an extractive snare?) into structurally distinct components. The functional component (identifying operators for extreme autonomous decision-making in high-threat environments) is genuine coordination; the extractive component (maintaining mystique, gatekeeping elite advancement, justifying defense spending) is genuine extraction. The snare classification reflects that extraction dominates the structural relationship — aspirants are trapped, suppression is very high, exit options are illusory. However, the scaffoldist reform perspective and the piton analyst perspective both highlight that the extraction is CONTESTABLE: if operational requirements changed (technology reduces physicality correlation) or reform pressure succeeded (metrics modernized), the extraction mechanism could lose force. The mandatrophy is resolved not by proving extraction dominates (it does — snare is correct) but by identifying which components are structurally necessary (legitimate coordination) and which are institutional inertia (removable). This allows the system to recommend specific reforms without naturalizing the constraint as immutable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    psychological_resilience_measurement,
    'Can selection''s psychological stress quantifiably predict mission performance under real-world operational stress, or does the correlation reflect reverse causation (selection creates cohesion that improves performance)?',
    'Longitudinal comparison: psychological metrics from selection vs actual mission performance metrics; control groups; comparative analysis with other special operations programs (SAS, GIGN, CAG) with different selection models',
    'If causal: extreme stress is necessary for operational readiness (justifies snare classification from Delta institutional perspective). If reverse causation: selection creates artificial stress that doesn''t predict mission capability (suggests selection is partly theater/snare extraction rather than genuine coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_resilience_measurement, empirical, 'Whether selection''s psychological stress predicts mission performance or reflects reverse causation').

omega_variable(
    operational_diversity_vs_selection_homogeneity,
    'Does extreme selection''s production of psychological/physical homogeneity improve or degrade operational capability across the diversity of real-world Delta missions (hostage rescue, counter-terrorism, direct action, intelligence support)?',
    'Analysis of mission success rates by mission type; comparison with less homogeneous special operations forces; evaluation of innovation and adaptation patterns in different selection regimes',
    'If homogeneity improves capability: selection is coordination mechanism (validates snare from institutional perspective). If diversity improves capability: extreme selection is extraction with degraded functional value (snare classification strengthens across perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(operational_diversity_vs_selection_homogeneity, empirical, 'Whether selection homogeneity improves or degrades operational performance').

omega_variable(
    technological_replacement_rate,
    'At what rate do technological advances (exoskeletons, remote weapons systems, autonomous coordination systems) reduce the correlation between extreme physicality and mission success, making the current selection threshold obsolete?',
    'Technical roadmap analysis; correlation decay studies; comparative mission success with mixed conventional/technological approaches',
    'If replacement rapid (5-10 years): scaffold perspective is structural (sunset is real). If replacement slow (20+ years): scaffold is aspirational. If no replacement: current selection becomes permanently necessary (mountain view has merit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_replacement_rate, empirical, 'Rate of technological replacement reducing physicality correlation with mission success').

omega_variable(
    extraction_beneficiary_identification,
    'Who specifically benefits from maintaining extreme selection exclusivity? (Delta maintains mystique and recruitment premium; Military-Industrial Complex justifies advanced equipment investment; Defense Budget justifies special operations premium funding.) Which beneficiary is primary?',
    'Institutional analysis of who receives funding, authority, prestige from selection maintenance; policy analysis of funding justifications; comparative analysis of selection tightness vs defense spending advocacy',
    'If Delta institutional: snare is justified selection necessity. If Military-Industrial Complex: snare is extractive theater maintained for external benefit. If broader defense budget justification: snare is structurally entrenched and difficult to reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_beneficiary_identification, conceptual, 'Primary institutional beneficiary of extreme selection exclusivity').

omega_variable(
    victim_coalition_possibility,
    'Can the trapped aspirant pool and broader officer corps form a coalition to demand reformed selection metrics (competency-based, inclusive, modernized)? Would such a coalition have sufficient organizational capacity?',
    'Analysis of military personnel union/association capacity; historical precedent for selection system reform; potential defection rates if alternative special operations pathways were created',
    'If coalition possible: powerless agents become organized; snare may degrade toward tangled_rope if extraction becomes contestable. If coalition impossible: snare persists (suppression ≥ 0.80).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(victim_coalition_possibility, empirical, 'Whether victim pool can form organizing coalition for selection reform').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(delta_force_selection_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(delta_tr_t0, delta_force_selection_2026, theater_ratio, 0, 0.32).
narrative_ontology:measurement(delta_tr_t8, delta_force_selection_2026, theater_ratio, 8, 0.35).
narrative_ontology:measurement(delta_tr_t16, delta_force_selection_2026, theater_ratio, 16, 0.38).

% Extraction over time
narrative_ontology:measurement(delta_be_t0, delta_force_selection_2026, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(delta_be_t8, delta_force_selection_2026, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(delta_be_t16, delta_force_selection_2026, base_extractiveness, 16, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(delta_force_selection_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(delta_force_selection_2026, special_operations_career_inequality).
narrative_ontology:affects_constraint(delta_force_selection_2026, military_recruitment_pipeline_saturation).

% DUAL FORMULATION NOTE:
% Delta selection is downstream of broader military selection systems (ROTC, OCS, Ranger School) but represents a distinct structural constraint. The upstream constraints filter for officer/NCO capability; Delta selection creates an additional extractive layer on top of those upstream filters. The network relationship captures that tightening upstream selection (earlier career filters) interacts with Delta's extreme selectivity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(delta_force_selection_2026, powerless, 0.95).
constraint_indexing:directionality_override(delta_force_selection_2026, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
