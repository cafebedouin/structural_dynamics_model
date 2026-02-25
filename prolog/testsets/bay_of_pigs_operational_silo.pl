% ============================================================================
% CONSTRAINT STORY: bay_of_pigs_operational_silo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bay_of_pigs_operational_silo, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: bay_of_pigs_operational_silo
 *   human_readable: The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)
 *   domain: political/military
 *
 * SUMMARY:
 *   Operation Zapata (the Bay of Pigs invasion) was structurally undermined
 *   by an extreme 'need to know' silo between the CIA, the Pentagon, and the
 *   Executive branch. This constraint was designed to ensure secrecy and
 *   plausible deniability but instead functioned to suppress critical
 *   military skepticism and flawed intelligence assessments. By preventing
 *   negative feedback from reaching decision-makers, the silo guaranteed the
 *   operation's failure, extracting the lives of the exile brigade and the
 *   geopolitical credibility of the United States.
 *
 * KEY AGENTS:
 *   - Brigade 2506 Exiles: Primary victim (powerless/trapped) — their lives and freedom were the primary cost of the silo's dysfunction.
 *   - CIA Planners: Primary beneficiary (institutional/arbitrage) — the silo protected their operational autonomy and control over the mission.
 *   - Pentagon Leadership: Institutional victim (institutional/constrained) — possessed critical expertise that was actively suppressed by the silo.
 *   - President Kennedy: Hybrid beneficiary/victim (powerful/constrained) — sought plausible deniability but was deprived of the information needed for a sound decision.
 *   - Analytical Observers (e.g., Taylor Commission): Post-hoc analysts who identified the silo as a key structural failure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bay_of_pigs_operational_silo, 0.68).
domain_priors:suppression_score(bay_of_pigs_operational_silo, 0.85).
domain_priors:theater_ratio(bay_of_pigs_operational_silo, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, extractiveness, 0.68).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(bay_of_pigs_operational_silo, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bay_of_pigs_operational_silo, tangled_rope).
narrative_ontology:human_readable(bay_of_pigs_operational_silo, "The CIA-Pentagon Intelligence/Action Silo (Operation Zapata)").
narrative_ontology:topic_domain(bay_of_pigs_operational_silo, "political/military").

domain_priors:requires_active_enforcement(bay_of_pigs_operational_silo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bay_of_pigs_operational_silo, cia_operational_autonomy).
narrative_ontology:constraint_beneficiary(bay_of_pigs_operational_silo, executive_plausible_deniability).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, brigade_2506_exiles).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, us_foreign_policy_credibility).
narrative_ontology:constraint_victim(bay_of_pigs_operational_silo, operational_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CUBAN EXILE (SNARE) — Completely trapped within the operational plan once deployed. Their lives and liberty are extracted by a flawed strategy they have no power to influence or exit. The silo's suppression of dissent directly leads to their capture and death. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.87.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE CIA PLANNER (ROPE) — Experiences the silo as a pure coordination mechanism essential for maintaining secrecy, operational control, and plausible deniability. From this viewpoint, the constraint solves the problem of inter-agency leaks and political interference. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.07. Negative extraction indicates a net beneficiary.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE PENTAGON SKEPTIC (TANGLED ROPE) — An institutional actor with relevant expertise but constrained from meaningful input by the CIA's silo. Sees both the coordination function (secrecy) and the severe extraction (suppression of military realism, leading to mission failure). d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.75.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE US PRESIDENT (TANGLED ROPE) — The ultimate decision-maker, yet constrained by the curated information flowing from the silo. Benefits from the 'plausible deniability' the silo provides, but is a victim of the suppressed intelligence that prevents a sound decision. The constraint extracts his agency. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.44.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — The post-mortem analysis sees the full structure. The silo is a hybrid: a tool for coordination (secrecy) that created a pathological, asymmetric extraction of truth and viability from the system, benefiting institutional autonomy at the cost of mission success. This is the canonical classification. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bay_of_pigs_operational_silo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bay_of_pigs_operational_silo, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bay_of_pigs_operational_silo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bay_of_pigs_operational_silo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68): High. The constraint extracts operational viability, truth, and ultimately human lives from the system. The cost of the failure, borne by the exiles and US prestige, was immense. Suppression (0.85): Very High. The core function of the 'need to know' protocol was to actively suppress information flow and external review, creating near-total lack of alternatives for those inside the planning loop and no voice for those outside. Theater Ratio (0.60): Significant. The performance of 'plausible deniability' and covert action tropes overrode functional, integrated military planning. The process was more about maintaining the appearance of a deniable operation than ensuring its success.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the CIA planners, the silo was a Rope, a necessary tool for coordination in a sensitive environment. For the Brigade 2506 exiles on the beach, it was a Snare, a death trap built from institutional arrogance and lies. For the sidelined Pentagon experts and the analytically-constrained President, it was a Tangled Rope, a dysfunctional system that mixed a legitimate goal (secrecy) with a pathological extraction of reality. The analytical view confirms the Tangled Rope as the most complete description of the structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are abstract institutional principles: the CIA's autonomy and the President's deniability. The victims are concrete: the soldiers on the ground and the nation's credibility. This maps directly to the d-value derivations. The CIA planners, serving institutional autonomy, have a low d-value and see a Rope. The exiles, as trapped victims, have a high d-value and experience a Snare. The Pentagon and President, being both inside and outside the core beneficiary group and having constrained exit, fall in the middle.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful resolution of mandatrophy. Labeling the silo as simply a 'failure of coordination' (Rope) would be a gross mischaracterization that ignores the lethal extraction imposed on the exiles. Labeling it as pure malice (Snare) ignores the genuine (if flawed) coordination goals of the planners. The Tangled Rope classification, from the analytical view, correctly identifies the structure as a coordination mechanism that became pathologically extractive due to extreme suppression, capturing the dual nature of the failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    groupthink_vs_structure,
    'Was the failure primarily due to the psychology of groupthink among the planners, or was it an inevitable outcome of the structural silo itself?',
    'Comparative analysis with other covert operations that had different information-sharing structures but similar psychological pressures.',
    'If primarily groupthink, the silo is less causal (lower ε). If primarily structural, the silo is the central failure mechanism (ε is correct).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(groupthink_vs_structure, conceptual, 'Distinguishing psychological groupthink from structural information siloing as the primary cause of failure.').

omega_variable(
    deniability_tradeoff,
    'At what point does the need for ''plausible deniability'' become so suppressive that it guarantees operational failure?',
    'Game-theoretic modeling of covert actions, correlating secrecy levels with success/failure rates across a large dataset of historical operations.',
    'Identifies whether ''plausible deniability'' is a manageable risk (Scaffold) or an inherently self-defeating constraint (Snare/Tangled Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deniability_tradeoff, empirical, 'Quantifying the tradeoff between plausible deniability and operational viability.').

omega_variable(
    pentagon_intervention_impact,
    'Would full Pentagon involvement have saved the operation, or merely made the failure larger and less deniable?',
    'Counterfactual military simulations based on declassified Pentagon assessments of the operational plan.',
    'If it would have succeeded, the silo is a pure Snare. If it would have failed anyway, the silo is more of a Piton, performing a ritual of secrecy around an already doomed plan.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pentagon_intervention_impact, empirical, 'Counterfactual analysis of whether Pentagon oversight could have prevented the operational failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bay_of_pigs_operational_silo, 1960, 1961).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bay__tr_t1960, bay_of_pigs_operational_silo, theater_ratio, 1960, 0.3).
narrative_ontology:measurement(bay__tr_t1960, bay_of_pigs_operational_silo, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(bay__tr_t1961, bay_of_pigs_operational_silo, theater_ratio, 1961, 0.6).

% Extraction over time
narrative_ontology:measurement(bay__be_t1960, bay_of_pigs_operational_silo, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(bay__be_t1960, bay_of_pigs_operational_silo, base_extractiveness, 1960, 0.55).
narrative_ontology:measurement(bay__be_t1961, bay_of_pigs_operational_silo, base_extractiveness, 1961, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bay_of_pigs_operational_silo, enforcement_mechanism).
narrative_ontology:affects_constraint(bay_of_pigs_operational_silo, us_intelligence_community_reform_1960s).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
