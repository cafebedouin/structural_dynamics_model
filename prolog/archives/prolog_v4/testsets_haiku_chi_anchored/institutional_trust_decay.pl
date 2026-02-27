% ============================================================================
% CONSTRAINT STORY: institutional_trust_decay
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_trust_decay, []).

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
 *   constraint_id: institutional_trust_decay
 *   human_readable: The Legitimacy Void
 *   domain: social/institutional_governance
 *
 * SUMMARY:
 *   Institutional trust decay describes a systemic condition where public
 *   belief in institutional competence and integrity evaporates. This is not
 *   a sudden collapse but a gradual extraction-through-legitimacy mechanism
 *   in which those dependent on formal institutions for essential services
 *   experience both degraded function and reduced psychological security. The
 *   constraint operates through belief rather than overt coercion: citizens
 *   internalize institutional illegitimacy and reduce demands on or
 *   cooperation with institutional systems, which accelerates performance
 *   degradation, which further erodes trust in a reinforcing cycle. The
 *   constraint extracts value from those dependent on institutional function
 *   by reducing their expectations and agency, while benefiting those with
 *   the ability to exit to alternative (private, informal, or international)
 *   systems. Theater ratio growth (0.35 → 0.81) indicates that formal
 *   institutions increasingly operate performatively, going through
 *   procedural motions without functional legitimacy. This creates a snare
 *   for the dependent population: they remain structurally dependent on
 *   institutions they no longer trust, have no credible exit option, and face
 *   suppression of their own claims to legitimacy.
 *
 * KEY AGENTS:
 *   - Dependent citizens: Primary victims (powerless/trapped) — cannot exit institutional dependence; bear full cost of trust erosion
 *   - Marginalized communities: Secondary victims (powerless/trapped) — historically positioned outside institutional trust networks; suffer accelerated harm from decay
 *   - Precarious workers: Secondary victims (moderate/constrained) — labor protection depends on institutional enforcement; face increased exploitation as trust decays
 *   - Elite institutions and corporations: Primary beneficiaries (institutional/arbitrage) — reduced accountability; can exit to private alternatives
 *   - Global capital holders: Secondary beneficiaries (powerful/mobile) — exploit regulatory arbitrage and instability; can exit to other jurisdictions
 *   - Formal institutional apparatus: Performative maintainer (institutional/constrained) — persists through inertia despite loss of functional legitimacy
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — can perceive the extraction mechanism and its structural targets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_trust_decay, 0.58).
domain_priors:suppression_score(institutional_trust_decay, 0.68).
domain_priors:theater_ratio(institutional_trust_decay, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_trust_decay, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_trust_decay, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(institutional_trust_decay, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_trust_decay, snare).
narrative_ontology:human_readable(institutional_trust_decay, "The Legitimacy Void").
narrative_ontology:topic_domain(institutional_trust_decay, "social/institutional_governance").

% --- Structural relationships ---
narrative_ontology:constraint_victim(institutional_trust_decay, public_epistemic_commons).
narrative_ontology:constraint_victim(institutional_trust_decay, marginalized_communities).
narrative_ontology:constraint_victim(institutional_trust_decay, subordinate_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CITIZEN (SNARE) — Citizens dependent on institutional services (healthcare, education, social safety net) have no exit option. As trust collapses, they bear the full cost: degraded service quality, internalized illegitimacy of their own claims, and psychological burden of navigating systems they cannot trust. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PRECARIOUS WORKER (SNARE) — Workers dependent on state labor protections and employment contracts cannot exit when institutional enforcement erodes. As trust decays, employers face reduced reputational cost for wage theft, unsafe conditions, and contract violation. d≈0.82, f(d)≈1.23, σ=1.0 → χ≈0.71.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL BENEFICIARY (ROPE) — Elite institutions, corporations, and well-positioned officials experience trust decay as liberation from constraint. Reduced accountability risk. Can arbitrage between failed public systems and private alternatives (private healthcare, elite education, private security). d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(institutional_trust_decay, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: GLOBAL CAPITAL HOLDER (TANGLED ROPE) — International investors and multinational actors experience mixed effects: trust decay in a specific nation reduces regulatory overhead (extraction benefit) but also increases instability and default risk (coordination cost). Can exit to other jurisdictions or arbitrage regulatory arbitrage opportunities. d≈0.45, f(d)≈0.52, σ=1.1 → χ≈0.33.
constraint_indexing:constraint_classification(institutional_trust_decay, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL THEATER (PITON) — Formal institutions (legislatures, courts, bureaucracies) persist in performative mode as trust erodes. Meetings occur, procedures are followed, official narratives are maintained, but actual legitimacy and enforcement capacity have collapsed. theater_ratio=0.81 indicates predominantly performative activity. The institutional apparatus maintains itself through inertia despite loss of functional legitimacy. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(institutional_trust_decay, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, institutional trust decay represents a snare: citizens are trapped in de facto dependence on institutions that have lost the capacity to fulfill their function. The constraint operates through psychological mechanisms (legitimacy belief) rather than overt coercion, making it difficult to perceive as extraction. But the extraction is real: those benefiting from institutional malfunction gain power while those dependent on institutional function lose security. d≈0.78, f(d)≈1.18, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(institutional_trust_decay, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_trust_decay_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_trust_decay, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_trust_decay, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_trust_decay, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_trust_decay, TR),
    TR >= 0.70.

:- end_tests(institutional_trust_decay_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint operates through legitimacy belief rather than overt coercion, but the extraction is substantial and measurable. Those dependent on institutional services lose both material benefits (degraded service quality) and psychological security (loss of belief in protective function). The extraction increases over time as more dependent populations lose faith and reduce cooperation. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) Citizens internalize institutional illegitimacy, suppressing their own claims; (2) Alternative institutions (informal, private, NGO) cannot fully substitute, leaving no credible exit; (3) Elite discourse naturalizes distrust as inevitable or deserved; (4) Collective action is suppressed by the atomization that trust decay produces. Theater ratio (0.81): Very high and growing. Institutions engage in extensive performative activity (ceremonies, official communications, procedural compliance) while lacking actual legitimacy or enforcement capacity. The growth trajectory (0.35 → 0.81) shows the constraint strengthening over time as institutions become increasingly disconnected from their professed function.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (institutional/arbitrage) sees reduced constraint and increased freedom. The analytical observer sees a snare for the dependent population. The piton perspective shows institutional systems persisting through theater despite loss of function. The powerless see no escape despite formal procedures for redress. The global capital holder sees mixed effects: reduced regulatory overhead but also increased instability and default risk. The perspectival gap is maximal: the same structural phenomenon (loss of institutional legitimacy) produces opposite effects for different populations, creating a snare for the dependent and a rope (or even a benefit) for the powerful.
 *
 * DIRECTIONALITY LOGIC:
 *   Dependent citizens: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. No exit option; dependent on institutional function; cannot credibly demand enforcement. Precarious workers: Victim + constrained → d≈0.82, f(d)≈1.23. High extraction. Some exit options (change jobs) but limited due to labor market constraints; institutional protections erode. Institutional beneficiaries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can exit to private alternatives; reduced accountability; arbitrage between failed public and functional private systems. Global capital holders: Mixed (mobile) → d≈0.45, f(d)≈0.52. Moderate extraction from instability risk but also benefit from regulatory arbitrage and reduced enforcement. Institutional theater: Constrained + institutional → d≈0.50, f(d)≈0.65. Moderate extraction. Institutions persist through inertia but lose actual functional capacity; they extract legitimacy belief while providing reduced value.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE MANDATROPHY RESOLUTION: The constraint is classified as snare from multiple perspectives (dependent citizen, precarious worker, analytical observer), with moderate to high extractiveness (0.58) and high suppression (0.68). The mandatrophy is resolved by demonstrating that the constraint exhibits genuine extraction (those dependent on institutions lose material and psychological security while those with exit options gain power) rather than pure coordination failure. The constraint could superficially appear as a coordination problem (low institutional trust → coordination failure → reduced mutual benefit), but the asymmetric beneficiary structure (elite gain while dependent lose) confirms extraction. The growing theater ratio (0.35 → 0.81) shows the system increasingly decoupled from actual function, consistent with snare degradation into piton. The analytical observer's perspective confirms this: institutional trust decay is not an inevitable property of modern governance but a contingent institutional arrangement that benefits the powerful while extracting from the dependent. The suppression mechanisms (internalized illegitimacy, lack of credible exit, atomization preventing collective action) are active and maintained, not naturally emergent. This resolves the mandatrophy: the snare classification is correct; the constraint is not a natural law but an extraction mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_vs_performance_threshold,
    'At what level of institutional performance does public belief in legitimacy collapse irreversibly?',
    'Longitudinal survey data on trust vs service quality metrics; causal inference on tipping points in historical collapses; experimental studies on threshold effects',
    'If threshold is high (>70% service quality): constraint is recoverable by institutional reform. If threshold is low (<40%): decay becomes self-reinforcing and irreversible without regime change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_performance_threshold, empirical, 'Performance threshold for trust collapse').

omega_variable(
    substitution_economy_viability,
    'Do alternative institutional systems (informal networks, private markets, NGOs) actually provide equivalent function or do they degrade service for the dependent population?',
    'Comparative analysis of service delivery and access across formal/informal/market alternatives; longitudinal tracking of outcomes for vulnerable populations during transitions',
    'If alternatives viable: trust decay becomes a filtered constraint (powerful/organized gain access to substitutes). If alternatives fail: constraint remains snare for all (no exit option actually works).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_economy_viability, empirical, 'Whether alternative institutional systems can substitute for collapsed formal institutions').

omega_variable(
    trust_recovery_possibility,
    'Can institutional legitimacy be rebuilt once it collapses, or is the decay path path-dependent and irreversible?',
    'Historical case studies of institutional restoration (post-war Japan, post-apartheid South Africa); experimental studies on belief updating after institutional failure; modeling of belief dynamics',
    'If recoverable: constraint is temporary (scaffold with long sunset). If irreversible: constraint becomes permanent (piton or snare with no exit).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(trust_recovery_possibility, empirical, 'Whether institutional legitimacy can be restored after collapse').

omega_variable(
    collective_action_capacity_during_decay,
    'Does institutional trust decay prevent or enable collective action for institutional reform?',
    'Analysis of protest movements, reform coalitions, and institutional change during periods of high distrust; modeling of coordination capacity under low institutional trust',
    'If decay enables action: victims may organize alternative power structures. If decay prevents action: victims remain atomized and trapped (snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_capacity_during_decay, empirical, 'Whether low trust enables or prevents collective action for change').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_trust_decay, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(itd_tr_t0, institutional_trust_decay, theater_ratio, 0, 0.35).
narrative_ontology:measurement(itd_tr_t10, institutional_trust_decay, theater_ratio, 10, 0.58).
narrative_ontology:measurement(itd_tr_t20, institutional_trust_decay, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(itd_be_t0, institutional_trust_decay, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(itd_be_t10, institutional_trust_decay, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(itd_be_t20, institutional_trust_decay, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_trust_decay, enforcement_mechanism).
narrative_ontology:affects_constraint(institutional_trust_decay, regulatory_capture_cycle).
narrative_ontology:affects_constraint(institutional_trust_decay, collective_action_problem).
narrative_ontology:affects_constraint(institutional_trust_decay, legitimacy_belief_dynamics).

% DUAL FORMULATION NOTE:
% Institutional trust decay is upstream of specific institutional failures (healthcare access, labor protection, educational quality) but represents a distinct structural constraint. The decay operates at the level of belief in institutional legitimacy rather than specific institutional function. This belief-level constraint creates conditions for downstream institutional capture and collective action failure. Each specific institutional failure has its own constraint story; trust decay is the meta-level condition that accelerates and reinforces them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_trust_decay, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
