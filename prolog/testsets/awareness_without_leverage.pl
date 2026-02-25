% ============================================================================
% CONSTRAINT STORY: awareness_without_leverage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_awareness_without_leverage, []).

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
 *   constraint_id: awareness_without_leverage
 *   human_readable: The Cassandra Paradox
 *   domain: social/political
 *
 * SUMMARY:
 *   The Cassandra Paradox describes the structural condition where an agent
 *   possesses accurate, often critical, foresight about a systemic risk but
 *   lacks the leverage or credibility to compel action. The constraint is not
 *   the lack of information, but the system's organized deafness to it. This
 *   deafness is maintained by institutional inertia, misaligned incentives,
 *   and active suppression by those who benefit from the status quo. The
 *   paradox creates a type-diverse landscape where the same set of facts is
 *   perceived as coordination (Rope), pure extraction (Snare), temporary
 *   dysfunction (Scaffold), or performative ritual (Piton), depending
 *   entirely on the observer's structural position.
 *
 * KEY AGENTS:
 *   - The Cassandra Agent: The expert/whistleblower with foresight (moderate/constrained) - primary victim.
 *   - Status Quo Stakeholders: Institutional leadership benefiting from inaction (institutional/arbitrage) - primary beneficiary.
 *   - The General Public: The population who will suffer the eventual consequences (powerless/trapped) - secondary victim.
 *   - The Bureaucratic Process: The institutional machinery of delay (institutional/constrained) - source of theater and inertia.
 *   - The Activist Coalition: Organized opposition attempting to force change (organized/mobile) - agents of potential sunset.
 *   - The Cynical Observer: The analyst who naturalizes the failure (analytical/analytical) - source of the false mountain claim.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(awareness_without_leverage, 0.55).
domain_priors:suppression_score(awareness_without_leverage, 0.75).
domain_priors:theater_ratio(awareness_without_leverage, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(awareness_without_leverage, extractiveness, 0.55).
narrative_ontology:constraint_metric(awareness_without_leverage, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(awareness_without_leverage, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(awareness_without_leverage, tangled_rope).
narrative_ontology:human_readable(awareness_without_leverage, "The Cassandra Paradox").
narrative_ontology:topic_domain(awareness_without_leverage, "social/political").

domain_priors:requires_active_enforcement(awareness_without_leverage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(awareness_without_leverage, status_quo_stakeholders).
narrative_ontology:constraint_victim(awareness_without_leverage, cassandra_agents).
narrative_ontology:constraint_victim(awareness_without_leverage, general_public).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GENERAL PUBLIC (SNARE) — Trapped within the system, they will bear the full, unavoidable cost of the predicted crisis. From their view, the system is purely extractive, silencing necessary warnings to their detriment. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE CASSANDRA AGENT (SNARE) — Possesses knowledge but is structurally silenced. Their exit is constrained by professional reputation and moral commitment. They experience the system as a high-suppression trap that invalidates their primary asset (knowledge). d≈0.80, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(awareness_without_leverage, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATUS QUO LEADERSHIP (ROPE) — Benefits from ignoring the warning, maximizing short-term stability, profit, or political capital. They can exit or insulate themselves before the crisis hits. For them, the system is a coordination mechanism to maintain order and suppress dissent. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07.
constraint_indexing:constraint_classification(awareness_without_leverage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACTIVIST COALITION (SCAFFOLD) — Organized groups that believe the warning see the current institutional failure as a temporary state. They are building parallel structures (awareness campaigns, political pressure) intended to force change, after which their intense mobilization can be sunset. d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(awareness_without_leverage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: THE BUREAUCRATIC PROCESS (PITON) — The institutional machinery itself (committees, review boards, studies) is largely performative. It gives the appearance of addressing the warning while functionally serving to delay action. Its own function has atrophied, persisting through inertia. theater_ratio=0.75 satisfies the piton gate (≥0.70).
constraint_indexing:constraint_classification(awareness_without_leverage, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE CYNICAL OBSERVER (MOUNTAIN) — This perspective naturalizes the failure, claiming that large human systems are inherently deaf to prophetic warnings. It frames a contingent social structure as an immutable law of nature. The engine will detect this as a false summit, as the base properties (ε=0.55, suppression=0.75) fail the mountain classification gates.
constraint_indexing:constraint_classification(awareness_without_leverage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(awareness_without_leverage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(awareness_without_leverage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(awareness_without_leverage, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(awareness_without_leverage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(awareness_without_leverage, TR),
    TR >= 0.70.

:- end_tests(awareness_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the deferred but significant cost of the eventual crisis, which is effectively extracted from the future to maintain present stability/profit. Suppression (0.75): Extremely high. The core of the paradox is the structural silencing of a correct warning through mechanisms like groupthink, credential-assassination, and bureaucratic delay. Theater Ratio (0.75): High. The system's response to the warning is typically performative: forming committees, commissioning endless studies, and holding listening sessions that create an illusion of action while ensuring none is taken. This high theater score is crucial for the Piton perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound and demonstrates the core of the indexical method. For leadership, maintaining the status quo is a successful coordination (Rope). For the public and the Cassandra, it's a trap (Snare). For activists, it's a temporary problem to be overcome (Scaffold). For the bureaucracy itself, it's a hollowed-out ritual (Piton). For the detached cynic, it's an unchangeable law of social physics (Mountain). The base properties are fixed; the classification shifts with the observer's relationship to power, exit, and time.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Status Quo Stakeholders) with arbitrage exit options derive a very low directionality (d), resulting in low or negative effective extraction (χ), hence they perceive a Rope. Victims (Public, Cassandra) with trapped or constrained exit options derive a very high d, leading to high χ and a Snare classification. Organized agents (Activists) have more agency, leading to a moderate d and a χ value that falls into the Scaffold or Tangled Rope range. The system's classification is driven by who benefits from the silence and who pays the price for the eventual failure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that a single phenomenon can be correctly classified as multiple types simultaneously. The error is to insist on a single 'true' classification. The Cassandra Paradox is not *just* a Snare or *just* a Piton; it is the entire presheaf of classifications over the different structural positions. The analytical task is to map this landscape, not to pick a peak. The framework correctly identifies the 'Mountain' view as a false summit, preventing the naturalization of a contingent, and deeply political, institutional failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_inertia,
    'Is the suppression of the warning a deliberate, coordinated act by beneficiaries, or an emergent property of uncoordinated institutional inertia?',
    'Internal communications analysis (e.g., leaked documents, whistleblower testimony) to distinguish between explicit strategy and implicit bias.',
    'If intentional, strengthens the Snare classification. If inertial, strengthens the Piton classification for the system''s core.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_inertia, empirical, 'Distinguishing between deliberate suppression and emergent institutional inertia.').

omega_variable(
    critical_mass_threshold,
    'Is there a quantifiable threshold of public awareness, elite consensus, or crisis proximity at which the system''s resistance collapses and the warning is heeded?',
    'Comparative historical analysis of ignored warnings that were eventually acted upon, correlating action with polling data, media saturation, and proximity to disaster.',
    'If a predictable threshold exists, the constraint is better modeled as a Scaffold with a conditional sunset clause. If not, it reinforces the Snare/Piton view of a more stable, self-perpetuating system.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(critical_mass_threshold, empirical, 'Threshold for public awareness to overcome systemic resistance.').

omega_variable(
    communication_framing_efficacy,
    'To what extent is the failure structural versus a failure of communication? Could a different rhetorical framing have overcome the resistance?',
    'A/B testing of crisis messaging on policy-maker proxies; retrospective analysis of cases where similar warnings were heeded, focusing on the communication strategy used.',
    'If framing is highly effective, the constraint''s suppression value is lower than estimated. If all frames fail, the suppression is confirmed to be deeply structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(communication_framing_efficacy, conceptual, 'Whether the failure is purely structural or a matter of communication framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(awareness_without_leverage, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(awar_tr_t0, awareness_without_leverage, theater_ratio, 0, 0.5).
narrative_ontology:measurement(awar_tr_t10, awareness_without_leverage, theater_ratio, 10, 0.65).
narrative_ontology:measurement(awar_tr_t20, awareness_without_leverage, theater_ratio, 20, 0.75).

% Extraction over time
narrative_ontology:measurement(awar_be_t0, awareness_without_leverage, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(awar_be_t10, awareness_without_leverage, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(awar_be_t20, awareness_without_leverage, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(awareness_without_leverage, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
