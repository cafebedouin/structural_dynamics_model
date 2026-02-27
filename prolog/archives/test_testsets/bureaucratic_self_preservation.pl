% ============================================================================
% CONSTRAINT STORY: bureaucratic_self_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bureaucratic_self_preservation, []).

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
 *   constraint_id: bureaucratic_self_preservation
 *   human_readable: The Inertial Office
 *   domain: political
 *
 * SUMMARY:
 *   The Inertial Office, or bureaucratic self-preservation, describes the
 *   tendency of administrative bodies to prioritize their own survival,
 *   budget growth, and institutional power over their stated public mission.
 *   This phenomenon, rooted in public choice theory (e.g., Niskanen's
 *   budget-maximizing model), creates a structural conflict between the
 *   public good and the incentives of the agency's personnel. Over time, the
 *   agency's function can become secondary to the performance of rituals that
 *   justify its existence, leading to high levels of theater and inefficient
 *   allocation of public resources.
 *
 * KEY AGENTS:
 *   - Agency Personnel/Leadership: Primary beneficiaries (institutional/arbitrage) who gain job security, prestige, and power from the institution's persistence and growth.
 *   - General Public/Taxpayers: Primary victims (powerless/trapped) who fund the bureaucracy and receive diminishing returns on their investment.
 *   - Intended Service Recipients: Specific victims (powerless/trapped) who directly experience the consequences of mission drift and degraded service.
 *   - Political Oversight Bodies: Observers and potential reformers (organized/mobile) who see the functional decay and performative nature of the agency.
 *   - Analytical Observers: Theorists (analytical/analytical) who may frame the phenomenon as an inevitable law of social organization.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bureaucratic_self_preservation, 0.55).
domain_priors:suppression_score(bureaucratic_self_preservation, 0.75).
domain_priors:theater_ratio(bureaucratic_self_preservation, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bureaucratic_self_preservation, extractiveness, 0.55).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, theater_ratio, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bureaucratic_self_preservation, tangled_rope).
narrative_ontology:human_readable(bureaucratic_self_preservation, "The Inertial Office").
narrative_ontology:topic_domain(bureaucratic_self_preservation, "political").

domain_priors:requires_active_enforcement(bureaucratic_self_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, agency_personnel).
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, senior_bureaucrats).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, general_public_taxpayers).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, intended_service_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PUBLIC (SNARE) — Experiences the system as pure, coercive extraction. Funds are taken via taxation, and the promised services are degraded or not delivered. There is no exit option. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AGENCY LEADERSHIP (ROPE) — Experiences the rules, budget processes, and personnel policies as necessary coordination mechanisms to manage a large organization. They benefit directly from budget growth and institutional stability. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07 (net beneficiary).
constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: MID-LEVEL BUREAUCRAT (TANGLED ROPE) — Benefits from the job security and stability (coordination) but is also constrained by the rigid rules and mission drift, which extracts their productive capacity for performative tasks. d≈0.65, f(d)≈1.00, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: POLITICAL OVERSIGHT (PITON) — Sees an institution whose original function has atrophied, replaced by performative rituals of compliance and reporting. The high theater_ratio (0.80) triggers the Piton classification. The committee's focus is on the visible decay of function, not just the extraction.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL (MOUNTAIN) — Views bureaucratic self-preservation as an immutable 'iron law' of organizations, a natural consequence of rational, self-interested actors in a system with diffuse accountability. This perspective naturalizes the phenomenon. The engine will flag this as a false summit, as the high ε and suppression are inconsistent with a true Mountain.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_self_preservation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bureaucratic_self_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR),
    TR >= 0.70.

:- end_tests(bureaucratic_self_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. Represents the significant diversion of public funds (taxes) and resources away from the intended mission toward budget padding, make-work, and institutional expansion. Suppression (0.75): Very high. Civil service protections, informational asymmetry, entrenched political alliances, and public apathy make meaningful reform or abolition extremely difficult. Theater Ratio (0.80): Very high. As the original mission becomes secondary, the agency's activities become increasingly performative—generating reports, holding meetings, and enforcing internal procedures that have little connection to external outcomes. This high ratio is a key indicator of a Piton from the perspective of an informed observer.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. Agency insiders experience the rules and budget cycles as necessary coordination (Rope). The public, who pays for it and gets poor service, experiences it as a coercive trap (Snare). A mid-level employee sees both the benefits of stability and the costs of inefficiency (Tangled Rope). Political overseers see a decayed, inertial institution whose function is mostly for show (Piton). An academic might see an immutable law of nature (Mountain). The classification depends entirely on the observer's structural relationship to the flow of resources and power.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (agency_personnel) with arbitrage exit options (e.g., senior officials moving to lobbying) have a low derived directionality (d), resulting in a Rope classification with negative effective extraction (χ). Victims (general_public) who are trapped have a very high d, leading to a high χ and a Snare classification. Agents with constrained exit and mixed incentives (mid-level_bureaucrat) fall in between, classifying as a Tangled Rope. The system correctly models how the same set of base properties can be perceived as beneficial coordination or coercive extraction depending on one's position.
 *
 * MANDATROPHY ANALYSIS:
 *   This story resolves the mandatrophy by demonstrating that 'bureaucracy' is not a monolithic evil (Snare) or a pure good (Rope). The framework's perspectivalism captures the reality that it is simultaneously a coordination mechanism for its members, an extractive mechanism for the public, and a decaying, performative entity for outside observers. A single classification would be a gross oversimplification; the set of classifications reveals the complete, conflicted nature of the institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    iron_law_vs_contingency,
    'Is bureaucratic self-preservation an inevitable ''iron law'' of large organizations, or is it a contingent outcome of specific political and institutional designs?',
    'Comparative analysis of public agencies with different accountability structures, funding mechanisms (e.g., block grants vs. performance-based), and personnel systems.',
    'If inevitable, it''s a Mountain that must be managed. If contingent, it''s a Tangled Rope or Snare that can be reformed or dismantled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iron_law_vs_contingency, conceptual, 'Whether bureaucratic inertia is an inevitable law or a contingent failure.').

omega_variable(
    metric_resistance,
    'Can meaningful performance metrics be designed that resist Goodhart''s Law and align the agency''s incentives with its public mission?',
    'Pilot programs testing alternative performance measurement frameworks (e.g., citizen-reported outcomes, randomized controlled trials for policy effectiveness).',
    'If yes, the constraint could be reformed into a Rope. If no, the extractive component is structural and likely to persist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(metric_resistance, empirical, 'Feasibility of creating non-gameable performance metrics for public agencies.').

omega_variable(
    abolition_threshold,
    'At what point does the cost of an agency''s institutional inertia and budget maximization exceed the social value of its remaining functions?',
    'Cost-benefit analysis comparing the agency''s total budget and compliance costs against the monetized value of its outputs, benchmarked against alternative service delivery models.',
    'Defines the threshold for a political decision to reform, privatize, or abolish the agency. This is a policy choice, not a purely technical one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abolition_threshold, preference, 'The social cost-benefit threshold for abolishing an inertial agency.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bureaucratic_self_preservation, 1980, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bure_tr_t1980, bureaucratic_self_preservation, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(bure_tr_t2000, bureaucratic_self_preservation, theater_ratio, 2000, 0.62).
narrative_ontology:measurement(bure_tr_t2024, bureaucratic_self_preservation, theater_ratio, 2024, 0.8).

% Extraction over time
narrative_ontology:measurement(bure_be_t1980, bureaucratic_self_preservation, base_extractiveness, 1980, 0.2).
narrative_ontology:measurement(bure_be_t2000, bureaucratic_self_preservation, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(bure_be_t2024, bureaucratic_self_preservation, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bureaucratic_self_preservation, resource_allocation).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, regulatory_capture).
narrative_ontology:affects_constraint(bureaucratic_self_preservation, public_trust_decline).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
