% ============================================================================
% CONSTRAINT STORY: evidence_half_life
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_evidence_half_life, []).

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
 *   constraint_id: evidence_half_life
 *   human_readable: The Epistemic Decay Constant
 *   domain: technological/scientific/legal
 *
 * SUMMARY:
 *   The rapid pace of information turnover and the ease of digital alteration
 *   have created a scenario where the 'half-life' of a piece of evidence can
 *   drop below the time required for judicial or scientific verification.
 *   This decay creates a structural tension, enabling malicious actors and
 *   opportunistic researchers to benefit while harming the judicial system,
 *   scientific community, and the ability of citizens to make informed
 *   decisions.
 *
 * KEY AGENTS:
 *   - Judicial System: Primary victim (moderate/constrained) - Struggles to verify evidence quickly enough.
 *   - Scientific Community: Secondary victim (moderate/constrained) - Vulnerable to misinterpretation and manipulation of evidence.
 *   - Uninformed Citizens: Primary victim (powerless/trapped) - Lacks the tools to discern valid information.
 *   - Malicious Actors: Primary beneficiary (institutional/arbitrage) - Exploit the gap between creation and verification of false evidence.
 *   - Opportunistic Researchers: Secondary beneficiary (institutional/arbitrage) - Capitalize on quickly fading relevance of evidence.
 *   - Analytical Observer: (analytical/analytical) - Understands the broader civilizational impact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(evidence_half_life, 0.6).
domain_priors:suppression_score(evidence_half_life, 0.7).
domain_priors:theater_ratio(evidence_half_life, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(evidence_half_life, extractiveness, 0.6).
narrative_ontology:constraint_metric(evidence_half_life, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(evidence_half_life, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(evidence_half_life, tangled_rope).
narrative_ontology:human_readable(evidence_half_life, "The Epistemic Decay Constant").
narrative_ontology:topic_domain(evidence_half_life, "technological/scientific/legal").

domain_priors:requires_active_enforcement(evidence_half_life).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(evidence_half_life, malicious_actors).
narrative_ontology:constraint_beneficiary(evidence_half_life, opportunistic_researchers).
narrative_ontology:constraint_victim(evidence_half_life, judicial_system).
narrative_ontology:constraint_victim(evidence_half_life, scientific_community).
narrative_ontology:constraint_victim(evidence_half_life, uninformed_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNINFORMED CITIZENS (SNARE) - Citizens are largely trapped in this system, unable to discern valid information from manipulated evidence. The speed and scale of misinformation make it nearly impossible for the average person to exit this state of confusion. They bear the costs of this decay, from poor decision-making to manipulation.
constraint_indexing:constraint_classification(evidence_half_life, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JUDICIAL SYSTEM (TANGLED ROPE) - The judicial system is constrained by the need to verify evidence while also facing an increasing rate of epistemic decay. They benefit from improved forensic tools and legal frameworks, but they are still victims of falsified or quickly outdated evidence. They have limited exit options due to their role in upholding the law.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCIENTIFIC COMMUNITY (TANGLED ROPE) - Scientists are caught in a loop. They develop evidence and methodologies but are constrained by the half-life of the knowledge they're developing. They benefit from increased research capacity and data availability but are equally vulnerable to misinterpretation or manipulation. Their exit is constrained by the need to remain objective and verifiable.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: MALICIOUS ACTORS (ROPE) - Actors who intentionally falsify or manipulate evidence benefit from the rapid decay rate. They exploit the gap between the creation of false evidence and its verification, turning the decay constant into a weapon. They have arbitrage exit options by moving to new platforms or methods as old ones are detected.
constraint_indexing:constraint_classification(evidence_half_life, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: Opportunistic Researchers (ROPE) - Those who capitalize on quickly fading relevance in scientific or technological evidence have a net positive immediate benefit. They are in arbitrage as an institution.
constraint_indexing:constraint_classification(evidence_half_life, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) - From a civilizational perspective, the observer recognizes that epistemic decay is an inherent challenge that requires continuous adaptation. However, they also recognize that the system is prone to exploitation and that asymmetric extraction is at play. Analysis of this decay constant is necessary.
constraint_indexing:constraint_classification(evidence_half_life, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evidence_half_life_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(evidence_half_life, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(evidence_half_life, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(evidence_half_life, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(evidence_half_life_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.60): High. Significant resources and cognitive effort are extracted from the judicial and scientific systems due to the need for continuous verification. Suppression (0.70): High. The rapid decay rate effectively suppresses the ability of citizens to discern valid information. Theater ratio (0.30): Low. Focus is on the decay and its consequences, less on performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   The malicious actors see coordination (Rope) - they are solving their own problems of manipulating information. The judicial and scientific communities see a mixed challenge of coordination and extraction (Tangled Rope) - they must manage resources while struggling to verify evidence. Uninformed citizens see pure extraction (Snare) - they are largely helpless in this system. The analytical observer recognizes the inherent challenge (Tangled Rope) and the need for solutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural role. Beneficiaries (malicious actors and researchers) have arbitrage exit options and experience low or negative extraction. Victims (judicial system, scientific community, citizens) are constrained or trapped and bear the costs of decay. The analytical observer attempts to view the situation objectively.
 *
 * MANDATROPHY ANALYSIS:
 *   This resolves the mandatrophy by recognizing that multiple valid perspectives exist. It's not just a Snare (pure extraction) because some actors benefit. It's not just a Rope (pure coordination) because it's being exploited. It's a complex system with inherent tensions and asymmetries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_speed_vs_decay,
    'At what point does the speed of verification fail to keep pace with the decay of evidence?',
    'Empirical study of legal cases and scientific findings, measuring the time required for verification against the half-life of relevant evidence.',
    'If verification lags: the system is vulnerable to widespread misinformation and injustice. If verification keeps pace: The system can function reliably.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_speed_vs_decay, empirical, 'Determines the tipping point where the verification process is overwhelmed by the decay rate.').

omega_variable(
    source_authenticity,
    'What technological or institutional mechanisms can reliably establish the authenticity and provenance of digital evidence?',
    'Develop and test different authentication methods, such as blockchain verification, digital watermarking, and multi-signature schemes. Analyze their resilience against manipulation.',
    'If authenticity can be reliably established: the decay constant is less of a threat. If not: the system is highly vulnerable to fabricated evidence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_authenticity, conceptual, 'Addresses the core problem of verifying the origin and integrity of evidence.').

omega_variable(
    public_awareness_effectiveness,
    'How effective are public awareness campaigns in educating individuals to critically evaluate information and identify manipulated evidence?',
    'Conduct studies to measure the impact of public awareness campaigns on critical thinking skills, media literacy, and susceptibility to misinformation.',
    'If effective: the public is more resilient to misinformation. If not: the public remains vulnerable to manipulation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(public_awareness_effectiveness, empirical, 'Assesses the potential for public education to mitigate the negative effects.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(evidence_half_life, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(evid_tr_t0, evidence_half_life, theater_ratio, 0, 0.1).
narrative_ontology:measurement(evid_tr_t5, evidence_half_life, theater_ratio, 5, 0.2).
narrative_ontology:measurement(evid_tr_t10, evidence_half_life, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(evid_be_t0, evidence_half_life, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(evid_be_t5, evidence_half_life, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(evid_be_t10, evidence_half_life, base_extractiveness, 10, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(evidence_half_life, information_standard).
narrative_ontology:affects_constraint(evidence_half_life, confirmation_bias).
narrative_ontology:affects_constraint(evidence_half_life, algorithmic_opacity).

% DUAL FORMULATION NOTE:
% Evidence half-life is the rate at which reliance upon evidence decays, and this can also decay understanding in an adversarial way if bad actors game the system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
