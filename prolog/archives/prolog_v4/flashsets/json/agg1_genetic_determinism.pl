% ============================================================================
% CONSTRAINT STORY: agg1_genetic_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agg1_genetic_determinism, []).

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
 *   constraint_id: agg1_genetic_determinism
 *   human_readable: The institutional claim that the AGG-1 gene deterministically causes aggression.
 *   domain: genetics/social_policy
 *
 * SUMMARY:
 *   The claim that the AGG-1 gene deterministically causes aggression is a
 *   complex issue with various perspectives. It highlights the tension
 *   between potential benefits, such as identifying individuals at risk, and
 *   significant risks, including stigmatization and the neglect of social
 *   determinants of behavior. The narrative also involves competing interests
 *   from pharmaceutical companies, the criminal justice system, and academic
 *   research.
 *
 * KEY AGENTS:
 *   - Individuals Labeled Aggressive: Primary target (powerless/trapped) – faces discrimination and loss of agency.
 *   - Pharmaceutical Companies: Primary beneficiary (institutional/arbitrage) – stands to profit from gene-based therapies.
 *   - Academic Researchers: Secondary actor (moderate/constrained) – benefits from funding but faces pressure to confirm the link.
 *   - Criminal Justice System: Institutional actor (institutional/constrained) - benefits from potential identification of aggressive individuals, faces ethical concerns.
 *   - Community Self-Determination: Primary victim (powerless/trapped) - suffers from suppressed alternative explanations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agg1_genetic_determinism, 0.55).
domain_priors:suppression_score(agg1_genetic_determinism, 0.7).
domain_priors:theater_ratio(agg1_genetic_determinism, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agg1_genetic_determinism, extractiveness, 0.55).
narrative_ontology:constraint_metric(agg1_genetic_determinism, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(agg1_genetic_determinism, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agg1_genetic_determinism, tangled_rope).
narrative_ontology:human_readable(agg1_genetic_determinism, "The institutional claim that the AGG-1 gene deterministically causes aggression.").
narrative_ontology:topic_domain(agg1_genetic_determinism, "genetics/social_policy").

domain_priors:requires_active_enforcement(agg1_genetic_determinism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(agg1_genetic_determinism, criminal_justice_system).
narrative_ontology:constraint_victim(agg1_genetic_determinism, individuals_labeled_aggressive).
narrative_ontology:constraint_victim(agg1_genetic_determinism, community_self_determination).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Individuals labeled as genetically predisposed to aggression are trapped by the label, facing potential discrimination and loss of agency. The deterministic framing suppresses alternative explanations for behavior.
constraint_indexing:constraint_classification(agg1_genetic_determinism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Researchers may be constrained by funding priorities and publication bias towards positive results confirming the link, but also benefit from career advancement and research grants by reinforcing this narrative.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% Pharmaceutical companies benefit from the perception of a genetic basis for aggression, opening up markets for potential gene-based therapies. They can arbitrage the claim into profit.
constraint_indexing:constraint_classification(agg1_genetic_determinism, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The criminal justice system, while potentially seeing benefits in identifying individuals predisposed to aggression, also faces the constraint of ethical considerations and potential legal challenges. The system's adherence to the genetic determinism claim is largely performative in maintaining the status quo without addressing social determinants.
constraint_indexing:constraint_classification(agg1_genetic_determinism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% From an analytical perspective, the claim is a tangled rope: it coordinates research efforts and potentially justifies interventions, but also extracts by narrowing the scope of analysis and justifying social control measures.
constraint_indexing:constraint_classification(agg1_genetic_determinism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agg1_genetic_determinism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agg1_genetic_determinism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agg1_genetic_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agg1_genetic_determinism, TR),
    TR >= 0.70.

:- end_tests(agg1_genetic_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim exhibits characteristics of a tangled rope. Extractiveness (0.55): Moderate. While the AGG-1 claim provides some benefits through potential therapeutic pathways, it also extracts value by focusing on genetic determinism and limiting the scope of analysis. Suppression (0.70): High. Alternative explanations for aggressive behavior, such as social and economic factors, are suppressed in favor of genetic explanations. Theater ratio (0.75): High. The association is often presented through performative metrics lacking full scientific validation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing structural positions of the agents. Individuals labeled aggressive face the brunt of extraction with little opportunity for escape (Snare). Pharmaceutical companies, with arbitrage options, see the claim as an opportunity (Rope). Researchers, constrained by funding, occupy a mixed position (Tangled Rope). The criminal justice system can be both beneficiary and constrained due to ethical issues (Piton). The analytical observer notes the conflicting forces (Tangled Rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the relationship to the AGG-1 claim. Pharmaceutical companies benefit directly through potential markets. Researchers are moderate because of career and funding incentives. Individuals labeled aggressive bear the costs of the deterministic framing. The criminal justice system is complex, benefiting from potential identification but also facing the constraint of ethical considerations.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing the diverse perspectives. The AGG-1 claim is not inherently good or bad; its classification depends on the structural position of the observer. Classifying it as a pure snare ignores the potential coordination benefits in research and intervention. Classifying it as a rope neglects the extraction from individuals and communities affected by the deterministic label.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    genetic_vs_environmental_contribution,
    'What is the relative contribution of the AGG-1 gene versus environmental factors in determining aggressive behavior?',
    'Longitudinal studies controlling for environmental variables; genome-wide association studies with diverse populations.',
    'If environmental factors are dominant, the claim shifts towards a scaffold (temporary focus) or piton (degraded explanation). If genetic contribution is substantial, the claim strengthens towards a tangled rope or even a mountain (though deterministic claims are unlikely in complex behavior).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genetic_vs_environmental_contribution, empirical, 'Relative contribution of gene vs environment').

omega_variable(
    definition_of_aggression,
    'How is ''aggression'' being defined and measured? Is the definition culturally biased or overly broad?',
    'Cross-cultural validation of aggression scales; qualitative studies exploring diverse manifestations of aggression.',
    'A narrow or culturally biased definition strengthens the snare-like properties. A broader, more nuanced definition may reveal alternative explanations and weaken the deterministic claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_aggression, conceptual, 'Definition and measurement of ''aggression''').

omega_variable(
    policy_implications_threshold,
    'At what level of genetic correlation should policy interventions based on AGG-1 be considered ethical and effective?',
    'Ethical debates; cost-benefit analyses of potential interventions; public opinion surveys.',
    'A low threshold strengthens the snare-like extraction from individuals and communities. A high threshold weakens the justification for interventions based solely on genetics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_implications_threshold, preference, 'Threshold for policy implications').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agg1_genetic_determinism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agg1_tr_t0, agg1_genetic_determinism, theater_ratio, 0, 0.3).
narrative_ontology:measurement(agg1_tr_t5, agg1_genetic_determinism, theater_ratio, 5, 0.7).
narrative_ontology:measurement(agg1_tr_t10, agg1_genetic_determinism, theater_ratio, 10, 0.75).

% Extraction over time
narrative_ontology:measurement(agg1_be_t0, agg1_genetic_determinism, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(agg1_be_t5, agg1_genetic_determinism, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(agg1_be_t10, agg1_genetic_determinism, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agg1_genetic_determinism, information_standard).
narrative_ontology:affects_constraint(agg1_genetic_determinism, gene_therapy_efficacy).
narrative_ontology:affects_constraint(agg1_genetic_determinism, social_determinism_behavior).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
