% ============================================================================
% CONSTRAINT STORY: honor_settlement_legitimacy__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_settlement_legitimacy__contraction_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: honor_settlement_legitimacy__contraction_reading
 *   human_readable: Dueling as Cognitively Unthinkable (Contraction Reading)
 *   domain: historical_sociology/legal_history/cultural_anthropology
 *
 * SUMMARY:
 *   This constraint describes the cultural transformation that rendered
 *   dueling cognitively unthinkable as a legitimate means of honor
 *   settlement, rather than merely illegal or socially disapproved. It is a
 *   'contraction reading' of the broader kernel of honor settlement
 *   legitimacy, emphasizing the deep cultural shift that foreclosed dueling
 *   as a normative possibility. The constraint is classified as a Mountain
 *   because, from the perspective of modern social cognition, the
 *   illegitimacy of dueling is an unchangeable, 'natural' feature of the
 *   cultural landscape, requiring no active enforcement to maintain its
 *   cognitive status.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_settlement_legitimacy__contraction_reading, 0.05).
domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, 0.95).
domain_priors:theater_ratio(honor_settlement_legitimacy__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, 0.98).
narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_settlement_legitimacy__contraction_reading, mountain).
narrative_ontology:human_readable(honor_settlement_legitimacy__contraction_reading, "Dueling as Cognitively Unthinkable (Contraction Reading)").
narrative_ontology:topic_domain(honor_settlement_legitimacy__contraction_reading, "historical_sociology/legal_history/cultural_anthropology").

domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_settlement_legitimacy__contraction_reading, '7bc03d2f-fc64-4c22-ab08-de7a250022a2').
narrative_ontology:cs_kernel_codification('7bc03d2f-fc64-4c22-ab08-de7a250022a2', implicit).
narrative_ontology:cs_authority_grounding('7bc03d2f-fc64-4c22-ab08-de7a250022a2', practice).
narrative_ontology:cs_interpretation_layer_present('7bc03d2f-fc64-4c22-ab08-de7a250022a2').
narrative_ontology:cs_reading_relation('7bc03d2f-fc64-4c22-ab08-de7a250022a2', honor_settlement_legitimacy__drop_reading, forecloses).
narrative_ontology:cs_reading_relation('7bc03d2f-fc64-4c22-ab08-de7a250022a2', honor_settlement_legitimacy__composite_reading, influences).
narrative_ontology:cs_axiom('7bc03d2f-fc64-4c22-ab08-de7a250022a2', foundational, private_violence_is_cognitively_illegitimate).
narrative_ontology:cs_axiom_status(private_violence_is_cognitively_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('7bc03d2f-fc64-4c22-ab08-de7a250022a2', private_violence_is_cognitively_illegitimate, deontological).
narrative_ontology:cs_axiom('7bc03d2f-fc64-4c22-ab08-de7a250022a2', foundational, honor_is_not_defended_by_combat).
narrative_ontology:cs_axiom_status(honor_is_not_defended_by_combat, holdable).
narrative_ontology:cs_axiom_grounding('7bc03d2f-fc64-4c22-ab08-de7a250022a2', honor_is_not_defended_by_combat, conventional).
narrative_ontology:cs_reference_frame('7bc03d2f-fc64-4c22-ab08-de7a250022a2', modern_non_violent_dispute_resolution).
narrative_ontology:cs_drift_state('7bc03d2f-fc64-4c22-ab08-de7a250022a2', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7bc03d2f-fc64-4c22-ab08-de7a250022a2', '').
narrative_ontology:cs_kernel_id(honor_settlement_legitimacy__contraction_reading, honor_settlement_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, modern_state_monopoly_on_violence).
narrative_ontology:constraint_beneficiary(honor_settlement_legitimacy__contraction_reading, bourgeois_public_sphere).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the cultural shift that renders private violence illegitimate, reinforcing its exclusive claim to the use of force. This reading sees the state as a passive beneficiary of a deeper cultural transformation, not its primary driver.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, modern_state_monopoly_on_violence, beneficiary,
    institutional, generational, arbitrage, national).

% Benefits from the establishment of new norms of civility and rational discourse, where disputes are settled through legal or rhetorical means rather than personal combat. This cultural framework is reinforced by dueling's cognitive disappearance.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, bourgeois_public_sphere, beneficiary,
    organized, generational, mobile, national).

% Individuals whose identity and social standing were historically tied to the honor code, for whom dueling was a legitimate means of redress. In this reading, their worldview becomes increasingly unintelligible and marginalized, leading to their cultural exclusion.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, honor_culture_adherents, excluded,
    powerless, biographical, identity_locked, local).

% Analyzes the historical process by which dueling transitioned from a social practice to a cultural impossibility. This seat seeks to understand the mechanisms of cognitive and normative contraction.
narrative_ontology:constraint_stakeholder(honor_settlement_legitimacy__contraction_reading, contemporary_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitated the coordination of social interactions around a new, non-violent framework for dispute resolution, where the very concept of dueling as a legitimate act became culturally incoherent.
% TRANSFER_FUNCTION: Transferred the legitimacy of dispute resolution from individual honor to state-sanctioned legal and social mechanisms, effectively transferring the 'right to violence' from individuals to the state.
% ABSENT_VOICES: The voices of those for whom dueling was a deeply ingrained and legitimate aspect of honor culture are absent from the modern discourse, their worldview having been rendered unintelligible by the cultural transformation. They would argue for the inherent right to defend one's honor through personal combat.
% DISAPPEARANCE_RATIONALE: If the cognitive unthinkability of dueling vanished overnight, the world would remain largely unchanged because the underlying cultural framework that makes dueling illegitimate is deeply embedded. It would not spontaneously re-emerge as a widespread practice; rather, it would remain a historical curiosity or a fringe activity, as its cognitive impossibility is a stable feature of modern social cognition.
% FOUNDING_PROBLEM: The problem of establishing a stable social order where private violence is not a legitimate means of dispute resolution, and where the state holds a monopoly on legitimate force.
% FOUNDING_PROBLEM_CORROBORATION: Historians and legal scholars corroborate that the problem of state monopoly on violence and the delegitimization of private violence remains a live concern, even if dueling itself is no longer a threat. The cultural framework that renders dueling unthinkable continues to serve this foundational purpose.
narrative_ontology:disappearance_verdict(honor_settlement_legitimacy__contraction_reading, world_unchanged).
narrative_ontology:founding_problem_status(honor_settlement_legitimacy__contraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_settlement_legitimacy__contraction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(honor_settlement_legitimacy__contraction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_settlement_legitimacy__contraction_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_settlement_legitimacy__contraction_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(honor_settlement_legitimacy__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(honor_settlement_legitimacy__contraction_reading),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(honor_settlement_legitimacy__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(honor_settlement_legitimacy__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because the constraint primarily represents a cultural shift that benefits society broadly by reducing private violence, rather than extracting from specific parties. Suppression is very high (0.95) because the cultural framework actively suppresses the very idea of dueling as legitimate, making it almost impossible to conceive of as a viable option. Accessibility collapse is near total (0.98) as the cultural framework makes alternatives to non-violent dispute resolution cognitively inaccessible. Resistance is negligible (0.02) because the cultural shift is so profound that active resistance to dueling's illegitimacy is almost non-existent in the modern context. The temporal measurements show a decline in extractiveness and a rise in suppression, reflecting the deepening of the cultural transformation over time.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a historical honor culture adherent, this constraint would be a Snare, actively suppressing their way of life. However, this reading focuses on the *outcome* of the cultural transformation from a modern perspective, where dueling's illegitimacy is a settled, 'natural' fact. The engine's classification will reflect this 'Mountain' status for the contemporary observer, while acknowledging the historical 'Snare' experience for those whose culture was displaced.
 *
 * DIRECTIONALITY LOGIC:
 *   The modern state and the bourgeois public sphere are beneficiaries, as the cultural shift reinforces their foundational principles. Honor culture adherents are 'excluded' in a deep sense: their entire framework of action is rendered unintelligible, leading to their marginalization rather than direct extraction. Contemporary observers are analytical, studying the historical process.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (to establish a non-violent social order) remains live, but its *mechanism* has shifted from active legal prohibition to deep cultural cognitive suppression. This prevents mislabeling it as a Piton, as its function is not atrophied but rather internalized and naturalized within the cultural framework. The 'unthinkability' is the constraint's enduring function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_legal_causation,
    'To what extent was the decline of dueling driven by cultural framework transformation (as this reading claims) versus legal prohibition and state enforcement?',
    'Comparative historical analysis of societies with varying legal enforcement but similar cultural shifts, or vice versa, to disentangle causal pathways.',
    'If legal enforcement was the primary driver, the constraint''s ''emerges_naturally'' claim would be weakened, potentially reclassifying it as a Snare or Tangled Rope maintained by active coercion. If cultural transformation was dominant, the Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_legal_causation, empirical, 'Disentangling the causal weight of cultural vs. legal factors in dueling''s decline.').

omega_variable(
    cognitive_unthinkability_measurement,
    'How can ''cognitive unthinkability'' be empirically measured or demonstrated, beyond mere absence of practice or legal prohibition?',
    'Analysis of historical texts, philosophical treatises, and popular culture for evidence of the *conceptual impossibility* or *moral absurdity* of dueling, rather than just its illegality or social disapproval.',
    'Stronger evidence of cognitive unthinkability reinforces the Mountain classification and the high suppression/accessibility collapse metrics. Weaker evidence might suggest a less profound cultural shift, pushing towards a Piton (theatrical maintenance of a dead practice) or a Snare (active suppression of a still-thinkable alternative).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_unthinkability_measurement, conceptual, 'Operationalizing and measuring the concept of ''cognitive unthinkability'' in historical contexts.').

omega_variable(
    honor_culture_adherent_agency,
    'Did honor culture adherents actively resist or attempt to preserve dueling, or did their cultural framework simply ''contract'' around them?',
    'Micro-historical studies of residual honor communities, examining their internal discourse, resistance strategies, and adaptation to the changing cultural landscape.',
    'Evidence of active, sustained resistance would increase the ''resistance'' metric and potentially challenge the ''emerges_naturally'' claim, suggesting a more coercive, less ''natural'' cultural shift. This could push the classification towards a Snare or Tangled Rope from the perspective of the adherents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(honor_culture_adherent_agency, empirical, 'Assessing the agency and resistance of honor culture adherents during the cultural transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_settlement_legitimacy__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hono_be_t1700, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(hono_be_t1750, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(hono_be_t1800, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1800, 0.07).
narrative_ontology:measurement(hono_be_t1850, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1850, 0.03).
narrative_ontology:measurement(hono_be_t1900, honor_settlement_legitimacy__contraction_reading, base_extractiveness, 1900, 0.01).

% Suppression requirement over time
narrative_ontology:measurement(hono_su_t1700, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1700, 0.7).
narrative_ontology:measurement(hono_su_t1750, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1750, 0.8).
narrative_ontology:measurement(hono_su_t1800, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(hono_su_t1850, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1850, 0.95).
narrative_ontology:measurement(hono_su_t1900, honor_settlement_legitimacy__contraction_reading, suppression_requirement, 1900, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
