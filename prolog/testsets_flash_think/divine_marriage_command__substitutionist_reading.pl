% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__substitutionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__substitutionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: divine_marriage_command__substitutionist_reading
 *   human_readable: Divine Marriage Command (Substitutionist Reading)
 *   domain: religious/political_theology/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates the 'substitutionist_reading' of the
 *   'divine_marriage_command' kernel. It asserts that the Manifesto
 *   represents a new divine revelation, superseding prior commands and making
 *   monogamy doctrinally required. This reading frames the shift as a
 *   theological evolution rather than a pragmatic response to external
 *   coercion. The constraint operates as a Tangled Rope, coordinating
 *   institutional survival and social acceptance while extracting significant
 *   costs from those who adhered to the prior practice of polygamy.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.85).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.9).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious/political_theology/commitment_systems").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c').
narrative_ontology:cs_kernel_codification('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', fixed_text).
narrative_ontology:cs_authority_grounding('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', lineage).
narrative_ontology:cs_interpretation_layer_present('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c').
narrative_ontology:cs_reading_relation('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_reading_relation('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_axiom('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', foundational, divine_will_is_progressive).
narrative_ontology:cs_axiom_status(divine_will_is_progressive, holdable).
narrative_ontology:cs_axiom_grounding('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', divine_will_is_progressive, theological).
narrative_ontology:cs_axiom('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', secondary, institutional_unity_reflects_divine_order).
narrative_ontology:cs_axiom_status(institutional_unity_reflects_divine_order, holdable).
narrative_ontology:cs_axiom_grounding('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', institutional_unity_reflects_divine_order, theological).
narrative_ontology:cs_reference_frame('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', monogamy_as_divine_law).
narrative_ontology:cs_drift_state('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e5d0db75-b1fe-455e-8e2c-1e4cd6ab608c', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, conforming_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamist_fundamentalists).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The highest ecclesiastical authority, responsible for receiving and interpreting divine revelation. They promulgated the Manifesto, establishing monogamy as the new, binding doctrine, and enforce it through ecclesiastical courts and excommunication. Their legitimacy is tied to maintaining institutional unity and compliance with secular law.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Members who accept the new doctrine and conform to monogamous marriage. They benefit from social acceptance, institutional standing, and the avoidance of legal persecution. Their conformity reinforces the new doctrine's legitimacy and their place within the community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, conforming_members, beneficiary,
    moderate, biographical, constrained, national).

% Members who continue to believe in and practice polygamy, viewing the Manifesto as a temporary suspension under duress rather than a doctrinal change. They face excommunication, social ostracization, and loss of institutional standing. Their identity is deeply tied to the prior practice, making exit or conformity extremely costly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamist_fundamentalists, payer,
    powerless, generational, identity_locked, local).

% Members who struggle with the new doctrine, perhaps due to personal conviction or family ties to polygamy, but may not actively practice it. They face social pressure to conform and risk marginalization if they voice dissent, but may not be as identity-locked as fundamentalists.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, dissenting_members, payer,
    moderate, biographical, constrained, national).

% Government bodies that historically persecuted the institution for polygamy. Their pressure was a key driver for the Manifesto. They observe the institution's compliance with monogamous laws, and their continued scrutiny underpins the enforcement of the new doctrine.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, secular_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies the religious community under a single, legally compliant marriage doctrine, ensuring institutional survival and social acceptance in a secular state.
% TRANSFER_FUNCTION: Transfers adherence to a new doctrine, social standing, and potentially family structures from those practicing polygamy to the new monogamous norm, consolidating institutional power and ensuring its legal standing.
% ABSENT_VOICES: Descendants of polygamist families who might feel their heritage is being erased; historical figures whose practices are now deemed apostate. Their voices are suppressed by the new doctrinal framing and institutional enforcement.
% DISAPPEARANCE_RATIONALE: If the divine command for monogamy and its enforcement vanished, the entire social and theological structure of the religious community would be thrown into chaos. The institution's relationship with its members and secular society would fundamentally change, potentially leading to a resurgence of polygamous practices and renewed conflict with external authorities.
% FOUNDING_PROBLEM: The institution faced existential threat from secular authorities due to its practice of polygamy, leading to legal persecution, confiscation of property, and social ostracization, jeopardizing its very existence.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, government decrees, and independent sociological studies from the period corroborate the severe external pressure and legal threats faced by the institution, confirming the existential nature of the founding problem. The need for institutional survival and social acceptance is ongoing.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__substitutionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__substitutionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) due to the profound personal and social costs imposed on those who must abandon deeply held practices and beliefs. Suppression is also very high (0.90) as the institution actively enforces the new doctrine through excommunication, social ostracization, and doctrinal policing. The theater ratio, while initially higher to establish the 'revelation' narrative, stabilizes at a moderate level (0.30) as the new doctrine becomes normalized, but still involves performative aspects to maintain its divine legitimacy. Resistance is significant (0.70) from those who view the change as apostasy, but this resistance is actively suppressed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership and conforming members, the constraint is a necessary divine command ensuring the community's future. From the perspective of polygamist fundamentalists, it is an act of coercion and apostasy, fundamentally altering their religious identity and practice. The engine's classification as a Tangled Rope captures this divergence, highlighting both the coordination function for the institution and the asymmetric extraction from dissenters.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary and agenda-setter, gaining unified doctrine and legal compliance. Conforming members benefit from social cohesion and institutional standing. Polygamist fundamentalists and dissenting members are the targets, bearing the costs of excommunication, social pressure, and the abandonment of their identity-locked practices. Secular authorities act as observers, their historical pressure having driven the initial shift, and their continued scrutiny underpins the enforcement of the new doctrine.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_pragmatism,
    'Is the Manifesto a genuine divine revelation, or primarily a pragmatic response to severe external coercion from secular authorities?',
    'Analysis of internal theological discourse preceding the Manifesto, comparison with other instances of ''revelation'' in the institution''s history, and independent historical accounts of the external pressures.',
    'If primarily pragmatic, the ''revelation'' aspect is a theatrical overlay, increasing the effective theater_ratio and shifting the constraint closer to a Snare for those who internalize the ''revelation'' as truth. If genuine, the theological grounding is stronger, dampening the perceived extractiveness for believers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_pragmatism, conceptual, 'Ambiguity between divine revelation and pragmatic institutional survival.').

omega_variable(
    superseding_vs_suspension,
    'Is the Manifesto a permanent supersession of prior divine commands regarding marriage, or a temporary suspension under duress, with the prior command remaining valid in principle?',
    'Further doctrinal pronouncements, or the emergence of new theological interpretations that explicitly address the permanence of the change. The persistence of ''continuationist'' groups also provides empirical evidence of this ambiguity.',
    'If a temporary suspension, the extractiveness for polygamist fundamentalists is amplified, as they perceive the constraint as an unjust, temporary imposition rather than a new divine law. If permanent, the extractiveness is still high, but framed within a new theological reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(superseding_vs_suspension, conceptual, 'Permanence of doctrinal change vs. temporary suspension.').

omega_variable(
    internalized_suppression_among_conforming_members,
    'To what extent have conforming members internalized the new monogamous doctrine, such that their adherence is self-enforced rather than externally coerced?',
    'Sociological studies on the attitudes and beliefs of conforming members, particularly those with family histories of polygamy, examining their psychological and social integration of the new norm.',
    'If internalized, the effective suppression for conforming members is lower, as they experience the constraint as a chosen norm. If adherence is still primarily driven by fear of excommunication or social pressure, the effective suppression remains high, even for those who outwardly conform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_among_conforming_members, empirical, 'Structural vs. internalized suppression mechanism for conforming members.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.5).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.3).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__substitutionist_reading, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.78).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.85).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.86).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__substitutionist_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.87).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.9).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.91).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__substitutionist_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'divine_marriage_command' kernel, each representing a distinct structural interpretation of the Manifesto's impact on marriage doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
