% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Dueling's Overdetermined Disappearance (Composite Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint models the decline of dueling as an overdetermined
 *   outcome, resulting from the simultaneous and independent action of
 *   multiple sufficient causes: legal prohibition, institutional
 *   modernization (e.g., courts, banking), cultural shifts (e.g., rise of
 *   dignity culture), and the trauma of the American Civil War. This
 *   'overdetermined composite reading' posits that no single cause was
 *   necessary, but several were sufficient, leading to a robust and
 *   irreversible decline. The constraint is claimed as a Tangled Rope because
 *   it involved both coordination (of new social norms and legal structures)
 *   and extraction (from those whose honor-based identities were suppressed).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.7).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Dueling's Overdetermined Disappearance (Composite Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, 'f75a0bb2-90dc-44e5-83a8-b0b65c0115de').
narrative_ontology:cs_kernel_codification('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', implicit).
narrative_ontology:cs_authority_grounding('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', distributed).
narrative_ontology:cs_reading_relation('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', dueling_disappearance_mechanism__institutional_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', foundational, multiple_sufficient_causes_operate_simultaneously).
narrative_ontology:cs_axiom_status(multiple_sufficient_causes_operate_simultaneously, holdable).
narrative_ontology:cs_axiom_grounding('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', multiple_sufficient_causes_operate_simultaneously, empirically_contingent).
narrative_ontology:cs_reference_frame('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', historical_causal_pluralism).
narrative_ontology:cs_drift_state('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', contemporary_historiography, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f75a0bb2-90dc-44e5-83a8-b0b65c0115de', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_merchant_class).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_society).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_gentlemen).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefited from the decline of dueling by consolidating its monopoly on legitimate violence and dispute resolution. Actively enforced legal prohibitions against dueling, gradually increasing penalties and social stigma.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, state_legal_apparatus, agenda_setter,
    institutional, generational, mobile, national).

% Benefited from a more stable, predictable social order where disputes were resolved through legal or commercial means rather than personal combat. Their economic interests favored a decline in honor-based violence.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, bourgeois_merchant_class, beneficiary,
    powerful, biographical, arbitrage, regional).

% The trauma of the Civil War made ritualized violence less appealing and less legitimate, contributing to a broader cultural shift away from dueling. This societal shift reinforced legal and institutional pressures.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, post_civil_war_society, beneficiary,
    organized, generational, constrained, national).

% Lost a central mechanism for defending personal honor and social standing. Faced increasing legal penalties and social ostracism for attempting to duel, forcing them to adapt or retreat from public life. Their identity was deeply tied to the practice.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_adherents, payer,
    moderate, biographical, identity_locked, local).

% A specific subset of honor culture adherents, particularly in the American South, who found their traditional means of dispute resolution and status maintenance eroded by the composite forces. Their social standing and self-concept were challenged.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_gentlemen, payer,
    moderate, biographical, identity_locked, regional).

% Analyze the complex interplay of legal, institutional, and cultural factors that led to dueling's decline. They seek to understand the relative weight and timing of each contributing cause.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, cultural_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The composite mechanisms (legal, institutional, cultural) collectively coordinated a shift in societal norms and dispute resolution, moving away from personal combat towards state-sanctioned or commercial means.
% TRANSFER_FUNCTION: Transferred the authority for dispute resolution and the definition of honor from individuals and honor-bound communities to the state and emerging bourgeois institutions. It also transferred social capital and legitimacy to those who conformed to the new norms.
% ABSENT_VOICES: Those who continued to believe in the sanctity of honor and the necessity of dueling, particularly in isolated pockets of traditional society, were increasingly marginalized and silenced by the dominant legal and cultural narratives.
% DISAPPEARANCE_RATIONALE: If the composite forces that led to dueling's decline had not existed, or had vanished, the social and legal landscape of the 19th century would have been profoundly different. Honor culture would have retained more influence, and state authority over violence would have been less consolidated.
% FOUNDING_PROBLEM: The problem of maintaining social order and resolving disputes in a rapidly modernizing society, where traditional honor codes clashed with emerging legal and commercial norms.
% FOUNDING_PROBLEM_CORROBORATION: Cultural historians and legal scholars attest that societies continually grapple with the tension between individual honor/dignity and state authority/legal process, making the underlying problem of dispute resolution a live one, even if dueling itself is dead. This corroboration comes from academic analysis outside the direct beneficiaries of dueling's decline.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, world_rearranges).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because the composite mechanisms effectively extracted the right to personal violence and honor defense from individuals, centralizing it within the state and new social institutions. Suppression is also high, as legal prohibitions were actively enforced, and cultural shifts created strong social pressure against dueling. Theater ratio is low because the decline was genuine and functional, not merely performative. The slight dip in extractiveness and suppression towards the end of the interval reflects the point where dueling had largely disappeared, and the active 'work' of its suppression became less intense, shifting to maintenance of the new norms.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the state and modernizing classes, the decline of dueling was a positive development, a move towards a more rational and orderly society. From the perspective of honor culture adherents, it was a loss of a vital mechanism for self-respect and social standing. This reading acknowledges the multiple, sometimes conflicting, motivations and outcomes.
 *
 * DIRECTIONALITY LOGIC:
 *   The state legal apparatus, the bourgeois merchant class, and post-Civil War society are beneficiaries, as they gained from the new social order. Honor culture adherents and southern gentlemen are victims, as their traditional practices and identities were suppressed. The composite nature means that different beneficiaries gained from different causal pathways, but all contributed to the overall decline.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causal_weight_distribution,
    'What was the precise causal weight of each independent sufficient condition (legal, institutional, cultural, traumatic) in dueling''s decline?',
    'Counterfactual historical analysis, comparative studies of societies with different combinations of these factors, or quantitative historical sociology if data permits.',
    'Resolving this would refine our understanding of which mechanisms were most potent, potentially shifting the emphasis on which beneficiaries gained most or which victims were most directly impacted by specific forces. It would not change the overall ''overdetermined'' verdict but would detail its internal structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_weight_distribution, empirical, 'Determining the relative contribution of each overdetermining cause.').

omega_variable(
    non_separability_of_extraction,
    'Given the overdetermined nature, is it possible to meaningfully separate the ''extraction'' attributable to each causal pathway, or is the composite extraction fundamentally non-separable?',
    'Conceptual analysis of causal attribution in overdetermined systems, potentially drawing on philosophical literature on causation and responsibility. If extraction is truly non-separable, then any attempt to assign a single ε to a specific mechanism is flawed.',
    'If non-separable, the current single extractiveness score for the composite is the only valid measure. If separable, it would imply that the kernel should be decomposed into sub-constraints for each causal pathway, each with its own ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_separability_of_extraction, conceptual, 'Whether extraction can be disaggregated across multiple overdetermining causes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 1800, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(duel_tr_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1820, 0.15).
narrative_ontology:measurement(duel_tr_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1840, 0.2).
narrative_ontology:measurement(duel_tr_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1860, 0.25).
narrative_ontology:measurement(duel_tr_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1880, 0.2).
narrative_ontology:measurement(duel_tr_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 1900, 0.15).

% Extraction over time
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1800, 0.4).
narrative_ontology:measurement(duel_be_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1820, 0.5).
narrative_ontology:measurement(duel_be_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1840, 0.6).
narrative_ontology:measurement(duel_be_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1860, 0.65).
narrative_ontology:measurement(duel_be_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1880, 0.68).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 1900, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1800, 0.3).
narrative_ontology:measurement(duel_su_t1820, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1820, 0.45).
narrative_ontology:measurement(duel_su_t1840, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1840, 0.6).
narrative_ontology:measurement(duel_su_t1860, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1860, 0.7).
narrative_ontology:measurement(duel_su_t1880, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1880, 0.75).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 1900, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism__institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel, focusing on the overdetermined composite causality. It is linked to sibling readings that emphasize cultural contraction and institutional displacement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
