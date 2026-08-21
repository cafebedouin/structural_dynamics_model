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
 *   domain: religious_authority/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint represents the 'substitutionist' reading of a divine
 *   marriage command, where a new 'Manifesto' is interpreted as a superseding
 *   revelation that doctrinally requires monogamy, replacing prior polygamous
 *   practices. This reading is driven by the institutional leadership to
 *   align with external societal norms and avoid persecution, but it creates
 *   significant internal extraction for those who adhere to the older
 *   doctrine. The claimed type is 'tangled_rope' because it genuinely
 *   coordinates institutional survival and social integration, but does so
 *   through asymmetric extraction from dissenting members.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__substitutionist_reading, 0.65).
domain_priors:suppression_score(divine_marriage_command__substitutionist_reading, 0.78).
domain_priors:theater_ratio(divine_marriage_command__substitutionist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__substitutionist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__substitutionist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__substitutionist_reading, "Divine Marriage Command (Substitutionist Reading)").
narrative_ontology:topic_domain(divine_marriage_command__substitutionist_reading, "religious_authority/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__substitutionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__substitutionist_reading, 'beb15ec5-004e-4122-8b10-38925d4dd494').
narrative_ontology:cs_kernel_codification('beb15ec5-004e-4122-8b10-38925d4dd494', formalized).
narrative_ontology:cs_authority_grounding('beb15ec5-004e-4122-8b10-38925d4dd494', lineage).
narrative_ontology:cs_interpretation_layer_present('beb15ec5-004e-4122-8b10-38925d4dd494').
narrative_ontology:cs_reading_relation('beb15ec5-004e-4122-8b10-38925d4dd494', divine_marriage_command__continuationist_reading, forecloses).
narrative_ontology:cs_reading_relation('beb15ec5-004e-4122-8b10-38925d4dd494', divine_marriage_command__coercion_visibility_reading, influences).
narrative_ontology:cs_axiom('beb15ec5-004e-4122-8b10-38925d4dd494', foundational, new_revelation_supersedes_prior_command).
narrative_ontology:cs_axiom_status(new_revelation_supersedes_prior_command, holdable).
narrative_ontology:cs_axiom_grounding('beb15ec5-004e-4122-8b10-38925d4dd494', new_revelation_supersedes_prior_command, theological).
narrative_ontology:cs_axiom('beb15ec5-004e-4122-8b10-38925d4dd494', foundational, monogamy_is_divine_law).
narrative_ontology:cs_axiom_status(monogamy_is_divine_law, holdable).
narrative_ontology:cs_axiom_grounding('beb15ec5-004e-4122-8b10-38925d4dd494', monogamy_is_divine_law, theological).
narrative_ontology:cs_reference_frame('beb15ec5-004e-4122-8b10-38925d4dd494', post_manifesto_monogamous_order).
narrative_ontology:cs_drift_state('beb15ec5-004e-4122-8b10-38925d4dd494', contemporary_fundamentalist_dissent, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('beb15ec5-004e-4122-8b10-38925d4dd494', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__substitutionist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(divine_marriage_command__substitutionist_reading, monogamous_members).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists).
narrative_ontology:constraint_victim(divine_marriage_command__substitutionist_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that issued the Manifesto, framing it as new revelation. They benefit from maintaining institutional legitimacy and avoiding federal persecution, which requires enforcing monogamy and suppressing dissent. Their authority is grounded in this new interpretation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Members who adhere to monogamy and benefit from the institution's social acceptance and legal standing. They are aligned with the new doctrine and see it as a path to continued prosperity and integration within wider society.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, monogamous_members, beneficiary,
    moderate, biographical, mobile, national).

% Members who believe polygamy is a divine command and reject the Manifesto as a doctrinal change. They face excommunication, social ostracization, and loss of community, making exit extremely costly due to deep identity fusion with the prior practice.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, polygamous_fundamentalists, payer,
    powerless, generational, identity_locked, local).

% Members who struggle with the doctrinal shift, feeling it compromises core tenets of their faith, but are not necessarily practicing polygamy. They bear the cost of internal conflict and potential loss of standing if they voice dissent too strongly.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, dissenting_members, payer,
    moderate, biographical, constrained, regional).

% The external authority that exerted pressure against polygamy, leading to the Manifesto. They observe the institution's compliance and maintain legal pressure against any resurgence of polygamous practices.
narrative_ontology:constraint_stakeholder(divine_marriage_command__substitutionist_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the institution's theological doctrine with external legal and social norms, allowing for continued institutional existence and growth within the broader society by presenting a unified, monogamous front.
% TRANSFER_FUNCTION: Transfers theological legitimacy and social acceptance to the institutional leadership and monogamous members, while transferring the cost of doctrinal shift, excommunication, and social marginalization to polygamous fundamentalists and dissenting members.
% ABSENT_VOICES: Historical figures and early adherents who practiced polygamy as a core tenet would object to the reinterpretation, but their voices are now framed as superseded by new revelation. External theological critics who view the shift as opportunistic rather than divinely inspired are also excluded from the internal discourse.
% DISAPPEARANCE_RATIONALE: If this substitutionist reading vanished, the institution would face an immediate crisis of legitimacy, with fundamentalist factions reasserting prior doctrines and the federal government likely renewing legal pressure. The social and theological landscape of the community would fragment, leading to significant reorganization.
% FOUNDING_PROBLEM: The institution faced existential threat from federal anti-polygamy laws, including confiscation of property and imprisonment of leaders, which jeopardized its very survival and the religious freedom of its members.
% FOUNDING_PROBLEM_CORROBORATION: Institutional leadership attests the problem is live, citing the need for continued social acceptance and legal standing. External historians and legal scholars corroborate the historical threat but often frame the 'solution' as a pragmatic adaptation rather than a purely divine command, noting the ongoing tension with fundamentalist groups.
narrative_ontology:disappearance_verdict(divine_marriage_command__substitutionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__substitutionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__substitutionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(divine_marriage_command__substitutionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__substitutionist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.65) because the doctrinal shift imposes severe costs on those who previously practiced polygamy, forcing them to abandon deeply held beliefs and practices or face excommunication. Suppression (0.78) is also high, as the institutional leadership actively enforces the new doctrine through social pressure, disciplinary action, and excommunication, effectively collapsing alternatives for members. The theater ratio (0.4) reflects the performative aspect of framing a pragmatic institutional adaptation as a purely divine revelation, obscuring the coercive pressures that drove the change. The initial rise in extractiveness and suppression reflects the period of active enforcement and doctrinal consolidation following the Manifesto.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, the Manifesto is a necessary and divinely guided adaptation for survival and progress. From the perspective of polygamous fundamentalists, it is a betrayal of core religious principles, enforced through coercion and leading to their marginalization. The engine's classification will highlight this divergence, showing a 'tangled_rope' for the institution as a whole, but potentially a 'snare' for the fundamentalists due to their identity-locked exit options and high extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership and monogamous members are beneficiaries, gaining social acceptance and continued institutional viability. Polygamous fundamentalists and dissenting members are victims, bearing the costs of doctrinal shift, social ostracization, and excommunication. The federal government acts as an external observer and enforcer of the new norm, indirectly benefiting from the institution's compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_vs_adaptation,
    'Is the Manifesto a genuine new revelation from divine authority, or a pragmatic institutional adaptation to external coercive pressure?',
    'Theological analysis of internal consistency with prior revelations, comparison with other religious traditions'' responses to similar external pressures, and historical documentation of internal debates within the leadership prior to the Manifesto''s issuance.',
    'If primarily an adaptation, the ''divine command'' framing is a theatrical overlay, increasing the effective theater_ratio and potentially reclassifying the constraint as a ''snare'' for dissenting members. If a genuine revelation, the extraction is a legitimate cost of adherence to evolving divine will.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_vs_adaptation, conceptual, 'Ambiguity regarding the true nature of the Manifesto''s origin and justification.').

omega_variable(
    identity_lock_severity,
    'How deeply are polygamous fundamentalists identity-locked to the prior doctrine, and what proportion of their suppression is internalized versus structural?',
    'Longitudinal studies of excommunicated members'' post-exit psychological and social well-being, and analysis of their ability to form alternative communities or belief systems. If suppression persists after structural barriers are removed, it indicates internalized identity lock.',
    'If identity lock is severe and internalized, the effective suppression and extractiveness for this group are higher than structural measures suggest, pushing their seat classification closer to a ''snare'' even if the overall constraint is a ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_severity, empirical, 'The extent to which identity fusion with prior doctrine makes exit from the new command prohibitively costly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__substitutionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__substitutionist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t10, divine_marriage_command__substitutionist_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__substitutionist_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(divi_tr_t30, divine_marriage_command__substitutionist_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__substitutionist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(divi_tr_t50, divine_marriage_command__substitutionist_reading, theater_ratio, 50, 0.4).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__substitutionist_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(divi_be_t10, divine_marriage_command__substitutionist_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__substitutionist_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(divi_be_t30, divine_marriage_command__substitutionist_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__substitutionist_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(divi_be_t50, divine_marriage_command__substitutionist_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(divi_su_t0, divine_marriage_command__substitutionist_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(divi_su_t10, divine_marriage_command__substitutionist_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(divi_su_t20, divine_marriage_command__substitutionist_reading, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(divi_su_t30, divine_marriage_command__substitutionist_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement(divi_su_t40, divine_marriage_command__substitutionist_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(divi_su_t50, divine_marriage_command__substitutionist_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__substitutionist_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__continuationist_reading).
narrative_ontology:affects_constraint(divine_marriage_command__substitutionist_reading, divine_marriage_command__coercion_visibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'divine_marriage_command' kernel. This 'substitutionist' reading asserts a new revelation supersedes prior commands, directly conflicting with the 'continuationist' reading (which holds polygamy remains valid) and influencing the 'coercion_visibility' reading (which frames the Manifesto as a pragmatic response to external pressure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
