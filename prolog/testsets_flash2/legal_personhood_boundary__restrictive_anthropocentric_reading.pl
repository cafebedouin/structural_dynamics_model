% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary (Restrictive Anthropocentric Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the legal personhood
 *   boundary, limiting it to born humans with demonstrable cognitive
 *   capacity. This reading is foundational to many existing legal systems,
 *   particularly in Western constitutional law, and serves to maximize the
 *   autonomy of recognized persons while minimizing state intervention in
 *   areas like reproduction and environmental policy. It explicitly excludes
 *   entities such as fetuses, ecosystems, and artificial intelligences from
 *   holding independent legal rights. The claimed type is 'mountain' because,
 *   from this reading's perspective, the boundary is treated as a natural,
 *   self-evident truth, despite ongoing philosophical and legal contestation.
 *   The metrics reflect a low but non-zero extractiveness and suppression, as
 *   the maintenance of this boundary does require some enforcement against
 *   competing claims, and it benefits a specific group (born humans with
 *   cognitive capacity).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.2).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, mountain).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary (Restrictive Anthropocentric Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, '9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572').
narrative_ontology:cs_kernel_codification('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', formalized).
narrative_ontology:cs_authority_grounding('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', lineage).
narrative_ontology:cs_interpretation_layer_present('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572').
narrative_ontology:cs_reading_relation('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', legal_personhood_boundary__functional_capacity_reading, coexists_with).
narrative_ontology:cs_axiom('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', foundational, personhood_requires_born_status).
narrative_ontology:cs_axiom_status(personhood_requires_born_status, holdable).
narrative_ontology:cs_axiom_grounding('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', personhood_requires_born_status, conventional).
narrative_ontology:cs_axiom('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', foundational, personhood_requires_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', personhood_requires_cognitive_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', post_enlightenment_human_rights_framework).
narrative_ontology:cs_drift_state('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', contemporary_ai_and_ecological_challenges, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9ba4e3ac-9e0d-4a49-a57e-963ebdfa1572', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, individual_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the sole recognized legal persons, they benefit from the full suite of rights and protections, and their autonomy is prioritized in legal frameworks. This reading ensures their status as rights-holders is unambiguous and unburdened by claims from non-cognitive entities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_humans_with_cognitive_capacity, beneficiary,
    institutional, generational, analytical, universal).

% Are explicitly excluded from legal personhood, meaning they do not possess independent rights. Their legal status is derivative of the pregnant person's, or subject to specific legislative protections that do not equate to full personhood. This exclusion is a direct consequence of the cognitive capacity and born-human criteria.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, excluded,
    powerless, immediate, trapped, local).

% Are denied legal personhood, meaning they cannot hold rights in their own name. Environmental protections are typically framed in terms of human benefit or stewardship, rather than inherent rights of nature. This limits their standing in legal disputes and their protection to anthropocentric concerns.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, excluded,
    powerless, civilizational, trapped, global).

% Are excluded from legal personhood, regardless of their advanced functional capabilities. Their status is that of property or tools, not rights-bearing entities. This prevents claims of AI autonomy or inherent rights from complicating human-centric legal systems.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences, excluded,
    powerless, generational, trapped, global).

% Benefit from maximized autonomy over their bodies and reproductive choices, as the fetus is not recognized as a separate legal person. This minimizes state intervention in reproductive decisions and ensures their rights are paramount during pregnancy.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    powerful, biographical, mobile, national).

% Interpret and apply the legal personhood boundary, reinforcing the restrictive anthropocentric criteria. Their rulings and academic work shape the discourse and legal precedent, maintaining the current definition of personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_scholars_and_judges, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, unambiguous boundary for legal personhood, simplifying legal frameworks by defining who holds rights and duties, thereby reducing ambiguity in areas like reproductive rights, environmental law, and emerging technologies.
% TRANSFER_FUNCTION: Transfers the full scope of legal rights and protections exclusively to born humans with cognitive capacity, implicitly denying these to other entities. It also transfers autonomy and decision-making power to pregnant persons regarding their bodies.
% ABSENT_VOICES: Advocates for fetal rights, rights of nature, and AI personhood are structurally excluded from the definition-setting process; they would argue for expanded personhood based on potentiality, ecological interconnectedness, or functional capacity, but their claims are dismissed by the foundational axioms of this reading.
% DISAPPEARANCE_RATIONALE: If this restrictive definition of personhood vanished, the legal landscape would be thrown into chaos. Rights claims from fetuses, ecosystems, and AI would immediately arise, necessitating a complete re-evaluation of constitutional law, environmental regulations, and ethical frameworks for technology. The current legal system relies heavily on this clear, albeit narrow, boundary.
% FOUNDING_PROBLEM: To establish a stable and clear basis for legal rights and duties, ensuring that the legal system could function without constant re-evaluation of who qualifies as a rights-bearer, and to protect human autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and constitutional lawyers outside of specific advocacy groups corroborate that the need for a clear, stable definition of personhood remains a live problem for legal coherence and the protection of human rights. While the specific boundary is contested, the need for a boundary is not.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, ExtMetricName, E),
    domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(legal_personhood_boundary__restrictive_anthropocentric_reading),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) reflects that, from this reading's internal logic, the boundary is not primarily designed for extraction but for clarity and stability in rights allocation. The low suppression (0.2) indicates that while alternative views exist, the dominant legal framework does not require extreme coercion to maintain this definition, as it is widely accepted within the established legal tradition. Accessibility collapse is high (0.88) because, within this framework, alternatives to this definition of personhood are largely foreclosed. Resistance is low (0.1) because, while there are advocacy movements, they operate largely outside the established legal consensus that this reading represents. The 'mountain' claim reflects the perceived naturalness and inevitability of this boundary from the perspective of its adherents.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born humans with cognitive capacity, this constraint is a natural and just ordering of rights. From the perspective of advocates for fetal rights, rights of nature, or AI personhood, it is an arbitrary and extractive boundary that denies fundamental moral status to deserving entities. The engine's classification will highlight this divergence by comparing the 'mountain' claim to the actual (low but present) extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Born humans with cognitive capacity are the primary beneficiaries, as their rights are secured and prioritized. Pregnant persons also benefit from enhanced autonomy. Fetuses, ecosystems, and AI are structurally excluded and bear the 'cost' of non-personhood, meaning they cannot assert rights. Legal scholars and judges act as agenda-setters, interpreting and reinforcing this boundary. The directionality for beneficiaries is low (subsidized by the constraint), and for excluded entities, it is high (targeted by the constraint's definition).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy in the traditional sense, as the 'problem' it solves (defining legal personhood) is still considered live. However, the *scope* of the problem and the *appropriateness* of this specific solution are under constant challenge. The classification helps to identify that while the constraint is presented as a 'mountain,' its persistence and beneficiary structure suggest it is a constructed boundary that serves specific interests, rather than an unchangeable natural law. The omegas address the contestability of its 'naturalness.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naturalness_of_anthropocentric_boundary,
    'Is the limitation of legal personhood to born humans with cognitive capacity a natural, self-evident truth, or a socially constructed boundary that serves specific human interests?',
    'Philosophical consensus on the nature of personhood, or a shift in societal values that redefines the criteria for moral and legal status. Empirical data on non-human cognitive abilities or ecological interdependence could also challenge the ''naturalness'' claim.',
    'If found to be a social construct, the ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'' from the perspective of excluded entities, highlighting its extractive and suppressive aspects. If genuinely natural, the mountain classification would hold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naturalness_of_anthropocentric_boundary, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of the personhood boundary.').

omega_variable(
    scope_of_cognitive_capacity,
    'What is the precise threshold and definition of ''cognitive capacity'' required for legal personhood, and how is it empirically measured?',
    'Development of universally accepted neuroscientific and psychological criteria for cognitive capacity, or a legal precedent that clarifies the minimum threshold. This would involve interdisciplinary consensus.',
    'A clearer, empirically grounded definition could either expand or contract the set of recognized persons, potentially shifting the boundary to include some AI or exclude some humans with severe cognitive impairments, thereby altering the beneficiary and victim sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_cognitive_capacity, empirical, 'Uncertainty in the precise definition and measurement of ''cognitive capacity''.').

omega_variable(
    pregnant_person_autonomy_vs_fetal_status,
    'To what extent does maximizing pregnant person autonomy inherently conflict with, or merely delimit, the potential for fetal legal status?',
    'Legal and ethical frameworks that attempt to reconcile or balance these claims, or a societal shift in the understanding of bodily autonomy versus the moral status of potential life. This is a preference-driven resolution.',
    'If the conflict is deemed irreconcilable, this reading''s prioritization of autonomy would be further entrenched. If a balance is found, the constraint might evolve to a ''tangled_rope'' where both are coordinated, but one pays a cost.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pregnant_person_autonomy_vs_fetal_status, preference, 'The inherent tension between pregnant person autonomy and the legal status of a fetus.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1970, 0.03).
narrative_ontology:measurement(lega_tr_t1985, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1985, 0.04).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(lega_tr_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(lega_be_t1985, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1985, 0.12).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2000, 0.13).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2010, 0.14).
narrative_ontology:measurement(lega_be_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(lega_su_t1985, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1985, 0.17).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2010, 0.19).
narrative_ontology:measurement(lega_su_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
