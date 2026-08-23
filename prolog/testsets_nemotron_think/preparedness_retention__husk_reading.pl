% ============================================================================
% CONSTRAINT STORY: preparedness_retention__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_retention__husk_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: preparedness_retention__husk_reading
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: disaster_preparedness/governance
 *
 * SUMMARY:
 *   Disaster preparedness systems in many jurisdictions have evolved into
 *   memorial performances: mandated drills, inspections, and compliance
 *   paperwork create the appearance of readiness while the tacit skills,
 *   equipment maintenance, and organizational memory needed for actual
 *   response atrophy. Resources flow to visible compliance (auditable drills,
 *   certified checklists) rather than to the costly, invisible work of
 *   retaining live competence (scenario-based training, equipment rotation,
 *   experienced personnel retention). The constraint is the system of
 *   mandated preparedness rituals; the husk reading claims this system is a
 *   snare — extraction of legitimacy and budgets under a coordination cover —
 *   with victims being the public and responders who face disasters without
 *   real capacity.
 *
 * KEY AGENTS:
 *   - disaster_management_agencies: Primary agenda_setter and beneficiary (institutional legitimacy) — institutional/identity_locked
 *   - political_leadership: Beneficiary (cheap visibility) — institutional/mobile
 *   - general_public: Primary victim (payer) — powerless/trapped
 *   - frontline_responders: Victim (payer) — moderate/constrained
 *   - vulnerable_populations: Victim (payer) — powerless/trapped
 *   - independent_experts: Observer — analytical/analytical
 *   - alternative_training_providers: Excluded — moderate/constrained
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_retention__husk_reading, 0.75).
domain_priors:suppression_score(preparedness_retention__husk_reading, 0.8).
domain_priors:theater_ratio(preparedness_retention__husk_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(preparedness_retention__husk_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_retention__husk_reading, snare).
narrative_ontology:human_readable(preparedness_retention__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_retention__husk_reading, "disaster_preparedness/governance").

domain_priors:requires_active_enforcement(preparedness_retention__husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_retention__husk_reading, '5d6b88d5-8276-4103-9257-478fdb16fd9c').
narrative_ontology:cs_kernel_codification('5d6b88d5-8276-4103-9257-478fdb16fd9c', distributed).
narrative_ontology:cs_authority_grounding('5d6b88d5-8276-4103-9257-478fdb16fd9c', extraction).
narrative_ontology:cs_interpretation_layer_present('5d6b88d5-8276-4103-9257-478fdb16fd9c').
narrative_ontology:cs_reading_relation('5d6b88d5-8276-4103-9257-478fdb16fd9c', preparedness_retention__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d6b88d5-8276-4103-9257-478fdb16fd9c', preparedness_retention__hybrid_reading, influences).
narrative_ontology:cs_axiom('5d6b88d5-8276-4103-9257-478fdb16fd9c', foundational, preparedness_is_ceremonial_not_competence).
narrative_ontology:cs_axiom_status(preparedness_is_ceremonial_not_competence, holdable).
narrative_ontology:cs_axiom_grounding('5d6b88d5-8276-4103-9257-478fdb16fd9c', preparedness_is_ceremonial_not_competence, empirically_contingent).
narrative_ontology:cs_axiom('5d6b88d5-8276-4103-9257-478fdb16fd9c', secondary, institutional_legitimacy_requires_visible_compliance).
narrative_ontology:cs_axiom_status(institutional_legitimacy_requires_visible_compliance, holdable).
narrative_ontology:cs_axiom_grounding('5d6b88d5-8276-4103-9257-478fdb16fd9c', institutional_legitimacy_requires_visible_compliance, conventional).
narrative_ontology:cs_reference_frame('5d6b88d5-8276-4103-9257-478fdb16fd9c', post_catastrophe_preparedness_mandate).
narrative_ontology:cs_drift_state('5d6b88d5-8276-4103-9257-478fdb16fd9c', contemporary_audit_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5d6b88d5-8276-4103-9257-478fdb16fd9c', '').
narrative_ontology:cs_kernel_id(preparedness_retention__husk_reading, preparedness_retention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, disaster_management_agencies).
narrative_ontology:constraint_beneficiary(preparedness_retention__husk_reading, political_leadership).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, general_public).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_retention__husk_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(preparedness_retention__husk_reading, institutional_legitimacy_through_visible_compliance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% They mandate and conduct drills and inspections to demonstrate preparedness, but allocate resources to visible compliance rather than deep skill retention. Their legitimacy and budget authority depend on the appearance of preparedness; their institutional identity is fused with the ritual framework.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, disaster_management_agencies, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_retention__husk_reading, disaster_management_agencies, beneficiary).

% They benefit from the appearance of preparedness without investing in costly long-term competence. Drills provide photo opportunities and audit trails; real competence requires sustained funding with no immediate visibility. They can move on before disasters expose the gap.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, political_leadership, beneficiary,
    institutional, biographical, mobile, national).

% They pay taxes for preparedness but receive only ritualistic drills. When disaster strikes, the lack of live competence costs lives and livelihoods. They cannot exit the jurisdiction and have no voice in preparedness priorities.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, general_public, payer,
    powerless, biographical, trapped, national).

% They participate in drills that don't build real competence, and when real events occur, they lack the tacit skills needed. They can change jobs but at high personal cost; many internalize the ritual as competence, making suppression partly internalized.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, frontline_responders, payer,
    moderate, biographical, constrained, regional).

% They are disproportionately harmed when response capacity fails (e.g., evacuation plans that exist only on paper). They have no voice in preparedness priorities and no exit options.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% They observe the gap between ritual and competence through after-action reviews and comparative studies, but are excluded from decision-making. Their analyses are cited in reports but do not shift resource allocation.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, independent_experts, observer,
    analytical, generational, analytical, global).

% They could provide competence-based, scenario-driven training but are excluded by the compliance framework that rewards checkbox drills. Their exclusion is structural: the mandate specifies ritual forms, not competence outcomes.
narrative_ontology:constraint_stakeholder(preparedness_retention__husk_reading, alternative_training_providers, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_retention__husk_reading, disaster_management_agencies).
narrative_ontology:fixing_cost_class(preparedness_retention__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement claims to coordinate societal readiness for disasters through standardized drills and inspections.
% TRANSFER_FUNCTION: Resources (funding, time, attention) are transferred from actual competence-building (training, equipment, maintenance) to visible compliance activities (drills, paperwork, inspections), benefiting institutional legitimacy at the expense of response capacity.
% ABSENT_VOICES: Vulnerable populations and frontline responders who would demand real competence over ritual are structurally excluded; alternative training providers who could offer competence-based approaches are kept out by the compliance framework.
% DISAPPEARANCE_RATIONALE: If the memorial performance disappeared, disaster management agencies would lose their primary legitimacy signal, political leadership would lose a cheap visibility tool, and the public would initially be less 'drilled' but resources could be redirected to actual training — the system would reorganize around genuine competence or collapse.
% FOUNDING_PROBLEM: The arrangement was built to ensure societal readiness for disasters after a catastrophic event revealed unpreparedness.
% FOUNDING_PROBLEM_CORROBORATION: Independent disaster studies (e.g., post-event investigations like the 2005 Hurricane Katrina reviews, 2011 Fukushima reports) attest that drill compliance does not correlate with response effectiveness; the agencies themselves acknowledge 'drill fatigue' but maintain the rituals for accountability.
narrative_ontology:disappearance_verdict(preparedness_retention__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_retention__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_retention__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_retention__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_retention__husk_reading, 0.75, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_retention__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_retention__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_retention__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.75) because budgets and personnel time are captured by ritual compliance rather than competence-building. Suppression is high (0.8) because mandates and funding rules actively suppress alternative training models. Theater ratio is very high (0.85) — the constraint's visible activity is almost entirely performative. Accessibility collapse (0.65) reflects that once the ritual framework is understood, alternatives (competence-based training) are structurally inaccessible due to locked resource flows. Resistance (0.5) is moderate: frontline responders and experts critique the system but lack leverage to change mandates. The claimed type is snare: the coordination story (preparedness) is cover for extraction of legitimacy and resources.
 *
 * PERSPECTIVAL GAP:
 *   From the agency seat, the constraint feels like coordination (they run drills, produce reports, satisfy audits). From the public/responder seats, the same structure operates as extraction: their taxes and labor fund rituals that do not protect them. The engine computes this divergence from the structural data: agenda_setter/beneficiary seats get low directionality (subsidy), payer seats get high directionality (extraction).
 *
 * DIRECTIONALITY LOGIC:
 *   Disaster management agencies and political leadership are beneficiaries: they collect legitimacy and budget authority (d near 0.0). General public, frontline responders, and vulnerable populations are payers: they bear the cost of failed response when disasters strike (d near 1.0). Frontline responders have constrained exit (cannot easily leave profession), making their effective extraction higher. Vulnerable populations are trapped (no jurisdictional exit). Alternative training providers are excluded (their coordination function would compete with the ritual).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-catastrophe readiness) is dead: the original catastrophe is historical, and the current rituals do not maintain the competence they were built to ensure. The arrangement persists because it solves a different problem: institutional legitimacy and budget protection. This is mandatrophy — the mandate has outlived its function but the constraint remains. The classification as snare (not rope or tangled_rope) prevents mislabeling the coordination cover as genuine function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the husk_reading of the preparedness_retention kernel; how do the sibling readings (competence_reading, hybrid_reading) structurally differ in their beneficiary/victim maps and extraction profiles?',
    'Author separate constraint stories for competence_reading and hybrid_reading; compare their base_properties and stakeholder structures to this reading.',
    'If competence_reading shows low extractiveness and high genuine coordination, the kernel is stratified (hybrid_reading correct). If husk_reading alone captures the dominant institutional pattern, the kernel is a contested cover story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Commitment-system framing: this reading vs. sibling readings of the same kernel').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of genuine competence-building structural (budget rules, mandate design) or internalized (frontline responders believe drills equal competence)?',
    'Post-exit suppression trajectory: if responders who leave the system still treat ritual as competence, internalized component is high.',
    'If internalized, effective suppression is higher than structural measure; the constraint persists even without active enforcement because the targets carry the suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in interpersonal/institutional constraints').

omega_variable(
    residual_coordination_function,
    'Do the rituals retain any genuine coordination value (e.g., minimal familiarity with procedures) or is the coordination story pure cover?',
    'Natural experiment: compare outcomes in jurisdictions that replaced ritual drills with competence-based training vs. those that kept rituals.',
    'If residual coordination exists, the constraint is tangled_rope not snare; if zero, snare classification is robust.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(residual_coordination_function, empirical, 'Whether the constraint has any genuine coordination function beneath the ceremonial layer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_retention__husk_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_retention__husk_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(prep_tr_t6, preparedness_retention__husk_reading, theater_ratio, 6, 0.62).
narrative_ontology:measurement(prep_tr_t12, preparedness_retention__husk_reading, theater_ratio, 12, 0.7).
narrative_ontology:measurement(prep_tr_t18, preparedness_retention__husk_reading, theater_ratio, 18, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_retention__husk_reading, theater_ratio, 24, 0.82).
narrative_ontology:measurement(prep_tr_t30, preparedness_retention__husk_reading, theater_ratio, 30, 0.85).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_retention__husk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_be_t6, preparedness_retention__husk_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(prep_be_t12, preparedness_retention__husk_reading, base_extractiveness, 12, 0.62).
narrative_ontology:measurement(prep_be_t18, preparedness_retention__husk_reading, base_extractiveness, 18, 0.68).
narrative_ontology:measurement(prep_be_t24, preparedness_retention__husk_reading, base_extractiveness, 24, 0.72).
narrative_ontology:measurement(prep_be_t30, preparedness_retention__husk_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_retention__husk_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(prep_su_t6, preparedness_retention__husk_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement(prep_su_t12, preparedness_retention__husk_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement(prep_su_t18, preparedness_retention__husk_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(prep_su_t24, preparedness_retention__husk_reading, suppression_requirement, 24, 0.77).
narrative_ontology:measurement(prep_su_t30, preparedness_retention__husk_reading, suppression_requirement, 30, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_retention__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_retention__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, emergency_response_capacity).
narrative_ontology:affects_constraint(preparedness_retention__husk_reading, public_trust_in_institutions).

% DUAL FORMULATION NOTE:
% This is the husk_reading of the preparedness_retention kernel. The competence_reading and hybrid_reading are sibling constraints. The kernel decomposes because the label 'preparedness' conflates a genuine coordination function (specialized institutions) with a ceremonial extraction layer (broad societal drills). Each reading authors its own ε and stakeholder map.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_retention__husk_reading, institutional, 0.1).
constraint_indexing:directionality_override(preparedness_retention__husk_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
