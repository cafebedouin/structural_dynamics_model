% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__prophetic_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__prophetic_override_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__prophetic_override_reading
 *   human_readable: Eternal Marriage Covenant (Prophetic Override Reading)
 *   domain: religious_law/political_theology/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint describes the 'prophetic override' reading of the eternal
 *   marriage covenant, specifically as it was applied to the practice of
 *   plural marriage in the late 19th and early 20th centuries. Under this
 *   reading, the doctrine of continuing revelation allows a living prophet to
 *   issue new revelations that supersede prior commandments, particularly
 *   when external circumstances (like federal pressure) threaten the church's
 *   existence. This mechanism enabled the church to adapt its practices to
 *   ensure institutional survival, but at a significant cost to members who
 *   had committed to the superseded practices.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, 0.65).
domain_priors:suppression_score(eternal_marriage_covenant__prophetic_override_reading, 0.7).
domain_priors:theater_ratio(eternal_marriage_covenant__prophetic_override_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(eternal_marriage_covenant__prophetic_override_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__prophetic_override_reading, tangled_rope).
narrative_ontology:human_readable(eternal_marriage_covenant__prophetic_override_reading, "Eternal Marriage Covenant (Prophetic Override Reading)").
narrative_ontology:topic_domain(eternal_marriage_covenant__prophetic_override_reading, "religious_law/political_theology/commitment_system_dynamics").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__prophetic_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__prophetic_override_reading, '9ea85fdc-6262-4e08-ae6d-1500494c8bf1').
narrative_ontology:cs_kernel_codification('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', formalized).
narrative_ontology:cs_authority_grounding('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', lineage).
narrative_ontology:cs_interpretation_layer_present('9ea85fdc-6262-4e08-ae6d-1500494c8bf1').
narrative_ontology:cs_reading_relation('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', eternal_marriage_covenant__immutable_commandment_reading, forecloses).
narrative_ontology:cs_reading_relation('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', eternal_marriage_covenant__temporal_accommodation_reading, influences).
narrative_ontology:cs_axiom('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', foundational, living_prophet_supersedes_prior_revelation).
narrative_ontology:cs_axiom_status(living_prophet_supersedes_prior_revelation, holdable).
narrative_ontology:cs_axiom_grounding('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', living_prophet_supersedes_prior_revelation, theological).
narrative_ontology:cs_axiom('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', secondary, institutional_survival_is_divine_mandate).
narrative_ontology:cs_axiom_status(institutional_survival_is_divine_mandate, holdable).
narrative_ontology:cs_axiom_grounding('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', institutional_survival_is_divine_mandate, theological).
narrative_ontology:cs_reference_frame('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', continuing_revelation_as_adaptive_mechanism).
narrative_ontology:cs_drift_state('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', post_manifesto_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9ea85fdc-6262-4e08-ae6d-1500494c8bf1', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_leadership).
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__prophetic_override_reading, church_institution).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, members_adhering_to_prior_revelation).
narrative_ontology:constraint_victim(eternal_marriage_covenant__prophetic_override_reading, dissenting_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the authority to receive and declare new revelation, which can supersede prior commandments. This power allows them to adapt the church's practices to external pressures, ensuring institutional survival and legal compliance, while maintaining a claim to divine guidance. They benefit from the flexibility this doctrine provides for institutional continuity.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Benefits from the ability to adapt its practices to avoid existential threats (e.g., legal persecution, loss of property) while preserving its core theological claims. The doctrine of continuing revelation provides the mechanism for this adaptation, ensuring the church's long-term survival and growth.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, church_institution, beneficiary,
    institutional, civilizational, constrained, global).

% Are required to abandon practices or beliefs previously understood as eternal commandments, leading to personal and communal disruption. Their identity is deeply tied to the church's teachings, making exit extremely costly. They bear the cognitive and social costs of doctrinal shifts.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, members_adhering_to_prior_revelation, payer,
    powerless, biographical, identity_locked, local).

% Are marginalized or excommunicated for refusing to accept new revelations or for continuing to adhere to superseded practices. They are excluded from the community and its benefits, facing severe social and spiritual penalties for non-compliance.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, dissenting_members, excluded,
    powerless, biographical, constrained, local).

% Exerts external pressure (e.g., legal threats, property confiscation) that acts as a catalyst for the prophetic override. While not directly part of the internal religious constraint, its actions are a key driver of the 'circumstances require' clause in the doctrine.
narrative_ontology:constraint_stakeholder(eternal_marriage_covenant__prophetic_override_reading, federal_government, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the church to maintain institutional coherence and legal standing in the face of changing external societal and legal norms, by providing a mechanism for doctrinal and practical adaptation under divine authority.
% TRANSFER_FUNCTION: Transfers the burden of doctrinal inconsistency and adaptation from the institutional leadership to individual members, who must reconcile new revelations with prior understandings and personal commitments. It also transfers authority from past prophets to the living prophet.
% ABSENT_VOICES: Members who believe in the immutability of prior divine commands, particularly those who suffered for adhering to them, are effectively silenced or excommunicated. Their perspective, emphasizing the eternal nature of specific commandments, is not accommodated within the framework of continuing revelation when it leads to a superseding revelation.
% DISAPPEARANCE_RATIONALE: If the doctrine of prophetic override vanished, the church would face an immediate crisis of legitimacy and institutional survival when confronted with external pressures that contradict prior revelations. It would either fracture into immutable-doctrine factions or be forced to abandon its claim to divine guidance, fundamentally altering its structure and authority.
% FOUNDING_PROBLEM: The church faced existential threats (legal persecution, property confiscation, disenfranchisement) from the federal government due to its practice of plural marriage, which was based on prior divine revelation.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, government decrees, and contemporary academic analyses corroborate the severe federal pressure. While the specific problem of plural marriage is 'dead' in terms of active practice, the underlying mechanism of prophetic override remains 'live' as a tool for institutional adaptation, as attested by church historians and independent scholars.
narrative_ontology:disappearance_verdict(eternal_marriage_covenant__prophetic_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(eternal_marriage_covenant__prophetic_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eternal_marriage_covenant__prophetic_override_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eternal_marriage_covenant__prophetic_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eternal_marriage_covenant__prophetic_override_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__prophetic_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__prophetic_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because members were compelled to abandon deeply held, divinely sanctioned practices, incurring significant personal and social costs. Suppression (0.70) is also high, as dissent or continued adherence to prior revelation led to excommunication or marginalization. The theater ratio (0.20) is relatively low, as the prophetic pronouncements were genuinely believed to be divine revelation, even if their timing coincided with external pressures. The claimed type is 'tangled_rope' because it coordinates institutional survival while extracting from members through doctrinal shifts enforced by ecclesiastical authority.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of church leadership, this was a necessary 'rope' to navigate an existential threat, preserving the institution for future generations. From the perspective of members adhering to prior revelation, it was a 'snare' that demanded the abandonment of sacred covenants under duress, with severe personal consequences. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership and the institution itself are beneficiaries, as the prophetic override mechanism ensures their survival and continuity (low d). Members adhering to prior revelation and dissenting members are targets, bearing the costs of doctrinal shifts and enforcement (high d). The federal government acts as an external force, not directly benefiting from the internal religious constraint but driving its activation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Tangled Rope prevents mislabeling this as a pure Rope (ignoring the extraction from members) or a pure Snare (ignoring the genuine coordination function of institutional survival). The 'dead' status of the founding problem (plural marriage persecution) combined with the 'world_rearranges' disappearance verdict suggests that while the specific historical problem is resolved, the mechanism of prophetic override remains a powerful, potentially extractive, tool for future adaptations, preventing mandatrophy by allowing the mandate to be redefined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_institutional_survival,
    'To what extent was the ''new revelation'' a direct expression of divine will, and to what extent was it an instrumental response to institutional survival pressures?',
    'Analysis of internal church deliberations, prophetic journals, and external historical accounts to identify the balance of theological and pragmatic considerations leading to the revelation.',
    'If primarily an instrumental response, the extractiveness and suppression metrics would be re-evaluated as higher, reflecting a more human-driven, less divinely sanctioned coercion. If primarily divine will, the constraint''s legitimacy (from the perspective of believers) would be higher, potentially lowering perceived extraction for some.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_survival, conceptual, 'Ambiguity between divine command and institutional pragmatism in prophetic revelation.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the suppression experienced by dissenting members primarily structural (excommunication, social ostracization) or internalized (deeply held belief in prophetic authority, fear of spiritual consequences)?',
    'Post-exit surveys and longitudinal studies of former members: if suppression persists as self-censorship or guilt after leaving the church, it indicates a significant internalized component.',
    'If internalized, the effective suppression is higher than the structural measure suggests, as the individual carries the suppression with them. This would amplify the ''snare'' aspect of the constraint for those individuals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Structural vs. internalized suppression mechanism for doctrinal non-compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__prophetic_override_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eter_tr_t1890, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1890, 0.1).
narrative_ontology:measurement(eter_tr_t1894, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1894, 0.15).
narrative_ontology:measurement(eter_tr_t1898, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1898, 0.18).
narrative_ontology:measurement(eter_tr_t1901, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1901, 0.19).
narrative_ontology:measurement(eter_tr_t1904, eternal_marriage_covenant__prophetic_override_reading, theater_ratio, 1904, 0.2).

% Extraction over time
narrative_ontology:measurement(eter_be_t1890, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1890, 0.55).
narrative_ontology:measurement(eter_be_t1894, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1894, 0.6).
narrative_ontology:measurement(eter_be_t1898, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1898, 0.63).
narrative_ontology:measurement(eter_be_t1901, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1901, 0.64).
narrative_ontology:measurement(eter_be_t1904, eternal_marriage_covenant__prophetic_override_reading, base_extractiveness, 1904, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(eter_su_t1890, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(eter_su_t1894, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1894, 0.65).
narrative_ontology:measurement(eter_su_t1898, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1898, 0.68).
narrative_ontology:measurement(eter_su_t1901, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1901, 0.69).
narrative_ontology:measurement(eter_su_t1904, eternal_marriage_covenant__prophetic_override_reading, suppression_requirement, 1904, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__prophetic_override_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__immutable_commandment_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__prophetic_override_reading, eternal_marriage_covenant__temporal_accommodation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'eternal_marriage_covenant' kernel. Its structural delta (federal pressure activates prophetic authority to supersede prior revelation) differentiates it from the 'immutable_commandment_reading' (polygamy as eternal law) and the 'temporal_accommodation_reading' (Manifesto suspends practice without renouncing doctrine). Each reading has distinct beneficiaries, victims, and extractiveness profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
