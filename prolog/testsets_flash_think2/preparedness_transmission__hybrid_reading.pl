% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Preparedness Transmission: Stratified Competence (Hybrid Reading)
 *   domain: Disaster Risk Management / Institutional Memory / Civil Defense Systems
 *
 * SUMMARY:
 *   This constraint describes a stratified state of disaster preparedness:
 *   physical infrastructure competence (e.g., engineering standards,
 *   construction codes) remains high and functional, while civilian
 *   coordination knowledge and institutional memory for public engagement
 *   have decayed significantly. The system performs drills and maintains
 *   infrastructure, but the ability to effectively coordinate the civilian
 *   population during a crisis has atrophied. This is a 'hybrid reading' of
 *   the broader 'preparedness_transmission' kernel, acknowledging both
 *   functional and degraded components.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.55).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.45).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.7).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.7).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, piton).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Preparedness Transmission: Stratified Competence (Hybrid Reading)").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "Disaster Risk Management / Institutional Memory / Civil Defense Systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, 'a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6').
narrative_ontology:cs_kernel_codification('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', formalized).
narrative_ontology:cs_authority_grounding('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', lineage).
narrative_ontology:cs_interpretation_layer_present('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6').
narrative_ontology:cs_reading_relation('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', preparedness_transmission__husk_reading, coexists_with).
narrative_ontology:cs_reading_relation('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_axiom('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', foundational, stratified_competence_exists).
narrative_ontology:cs_axiom_status(stratified_competence_exists, holdable).
narrative_ontology:cs_axiom_grounding('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', stratified_competence_exists, empirically_contingent).
narrative_ontology:cs_axiom('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', foundational, civilian_coordination_knowledge_decayed).
narrative_ontology:cs_axiom_status(civilian_coordination_knowledge_decayed, holdable).
narrative_ontology:cs_axiom_grounding('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', civilian_coordination_knowledge_decayed, empirically_contingent).
narrative_ontology:cs_reference_frame('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', integrated_civil_defense_competence).
narrative_ontology:cs_drift_state('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', contemporary_post_disaster_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a2f8b525-e725-4e94-9cb7-ec0bdf6e8fe6', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, engineering_professionals).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_defense_agencies).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_population).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, local_emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, engineering_excellence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain high competence in physical infrastructure design and maintenance, ensuring structures withstand disasters. They benefit from clear standards, continuous professional development, and the continued funding of infrastructure projects.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, engineering_professionals, beneficiary,
    institutional, biographical, mobile, national).

% Responsible for overall disaster preparedness, including both physical infrastructure and civilian coordination. They effectively manage physical assets and engineering standards but struggle with effective civilian engagement, knowledge transmission, and adapting to evolving coordination needs.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Directly affected by disasters, they suffer from the decay in civilian coordination knowledge, leading to confusion, delayed response, and increased vulnerability during emergencies. They bear the diffuse costs of an ineffective coordination layer.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_population, payer,
    powerless, immediate, trapped, local).

% First on scene during disasters, they face increased challenges and risks due to the lack of civilian coordination and preparedness. They often have to manage disorganized populations and fill gaps left by decayed public knowledge, increasing their operational burden.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, local_emergency_responders, payer,
    organized, immediate, constrained, local).

% Study how institutional knowledge is preserved and lost, observing the stratification of preparedness transmission and its implications for disaster resilience. They provide critical analysis but have limited direct power to alter the system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, institutional_memory_experts, observer,
    analytical, generational, analytical, global).

% Advocate for improved civilian preparedness and coordination, but often find their concerns about decaying knowledge are not fully integrated into official planning or resource allocation. Their proposals for bottom-up coordination are frequently overlooked.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, public_safety_advocates, excluded,
    moderate, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_transmission__hybrid_reading, diffuse).
narrative_ontology:fixing_cost_class(preparedness_transmission__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To ensure effective response to disasters by maintaining robust physical infrastructure and, nominally, coordinating civilian action and knowledge transmission.
% TRANSFER_FUNCTION: Transfers safety and resilience from effective physical infrastructure to the public, but fails to transfer effective coordination knowledge to the civilian population, leaving them vulnerable. The cost of this failure is borne by the public and local responders.
% ABSENT_VOICES: Public safety advocates and community organizers who emphasize bottom-up civilian coordination are often marginalized in planning processes dominated by top-down infrastructure and response logistics. Their insights into community-level needs and capacities are not fully integrated.
% DISAPPEARANCE_RATIONALE: If the stratified preparedness system vanished, the immediate aftermath of a disaster would be even more chaotic due to the lack of any coordinated response. However, it would force a fundamental rethinking of how civilian coordination and institutional memory are managed, potentially leading to more effective, integrated systems in the long run.
% FOUNDING_PROBLEM: To protect populations from natural and man-made disasters through robust infrastructure and organized civil defense, ensuring a coordinated and effective societal response.
% FOUNDING_PROBLEM_CORROBORATION: Engineering and infrastructure experts attest the physical preparedness problem is live and well-managed. Institutional memory experts and public safety advocates attest that the civilian coordination problem is decaying or dead, with the current system failing to address it effectively; this is supported by post-disaster analyses and academic studies from outside the benefiting parties.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Piton because its primary function (comprehensive societal preparedness) has atrophied in the civilian coordination layer, yet the overall system persists due to inertia and the continued functionality of the physical infrastructure component. Extraction (0.55) is diffuse, representing the increased vulnerability and costs borne by the civilian population due to inadequate coordination. Suppression (0.45) reflects the passive neglect of civilian coordination alternatives and the difficulty of challenging established institutional priorities. Theater ratio (0.7) is high because many civilian preparedness drills become performative, lacking genuine knowledge transmission or effective coordination practice. The measurement series show a gradual increase in extraction and theatricality, and a hardening of suppression (reflecting the increasing difficulty of changing the system) over the 50-year interval, as civilian coordination knowledge decayed.
 *
 * PERSPECTIVAL GAP:
 *   Engineering professionals and civil defense agencies, focused on physical infrastructure, might perceive the system as a functional Rope or even a Mountain (for engineering standards). In contrast, the civilian population and local emergency responders experience the decayed coordination as a Piton, where the system's original promise of comprehensive safety has atrophied, leaving them exposed to diffuse costs and chaos during crises.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineering professionals and civil defense agencies are beneficiaries (d near 0.0) as their core functions are maintained and funded. The civilian population and local emergency responders are payers (d near 1.0) as they bear the costs of the decayed coordination. Public safety advocates are excluded, their concerns not fully integrated into the system's agenda.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits clear mandatrophy in its civilian coordination component: the original mandate for comprehensive preparedness has outlived its effective function in this area. The system continues to operate, but its output for civilian coordination is largely performative, with no concentrated beneficiary actively maintaining the decayed function, and no single party sufficiently harmed to force a systemic fix. The 'contested' status of the founding problem corroborates this: the problem of physical infrastructure is addressed, but the problem of civilian coordination is not, yet the system persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the decay in civilian coordination knowledge due to structural neglect (lack of funding/focus) or internalized (public apathy, belief that authorities will handle everything)?',
    'Post-disaster analysis of public behavior and institutional funding priorities: if public apathy persists after structural barriers are addressed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the civilian population carries the suppression with them, making effective coordination harder to re-establish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for civilian coordination decay.').

omega_variable(
    mandate_drift_stratification,
    'Is the original mandate for comprehensive preparedness still active and equally applied to both physical infrastructure and civilian coordination, or has it implicitly drifted to prioritize physical infrastructure?',
    'Analysis of budget allocations, policy documents, and institutional rhetoric over time: a clear shift in resource allocation and stated priorities towards physical infrastructure would confirm mandate drift.',
    'If mandate drift is confirmed, the ''piton'' classification for civilian coordination is strengthened, indicating a systemic re-prioritization rather than mere atrophy. If the mandate is still equally applied, the decay is a failure of execution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_stratification, conceptual, 'Whether the preparedness mandate has implicitly stratified over time.').

omega_variable(
    kernel_reading_validity,
    'Is this ''hybrid_reading'' the most accurate description of preparedness transmission, or is the system closer to a ''husk_reading'' (all performance, no substance) or ''competence_reading'' (fully functional)?',
    'Comprehensive, independent, multi-domain assessment of preparedness, including both physical infrastructure resilience and civilian coordination effectiveness during large-scale drills or actual events.',
    'If the ''husk_reading'' is validated, the overall extractiveness and theater ratio would be higher, and the system would be a more severe Piton. If the ''competence_reading'' is validated, the system would be closer to a Rope or Mountain, with lower extraction and theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_validity, empirical, 'Assessing the overall functional status of preparedness transmission across all components.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1970, preparedness_transmission__hybrid_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(prep_tr_t1980, preparedness_transmission__hybrid_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__hybrid_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement(prep_tr_t2000, preparedness_transmission__hybrid_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(prep_tr_t2010, preparedness_transmission__hybrid_reading, theater_ratio, 2010, 0.6).
narrative_ontology:measurement(prep_tr_t2020, preparedness_transmission__hybrid_reading, theater_ratio, 2020, 0.7).

% Extraction over time
narrative_ontology:measurement(prep_be_t1970, preparedness_transmission__hybrid_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(prep_be_t1980, preparedness_transmission__hybrid_reading, base_extractiveness, 1980, 0.35).
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__hybrid_reading, base_extractiveness, 1990, 0.45).
narrative_ontology:measurement(prep_be_t2000, preparedness_transmission__hybrid_reading, base_extractiveness, 2000, 0.5).
narrative_ontology:measurement(prep_be_t2010, preparedness_transmission__hybrid_reading, base_extractiveness, 2010, 0.52).
narrative_ontology:measurement(prep_be_t2020, preparedness_transmission__hybrid_reading, base_extractiveness, 2020, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1970, preparedness_transmission__hybrid_reading, suppression_requirement, 1970, 0.2).
narrative_ontology:measurement(prep_su_t1980, preparedness_transmission__hybrid_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__hybrid_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(prep_su_t2000, preparedness_transmission__hybrid_reading, suppression_requirement, 2000, 0.35).
narrative_ontology:measurement(prep_su_t2010, preparedness_transmission__hybrid_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(prep_su_t2020, preparedness_transmission__hybrid_reading, suppression_requirement, 2020, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, disaster_response_funding_allocation).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, public_trust_in_institutions).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('hybrid_reading') of the 'preparedness_transmission' kernel, which also includes 'husk_reading' and 'competence_reading'. Each reading offers a distinct structural interpretation of the same underlying phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
