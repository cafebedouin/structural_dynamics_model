% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status: Messianic Deferral Reading
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint describes the 'messianic deferral' reading of the
 *   Kodashim (sacrificial laws) commandment status within Jewish Halakhic
 *   theory. Following the destruction of the Second Temple, the performance
 *   of these commandments became impossible. This reading asserts that while
 *   performance is suspended, the commandments are not obsolete; rather,
 *   their study and theoretical maintenance are crucial for readiness for a
 *   future messianic restoration of the Temple. This reading frames the
 *   extensive scholarly engagement with these laws as a vital, active form of
 *   religious commitment, even in the absence of physical performance. The
 *   constraint is claimed as a Rope by its adherents, but its metrics reflect
 *   a Tangled Rope due to the opportunity costs and resource allocation it
 *   demands from the community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.45).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.6).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.45).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status: Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious_studies/halakhic_theory").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '9be75b39-1613-4fea-99cf-9c6791679b41').
narrative_ontology:cs_kernel_codification('9be75b39-1613-4fea-99cf-9c6791679b41', fixed_text).
narrative_ontology:cs_authority_grounding('9be75b39-1613-4fea-99cf-9c6791679b41', lineage).
narrative_ontology:cs_interpretation_layer_present('9be75b39-1613-4fea-99cf-9c6791679b41').
narrative_ontology:cs_reading_relation('9be75b39-1613-4fea-99cf-9c6791679b41', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('9be75b39-1613-4fea-99cf-9c6791679b41', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('9be75b39-1613-4fea-99cf-9c6791679b41', foundational, commandment_temporally_suspended_not_obsolete).
narrative_ontology:cs_axiom_status(commandment_temporally_suspended_not_obsolete, holdable).
narrative_ontology:cs_axiom_grounding('9be75b39-1613-4fea-99cf-9c6791679b41', commandment_temporally_suspended_not_obsolete, deontological).
narrative_ontology:cs_axiom('9be75b39-1613-4fea-99cf-9c6791679b41', foundational, study_maintains_readiness_for_restoration).
narrative_ontology:cs_axiom_status(study_maintains_readiness_for_restoration, holdable).
narrative_ontology:cs_axiom_grounding('9be75b39-1613-4fea-99cf-9c6791679b41', study_maintains_readiness_for_restoration, theological).
narrative_ontology:cs_reference_frame('9be75b39-1613-4fea-99cf-9c6791679b41', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('9be75b39-1613-4fea-99cf-9c6791679b41', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9be75b39-1613-4fea-99cf-9c6791679b41', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_movement_adherents).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_needs).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, community_resources).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, halakhic_continuity).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_redemption_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the Halakha, including the Kodashim laws. Their intellectual work maintains the 'readiness' for future restoration, solidifying their role as custodians of tradition and justifying the deferral. They benefit from the intellectual engagement and the perpetuation of their scholarly authority.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% Derive meaning and purpose from the belief in the future restoration of the Temple and its sacrificial service. The deferral reading provides a framework for their eschatological hopes and justifies their present-day commitment to study and preparation. They are sustained by the narrative of future fulfillment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, messianic_movement_adherents, beneficiary,
    organized, civilizational, identity_locked, global).

% Represent the immediate, tangible needs of the community (e.g., social welfare, economic development) that might be deprioritized or under-resourced due to the emphasis on abstract study and future-oriented preparation. They bear the opportunity cost of resources and attention diverted to the deferred commandment.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_needs, payer,
    powerless, immediate, trapped, local).

% Financial and human capital within the community that is allocated towards maintaining scholarly institutions, publishing texts, and supporting individuals dedicated to the study of Kodashim, rather than being directed to other communal priorities. This represents the material cost of the deferral.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, community_resources, payer,
    powerless, biographical, constrained, local).

% Observe and critique the allocation of resources and intellectual effort towards a commandment that cannot be performed. They question the practical utility and opportunity costs, often from a utilitarian or humanist perspective, but have no direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, secular_critics, observer,
    moderate, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective religious life and intellectual activity of a community around a shared future vision, ensuring the preservation of complex ritual knowledge across generations despite its present inapplicability.
% TRANSFER_FUNCTION: Transfers intellectual and material resources (scholarly effort, communal funds) from immediate communal needs and alternative religious expressions towards the maintenance of a deferred ritual system and the authority of its interpreters.
% ABSENT_VOICES: Those who prioritize immediate social justice, economic development, or alternative forms of spiritual engagement would object to the extensive allocation of resources and intellectual focus on a presently unperformable commandment. Their voices are often marginalized by the dominant religious discourse.
% DISAPPEARANCE_RATIONALE: If the messianic deferral reading vanished, the entire structure of rabbinic authority, scholarly institutions, and messianic movements would be fundamentally challenged. Resources and intellectual energy would be reallocated, and the community's collective identity and future orientation would undergo a profound reordering.
% FOUNDING_PROBLEM: The destruction of the Temple left a core set of commandments (Kodashim, sacrificial laws) unperformable, creating a crisis of religious continuity and meaning for a community whose covenantal life was centered on the Temple.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic tradition and historical texts attest to the profound crisis caused by the Temple's destruction. While secular critics might question the 'problem' itself, the historical and theological sources within the tradition consistently corroborate the foundational challenge of maintaining covenantal life without the Temple.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) stems from the opportunity cost: resources (intellectual, financial, communal attention) are diverted to the study of unperformable laws, rather than immediate communal needs. Suppression (0.6) is moderate, arising from the strong social and religious pressure to conform to rabbinic authority and the messianic narrative, which discourages questioning the value of this deferral. Theater ratio (0.1) is low because the study is genuinely rigorous and believed to be functional for future readiness, not merely performative. Accessibility collapse (0.7) is high because for adherents, the alternative of simply abandoning these laws is largely unthinkable within the framework of Halakhic continuity. Resistance (0.15) is low, as overt resistance to this established religious framework is rare, though internal questioning and quiet re-prioritization of resources may occur.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rabbinic scholars and messianic adherents, this constraint is a vital Rope, ensuring the continuity of tradition and preparing for redemption. From the perspective of those focused on present-generation needs or secular critics, it appears as a Tangled Rope, extracting resources and attention for a deferred, abstract goal, with the coordination function serving to legitimize this extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars and messianic movement adherents are beneficiaries, as their authority, purpose, and identity are deeply intertwined with this reading. Present-generation needs and community resources are payers, bearing the opportunity costs and direct allocation of resources. Secular critics are observers, analyzing the dynamics without direct participation or enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by actively re-framing the 'suspension' as 'deferral with active preparation.' The mandate (to observe Kodashim) is not obsolete but transformed into a mandate for study and readiness. This active re-interpretation prevents the constraint from becoming a Piton, as there are clear beneficiaries (scholars, messianic adherents) who actively maintain its function, albeit a re-interpreted one. The 'founding problem status' being 'live' (though contested) further reinforces this, preventing a clear mandatrophy verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opportunity_cost_quantification,
    'What is the quantifiable opportunity cost of resources (intellectual, financial, human capital) allocated to the study of Kodashim versus alternative communal needs?',
    'Detailed economic and sociological studies within religious communities, comparing resource allocation patterns and their impact on various communal sectors.',
    'A high quantifiable opportunity cost would strengthen the ''extraction'' component of the constraint, potentially shifting its classification closer to a Snare from the perspective of the ''present_generation_needs'' seat. A low cost would support the ''coordination'' aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opportunity_cost_quantification, empirical, 'Quantifying the real-world impact of resource allocation choices.').

omega_variable(
    messianic_deferral_vs_performance_only,
    'Is the ''messianic deferral'' reading genuinely distinct from a ''performance only'' reading, or is the ''study for readiness'' merely a sophisticated form of ''husk'' maintenance?',
    'Theological and philosophical analysis of the internal coherence and practical implications of each reading, particularly regarding the nature of ''readiness'' and ''fulfillment'' in the absence of performance.',
    'If the ''study for readiness'' is deemed functionally equivalent to ''husk'' maintenance, the extractiveness might be re-evaluated as higher (more theater, less genuine coordination), potentially pushing the classification towards a Piton or Snare, as the coordination story becomes weaker.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_deferral_vs_performance_only, conceptual, 'Distinguishing active deferral from passive suspension.').

omega_variable(
    identity_lock_strength,
    'How strong is the ''identity_locked'' exit option for rabbinic scholars and messianic adherents? Would a shift away from this reading fundamentally undermine their professional or spiritual identity?',
    'Sociological studies of religious identity formation, interviews with individuals who have shifted their views on these matters, and analysis of institutional responses to such shifts.',
    'If the identity lock is extremely strong, it amplifies the effective suppression and extractiveness for these groups, even if they are beneficiaries, as their ''benefit'' is tied to a system with high internal costs for dissent. If weaker, it suggests more agency and less structural coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_strength, empirical, 'Assessing the binding force of identity on adherence to the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__messianic_deferral, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t400, kodashim_commandment_status__messianic_deferral, theater_ratio, 400, 0.07).
narrative_ontology:measurement(koda_tr_t800, kodashim_commandment_status__messianic_deferral, theater_ratio, 800, 0.08).
narrative_ontology:measurement(koda_tr_t1200, kodashim_commandment_status__messianic_deferral, theater_ratio, 1200, 0.09).
narrative_ontology:measurement(koda_tr_t1600, kodashim_commandment_status__messianic_deferral, theater_ratio, 1600, 0.095).
narrative_ontology:measurement(koda_tr_t1950, kodashim_commandment_status__messianic_deferral, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__messianic_deferral, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(koda_be_t400, kodashim_commandment_status__messianic_deferral, base_extractiveness, 400, 0.35).
narrative_ontology:measurement(koda_be_t800, kodashim_commandment_status__messianic_deferral, base_extractiveness, 800, 0.4).
narrative_ontology:measurement(koda_be_t1200, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1200, 0.42).
narrative_ontology:measurement(koda_be_t1600, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1600, 0.44).
narrative_ontology:measurement(koda_be_t1950, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1950, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__messianic_deferral, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(koda_su_t400, kodashim_commandment_status__messianic_deferral, suppression_requirement, 400, 0.52).
narrative_ontology:measurement(koda_su_t800, kodashim_commandment_status__messianic_deferral, suppression_requirement, 800, 0.55).
narrative_ontology:measurement(koda_su_t1200, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1200, 0.57).
narrative_ontology:measurement(koda_su_t1600, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1600, 0.59).
narrative_ontology:measurement(koda_su_t1950, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1950, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, halakhic_curriculum_design).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, communal_resource_allocation_priorities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
