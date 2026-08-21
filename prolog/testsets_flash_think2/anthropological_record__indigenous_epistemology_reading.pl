% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Relational Continuity via Oral Tradition (Indigenous Epistemology)
 *   domain: epistemology/anthropology/cultural
 *
 * SUMMARY:
 *   This constraint story instantiates the 'indigenous_epistemology_reading'
 *   of the 'anthropological_record' kernel. From this perspective, the record
 *   of relational continuity with ancestors and place, knowable through
 *   sustained oral tradition, is a foundational truth. It asserts the
 *   sufficiency and primacy of oral tradition for this specific knowledge,
 *   subordinating material and scriptural frameworks. The constraint itself,
 *   as an internal epistemological framework, is not extractive from its
 *   adherents, hence low extractiveness. Its persistence is due to its
 *   inherent truth for the community, making it a Mountain, albeit one
 *   fiercely contested by external knowledge systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.1).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.2).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, mountain).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Relational Continuity via Oral Tradition (Indigenous Epistemology)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/anthropology/cultural").

domain_priors:emerges_naturally(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, '7e6f230f-7f52-4876-8d97-b2b811baa39f').
narrative_ontology:cs_kernel_codification('7e6f230f-7f52-4876-8d97-b2b811baa39f', implicit).
narrative_ontology:cs_authority_grounding('7e6f230f-7f52-4876-8d97-b2b811baa39f', practice).
narrative_ontology:cs_interpretation_layer_present('7e6f230f-7f52-4876-8d97-b2b811baa39f').
narrative_ontology:cs_reading_relation('7e6f230f-7f52-4876-8d97-b2b811baa39f', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('7e6f230f-7f52-4876-8d97-b2b811baa39f', anthropological_record__creationist_reading, forecloses).
narrative_ontology:cs_axiom('7e6f230f-7f52-4876-8d97-b2b811baa39f', foundational, oral_tradition_is_primary_epistemic_source).
narrative_ontology:cs_axiom_status(oral_tradition_is_primary_epistemic_source, holdable).
narrative_ontology:cs_axiom_grounding('7e6f230f-7f52-4876-8d97-b2b811baa39f', oral_tradition_is_primary_epistemic_source, conventional).
narrative_ontology:cs_axiom('7e6f230f-7f52-4876-8d97-b2b811baa39f', foundational, ancestral_relationality_is_knowable).
narrative_ontology:cs_axiom_status(ancestral_relationality_is_knowable, holdable).
narrative_ontology:cs_axiom_grounding('7e6f230f-7f52-4876-8d97-b2b811baa39f', ancestral_relationality_is_knowable, deontological).
narrative_ontology:cs_reference_frame('7e6f230f-7f52-4876-8d97-b2b811baa39f', ancestral_continuity_through_oral_tradition).
narrative_ontology:cs_drift_state('7e6f230f-7f52-4876-8d97-b2b811baa39f', contemporary_post_colonial_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7e6f230f-7f52-4876-8d97-b2b811baa39f', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, elders_knowledge_keepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, anthropologists_sympathetic).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, naturalist_scientists).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, creationist_theologians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their identity, cultural continuity, and understanding of their relationship to land and ancestors are fundamentally grounded in this knowledge system. They benefit from its validation and perpetuation.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, indigenous_communities, beneficiary,
    organized, generational, identity_locked, global).

% They are the primary custodians, interpreters, and transmitters of the oral traditions. Their authority within the community is derived from this role, and they actively work to sustain the knowledge system.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, elders_knowledge_keepers, agenda_setter,
    powerful, generational, identity_locked, local).

% Academics who recognize and advocate for the validity of indigenous epistemologies, benefiting from the expanded understanding of knowledge systems and often collaborating with communities. They gain intellectual insight and credibility from this recognition.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, anthropologists_sympathetic, beneficiary,
    moderate, biographical, constrained, global).

% Adherents to a materialist scientific method that often dismisses oral tradition as 'anecdotal' or 'mythological'. This reading challenges their epistemic primacy, requiring them to either subordinate their framework for certain knowledge or face intellectual incoherence in this context.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, naturalist_scientists, payer,
    institutional, biographical, constrained, global).

% Adherents to scriptural interpretations of origins. This reading challenges their claims of universal truth and historical authority, requiring them to either subordinate their framework or dismiss indigenous narratives as incompatible.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, creationist_theologians, payer,
    institutional, generational, constrained, global).

% Historical and contemporary institutions (e.g., legal systems, educational bodies) that have systematically suppressed or invalidated indigenous knowledge. This reading directly challenges their foundational assumptions and authority over land and history.
narrative_ontology:constraint_stakeholder(anthropological_record__indigenous_epistemology_reading, colonial_institutions, excluded,
    institutional, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:fixing_cost_class(anthropological_record__indigenous_epistemology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the transmission and interpretation of ancestral knowledge, historical narratives, and land-based relationships, ensuring cultural continuity and collective identity for indigenous communities.
% TRANSFER_FUNCTION: Transfers intergenerational knowledge, cultural authority, and a sense of belonging from ancestors and place to present and future generations, reinforcing community bonds and responsibilities.
% ABSENT_VOICES: Mainstream scientific and religious institutions, whose epistemologies are often privileged in dominant societies, are structurally excluded from the internal validation and interpretation of this knowledge system. They would typically challenge its epistemic status.
% DISAPPEARANCE_RATIONALE: If the belief in relational continuity knowable via oral tradition vanished, indigenous communities would lose their foundational understanding of their history, identity, and connection to their ancestral lands, leading to profound cultural and social disintegration.
% FOUNDING_PROBLEM: The fundamental need to preserve, transmit, and validate the unique knowledge, history, and identity of indigenous peoples across generations, especially in contexts where written records are absent, insufficient, or imposed by external powers.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous scholars, community elders, and international human rights organizations consistently corroborate the ongoing vitality and necessity of oral traditions for cultural survival and self-determination, providing evidence from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(anthropological_record__indigenous_epistemology_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__indigenous_epistemology_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__indigenous_epistemology_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(anthropological_record__indigenous_epistemology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__indigenous_epistemology_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, ExtMetricName, E),
    domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(anthropological_record__indigenous_epistemology_reading),
    narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.1) and theater ratio (0.1) reflect that, from the perspective of this reading, the knowledge system is a genuine, non-extractive truth for its community. Accessibility collapse is high (0.85) because oral tradition is deemed essential for accessing this specific type of relational knowledge. Resistance (0.15) is low internally, as the community largely accepts this framework. Suppression (0.2) is low for the internal operation of the constraint, but the decreasing 'suppression_requirement' over the interval (from 0.4 to 0.2) reflects the growing global recognition and self-assertion of indigenous epistemologies, reducing the external pressure required to maintain the system against dominant narratives.
 *
 * PERSPECTIVAL GAP:
 *   The 'mountain' classification reflects the internal, foundational truth of this epistemology for its adherents. However, external perspectives (e.g., naturalist or creationist readings) would classify the *assertion* of this constraint as a 'snare' or 'tangled_rope' due to its perceived 'suppression' of their own epistemic claims or its 'extraction' of intellectual authority. The engine's per-seat classification will highlight this divergence based on the declared stakeholder roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous communities and elders are beneficiaries and agenda-setters, as the constraint validates and empowers their knowledge system. Sympathetic anthropologists also benefit from expanded intellectual frameworks. Naturalist scientists and creationist theologians are 'payers' in an intellectual sense, as their frameworks are challenged or deemed insufficient by this reading, requiring them to adjust or dismiss. Colonial institutions are 'excluded' as their historical invalidation of indigenous knowledge is directly challenged by this reading's assertion of authority.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_primacy_contest,
    'Is the indigenous epistemology''s claim of ''material evidence insufficient without oral tradition'' a universal epistemic truth, or a culturally specific assertion of authority?',
    'Cross-cultural comparative studies of knowledge systems that explore the limits of materialist empiricism for certain types of relational knowledge, alongside ongoing dialogue between indigenous and Western scholars.',
    'If universally valid, it strengthens the ''mountain'' classification and mandates a re-evaluation of dominant epistemologies. If culturally specific, it remains a ''mountain'' for its adherents but its ''forecloses'' relation to other readings might be re-evaluated as ''coexists_with'' in a pluralistic framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_primacy_contest, conceptual, 'Ambiguity regarding the universal vs. culturally specific nature of epistemic primacy.').

omega_variable(
    natural_law_vs_cultural_construct,
    'Is the ''relational continuity with ancestors and place'' an emergent natural law for indigenous communities, or a deeply embedded cultural construct?',
    'Longitudinal studies of indigenous communities'' relationship with land and history, combined with philosophical analysis of ''naturalness'' in cultural contexts. The distinction may be more conceptual than empirical.',
    'If more ''natural law'', it reinforces the ''mountain'' classification. If more ''cultural construct'', it might shift towards a ''rope'' or ''tangled_rope'' if its persistence is seen as requiring active, though internal, coordination and defense.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_construct, conceptual, 'Ambiguity of natural law vs. cultural construct for indigenous relationality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t1950, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(anth_tr_t1970, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anth_tr_t1990, anthropological_record__indigenous_epistemology_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(anth_tr_t2010, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(anth_tr_t2024, anthropological_record__indigenous_epistemology_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(anth_be_t1950, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1950, 0.1).
narrative_ontology:measurement(anth_be_t1970, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1970, 0.1).
narrative_ontology:measurement(anth_be_t1990, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 1990, 0.1).
narrative_ontology:measurement(anth_be_t2010, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2010, 0.1).
narrative_ontology:measurement(anth_be_t2024, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t1950, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1950, 0.4).
narrative_ontology:measurement(anth_su_t1970, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(anth_su_t1990, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(anth_su_t2010, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2010, 0.25).
narrative_ontology:measurement(anth_su_t2024, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
