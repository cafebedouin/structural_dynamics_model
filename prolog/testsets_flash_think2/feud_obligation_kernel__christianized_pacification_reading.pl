% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification of Blood Feud Obligations
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the
 *   'christianized_pacification_reading' of the 'feud_obligation_kernel'. It
 *   describes the efforts by the medieval Church and emerging royal
 *   authorities to suppress traditional blood-feud obligations by framing
 *   them as violations of divine law and centralizing the authority for
 *   legitimate violence. This reading emphasizes the imposition of a new,
 *   divinely sanctioned order over existing customary practices, leading to
 *   significant extraction of local autonomy and power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.8).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.9).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification of Blood Feud Obligations").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4').
narrative_ontology:cs_kernel_codification('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', formalized).
narrative_ontology:cs_authority_grounding('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', lineage).
narrative_ontology:cs_interpretation_layer_present('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4').
narrative_ontology:cs_reading_relation('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', foundational, divine_prohibition_on_vengeance).
narrative_ontology:cs_axiom_status(divine_prohibition_on_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', divine_prohibition_on_vengeance, theological).
narrative_ontology:cs_axiom('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', foundational, monopoly_on_legitimate_violence).
narrative_ontology:cs_axiom_status(monopoly_on_legitimate_violence, holdable).
narrative_ontology:cs_axiom_grounding('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', monopoly_on_legitimate_violence, conventional).
narrative_ontology:cs_reference_frame('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', divine_peace_and_order).
narrative_ontology:cs_drift_state('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', late_medieval_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('54fdfd3a-8dc4-49b4-ad6d-0a7f37825ce4', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_families).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces divine law prohibiting vengeance through penitential discipline, excommunication, and moral suasion. Benefits from an expanded interpretive monopoly on legitimate violence and increased jurisdictional reach over social conflicts, consolidating its spiritual and temporal power.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Collaborates with the Church to centralize the right to legitimate violence, replacing private feuds with royal justice and courts. Benefits from increased control over its territory, reduced internal conflict, and the expansion of secular law, strengthening its nascent state-building efforts.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Bear spiritual penalties (excommunication, damnation) and temporal penalties (fines, imprisonment, loss of land) for continuing blood feuds. They lose their traditional means of seeking justice and maintaining honor, often feeling trapped between customary obligations and the overwhelming power of Church and Crown.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feuding_families, payer,
    powerless, generational, identity_locked, local).

% Caught between traditional kin-based obligations to participate in feuds and the increasing pressure from Church and Crown to abandon them. They experience both the benefits of reduced violence and the loss of local autonomy and customary forms of justice, often facing social disruption as old systems are dismantled.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_communities, payer,
    moderate, generational, constrained, local).

% Represent older, pre-Christian systems of justice and social order that are actively suppressed and delegitimized by the Christianized pacification efforts. Their voices are systematically excluded from the discourse on legitimate violence, and their practices are condemned as barbaric or heretical.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, pagan_tradition_keepers, excluded,
    powerless, generational, identity_locked, local).

% Analyze the historical dynamics of blood feuds and their suppression, examining the motivations and impacts of the Christianized pacification efforts from a detached, scholarly perspective. They evaluate primary sources and archaeological evidence to reconstruct the structural shifts in medieval society.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, analytical_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a unified, divinely sanctioned framework for justice and peace, preventing endless cycles of private vengeance and promoting social order under ecclesiastical and royal authority, thereby reducing social instability and consolidating power.
% TRANSFER_FUNCTION: Transfers the right to legitimate violence and dispute resolution from kin groups and customary law to ecclesiastical and royal institutions; transfers spiritual peril and temporal penalties to those who continue feuding.
% ABSENT_VOICES: Traditional kin-group elders and pagan religious leaders, who would argue for the legitimacy and necessity of customary feuding as a self-regulating system of justice and honor in the absence of centralized authority. Their perspectives are actively suppressed and demonized by the dominant Christian narrative.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification efforts vanished overnight, traditional feuding practices would likely re-emerge in areas where centralized authority was weak, leading to a fragmentation of justice systems, increased private violence, and a significant challenge to the legitimacy of both Church and Crown.
% FOUNDING_PROBLEM: Widespread private vengeance (blood feuds) leading to chronic social instability, economic disruption, and challenges to the emerging ecclesiastical and royal authority structures in early medieval Europe.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chronicles, royal decrees, and capitularies from the period attest to the pervasive problem of feuding and the efforts to suppress it. Modern historians (outside the benefiting parties) corroborate the social disruption caused by feuds but also highlight the role of pacification in centralizing power and suppressing alternative forms of social organization.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88 at interval end) because the constraint fundamentally redefines legitimate justice, stripping kin groups of their traditional rights and imposing spiritual and temporal penalties. Suppression is very high (0.94) as the Church and Crown actively enforce this new order through religious doctrine, legal sanctions, and military force, aiming for complete eradication of feuding. Theater ratio is moderate (0.49) as while genuine efforts for peace exist, a significant portion of the enforcement activity serves to expand institutional power and control, often through performative rituals of penitence and public pronouncements. Accessibility collapse is high (0.85) because the new legal and religious framework delegitimizes and criminalizes alternatives, making traditional feuding increasingly untenable. Resistance is also high (0.7) as feuding was a deeply ingrained social practice, leading to centuries of ongoing struggle and adaptation rather than immediate compliance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Church and royal authority, this constraint represents the establishment of divine order and peace, a necessary step for social stability and salvation. From the perspective of feuding families and local communities, it is an imposition that strips them of their customary rights, honor, and means of justice, often leading to spiritual peril and temporal punishment for adhering to ancestral traditions. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church hierarchy and royal authority are clear beneficiaries and agenda-setters, gaining immense spiritual and temporal jurisdiction, consolidating power, and establishing a new social order. Feuding families and local communities are the primary targets and payers, losing autonomy, facing severe penalties, and having their traditional identities challenged. Their exit options are severely constrained or identity-locked due to deep-seated cultural norms and the overwhelming power of the new authorities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to bring peace and divine order, which is a genuine coordination function. However, the analysis reveals that this coordination is deeply intertwined with the asymmetric extraction of power and jurisdictional control by the Church and Crown. The persistence of the constraint, even as feuds adapt, indicates that its function has shifted from pure pacification to maintaining institutional authority and control over violence. The high extractiveness and suppression suggest it operates as a Tangled Rope, where the coordination story serves to legitimize significant institutional gain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_legitimacy_ambiguity,
    'Is blood feuding a destructive cycle of violence, or a legitimate, self-regulating mechanism for justice and deterrence in the absence of centralized authority?',
    'Comparative anthropological studies of stateless societies, and historical analysis of the social functions of feuds prior to Christianization and state-building.',
    'If feuds are primarily a legitimate coordination mechanism, the Christianized pacification is a Snare that suppresses a functional alternative. If primarily destructive, it is a Tangled Rope with a stronger coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the inherent nature and social function of blood feuds.').

omega_variable(
    divine_law_naturalness,
    'Is the divine law prohibiting vengeance a natural, immutable constraint, or a constructed theological and legal framework serving the institutional power of the Church and Crown?',
    'Theological and philosophical analysis of the origins and interpretations of divine law, alongside historical analysis of its application in specific political contexts.',
    'If divine law is a constructed constraint, its ''naturalness'' claim is a cover for extraction, amplifying the Snare/Tangled Rope classification. If genuinely natural, the extraction is a consequence of human resistance to a Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_naturalness, conceptual, 'The naturalness vs. constructedness of divine law as a constraint.').

omega_variable(
    suppression_effectiveness_and_transformation,
    'How effective was the Christianized pacification in truly suppressing feuds, versus merely transforming them into other forms of conflict (e.g., duels, legal battles, political factionalism) or driving them underground?',
    'Detailed micro-historical studies of local communities and legal records, tracking the incidence and nature of violence over centuries, and analyzing the evolution of dispute resolution mechanisms.',
    'If feuds were merely transformed, the measured suppression is partly theatrical or incomplete, suggesting a higher theater_ratio and a more persistent underlying resistance, potentially shifting the classification towards Piton or a more entrenched Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_effectiveness_and_transformation, empirical, 'The true extent of feud suppression versus transformation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 800, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t800, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(feud_tr_t850, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 850, 0.25).
narrative_ontology:measurement(feud_tr_t900, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 900, 0.3).
narrative_ontology:measurement(feud_tr_t950, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 950, 0.35).
narrative_ontology:measurement(feud_tr_t1000, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(feud_tr_t1050, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1050, 0.42).
narrative_ontology:measurement(feud_tr_t1100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1100, 0.45).
narrative_ontology:measurement(feud_tr_t1150, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1150, 0.47).
narrative_ontology:measurement(feud_tr_t1200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 1200, 0.49).

% Extraction over time
narrative_ontology:measurement(feud_be_t800, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 800, 0.65).
narrative_ontology:measurement(feud_be_t850, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 850, 0.7).
narrative_ontology:measurement(feud_be_t900, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 900, 0.75).
narrative_ontology:measurement(feud_be_t950, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 950, 0.78).
narrative_ontology:measurement(feud_be_t1000, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1000, 0.8).
narrative_ontology:measurement(feud_be_t1050, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1050, 0.82).
narrative_ontology:measurement(feud_be_t1100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1100, 0.84).
narrative_ontology:measurement(feud_be_t1150, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1150, 0.86).
narrative_ontology:measurement(feud_be_t1200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 1200, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t800, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 800, 0.7).
narrative_ontology:measurement(feud_su_t850, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 850, 0.75).
narrative_ontology:measurement(feud_su_t900, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 900, 0.8).
narrative_ontology:measurement(feud_su_t950, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 950, 0.85).
narrative_ontology:measurement(feud_su_t1000, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1000, 0.88).
narrative_ontology:measurement(feud_su_t1050, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1050, 0.9).
narrative_ontology:measurement(feud_su_t1100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1100, 0.92).
narrative_ontology:measurement(feud_su_t1150, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1150, 0.93).
narrative_ontology:measurement(feud_su_t1200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 1200, 0.94).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
