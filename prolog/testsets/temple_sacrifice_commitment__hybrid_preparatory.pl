% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__hybrid_preparatory
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__hybrid_preparatory, []).

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
    narrative_ontology:measurement_basis/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: temple_sacrifice_commitment__hybrid_preparatory
 *   human_readable: Temple Sacrifice Commitment in Suspended Preparatory State
 *   domain: religious_law/halakhic_tradition
 *
 * SUMMARY:
 *   The halakhic commitment to perform Temple sacrifice became materially
 *   impossible after the Temple's destruction in 70 CE. The mainstream
 *   Orthodox Jewish reading maintains that the commitment remains eternally
 *   binding but is held in suspended preparatory state: study of the law is
 *   obligatory, resources must fund scholarship to preserve the tradition,
 *   and the obligation will be fully instantiated upon messianic restoration.
 *   This is neither mere archiving (which would suggest the commitment is
 *   dead) nor full occupation (which would require material performance). The
 *   hybrid preparatory reading is contested by three alternatives:
 *   study_as_exercise (study itself is performance), performance_only (study
 *   without performance is archival preservation, not occupation), and
 *   symbolic_transformation (the commitment has been authorized to transform
 *   into prayer and study as permanent instantiation). This story authors the
 *   hybrid preparatory reading as ONE constraint in a constraint family; its
 *   ε-invariance and classification hold only under this reading. Sibling
 *   readings (study_as_exercise, performance_only, symbolic_transformation)
 *   are separate constraint stories linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - halakhic_scholarship_community: Institutional agenda-setters who maintain sacrifice law study and derive authority from mastery of this domain; they control the interpretation of obligation and resource justification.
 *   - study_resource_contributors: Moderate-power payers who fund yeshivas and scholarship under the understanding that they are fulfilling a binding obligation; their exit options are constrained by identity-lock and community standing.
 *   - non_beneficiary_community_members: Powerless payers who bear the opportunity cost of resource redirection but do not benefit from scholarship authority; they are structurally unheard in the obligation discussion.
 *   - messianic_tradition_guardians: Institutional beneficiaries whose authority narrative depends on maintaining the framework that the commitment is binding and suspended, not abandoned or transformed.
 *   - performance_only_advocates (excluded): A dissenting interpretive tradition that holds study is archival preservation, not occupation.
 *   - study_as_exercise_advocates (excluded): A competing reading that holds study IS performance of the divine command.
 *   - symbolic_transformation_advocates (excluded): A fourth reading that holds the commitment has been authorized to transform permanently into prayer and study.
 *   - analytical_observer: External scholarly seat examining the structural dynamics of commitment under material impossibility.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, 0.58).
domain_priors:suppression_score(temple_sacrifice_commitment__hybrid_preparatory, 0.42).
domain_priors:theater_ratio(temple_sacrifice_commitment__hybrid_preparatory, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, extractiveness, 0.58).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__hybrid_preparatory, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__hybrid_preparatory, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__hybrid_preparatory, "Temple Sacrifice Commitment in Suspended Preparatory State").
narrative_ontology:topic_domain(temple_sacrifice_commitment__hybrid_preparatory, "religious_law/halakhic_tradition").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__hybrid_preparatory).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__hybrid_preparatory, 'a031d796-0edd-4745-b2ff-58e209676996').
narrative_ontology:cs_kernel_codification('a031d796-0edd-4745-b2ff-58e209676996', fixed_text).
narrative_ontology:cs_authority_grounding('a031d796-0edd-4745-b2ff-58e209676996', lineage).
narrative_ontology:cs_interpretation_layer_present('a031d796-0edd-4745-b2ff-58e209676996').
narrative_ontology:cs_reading_relation('a031d796-0edd-4745-b2ff-58e209676996', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('a031d796-0edd-4745-b2ff-58e209676996', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('a031d796-0edd-4745-b2ff-58e209676996', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('a031d796-0edd-4745-b2ff-58e209676996', foundational, suspension_is_temporary_not_permanent).
narrative_ontology:cs_axiom_status(suspension_is_temporary_not_permanent, holdable).
narrative_ontology:cs_axiom_grounding('a031d796-0edd-4745-b2ff-58e209676996', suspension_is_temporary_not_permanent, deontological).
narrative_ontology:cs_axiom('a031d796-0edd-4745-b2ff-58e209676996', foundational, study_maintains_commitment_preparatory_not_direct).
narrative_ontology:cs_axiom_status(study_maintains_commitment_preparatory_not_direct, holdable).
narrative_ontology:cs_axiom_grounding('a031d796-0edd-4745-b2ff-58e209676996', study_maintains_commitment_preparatory_not_direct, deontological).
narrative_ontology:cs_reference_frame('a031d796-0edd-4745-b2ff-58e209676996', temple_destroyed_commitment_suspended_pending_restoration).
narrative_ontology:cs_drift_state('a031d796-0edd-4745-b2ff-58e209676996', contemporary_messianic_timeline_contested, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a031d796-0edd-4745-b2ff-58e209676996', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholarship_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__hybrid_preparatory, messianic_tradition_guardians).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, study_resource_contributors).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__hybrid_preparatory, non_beneficiary_community_members).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, divine_command_perpetual_bindingness).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__hybrid_preparatory, messianic_restoration_premise).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and yeshiva educators maintain the apparatus of sacrifice law study: detailed legal analysis, hermeneutic frameworks, practical detail work on laws that cannot be performed. They justify the resource investment as fulfilling a divine obligation to occupy the commitment through study, even in suspension. Their institutional identity is bound to this interpretive tradition; scholarship authority and legitimacy derive from mastery of this domain. Resource flows to them through community funding and institutional hierarchy; they control which interpretations are taught and transmitted.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholarship_community, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholarship_community, beneficiary).

% Community members and donors fund yeshivas, scholarship positions, and study infrastructure dedicated to sacrifice law. They contribute because they accept the obligation to support the commitment's preservation, but the connection between their resource transfer and any measurable outcome remains uncertain — they fund for a future that may not arrive (messianic restoration) or may never require the law's actual performance. Exit from funding means withdrawing from community standing and rejecting a religious obligation; the identity-locked framework makes exit costly.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, study_resource_contributors, payer,
    moderate, biographical, constrained, global).

% Community members subject to the collective obligation structure who do not themselves benefit from scholarship authority or institutional position. They bear the opportunity cost of collective resources directed to sacrifice law study rather than alternative uses. They cannot exit the obligation structure without exiting the community entirely; their dissent is structurally unheard because the obligation is presented as non-negotiable divine command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, non_beneficiary_community_members, payer,
    powerless, biographical, trapped, global).

% Religious authorities and institutional custodians who maintain the interpretive claim that the commitment is held in abeyance for messianic restoration, not abandoned. This reading preserves the authority structure's core narrative — that the law is eternally binding and the Temple practice will be restored. They benefit by maintaining the framework that justifies their authority over interpretation and community life. Their exit from this reading would collapse the institutional narrative.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, messianic_tradition_guardians, beneficiary,
    institutional, civilizational, identity_locked, global).

% A smaller, dissenting interpretive tradition holds that study of sacrifice law without performance is archival preservation, not occupation of the commitment. They argue that resources directed to non-performable study should be redirected to living law or that the commitment should be acknowledged as suspended rather than actively maintained. They are intellectually and institutionally marginal; their objections are not part of the mainstream conversation about obligation structure.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, performance_only_advocates, excluded,
    moderate, biographical, constrained, regional).

% A competing reading holds that study of sacrifice law IS itself performance of the divine command; intellectual engagement with the text and legal system occupies the commitment in the absence of material conditions. This reading shifts the measure of obligation fulfillment entirely into the scholarly domain. They are present in some yeshiva communities but subordinate to the dominant hybrid preparatory reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, study_as_exercise_advocates, excluded,
    moderate, biographical, constrained, regional).

% A fourth reading holds that the sacrifice commitment has undergone authorized transformation — prayer and study are now the new instantiation, not substitutes for suspension. This reading dissolves the future-contingency by treating transformation as permanent and legitimate. They argue the commitment continues, not in suspension, but in transformed form. This reading is institutionally marginalized within Orthodox communities, though more prominent in other Jewish traditions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, symbolic_transformation_advocates, excluded,
    moderate, biographical, constrained, regional).

% External scholarly analysis of halakhic commitment systems: the structural relationship between law, obligation, performance, and institutional authority under conditions of material impossibility. Observes how the hybrid preparatory reading manages competing claims about obligation binding and resource justification.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__hybrid_preparatory, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__hybrid_preparatory, halakhic_scholarship_community).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__hybrid_preparatory, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the intellectual and hermeneutic apparatus of sacrifice law across generations; maintains the legal tradition's technical detail, interpretive frameworks, and argumentative depth so that if material conditions allow (messianic restoration), the law can be instantiated rather than reconstructed from archival fragments. Coordinates scholarly community effort around a shared commitment structure and transmits the tradition across generations with full mastery.
% TRANSFER_FUNCTION: Moves cognitive resources, study time, funding, and institutional authority from the broader community to the halakhic scholarship community and messianic tradition guardians, justified by the obligation to maintain the commitment in suspended state. The transfer is framed as fulfilling a divine command to preserve the law's bindingness.
% ABSENT_VOICES: Performance-only advocates argue study is archival preservation, not obligation-fulfilling occupation, and that resources should be redirected or the commitment acknowledged as dead. Study-as-exercise advocates argue study itself IS performance and question why additional resource justification is needed. Symbolic-transformation advocates argue the commitment has been permanently transformed into prayer and study, eliminating the suspension framework and the future-contingency resource justification. These three readings are intellectually present in halakhic discourse but institutionally marginalized relative to the hybrid preparatory mainstream.
% DISAPPEARANCE_RATIONALE: If the commitment to maintain sacrifice law in suspended state vanished, resource flows to yeshivas teaching sacrifice law would cease or redirect, institutional positions grounded in sacrifice law mastery would lose their primary authority basis, the messianic tradition narrative would collapse, and the interpretive tradition's claim to perpetual bindingness would reorganize around alternative readings (symbolic-transformation, study-as-exercise, or explicit abandonment). The structure of obligation would shift from 'study maintains commitment in suspension' to one of the sibling readings or to institutional reframing.
% FOUNDING_PROBLEM: The destruction of the Temple rendered sacrifice law materially non-performable. The religious obligation to occupy the commitment remained binding (under the mainstream halakhic reading). The founding problem was preserving the commitment's authority and transmission when its primary mode of fulfillment became impossible, and maintaining the framework for messianic restoration when that event remained uncertain and indefinite.
% FOUNDING_PROBLEM_CORROBORATION: The halakhic scholarship community and messianic tradition guardians attest the problem remains live — the Temple is not restored, the commitment remains binding, study is the obligatory form of occupation in suspension. Conservative, Reform, and Reconstructionist Jewish communities attest the problem has been resolved through authorized transformation or reinterpretation, not merely suspended. Some Orthodox dissenters (study-as-exercise and performance-only advocates) attest that the suspension framing obscures either a direct performance claim (study) or an archival reality (dead commitment). Historical testimony from Talmudic sources (Avot 1:17, Menachot 110a) and medieval responsa from Maimonides and others supports the suspension framing. Contemporary sociological analysis from outside the benefiting parties (academic Jewish studies, secular scholarship on religious institutions) documents the resource flows, institutional authority structure, and identity-lock mechanisms sustaining the constraint.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__hybrid_preparatory, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__hybrid_preparatory, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__hybrid_preparatory, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__hybrid_preparatory, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__hybrid_preparatory, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__hybrid_preparatory_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58) rather than low because the constraint extracts cognitive resources, study time, and community funding from the contributor base, justified by a future contingency (messianic restoration) that may never arrive or may not require the law's actual performance. The extraction is not pure — there is a genuine coordination function in preserving the intellectual tradition — but it is asymmetric: the halakhic scholarship community and messianic tradition guardians benefit from institutional authority and resource flows; the broader community bears opportunity costs and funding obligations. Suppression (0.42) is lower than extractiveness because the constraint relies more on identity-lock and collective obligation framing than on coercive force; dissenting readings are marginalized rather than violently suppressed. Theater ratio (0.48) is moderate, reflecting that while genuine scholarship occurs, a growing share of resource justification rhetoricizes the future contingency rather than defending it on immediate grounds. The measurement series show mild extractiveness accumulation over the interval (from 0.48 to 0.58), reflecting gradual shift of rhetoric from 'obligation binding under all conditions' to 'resources justified by preparation for restoration'; theater ratio rises correspondingly (0.35 to 0.48), indicating increasing performative work to maintain the commitment's plausibility. Suppression remains stable (0.38 to 0.42) because the identity-lock mechanism does not require reinforcement — it is internalized. The claimed_type is tangled_rope: genuine coordination function (preserving intellectual tradition, which has value independent of performance) plus asymmetric extraction (resources extracted from non-beneficiary community members under obligation framing) plus active enforcement (institutional suppression of alternative readings, marginalizing dissenters from authority and teaching). The claim/metric independence is deliberate: the scenario presents this reading as the mainstream institutional framing; the authored metrics describe what the constraint actually does (asymmetric resource extraction with coordination cover).
 *
 * PERSPECTIVAL GAP:
 *   Agenda-setters compute (from the engine's derivation) as net beneficiaries with moderate extraction, seeing the constraint as coordination. Contributors and non-beneficiary members compute as targets with high extraction, experiencing the constraint as obligation enforcement. This divergence is the measurement the framework exists to take. The authored claim (tangled_rope) does not adjudicate the divergence; the engine detects it.
 *
 * DIRECTIONALITY LOGIC:
 *   The halakhic scholarship community derives directionality near the beneficiary end (d~0.15) because they collect resource flows, control interpretive authority, and have mobile exit options (they could teach other domains, though identity-lock makes this costly). They are the primary beneficiary of the constraint's maintenance. Study resource contributors sit near moderate extraction (d~0.65) because they pay voluntarily under obligation framing, but their exit is constrained by identity and community standing; the contribution flow to them is legitimacy and collective responsibility fulfillment, but the benefit is uncertain and future-contingent. Non-beneficiary community members sit at high extraction (d~0.90) because they bear opportunity costs with minimal benefit and their dissent is not heard; they are powerless and trapped in the collective obligation. Messianic tradition guardians sit near d~0.20 because they benefit from the suspension framework maintaining their authority, but they also carry the obligation to maintain the tradition (so they incur costs as well); their exit from this reading would collapse their authority, making them identity-locked to it. The excluded dissenters are not directionality-classified because they are not stakeholder seats in THIS constraint — they are analytically excluded voices whose alternative readings constitute OTHER constraints.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is 'preserve the commitment to perform Temple sacrifice in suspended state, maintaining the intellectual and institutional apparatus for messianic restoration.' The founding problem (Temple destruction, material impossibility of performance) remains contested as either live (commitment still binding, suspension is temporary) or dead (commitment has been transformed or should be abandoned, suspension is permanent). The founding_problem_status is authored as 'contested' because the halakhic scholarship community attests it is live, while alternative communities and dissenters attest it is resolved through transformation or reinterpretation. If the founding problem is dead and the commitment has been authorized to transform (symbolic_transformation reading), then the mandate — preserve it in suspension for restoration — is obsolete. The constraint would then be a mandatrophy case: resources directed to maintaining suspension when the commitment has already transformed. The measurement series show theater ratio rising (0.35 to 0.48), consistent with increasing performative work to maintain a mandate that may be obsolete. The omegas on suspension-vs-transformation and messianic-timeline-ambiguity directly address whether mandatrophy exists. If the symbolic_transformation reading becomes mainstream, the constraint reclassifies and mandatrophy_resolved becomes true.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_transformation,
    'Is the commitment genuinely suspended (temporarily non-performable, awaiting restoration) or has it undergone authorized permanent transformation (prayer and study are now the binding instantiation)?',
    'Documentary or textual evidence from halakhic authorities acknowledging and legitimizing transformation as permanent rather than suspended; shifts in institutional framing from ''until restoration'' to ''as reformed practice''; empirical observation of whether communities describe the commitment as awaiting future performance or as completed transformation.',
    'If suspended: the hybrid preparatory reading stands and resources are justified by future contingency. If transformed: the commitment is no longer extractive suspension but a reformed constraint with different beneficiary structure and different obligation binding. The classification would shift from tangled_rope (asymmetric extraction with coordination) toward rope (genuine coordination without future-contingency extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suspension_vs_transformation, conceptual, 'Whether suspension is temporary or permanent transformation.').

omega_variable(
    performability_boundary,
    'What level of material condition would constitute ''restoration''? Is a rebuilt Temple structurally necessary, or could restoration occur through alternative institutional means (a restored Jewish state, restored sacrificial authority, etc.)?',
    'Textual analysis of halakhic sources on restoration conditions; contemporary halakhic rulings on whether sacrifice law can be instantiated without a Temple structure; empirical observation of community responses to partial restoration scenarios (e.g., if a Temple were rebuilt but not the entire institutional apparatus).',
    'A narrow restoration boundary (Temple structure only) keeps the commitment more decisively suspended and justifies long-term study resource allocation. A broad boundary (multiple paths to restoration) makes the suspension more contingent and reduces the certainty of long-term study focus. Either way, the extractiveness remains moderate because the beneficiary classes are locked into the interpretation regardless of performance conditions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performability_boundary, conceptual, 'What conditions constitute sufficient restoration to instantiate the commitment.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the measured suppression (0.42) primarily structural (external pressure to accept the commitment and fund study) or internalized (believers have fused their identity with the obligation such that questioning it feels like self-rejection)?',
    'Post-exit trajectory analysis: do individuals who exit the community and disconnect from the obligation framework continue to experience suppression (internalized), or do they recover autonomous choice (structural)? Comparison of suppression intensity across communities with different institutional enforcement intensity.',
    'If internalized: the effective suppression is higher than the structural measure suggests; exit for resource contributors is more costly because it breaks identity. If structural: alternative frameworks and exit paths would be more viable if institutional enforcement relaxed. The classification (tangled_rope) remains stable, but the ''victims'' seat''s actual exit options may be materially constrained differently than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether suppression of alternative readings is structural or internalized identity-lock.').

omega_variable(
    messianic_timeline_ambiguity,
    'How long can the commitment remain in suspended preparatory state before the resource allocation itself becomes indefensible? Is there an implicit or explicit timeline for messianic restoration, or is indefinite suspension institutionally sustainable?',
    'Historical analysis of how long similar commitments have been sustained in suspension across religious traditions; contemporary halakhic discussion of obligation binding under indefinite material impossibility; empirical observation of resource contribution sustainability across generations.',
    'If messianic restoration is expected within a bounded timeframe (centuries), the resource allocation is defensible as preparation. If indefinite, the constraint increasingly looks like a snare (pure extraction justified by a non-falsifiable future condition) rather than a tangled rope (coordination with asymmetric cost). The theater ratio would rise, suggesting performative rather than functional resource use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_timeline_ambiguity, preference, 'Implicit or explicit timeline for messianic restoration that justifies indefinite suspension.').

omega_variable(
    kernel_reading_structure,
    'This constraint is ONE READING of the contested temple_sacrifice_commitment kernel. The sibling readings (study_as_exercise, performance_only, symbolic_transformation) instantiate the same kernel differently. Which reading is structurally correct?',
    'Documentary evidence from halakhic sources on the authoritative status of suspension vs. transformation; institutional consensus measures across Jewish communities; empirical observation of which reading dominates teaching and practice.',
    'The classification of this constraint as tangled_rope depends on the hybrid preparatory reading''s framing that suspension is temporary and resources are justified by future restoration. If a sibling reading (study_as_exercise, symbolic_transformation) becomes the institutional mainstream, the constraint reclassifies: study_as_exercise shifts it toward rope (study is direct fulfillment), symbolic_transformation shifts it toward scaffold (transformation is complete, not suspended). The constraint_id and story remain fixed, but the computed type diverges.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Kernel reading contest: which interpretation of the commitment is authoritative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__hybrid_preparatory, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t4, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 4, 0.39).
narrative_ontology:measurement_basis(temp_tr_t4, observed).
narrative_ontology:measurement(temp_tr_t8, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 8, 0.43).
narrative_ontology:measurement_basis(temp_tr_t8, observed).
narrative_ontology:measurement(temp_tr_t12, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 12, 0.46).
narrative_ontology:measurement_basis(temp_tr_t12, observed).
narrative_ontology:measurement(temp_tr_t16, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 16, 0.47).
narrative_ontology:measurement_basis(temp_tr_t16, observed).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_commitment__hybrid_preparatory, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(temp_tr_t20, projected).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t4, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(temp_be_t4, observed).
narrative_ontology:measurement(temp_be_t8, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 8, 0.55).
narrative_ontology:measurement_basis(temp_be_t8, observed).
narrative_ontology:measurement(temp_be_t12, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 12, 0.57).
narrative_ontology:measurement_basis(temp_be_t12, observed).
narrative_ontology:measurement(temp_be_t16, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(temp_be_t16, observed).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_commitment__hybrid_preparatory, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(temp_be_t20, projected).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t4, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 4, 0.4).
narrative_ontology:measurement_basis(temp_su_t4, observed).
narrative_ontology:measurement(temp_su_t8, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 8, 0.41).
narrative_ontology:measurement_basis(temp_su_t8, observed).
narrative_ontology:measurement(temp_su_t12, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 12, 0.42).
narrative_ontology:measurement_basis(temp_su_t12, observed).
narrative_ontology:measurement(temp_su_t16, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 16, 0.42).
narrative_ontology:measurement_basis(temp_su_t16, observed).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_commitment__hybrid_preparatory, suppression_requirement, 20, 0.42).
narrative_ontology:measurement_basis(temp_su_t20, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__hybrid_preparatory, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__hybrid_preparatory, 0.12).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__hybrid_preparatory, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested temple_sacrifice_commitment kernel. The constraint family consists of four sibling constraints, each instantiating a different reading of the same halakhic commitment under conditions of material impossibility. Each sibling has distinct ε (extractiveness), beneficiary/victim structure, and classification. The hybrid_preparatory reading (this file) maintains that study preserves the commitment in suspended state pending messianic restoration, justifying resource allocation but introducing extraction because the future is uncertain. The study_as_exercise reading treats study as direct performance and shifts to rope or mountain. The performance_only reading treats study as archival and shifts to snare or piton. The symbolic_transformation reading treats transformation as permanent authorization and shifts to scaffold. All four readings are linked via network.affects_constraints to indicate structural interdependence: which reading becomes institutional mainstream determines which constraint becomes dominant.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, moderate, 0.85).
constraint_indexing:directionality_override(temple_sacrifice_commitment__hybrid_preparatory, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
