% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Feud Obligation Suppression via Ecclesiastical Pacification (Christianized Reading)
 *   domain: legal/religious/medieval
 *
 * SUMMARY:
 *   This constraint instantiates the ecclesiastical-pacification reading of
 *   blood-feud obligation: the Church declares that kinship vengeance
 *   violates divine law prohibiting human vengeance, and claims sole
 *   authority to legitimize violence and to release feud participants from
 *   obligation through penitential discipline. All feud participants enter
 *   the victim set—they face spiritual peril from an obligation the reading
 *   reframes as sin. The Church and monarchy enter the beneficiary set—they
 *   gain jurisdictional expansion, interpretive monopoly on legitimate
 *   violence, and suppression of a private-justice mechanism that competes
 *   with their authority. This reading structurally diverges from the
 *   stateless-coordination reading (which frames feuds as self-enforcing
 *   justice) and the extraction-cycle reading (which frames feuds as
 *   destructive resource depletion). The claim/metric gap is intentional: the
 *   constraint is CLAIMED as tangled_rope (genuine pacification coordination
 *   + asymmetric extraction via ecclesiastical authority), while the metrics
 *   describe substantially extractive, actively enforced operation. The
 *   engine measures that divergence from structural data; do not reconcile
 *   claim to metrics.
 *
 * KEY AGENTS:
 *   - feud_participants: victims bearing spiritual peril and identity-lock from reframed obligation
 *   - ecclesiastical_authority: agenda-setter and beneficiary, claims interpretive monopoly on divine law and legitimate violence
 *   - royal_institution: beneficiary and co-agenda-setter, gains private-violence suppression and theological legitimacy
 *   - affected_kinship_groups: victims facing suppression of honor-based dispute resolution
 *   - alternative_dispute_mechanisms (wergeld, secular arbitration): excluded by assertion of ecclesiastical monopoly
 *   - theological_dissenters: excluded from authority structure that adjudicates divine law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.78).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.71).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Feud Obligation Suppression via Ecclesiastical Pacification (Christianized Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal/religious/medieval").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '1ad80b69-9122-4ec2-b681-2c79a3a7185f').
narrative_ontology:cs_kernel_codification('1ad80b69-9122-4ec2-b681-2c79a3a7185f', fixed_text).
narrative_ontology:cs_authority_grounding('1ad80b69-9122-4ec2-b681-2c79a3a7185f', lineage).
narrative_ontology:cs_interpretation_layer_present('1ad80b69-9122-4ec2-b681-2c79a3a7185f').
narrative_ontology:cs_reading_relation('1ad80b69-9122-4ec2-b681-2c79a3a7185f', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('1ad80b69-9122-4ec2-b681-2c79a3a7185f', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('1ad80b69-9122-4ec2-b681-2c79a3a7185f', foundational, vengeance_inherently_sinful).
narrative_ontology:cs_axiom_status(vengeance_inherently_sinful, holdable).
narrative_ontology:cs_axiom_grounding('1ad80b69-9122-4ec2-b681-2c79a3a7185f', vengeance_inherently_sinful, deontological).
narrative_ontology:cs_axiom('1ad80b69-9122-4ec2-b681-2c79a3a7185f', foundational, ecclesiastical_monopoly_on_violence_legitimacy).
narrative_ontology:cs_axiom_status(ecclesiastical_monopoly_on_violence_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('1ad80b69-9122-4ec2-b681-2c79a3a7185f', ecclesiastical_monopoly_on_violence_legitimacy, conventional).
narrative_ontology:cs_reference_frame('1ad80b69-9122-4ec2-b681-2c79a3a7185f', divine_law_against_human_vengeance).
narrative_ontology:cs_drift_state('1ad80b69-9122-4ec2-b681-2c79a3a7185f', late_medieval_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1ad80b69-9122-4ec2-b681-2c79a3a7185f', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_institution).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feud_participants).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, affected_kinship_groups).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, divine_law_against_vengeance).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_monopoly_on_violence_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Blood-kin bound by reciprocal obligation to pursue vengeance for slain or dishonored kinfolk. Under the Christianized reading, these participants face spiritual peril: the feud obligation itself is reframed as a sin against divine law. Exit means abandoning familial honor and kinship identity, or submitting to ecclesiastical penitential discipline that dissolves the obligation through spiritual authority rather than through killing. They bear the cost of living under a prohibition against an obligation their entire social identity is structured around.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feud_participants, payer,
    moderate, biographical, identity_locked, regional).

% Church claims sole legitimate authority to interpret divine law and to adjudicate the spiritual status of violence. Under this reading, the Church administers penitential discipline to 'release' participants from feud obligations by spiritual means, thereby expanding its jurisdictional reach into secular aristocratic disputes and consolidating interpretive monopoly over what constitutes legitimate use of force. The Church collects expanded authority, confession traffic, and institutional leverage over secular rulers who must legitimize their own violence through ecclesiastical approval.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority, beneficiary).

% Monarchy has interest in suppressing private feud violence to consolidate monopoly on legitimate violence authority. Under the Christianized reading, royal institution gains theological legitimacy for its own violence (defense, punishment, war) by delegating to Church the task of pacifying feuds through penitential authority. The Crown benefits from reduced destabilizing private violence and gains partnership with Church in suppressing an obligation that competes with royal jurisdiction.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_institution, beneficiary,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_institution, agenda_setter).

% Extended kin networks whose prestige, territorial security, and alliance networks depend on the capacity to conduct feuds and maintain honor through reciprocal violence. Under the Christianized reading, these groups face simultaneous doctrinal prohibition (divine law against vengeance) and practical suppression (ecclesiastical discipline, royal enforcement). They can exit only by accepting permanent status loss and delegated institutional authority over their own honor and justice.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, affected_kinship_groups, payer,
    powerful, generational, constrained, regional).

% Secular arbitration, compensation wergeld systems, and honor-restorative mechanisms that operated alongside feuds are structurally displaced by ecclesiastical pacification claims. They would offer dispute resolution without doctrinal prohibition or ecclesiastical dependency, but are kept marginal by the reading's assertion that only divine authority and its ecclesiastical interpreter can legitimately resolve kinship violence.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, alternative_dispute_mechanisms, excluded,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_non_agent(feud_obligation_kernel__christianized_pacification_reading, alternative_dispute_mechanisms).

% Communities and clerics who dispute the interpretation that blood-feud is intrinsically sinful, or who see ecclesiastical pacification as institutional power-grab rather than doctrinal necessity. They are structurally excluded from the ecclesiastical authority structure that adjudicates the meaning of divine law, yet remain geographically subject to the enforcement machinery.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, theological_dissenters, excluded,
    moderate, biographical, trapped, continental).

% Historical analyst examining the reading from outside its epistemic frame. Can assess whether the Christianized pacification reading reflects genuine doctrinal commitment or strategic institutional expansion, and compare it against sibling readings grounded in coordination function and extraction-cycle dynamics.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, temporal_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_authority).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pacification of private violence by centralizing authority over legitimate force through ecclesiastical interpretation of divine law: the reading coordinates feud suppression by reframing the obligation itself as violation of a transcendent prohibition, not merely as socially disruptive. Resolution occurs through penitential discipline rather than kinship reciprocity.
% TRANSFER_FUNCTION: Transfers jurisdictional authority over kinship violence from autonomous kin networks to ecclesiastical institutions; transfers prestige from kinship-based honor to Church-mediated salvation status; transfers the legitimacy of violence enforcement from reciprocal obligation to delegated royal and ecclesiastical monopoly.
% ABSENT_VOICES: Secular arbitration traditions and compensation-based dispute resolution would argue that feuds can be resolved through restitution and negotiated honor-restoration without doctrinal prohibition or ecclesiastical dependency. Kinship honor systems would contest the premise that vengeance is inherently sinful. These alternatives are structurally excluded because the reading asserts that only divine authority—interpreted through ecclesiastical monopoly—can legitimate violence resolution.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification reading vanished—i.e., if ecclesiastical doctrine no longer prohibited vengeance and Church no longer mediated legitimate violence—feud participants would lack the doctrinal prohibition that transforms obligation into sin, and would lack the ecclesiastical authority path for dissolving obligations. Kinship networks would return to managing feuds through reciprocal violence and/or secular negotiation. Ecclesiastical and royal institutions would lose jurisdictional reach and theological legitimacy for their own violence monopoly. The world reorganizes around decentralized kinship authority rather than ecclesiastical-royal pacification.
% FOUNDING_PROBLEM: Uncontrolled blood-feud violence fragmenting Christian polities, destabilizing territorial authority, preventing economic development, creating perpetual cycles of reciprocal killing within and across kin networks. The Christianized reading frames this as a problem of theological legitimacy: feuds persist because kinship obligation is framed as honorable, even sacred; the solution is doctrinal reinterpretation declaring vengeance sinful and delegating legitimate violence authority to God's earthly representatives (Church and divinely-sanctioned monarchy).
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical authorities and royal chroniclers testify that blood-feud is the founding problem and that Christianization suppresses it. Feud participants and affected kinship groups, testimony from alternative legal traditions, and historians examining tribal-era coordination dynamics contest whether feuds are inherently destructive or whether the 'problem' is constructed to justify institutional expansion. No external witnesses affirm the founding problem without institutional interest in the pacification reading.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness measures 0.78 at interval end because the ecclesiastical pacification reading concentrates authority over violence legitimacy in Church and Crown, extracting from kinship networks the autonomous capacity to manage honor and justice. Suppression is high (0.71) because the reading enforces a doctrinal prohibition on an obligation structured into kinship identity—participants cannot exit without abandoning family honor. Theater ratio rises from 0.28 to 0.42 over the interval because the ecclesiastical apparatus increasingly emphasizes performative penitence and doctrinal conformity over actual reduction in conflict (feuds continue; penitential theater substitutes for ecclesiastical inability to eliminate the underlying obligation structure). Accessibility collapse is moderate (0.68): alternatives to feud (wergeld, secular arbitration) remain conceptually available but are systematically undermined by ecclesiastical claims that only Church-mediated resolution carries theological legitimacy. Resistance is substantial (0.59) because feud participants and kinship groups continue to conduct feuds despite ecclesiastical prohibition, indicating active pushback against the reading's normative claim.
 *
 * PERSPECTIVAL GAP:
 *   Ecclesiastical and royal seats experience the arrangement as genuine coordination (violence suppression) plus legitimate collection (expanded authority for solving a real problem). Feud-participant and kinship-group seats experience the arrangement as asymmetric extraction (suppression of autonomous justice capacity) plus spiritual coercion (obligation reframed as sin, with exit requiring submission to ecclesiastical authority). The metrics reflect the target-seat experience: high extractiveness (suppression of justice autonomy) and high suppression (doctrinal prohibition backed by confessional discipline and royal enforcement). The beneficiary seats would author lower extractiveness if claiming from their perspective; the engine detects this divergence from the structural data (beneficiary/victim declarations + exit options).
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants and kinship groups are targets (d near 1.0): they bear suppression, lose autonomous justice authority, face identity-lock from kinship obligation reframed as sin. Ecclesiastical authority is the beneficiary (d near 0.0): it collects jurisdictional expansion, interpretive monopoly, expanded penitential authority. Royal institution is a beneficiary (d near 0.0): it gains private-violence suppression and theological legitimacy for its own force monopoly. Alternative dispute mechanisms are excluded (d undefined): they would compete but are structurally pushed out by ecclesiastical monopoly claims. The directionality chain: beneficiaries (Church/Crown) set agenda, enforce doctrine, suppress alternatives; victims (feud participants) face prohibition on an obligation their identity is built around, with exit path requiring submission to ecclesiastical authority and permanent status loss.
 *
 * MANDATROPHY ANALYSIS:
 *   The Christianized pacification reading claims genuine coordination function: feud suppression via doctrinal reinterpretation solves a real founding problem (uncontrolled kinship violence fragmenting polities). But the metrics indicate the founding problem is contested and the solution is substantially extractive. The mandatrophy frame: (founding_problem_status=contested AND disappearance_verdict=world_rearranges AND extracted_jurisdiction_concentrated) suggests the reading is maintaining a claim of coordination (pacification) that masks extraction (ecclesiastical-royal authority consolidation). The theater_ratio rise (0.28 to 0.42) indicates the pacification function is increasingly performative—penitential theater rather than actual violence reduction. Mandatrophy is NOT resolved; the reading is a live case where coordination and extraction are entangled and the dispute over which is primary is the fundamental contestation across the kernel's sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrinal_vs_institutional_motivation,
    'Does the Christianized prohibition on vengeance reflect genuine doctrinal commitment to divine law against human judgment, or is it a strategic institutional expansion by Church and Crown seeking to consolidate authority over violence?',
    'Comparative analysis of doctrinal consistency: does the Church apply the divine-law prohibition consistently across all violence, or selectively exempt royal/ecclesiastical violence? Historical evidence of whether ecclesiastical authorities advocated pacification before institutional incentives to suppress feuds became clear.',
    'If doctrinal, the reading is coordination grounded in genuine theological principle; if strategic, the reading is extraction using doctrine as cover. Classification would shift from tangled_rope (coordination+extraction) toward snare (doctrine as pure cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_vs_institutional_motivation, conceptual, 'Whether Christianized pacification is genuine doctrinal commitment or strategic institutional power consolidation.').

omega_variable(
    identity_lock_mechanism,
    'Is the ''identity-locked'' exit status of feud participants permanent (kinship identity cannot be abandoned without total social annihilation), or is it conditional on community enforcement of honor norms that ecclesiastical authority can eventually overwrite?',
    'Longitudinal observation of whether second and third generations of penitentially-released families re-enter feuds, or whether ecclesiastical authority becomes internalized such that honor-from-kinship eventually recedes as an identity anchor.',
    'If permanent identity-lock, the constraint remains highly extractive indefinitely (d stays near 1.0). If conditional, d shifts toward constrained as religious identity substitutes for kinship identity across generations, reducing but not eliminating the constraint''s extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether kinship identity-lock is structurally permanent or can be rewritten by institutional authority over time.').

omega_variable(
    kernel_reading_boundary,
    'What specific structural features distinguish the Christianized pacification reading from its siblings? Does the reading''s core claim (feud is sinful; only Church legitimates violence) foreclose the stateless-coordination reading (feud is self-enforcing justice), or do the readings merely occupy different parties'' claims without logical incompatibility?',
    'Examine whether a kinship group can simultaneously hold that feuds provide legitimate justice AND that vengeance is sinful—i.e., whether the readings can coexist within one framework, or whether adopting the Christianized reading logically requires rejecting the coordination reading''s premise.',
    'If readings foreclose each other, the kernel exhibits genuine logical incompatibility and classification edges are ''forecloses''. If readings coexist across different parties'' frameworks, the relation is ''coexists_with''. If one reading creates institutional pressure that makes the other harder to maintain (but doesn''t logically rule it out), the relation is ''influences''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the Christianized reading logically forecloses or coexists with sibling readings.').

omega_variable(
    suppression_internalization,
    'As ecclesiastical authority deepens over generations, does the suppression of feud obligation become internalized (participants come to believe vengeance is sinful) or remain structural (suppression persists only through active enforcement and exclusion of dissenting voices)?',
    'Post-suppression behavioral analysis: if feud obligation were to be ecclesiastically legitimized again, would participants resume feuds immediately (structural suppression only) or would they exhibit continued reluctance (internalized suppression)? Or: do confessional practices reveal sincere belief in sin or theatrical compliance?',
    'If internalized, the suppression is deeper and the constraint''s effective hold stronger; if structural, the suppression depends on active ecclesiastical and royal enforcement. A shift toward internalization would indicate a successful pacification reading; structural suppression suggests the reading is extractive theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of feud obligation is internalized as belief in sin or remains externally enforced.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(feud_tr_t0, observed).
narrative_ontology:measurement(feud_tr_t5, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 5, 0.31).
narrative_ontology:measurement_basis(feud_tr_t5, observed).
narrative_ontology:measurement(feud_tr_t10, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(feud_tr_t10, observed).
narrative_ontology:measurement(feud_tr_t15, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(feud_tr_t15, observed).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(feud_tr_t25, observed).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(feud_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(feud_be_t0, observed).
narrative_ontology:measurement(feud_be_t5, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement_basis(feud_be_t5, observed).
narrative_ontology:measurement(feud_be_t10, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement_basis(feud_be_t10, observed).
narrative_ontology:measurement(feud_be_t15, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement_basis(feud_be_t15, observed).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 25, 0.74).
narrative_ontology:measurement_basis(feud_be_t25, observed).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 40, 0.78).
narrative_ontology:measurement_basis(feud_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(feud_su_t0, observed).
narrative_ontology:measurement(feud_su_t5, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement_basis(feud_su_t5, observed).
narrative_ontology:measurement(feud_su_t10, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(feud_su_t10, observed).
narrative_ontology:measurement(feud_su_t15, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement_basis(feud_su_t15, observed).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 25, 0.67).
narrative_ontology:measurement_basis(feud_su_t25, observed).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(feud_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.14).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel__extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% The feud_obligation_kernel decomposes into three structurally distinct constraint readings: (1) christianized_pacification_reading frames feud as doctrinal violation requiring ecclesiastical suppression (present story); (2) stateless_coordination_reading frames feud as self-enforcing justice mechanism in absence of centralized authority; (3) extraction_cycle_reading frames feud as destructive resource depletion preventing territorial consolidation. The three readings share a common kernel (the contested status of blood-feud obligation) but diverge radically in their ε values, beneficiary/victim structures, and institutional implications. Each reading is a separate constraint with its own classification. The network links show how the Christianized reading's institutional expansion (ecclesiastical monopoly on violence legitimacy) creates structural pressure on the coordination reading (makes feud-as-justice harder to defend) and the extraction reading (shifts framing from resource depletion to doctrinal sin). All three readings remain live in medieval discourse; no single framework holds all three simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__christianized_pacification_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
