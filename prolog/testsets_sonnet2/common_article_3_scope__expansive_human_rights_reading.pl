% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 as Universal Humanitarian Floor (Expansive Human Rights Reading)
 *   domain: international_law/human_rights
 *
 * SUMMARY:
 *   This story instantiates the expansive human rights reading of the Common
 *   Article 3 scope kernel: CA3's minimum humanitarian standards attach to
 *   any organized armed violence, regardless of whether a state formally
 *   classifies the situation as an armed conflict, an internal disturbance,
 *   or ordinary law enforcement. Under this reading, human rights monitoring
 *   bodies and international courts determine when 'organized armed violence'
 *   exists, and their determination controls even when it conflicts with the
 *   state's own characterization. This produces a genuine coordination
 *   function (a humanitarian floor that cannot be evaded by definitional
 *   gamesmanship) coupled with an asymmetric cost imposed on states
 *   conducting internal security operations, who face external monitoring,
 *   reputational exposure, and potential prosecution they did not anticipate
 *   when they classified their own operations as policing. This is ONE of
 *   three readings of the common_article_3_scope kernel; the
 *   state_centric_reading and icrc_customary_reading are separate constraints
 *   with their own ε values, not alternative measurements of this one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.42).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.35).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 as Universal Humanitarian Floor (Expansive Human Rights Reading)").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_law/human_rights").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'e8da9689-92f5-4b19-85a7-d6d9f12bc347').
narrative_ontology:cs_kernel_codification('e8da9689-92f5-4b19-85a7-d6d9f12bc347', fixed_text).
narrative_ontology:cs_authority_grounding('e8da9689-92f5-4b19-85a7-d6d9f12bc347', extraction).
narrative_ontology:cs_interpretation_layer_present('e8da9689-92f5-4b19-85a7-d6d9f12bc347').
narrative_ontology:cs_reading_relation('e8da9689-92f5-4b19-85a7-d6d9f12bc347', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('e8da9689-92f5-4b19-85a7-d6d9f12bc347', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('e8da9689-92f5-4b19-85a7-d6d9f12bc347', foundational, protection_attaches_to_violence_not_classification).
narrative_ontology:cs_axiom_status(protection_attaches_to_violence_not_classification, holdable).
narrative_ontology:cs_axiom_grounding('e8da9689-92f5-4b19-85a7-d6d9f12bc347', protection_attaches_to_violence_not_classification, deontological).
narrative_ontology:cs_axiom('e8da9689-92f5-4b19-85a7-d6d9f12bc347', secondary, external_bodies_hold_final_classificatory_authority).
narrative_ontology:cs_axiom_status(external_bodies_hold_final_classificatory_authority, holdable).
narrative_ontology:cs_axiom_grounding('e8da9689-92f5-4b19-85a7-d6d9f12bc347', external_bodies_hold_final_classificatory_authority, conventional).
narrative_ontology:cs_reference_frame('e8da9689-92f5-4b19-85a7-d6d9f12bc347', geneva_conventions_1949_minimum_floor).
narrative_ontology:cs_drift_state('e8da9689-92f5-4b19-85a7-d6d9f12bc347', post_cold_war_tribunal_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e8da9689-92f5-4b19-85a7-d6d9f12bc347', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_commanders).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, domestic_law_enforcement_agencies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by state forces during operations the state characterizes as internal security policing rather than armed conflict. Under this reading, they receive CA3 protections (no cruel treatment, no summary execution, judicial guarantees) regardless of how the state labels the situation. Their entire protection depends on the classification threshold being read low.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Live in areas experiencing organized violence below the threshold states traditionally required for humanitarian law to attach. Under this reading, they gain protection and a monitoring apparatus regardless of state classification, but have no direct power to compel compliance beyond appeal to external bodies.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, biographical, trapped, regional).

% NGOs, UN mechanisms, and international courts that adopt and apply this expansive reading to assess state conduct, issue findings, and refer cases for prosecution. They set the interpretive agenda by determining when 'organized armed violence' exists, effectively deciding when CA3 attaches independent of the state's own characterization.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, agenda_setter,
    institutional, generational, analytical, global).

% Conduct counter-gang, counter-cartel, or internal security operations that they classify as law enforcement, not armed conflict. Under this reading, their operations are pulled into humanitarian law's ambit anyway, exposing personnel to war-crimes liability and international scrutiny they did not anticipate when the operation began under a policing framework.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, immediate, constrained, national).

% Plan and execute operations against organized armed groups below the intensity threshold traditionally required for non-international armed conflict. This reading holds them to CA3 standards of humane treatment and judicial guarantees for detainees regardless of their own operational classification, constraining tactics they consider legitimate under domestic emergency law.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_commanders, payer,
    powerful, biographical, constrained, national).

% Police and paramilitary units responding to gang violence, cartel warfare, or civil unrest. This reading potentially subjects their use-of-force decisions to international humanitarian law scrutiny instead of solely domestic policing and human rights law, a jurisdictional expansion they did not consent to and cannot avoid once violence is deemed 'organized.'
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, domestic_law_enforcement_agencies, payer,
    institutional, immediate, constrained, national).

% Would prefer to retain sole authority over classifying their own internal security situations, since classification determines which legal regime and which external accountability mechanisms apply. Under this expansive reading their classification is treated as evidence, not as dispositive, sidelining their preferred sovereign prerogative.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_governments, excluded,
    institutional, generational, constrained, national).

% Track how this reading interacts with customary practice and formal treaty commentary; they neither fully endorse nor reject the expansive threshold but observe its effects on state compliance behavior and on the coherence of the broader CA3 kernel across readings.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_and_treaty_bodies, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__expansive_human_rights_reading, diffuse).
narrative_ontology:fixing_cost_class(common_article_3_scope__expansive_human_rights_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a genuinely low, uniform floor of humane treatment (no torture, no summary killing, judicial guarantees) that applies the instant organized violence exists, closing the gap where states could otherwise engineer non-application by refusing to classify a conflict as armed conflict.
% TRANSFER_FUNCTION: Moves protective guarantees and monitoring authority from state security apparatuses to detained persons, affected civilians, and international human rights bodies; correspondingly moves operational latitude and unilateral classification authority away from states and toward external interpretive institutions.
% ABSENT_VOICES: State governments and their legal advisors, who would argue this reading strips them of a sovereign classification prerogative they consider essential to distinguishing ordinary policing from armed conflict; they are present in diplomatic fora but structurally outvoted within human-rights monitoring and judicial bodies that have adopted this reading.
% DISAPPEARANCE_RATIONALE: Detainees and monitoring bodies would say the world rearranges sharply — protections currently claimed under this reading would evaporate for populations caught in ambiguous, sub-threshold violence, and prosecutions premised on this reading would become unavailable. States conducting internal security operations would say little changes, since they never accepted this reading's application to their operations in the first place and continue under domestic law regardless.
% FOUNDING_PROBLEM: States were evading Geneva Convention obligations by refusing to formally characterize internal violence as 'armed conflict,' leaving persons in low-intensity but genuinely organized violence (death squads, internal repression, undeclared civil wars) without any humanitarian floor.
% FOUNDING_PROBLEM_CORROBORATION: UN Human Rights Council special rapporteurs and international criminal tribunals (outside the direct beneficiary population) have repeatedly found, in cases such as Tadić and subsequent ICTY/ICTR jurisprudence, that states manipulate classification to avoid humanitarian obligations — corroborating that the founding problem persists independent of the beneficiaries' own advocacy.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, contested).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).
:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42 by interval end) because the reading imposes real compliance costs and legal exposure on state actors without those actors receiving a commensurate coordination benefit from the expanded scope — from the state's perspective, the reading extracts operational latitude and jurisdictional certainty. Suppression is comparatively low (0.35) because the mechanism is legal/reputational and judicial, not coercive in the direct sense; states retain the practical ability to ignore rulings, though at increasing cost as international jurisprudence hardens (reflected in the rising suppression_requirement series, tracking the maturation of tribunal enforcement infrastructure like ICTY/ICTR precedent). Resistance is high (0.72) because states actively contest this reading in diplomatic and legal fora, refusing to concede that low-level operations trigger armed-conflict obligations.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of human rights monitoring bodies and detained persons, this reading is coordination — a hard floor that cannot be gamed away by refusing to use the word 'conflict.' From the seat of state security forces and counterinsurgency commanders, the same reading appears as externally imposed extraction: their operational discretion and their own classification authority are overridden by institutions that were not party to the operational decision. The engine should compute these as genuinely different seat-level classifications from the same structural data — this is a design feature of the tangled_rope declaration, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and civilian populations are structural beneficiaries under this reading — they gain protections they would not have under a narrower threshold, and their exit options are trapped (they cannot opt out of the violence around them), which is precisely why the expansive floor exists to protect them regardless of classification. State security forces, counterinsurgency commanders, and domestic law enforcement bear the cost: their operations, once framed as internal matters exempt from international humanitarian law, are pulled into a legal regime with prosecutorial consequences. Their exit options are constrained rather than trapped — they can alter tactics or accept legal exposure, but cannot exit the jurisdictional pull of the reading once adopted by monitoring bodies with institutional standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — states evading humanitarian obligations through definitional avoidance — remains live and well corroborated by tribunal jurisprudence outside the beneficiary population itself, so this reading has not drifted into pure inertial performance. The classification as tangled_rope (rather than snare) reflects that there IS a genuine coordination function serving real protective need, not merely extraction dressed as protection; the corresponding victim declaration for state security actors prevents this reading from being mislabeled as pure Rope, which would erase the real cost states experience under it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_definition_indeterminacy,
    'What precisely counts as ''organized armed violence'' sufficient to trigger CA3 under this reading, and who has final authority to determine that threshold has been met?',
    'Accumulated international tribunal jurisprudence (ICTY/ICTR/ICC organization-and-intensity case law) narrowing or widening the operative definition over time; state acceptance or rejection of specific rulings as precedent.',
    'A narrow, well-settled definition would make this reading converge toward the state_centric_reading in practice; a broad, elastic definition sustains this reading''s distinct expansive character and its higher extraction from state security actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_definition_indeterminacy, conceptual, 'Indeterminacy in the operative definition of ''organized armed violence'' under the expansive reading.').

omega_variable(
    external_authority_legitimacy,
    'Do human rights monitoring bodies and international courts possess legitimate authority to override a state''s own classification of its internal security operations, or does this constitute an encroachment on sovereign prerogative?',
    'State consent patterns to international jurisdiction (ratification of optional protocols, acceptance of tribunal jurisdiction) versus unilateral assertions of universal jurisdiction by monitoring bodies without such consent.',
    'If legitimacy is well-grounded in accepted treaty commitments, the reading operates as genuine coordination backed by consent; if authority is asserted without state consent in many cases, the extraction component is closer to unilateral imposition than negotiated floor-setting.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_authority_legitimacy, conceptual, 'Contested legitimacy of external classification authority versus state sovereignty.').

omega_variable(
    sibling_reading_divergence_location,
    'Where exactly does this reading''s classification of a given real-world operation diverge from the state_centric_reading''s classification, and how often does that divergence actually occur in practice versus remaining theoretical?',
    'Comparative case coding of actual internal security operations (cartel conflicts, gang suppression campaigns, low-intensity insurgencies) against both readings'' thresholds to measure empirical divergence rate.',
    'High divergence rate confirms these are structurally distinct constraints with materially different victim sets and extraction profiles; low divergence rate would suggest the readings converge in most real cases and differ mainly in edge cases and rhetoric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_divergence_location, empirical, 'Empirical frequency of classificatory divergence between this reading and the state-centric sibling reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t8, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(comm_tr_t16, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(comm_tr_t24, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 24, 0.18).
narrative_ontology:measurement(comm_tr_t32, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(comm_tr_t40, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comm_be_t8, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(comm_be_t16, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(comm_be_t24, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(comm_be_t32, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(comm_be_t40, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(comm_su_t8, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement(comm_su_t16, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 16, 0.28).
narrative_ontology:measurement(comm_su_t24, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 24, 0.31).
narrative_ontology:measurement(comm_su_t32, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 32, 0.33).
narrative_ontology:measurement(comm_su_t40, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 40, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__expansive_human_rights_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'CA3 scope' per the ε-invariance principle. Each reading carries its own ε: the expansive_human_rights_reading (this story) authors moderate extraction concentrated on state security actors; the state_centric_reading authors lower extraction confined to genuine armed-conflict-intensity situations; the icrc_customary_reading authors extraction contingent on and tracking evolving state practice. All three are linked via affects_constraints rather than merged, because measuring 'CA3 scope' by the threshold-determination observable yields materially different ε depending on which reading is applied.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
