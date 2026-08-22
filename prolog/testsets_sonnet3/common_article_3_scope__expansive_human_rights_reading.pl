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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: Common Article 3 as Universal Humanitarian Floor (Expansive Human Rights Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   Common Article 3 to the 1949 Geneva Conventions sets minimum humanitarian
 *   standards for 'armed conflict not of an international character.' The
 *   expansive human rights reading, crystallized in the ICTY's Tadic
 *   jurisdiction decision and pushed further by human rights bodies and NGOs,
 *   holds that CA3 applies to any organized armed violence meeting minimal
 *   intensity/organization criteria, as an unwaivable floor, regardless of
 *   whether the state involved acknowledges the existence of an 'armed
 *   conflict.' This reading is deliberately generated as ONE constraint among
 *   three siblings sharing the CA3 kernel: the state-centric reading (which
 *   insists on higher intensity/organization thresholds and preserves state
 *   classification discretion) and the ICRC customary reading (which locates
 *   scope in evolving customary practice rather than either a fixed threshold
 *   or an expansive rights floor). This story authors ε, metrics,
 *   beneficiaries, and victims for the expansive reading alone; the siblings
 *   are separate constraints linked via cs_structure.reading_relations, not
 *   alternative measurements of the same ε.
 *
 * KEY AGENTS:
 *   - detained_persons: primary beneficiaries (powerless/trapped) — gain a durable, externally-enforceable protection claim
 *   - state_security_forces and counterinsurgency_commanders: primary targets (institutional-powerful/constrained) — bear expanded legal exposure and operational constraint
 *   - human_rights_monitoring_bodies and international_criminal_tribunals: agenda-setters who operationalize the reading and gain jurisdictional reach from it
 *   - governments_facing_internal_unrest: excluded from the interpretive venues that apply this reading, despite bearing its costs directly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.42).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.38).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "Common Article 3 as Universal Humanitarian Floor (Expansive Human Rights Reading)").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, '6bca7e93-7996-4178-8401-7a54bb991c64').
narrative_ontology:cs_kernel_codification('6bca7e93-7996-4178-8401-7a54bb991c64', fixed_text).
narrative_ontology:cs_authority_grounding('6bca7e93-7996-4178-8401-7a54bb991c64', expertise).
narrative_ontology:cs_interpretation_layer_present('6bca7e93-7996-4178-8401-7a54bb991c64').
narrative_ontology:cs_reading_relation('6bca7e93-7996-4178-8401-7a54bb991c64', common_article_3_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('6bca7e93-7996-4178-8401-7a54bb991c64', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('6bca7e93-7996-4178-8401-7a54bb991c64', foundational, humanitarian_floor_attaches_independent_of_classification).
narrative_ontology:cs_axiom_status(humanitarian_floor_attaches_independent_of_classification, holdable).
narrative_ontology:cs_axiom_grounding('6bca7e93-7996-4178-8401-7a54bb991c64', humanitarian_floor_attaches_independent_of_classification, deontological).
narrative_ontology:cs_axiom('6bca7e93-7996-4178-8401-7a54bb991c64', secondary, protection_gaps_from_classification_disputes_are_impermissible).
narrative_ontology:cs_axiom_status(protection_gaps_from_classification_disputes_are_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('6bca7e93-7996-4178-8401-7a54bb991c64', protection_gaps_from_classification_disputes_are_impermissible, instrumental).
narrative_ontology:cs_reference_frame('6bca7e93-7996-4178-8401-7a54bb991c64', id_1949_geneva_conventions_minimum_floor).
narrative_ontology:cs_drift_state('6bca7e93-7996-4178-8401-7a54bb991c64', post_tadic_tribunal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6bca7e93-7996-4178-8401-7a54bb991c64', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detained_persons).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_commanders).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, internal_security_ministries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held by state forces during internal violence that may not rise to the state's own definition of 'armed conflict.' Under this reading, they are guaranteed humane treatment, protection from summary execution, torture, and degrading treatment, and basic judicial guarantees regardless of how the state labels the situation. Their only leverage is the external legal claim itself; they have no independent power to enforce it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detained_persons, beneficiary,
    powerless, immediate, trapped, national).

% Live in areas where organized violence occurs below the threshold states typically recognize as war. Under the expansive reading, they are entitled to the CA3 floor of protection from violence to life and person, hostage-taking, and outrages upon dignity, extending humanitarian coverage into situations previously treated as ordinary law enforcement.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, biographical, trapped, regional).

% Conduct counterinsurgency, riot suppression, and internal security operations. Under this reading, their operations become subject to CA3 obligations and external scrutiny even when the state insists the situation is domestic law enforcement, not armed conflict. Their tactical latitude narrows and their personnel face exposure to international monitoring and potential prosecution for conduct previously treated as a purely domestic matter.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces, payer,
    institutional, immediate, constrained, national).

% Plan and order operations against organized armed groups. This reading exposes them personally to command-responsibility liability under international criminal law even in conflicts their own government refuses to classify as armed conflict. They cannot exit the legal framework by relabeling the operation; the classification is made externally by monitors and tribunals, not by the state chain of command.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, counterinsurgency_commanders, payer,
    powerful, biographical, constrained, national).

% Set doctrine and rules of engagement for internal security operations, and simultaneously bear the political and legal cost when the expansive reading pulls their operations into humanitarian law's ambit. They lose the discretion to define situations as ordinary crime control exempt from international humanitarian scrutiny.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, internal_security_ministries, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, internal_security_ministries, agenda_setter).

% NGOs, UN special rapporteurs, and regional human rights courts invoke and apply the expansive reading to bring state internal security operations under humanitarian scrutiny. They gain jurisdictional reach and legitimacy from treating CA3 as a floor triggered by any organized armed violence, independent of state classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, human_rights_monitoring_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Rely on the expansive reading (as developed in Tadic and subsequent jurisprudence) to assert jurisdiction over internal violence that states attempt to characterize as below the threshold of armed conflict. The reading expands their docket and their authority to adjudicate state conduct.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_criminal_tribunals, agenda_setter).

% Would object that the expansive reading strips them of sovereign discretion to manage internal order as law enforcement rather than armed conflict, and would prefer the state-centric threshold reading. Their objection is heard in diplomatic and treaty-negotiation forums, but not inside the human-rights monitoring and tribunal apparatus that applies this reading in practice.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, governments_facing_internal_unrest, excluded,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single non-derogable floor of humane treatment that applies the moment organized armed violence occurs, so that no actor — state or non-state — can escape minimum humanitarian obligations by disputing the conflict's legal label. This solves the real coordination problem of protection gaps that open whenever classification is contested.
% TRANSFER_FUNCTION: Moves discretion away from states over how to characterize internal violence, and moves legal exposure and behavioral constraint onto state security forces and commanders; in return it moves protection and a durable legal claim onto detainees and civilians who previously depended entirely on domestic law for safety.
% ABSENT_VOICES: Governments conducting internal security operations are structurally excluded from the venues (tribunals, UN monitoring mechanisms, human rights courts) that apply this reading — they argue their position in treaty negotiation and diplomatic pushback, but the expansive reading is operationalized by bodies where their classification preference carries no controlling weight.
% DISAPPEARANCE_RATIONALE: If the expansive reading were abandoned, states could relabel internal armed violence as ordinary law enforcement and escape CA3 scrutiny entirely; detainees and civilians in low-intensity or ambiguous conflicts would lose their primary international legal claim to humane treatment, and tribunals would lose jurisdictional reach they currently exercise over internal security operations.
% FOUNDING_PROBLEM: States were evading humanitarian obligations by refusing to classify internal violence as 'armed conflict,' leaving people harmed in civil unrest, counterinsurgency, and internal repression without any humanitarian law floor, since human rights law alone (with its derogation clauses) was seen as insufficient protection during emergencies.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by the ICTY Appeals Chamber's Tadic jurisdictional decision (1995), which is itself a tribunal ruling rather than a human-rights-advocacy document, and by the ICRC's own commentary noting persistent state practice of denying armed-conflict classification to avoid IHL obligations. Governments subject to the reading dispute the SCOPE of the problem's persistence but do not deny that classification evasion has occurred historically.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) rather than high because the reading's core function — extending a humanitarian floor to previously uncovered populations — is a genuine coordination achievement, not primarily a rent-extraction device; the extraction component is the erosion of state discretion and the imposition of external legal exposure on security forces, which the reading treats as a legitimate cost of universal protection rather than illegitimate taking. Suppression (0.38) reflects real but incomplete enforcement capacity: monitoring bodies and tribunals can investigate and prosecute, but cannot compel state compliance the way a domestic legal system could. Resistance is high (0.72) because states facing internal unrest actively contest the classification and lobby against the expansive reading in diplomatic and treaty fora — this is exactly the resistance dynamic expected of a reading that redistributes discretion away from powerful institutional actors.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (detainees, civilians, monitoring bodies), this reading looks like coordination: a floor that closes protection gaps everyone should want closed. From the state security seats, the same reading looks like externally-imposed extraction of operational latitude and exposure to prosecution for conduct they consider legitimate internal governance. The engine should compute these as different seat-level types from the same structural data — that divergence is the point of a tangled_rope classification rather than a defect in the data.
 *
 * DIRECTIONALITY LOGIC:
 *   Detained persons and civilian populations are structural beneficiaries under this reading: the constraint subsidizes their protection by attaching irrespective of state classification, so their derived directionality sits near the full-beneficiary end. State security forces, commanders, and security ministries are structural targets: the reading imposes obligations and legal exposure on them that they did not choose and cannot escape by relabeling their operations, placing their derived directionality near the full-target end. Monitoring bodies and tribunals sit as agenda-setters whose institutional standing is enhanced by the reading's broad application — a secondary beneficiary relationship distinct from the primary humanitarian beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — states evading humanitarian obligations via classification games — remains live (state practice of denying armed-conflict status persists into the present), so this is not a case of an arrangement outliving its function. The expansive reading's active enforcement and contested resistance profile indicate a functioning, if imperfectly enforced, coordination mechanism rather than an inertial piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_indeterminacy,
    'Where precisely does ''organized armed violence'' begin, such that CA3''s floor attaches, versus ordinary law enforcement that remains outside humanitarian law entirely?',
    'Comparative analysis of tribunal jurisprudence (Tadic and successors) applying the intensity/organization test to borderline cases, cross-checked against ICRC customary law studies and state objection patterns.',
    'A very low threshold validates the expansive reading''s practical reach but increases the perceived extraction from state security forces; a threshold that converges with the state-centric reading would narrow the practical gap between the two readings even though their normative premises remain distinct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_indeterminacy, conceptual, 'Ambiguity in where the expansive reading''s organized-violence threshold actually sits in practice.').

omega_variable(
    reading_selection_as_framing,
    'Is the choice of the expansive human rights reading over the state-centric or customary readings itself a contested political act, or does it reflect a genuine legal-doctrinal convergence (as claimed by tribunals like the ICTY)?',
    'Track whether state parties to the Geneva Conventions formally accede to the expansive reading''s threshold in subsequent treaty practice or persistently object (persistent objector doctrine), versus whether tribunal jurisprudence continues to expand unilaterally.',
    'If states broadly accede, this reading converges toward genuine customary status; if persistent objection continues, the reading remains contested, and treating it as a settled floor rather than an advocacy position would overstate its authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_as_framing, conceptual, 'Whether the expansive reading is settled law or an actively contested normative position within the kernel.').

omega_variable(
    enforcement_capacity_gap,
    'Given that human rights monitoring bodies and tribunals lack direct enforcement power over sovereign states, how much of the extraction measured here is realized versus merely claimed?',
    'Empirical study of prosecution and compliance rates for CA3 violations in situations states classified as internal law enforcement, versus situations they classified as armed conflict.',
    'Low realized enforcement would suggest the extraction from state security forces is more reputational/legal-exposure risk than concrete operational constraint, softening the tangled_rope classification toward something closer to a rope with aspirational teeth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Gap between the reading''s claimed reach and its actually enforced reach against non-cooperating states.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1949, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(comm_tr_t2005, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(comm_tr_t2015, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(comm_tr_t2025, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1977, 0.2).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(comm_be_t2005, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2005, 0.34).
narrative_ontology:measurement(comm_be_t2015, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2015, 0.39).
narrative_ontology:measurement(comm_be_t2025, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1949, 0.15).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1977, 0.2).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1995, 0.3).
narrative_ontology:measurement(comm_su_t2005, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(comm_su_t2015, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2015, 0.37).
narrative_ontology:measurement(comm_su_t2025, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'CA3 scope' under the ε-invariance principle. The expansive_human_rights_reading authors the broadest beneficiary set and highest state-facing suppression/resistance profile of the three; state_centric_reading authors a narrower beneficiary set constrained by intensity/organization thresholds and lower state-facing extraction; icrc_customary_reading authors scope as a function of evolving state practice, with ε tracking customary consolidation rather than either fixed threshold or rights-maximizing floor. Each carries its own ε, its own claimed_type, and its own stakeholder set; they are linked here rather than merged because measuring CA3 scope through the rights-maximizing observable versus the state-practice observable versus the intensity-threshold observable yields materially different ε values — the ε-invariance test requires decomposition rather than a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
