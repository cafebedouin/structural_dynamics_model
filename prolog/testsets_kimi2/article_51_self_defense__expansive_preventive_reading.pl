% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Expansive Preventive Self-Defense Reading of UN Charter Article 51
 *   domain: international_law/security_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the expansive preventive reading of UN
 *   Charter Article 51, under which self-defense extends to preemptive or
 *   preventive uses of force against non-state actors or emerging threats
 *   when the acting state self-judges necessity. The kernel is the Article 51
 *   text ('inherent right of individual or collective self-defence if an
 *   armed attack occurs'); this reading stretches it to cover threats that
 *   have not yet materialized and actors not attributable to states. It is
 *   structurally distinct from the narrow armed-attack reading (which
 *   requires an actual or imminent armed attack by a state) and the
 *   unable/unwilling doctrine (which preserves host-state attribution). The
 *   constraint coordinates expectations around unilateral security action but
 *   extracts authority from the UN collective security framework and extracts
 *   life and security from target-region populations.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Primary beneficiary/agenda_setter (powerful/mobile) â asserts expansive reading, self-judges necessity, captures strategic autonomy.
 *   - defense_sectors: Secondary beneficiary (organized/mobile) â captures procurement and operational rents from expanded legal mandates.
 *   - target_region_populations: Primary target (powerless/trapped) â bear the direct costs of preventive strikes without recourse.
 *   - multilateral_veto_authority: Secondary target (institutional/trapped) â bears the erosion of UN Charter collective security monopoly.
 *   - international_legal_community: Analytical observer (analytical/analytical) â produces competing interpretations without enforcement capacity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Expansive Preventive Self-Defense Reading of UN Charter Article 51").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '38b680b9-ef37-4d04-9c28-c8fc205c3b07').
narrative_ontology:cs_kernel_codification('38b680b9-ef37-4d04-9c28-c8fc205c3b07', fixed_text).
narrative_ontology:cs_authority_grounding('38b680b9-ef37-4d04-9c28-c8fc205c3b07', lineage).
narrative_ontology:cs_interpretation_layer_present('38b680b9-ef37-4d04-9c28-c8fc205c3b07').
narrative_ontology:cs_reading_relation('38b680b9-ef37-4d04-9c28-c8fc205c3b07', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('38b680b9-ef37-4d04-9c28-c8fc205c3b07', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('38b680b9-ef37-4d04-9c28-c8fc205c3b07', foundational, unilateral_necessity_self_judging).
narrative_ontology:cs_axiom_status(unilateral_necessity_self_judging, holdable).
narrative_ontology:cs_axiom_grounding('38b680b9-ef37-4d04-9c28-c8fc205c3b07', unilateral_necessity_self_judging, conventional).
narrative_ontology:cs_axiom('38b680b9-ef37-4d04-9c28-c8fc205c3b07', foundational, non_state_threat_imminence_redefined).
narrative_ontology:cs_axiom_status(non_state_threat_imminence_redefined, holdable).
narrative_ontology:cs_axiom_grounding('38b680b9-ef37-4d04-9c28-c8fc205c3b07', non_state_threat_imminence_redefined, empirically_contingent).
narrative_ontology:cs_reference_frame('38b680b9-ef37-4d04-9c28-c8fc205c3b07', state_sovereignty_security_prerogative).
narrative_ontology:cs_drift_state('38b680b9-ef37-4d04-9c28-c8fc205c3b07', post_9_11_counterterrorism_peak, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('38b680b9-ef37-4d04-9c28-c8fc205c3b07', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, defense_sectors).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Assert expansive interpretations of Article 51 through state practice, legal memoranda, and Security Council vetoes. Self-judge necessity for preventive strikes against non-state actors. Capture strategic autonomy and legal impunity for unilateral force.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    powerful, generational, mobile, global).

% Benefit from expanded operational mandates, procurement cycles, and contracting opportunities justified by preventive counterterrorism and emerging threat doctrines.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, defense_sectors, beneficiary,
    organized, biographical, mobile, global).

% Inhabit regions designated as threat sources or terrorist havens. Subject to unilateral preventive strikes, drone campaigns, and military intervention without recourse to UN Security Council process or individual legal remedy.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_populations, payer,
    powerless, immediate, trapped, local).

% The UN Security Council's exclusive authority to authorize force under Chapter VII is eroded when powerful states bypass the Council through unilateral preventive self-defense claims. Security Council resolutions are bypassed or vetoed; the collective security function atrophies.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, multilateral_veto_authority, payer,
    institutional, generational, trapped, global).

% Debates the scope of Article 51 through scholarship, ICJ proceedings, and treaty commentary. Produces competing interpretations but lacks enforcement capacity over state practice.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_legal_community, observer,
    analytical, generational, analytical, global).

% States whose sovereignty is violated by preventive strikes on their territory. Their objections are heard in UN forums but overridden by the acting state's unilateral necessity determination and veto power.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, excluded_target_states, excluded,
    moderate, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for states to respond to catastrophic threats from non-state actors or emerging dangers when the UN Security Council is paralyzed or too slow, preserving international order against security vacuums.
% TRANSFER_FUNCTION: Transfers the authority to judge necessity and legitimately use force from the UN Security Council collective process to individual militarily capable states, and transfers the costs of military action from those states to target-region populations and the multilateral legal order.
% ABSENT_VOICES: Target-region populations and their representative governments are largely excluded from necessity determinations; the ICJ and majority of non-aligned states are present in discourse but structurally excluded from vetoing specific operations.
% DISAPPEARANCE_RATIONALE: If the expansive preventive reading vanished and was replaced by strict adherence to the narrow armed-attack requirement, militarily capable states would lose legal cover for unilateral counterterrorism and preventive campaigns; the UN Security Council would regain its monopoly on legitimate force authorization; target-region populations would face fewer unilateral strikes but potentially greater security vacuums where collective action fails.
% FOUNDING_PROBLEM: The UN Charter's collective security mechanism was designed for interstate war and cannot respond swiftly to non-state actor attacks or imminent WMD threats, leaving states defenseless if strictly bound to Security Council authorization.
% FOUNDING_PROBLEM_CORROBORATION: Militarily capable states and defense-sector analysts attest the problem is live, citing terrorism and proliferation. The UN Secretary-General and majority of International Court of Justice judges attest the collective security mechanism remains viable but politically blocked; non-aligned states and international legal scholars outside the benefiting parties argue the founding problem is exaggerated to justify hegemonic prerogative.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the necessity determination is self-judged by the acting state, eliminating external constraint and transferring costs to target populations and multilateral authority. Suppression (0.72) reflects the active diplomatic, legal, and veto-powered suppression of alternative narrow interpretations and of UNSC condemnation. Theater ratio (0.45) captures the partial performativity of the legal argumentation: the coordination function (security against non-state actors) is genuine, but legal reasoning is heavily instrumentalized to cover strategic objectives. Accessibility collapse (0.60) is moderate because the narrow reading remains intellectually available but is politically inaccessible to weak states. Resistance (0.68) is substantial: the Non-Aligned Movement, ICJ majorities, and most legal scholars oppose the reading, though they cannot block powerful state practice.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (militarily capable states) experiences this constraint as a necessary legal tool for security in a world of transnational terrorism and paralyzed collective security. The payer seats (target-region populations, multilateral authority) experience it as the erosion of legal protections and the normalization of unilateral violence. The engine computes this divergence from structural data: agenda-setters have mobile exit and institutional power; payers are trapped with powerless or institutional scope.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states are declared beneficiaries with mobile exit and global scope â their directionality sits near the beneficiary pole, damping effective extraction. Defense sectors are beneficiaries with organized power and mobile exit. Target-region populations are declared victims (payer role) with powerless status, trapped exit, and local scope â their directionality sits near the full-target pole, amplifying effective extraction. The multilateral veto authority is a payer with institutional power but trapped exit (the UN cannot abandon its own Charter) and global scope; its directionality is high despite institutional power because it is structurally locked into bearing the constraint's erosion.
 *
 * MANDATROPHY ANALYSIS:
 *   The expansive reading risks misclassification as pure extraction (Snare) because its victims are severe and its beneficiaries concentrated. However, the coordination function is non-zero: in a world where non-state actors pose catastrophic threats and the UNSC is paralyzed by great-power rivalry, a total prohibition on unilateral force may leave states genuinely defenseless. The Tangled Rope classification captures this hybridity: the constraint genuinely coordinates security expectations (providing a legal pathway for unilateral action) while asymmetrically extracting from target populations and multilateral legitimacy. The mandate has not atrophied into Piton because the coordination story is still actively invoked and the strategic environment still produces threats; the constraint is not merely theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_reading_kernel_status,
    'Does the expansive preventive reading of Article 51 represent a genuine interpretation of the Charter text, or a functional replacement of the Charter''s collective security framework with a unilateral state prerogative?',
    'ICJ advisory opinion or comprehensive state practice survey establishing whether Article 51''s ''inherent right'' was understood at inception to include preventive force against non-state actors.',
    'If resolved as replacement, the constraint reclassifies toward extraction (Snare/Tangled Rope with high theater); if resolved as genuine interpretation, the coordination function gains legitimacy and classification shifts toward Rope/Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_kernel_status, conceptual, 'Whether the expansive reading is interpretation or functional replacement of collective security.').

omega_variable(
    necessity_self_judgment_ambiguity,
    'Is the self-judging necessity standard an irreducible feature of decentralized international legal order, or a constructed loophole enabling great-power exemption?',
    'Comparative analysis of necessity determinations across powerful vs. weak states; if weak states'' preventive claims are rejected while powerful states'' are accepted, the standard is constructed extraction.',
    'If constructed, directionality for powerful states is lower than structural derivation suggests (they are beneficiaries of a constructed privilege); if irreducible, the asymmetry is systemic rather than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_self_judgment_ambiguity, empirical, 'Whether self-judging necessity is systemic decentralization or constructed great-power privilege.').

omega_variable(
    collective_security_obsolescence,
    'Has the UN collective security mechanism become obsolete for addressing non-state actor threats, making unilateral self-defense a necessary coordination substitute?',
    'Empirical assessment of UNSC responsiveness to non-state actor threats pre- and post-9/11; if paralysis is structural, the coordination function is genuine; if paralysis is selective (great-power-political), the constraint is extraction.',
    'If the coordination function is genuine, the constraint retains Tangled Rope classification; if the collective security mechanism is deliberately bypassed rather than genuinely paralyzed, classification shifts toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_security_obsolescence, empirical, 'Whether collective security obsolescence is genuine or a cover story for unilateral prerogative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 23).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(arti_tr_t3, article_51_self_defense__expansive_preventive_reading, theater_ratio, 3, 0.55).
narrative_ontology:measurement(arti_tr_t6, article_51_self_defense__expansive_preventive_reading, theater_ratio, 6, 0.5).
narrative_ontology:measurement(arti_tr_t9, article_51_self_defense__expansive_preventive_reading, theater_ratio, 9, 0.42).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__expansive_preventive_reading, theater_ratio, 12, 0.4).
narrative_ontology:measurement(arti_tr_t15, article_51_self_defense__expansive_preventive_reading, theater_ratio, 15, 0.43).
narrative_ontology:measurement(arti_tr_t18, article_51_self_defense__expansive_preventive_reading, theater_ratio, 18, 0.44).
narrative_ontology:measurement(arti_tr_t21, article_51_self_defense__expansive_preventive_reading, theater_ratio, 21, 0.45).
narrative_ontology:measurement(arti_tr_t23, article_51_self_defense__expansive_preventive_reading, theater_ratio, 23, 0.45).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(arti_be_t3, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 3, 0.7).
narrative_ontology:measurement(arti_be_t6, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(arti_be_t9, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 9, 0.58).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 12, 0.64).
narrative_ontology:measurement(arti_be_t15, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(arti_be_t18, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 18, 0.71).
narrative_ontology:measurement(arti_be_t21, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 21, 0.74).
narrative_ontology:measurement(arti_be_t23, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 23, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arti_su_t3, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(arti_su_t6, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(arti_su_t9, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 9, 0.58).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(arti_su_t15, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(arti_su_t18, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 18, 0.68).
narrative_ontology:measurement(arti_su_t21, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(arti_su_t23, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 23, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Article 51 self-defense kernel. It is decomposed from the natural-language label 'Article 51 self-defense' because the expansive preventive reading (high extraction, self-judging necessity) is structurally distinct from the narrow armed attack reading (low extraction, strict attribution) and the unable/unwilling doctrine reading (hybrid extraction through host-state responsibility). Each reading carries a distinct epsilon and stakeholder topology.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
