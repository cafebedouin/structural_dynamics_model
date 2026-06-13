% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Total War Winnability Post-1945 (Strategic Culture Drift Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint models the strategic culture drift reading of post-1945
 *   total war winnability. The claim is that total war remains physically and
 *   institutionally reachable — the capability to mobilize society for
 *   unlimited objectives, the military-industrial infrastructure, the
 *   doctrinal knowledge — but has been dropped from elite strategic discourse
 *   through an ideational shift, not through structural impossibility.
 *   Defense intellectuals and professional military establishments maintain
 *   the constraint via theoretical consensus that total war is irrational,
 *   illegitimate, or impossible, while the underlying capacity atrophies
 *   through institutional forgetting rather than active destruction. This
 *   reading coexists with the normative reading (Article 2(4) made it
 *   illegitimate) and the structural reading (nuclear weapons made it
 *   impossible), and each produces a different constraint story with
 *   different extraction profiles and beneficiary structures. This story
 *   traces the piton character: the founding problem (how to make sense of
 *   total war after 1945) is dead; the constraint persists through theatrical
 *   maintenance (rehearsals of its unthinkability) rather than because anyone
 *   actively defends it as necessary or good.
 *
 * KEY AGENTS:
 *   - limited_war_defense_intellectuals: defend disciplinary consensus that total war is irrational; identity-locked to the constraint's maintenance
 *   - state_strategic_planners: operate under the constraint as if it were natural; sacrifice flexibility by accepting it as inevitable
 *   - nuclear_weapons_states: set and enforce the constraint through doctrine; trapped at both ends (cannot use total war, cannot openly abandon the norm)
 *   - rival_strategic_traditions: excluded from formal discourse; maintain alternative frameworks that don't share the Western constraint
 *   - military_historians: observe the gap between reachability and discussability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.58).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.72).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.58).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Total War Winnability Post-1945 (Strategic Culture Drift Reading)").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international_relations/strategic_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '50484328-e965-4e43-8044-bdb2eaa3f1c6').
narrative_ontology:cs_kernel_codification('50484328-e965-4e43-8044-bdb2eaa3f1c6', distributed).
narrative_ontology:cs_authority_grounding('50484328-e965-4e43-8044-bdb2eaa3f1c6', extraction).
narrative_ontology:cs_interpretation_layer_present('50484328-e965-4e43-8044-bdb2eaa3f1c6').
narrative_ontology:cs_reading_relation('50484328-e965-4e43-8044-bdb2eaa3f1c6', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('50484328-e965-4e43-8044-bdb2eaa3f1c6', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_axiom('50484328-e965-4e43-8044-bdb2eaa3f1c6', foundational, total_war_unreachable_via_ideational_shift).
narrative_ontology:cs_axiom_status(total_war_unreachable_via_ideational_shift, holdable).
narrative_ontology:cs_axiom_grounding('50484328-e965-4e43-8044-bdb2eaa3f1c6', total_war_unreachable_via_ideational_shift, empirically_contingent).
narrative_ontology:cs_axiom('50484328-e965-4e43-8044-bdb2eaa3f1c6', foundational, institutional_forgetting_sustains_constraint).
narrative_ontology:cs_axiom_status(institutional_forgetting_sustains_constraint, holdable).
narrative_ontology:cs_axiom_grounding('50484328-e965-4e43-8044-bdb2eaa3f1c6', institutional_forgetting_sustains_constraint, instrumental).
narrative_ontology:cs_reference_frame('50484328-e965-4e43-8044-bdb2eaa3f1c6', total_war_reachable_pre1945).
narrative_ontology:cs_drift_state('50484328-e965-4e43-8044-bdb2eaa3f1c6', contemporary_2024, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('50484328-e965-4e43-8044-bdb2eaa3f1c6', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, professional_military_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, state_strategic_planners).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, nuclear_weapons_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defend a disciplinary consensus that total war is strategically illegitimate and irrational, anchoring careers and institutional authority in limited war theory. They benefit from the constraint's maintenance because it validates their professional expertise and makes their prescriptions relevant to state actors. Challenge to the constraint's reachability undermines their epistemological foundation. The constraint's theatrical maintenance (rehearsals of its impossibility) sustains their disciplinary authority.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    institutional, generational, identity_locked, global).

% Operate under a reigning assumption that total war is off the table, constraining their strategic thinking to limited objectives, proportional force, and negotiated settlement. They sacrifice strategic flexibility by accepting the constraint as natural or inevitable rather than as a choice point. Reframing total war as reachable would force explicitly choosing to reject it rather than assuming its impossibility.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, state_strategic_planners, payer,
    institutional, biographical, constrained, global).

% Maintain and enforce the constraint through professional military doctrine, strategic guidance that denies total war legitimacy, and multilateral norm-setting (though rarely explicitly). They bear the cost of the constraint in the form of reduced strategic options in extremis; they set its terms through doctrinal revision cycles and by controlling what scenarios are entertained in war gaming.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, nuclear_weapons_states, agenda_setter,
    powerful, civilizational, trapped, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, nuclear_weapons_states, payer).

% Operate within non-Western strategic cultures (Russian Gerasimov doctrine, Chinese warfighting theory, Islamic jurisprudence on jihad) that may not share the post-1945 Western constraint on total war legitimacy but are excluded from formal discourse on the constraint's validity. Their frameworks remain intellectually live but institutionally marginalized in English-language strategic studies.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, rival_strategic_traditions, excluded,
    moderate, generational, identity_locked, regional).

% Document that total war remains physically and institutionally reachable but note the dramatic drop in serious proposals for it in elite strategic discourse. They track the constraint's theatrical maintenance and the gap between what is possible and what is said to be possible.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, military_historians, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared framework among great powers that total war is off-limits, enabling predictability in strategic competition by constraining the scope of rational conflict escalation. Coordinates expectations about what wars can be and how they should terminate.
% TRANSFER_FUNCTION: Transfers strategic flexibility and existential optionality from state planners to the defense intellectual class whose authority and employment depend on total war remaining unthinkable. Moves the burden of constraint-maintenance from explicit political choice (we are choosing to limit war) to assumed naturalness (total war is impossible/irrational/unthinkable).
% ABSENT_VOICES: Non-Western strategic traditions and adversarial states who question the constraint's legitimacy or necessity are structurally excluded from formal discourse — they remain marginalized in peer-reviewed journals and policy circles. Realist scholars and strategists skeptical of the constraint's persistence are similarly sidelined. Future war planners in existential crises are absent from current deliberation but will decide whether the constraint holds.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared — i.e., if elite discourse shifted to treating total war as a reachable and discussable option — strategic planning would immediately become more militaristically ambitious, war gaming scenarios would expand to include maximalist objectives, doctrinal guidance would reorient, and the framing of rational state behavior in conflict would transform. The defense intellectual consensus would fracture; new theoretical schools would emerge; policy options currently deemed unthinkable would re-enter deliberation.
% FOUNDING_PROBLEM: Post-1945, the spread of nuclear weapons, the rise of humanitarian norms, and the failure of total war to produce political success in Korea and Vietnam created a crisis of confidence in total war as a strategic instrument. The constraint emerged as a learned response to institutional trauma.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapons no longer require total war for deterrence; humanitarian law has matured; military analysts outside the Western defense establishment acknowledge that total war is structurally and legally constrained but do not accept the constraint as permanent. The founding problem (how to make total war rational after 1945) has been displaced by technological and normative shifts that make the constraint look like permanent necessity rather than contingent response.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The measurements show a rising theater ratio (0.25 in 1945 to 0.68 in 2024) coupled with rising suppression requirement — the hallmark of a piton. The actual operative function (preventing escalation to total war) could be maintained by explicit choice or by normative commitment, but instead it is maintained by institutional performance: war games never reach it, doctrinal statements reject it as a category, policy papers treat it as unthinkable without defending why. Extractiveness rises from 0.35 to 0.58 and plateaus — it captures the benefit to the limited-war intellectual class (sustained authority, employment, policy relevance) and the cost to strategic planners (constrained options). The suppression requirement stays high and stable because elites actively suppress the idea that total war could be rationally chosen, even as the underlying capacity persists. The theater ratio rises because the actual work of preventing total war is increasingly replaced by ritualized denials of its possibility. Accessibility collapse is moderate (0.62) because the constraint is not backed by physical barriers — it is backed by consensus, which remains contestable. Resistance is moderate (0.55) because some strategic thinkers and adversarial states resist the constraint's premises, but their resistance is sidelined rather than suppressed outright.
 *
 * PERSPECTIVAL GAP:
 *   From the defense intellectual seat, the constraint is legitimate and necessary — total war is genuinely irrational post-1945, and the maintenance of that consensus is the core mission of strategic studies. From the state planner seat, the constraint is an assumption they inherit and must work within, but which they cannot articulate as chosen. From the nuclear weapons state seat, the constraint is simultaneously real (they enforce it doctrinally) and fictional (they know total war remains reachable and must maintain it through performance). The engine should compute this as a piton from every seat — but with differing d values: beneficiaries get low d (they collect the constraint's outputs); payers get high d (they bear its costs in reduced strategic options); the nuclear states sit near 0.5 (they both enforce and are constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   The limited-war defense intellectuals are declared beneficiaries because their entire disciplinary authority and professional apparatus depend on total war remaining unspeakable. They benefit from the constraint's maintenance without running it — universities, think tanks, journal space, policy access all flow from the consensus that total war is irrational. State strategic planners are payers because they operate under the constraint as a boundary condition, sacrificing the option of maximalist objectives and existential mobilization. The constraint's suppression mechanism is directed at planners: doctrinal guidance, peer review, policy consensus all enforce the boundary without making it explicit. Nuclear weapons states sit at the agenda-setter position because they maintain the constraint through doctrine and norm-setting, but they are also constrained by it — they cannot openly propose total war as a rational option, even in existential crisis, without fracturing their own legitimacy narratives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic piton structure: the founding problem (how to make strategic sense after the failure of total war in Korea and Vietnam) has been solved and superseded. The constraint persists not because the founding problem remains live, but because the institutional apparatus around it has become self-maintaining. Defense intellectuals have career incentives to keep the constraint in place; military doctrines have accumulated assumptions that treat it as unchangeable; policy consensus has hardened. The constraint is not actively maintained because anyone thinks it necessary — it is maintained because dismantling it would require collective action across institutions with no incentive to coordinate on revision. The piton diagnosis is supported by the theater ratio trajectory: as the actual strategic reason for the constraint has weakened (nuclear deterrence is less dependent on total-war prohibition; humanitarian norms have become stronger independent barriers; military technology has shifted toward precision), the theatrical maintenance has increased. The constraint is now more performance than function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reachability_vs_realism,
    'Is total war truly reachable (physically and institutionally), or has the constraint become so embedded in military-technical and organizational structures that reaching it would require fundamental institutional reconstruction that makes it effectively unreachable?',
    'War-gaming scenario with explicit total-war objectives; comparative analysis of Cold War vs. contemporary military planning documents to detect whether the capability to mobilize for total war persists in doctrine.',
    'If reachability is increasingly fictional, the constraint becomes more mountain-like (structural fact of modern warfare) and less piton-like (atrophied institutional capacity). The ε would shift downward and the claimed type would need revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reachability_vs_realism, empirical, 'Whether total war remains structurally reachable or has become institutionally impossible.').

omega_variable(
    beneficiary_identity_lock_mechanism,
    'Is the defense intellectual class genuinely identity-locked to the constraint (their professional identity cannot survive challenge to total-war prohibition), or do they have intellectual and career optionality to survive and even thrive in a discourse that permits total-war discussion?',
    'Historical pattern study: when the constraint has been challenged (Mearsheimer, Posen revisionist work, contemporary China strategy debates), do defense intellectuals adapt and find new authority niches, or do they cling to the constraint?',
    'If they have optionality, the constraint is less extractive than modeled and the beneficiary identification weakens. If they are genuinely identity-locked, the extraction is higher and the piton diagnosis is stronger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_lock_mechanism, empirical, 'Whether defense intellectuals are genuinely locked into constraining-total-war positions or have room to adapt.').

omega_variable(
    theater_ratio_driver_ambiguity,
    'Is the rising theater ratio driven by institutional forgetting (the actual capability to conduct total war is atrophying, so more energy goes to denying it could happen), or by increasing rhetorical performance (the capability persists but requires more elaborate ritualistic denial)?',
    'Compare military education curricula, wargaming scenarios, and doctrinal depth across decades; audit whether total-war planning infrastructure is being actively starved or actively maintained in secret.',
    'If atrophying, the constraint is becoming mountain-like (reachability is actually declining). If being maintained in secret while denied publicly, the constraint is more purely theatrical and piton diagnosis strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_driver_ambiguity, empirical, 'Whether rising theater ratio reflects actual capacity loss or merely increasing rhetorical performance.').

omega_variable(
    kernel_reading_boundary,
    'Where does the strategic_culture_drift reading end and the normative_reading_drop reading begin? Does ideational shift (this reading) fully explain the constraint, or is the normative shift (Article 2(4), humanitarian law) the primary driver and this reading merely describes how the normative constraint is maintained?',
    'Counterfactual: would the constraint persist as strongly in the absence of normative law if strategic culture alone were the driver? Historical analysis of pre-1950 strategic discourse to detect whether the ideational shift preceded or followed normative codification.',
    'If normative law is the primary driver, this reading should decompose; the ε and beneficiary structure would shift. If ideational shift is primary, the piton diagnosis stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'The boundary between this reading and the normative reading of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1945, 0.25).
narrative_ontology:measurement_basis(tota_tr_t1945, observed).
narrative_ontology:measurement(tota_tr_t1962, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1962, 0.42).
narrative_ontology:measurement_basis(tota_tr_t1962, observed).
narrative_ontology:measurement(tota_tr_t1980, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1980, 0.58).
narrative_ontology:measurement_basis(tota_tr_t1980, observed).
narrative_ontology:measurement(tota_tr_t2000, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2000, 0.65).
narrative_ontology:measurement_basis(tota_tr_t2000, observed).
narrative_ontology:measurement(tota_tr_t2015, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2015, 0.68).
narrative_ontology:measurement_basis(tota_tr_t2015, observed).
narrative_ontology:measurement(tota_tr_t2024, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2024, 0.68).
narrative_ontology:measurement_basis(tota_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement_basis(tota_be_t1945, observed).
narrative_ontology:measurement(tota_be_t1962, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1962, 0.48).
narrative_ontology:measurement_basis(tota_be_t1962, observed).
narrative_ontology:measurement(tota_be_t1980, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1980, 0.54).
narrative_ontology:measurement_basis(tota_be_t1980, observed).
narrative_ontology:measurement(tota_be_t2000, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2000, 0.57).
narrative_ontology:measurement_basis(tota_be_t2000, observed).
narrative_ontology:measurement(tota_be_t2015, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement_basis(tota_be_t2015, observed).
narrative_ontology:measurement(tota_be_t2024, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2024, 0.58).
narrative_ontology:measurement_basis(tota_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1945, 0.45).
narrative_ontology:measurement_basis(tota_su_t1945, observed).
narrative_ontology:measurement(tota_su_t1962, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1962, 0.58).
narrative_ontology:measurement_basis(tota_su_t1962, observed).
narrative_ontology:measurement(tota_su_t1980, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 1980, 0.68).
narrative_ontology:measurement_basis(tota_su_t1980, observed).
narrative_ontology:measurement(tota_su_t2000, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2000, 0.71).
narrative_ontology:measurement_basis(tota_su_t2000, observed).
narrative_ontology:measurement(tota_su_t2015, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement_basis(tota_su_t2015, observed).
narrative_ontology:measurement(tota_su_t2024, total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 2024, 0.72).
narrative_ontology:measurement_basis(tota_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__strategic_culture_drift, 0.12).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945__structural_contraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'Can total war be won after 1945?' The strategic_culture_drift reading attributes the constraint to ideational shift in strategic culture — elite discourse has made total war unthinkable. The normative_reading_drop reading attributes it to Article 2(4) and humanitarian law codification. The structural_contraction_reading attributes it to nuclear weapons making total war physically impossible. Each generates a different constraint with different ε, different beneficiary structures, and different piton vs. tangled-rope diagnoses. Constraint family requires all three stories be authored separately with linked network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__strategic_culture_drift, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
