% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Normative Illegitimacy of Total War (post-1945)
 *   domain: international_relations/strategic_studies/commitment_system
 *
 * SUMMARY:
 *   This constraint story instantiates the 'normative_reading_drop' reading
 *   of the contested kernel 'total_war_winnability_post1945'. The reading
 *   holds that total war remains physically possible (nuclear weapons exist,
 *   industrial capacity exists) but became normatively illegitimate through
 *   the UN Charter Article 2(4) prohibition on the use of force and the
 *   post-1945 development of International Humanitarian Law. The constraint
 *   is a coordination mechanism: states mutually forego total war in exchange
 *   for reciprocal restraint, enforced through a treaty-based institutional
 *   order. Beneficiaries are global civilian populations and small states;
 *   the constrained parties are revisionist great powers and aspirant
 *   hegemons whose expansionist ambitions the normative order blocks. This
 *   reading coexists with two sibling readings:
 *   'structural_contraction_reading' (nuclear weapons physically removed
 *   total war from the reachable space) and 'strategic_culture_drift' (total
 *   war dropped from elite discourse via ideational shift). The three
 *   readings are not logically foreclosing — they identify different causal
 *   mechanisms that may operate simultaneously.
 *
 * KEY AGENTS:
 *   - UN Security Council P5: agenda_setter/beneficiary (institutional/analytical) — authors and selectively enforces the normative order
 *   - Global civilian populations: beneficiary (powerless/trapped) — primary intended beneficiaries, no exit from interstate system
 *   - Small states under security umbrella: beneficiary (moderate/constrained) — depend on normative order for survival
 *   - Revisionist great powers: payer (powerful/constrained) — bear costs of compliance, face suppression when testing boundaries
 *   - Aspirant hegemons: payer (organized/identity_locked) — denied traditional escalation ladder, exit means abandoning self-conception
 *   - International legal community: observer (organized/analytical) — maintains interpretive infrastructure
 *   - Humanitarian NGOs: beneficiary (moderate/mobile) — operationalize the constraint, identity fused to IHL regime
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.22).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.38).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.22).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, accessibility_collapse, 0.32).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Normative Illegitimacy of Total War (post-1945)").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies/commitment_system").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '18416612-c576-433e-b2d0-210461df99b4').
narrative_ontology:cs_kernel_codification('18416612-c576-433e-b2d0-210461df99b4', formalized).
narrative_ontology:cs_authority_grounding('18416612-c576-433e-b2d0-210461df99b4', lineage).
narrative_ontology:cs_interpretation_layer_present('18416612-c576-433e-b2d0-210461df99b4').
narrative_ontology:cs_reading_relation('18416612-c576-433e-b2d0-210461df99b4', total_war_winnability_post1945__structural_contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('18416612-c576-433e-b2d0-210461df99b4', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('18416612-c576-433e-b2d0-210461df99b4', foundational, aggressive_war_categorically_prohibited).
narrative_ontology:cs_axiom_status(aggressive_war_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('18416612-c576-433e-b2d0-210461df99b4', aggressive_war_categorically_prohibited, conventional).
narrative_ontology:cs_axiom('18416612-c576-433e-b2d0-210461df99b4', foundational, civilian_immunity_non_derogable).
narrative_ontology:cs_axiom_status(civilian_immunity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('18416612-c576-433e-b2d0-210461df99b4', civilian_immunity_non_derogable, deontological).
narrative_ontology:cs_reference_frame('18416612-c576-433e-b2d0-210461df99b4', un_charter_1945_founding_moment).
narrative_ontology:cs_drift_state('18416612-c576-433e-b2d0-210461df99b4', post_cold_war_humanitarian_intervention_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('18416612-c576-433e-b2d0-210461df99b4', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, small_states_under_security_umbrella).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, aspirant_hegemons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, un_security_council_p5).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, humanitarian_ngos).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, un_charter_article_2_4).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, ihl_proportionality_distinction).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__normative_reading_drop, crimes_against_humanity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and enforce the UN Charter framework that criminalizes aggressive war. The P5 hold veto power over enforcement authorization, making them both architects and selective enforcers of the normative order. They benefit from a system that locks in their privileged status while constraining revisionist challengers.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, un_security_council_p5, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__normative_reading_drop, un_security_council_p5, beneficiary).

% The primary intended beneficiaries of the normative prohibition on total war. They cannot exit the interstate system and have no leverage over its rules, but the constraint's coordination function — limiting deliberate targeting of civilians, restricting means of warfare — directly reduces their expected harm from major conflict.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations, beneficiary,
    powerless, biographical, trapped, global).

% States that rely on the normative order and extended deterrence for survival. They benefit from a world where conquest is stigmatized and territorial integrity is a norm. Their exit options are limited — they cannot individually guarantee their security — but they gain disproportionately from the constraint's operation.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, small_states_under_security_umbrella, beneficiary,
    moderate, biographical, constrained, regional).

% States whose strategic ambitions require territorial revision or sphere-of-influence expansion that the normative order forbids. They bear the costs of compliance (forgone conquest, sanctions risk, legitimacy deficits) and face active suppression when they test boundaries. Exit from the normative order is structurally possible but carries extreme reputational and coalition-building costs.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, revisionist_great_powers, payer,
    powerful, biographical, constrained, global).

% Regional powers seeking dominant status whose path to hegemony historically ran through total war. The normative order denies them the traditional escalation ladder. Their exit is identity-locked: abandoning revisionist aims means abandoning the self-conception that drives their domestic legitimacy and strategic doctrine.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, aspirant_hegemons, payer,
    organized, biographical, identity_locked, regional).

% Scholars, judges, and practitioners who maintain the interpretive infrastructure of IHL and the UN Charter. They adjudicate compliance, develop doctrine, and legitimate (or delegitimate) state behavior. Their professional existence depends on the constraint's normative vitality.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, international_legal_community, observer,
    organized, generational, analytical, global).

% Organizations that operationalize the normative constraint through monitoring, advocacy, and direct protection. They benefit from the legal framework that authorizes their access and legitimizes their claims. Their exit is mobile — they could pivot to other causes — but their institutional identity is fused to the IHL regime.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__normative_reading_drop, humanitarian_ngos, beneficiary,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of mutual restraint: states forego the option of total war (unlimited violence for unlimited ends) in exchange for reciprocal assurance that others will do the same, reducing the expected cost of security competition for all.
% TRANSFER_FUNCTION: Transfers strategic latitude from revisionist/aspirant powers (who lose the option of unlimited escalation) to global civilian populations and status-quo beneficiaries (who gain protection from total war's worst effects). The transfer is enforced through legitimacy costs, sanctions regimes, and the institutional architecture of the UN Charter and IHL.
% ABSENT_VOICES: Populations in non-signatory or failed-state zones where the normative order has no purchase; future generations who inherit the constraint but had no voice in its creation; insurgent and non-state armed groups who are bound by IHL but excluded from its authorship.
% DISAPPEARANCE_RATIONALE: If the normative prohibition on total war vanished overnight, the strategic calculus of great-power competition would shift immediately: escalation thresholds would rise, targeting restraints would erode, and the institutional architecture of arms control and humanitarian law would lose its foundation. The world would rearrange toward a pre-1945 logic of unlimited war as a legitimate policy instrument.
% FOUNDING_PROBLEM: The catastrophe of 1914-1945 — two industrialized total wars that killed ~70-85 million people and demonstrated that the pre-1914 order's legitimacy mechanisms (balance of power, limited war conventions) had catastrophically failed to restrain escalation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record of 1914-1945 (independent of any beneficiary), by the UN Charter's preamble ('save succeeding generations from the scourge of war'), and by continuing great-power rhetoric that invokes the horror of total war as justification for restraint. No serious analyst claims the problem is solved; nuclear deterrence and normative prohibition are twin partial answers to a live problem.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__normative_reading_drop, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__normative_reading_drop, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__normative_reading_drop, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(total_war_winnability_post1945__normative_reading_drop, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__normative_reading_drop, 0.22, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).
:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22) because the constraint's primary function is coordination (mutual restraint) not extraction — the 'payment' from revisionist powers is the price of coordination, not a rent captured by a concentrated beneficiary. Suppression is moderate (0.38) because enforcement is real (sanctions, legitimacy costs, ICC, UNSC authorization) but inconsistent and heavily mediated by P5 interests. Theater ratio (0.28) reflects genuine performative compliance: states ratify treaties and emit rhetoric while pursuing limited wars that test boundaries (Korea, Vietnam, Gulf Wars, Ukraine). Accessibility collapse (0.32) is low because alternatives (limited war, proxy war, hybrid warfare) remain fully available — the constraint closes only the total-war option. Resistance (0.55) is significant because revisionist powers actively contest the normative order's legitimacy and test its enforcement boundaries. The claimed type is rope: a genuine coordination problem solved via treaty with active enforcement, net beneficiaries, and no concentrated extractor.
 *
 * PERSPECTIVAL GAP:
 *   From the P5/agenda-setter seat, the constraint is a successful coordination achievement (rope) that they administer. From revisionist/aspirant payer seats, the same structure operates as asymmetric suppression — a rope that binds them but not the P5 (who wage limited wars freely). From the civilian beneficiary seat, it is a protection regime with real but incomplete coverage. The engine computes this divergence from the structural data; the divergence IS the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 are structural beneficiaries (d near 0.0): they wrote the rules, hold veto power, and the order locks in their privilege. Global civilians are beneficiaries (d near 0.0) but trapped — they collect protection without agency. Small states are beneficiaries (d ~0.2) with constrained exit — they gain disproportionately but cannot exit the system. Revisionist powers are payers (d ~0.8) with constrained exit — they bear compliance costs and face suppression, but can (at high cost) defect. Aspirant hegemons are payers (d ~0.9) with identity_locked exit — their strategic identity requires revisionism, making exit existentially costly. Legal community and NGOs are observers/beneficiaries with analytical/mobile exit — they sit near symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing 1914-1945 style total war) remains live — nuclear weapons and normative prohibition are twin partial answers. The constraint has not atrophied into a piton because its coordination function is actively invoked in every major crisis (Cuban Missile Crisis, 1991 Gulf War, Ukraine 2022). However, theater ratio has risen as the gap between rhetorical commitment and operational practice has widened (humanitarian intervention doctrine, 'responsibility to protect' vs. selective enforcement). This is mandatrophy pressure, not resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_vs_structural_causality,
    'How much of the observed drop in total war winnability is attributable to normative illegitimacy vs. nuclear structural contraction vs. strategic culture drift?',
    'Counterfactual analysis: would total war have remained thinkable without nuclear weapons but with the normative order? Would the normative order have emerged without the structural shock of nuclear weapons? Requires disaggregating the three mechanisms in historical cases where they diverge (e.g., conventional-only great power dyads, nuclear-armed revisionists).',
    'If normative illegitimacy is the dominant causal mechanism, the constraint is a genuine rope (coordination achievement). If structural contraction dominates, the constraint is a mountain (physical impossibility) with a normative veneer — false summit candidate. If strategic culture drift dominates, the constraint is a piton (theatrical maintenance of a faded coordination).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(normative_vs_structural_causality, conceptual, 'Causal attribution across the three sibling readings of the kernel').

omega_variable(
    p5_enforcement_selectivity,
    'Is the P5''s selective enforcement of the normative order a bug (hypocrisy undermining legitimacy) or a feature (managed flexibility preventing system collapse)?',
    'Compare enforcement outcomes: cases where P5 authorized enforcement against non-P5 (Gulf War 1991, Libya 2011) vs. cases where P5 blocked enforcement against themselves or allies (Vietnam, Afghanistan, Iraq 2003, Ukraine 2022 vetoes). Assess whether the system''s survival depends on this selectivity.',
    'If a feature, the constraint is a robust rope with built-in pressure valves. If a bug, the constraint is a tangled_rope (coordination with asymmetric extraction: P5 extract exemption while constraining others) or a snare (normative cover for P5 impunity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(p5_enforcement_selectivity, empirical, 'Whether P5 enforcement selectivity is structural necessity or legitimacy corrosion').

omega_variable(
    identity_lock_mechanism_aspirants,
    'What specific identity-fusion mechanism binds aspirant hegemons to revisionist aims — domestic legitimacy dependency, strategic doctrine path-dependence, elite self-conception, or institutional inheritance?',
    'Comparative case study of aspirant hegemons (Wilhelmine Germany, Imperial Japan, Cold War USSR, contemporary China, Iran, Turkey): trace the domestic-political and ideational sources of revisionist commitment. Assess reversibility — which mechanisms would break under what conditions?',
    'If identity_lock is primarily domestic-legitimacy-dependent, it may crack under economic stress. If primarily doctrinal/self-conception, it is more durable. Determines whether aspirant hegemons are permanently constrained (identity_locked → high d) or potentially mobile (constrained → lower d).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_aspirants, conceptual, 'Mechanism of identity_lock for aspirant hegemons — determines their directionality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t1945, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1945, 0.1).
narrative_ontology:measurement(tota_tr_t1960, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(tota_tr_t1975, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(tota_tr_t1990, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(tota_tr_t2005, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2005, 0.25).
narrative_ontology:measurement(tota_tr_t2025, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(tota_be_t1945, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(tota_be_t1960, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(tota_be_t1975, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1975, 0.2).
narrative_ontology:measurement(tota_be_t1990, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(tota_be_t2005, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2005, 0.21).
narrative_ontology:measurement(tota_be_t2025, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 2025, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t1945, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1945, 0.3).
narrative_ontology:measurement(tota_su_t1960, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1960, 0.35).
narrative_ontology:measurement(tota_su_t1975, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(tota_su_t1990, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 1990, 0.35).
narrative_ontology:measurement(tota_su_t2005, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2005, 0.37).
narrative_ontology:measurement(tota_su_t2025, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 2025, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(total_war_winnability_post1945__normative_reading_drop, 0.1).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, nuclear_deterrence_stability).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, ihl_compliance_regime).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, unsc_authorization_politics).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'total_war_winnability_post1945'. The structural_contraction_reading (nuclear weapons physically removed total war) and strategic_culture_drift (ideational shift in strategic culture) are sibling constraints. All three share the same referent (post-1945 total war winnability) but author different causal mechanisms and thus different ε values, beneficiary/victim structures, and claimed types. They form a constraint family linked by mutual affects_constraints edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, institutional, 0.05).
constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, powerful, 0.75).
constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
