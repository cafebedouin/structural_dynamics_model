% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA as Scaled Reciprocal Commitment with Graduated Enforcement
 *   domain: International Law / Nuclear Non-Proliferation / Treaty Compliance
 *
 * SUMMARY:
 *   This story instantiates the graduated-compliance reading of the JCPOA
 *   kernel: the deal as a scaled reciprocal commitment where enforcement
 *   tracks the severity of measured violation rather than triggering binary
 *   collapse or unilateral abandonment. Under this reading, the E3/EU
 *   coordinators and the IAEA jointly administer a calibrated ratchet —
 *   partial sanctions relief withdrawal proportional to enrichment increases
 *   — that keeps both parties inside the arrangement through disputes that
 *   would sink a binary treaty or that a transactional actor could void
 *   unilaterally. The coordination function (sustained mutual engagement
 *   despite imperfect compliance) is real, but it is bought by imposing costs
 *   on Iranian civilians, regional security-skeptic states, and both sides'
 *   hardline constituencies, none of whom have a vote on the calibration.
 *   This is one of three sibling constraints on the same underlying kernel
 *   (jcpoa_treaty_bindingness); the binding_multilateral_reading treats the
 *   same standing arrangement as requiring consensus-based dissolution (much
 *   higher accessibility_collapse, near-zero unilateral exit for any party),
 *   and the transactional_provisional_reading treats it as voidable on
 *   unilateral bad-faith determination (much higher volatility, near-zero
 *   suppression since either side can walk at will). All three share the same
 *   underlying diplomatic history but instantiate structurally distinct
 *   constraints with different ε, different victim sets, and different
 *   enforcement logics.
 *
 * KEY AGENTS:
 *   - e3_eu_coordinators: agenda_setter (institutional/constrained) — administers the graduated ratchet
 *   - iranian_government: payer/beneficiary (institutional/constrained) — bears and benefits from calibrated relief
 *   - iranian_civilian_economy: payer (powerless/trapped) — absorbs ratchet consequences with no voice
 *   - sanctions_relief_dependent_economic_actors: beneficiary (moderate/constrained) — profits from incremental re-engagement windows
 *   - iaea_verification_apparatus: beneficiary/agenda_setter (institutional/analytical) — institutional relevance tied to proportionality function
 *   - us_congress_hardliners: excluded (powerful/mobile) — locked out of the calibration process
 *   - regional_proliferation_skeptics: payer (moderate/constrained) — bears externalities of tolerated violation tiers
 *   - hardline_domestic_constituencies_on_both_sides: payer (organized/mobile) — denied the clean rupture their politics requires
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.38).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA as Scaled Reciprocal Commitment with Graduated Enforcement").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "International Law / Nuclear Non-Proliferation / Treaty Compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, '3a796545-dd68-44d8-8039-318e342a1320').
narrative_ontology:cs_kernel_codification('3a796545-dd68-44d8-8039-318e342a1320', distributed).
narrative_ontology:cs_authority_grounding('3a796545-dd68-44d8-8039-318e342a1320', distributed).
narrative_ontology:cs_reading_relation('3a796545-dd68-44d8-8039-318e342a1320', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a796545-dd68-44d8-8039-318e342a1320', jcpoa_treaty_bindingness__transactional_provisional_reading, influences).
narrative_ontology:cs_axiom('3a796545-dd68-44d8-8039-318e342a1320', foundational, proportional_response_sustains_engagement_better_than_binary_breach).
narrative_ontology:cs_axiom_status(proportional_response_sustains_engagement_better_than_binary_breach, holdable).
narrative_ontology:cs_axiom_grounding('3a796545-dd68-44d8-8039-318e342a1320', proportional_response_sustains_engagement_better_than_binary_breach, instrumental).
narrative_ontology:cs_axiom('3a796545-dd68-44d8-8039-318e342a1320', secondary, compliance_is_a_measurable_scale_not_a_binary_state).
narrative_ontology:cs_axiom_status(compliance_is_a_measurable_scale_not_a_binary_state, holdable).
narrative_ontology:cs_axiom_grounding('3a796545-dd68-44d8-8039-318e342a1320', compliance_is_a_measurable_scale_not_a_binary_state, empirically_contingent).
narrative_ontology:cs_reference_frame('3a796545-dd68-44d8-8039-318e342a1320', dual_track_calibrated_reciprocity_2015).
narrative_ontology:cs_drift_state('3a796545-dd68-44d8-8039-318e342a1320', post_2018_us_withdrawal_partial_reconstruction, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3a796545-dd68-44d8-8039-318e342a1320', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_dependent_economic_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_proliferation_skeptics).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_domestic_constituencies_on_both_sides).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% France, Germany, the UK, and the EU jointly administer the graduated compliance mechanism (including the Dispute Resolution Mechanism), calibrating snapback triggers to measured enrichment levels rather than treating any violation as automatically fatal to the deal. They set the tempo of the ratchet and absorb diplomatic cost when either side accuses them of being too lenient or too harsh.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, e3_eu_coordinators, agenda_setter,
    institutional, generational, constrained, continental).

% Accepts intrusive verification and enrichment caps in exchange for calibrated sanctions relief, with the understanding that relief scales down (not off) if compliance slips, rather than collapsing entirely. Bears the cost of the ratchet directly through economic contraction when relief is partially withdrawn, but retains a path back to fuller relief through renewed compliance — the graduated design is its main incentive to stay inside the framework at all.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_government, beneficiary).

% Absorbs the real-world consequences of every graduated snapback — currency depreciation, medical supply shortages, employment contraction — regardless of which government decision triggered it. Has no seat in the compliance assessment process and cannot influence the pace of the ratchet in either direction.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_civilian_economy, payer,
    powerless, biographical, trapped, national).

% European and Asian firms positioned to re-enter Iranian markets under partial relief windows. They benefit specifically from the graduated design because it creates predictable, incremental re-engagement opportunities rather than an all-or-nothing legal cliff; a binding or transactional-voidable reading would make their investment planning far riskier.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, sanctions_relief_dependent_economic_actors, beneficiary,
    moderate, biographical, constrained, regional).

% Conducts the inspections and compliance reporting that make graduated assessment technically possible. Its institutional relevance and funding are reinforced by being the trusted, proportionality-producing measurement body — a role that only exists because the framework is scaled rather than binary.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_apparatus, agenda_setter).

% View graduated, proportional enforcement as functionally indistinguishable from tolerating incremental violations. They are structurally outside the E3/EU-run assessment process and can only object through domestic legislation or unilateral sanctions reimposition, which the graduated framework treats as disruptive noise rather than a legitimate compliance signal.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, us_congress_hardliners, excluded,
    powerful, biographical, mobile, national).

% Gulf states and Israel bear the security externality of any enrichment increase tolerated under the graduated tiers, without being party to the compliance assessment or having a vote on where the tolerance thresholds sit. A framework that treats moderate violations as manageable rather than dealbreaking directly discounts their stated security concerns.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, regional_proliferation_skeptics, payer,
    moderate, generational, constrained, regional).

% Political factions in both Tehran and Washington whose narratives require either full capitulation or full defiance; a proportional, de-escalation-oriented framework denies them the clean rupture or clean victory their domestic politics run on, costing them rhetorical ground each time the ratchet holds rather than breaks.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, hardline_domestic_constituencies_on_both_sides, payer,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for continued nuclear restraint and continued economic engagement to coexist despite intermittent, partial violations on either side — allowing both parties to remain inside the arrangement through compliance dips that would sink a binary treaty.
% TRANSFER_FUNCTION: Moves calibrated sanctions relief and market access toward Iran in proportion to verified enrichment restraint, and withdraws it in proportion to verified violation; the calibration work itself is performed by E3/EU coordinators and the IAEA, whose institutional standing is reinforced by administering the scale.
% ABSENT_VOICES: Iranian civilians bear the material consequences of every ratchet movement without a seat in the assessment; regional security-skeptic states bear externalities from tolerated violation tiers without a vote on where the tiers are set; hardliners on both sides are procedurally excluded because the framework is designed precisely to route around all-or-nothing demands.
% DISAPPEARANCE_RATIONALE: If the graduated mechanism vanished, both sides would be forced back to a binary posture — either full compliance/full relief or full breach/full sanctions — collapsing the incremental re-engagement market, ending the IAEA's proportionality-producing verification role, and handing immediate advantage to hardline constituencies on both sides who prefer rupture to a managed scale.
% FOUNDING_PROBLEM: Neither side trusted the other enough for either full upfront relief or full upfront denuclearization; a purely binary compliance standard would have collapsed at the first inspection dispute, so the parties needed a way to keep the arrangement alive through partial, ambiguous, or contested violations.
% FOUNDING_PROBLEM_CORROBORATION: E3/EU coordinators and IAEA officials attest the graduated design remains necessary given persistent partial-compliance ambiguity. Independent nonproliferation analysts outside the administering parties (e.g. arms-control research institutes) corroborate that binary compliance standards have historically triggered earlier framework collapse in comparable cases, but also note the scale itself has become a vehicle for indefinite deferral of a clear compliance verdict rather than a temporary bridge.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) reflecting genuine coordination value offset by real costs concentrated on non-consenting parties (Iranian civilians, regional skeptics). Suppression is comparatively low (0.38) because the graduated design explicitly preserves exit-adjacent options for both principal parties — this is what distinguishes it from the binding reading, where suppression would be much higher (near-total foreclosure of unilateral exit). Theater ratio rises modestly over the interval (0.15 to 0.30) as the calibration apparatus increasingly substitutes procedural compliance-scoring theater for a clear compliance verdict — an early-warning signal that the graduated mechanism, meant to bridge trust gaps temporarily, is drifting toward indefinite deferral.
 *
 * DIRECTIONALITY LOGIC:
 *   E3/EU coordinators and the IAEA sit near the beneficiary end: they administer the scale and their institutional relevance is reinforced by its continued operation. Iranian civilians and regional security-skeptics sit near the target end: trapped or constrained exit, no calibration voice, direct exposure to ratchet consequences. The Iranian government and sanctions-dependent economic actors occupy a genuinely mixed position — real benefit from incremental relief windows, real cost from incremental withdrawal — which the graduated design is specifically built to produce rather than resolve.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (mutual distrust too deep for either full upfront relief or full upfront denuclearization) remains partially live, but the corroboration trail shows independent analysts increasingly reading the graduated scale as a mechanism for indefinite deferral of a clear compliance verdict rather than a temporary bridge to one. The rising theater_ratio is the diagnostic signal: proportionality-scoring is beginning to substitute for resolution rather than enabling it. This is exactly the mandatrophy pattern the classification exists to catch — a coordination mechanism whose sunset condition (a clear compliance verdict) never arrives because the mechanism's own administrators benefit from its perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduated_scale_vs_binary_treaty_framing,
    'Is the JCPOA''s actual operative logic a genuinely scaled, proportional-response system, or is the graduated language a diplomatic gloss over what is structurally still a binding-or-broken treaty that participants are choosing to interpret flexibly for political convenience?',
    'Examine whether snapback triggers have ever actually been applied proportionally in practice (partial relief withdrawal matching partial violation) versus applied as de facto binary decisions dressed in proportional language; review E3/EU internal deliberation records where available.',
    'If the graduated mechanism is substantively binary in practice, this story''s claimed_type and metrics describe a reading that doesn''t match operative behavior, and the constraint is closer to the binding_multilateral_reading in practice despite different formal design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduated_scale_vs_binary_treaty_framing, conceptual, 'Whether graduated/proportional framing reflects real operative logic or diplomatic cover for binary decisions.').

omega_variable(
    iaea_institutional_capture_of_calibration,
    'Does the IAEA''s institutional interest in remaining the indispensable proportionality-measurement body create pressure to keep the compliance question permanently open rather than resolved, independent of the actual state of Iranian enrichment?',
    'Compare IAEA reporting language and inspection frequency trends against independent enrichment-monitoring data from non-JCPOA-affiliated sources over the same period; look for asymmetric caution in resolving ambiguous cases toward continued ambiguity.',
    'If institutional self-interest is shaping the calibration, part of the measured coordination value is actually captured value accruing to the verification apparatus itself, which would push the classification toward a more purely extractive read for that seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(iaea_institutional_capture_of_calibration, empirical, 'Whether IAEA institutional incentives bias the compliance scale toward permanent ambiguity.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the diplomatic record specifically justifies reading the JCPOA as a graduated/proportional commitment rather than as the binding multilateral treaty its text formally establishes, or as the unilaterally-voidable framework the 2018 US withdrawal treated it as?',
    'The choice of this reading over its siblings was guided by the Dispute Resolution Mechanism''s design (calibrated, staged escalation rather than binary breach-and-terminate) and by observed E3/EU practice of partial, incremental relief adjustments rather than all-or-nothing responses to Iranian enrichment increases. An alternative framing emphasizing the formal UNSC Resolution 2231 endorsement would support the binding_multilateral_reading instead; a framing emphasizing the US unilateral 2018 exit would support the transactional_provisional_reading instead.',
    'Different readings assign the same underlying diplomatic history to structurally distinct constraints with different ε, different suppression, and different victim sets; the graduated reading is defensible on DRM design and E3/EU practice, but the formal treaty text and the 2018 precedent are live counter-evidence for the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, conceptual, 'Documents the framing choice underlying this reading''s selection over its two siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(jcpo_tr_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 12, 0.27).
narrative_ontology:measurement(jcpo_tr_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(jcpo_tr_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 20, 0.3).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.32).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement(jcpo_be_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 12, 0.4).
narrative_ontology:measurement(jcpo_be_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 16, 0.41).
narrative_ontology:measurement(jcpo_be_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 20, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.34).
narrative_ontology:measurement(jcpo_su_t12, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 12, 0.36).
narrative_ontology:measurement(jcpo_su_t16, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 16, 0.37).
narrative_ontology:measurement(jcpo_su_t20, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__graduated_compliance_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__graduated_compliance_reading, iran_sanctions_regime_calibration).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposing the natural-language concept 'JCPOA bindingness' per the epsilon-invariance principle: binding_multilateral_reading (treaty-law framing, high suppression, near-mountain from the legal-formalist seat), graduated_compliance_reading (this file — proportional enforcement, moderate ε, tangled_rope), and transactional_provisional_reading (unilateral-voidability framing, low suppression, high volatility). Each carries its own ε and classification; they are linked here rather than merged because merging would violate epsilon-invariance — measuring the same diplomatic history through different bindingness assumptions yields incommensurable extraction values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
