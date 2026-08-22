% ============================================================================
% CONSTRAINT STORY: geneva_conventions_1949__humanitarian_ceiling_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_1949__humanitarian_ceiling_reading, []).

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
 *   constraint_id: geneva_conventions_1949__humanitarian_ceiling_reading
 *   human_readable: Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Reading)
 *   domain: international_humanitarian_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the humanitarian-ceiling reading of the Geneva
 *   Conventions 1949 kernel: the claim that the Conventions establish an
 *   absolute floor of humane treatment that binds a state's own conduct
 *   regardless of whether the adversary reciprocates or whether the conflict
 *   is against irregular forces. This is a distinct constraint from the
 *   conditional-reciprocity reading (which would degrade protections
 *   proportionally to adversary non-compliance) and the security-maximization
 *   reading (which would treat the Conventions as peacetime aspiration
 *   subordinate to operational necessity). All three readings share the same
 *   kernel text (the 1949 Conventions plus Additional Protocols and customary
 *   IHL) but produce structurally different victim sets, different
 *   suppression profiles, and different ε values — they are linked here via
 *   network.affects_constraints, not merged into one story.
 *
 * KEY AGENTS:
 *   - civilian_populations_in_conflict_zones: primary beneficiary (powerless/trapped) — receives protection regardless of reciprocity
 *   - detained_and_captured_persons: primary beneficiary (powerless/trapped) — protected even absent full POW status
 *   - state_military_operational_commanders: primary payer (institutional/constrained) — bears asymmetric restraint burden
 *   - international_committee_of_the_red_cross: agenda-setter and interpretive authority (institutional/analytical)
 *   - opposing_irregular_forces: excluded from formal architecture despite being central to the reciprocity question
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_1949__humanitarian_ceiling_reading, 0.42).
domain_priors:suppression_score(geneva_conventions_1949__humanitarian_ceiling_reading, 0.78).
domain_priors:theater_ratio(geneva_conventions_1949__humanitarian_ceiling_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_1949__humanitarian_ceiling_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_1949__humanitarian_ceiling_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_1949__humanitarian_ceiling_reading, "Geneva Conventions 1949 — Humanitarian Ceiling Reading (Absolute Minimums Reading)").
narrative_ontology:topic_domain(geneva_conventions_1949__humanitarian_ceiling_reading, "international_humanitarian_law/political_philosophy").

domain_priors:requires_active_enforcement(geneva_conventions_1949__humanitarian_ceiling_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_1949__humanitarian_ceiling_reading, '60c8b902-f20f-45c8-a952-07ba026ef3f3').
narrative_ontology:cs_kernel_codification('60c8b902-f20f-45c8-a952-07ba026ef3f3', formalized).
narrative_ontology:cs_authority_grounding('60c8b902-f20f-45c8-a952-07ba026ef3f3', lineage).
narrative_ontology:cs_interpretation_layer_present('60c8b902-f20f-45c8-a952-07ba026ef3f3').
narrative_ontology:cs_reading_relation('60c8b902-f20f-45c8-a952-07ba026ef3f3', geneva_conventions_1949__conditional_reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('60c8b902-f20f-45c8-a952-07ba026ef3f3', geneva_conventions_1949__security_maximization_reading, forecloses).
narrative_ontology:cs_axiom('60c8b902-f20f-45c8-a952-07ba026ef3f3', foundational, protection_attaches_to_humanity_not_reciprocal_status).
narrative_ontology:cs_axiom_status(protection_attaches_to_humanity_not_reciprocal_status, holdable).
narrative_ontology:cs_axiom_grounding('60c8b902-f20f-45c8-a952-07ba026ef3f3', protection_attaches_to_humanity_not_reciprocal_status, deontological).
narrative_ontology:cs_axiom('60c8b902-f20f-45c8-a952-07ba026ef3f3', foundational, jus_in_bello_binds_independent_of_adversary_conduct).
narrative_ontology:cs_axiom_status(jus_in_bello_binds_independent_of_adversary_conduct, holdable).
narrative_ontology:cs_axiom_grounding('60c8b902-f20f-45c8-a952-07ba026ef3f3', jus_in_bello_binds_independent_of_adversary_conduct, conventional).
narrative_ontology:cs_reference_frame('60c8b902-f20f-45c8-a952-07ba026ef3f3', post_wwii_universal_humanitarian_minimum).
narrative_ontology:cs_drift_state('60c8b902-f20f-45c8-a952-07ba026ef3f3', post_9_11_asymmetric_warfare_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('60c8b902-f20f-45c8-a952-07ba026ef3f3', '').
narrative_ontology:cs_kernel_id(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, detained_and_captured_persons).
narrative_ontology:constraint_beneficiary(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants_denied_pow_status).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders).
narrative_ontology:constraint_victim(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_force_personnel).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, human_dignity_as_non_derogable_floor).
narrative_ontology:constraint_vindicates(geneva_conventions_1949__humanitarian_ceiling_reading, jus_in_bello_independence_from_jus_ad_bellum).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live inside active theaters of war with no ability to leave the zone of danger. The ceiling reading forbids their targeting or use as leverage regardless of whether the opposing force reciprocates; they receive this protection as a floor that does not depend on their own government's or their attackers' compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, civilian_populations_in_conflict_zones, beneficiary,
    powerless, immediate, trapped, national).

% Held by a state military after capture. Under the ceiling reading they retain protection against torture, summary execution, and inhumane treatment even if classified as unlawful or irregular combatants who do not qualify for full POW status — the protection attaches to their humanity, not to a status test.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, detained_and_captured_persons, beneficiary,
    powerless, biographical, trapped, national).

% Fight without uniforms or a recognized chain of command and are therefore excluded from full Third Convention protections by most state militaries. Under the ceiling reading, Common Article 3 and customary IHL still bar their torture or extrajudicial killing — this is the exact seat the conditional-reciprocity reading would strip.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, irregular_combatants_denied_pow_status, beneficiary,
    powerless, biographical, trapped, national).

% Plan and execute operations against adversaries who frequently violate the conventions — using civilians as shields, executing captured personnel, feigning surrender. The ceiling reading requires the commander's own force to hold the line regardless of what the adversary does, absorbing tactical risk and force-protection cost that reciprocal readings would let them shed.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, state_military_operational_commanders, payer,
    institutional, immediate, constrained, national).

% Individual soldiers and officers on the ground who must apply restrictive rules of engagement against an adversary not bound by the same rules, at personal risk, and who face courts-martial or prosecution if they breach the ceiling even under battlefield pressure or provocation.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, counterinsurgency_force_personnel, payer,
    moderate, immediate, trapped, regional).

% Guardian and interpreter of the Conventions; monitors compliance, visits detainees, and issues authoritative commentary that has historically pushed interpretation toward the ceiling reading (e.g. ICRC Commentary on Common Article 3 and customary law studies). Administers the interpretive apparatus but does not itself hold coercive enforcement power.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_committee_of_the_red_cross, agenda_setter,
    institutional, civilizational, analytical, global).

% Not parties to the Conventions in the formal sense and frequently violate them without facing the same institutional consequences a state military faces. Their voice on why reciprocity should or should not condition protection is absent from the treaty-drafting and enforcement architecture, which was built by and for states.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, opposing_irregular_forces, excluded,
    moderate, immediate, mobile, national).

% Adjudicate individual criminal responsibility for grave breaches after the fact, drawing on the ceiling reading's non-reciprocal logic (e.g. ICTY jurisprudence on Common Article 3 applying to all parties regardless of conflict classification). Their doctrine feeds back into how the reading is defended in later conflicts.
narrative_ontology:constraint_stakeholder(geneva_conventions_1949__humanitarian_ceiling_reading, international_criminal_tribunals, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_1949__humanitarian_ceiling_reading, diffuse).
narrative_ontology:fixing_cost_class(geneva_conventions_1949__humanitarian_ceiling_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, universally-applicable floor of treatment that all parties can predict and rely on regardless of shifting battlefield conditions, preventing the collapse of restraint into a race to the bottom triggered by any single violation.
% TRANSFER_FUNCTION: Moves tactical latitude, intelligence-gathering options, and force-protection flexibility away from state military commanders and personnel, and moves survival, physical security, and basic dignity protections toward civilians, detainees, and irregular combatants who would otherwise be denied protection under a reciprocity or necessity test.
% ABSENT_VOICES: Opposing irregular forces who violate the Conventions are excluded from the formal treaty and enforcement architecture; their calculus for why they don't reciprocate (asymmetric capability, decentralized command, deliberate strategy) is not represented in how the ceiling reading is drafted or defended, only reacted to.
% DISAPPEARANCE_RATIONALE: If the humanitarian-ceiling reading were abandoned in favor of conditional reciprocity or security maximization, detainee treatment, civilian protection, and prosecutable war-crime standards would immediately become contingent on adversary conduct rather than fixed — the entire architecture of grave-breach prosecution, ICRC detainee visits, and non-derogable Common Article 3 protections depends on this reading holding as the operative interpretation in tribunals and military doctrine.
% FOUNDING_PROBLEM: The aftermath of WWII revealed that treating humanitarian protection as conditional on enemy reciprocity produced catastrophic collapses in restraint — reprisal logic and 'the other side started it' rationales were used to justify atrocities on all sides, and combatants captured out of uniform (partisans, resistance fighters) were executed en masse under legalistic status arguments.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC and international criminal tribunals (ICTY, ICTR) attest the founding problem remains live, citing continued reprisal-logic atrocities in asymmetric conflicts as evidence the ceiling function is still needed. State military legal advisors and security-maximization commentators — outside the direct beneficiary set — counter that the founding problem was specific to symmetric interstate war between uniformed armies and does not map cleanly onto contemporary irregular warfare, making the founding problem's continued applicability itself the live dispute rather than a settled genealogy.
narrative_ontology:disappearance_verdict(geneva_conventions_1949__humanitarian_ceiling_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_1949__humanitarian_ceiling_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(geneva_conventions_1949__humanitarian_ceiling_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_1949__humanitarian_ceiling_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).
:- end_tests(geneva_conventions_1949__humanitarian_ceiling_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at 2024) because the ceiling reading imposes a genuine, asymmetric cost on state militaries — restricted tactics, force-protection risk, prosecutorial exposure — that is not offset by any reciprocal gain when the adversary does not comply; this is a real transfer, not merely coordination cost. Suppression is high (0.78) because the reading's entire structural claim is that security rationales for relaxing protection must be actively suppressed — commanders cannot invoke adversary non-compliance to justify reciprocal degradation, and this suppression must be enforced through military law, courts-martial, and international tribunal doctrine. The theater ratio is comparatively low (0.28) — most of the apparatus (ICRC monitoring, grave-breach prosecution, detainee visitation) performs real function, though rising slightly over the interval as compliance-signaling and doctrine-development activity has grown relative to enforcement capacity, especially post-2001. Accessibility collapse is moderate-high (0.62): once the ceiling framing is accepted, the reciprocity-based alternative is largely foreclosed as a legal option for states party to the Conventions, though it persists as a live political and operational argument.
 *
 * PERSPECTIVAL GAP:
 *   From the ICRC's and tribunal's analytical seat, this reading is a hard-won coordination achievement preventing reprisal spirals — a genuine Rope-like function stabilizing restraint across all parties. From the operational commander's seat facing an adversary who ignores the Conventions entirely, the same structure computes as an enforced, asymmetric extraction of tactical latitude with no reciprocal benefit — the coordination story and the extraction reality occupy the same structure, which is why tangled_rope (not rope, not snare) is the structurally honest claim: there is a real coordination function (predictable restraint prevents systemic collapse) AND a real asymmetric cost borne by one class of party (state forces facing non-compliant adversaries), sustained only by active enforcement (military law, tribunal jurisprudence, treaty ratification pressure).
 *
 * DIRECTIONALITY LOGIC:
 *   Civilians, detainees, and irregular combatants denied POW status are beneficiaries under this reading specifically because their protection is decoupled from reciprocity — the reading's entire point is to prevent their protection from being conditioned on adversary behavior they do not control (d near the beneficiary end for these groups is a DIRECT product of the reading's logic, not an artifact of general IHL). State military commanders and counterinsurgency personnel are payers: they bear the cost of restraint precisely because the reading forbids them from degrading protection reciprocally, even when facing an adversary that executes prisoners or uses civilians as shields. This is the asymmetric burden the expected structural delta names explicitly.
 *
 * MANDATROPHY ANALYSIS:
 *   The ceiling reading's mandate — preventing atrocity justified by 'the other side started it' — remains contested as live rather than resolved-dead: tribunals and the ICRC attest it is still functioning against real reprisal-logic pressure in ongoing asymmetric conflicts (founding_problem_status: contested, not dead), which forecloses a straightforward mandatrophy verdict. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine, still-functioning coordination achievement as pure extraction merely because it imposes real, asymmetric costs on state actors — the costs are real, but so is the coordination function they purchase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reciprocity_versus_universality_kernel_ambiguity,
    'Does the 1949 Conventions kernel, properly interpreted, actually commit states to unconditional humanitarian minimums, or is the ceiling reading itself a post-hoc universalization of a text drafted primarily to regulate symmetric interstate war between uniformed armies?',
    'Comparative analysis of the travaux préparatoires (drafting history) against subsequent ICRC commentary and ICTY/ICTR jurisprudence extending Common Article 3 to non-international and asymmetric conflicts — tracking whether this extension was foreseen by drafters or is genuinely a later interpretive expansion.',
    'If the ceiling reading is a later interpretive expansion beyond original drafter intent, the security_maximization_reading''s claim that irregular warfare falls outside the kernel''s original design gains structural support, weakening this reading''s claim to be the kernel''s true content rather than one contested extension of it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_versus_universality_kernel_ambiguity, conceptual, 'Whether the humanitarian-ceiling reading reflects original kernel intent or a later interpretive universalization.').

omega_variable(
    asymmetric_burden_beneficiary_structure,
    'Is the asymmetric burden borne by state military commanders and personnel a legitimate feature of a genuine coordination achievement (restraint has to start somewhere and states, having formal command structures, can bear it), or does it function as a structural extraction that disadvantages compliant actors relative to non-compliant ones with no corrective mechanism?',
    'Longitudinal study of conflict outcomes comparing forces that maintained strict IHL compliance under this reading against forces that adopted graduated-reciprocity postures, controlling for conflict type and adversary behavior, to assess whether the ceiling reading''s asymmetry produces worse tactical outcomes without corresponding strategic or legitimacy gains.',
    'If the asymmetry produces no offsetting strategic or legitimacy benefit, the tangled_rope classification''s coordination-function claim weakens, and the constraint would look structurally closer to a snare from the state military seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_burden_beneficiary_structure, empirical, 'Whether the state-military burden purchases a genuine strategic or legitimacy return, or is uncompensated extraction.').

omega_variable(
    irregular_combatant_beneficiary_reality_gap,
    'Do irregular combatants denied POW status actually receive the Common Article 3 protections this reading claims for them in practice, or does the ceiling reading''s theoretical universality mask widespread non-enforcement against exactly this population (e.g. extraordinary rendition, targeted killing programs, indefinite detention outside courts-martial oversight)?',
    'Cross-reference ICRC detainee-access records and documented cases of irregular-combatant treatment against the formal legal standard, tracking the gap between the reading''s stated protection and its enforced protection across multiple asymmetric conflicts (e.g. post-2001 detention practices).',
    'A wide enforcement gap would indicate the ceiling reading functions partly as theater for this beneficiary group specifically — raising the effective theater_ratio for that seat above the story-level average and suggesting the beneficiary declaration for irregular_combatants_denied_pow_status overstates actual benefit received.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irregular_combatant_beneficiary_reality_gap, empirical, 'Gap between the ceiling reading''s claimed protection of irregular combatants and documented enforcement practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_1949__humanitarian_ceiling_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1949, 0.12).
narrative_ontology:measurement(gene_tr_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(gene_tr_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(gene_tr_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1949, 0.22).
narrative_ontology:measurement(gene_be_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(gene_be_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 1990, 0.31).
narrative_ontology:measurement(gene_be_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2001, 0.36).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(gene_su_t1970, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1970, 0.62).
narrative_ontology:measurement(gene_su_t1990, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(gene_su_t2001, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2001, 0.72).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2010, 0.76).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_1949__humanitarian_ceiling_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_1949__humanitarian_ceiling_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(geneva_conventions_1949__humanitarian_ceiling_reading, 0.12).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__conditional_reciprocity_reading).
narrative_ontology:affects_constraint(geneva_conventions_1949__humanitarian_ceiling_reading, geneva_conventions_1949__security_maximization_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the geneva_conventions_1949 kernel. The humanitarian_ceiling_reading (this file) authors ε=0.42, tangled_rope, with irregular combatants and civilians as beneficiaries and state military personnel as payers. The conditional_reciprocity_reading authors a different beneficiary/victim structure (protections that degrade with adversary non-compliance, shifting cost toward civilians in low-compliance conflicts) and likely a different ε. The security_maximization_reading treats the Conventions as subordinate to operational necessity and would author the lowest ε for state actors and a correspondingly higher one for civilian/detainee populations. Each reading is a separate constraint per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
