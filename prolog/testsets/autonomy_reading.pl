% ============================================================================
% CONSTRAINT STORY: autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_autonomy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: autonomy_reading
 *   human_readable: Bodily Autonomy Reading of the Personhood Boundary Kernel
 *   domain: moral_philosophy/metaethics/reproductive_rights
 *
 * SUMMARY:
 *   This story instantiates the autonomy reading of the
 *   personhood_boundary_kernel: within the relevant developmental window, the
 *   embryo is treated as essentially part of the mother's body and the
 *   mother's uncoerced decision-right controls. This is one of three
 *   structurally distinct readings of a single contested kernel — the moral
 *   status of the developing embryo/fetus relative to the pregnant woman's
 *   rights. The sibling readings (golden_rule_consistency_reading,
 *   personhood_continuity_reading) are separate constraint stories with their
 *   own ε, their own beneficiary/victim sets, and their own classification;
 *   they are not alternative measurements of this constraint but different
 *   constraints entirely. This story's ε is stable and low because, on its
 *   own terms, the coordination function (a clean decision-authority rule
 *   avoiding case-by-case metaphysical adjudication) dominates and there is
 *   no victim group internal to the reading's own premises — the embryo/fetus
 *   is not a rights-holder under this reading, so no extraction from it is
 *   authored here.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(autonomy_reading, 0.28).
domain_priors:suppression_score(autonomy_reading, 0.42).
domain_priors:theater_ratio(autonomy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(autonomy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(autonomy_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(autonomy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(autonomy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(autonomy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(autonomy_reading, rope).
narrative_ontology:human_readable(autonomy_reading, "Bodily Autonomy Reading of the Personhood Boundary Kernel").
narrative_ontology:topic_domain(autonomy_reading, "moral_philosophy/metaethics/reproductive_rights").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(autonomy_reading, '7775588e-a8f1-4137-adbd-5e146b6c22e0').
narrative_ontology:cs_kernel_codification('7775588e-a8f1-4137-adbd-5e146b6c22e0', distributed).
narrative_ontology:cs_authority_grounding('7775588e-a8f1-4137-adbd-5e146b6c22e0', distributed).
narrative_ontology:cs_reading_relation('7775588e-a8f1-4137-adbd-5e146b6c22e0', personhood_boundary_kernel__golden_rule_consistency_reading, coexists_with).
narrative_ontology:cs_reading_relation('7775588e-a8f1-4137-adbd-5e146b6c22e0', personhood_boundary_kernel__personhood_continuity_reading, forecloses).
narrative_ontology:cs_axiom('7775588e-a8f1-4137-adbd-5e146b6c22e0', foundational, bodily_autonomy_is_prior_to_third_party_claims).
narrative_ontology:cs_axiom_status(bodily_autonomy_is_prior_to_third_party_claims, holdable).
narrative_ontology:cs_axiom_grounding('7775588e-a8f1-4137-adbd-5e146b6c22e0', bodily_autonomy_is_prior_to_third_party_claims, deontological).
narrative_ontology:cs_axiom('7775588e-a8f1-4137-adbd-5e146b6c22e0', foundational, embryo_lacks_independent_moral_standing_in_relevant_window).
narrative_ontology:cs_axiom_status(embryo_lacks_independent_moral_standing_in_relevant_window, holdable).
narrative_ontology:cs_axiom_grounding('7775588e-a8f1-4137-adbd-5e146b6c22e0', embryo_lacks_independent_moral_standing_in_relevant_window, conventional).
narrative_ontology:cs_reference_frame('7775588e-a8f1-4137-adbd-5e146b6c22e0', liberal_bodily_sovereignty_framework).
narrative_ontology:cs_drift_state('7775588e-a8f1-4137-adbd-5e146b6c22e0', post_dobbs_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('7775588e-a8f1-4137-adbd-5e146b6c22e0', '').
narrative_ontology:cs_kernel_id(autonomy_reading, personhood_boundary_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(autonomy_reading, pregnant_women).
narrative_ontology:constraint_beneficiary(autonomy_reading, reproductive_autonomy_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(autonomy_reading, physicians_and_clinics).
narrative_ontology:constraint_victim(autonomy_reading, physicians_and_clinics).
narrative_ontology:constraint_vindicates(autonomy_reading, bodily_autonomy_doctrine).
narrative_ontology:constraint_vindicates(autonomy_reading, uncoerced_decision_primacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the decision-right over continuing or ending a pregnancy under this reading because the embryo/fetus is treated as not yet possessing an independent rights claim that could override the woman's control of her own body. Where this reading is legally instantiated, she can act on the decision without needing to justify it against a competing rights-holder; where it is not instantiated, she faces legal or social barriers to the same decision.
narrative_ontology:constraint_stakeholder(autonomy_reading, pregnant_women, beneficiary,
    moderate, biographical, constrained, national).

% Advocate for and help codify this reading into law and medical practice, arguing from bodily autonomy and non-coercion principles. They set the terms of the legal and clinical framework under which the decision-right operates, and shift jurisdictions and venues where the reading is legally unavailable.
narrative_ontology:constraint_stakeholder(autonomy_reading, reproductive_autonomy_movements, agenda_setter,
    organized, generational, mobile, national).

% Operate within the legal window this reading opens, providing procedures without needing to adjudicate fetal personhood claims themselves. They also bear the cost of political and sometimes physical targeting where the reading is contested, and face professional risk when jurisdictions revert to a different reading.
narrative_ontology:constraint_stakeholder(autonomy_reading, physicians_and_clinics, beneficiary,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(autonomy_reading, physicians_and_clinics, payer).

% Hold that moral status attaches at conception and object that this reading excludes the embryo from the moral community during exactly the window where, on their view, protection matters most. They are not silenced but are structurally excluded from this reading's own internal premises — the reading's foundational claim is constructed so their objection cannot be adjudicated from inside it.
narrative_ontology:constraint_stakeholder(autonomy_reading, personhood_continuity_advocates, excluded,
    organized, civilizational, mobile, national).

% Has no independent standing within this reading during the relevant developmental window; its interests, if any, are subsumed under the woman's decision-right rather than weighed as a separate claim. Listed for narrative completeness as a non-agent entity whose moral status is precisely what this reading and its siblings dispute; not treated as a rights-bearing party under this reading's own terms.
narrative_ontology:constraint_stakeholder(autonomy_reading, the_embryo_fetus, excluded,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_non_agent(autonomy_reading, the_embryo_fetus).

% Adjudicate which reading of the personhood boundary kernel becomes codified law in a given jurisdiction, hearing arguments grounded in this reading alongside its siblings and setting the enforceable window in which the decision-right operates.
narrative_ontology:constraint_stakeholder(autonomy_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, generalizable principle — bodily autonomy and freedom from coercion over one's own body — that resolves who has final decision authority over a pregnancy without requiring case-by-case adjudication of contested metaphysical claims about fetal moral status.
% TRANSFER_FUNCTION: Moves final decision authority over continuing or ending a pregnancy to the pregnant woman alone during the relevant window, and correspondingly withholds independent decision-standing from the embryo/fetus and from third parties (partners, state, physicians) who might otherwise claim a veto.
% ABSENT_VOICES: Personhood-continuity advocates would object that the reading excludes a party (the embryo) that their own framework treats as the primary rights-holder; they are present in public debate but structurally excluded from adjudication within this reading's internal premises. The embryo itself, being a non-agent under this reading, has no voice by construction.
% DISAPPEARANCE_RATIONALE: If this reading vanished as a live legal and moral framework, decision authority over pregnancy would default to whichever sibling reading fills the vacuum (personhood_continuity or golden_rule_consistency), materially changing who can obtain a legal abortion, under what conditions, and who bears legal or social risk for the decision.
% FOUNDING_PROBLEM: Historically, women's control over their own bodies and reproductive decisions was subordinated to male guardianship, church doctrine, or state interest in population/labor supply, without regard to their consent; this reading was built to establish uncoerced self-determination over one's own body as a foundational and prior claim.
% FOUNDING_PROBLEM_CORROBORATION: Reproductive-autonomy movements and many bioethicists attest the founding problem (coerced control of women's reproductive bodies) remains live in jurisdictions restricting abortion access. Personhood-continuity advocates, arguing from outside this reading's beneficiary set, dispute that autonomy is the correct frame at all, holding instead that the founding problem this reading solves is real but secondary to an unaddressed harm to the embryo; legal historians outside either advocacy camp corroborate that guardianship-based restriction on women's bodily decisions was a documented historical practice this reading responds to.
narrative_ontology:disappearance_verdict(autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-22',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(autonomy_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(autonomy_reading_tests).
:- end_tests(autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) and drifts only slightly upward over the interval: the reading does not extract from a declared victim group under its own premises, but authored extractiveness is nonzero because implementing the reading as law does impose real costs on excluded/objecting parties (personhood-continuity advocates lose standing to prevent an outcome they consider harmful) even though those parties are not victims within THIS reading's structure. Suppression (0.42) reflects that the reading, once codified, does foreclose certain claims (third-party veto, embryo standing) rather than merely coordinating around them — this is a structural feature of adopting any one reading of a contested kernel, not evidence of coercive overhead. Resistance (0.72) is high because the reading remains one of the most actively contested claims in Western moral and legal discourse; accessibility_collapse (0.35) is moderate-low because the sibling readings remain fully live, articulated, and legally available in other jurisdictions — this is precisely NOT a mountain: alternatives have not collapsed, they compete.
 *
 * PERSPECTIVAL GAP:
 *   From the pregnant woman's seat, the reading functions as coordination: it resolves ambiguity about who decides, without requiring her to litigate metaphysics. From the personhood-continuity advocate's seat, the same reading functions as an act of exclusion that denies a rights claim they hold as foundational. The engine will compute these as structurally different experiences of the same authored constraint; this divergence is the data, not an error to be smoothed over.
 *
 * DIRECTIONALITY LOGIC:
 *   Pregnant women are the clear beneficiaries under this reading's own premises — decision authority is vested in them without a competing internal claim. Reproductive autonomy movements are agenda-setters who codify and defend the reading. Physicians occupy a dual position: beneficiaries of legal clarity but payers of professional/political risk. Personhood-continuity advocates are excluded rather than victimized in the technical sense used here — they are not extracted from BY this reading, they are foreclosed FROM WITHIN it, which is why they appear as excluded rather than as a victims-array entry; declaring them a victim would conflate this reading with the sibling reading's own victim structure, which is exactly the confusion the kernel-decomposition rule exists to prevent.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coerced control of women's reproductive bodies by external parties — retains contested but substantial vitality (founding_problem_status: contested, not dead), so this is not a case of mandatrophy in the classic sense (a mandate outliving its function). Rather, the contest is over WHICH founding problem is primary: this reading treats bodily coercion as the primary harm; the personhood_continuity sibling treats fetal death as the primary harm. Neither reading has become a hollow shell defending only its own persistence — both are live, contested claims corroborated (in different directions) by parties outside their own beneficiary sets.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the autonomy_reading''s premise diverge from personhood_continuity_reading''s premise — is the disagreement about WHEN moral status attaches, or about WHETHER bodily autonomy can ever be overridden by another party''s moral status once attached?',
    'Careful decomposition of each reading''s foundational axioms (see cs_structure.axioms in each sibling file) to locate whether the readings disagree on a factual/developmental question (timing of moral status) or a normative-priority question (which right trumps which, given attached status).',
    'If the disagreement is purely about timing, the readings could in principle converge given new developmental biology; if it is about normative priority even given identical timing facts, the readings are foreclosed against each other in a way no empirical resolution can close.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the structural disagreement between sibling kernel readings.').

omega_variable(
    autonomy_primacy_universality,
    'Does the bodily-autonomy claim this reading rests on generalize consistently to other bodily-autonomy contexts (e.g., organ conscription, mandatory medical treatment), or is it specially constructed for the pregnancy case?',
    'Cross-domain consistency check: examine whether advocates of this reading apply the same non-coercion principle with equal force in analogous bodily-autonomy disputes outside reproduction.',
    'If the principle generalizes consistently, the reading''s foundational axiom is more robust (holdable across contexts); if it is invoked selectively, the axiom''s status as a genuinely foundational (rather than ad hoc) claim weakens, which would bear on how the axiom''s grounding_type should be read.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_primacy_universality, conceptual, 'Testing whether the autonomy axiom is genuinely general or case-specific.').

omega_variable(
    embryo_moral_status_indeterminacy,
    'Is the exclusion of the embryo/fetus from rights-holder status in this reading a defensible metaethical position or a structurally convenient premise that avoids the harder question?',
    'No empirical resolution mechanism exists for this question by construction — it is a metaethical dispute about what grounds moral status, not an empirical fact about embryos. Philosophical argument and reflective-equilibrium analysis are the only available tools, and they have not converged across sixty years of active debate.',
    'If the exclusion is defensible on independent metaethical grounds (not merely convenient for this reading''s conclusion), the reading''s foundational axiom is robust; if it is merely stipulated to reach the desired conclusion, the reading is circular relative to its own stated premise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(embryo_moral_status_indeterminacy, conceptual, 'Whether embryo exclusion from rights-holder status is independently grounded or question-begging.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(autonomy_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(auto_tr_t1970, autonomy_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement_basis(auto_tr_t1970, observed).
narrative_ontology:measurement(auto_tr_t1979, autonomy_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement_basis(auto_tr_t1979, observed).
narrative_ontology:measurement(auto_tr_t1988, autonomy_reading, theater_ratio, 1988, 0.12).
narrative_ontology:measurement_basis(auto_tr_t1988, observed).
narrative_ontology:measurement(auto_tr_t1997, autonomy_reading, theater_ratio, 1997, 0.13).
narrative_ontology:measurement_basis(auto_tr_t1997, observed).
narrative_ontology:measurement(auto_tr_t2010, autonomy_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement_basis(auto_tr_t2010, observed).
narrative_ontology:measurement(auto_tr_t2025, autonomy_reading, theater_ratio, 2025, 0.15).
narrative_ontology:measurement_basis(auto_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(auto_be_t1970, autonomy_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement_basis(auto_be_t1970, observed).
narrative_ontology:measurement(auto_be_t1979, autonomy_reading, base_extractiveness, 1979, 0.2).
narrative_ontology:measurement_basis(auto_be_t1979, observed).
narrative_ontology:measurement(auto_be_t1988, autonomy_reading, base_extractiveness, 1988, 0.24).
narrative_ontology:measurement_basis(auto_be_t1988, observed).
narrative_ontology:measurement(auto_be_t1997, autonomy_reading, base_extractiveness, 1997, 0.23).
narrative_ontology:measurement_basis(auto_be_t1997, observed).
narrative_ontology:measurement(auto_be_t2010, autonomy_reading, base_extractiveness, 2010, 0.26).
narrative_ontology:measurement_basis(auto_be_t2010, observed).
narrative_ontology:measurement(auto_be_t2025, autonomy_reading, base_extractiveness, 2025, 0.28).
narrative_ontology:measurement_basis(auto_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(autonomy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(autonomy_reading, identity_coordination).
narrative_ontology:affects_constraint(autonomy_reading, golden_rule_consistency_reading).
narrative_ontology:affects_constraint(autonomy_reading, personhood_continuity_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language concept 'the abortion rights kernel' per the ε-invariance principle. autonomy_reading treats bodily autonomy as the controlling principle and yields a low-extraction, no-internal-victim structure (rope-leaning). personhood_continuity_reading treats moral status at conception as controlling and yields a structure with the embryo as a declared victim (likely tangled_rope or snare depending on enforcement). golden_rule_consistency_reading applies a reciprocity test and yields yet a third structure. Each carries its own ε; none averages or hedges across the others. All three are linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
