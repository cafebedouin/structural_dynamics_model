% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Meaning
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the living constitutionalist reading of the
 *   contested kernel over what the U.S. Constitution means: that its
 *   principles (liberty, equal protection, due process) persist across time
 *   while their concrete application is understood to evolve with social
 *   attitudes, scientific knowledge, and circumstances the ratifying
 *   generation did not anticipate. This is one of three structurally distinct
 *   readings of the same kernel text — the originalist reading (meaning fixed
 *   at ratification) and the positivist reading (validity from enactment
 *   procedure alone, independent of moral content) are separate constraints
 *   with their own ε values, not alternate measurements of this one. The
 *   living constitutionalist reading is authored here as a tangled rope: it
 *   performs a genuine coordination function (keeping a near-unamendable text
 *   workable across centuries) while creating an asymmetric extraction —
 *   legislative majorities and originalist litigants pay in predictability
 *   and democratic finality what expanding rights claimants and the judiciary
 *   itself gain in interpretive latitude and protective coverage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.38).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.32).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Meaning").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '5e2efb1c-d7b6-409d-9f3c-53a318601402').
narrative_ontology:cs_kernel_codification('5e2efb1c-d7b6-409d-9f3c-53a318601402', fixed_text).
narrative_ontology:cs_authority_grounding('5e2efb1c-d7b6-409d-9f3c-53a318601402', lineage).
narrative_ontology:cs_interpretation_layer_present('5e2efb1c-d7b6-409d-9f3c-53a318601402').
narrative_ontology:cs_reading_relation('5e2efb1c-d7b6-409d-9f3c-53a318601402', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5e2efb1c-d7b6-409d-9f3c-53a318601402', us_constitution_meaning__positivist_reading, influences).
narrative_ontology:cs_axiom('5e2efb1c-d7b6-409d-9f3c-53a318601402', foundational, principle_application_evolves_with_circumstance).
narrative_ontology:cs_axiom_status(principle_application_evolves_with_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('5e2efb1c-d7b6-409d-9f3c-53a318601402', principle_application_evolves_with_circumstance, conventional).
narrative_ontology:cs_axiom('5e2efb1c-d7b6-409d-9f3c-53a318601402', foundational, contemporary_moral_consensus_is_legitimate_interpretive_input).
narrative_ontology:cs_axiom_status(contemporary_moral_consensus_is_legitimate_interpretive_input, holdable).
narrative_ontology:cs_axiom_grounding('5e2efb1c-d7b6-409d-9f3c-53a318601402', contemporary_moral_consensus_is_legitimate_interpretive_input, instrumental).
narrative_ontology:cs_reference_frame('5e2efb1c-d7b6-409d-9f3c-53a318601402', textual_principle_continuity).
narrative_ontology:cs_drift_state('5e2efb1c-d7b6-409d-9f3c-53a318601402', post_warren_court_consolidation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5e2efb1c-d7b6-409d-9f3c-53a318601402', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, originalist_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, future_generations).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, future_generations).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__living_constitutionalist_reading, evolving_standards_of_decency_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets constitutional text by identifying the enduring principle behind a clause and applying it to contemporary circumstances the framers could not have anticipated. This grants judges interpretive latitude to expand or contract the practical meaning of rights provisions across generations without formal amendment. The judiciary's own institutional authority and relevance are enlarged by retaining this discretion.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Groups whose claims to protection (based on sexuality, reproductive autonomy, criminal procedure, technological privacy) did not exist or were not contemplated at ratification. They benefit directly when courts read enduring principles (equal protection, due process, liberty) as applying to their circumstances despite the absence of an originalist textual anchor. They have no alternative forum if this reading is rejected — legislative change is often blocked by the same majorities the constitutional claim seeks to overcome.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_in_evolving_social_contexts, beneficiary,
    powerless, biographical, trapped, national).

% Enact statutes reflecting current majority preferences, which can be invalidated or superseded when courts find those preferences in tension with an evolved reading of constitutional principle. Their remedy is a constitutional amendment, an extraordinarily high bar, or waiting for judicial composition to change. They bear the cost of having their democratically-produced policy choices overridden by an interpretive method they did not consent to and cannot easily reverse.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legislative_majorities, payer,
    organized, biographical, constrained, national).

% Parties who litigate on the theory that constitutional text carries a fixed historical meaning find that meaning contestable and displaceable by an evolving-principle argument on the other side. Their predictive confidence in outcomes is degraded because the applicable standard can shift with the composition of the bench and prevailing social consensus rather than remaining anchored to a stable historical referent.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_litigants, payer,
    moderate, biographical, constrained, national).

% Inherit whichever interpretive trajectory the judiciary sets today. They benefit if the doctrine keeps the Constitution responsive to unforeseen circumstances (climate, technology, demographic change); they pay if today's court entrenches an idiosyncratic reading of 'contemporary consensus' that later generations find as constraining as any fixed original meaning, without the amendment process ever being invoked.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__living_constitutionalist_reading, future_generations, payer).

% Study and critique the doctrine's application across decades, documenting where 'evolving standards' language has tracked genuine social consensus versus where it has substituted judicial preference for consensus that was contested or absent. Their analysis is often cited by both defenders and critics of the doctrine without resolving the underlying dispute.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a 234-year-old founding text to remain a workable governing instrument for circumstances the drafters could not foresee (electronic surveillance, in vitro fertilization, corporate personhood in digital markets) without requiring the near-impossible supermajority coordination of a formal amendment for every adaptation.
% TRANSFER_FUNCTION: Moves interpretive authority over the practical content of constitutional rights from legislatures and the amendment process to the federal judiciary, and moves protective coverage toward claimant groups whose circumstances postdate ratification, at the expense of majorities whose statutory preferences are displaced by the reinterpreted principle.
% ABSENT_VOICES: The framers themselves cannot testify to whether they intended their principles to be read this expansively; legislative majorities whose enactments are invalidated under this method have no direct voice in the interpretive act itself — their remedy runs only through subsequent appointments or the amendment process, both slow and uncertain.
% DISAPPEARANCE_RATIONALE: If living constitutionalism were abandoned entirely in favor of a fixed-meaning-only regime, an entire body of doctrine recognizing rights not enumerated or contemplated at ratification (contraceptive privacy, same-sex marriage, incorporation of many procedural protections against the states) would lose its interpretive foundation, forcing those questions back into the amendment process or into legislatures where the constitutional claimants are frequently structural minorities.
% FOUNDING_PROBLEM: A written constitution with a supermajority amendment threshold will, over centuries, encounter circumstances and moral questions its drafters never addressed; a purely fixed-meaning approach risks either freezing protections at 18th- or 19th-century social assumptions or forcing constant recourse to an amendment process that is functionally near-impossible to invoke on contested social questions.
% FOUNDING_PROBLEM_CORROBORATION: Sitting justices across the ideological spectrum, including originalist-aligned jurists, acknowledge some interpretive adaptation is unavoidable even while disputing its proper scope; independent political scientists studying the amendment process document its near-total disuse since the mid-20th century as external corroboration that the founding problem (an unamendable-in-practice text meeting new circumstances) remains structurally live outside the judiciary's own self-interested account of its role.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) reflects genuine but bounded transfer: the doctrine redistributes interpretive authority toward courts and protective coverage toward previously unprotected claimants, but it operates within textual and precedential constraints, not unbounded discretion. Suppression (0.32) is moderate — legislative majorities retain the amendment path and appointment power as (slow) counter-levers, so alternatives are not fully foreclosed, only heavily disfavored by procedural cost. Theater ratio (0.22) is low-moderate: most 'evolving standards' opinions do real interpretive work grounded in doctrine and precedent, though a rising share since the 1980s reflects results-oriented reasoning dressed in consensus language. Accessibility collapse (0.35) is moderate: originalism remains a live, practiced alternative methodology on the bench, so the living reading has not foreclosed its rival — this is itself evidence for coexists_with rather than forecloses in the reading-relations below. Resistance (0.58) is substantial: originalist scholarship, legislative pushback, and a multi-decade judicial appointments strategy specifically targeting this doctrine constitute active, organized resistance, which is exactly what a contested kernel reading should show.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary sits as both agenda-setter and beneficiary: it administers the doctrine and its institutional discretion and relevance expand under this reading. Rights claimants in evolving contexts are the clearest beneficiaries — the doctrine is often their only viable path to protection absent a legislature willing to act. Legislative majorities and originalist litigants are payers: their policy choices and predictive legal expectations are the resource the doctrine can override. Future generations are dual-positioned, beneficiaries of continued adaptability but also potential payers if today's 'consensus' reading calcifies into tomorrow's unexamined orthodoxy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an unamendable-in-practice text meeting unforeseen circumstances) remains genuinely live — the amendment process has been used only once in over half a century for a substantive rights question — so this is not a classic mandatrophy case of a dead mandate persisting by inertia. The tangled_rope classification itself performs the mandatrophy-prevention function here: it refuses to let the genuine coordination need (adaptability) be treated as sufficient justification for ignoring the asymmetric extraction (a stable class of payers whose democratic outputs are made contingent on judicial reading), while also refusing to let the extraction be treated as sufficient grounds to dismiss the coordination need as pure pretext.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principle_vs_preference_line,
    'When a court identifies an ''enduring principle'' beneath a clause and applies it to new circumstances, is it discovering something latent in the text''s original commitment, or importing contemporary judicial policy preference under textual cover?',
    'Comparative doctrinal analysis tracing whether ''evolving standards'' rulings correlate more strongly with genuine, externally-measurable shifts in broad social consensus (state legislative counts, polling over multi-decade windows) or with the ideological composition of the deciding court at the time of decision.',
    'If rulings track externally measurable consensus shifts, the coordination function is well-supported and extraction is more bounded; if rulings track court composition more than measured social consensus, the doctrine functions closer to unbounded judicial policymaking and the extraction component dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_vs_preference_line, empirical, 'Whether evolving-standards reasoning tracks genuine social consensus or judicial preference.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the underlying constitutional kernel genuinely ambiguous as between the living, originalist, and positivist readings, or does the text itself resolve toward one reading that the others depart from for extrinsic (political, institutional) reasons?',
    'This is a jurisprudential/conceptual question not resolvable by empirical measurement alone; it would require settling deep questions in legal philosophy about the nature of textual meaning and constitutional authority that the discipline has not resolved after two centuries of dispute.',
    'If the kernel is genuinely indeterminate, all three readings are equally legitimate contenders and their comparative extraction/suppression profiles are the primary basis for evaluation; if the text resolves toward one reading, the others carry an additional burden of justification beyond structural analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the constitutional kernel is genuinely ambiguous across the three declared readings or resolves toward one.').

omega_variable(
    counter_majoritarian_risk_measurement,
    'How should the ''victim'' status of legislative majorities be weighed against the ''victim'' status of rights claimants who would be unprotected under alternative readings, given that both cannot be simultaneously minimized?',
    'No empirical resolution exists; this is a preference-laden question about how much counter-majoritarian risk a constitutional system should tolerate in exchange for protecting minority rights claims unanticipated by the original text — a foundational, unresolved question in democratic theory.',
    'Different weightings would not change the structural facts (who benefits, who pays) but would change the normative evaluation of whether the tangled_rope''s extraction is justified or excessive relative to its coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counter_majoritarian_risk_measurement, preference, 'Normative weighting between counter-majoritarian risk and unenumerated rights protection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 1954, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1954, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(us_c_tr_t1968, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1968, 0.14).
narrative_ontology:measurement(us_c_tr_t1985, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(us_c_tr_t2003, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(us_c_tr_t2015, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(us_c_tr_t2026, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1954, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(us_c_be_t1968, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1968, 0.34).
narrative_ontology:measurement(us_c_be_t1985, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 1985, 0.4).
narrative_ontology:measurement(us_c_be_t2003, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2003, 0.42).
narrative_ontology:measurement(us_c_be_t2015, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2015, 0.36).
narrative_ontology:measurement(us_c_be_t2026, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1954, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1954, 0.22).
narrative_ontology:measurement(us_c_su_t1968, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1968, 0.26).
narrative_ontology:measurement(us_c_su_t1985, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement(us_c_su_t2003, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2003, 0.29).
narrative_ontology:measurement(us_c_su_t2015, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2015, 0.31).
narrative_ontology:measurement(us_c_su_t2026, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 2026, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_meaning__living_constitutionalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kernel us_constitution_meaning, decomposed per the ε-invariance principle because the natural-language label 'constitutional meaning' conflates structurally distinct claims about where interpretive authority sits and what inputs are legitimate. The living_constitutionalist_reading (this file) treats contemporary consensus as a legitimate application-level input while enduring principles remain fixed; originalist_reading treats meaning itself as fixed at ratification; positivist_reading treats validity as flowing from enactment procedure independent of moral content. Each has distinct beneficiary/victim structure and distinct ε — they are linked here, not merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
