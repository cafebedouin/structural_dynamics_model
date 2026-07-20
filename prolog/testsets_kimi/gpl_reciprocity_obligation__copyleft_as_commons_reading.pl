% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_commons_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_commons_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_commons_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Commons Preservation)
 *   domain: software licensing / intellectual property / open source governance
 *
 * SUMMARY:
 *   The GNU General Public License's reciprocity obligation requires that
 *   distributed derivative works of GPL-licensed software be licensed under
 *   the same terms. This constraint story instantiates the 'copyleft as
 *   commons' reading of the GPL reciprocity kernel: the obligation is
 *   interpreted as institutional technology that prevents enclosure of the
 *   software commons by forcing contributors and distributors to reciprocate.
 *   Key agents include the Free Software Foundation as agenda-setter, the
 *   copyleft community as beneficiary, proprietary integrators as payers, and
 *   permissive license communities as excluded voices. The reading is
 *   contested: sibling readings frame the same license text as preserving
 *   user freedoms or as illegitimately restricting business models. The
 *   structural claim here is that the constraint coordinates a genuine
 *   commons while asymmetrically extracting from actors who would prefer
 *   proprietary enclosure.
 *
 * KEY AGENTS:
 *   - free_software_foundation: Agenda-setter (institutional/analytical) â authors and enforces the license terms
 *   - copyleft_community: Primary beneficiary (organized/mobile) â contributes to and draws from the protected commons
 *   - proprietary_integrators: Primary payer (powerful/constrained) â barred from enclosing improvements; must disclose or abstain
 *   - downstream_developers: Secondary beneficiary (moderate/mobile) â receives source code and rights without bearing direct costs
 *   - permissive_license_communities: Excluded voice (organized/mobile) â argues against reciprocity but is structurally outside the GPL commons
 *   - judicial_interpreters: Observer (institutional/analytical) â adjudicates boundary questions (derivative works, linking) without agenda-setting
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.48).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_commons_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_commons_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_commons_reading, "GPL Reciprocity Obligation (Copyleft as Commons Preservation)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_commons_reading, "software licensing / intellectual property / open source governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_commons_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_commons_reading, '86eed9ad-90ad-414c-ac55-2316b1f3738b').
narrative_ontology:cs_kernel_codification('86eed9ad-90ad-414c-ac55-2316b1f3738b', fixed_text).
narrative_ontology:cs_authority_grounding('86eed9ad-90ad-414c-ac55-2316b1f3738b', lineage).
narrative_ontology:cs_interpretation_layer_present('86eed9ad-90ad-414c-ac55-2316b1f3738b').
narrative_ontology:cs_reading_relation('86eed9ad-90ad-414c-ac55-2316b1f3738b', gpl_reciprocity_obligation__copyleft_as_freedom_reading, coexists_with).
narrative_ontology:cs_reading_relation('86eed9ad-90ad-414c-ac55-2316b1f3738b', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_axiom('86eed9ad-90ad-414c-ac55-2316b1f3738b', foundational, mandatory_reciprocity_preserves_commons_integrity).
narrative_ontology:cs_axiom_status(mandatory_reciprocity_preserves_commons_integrity, holdable).
narrative_ontology:cs_axiom_grounding('86eed9ad-90ad-414c-ac55-2316b1f3738b', mandatory_reciprocity_preserves_commons_integrity, instrumental).
narrative_ontology:cs_reference_frame('86eed9ad-90ad-414c-ac55-2316b1f3738b', reciprocal_commons_integrity).
narrative_ontology:cs_drift_state('86eed9ad-90ad-414c-ac55-2316b1f3738b', cloud_computing_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('86eed9ad-90ad-414c-ac55-2316b1f3738b', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_commons_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_community).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_commons_reading, commons_based_reciprocity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the GPL license texts, publishes interpretive guidance on derivative works and linking, funds enforcement litigation, and stewards the legal boundary of the copyleft commons. Its authority derives from continuity with the GNU project lineage since 1985.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, free_software_foundation, agenda_setter,
    institutional, generational, analytical, global).

% Developers and maintainers who choose GPL licenses for their projects, contribute code to GPL commons such as the Linux kernel and GNU toolchain, and receive in return guaranteed access to derivative works distributed by others. They accept the reciprocity obligation as a condition of participation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_community, beneficiary,
    organized, generational, mobile, global).

% Secondary developers and users who build upon or deploy GPL-licensed software, benefiting from source availability and modification rights without necessarily contributing upstream. They are subsidized by the commons but do not administer it.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, downstream_developers, beneficiary,
    moderate, biographical, mobile, global).

% Corporations and product teams seeking to incorporate GPL-licensed components into proprietary software or hardware firmware. They face the choice of releasing their source code under GPL, redesigning to avoid GPL code, or risking copyright infringement claims. They experience the obligation as a barrier to their preferred business model.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Developers and advocates around BSD, MIT, and Apache licenses who argue that mandatory reciprocity reduces adoption and corporate contribution. They are structurally excluded from GPL commons governance because their licensing philosophy rejects the reciprocity premise.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, permissive_license_communities, excluded,
    organized, generational, mobile, global).

% National courts and judges who adjudicate disputes over GPL enforceability, derivative work boundaries, and remedies for non-compliance. They interpret the fixed license text but do not set licensing policy.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_commons_reading, judicial_interpreters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_commons_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a shared software commons by preventing free-riders from capturing contributions into proprietary enclosures, ensuring that improvements to shared infrastructure remain available to all participants.
% TRANSFER_FUNCTION: Moves source code disclosure obligations and modification rights from individual downstream distributors to the commons at large, by conditioning access to existing GPL code on reciprocal contribution of derivative works.
% ABSENT_VOICES: Permissive licensing advocates argue that reciprocity stifles adoption and corporate contribution; proprietary business model proponents are structurally excluded because their desired integration practices violate the reciprocity requirement and thus the GPL commons excludes them by design.
% DISAPPEARANCE_RATIONALE: If the reciprocity obligation vanished overnight, proprietary integrators would immediately capture GPL code into closed products without disclosure; the copyleft commons would unravel as contributors defected to permissive licenses or stopped contributing, and the institutional equilibrium of shared infrastructure would shift toward enclosure.
% FOUNDING_PROBLEM: The tragedy of the commons in software: developers contributing to shared codebases saw their work appropriated into proprietary products without reciprocity, destroying incentives to maintain shared infrastructure (observed in the Unix wars and proprietary Unix era of the 1980s).
% FOUNDING_PROBLEM_CORROBORATION: Software historians and economists document the enclosure of academic and shared Unix tools into proprietary systems during the 1980s. However, scholars of the permissive ecosystem (e.g., BSD, Apache) and corporate open-source strategists contest whether reciprocity was necessary, arguing that permissive licensing achieved comparable or superior commons outcomes without mandatory extraction.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_commons_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_commons_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_commons_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.55, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_commons_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_commons_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is medium (0.55) because the reciprocity obligation genuinely forces proprietary actors to surrender enclosure rents, but the value extracted is conditional on their desire to use GPL code. Suppression is moderate (0.48): the constraint suppresses proprietary enclosure but does not suppress the existence of alternative licensing paradigms (BSD/MIT, proprietary reimplementation). Theater ratio (0.30) reflects that most GPL enforcement activity is functionally directed at preserving commons boundaries, though some high-profile litigation has performative elements. Accessibility collapse (0.45) is moderate because understanding the constraint does not eliminate alternatives (permissive licenses remain available), though it collapses the option of proprietary integration for those who have already built on GPL code. Resistance (0.52) is substantial due to decades of corporate lobbying against copyleft, anti-GPL sentiment in venture capital, and the proliferation of permissive alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The copyleft community experiences the constraint as protective coordination (low directionality, subsidized by guaranteed access to improvements), while proprietary integrators experience it as coercive extraction (high directionality, forced disclosure of proprietary derivatives). Judicial interpreters occupy a near-symmetric analytical seat where the constraint appears as a legally novel but textually determinate enforcement mechanism. The engine computes this divergence from the structural data: beneficiary declarations for the community versus victim declarations for proprietary integrators, differentiated by exit options (mobile versus constrained).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (copyleft_community, downstream_developers) have low directionality because the constraint subsidizes their access to a growing commons. Victims (proprietary_integrators) have high directionality because the constraint extracts proprietary value from them and channels it into the commons. The Free Software Foundation, despite being the agenda-setter, does not directly capture financial extraction; its authority derives from lineage and institutional mission, placing it near the symmetric-to-beneficiary end. Permissive license communities are excluded from the constraint's operation entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy mislabeling because the coordination function (commons preservation) is structurally inseparable from the extraction (forced reciprocity). It is not a pure snare because the commons genuinely grows and remains accessible; it is not a pure rope because proprietary integrators are asymmetrically harmed. The active enforcement requirement (copyright infringement litigation) is necessary to maintain the boundary against enclosure, confirming tangled_rope rather than rope. Were enforcement to atrophy, the commons would likely be enclosed, so the constraint's persistence depends on continued enforcement â consistent with tangled_rope, not piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the GPL reciprocity obligation best modeled as commons-preservation, freedom-preservation, or business-restriction â or do these framings collapse into the same structural enforcement mechanism viewed from different seats?',
    'Cross-reading comparison: if the same enforcement actions (GPL litigation) are vindicated under one reading and condemned under another, the classification is reading-dependent and the kernel must remain decomposed.',
    'If the framings are structurally indistinguishable, the three sibling constraints should merge; if they produce different directionality profiles and epsilon values, the decomposition is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the three copyleft readings are distinct constraints or observer-relative framings.').

omega_variable(
    commons_enclosure_counterfactual,
    'Would the software commons have been enclosed in the absence of GPL reciprocity, or would permissive licensing have sustained equivalent shared infrastructure?',
    'Comparative historical analysis of GPL and permissive ecosystems (Linux vs. BSD) controlling for network effects and corporate strategy.',
    'If the commons would have survived without reciprocity, the coordination function is weaker than claimed and the constraint leans toward snare; if enclosure was inevitable without reciprocity, the tangled_rope classification is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_enclosure_counterfactual, empirical, 'Counterfactual necessity of reciprocity for commons survival.').

omega_variable(
    derivative_work_boundary_ambiguity,
    'Does the legal uncertainty around what constitutes a derivative work (linking, combining, dynamic vs. static) create extraction beyond the stated commons-preservation purpose?',
    'Judicial clarification or statutory definition of derivative work in software; comparative analysis across jurisdictions.',
    'If boundary ambiguity allows over-reaching claims, effective extraction is higher than the intended institutional design; if boundaries are narrow, extraction tracks the commons-preservation intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_boundary_ambiguity, conceptual, 'Legal boundary ambiguity in derivative works.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_commons_tr_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_commons_tr_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(gpl_commons_tr_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(gpl_commons_tr_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(gpl_commons_tr_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement(gpl_commons_tr_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 25, 0.3).
narrative_ontology:measurement(gpl_commons_tr_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(gpl_commons_be_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl_commons_be_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(gpl_commons_be_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(gpl_commons_be_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(gpl_commons_be_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(gpl_commons_be_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 25, 0.54).
narrative_ontology:measurement(gpl_commons_be_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(gpl_commons_su_t0, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(gpl_commons_su_t5, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(gpl_commons_su_t10, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 10, 0.35).
narrative_ontology:measurement(gpl_commons_su_t15, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(gpl_commons_su_t20, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(gpl_commons_su_t25, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(gpl_commons_su_t30, gpl_reciprocity_obligation__copyleft_as_commons_reading, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_commons_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_commons_reading, 0.08).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_freedom_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_commons_reading, copyleft_as_restriction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gpl_reciprocity_obligation kernel. The kernel is the GPL's mandatory reciprocity clause. This reading interprets it as institutional technology preventing commons enclosure, structurally distinct from the freedom-preservation reading (which centers user liberties) and the business-restriction reading (which centers proprietary business model constraints). Each reading has a different beneficiary/victim structure and epsilon value.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
