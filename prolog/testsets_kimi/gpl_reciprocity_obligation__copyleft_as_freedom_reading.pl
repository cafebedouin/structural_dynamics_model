% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: legal/software_governance
 *
 * SUMMARY:
 *   The GNU General Public License's copyleft clause requires distributors of
 *   derivative works to offer corresponding source code under the same
 *   license terms. In the copyleft-as-freedom reading, this reciprocity
 *   obligation is framed as a defensive mechanism that preserves downstream
 *   user autonomy by preventing proprietary capture of free software.
 *   Proprietary integrators who will not reciprocate are structurally
 *   excluded from incorporating copylefted code, while downstream users
 *   receive a guarantee that their freedoms to run, study, modify, and share
 *   will travel with the software. The constraint coordinates a planetary
 *   software commons while asymmetrically extracting compliance costs and
 *   business-model restrictions from actors seeking to enclose that commons.
 *
 * KEY AGENTS:
 *   - downstream_users: primary beneficiary (powerless/mobile) â receives guaranteed source and modification rights
 *   - proprietary_integrators: primary target/victim (powerful/constrained) â bears obligation to disclose source or forgo use
 *   - copyleft_licensors: agenda_setter (organized/analytical) â selects GPL and enforces terms to prevent capture
 *   - judiciary: observer (institutional/analytical) â adjudicates derivative-work boundaries and license violations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.78).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "legal/software_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'ff695506-5469-49fc-a703-6af44e003e4a').
narrative_ontology:cs_kernel_codification('ff695506-5469-49fc-a703-6af44e003e4a', formalized).
narrative_ontology:cs_authority_grounding('ff695506-5469-49fc-a703-6af44e003e4a', lineage).
narrative_ontology:cs_interpretation_layer_present('ff695506-5469-49fc-a703-6af44e003e4a').
narrative_ontology:cs_reading_relation('ff695506-5469-49fc-a703-6af44e003e4a', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('ff695506-5469-49fc-a703-6af44e003e4a', gpl_reciprocity_obligation__copyleft_as_commons_reading, influences).
narrative_ontology:cs_axiom('ff695506-5469-49fc-a703-6af44e003e4a', foundational, proprietary_capture_is_freedom_violation).
narrative_ontology:cs_axiom_status(proprietary_capture_is_freedom_violation, holdable).
narrative_ontology:cs_axiom_grounding('ff695506-5469-49fc-a703-6af44e003e4a', proprietary_capture_is_freedom_violation, deontological).
narrative_ontology:cs_axiom('ff695506-5469-49fc-a703-6af44e003e4a', foundational, reciprocal_source_rights_preserve_autonomy).
narrative_ontology:cs_axiom_status(reciprocal_source_rights_preserve_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('ff695506-5469-49fc-a703-6af44e003e4a', reciprocal_source_rights_preserve_autonomy, deontological).
narrative_ontology:cs_reference_frame('ff695506-5469-49fc-a703-6af44e003e4a', software_freedom_preservation).
narrative_ontology:cs_drift_state('ff695506-5469-49fc-a703-6af44e003e4a', contemporary_cloud_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ff695506-5469-49fc-a703-6af44e003e4a', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive software with legally guaranteed rights to run, study, modify, and distribute corresponding source code. Benefit from the inability of intermediate distributors to remove these freedoms or add proprietary restrictions. Can exit by choosing non-copyleft software, but within the GPL ecosystem they are the structurally protected party.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    powerless, biographical, mobile, global).

% Corporations and developers seeking to integrate GPL-licensed code into proprietary products. They face the choice of complying with source disclosure and reciprocal licensing requirements, forgoing the integration entirely, or risking copyright infringement litigation. Their preferred business model of proprietary enclosure is directly constrained by the license terms.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Original copyright holders and the Free Software Foundation who select GPL licenses and enforce their terms. They set the legal conditions under which code may be used, distributed, and modified, with the explicit intent of preventing proprietary capture and preserving downstream user freedoms. They do not typically extract financial rents but advance an ideological and institutional commitment.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_licensors, agenda_setter,
    organized, generational, analytical, global).

% Courts and legal systems that adjudicate GPL enforcement cases, interpret what constitutes a derivative work, and determine whether license violations have occurred. They do not set the license terms but authoritatively determine their legal effect within their respective jurisdictions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a global software commons by ensuring that downstream recipients of code cannot be deprived of source access and modification rights by intermediate distributors; solves the collective-action problem of contributing to shared code when proprietary capture is possible.
% TRANSFER_FUNCTION: Moves the obligation to disclose corresponding source code and license derivative works reciprocally from proprietary integrators to downstream users, and moves the legal right to modify and redistribute from licensors to all subsequent recipients.
% ABSENT_VOICES: Permissive licensing advocates (BSD/MIT proponents) who argue that reciprocity obligations impede software dissemination and that freedom is better served by non-interference; proprietary integrators who experience the constraint as a business limitation but are structurally positioned as defendants rather than participants in license design.
% DISAPPEARANCE_RATIONALE: If the GPL reciprocity obligation vanished overnight, downstream users would lose guaranteed source access to derivative works, proprietary integrators could immediately enclosure previously protected codebases, and major ecosystems like the Linux kernel would face a licensing crisis forcing rapid reorganization around alternative licenses or proprietary forks.
% FOUNDING_PROBLEM: Proprietary software vendors were capturing freely shared code, removing source access, and restricting user freedoms, creating a dynamic where contributors to shared software saw their work enclosed and users received only opaque binaries without the ability to modify or share.
% FOUNDING_PROBLEM_CORROBORATION: Early free software advocates including Richard Stallman documented specific enclosure incidents (the printer driver case). Independent historians of computing corroborate the broader pattern of 1980s proprietary Unix fragmentation and enclosure of academic software. Proprietary software vendors and some open-source advocates dispute that this problem required or was best solved by reciprocal licensing obligations.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.65) because proprietary integrators face a forced choice between revealing source code or abandoning integration, representing a real transfer of control over their derivative works. Suppression is high (0.78) because the constraint actively extinguishes proprietary integration as a legal option for GPL-covered code through copyright enforcement. Theater ratio is low-moderate (0.25): most GPL enforcement is functional and produces compliance, though some organizational compliance activity is performative. Accessibility collapse (0.58) reflects that once a codebase is GPL-licensed, proprietary capture alternatives close for that specific work. Resistance (0.52) is moderate: proprietary actors engage in legal evasion, clean-room reimplementation, and lobbying for weaker copyleft. The measurement series tracks the maturation of enforcement infrastructure from the early FSF era through the rise of compliance engineering and organizations like the Software Freedom Conservancy.
 *
 * PERSPECTIVAL GAP:
 *   The downstream user seat computes as beneficiary with low directionality; the proprietary integrator seat computes as victim with high directionality. The copyleft licensor seat computes near the beneficiary pole because its analytical exit and agenda-setting role align with the constraint's purpose. The judiciary sits at symmetric/analytical. The divergence between the proprietary integrator's high-extraction seat and the downstream user's low-extraction seat is the structural signature of the tangled rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream_users are named beneficiaries with mobile exit options (can choose permissive alternatives), producing low d and damped effective extraction. Proprietary_integrators are named victims with constrained exit (must comply or not use), producing high d and amplified extraction. Copyleft_licensors are agenda_setters with analytical exit; they are structurally subsidized by the constraint (d near 0.0) because it advances their ideological commitment rather than extracting from them. The judiciary is analytical and uninvolved in the transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The freedom reading prevents mandatrophy mislabeling by maintaining a live coordination function: downstream users genuinely retain freedoms they would lose under permissive licensing that permitted proprietary capture. Without this coordination function, the constraint would read as a pure snare (proprietary integrators pay, nobody benefits in a structurally declared way). Without the victim group, it would read as a rope. The coexistence of both â beneficiaries who receive coordination goods and victims who bear extraction costs â is exactly the tangled rope signature. The reading resists collapse into either pure extraction or pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cloud_distribution_freedom_boundary,
    'Does SaaS/cloud distribution without source release constitute proprietary capture that the GPL freedom reading fails to prevent, or a legitimate service model outside the kernel''s scope?',
    'Comparative analysis of AGPL adoption rates and cloud provider compliance behaviors; empirical study of whether SaaS users experience the same freedom loss as binary recipients.',
    'If SaaS is capture, the GPL''s freedom preservation is incomplete and the reading''s coordination claim is weaker than asserted; if not, the scope boundary is correctly drawn.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cloud_distribution_freedom_boundary, empirical, 'Whether network use constitutes freedom-violating capture under the freedom reading').

omega_variable(
    enforcement_motivation_ambiguity,
    'Is GPL enforcement primarily motivated by freedom preservation, or by reputation maintenance, competitive advantage, or institutional survival?',
    'Systematic review of enforcement case outcomes â whether they produce source release or financial settlement â and ethnographic study of enforcer motivations.',
    'If enforcement produces settlements without source release, the extraction accrues to enforcers rather than downstream users, shifting type toward snare; if source release dominates, the coordination function is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_motivation_ambiguity, empirical, 'Whether enforcement tracks freedom outcomes or extraction motives').

omega_variable(
    kernel_reading_contest,
    'This constraint is the freedom reading of a contested kernel; sibling readings reframe proprietary integrators as constrained actors rather than victims and downstream freedoms as incidental rather than primary. Which framing governs the structural classification?',
    'Cross-reading corpus comparison; engine classification divergence analysis between this story and sibling stories.',
    'If the restriction reading governs, the constraint computes as more extractive with reversed directionality; if the commons reading governs, the beneficiary set expands to the commons as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between freedom, restriction, and commons readings of the GPL kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_freedom_tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_freedom_tr_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 7, 0.13).
narrative_ontology:measurement(gpl_freedom_tr_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 14, 0.16).
narrative_ontology:measurement(gpl_freedom_tr_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 21, 0.2).
narrative_ontology:measurement(gpl_freedom_tr_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 28, 0.23).
narrative_ontology:measurement(gpl_freedom_tr_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 35, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl_freedom_be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gpl_freedom_be_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 7, 0.5).
narrative_ontology:measurement(gpl_freedom_be_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 14, 0.55).
narrative_ontology:measurement(gpl_freedom_be_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 21, 0.6).
narrative_ontology:measurement(gpl_freedom_be_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 28, 0.63).
narrative_ontology:measurement(gpl_freedom_be_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 35, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl_freedom_su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(gpl_freedom_su_t7, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(gpl_freedom_su_t14, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 14, 0.65).
narrative_ontology:measurement(gpl_freedom_su_t21, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 21, 0.7).
narrative_ontology:measurement(gpl_freedom_su_t28, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 28, 0.75).
narrative_ontology:measurement(gpl_freedom_su_t35, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 35, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_commons_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the gpl_reciprocity_obligation kernel. The copyleft_as_freedom_reading frames the GPL as preserving user autonomy against proprietary capture. Sibling readings frame it as business-model restriction or commons-enclosure prevention. Each reading produces distinct epsilon values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
