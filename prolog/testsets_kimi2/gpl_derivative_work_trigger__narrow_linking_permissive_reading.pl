% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__narrow_linking_permissive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__narrow_linking_permissive_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__narrow_linking_permissive_reading
 *   human_readable: Narrow Linking Permissive Reading of GPL Derivative Work Trigger
 *   domain: software_licensing/copyright_law/open_source_governance
 *
 * SUMMARY:
 *   This constraint story treats the narrow_linking_permissive_reading of the
 *   gpl_derivative_work_trigger kernel as a standing legal-interpretive
 *   arrangement that structures the software licensing ecosystem. Under this
 *   reading, linking software modulesâwhether static or
 *   dynamicâconstitutes mere aggregation under copyright law and does not
 *   trigger GPL source-disclosure obligations. Only direct modifications to
 *   GPL-licensed code oblige the modifier. The arrangement coordinates mixed
 *   proprietary/open ecosystems by reducing legal uncertainty for proprietary
 *   vendors, while asymmetrically extracting software freedom from end users
 *   and frustrating the license-drafters' copyleft propagation intent.
 *
 * KEY AGENTS:
 *   - proprietary_software_vendors: Primary beneficiary (powerful/mobile) â retain proprietary control and commercial moats when linking to GPL libraries
 *   - end_users_of_proprietary_software: Primary target (powerless/trapped) â lose source availability and modification rights for combined works
 *   - gpl_authors_and_community: Secondary target (organized/constrained) â license propagation intent structurally blocked by judicial interpretation
 *   - fsf_copyleft_advocates: Excluded voice (organized/constrained) â argue for broad copyleft but sidelined in dominant commercial legal practice
 *   - legal_interpreters_and_courts: Agenda setter (institutional/analytical) â administer the derivative-work boundary through precedent and doctrinal interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.62).
domain_priors:suppression_score(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.58).
domain_priors:theater_ratio(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__narrow_linking_permissive_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "Narrow Linking Permissive Reading of GPL Derivative Work Trigger").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__narrow_linking_permissive_reading, "software_licensing/copyright_law/open_source_governance").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__narrow_linking_permissive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'b5d8aef9-b110-465b-a82e-74dad9ad0ac3').
narrative_ontology:cs_kernel_codification('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', formalized).
narrative_ontology:cs_authority_grounding('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', lineage).
narrative_ontology:cs_interpretation_layer_present('b5d8aef9-b110-465b-a82e-74dad9ad0ac3').
narrative_ontology:cs_reading_relation('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', gpl_derivative_work_trigger__broad_copyleft_reading, forecloses).
narrative_ontology:cs_reading_relation('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', gpl_derivative_work_trigger__interface_boundary_reading, coexists_with).
narrative_ontology:cs_axiom('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', foundational, linking_is_statutory_aggregation).
narrative_ontology:cs_axiom_status(linking_is_statutory_aggregation, holdable).
narrative_ontology:cs_axiom_grounding('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', linking_is_statutory_aggregation, conventional).
narrative_ontology:cs_axiom('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', foundational, proprietary_module_independence).
narrative_ontology:cs_axiom_status(proprietary_module_independence, holdable).
narrative_ontology:cs_axiom_grounding('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', proprietary_module_independence, conventional).
narrative_ontology:cs_reference_frame('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', narrow_copyright_derivative_doctrine).
narrative_ontology:cs_drift_state('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', contemporary_legal_practice, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('b5d8aef9-b110-465b-a82e-74dad9ad0ac3', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_software).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_authors_and_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Combine proprietary application code with GPL libraries through static or dynamic linking without disclosing source code or triggering copyleft obligations. Their business models and competitive moats depend on maintaining proprietary control over their modules while leveraging open-source infrastructure.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors, beneficiary,
    powerful, biographical, mobile, global).

% Receive and run software that incorporates GPL components via linking but are not provided corresponding source code or the legal right to modify and redistribute the combined work. They cannot practically audit, modify, or self-host the software they depend on because the narrow interpretive wall blocks GPL propagation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, end_users_of_proprietary_software, payer,
    powerless, immediate, trapped, global).

% Publish code under the GNU General Public License expecting that linking with their work will extend source-availability guarantees to users of combined works. The narrow reading structurally frustrates this intent by judicially limiting the license's reach to direct modifications only.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, gpl_authors_and_community, payer,
    organized, generational, constrained, global).

% Advocate for the broad copyleft position that linking creates a derivative work and triggers source-disclosure obligations. They are structurally excluded from the dominant commercial and legal practice that has normalized the narrow permissive reading, though they continue to contest it through license drafting and public argument.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, fsf_copyleft_advocates, excluded,
    organized, generational, constrained, global).

% Judges, attorneys, and legal scholars interpret the scope of derivative works under copyright statute and apply it to software linking. Their precedents and doctrinal writings establish the enforceable boundary between permissible aggregation and obligating derivation, administering the constraint's practical effect across jurisdictions.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__narrow_linking_permissive_reading, legal_interpreters_and_courts, agenda_setter,
    institutional, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__narrow_linking_permissive_reading, proprietary_software_vendors).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__narrow_linking_permissive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty in mixed proprietary and open-source ecosystems by drawing a bright-line boundary: software linking alone does not trigger copyleft, enabling proprietary vendors to participate in GPL ecosystems without fear of license contagion.
% TRANSFER_FUNCTION: Moves source-code disclosure obligations and user modification rights away from combined works that link to GPL libraries, transferring the freedom deficit to end users who would have received those rights under a broad copyleft reading.
% ABSENT_VOICES: The Free Software Foundation and copyleft advocates who drafted the GPL are excluded from the interpretive mainstream; they would argue that linking is a functional form of derivation and that users deserve source availability for the entire combined work. Their absence from dominant commercial legal practice shapes the consensus that the narrow reading is the operative norm.
% DISAPPEARANCE_RATIONALE: If courts and practitioners abandoned the narrow reading overnight, proprietary vendors would face immediate obligation to release source code for modules linked to GPL libraries, or cease such linking. The structure of the commercial software stack would reorganize around either broad copyleft compliance or avoidance of GPL components entirely.
% FOUNDING_PROBLEM: Copyright law's concept of a derivative work was developed for literary and artistic works before software; the application of this concept to software linking created doctrinal ambiguity that threatened to either paralyze proprietary software development or nullify copyleft through uncertainty.
% FOUNDING_PROBLEM_CORROBORATION: Proprietary software industry attorneys attest the problem was legal certainty for their clients. Copyleft advocates and the FSF attest the problem was license enforceability, not proprietary convenience. Independent legal scholars outside both camps acknowledge the statutory ambiguity but dispute whether the narrow reading solves the correct problem.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__narrow_linking_permissive_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__narrow_linking_permissive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__narrow_linking_permissive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__narrow_linking_permissive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderately high because the reading enables systematic enclosure: proprietary vendors capture value from GPL infrastructure without reciprocating source availability. Suppression (0.58) reflects the active legal-doctrinal suppression of the broad copyleft alternative through precedent, compliance industry normalization, and corporate legal practice. Theater ratio is low-moderate (0.25) because the legal reasoning has genuine analytical substance, though a performative element exists in post-hoc rationalizations of proprietary enclosure. Accessibility collapse (0.48) captures how alternative legal interpretations become harder to enforce once the narrow reading is entrenched in commercial practice. Resistance (0.60) is elevated because copyleft advocates and some licensors actively contest the reading.
 *
 * PERSPECTIVAL GAP:
 *   The proprietary vendor seat experiences the constraint as protective coordination that enables lawful participation in open ecosystems. The end-user and GPL author seats experience the same structure as extraction of freedoms that the license text purported to guarantee. The engine computes this divergence from the structural data: identical legal facts generate opposite directionality depending on whether the agent is shielded by or exposed to the interpretive boundary.
 *
 * DIRECTIONALITY LOGIC:
 *   Proprietary vendors are structural beneficiaries (low d) because the constraint subsidizes their business model by removing copyleft risk. End users are structural targets (high d) because they bear the freedom deficit. GPL authors are also targets (high d) because their license intent is structurally overridden. Legal interpreters sit near symmetric or agenda-setter positions because they administer the boundary without directly capturing its economic transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   The narrow reading prevents misclassification as pure extraction by preserving a genuine coordination function: without a clear safe harbor for linking, proprietary vendors might avoid GPL code entirely, starving ecosystems of adoption and investment. However, the Tangled Rope classification captures that the same structure that coordinates coexistence also asymmetrically extracts from users and licensors. If the coordination function were absentâif the reading served no purpose but proprietary enclosureâit would compute as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linking_derivation_ambiguity,
    'Does copyright law''s statutory definition of derivative work inherently include or exclude software linking as a matter of doctrinal interpretation?',
    'Authoritative high-court ruling or legislative amendment explicitly addressing whether software linking satisfies the derivative-work predicate under copyright statute.',
    'Would resolve whether the narrow reading is a natural implication of copyright law or a constructed interpretation serving proprietary enclosure interests; could shift classification toward mountain (if statutory) or snare (if constructed extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linking_derivation_ambiguity, conceptual, 'Irreducible ambiguity in statutory interpretation of derivative work for software linking').

omega_variable(
    coordination_vs_extraction_function,
    'Does the narrow reading primarily provide necessary legal certainty for mixed proprietary/open ecosystems, or does it function as a structural subsidy to proprietary software vendors at the expense of user freedoms?',
    'Comparative empirical analysis of software ecosystem behavior in jurisdictions with broad versus narrow derivative-work interpretations; measurement of GPL adoption, proprietary participation, and end-user source availability.',
    'If the reading''s primary effect is coordination with incidental extraction, the Tangled Rope classification holds. If the coordination story is cover for systemic enclosure, the computed classification migrates toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_function, empirical, 'Structural ambiguity between genuine coordination and proprietary extraction').

omega_variable(
    kernel_reading_sibling_divergence,
    'This constraint is one reading of the gpl_derivative_work_trigger kernel. How would the structural classification change if the broad_copyleft_reading or interface_boundary_reading were adopted instead?',
    'Comparative constraint-story analysis across the three sibling readings, examining reversed beneficiary/victim structures and shifted extractiveness profiles.',
    'The broad_copyleft_reading would likely reverse the beneficiary/victim structure (end users and GPL authors as beneficiaries, proprietary vendors as payers). The interface_boundary_reading would likely produce a middling extractiveness profile conditional on API cleanliness.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_sibling_divergence, conceptual, 'Committer-frame omega documenting the kernel decomposition and sibling structural deltas').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__narrow_linking_permissive_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_linking_tr_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl_narrow_linking_tr_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(gpl_narrow_linking_tr_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(gpl_narrow_linking_tr_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(gpl_narrow_linking_tr_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(gpl_narrow_linking_tr_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 25, 0.23).
narrative_ontology:measurement(gpl_narrow_linking_tr_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, theater_ratio, 30, 0.25).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_linking_be_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(gpl_narrow_linking_be_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(gpl_narrow_linking_be_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(gpl_narrow_linking_be_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(gpl_narrow_linking_be_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gpl_narrow_linking_be_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(gpl_narrow_linking_be_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(gpl_narrow_linking_su_t0, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gpl_narrow_linking_su_t5, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 5, 0.3).
narrative_ontology:measurement(gpl_narrow_linking_su_t10, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(gpl_narrow_linking_su_t15, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement(gpl_narrow_linking_su_t20, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(gpl_narrow_linking_su_t25, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 25, 0.55).
narrative_ontology:measurement(gpl_narrow_linking_su_t30, gpl_derivative_work_trigger__narrow_linking_permissive_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, broad_copyleft_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__narrow_linking_permissive_reading, interface_boundary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the gpl_derivative_work_trigger kernel, decomposing the colloquial label 'GPL derivative work trigger' into structurally distinct interpretive claims. Each reading carries a different epsilon, beneficiary/victim structure, and classification. They are linked as a constraint family through mutual network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
