% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Carveout Reading
 *   domain: intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the hybrid_carveout_reading of the
 *   derivative_work_statutory_boundary kernel. Under this reading, copyright
 *   law distinguishes between non-commercial transformative uses (permitted
 *   without authorization) and commercial derivative uses (requiring
 *   rights-holder authorization). The arrangement coordinates non-commercial
 *   cultural production while extracting licensing value from commercial
 *   developers. Key agents include rights holders who benefit from commercial
 *   licensing fees, non-commercial users who gain a categorical exemption,
 *   commercial developers who bear compliance and licensing costs, and the
 *   legislative and judicial authorities who define and enforce the boundary.
 *   Sibling readingsâenclosure (all uses require authorization) and
 *   coordination (transformative uses are free regardless of commercial
 *   status)ârepresent structurally distinct positions in the same kernel.
 *
 * KEY AGENTS:
 *   - rights_holders (organized/global): Primary beneficiary â collects licensing revenue from commercial derivative uses.
 *   - non_commercial_users (moderate/global): Secondary beneficiary â exempt from authorization for transformative non-commercial uses.
 *   - commercial_developers (moderate/global): Primary target â must secure licenses for commercial derivatives, bearing transaction costs and creative constraints.
 *   - copyright_legislature (institutional/national): Agenda setter â defines the statutory commercial/non-commercial boundary.
 *   - copyright_judiciary (institutional/national): Observer â interprets the boundary in infringement disputes.
 *   - digital_rights_advocates (moderate/global): Excluded voice â argues for narrower copyright scope and against commercial licensing requirements.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.55).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Carveout Reading").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '4c094e7d-6100-4e78-b3e2-c8a43d9ca64b').
narrative_ontology:cs_kernel_codification('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', formalized).
narrative_ontology:cs_authority_grounding('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', lineage).
narrative_ontology:cs_interpretation_layer_present('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b').
narrative_ontology:cs_reading_relation('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', foundational, commercial_status_triggers_authorization).
narrative_ontology:cs_axiom_status(commercial_status_triggers_authorization, holdable).
narrative_ontology:cs_axiom_grounding('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', commercial_status_triggers_authorization, conventional).
narrative_ontology:cs_axiom('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', foundational, non_commercial_transformative_exemption).
narrative_ontology:cs_axiom_status(non_commercial_transformative_exemption, holdable).
narrative_ontology:cs_axiom_grounding('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', non_commercial_transformative_exemption, conventional).
narrative_ontology:cs_reference_frame('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', statutory_derivative_authority).
narrative_ontology:cs_drift_state('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', digital_reproduction_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4c094e7d-6100-4e78-b3e2-c8a43d9ca64b', '').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_users).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_exploitation_doctrine).
narrative_ontology:constraint_vindicates(derivative_work_statutory_boundary__hybrid_carveout_reading, statutory_boundary_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive copyrights and collect licensing revenue from commercial derivative uses. The hybrid carveout preserves their market power over commercial exploitation while conceding non-commercial transformative activity.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holders, beneficiary,
    organized, generational, mobile, global).

% Create transformative works without seeking authorization provided the use remains non-commercial. The carveout gives them a categorical safe harbor that lowers legal risk for remix, fan works, and educational adaptation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_users, beneficiary,
    moderate, biographical, mobile, global).

% Develop commercial products or services that incorporate or transform pre-existing copyrighted expression. Must secure licenses or face infringement liability; the non-commercial exemption does not apply to them, creating a structural cost floor and creative constraint.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_developers, payer,
    moderate, biographical, constrained, global).

% Defines the statutory boundary between exempt non-commercial transformative uses and commercial uses requiring authorization. Sets enforcement frameworks and can amend the scope of the carveout.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_legislature, agenda_setter,
    institutional, generational, analytical, national).

% Interprets the commercial-non-commercial distinction in infringement litigation; adjudicates whether a use is transformative and whether it crosses into commercial exploitation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_judiciary, observer,
    institutional, generational, analytical, national).

% Argue for a narrower copyright scope and against commercial licensing requirements for derivatives. They are structurally underrepresented in the legislative and treaty processes that set the boundary.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, digital_rights_advocates, excluded,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(derivative_work_statutory_boundary__hybrid_carveout_reading, rights_holders).
narrative_ontology:fixing_cost_class(derivative_work_statutory_boundary__hybrid_carveout_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable legal boundary that exempts non-commercial transformative uses from authorization while requiring it for commercial exploitation, reducing uncertainty for creators and rights holders about when licensing is necessary.
% TRANSFER_FUNCTION: Moves licensing revenue and creative control from commercial developers to rights holders for commercial derivative works; moves freedom to create without authorization to non-commercial users.
% ABSENT_VOICES: Public domain advocates and developers from jurisdictions with broad fair-use traditions are underrepresented; they would argue for a unified standard regardless of commercial status.
% DISAPPEARANCE_RATIONALE: If the boundary vanished, commercial developers would freely incorporate existing works without licensing, non-commercial users would lose their categorical safe harbor (though other doctrines might cover some uses), and rights holders would lose a defined revenue channel from derivative commercial exploitation.
% FOUNDING_PROBLEM: Uncertainty over the scope of derivative works and the need to balance incentives for original creators against follow-on innovation, particularly distinguishing commercial exploitation from cultural or educational reuse.
% FOUNDING_PROBLEM_CORROBORATION: Rights holders and collecting societies attest the problem remains live due to digital appropriation. Academic commentators and digital rights organizations outside the benefiting parties contest that the original incentive problem persists at the claimed severity; empirical evidence on creator substitution effects is disputed.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.6, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.60) because the constraint extracts only from commercial developers while exempting non-commercial users, creating a bifurcated structure rather than universal extraction. Suppression (0.55) reflects the legal suppression of unauthorized commercial derivative uses; non-commercial uses are not suppressed. Theater_ratio (0.35) captures the increasing performative aspect of rights-holder enforcement campaigns that frame licensing as moral duty rather than statutory market segmentation. Accessibility_collapse (0.60) is moderate because alternatives (fair use, public domain, licensing markets) partially exist but are costly to navigate for commercial actors. Resistance (0.45) reflects sustained lobbying and litigation from commercial developers and platforms. The measurement series tracks the gradual hardening of the commercial licensing regime from early digital ambiguity to present-day enforcement norms.
 *
 * PERSPECTIVAL GAP:
 *   Non-commercial users experience the constraint as a ropeâa clear safe harbor enabling remix and transformative culture. Rights holders experience it as a rope that secures a revenue channel. Commercial developers experience the same legal structure as a snare, where the carveout they do not qualify for functions as a pricing floor and creative control ceiling. The engine computes these divergent seat classifications from the same structural data: beneficiary declarations push directionality toward subsidy for non-commercial users and rights holders, while victim declarations push directionality toward extraction for commercial developers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural split: rights_holders and non_commercial_users are declared beneficiaries, yielding low d (near 0.0â0.2), which dampens effective extraction for those seats. commercial_developers are declared victims with constrained exit options, yielding high d (near 0.8â1.0), amplifying effective extraction. No override is needed because the beneficiary-victim split cleanly maps to the commercial-non-commercial categories.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid carveout prevents a mandatrophy misclassification in both directions. Without the non-commercial exemption, the constraint would be a pure enclosure reading (snare): all derivative uses require authorization, and the coordination function collapses to zero. With the exemption, a genuine coordination benefit exists for non-commercial users, disqualifying pure snare classification. Conversely, if the commercial requirement were absent, the constraint would be a coordination reading (rope): transformative uses are free. The presence of both elementsâcoordination for non-commercial users and extraction from commercial developersâforces tangled_rope classification, correctly capturing the dual nature of the arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_status_boundary,
    'Does the commercial/non-commercial distinction in derivative work authorization map to a structurally coherent coordination function, or does it create arbitrary extraction at the boundary?',
    'Jurisprudential analysis of how courts draw the line between commercial and non-commercial use; empirical study of chilling effects on commercial developers operating near the boundary.',
    'If the boundary is incoherent, the coordination story dissolves and the constraint reads as a snare for commercial developers; if coherent, the carveout is a genuine coordination mechanism with asymmetric cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_status_boundary, conceptual, 'Whether the commercial-non-commercial boundary is structurally coherent or arbitrary.').

omega_variable(
    kernel_reading_contest,
    'Does the hybrid carveout reading foreclose the enclosure reading, or do they coexist within different national copyright frameworks?',
    'Comparative law analysis showing whether jurisdictions adopting hybrid carveouts structurally reject enclosure readings.',
    'If they coexist, the kernel is distributed across jurisdictions; if hybrid forecloses enclosure within unified legal frameworks, the kernel is formally contested at the domestic level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between hybrid and enclosure readings across jurisdictions.').

omega_variable(
    non_commercial_user_benefit_verification,
    'Do non-commercial transformative users genuinely benefit from the carveout, or does the commercial licensing requirement upstream suppress tools and platforms they depend on?',
    'Study of platform terms of service and content moderation under the carveout regime; analysis of whether non-commercial hosting services are chilled by downstream commercial liability.',
    'If suppression propagates to non-commercial channels, the categorical beneficiary split is illusory and effective extraction is higher than the structural measure suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_commercial_user_benefit_verification, empirical, 'Whether non-commercial users'' exemption is undermined by upstream commercial licensing pressure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deri_tr_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(deri_tr_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 16, 0.26).
narrative_ontology:measurement(deri_tr_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 24, 0.3).
narrative_ontology:measurement(deri_tr_t32, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 32, 0.32).
narrative_ontology:measurement(deri_tr_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 40, 0.35).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(deri_be_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(deri_be_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 16, 0.5).
narrative_ontology:measurement(deri_be_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(deri_be_t32, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(deri_be_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 40, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(deri_su_t8, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(deri_su_t16, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 16, 0.48).
narrative_ontology:measurement(deri_su_t24, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(deri_su_t32, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(deri_su_t40, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% The derivative_work_statutory_boundary kernel decomposes into three Îµ-invariant constraints: enclosure_reading (high extraction, universal authorization), coordination_reading (low extraction, universal permission for transformative use), and hybrid_carveout_reading (moderate extraction, bifurcated by commercial status). This story is the hybrid_carveout_reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
