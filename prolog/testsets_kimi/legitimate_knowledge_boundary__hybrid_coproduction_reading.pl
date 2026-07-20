% ============================================================================
% CONSTRAINT STORY: legitimate_knowledge_boundary__hybrid_coproduction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_knowledge_boundary__hybrid_coproduction_reading, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: legitimate_knowledge_boundary__hybrid_coproduction_reading
 *   human_readable: Hybrid Co-Production Knowledge Legitimacy Boundary
 *   domain: epistemology/science_and_technology_studies/political_theory
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_coproduction_reading of the
 *   contested kernel legitimate_knowledge_boundary. It enforces a boundary
 *   around legitimate knowledge that requires integration of methodological
 *   rigor and experiential validity through formal co-production processes.
 *   The reading is one of three live positions: the
 *   credentialed_expertise_reading (methodology alone suffices), the
 *   experiential_pluralism_reading (lived experience is primary), and this
 *   hybrid reading (both are mandatory). The constraint operates through
 *   funding gates, journal accreditation, and ethics review, creating genuine
 *   coordination between science and communities while also imposing
 *   asymmetric costs on those who cannot afford the dual infrastructure.
 *
 * KEY AGENTS:
 *   - coproduction_policy_bodies: Agenda setter (institutional/constrained/global) â administers dual-validation standards and funding compliance
 *   - hybrid_research_centers: Primary beneficiary (organized/constrained/national) â intermediaries who capture funding and legitimacy under the hybrid regime
 *   - disciplinary_tradition_researchers: Primary target/payer (moderate/identity_locked/national) â bear retooling costs and exclusion from funding
 *   - community_knowledge_holders: Secondary target/payer (powerless/trapped/local) â bear translation and engagement costs without guaranteed authorship control
 *   - sts_observers: Analytical observer (analytical/analytical/global) â traces boundary construction and labor extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58).
domain_priors:suppression_score(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.52).
domain_priors:theater_ratio(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(legitimate_knowledge_boundary__hybrid_coproduction_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_knowledge_boundary__hybrid_coproduction_reading, tangled_rope).
narrative_ontology:human_readable(legitimate_knowledge_boundary__hybrid_coproduction_reading, "Hybrid Co-Production Knowledge Legitimacy Boundary").
narrative_ontology:topic_domain(legitimate_knowledge_boundary__hybrid_coproduction_reading, "epistemology/science_and_technology_studies/political_theory").

domain_priors:requires_active_enforcement(legitimate_knowledge_boundary__hybrid_coproduction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_knowledge_boundary__hybrid_coproduction_reading, '66c023d0-fea6-418c-824e-59f59ac5d784').
narrative_ontology:cs_kernel_codification('66c023d0-fea6-418c-824e-59f59ac5d784', formalized).
narrative_ontology:cs_authority_grounding('66c023d0-fea6-418c-824e-59f59ac5d784', expertise).
narrative_ontology:cs_interpretation_layer_present('66c023d0-fea6-418c-824e-59f59ac5d784').
narrative_ontology:cs_reading_relation('66c023d0-fea6-418c-824e-59f59ac5d784', legitimate_knowledge_boundary__credentialed_expertise_reading, coexists_with).
narrative_ontology:cs_reading_relation('66c023d0-fea6-418c-824e-59f59ac5d784', legitimate_knowledge_boundary__experiential_pluralism_reading, coexists_with).
narrative_ontology:cs_axiom('66c023d0-fea6-418c-824e-59f59ac5d784', foundational, dual_validation_mandatory).
narrative_ontology:cs_axiom_status(dual_validation_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('66c023d0-fea6-418c-824e-59f59ac5d784', dual_validation_mandatory, instrumental).
narrative_ontology:cs_reference_frame('66c023d0-fea6-418c-824e-59f59ac5d784', participatory_expertise_framework).
narrative_ontology:cs_drift_state('66c023d0-fea6-418c-824e-59f59ac5d784', post_participatory_turn, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('66c023d0-fea6-418c-824e-59f59ac5d784', '').
narrative_ontology:cs_kernel_id(legitimate_knowledge_boundary__hybrid_coproduction_reading, legitimate_knowledge_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_knowledge_boundary__hybrid_coproduction_reading, hybrid_research_centers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_tradition_researchers).
narrative_ontology:constraint_victim(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer research funding and accreditation criteria that require both methodological protocols and community partnership plans. They evaluate grant applications against dual-validation rubrics and enforce compliance through milestone reporting, ethics review, and journal accreditation.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, coproduction_policy_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Receive dedicated funding streams and institutional prestige by specializing in participatory and transdisciplinary methods. Their staff are trained in both qualitative community engagement and quantitative analysis, positioning them as natural intermediaries for legitimate knowledge production under the dual-validation regime.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, hybrid_research_centers, beneficiary,
    organized, biographical, constrained, national).

% Conduct laboratory or field research using established disciplinary methods. They now face funding eligibility restrictions and journal rejection where experiential validation is absent, requiring them to add community-engagement components that fall outside their training, timelines, and research goals.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, disciplinary_tradition_researchers, payer,
    moderate, biographical, identity_locked, national).

% Hold place-based, embodied, or cultural expertise about environmental and social conditions. To gain legitimacy under this framework, they must translate their knowledge into methodological formats, travel to academic meetings, and accept outsider framing of their experience, often without final control over findings, authorship, or how their knowledge is used.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, community_knowledge_holders, payer,
    powerless, generational, trapped, local).

% Study how the boundary between legitimate and illegitimate knowledge is constructed and enforced. They trace funding flows, publication patterns, and authorship distributions to assess whether co-production redistributes authority or extracts labor from less powerful participants.
narrative_ontology:constraint_stakeholder(legitimate_knowledge_boundary__hybrid_coproduction_reading, sts_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimate_knowledge_boundary__hybrid_coproduction_reading, hybrid_research_centers).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Bridges the trust and relevance gap between scientific institutions and affected communities by requiring that knowledge claims be validated through both methodological rigor and lived experience, producing research that is intended to be robust and socially accountable.
% TRANSFER_FUNCTION: Moves epistemic legitimacy and research funding from mono-disciplinary or purely experiential knowledge producers toward hybrid institutions that can perform both validation types; moves labor, translation costs, and compliance burdens from community knowledge holders and traditional researchers to the dual-validation infrastructure.
% ABSENT_VOICES: Pure methodological purists who reject experiential validity as epistemically irrelevant, and radical experiential pluralists who view mandatory methodological framing as colonial or extractive, are both structurally disadvantaged in mainstream funding and publication venues; they would object but are not in the room where standards are set.
% DISAPPEARANCE_RATIONALE: If the dual-validation requirement disappeared overnight, funding flows would revert toward traditional disciplinary channels and autonomous community-based knowledge projects; hybrid research centers would lose their privileged intermediary position; journals and policy bodies would reorganize around simpler legitimacy boundaries.
% FOUNDING_PROBLEM: The trust deficit between scientific institutions and affected communities, producing research that is methodologically sound but policy-irrelevant or harmful, alongside community knowledge that is experientially rich but institutionally ignored.
% FOUNDING_PROBLEM_CORROBORATION: STS scholars and critical policy studies researchers outside the hybrid funding stream attest to the science-society trust gap. However, the specific co-production remedy is primarily advanced by the policy bodies and hybrid centers that benefit from it. Independent evaluations from marginalized communities are mixed: some corroborate the problem, others reject the hybrid solution as a new form of extraction.
narrative_ontology:disappearance_verdict(legitimate_knowledge_boundary__hybrid_coproduction_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_knowledge_boundary__hybrid_coproduction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(legitimate_knowledge_boundary__hybrid_coproduction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_knowledge_boundary__hybrid_coproduction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_knowledge_boundary__hybrid_coproduction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because the dual-validation requirement redirects substantial epistemic legitimacy and funding toward hybrid institutions and imposes compliance costs on both traditional researchers and community holders. Suppression (0.52) reflects active enforcement through funding eligibility, journal gatekeeping, and ethics accreditation; alternatives (pure disciplinary or pure community knowledge) remain possible but are delegitimized within mainstream channels. Theater ratio (0.35) captures the growing share of performative co-productionâbox-ticking community engagement that satisfies audit requirements without redistributing authority. Accessibility collapse (0.50) is moderate because pure alternatives are still visible and practiced at the margins, but they are increasingly coded as illegitimate in policy-relevant science. Resistance (0.55) is moderate and bidirectional: methodological purists resist the experiential requirement as diluting rigor, while some community holders resist methodological framing as extractive. Measurements share one time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (policy bodies, hybrid centers) experience the constraint as necessary coordination that repairs science-society trust. The payer seats (disciplinary researchers, community holders) experience it as gatekeeping that extracts labor and redirects resources. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Hybrid research centers are declared beneficiaries and sit at organized power with constrained exit, yielding a low directionality value. Community knowledge holders and disciplinary researchers are declared victims: the former are powerless and trapped, yielding very high directionality; the latter are moderate but identity-locked, yielding high directionality. Policy bodies are not declared beneficiaries or victims; their directionality is derived from institutional power and constrained exit, sitting near the symmetric middle but slightly toward the beneficiary side because they administer the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because it carries a genuine coordination function (trust, relevance, accountability) that would be lost if it were a pure snare, while the declared victims and active enforcement prevent mislabeling it as a pure rope. It is not a scaffold because it lacks a sunset clause and its justification is the steady state, not a transition. It is not a piton because the beneficiary seat actively profits and maintains the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    co_production_extraction_ambiguity,
    'Does the co-production requirement genuinely redistribute epistemic authority to communities, or does it extract experiential labor while preserving academic control over validation, framing, and publication?',
    'Longitudinal authorship and agenda-control analysis in co-produced studies: if community partners hold co-first authorship and co-design control, redistribution is genuine; if their role is limited to consultation and data provision, extraction dominates.',
    'If extraction dominates, the constraint''s effective extractiveness is higher than its coordination benefit suggests, pushing the computed type toward snare; if redistribution is genuine, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(co_production_extraction_ambiguity, empirical, 'Whether co-production empowers communities or extracts their labor').

omega_variable(
    kernel_reading_stability,
    'Is the hybrid reading structurally stable, or does it collapse into one of its sibling readings under resource contraction or political pressure?',
    'Funding-shock analysis: when budgets contract, observe whether hybrid centers abandon experiential validity to survive (collapse toward credentialed_expertise) or whether communities abandon methodological standards (collapse toward experiential_pluralism).',
    'Collapse under pressure would reveal the hybrid constraint as a transient scaffold or unstable rope rather than a sustainable tangled rope, and would reclassify the reading accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_stability, conceptual, 'Structural stability of the hybrid reading against its siblings').

omega_variable(
    barrier_legitimacy,
    'Are the moderate barriers (dual validation, infrastructure investment) necessary for epistemic quality and social accountability, or do they function as a gatekeeping mechanism that privileges well-resourced hybrid institutions?',
    'Comparative outcome studies across jurisdictions with varying barrier intensity, measuring policy uptake, community satisfaction, and knowledge robustness.',
    'If the barriers primarily gatekeep, the constraint''s extractiveness and suppression are higher than currently measured; if they are quality-necessary, the coordination function is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(barrier_legitimacy, conceptual, 'Whether dual-validation barriers serve quality or gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_knowledge_boundary__hybrid_coproduction_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lkb_hcr_tr_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lkb_hcr_tr_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(lkb_hcr_tr_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(lkb_hcr_tr_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 15, 0.26).
narrative_ontology:measurement(lkb_hcr_tr_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(lkb_hcr_tr_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 25, 0.33).
narrative_ontology:measurement(lkb_hcr_tr_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(lkb_hcr_be_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(lkb_hcr_be_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(lkb_hcr_be_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(lkb_hcr_be_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 15, 0.46).
narrative_ontology:measurement(lkb_hcr_be_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(lkb_hcr_be_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(lkb_hcr_be_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(lkb_hcr_su_t0, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(lkb_hcr_su_t5, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 5, 0.28).
narrative_ontology:measurement(lkb_hcr_su_t10, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(lkb_hcr_su_t15, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 15, 0.44).
narrative_ontology:measurement(lkb_hcr_su_t20, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement(lkb_hcr_su_t25, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement(lkb_hcr_su_t30, legitimate_knowledge_boundary__hybrid_coproduction_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_knowledge_boundary__hybrid_coproduction_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
