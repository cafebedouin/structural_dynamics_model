% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint represents the GPL's reciprocity obligation as a
 *   mechanism for preserving user freedoms, a reading championed by the Free
 *   Software Foundation. It views the 'viral' nature of copyleft as a
 *   necessary defense against proprietary capture, ensuring that software
 *   remains free for all downstream users. The constraint is classified as a
 *   Tangled Rope because it genuinely coordinates the free software community
 *   while simultaneously extracting (in the form of forced open-sourcing)
 *   from proprietary integrators, requiring active enforcement to maintain
 *   its 'viral' effect.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.78).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'c39aaeb1-a151-4dfd-938e-c13a9c9b42c4').
narrative_ontology:cs_kernel_codification('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', fixed_text).
narrative_ontology:cs_authority_grounding('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', lineage).
narrative_ontology:cs_interpretation_layer_present('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4').
narrative_ontology:cs_reading_relation('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', foundational, software_freedom_is_fundamental).
narrative_ontology:cs_axiom_status(software_freedom_is_fundamental, holdable).
narrative_ontology:cs_axiom_grounding('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', software_freedom_is_fundamental, deontological).
narrative_ontology:cs_axiom('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', foundational, proprietary_capture_is_a_threat_to_freedom).
narrative_ontology:cs_axiom_status(proprietary_capture_is_a_threat_to_freedom, holdable).
narrative_ontology:cs_axiom_grounding('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', proprietary_capture_is_a_threat_to_freedom, empirically_contingent).
narrative_ontology:cs_reference_frame('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', original_gpl_v2_philosophy).
narrative_ontology:cs_drift_state('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', contemporary_software_ecosystem, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c39aaeb1-a151-4dfd-938e-c13a9c9b42c4', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_software_vendors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the assurance that software derived from GPL-licensed code will remain free and modifiable, preventing vendor lock-in and ensuring access to source code. Their freedom to use, study, modify, and distribute is preserved.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    organized, generational, mobile, global).

% Actively promotes and defends the GPL, viewing it as a fundamental tool for software freedom. They enforce the license through legal action and community pressure, ensuring that derived works also adhere to copyleft principles. Their identity is deeply tied to the philosophy of free software.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, free_software_community, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Are constrained by the GPL's viral nature, which prevents them from incorporating GPL-licensed code into proprietary products without open-sourcing their entire derivative work. This limits their business models and forces a choice between avoiding GPL code or adopting copyleft.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Face a significant barrier to using GPL components in their commercial, closed-source offerings. The reciprocity obligation forces them to either contribute their modifications back to the community or develop proprietary alternatives, incurring additional costs.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_software_vendors, payer,
    powerful, biographical, constrained, global).

% Advocate for more permissive licenses (e.g., MIT, Apache) that allow proprietary integration, arguing they foster broader adoption and commercial innovation. Their perspective is often marginalized in discussions dominated by copyleft proponents, who view such licenses as 'weak' or 'non-free'.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, alternative_licensing_advocates, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the development and distribution of software that guarantees user freedom, ensuring that all derivative works contribute back to a shared pool of free software, preventing proprietary forks and fragmentation.
% TRANSFER_FUNCTION: Transfers the obligation to share source code and modifications from the original author to any distributor of a derivative work, effectively 'capturing' proprietary extensions and making them free for all users.
% ABSENT_VOICES: Advocates for more permissive licensing models, who believe that the GPL's 'viral' nature stifles innovation and limits adoption by commercial entities, are often excluded from the core discourse of the free software community, which prioritizes freedom over commercial flexibility.
% DISAPPEARANCE_RATIONALE: If the GPL's reciprocity obligation vanished, proprietary integrators would immediately fork GPL-derived projects, incorporate them into closed-source products, and capture user bases, leading to a rapid enclosure of previously free software and a significant loss of user freedoms.
% FOUNDING_PROBLEM: The problem of software becoming proprietary and users losing the freedom to use, study, modify, and distribute it, leading to vendor lock-in and control over digital life.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and its proponents consistently attest that the threat of proprietary capture remains live, citing ongoing attempts by commercial entities to leverage open-source code without contributing back. Independent legal scholars and open-source activists corroborate this ongoing tension.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is high from the perspective of proprietary integrators, who are forced to choose between avoiding GPL code or open-sourcing their own. Suppression (0.78) is also high, as the license actively prevents alternative licensing models for derivative works and is legally enforced. The theater ratio is low (0.1) because the license's function is direct and effective, with little performative overhead. The claimed type is Tangled Rope, reflecting the dual function of coordinating freedom for some while extracting compliance from others.
 *
 * PERSPECTIVAL GAP:
 *   The free software community perceives the GPL as a pure Rope, a coordination mechanism that guarantees freedom. Proprietary integrators, however, experience it as a Snare, a coercive mechanism that restricts their business models. This reading acknowledges the coordination function for users while recognizing the extractive and suppressive nature for commercial entities.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users and the free software community are clear beneficiaries (d near 0.0), as the license directly serves their interest in software freedom. Proprietary integrators and commercial software vendors are the targets (d near 1.0), as they bear the cost of the reciprocity obligation. Alternative licensing advocates are excluded, as their preferred models are incompatible with the GPL's core principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_vs_restriction_framing,
    'Is the GPL''s reciprocity obligation primarily a mechanism for ''freedom'' (as defined by the free software movement) or a ''restriction'' on developers'' choices and business models?',
    'Analysis of developer surveys and economic impact studies across different licensing regimes, focusing on perceived autonomy and innovation outcomes.',
    'If primarily a restriction, the extractiveness and suppression metrics would be re-evaluated upwards, potentially shifting the classification towards Snare for a broader set of stakeholders. If primarily freedom-enhancing, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(freedom_vs_restriction_framing, conceptual, 'Ambiguity in framing the GPL''s core effect.').

omega_variable(
    enforcement_legitimacy,
    'To what extent is the legal enforcement of the GPL perceived as legitimate by those it constrains, versus being seen as an arbitrary imposition?',
    'Legal precedent analysis, industry compliance rates, and qualitative studies of developer attitudes towards GPL enforcement actions.',
    'Low perceived legitimacy would increase the effective suppression and resistance metrics, indicating a more coercive and less coordinated constraint. High perceived legitimacy would support the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_legitimacy, empirical, 'Legitimacy of GPL enforcement actions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gpl__tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(gpl__tr_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(gpl__tr_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(gpl__tr_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(gpl__be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(gpl__be_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(gpl__be_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 30, 0.65).
narrative_ontology:measurement(gpl__be_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 40, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(gpl__su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.73).
narrative_ontology:measurement(gpl__su_t20, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(gpl__su_t30, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 30, 0.78).
narrative_ontology:measurement(gpl__su_t40, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, copyleft_as_commons_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
