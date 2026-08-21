% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__behavioral_control_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__behavioral_control_reading, []).

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
 *   constraint_id: hoa_covenant_scope__behavioral_control_reading
 *   human_readable: HOA Covenant: Behavioral Control Reading
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story represents the 'behavioral control' reading of an
 *   HOA covenant, where its primary function is perceived as enforcing
 *   aesthetic uniformity and behavioral conformity to maximize property
 *   values. This reading emphasizes the subjective and often arbitrary nature
 *   of the rules, and the coercive mechanisms used to enforce them, leading
 *   to a Snare classification. The metrics reflect moderate extraction and
 *   high suppression, with a low but increasing theater ratio as the stated
 *   purpose (property value protection) becomes a cover for control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__behavioral_control_reading, 0.45).
domain_priors:suppression_score(hoa_covenant_scope__behavioral_control_reading, 0.7).
domain_priors:theater_ratio(hoa_covenant_scope__behavioral_control_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(hoa_covenant_scope__behavioral_control_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__behavioral_control_reading, snare).
narrative_ontology:human_readable(hoa_covenant_scope__behavioral_control_reading, "HOA Covenant: Behavioral Control Reading").
narrative_ontology:topic_domain(hoa_covenant_scope__behavioral_control_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__behavioral_control_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__behavioral_control_reading, '125ff2c9-700d-44fc-ad9f-23e2f7c12c8a').
narrative_ontology:cs_kernel_codification('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', formalized).
narrative_ontology:cs_authority_grounding('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', lineage).
narrative_ontology:cs_interpretation_layer_present('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a').
narrative_ontology:cs_reading_relation('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', hoa_covenant_scope__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', foundational, aesthetic_uniformity_maximizes_value).
narrative_ontology:cs_axiom_status(aesthetic_uniformity_maximizes_value, holdable).
narrative_ontology:cs_axiom_grounding('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', aesthetic_uniformity_maximizes_value, empirically_contingent).
narrative_ontology:cs_axiom('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', foundational, behavioral_conformity_ensures_community_harmony).
narrative_ontology:cs_axiom_status(behavioral_conformity_ensures_community_harmony, holdable).
narrative_ontology:cs_axiom_grounding('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', behavioral_conformity_ensures_community_harmony, empirically_contingent).
narrative_ontology:cs_reference_frame('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', property_value_protection_framework).
narrative_ontology:cs_drift_state('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', contemporary_enforcement_practices, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('125ff2c9-700d-44fc-ad9f-23e2f7c12c8a', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__behavioral_control_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, conformist_majority).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__behavioral_control_reading, property_management_companies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and enforces the covenant, issuing fines and imposing restrictions on aesthetic choices and behaviors. Believes these actions protect property values and community standards. Benefits from perceived order and control.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, hoa_board, agenda_setter,
    institutional, biographical, constrained, local).

% Actively supports the HOA board's enforcement of aesthetic and behavioral rules, believing it maintains property values and a desirable community image. Benefits from the perceived stability and conformity.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, board_aligned_homeowners, beneficiary,
    organized, biographical, constrained, local).

% Passively benefits from the general aesthetic uniformity and perceived higher property values, often without actively participating in enforcement. Avoids conflict by conforming to rules.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, conformist_majority, beneficiary,
    moderate, biographical, constrained, local).

% Bear the costs of fines, legal challenges, and social pressure for violating aesthetic or behavioral rules (e.g., specific paint colors, yard signs, flag displays). Their identity is often tied to personal expression, making conformity a high cost. Exit means selling their home.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, nonconformist_homeowners, payer,
    powerless, immediate, identity_locked, local).

% Seek to express individuality through their property's appearance, often clashing with the HOA's uniform standards. They face direct enforcement and financial penalties, with limited recourse due to the covenant's broad language.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, marginal_aesthetics_advocates, payer,
    powerless, biographical, constrained, local).

% Contracted by the HOA board to administer and enforce the covenant. They benefit financially from the ongoing need for enforcement, including processing fines and managing disputes, regardless of the underlying justification.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__behavioral_control_reading, property_management_companies, beneficiary,
    organized, generational, mobile, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The covenant coordinates a shared aesthetic and behavioral standard, aiming to prevent perceived 'blight' or 'disruptive' activities that might negatively impact property values or community cohesion.
% TRANSFER_FUNCTION: Transfers financial penalties (fines) and control over personal expression from nonconformist homeowners to the HOA board and, indirectly, to the conformist majority who benefit from the enforced uniformity.
% ABSENT_VOICES: Homeowners who value individual expression over strict uniformity, or those with diverse aesthetic preferences, are often marginalized or silenced in HOA decision-making processes, leading to a lack of representation for alternative views.
% DISAPPEARANCE_RATIONALE: If the covenant's behavioral control aspects vanished, individual homeowners would immediately exercise greater autonomy over their property's appearance and their lifestyle choices. This would lead to a more diverse aesthetic landscape, potentially altering property values (up or down depending on individual preference) and shifting the power dynamics within the community.
% FOUNDING_PROBLEM: The covenant was established to prevent perceived degradation of property values due to inconsistent aesthetic standards and uncontrolled resident behavior, aiming to create a stable and predictable living environment.
% FOUNDING_PROBLEM_CORROBORATION: The HOA board and many long-term residents attest that the problem of maintaining property values and community standards is still live, citing examples of potential non-conformity. However, nonconformist homeowners and some external observers argue that the problem has been over-solved, and the covenant now serves primarily to enforce subjective preferences rather than address genuine threats to property value.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__behavioral_control_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__behavioral_control_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__behavioral_control_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__behavioral_control_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__behavioral_control_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__behavioral_control_reading_tests).
:- end_tests(hoa_covenant_scope__behavioral_control_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while direct financial costs (fines) are present, the primary extraction is often the suppression of individual expression and autonomy. Suppression (0.7) is high due to the broad scope of rules (subjective aesthetics, lifestyle, speech like yard signs) and the HOA's enforcement power, making exit difficult for homeowners. Theater ratio (0.2) is low but rising, as the stated goal of 'property value maximization' increasingly serves as a justification for enforcing subjective preferences rather than addressing objective threats. Accessibility collapse (0.6) is moderate, as homeowners can technically sell and move, but the cost of doing so to escape specific rules is high. Resistance (0.4) is present but often fragmented, as individual homeowners struggle against an organized institutional power.
 *
 * PERSPECTIVAL GAP:
 *   The HOA board and aligned homeowners perceive the covenant as a legitimate tool for coordination and value protection (closer to a Rope or even Mountain in their view). Nonconformist homeowners experience it as a Snare, extracting their autonomy and financial resources for subjective compliance. This divergence is central to the kernel contest.
 *
 * DIRECTIONALITY LOGIC:
 *   The HOA board and aligned homeowners are beneficiaries, gaining from the enforced conformity and perceived stability. Nonconformist homeowners and advocates for marginal aesthetics are victims, bearing the direct costs of fines and the indirect cost of suppressed expression. Property management companies benefit financially from administering the enforcement. The 'identity_locked' exit option for nonconformist homeowners reflects that their self-concept is often tied to personal expression, making conformity a high personal cost and exit (selling their home) a significant disruption.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a simple 'Rope' for coordination. While some coordination exists (e.g., shared infrastructure), the high suppression and extractiveness from specific groups, coupled with the broad scope of behavioral control, indicate a function beyond mere coordination. The persistence of rules enforcing subjective aesthetics, even when their link to property value is tenuous, suggests a drift towards extraction and control, rather than pure coordination or a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_value_link_ambiguity,
    'To what extent do the enforced aesthetic and behavioral rules genuinely impact property values, versus reflecting subjective preferences of the HOA board or majority?',
    'Independent, longitudinal economic studies comparing property value trends in similar communities with varying levels of aesthetic and behavioral control, controlling for other market factors.',
    'If the link is weak or non-existent, it strengthens the Snare classification by exposing the ''property value maximization'' justification as a cover for control. If the link is strong, it would push the constraint closer to a Tangled Rope by demonstrating a genuine, albeit extractive, coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_value_link_ambiguity, empirical, 'Empirical link between covenant rules and property value.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (e.g., legal enforceability of fines, difficulty of exit) or internalized (e.g., social pressure, desire to avoid conflict, identity fusion with ''good neighbor'' ideal)?',
    'Post-exit surveys and interviews with former residents, or analysis of resistance patterns: if suppression persists as self-censorship even after direct enforcement is removed, it indicates internalized components.',
    'If internalized suppression is a significant component, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. This would deepen the Snare classification by highlighting the psychological costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism.').

omega_variable(
    framing_underdetermination_behavioral_control,
    'Is this constraint best framed as a ''behavioral control'' mechanism, or is that framing itself an interpretation that overemphasizes its coercive aspects compared to its ''coordination'' or ''extraction'' functions?',
    'Analysis of legal challenges and resident complaints: if the majority of disputes center on subjective aesthetic or lifestyle rules, the behavioral control framing is corroborated. If disputes primarily concern infrastructure or financial matters, alternative framings gain strength.',
    'If the behavioral control framing is robust, the Snare classification holds. If an alternative framing (e.g., coordination) is more accurate, the classification might shift to Tangled Rope or even Rope, depending on the balance of benefits and costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_behavioral_control, conceptual, 'Ambiguity in framing the HOA covenant''s primary function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__behavioral_control_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__behavioral_control_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 15, 0.44).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__behavioral_control_reading, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__behavioral_control_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
