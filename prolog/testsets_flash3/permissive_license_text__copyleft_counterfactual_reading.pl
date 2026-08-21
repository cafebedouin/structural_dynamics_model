% ============================================================================
% CONSTRAINT STORY: permissive_license_text__copyleft_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__copyleft_counterfactual_reading, []).

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
 *   constraint_id: permissive_license_text__copyleft_counterfactual_reading
 *   human_readable: Permissive License Text (Copyleft Counterfactual Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story represents the 'copyleft counterfactual reading' of
 *   permissive license text. From this perspective, the absence of a
 *   reciprocity requirement in permissive licenses (like MIT or Apache) is
 *   not a feature but a structural flaw that enables proprietary entities to
 *   exploit the open-source commons without contributing back. The
 *   constraint, therefore, is the *implicit permission* granted by permissive
 *   licenses, which is seen as extractive because it allows for uncompensated
 *   enclosure. The 'viral' nature of copyleft (e.g., GPL) is presented as the
 *   necessary alternative to counteract this exploitation. This reading
 *   frames the permissive license as a 'tangled rope' because it facilitates
 *   coordination (sharing code) but with an asymmetric extraction
 *   (proprietary enclosure).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, 0.78).
domain_priors:suppression_score(permissive_license_text__copyleft_counterfactual_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__copyleft_counterfactual_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(permissive_license_text__copyleft_counterfactual_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__copyleft_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(permissive_license_text__copyleft_counterfactual_reading, "Permissive License Text (Copyleft Counterfactual Reading)").
narrative_ontology:topic_domain(permissive_license_text__copyleft_counterfactual_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__copyleft_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__copyleft_counterfactual_reading, 'ba350120-8543-440f-be8e-94112a921584').
narrative_ontology:cs_kernel_codification('ba350120-8543-440f-be8e-94112a921584', fixed_text).
narrative_ontology:cs_authority_grounding('ba350120-8543-440f-be8e-94112a921584', practice).
narrative_ontology:cs_interpretation_layer_present('ba350120-8543-440f-be8e-94112a921584').
narrative_ontology:cs_reading_relation('ba350120-8543-440f-be8e-94112a921584', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('ba350120-8543-440f-be8e-94112a921584', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_axiom('ba350120-8543-440f-be8e-94112a921584', foundational, reciprocity_is_foundational_to_commons).
narrative_ontology:cs_axiom_status(reciprocity_is_foundational_to_commons, holdable).
narrative_ontology:cs_axiom_grounding('ba350120-8543-440f-be8e-94112a921584', reciprocity_is_foundational_to_commons, deontological).
narrative_ontology:cs_axiom('ba350120-8543-440f-be8e-94112a921584', foundational, unrestricted_use_enables_exploitation).
narrative_ontology:cs_axiom_status(unrestricted_use_enables_exploitation, holdable).
narrative_ontology:cs_axiom_grounding('ba350120-8543-440f-be8e-94112a921584', unrestricted_use_enables_exploitation, empirically_contingent).
narrative_ontology:cs_reference_frame('ba350120-8543-440f-be8e-94112a921584', gpl_v2_era_commons_protection).
narrative_ontology:cs_drift_state('ba350120-8543-440f-be8e-94112a921584', contemporary_permissive_license_dominance, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('ba350120-8543-440f-be8e-94112a921584', '').
narrative_ontology:cs_kernel_id(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates).
narrative_ontology:constraint_beneficiary(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies).
narrative_ontology:constraint_victim(permissive_license_text__copyleft_counterfactual_reading, derivative_product_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of copyleft licenses (e.g., GPL) who argue that permissive licenses enable exploitation by allowing proprietary derivatives without contributing back to the commons. They actively enforce copyleft licenses to ensure reciprocity.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, copyleft_advocates, agenda_setter,
    organized, generational, constrained, global).

% Companies that build proprietary software products, often incorporating permissively licensed components. From this reading, they are 'victims' of the copyleft constraint, as it forces them to open-source their derivatives or avoid copylefted code, limiting their ability to capture value exclusively.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, proprietary_software_companies, payer,
    powerful, biographical, constrained, global).

% Individual developers or smaller teams who wish to build proprietary derivative products from permissively licensed code. They face the choice of either complying with copyleft's reciprocity or avoiding such code, which can limit their technical options.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, derivative_product_developers, payer,
    moderate, biographical, constrained, global).

% Organizations that promote and defend the open-source ecosystem, often aligning with copyleft principles to ensure the growth of the commons. They benefit from the 'viral' nature of copyleft, which expands the pool of freely available code.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, open_source_foundations, beneficiary,
    institutional, generational, mobile, global).

% Developers who choose permissive licenses (e.g., MIT, Apache) for their code, believing it maximizes adoption and freedom. From the copyleft counterfactual reading, their choice inadvertently enables exploitation, but they are not part of the copyleft enforcement conversation.
narrative_ontology:constraint_stakeholder(permissive_license_text__copyleft_counterfactual_reading, permissive_license_authors, excluded,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that improvements and derivatives of open-source software remain open and contribute back to the shared commons, preventing proprietary enclosure of publicly developed code.
% TRANSFER_FUNCTION: Transfers the obligation of reciprocity (sharing source code for derivatives) from proprietary developers back to the open-source commons, ensuring that the 'freedom to use' is preserved for all future users.
% ABSENT_VOICES: Developers who prefer permissive licenses are largely excluded from the copyleft enforcement discourse, as their licenses do not impose reciprocity. They would argue that maximizing adoption, even by proprietary users, is a valid form of coordination.
% DISAPPEARANCE_RATIONALE: If copyleft licenses and their enforcement vanished, a significant portion of the open-source ecosystem would likely be absorbed into proprietary products without contributing back, leading to a 'tragedy of the commons' for software development and a fundamental shift in the balance of power between open and proprietary models.
% FOUNDING_PROBLEM: The problem of proprietary software companies taking open-source code, making improvements, and then closing off those improvements, effectively privatizing collective work and undermining the open-source ethos.
% FOUNDING_PROBLEM_CORROBORATION: The Free Software Foundation and various open-source legal experts consistently attest to the ongoing nature of this problem, citing numerous instances of proprietary enclosure. Independent legal analysis of software licensing trends also corroborates the persistent tension between permissive and copyleft approaches.
narrative_ontology:disappearance_verdict(permissive_license_text__copyleft_counterfactual_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__copyleft_counterfactual_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__copyleft_counterfactual_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(permissive_license_text__copyleft_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__copyleft_counterfactual_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__copyleft_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__copyleft_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the 'cost' to the commons (loss of open derivatives) is substantial, while proprietary developers gain exclusive value. Suppression (0.65) reflects the legal and social pressure exerted by copyleft advocates to enforce reciprocity, which acts as a counter-suppression against proprietary enclosure. Theater ratio is low (0.20) as the debate is highly functional and ideological, with little performative maintenance. Accessibility collapse is moderate (0.40) because proprietary developers still have the 'option' to comply with copyleft or avoid copylefted code, but this choice is constrained by market and technical realities. Resistance (0.70) is high due to ongoing legal challenges and ideological debates between copyleft and permissive license proponents.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who see permissive licensing as maximizing freedom and those (like this reading) who see it as enabling exploitation. The engine's classification will highlight how the same 'permissive' text can be read as a coordination mechanism (by permissive advocates) or an extractive one (by copyleft advocates), depending on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyleft advocates and open-source foundations are beneficiaries (d near 0.0) as the constraint (viral reciprocity) ensures the growth and protection of the commons they champion. Proprietary software companies and derivative product developers are targets (d near 1.0) as they are forced to either contribute their derivatives or avoid copylefted code, which is seen as an extraction from their potential proprietary gains. Permissive license authors are excluded from this specific enforcement dynamic, though their choices are central to the debate.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading argues that the 'mandate' of permissive licenses (to maximize adoption) has been co-opted to enable extraction. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring the extraction) or a Snare (ignoring the coordination function of code sharing). It highlights the hybrid nature where a genuine coordination function (code sharing) is intertwined with an asymmetric extraction (proprietary enclosure without reciprocity).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_proprietary_enclosure,
    'What is the quantifiable economic and social cost of proprietary enclosure of open-source derivatives that do not contribute back to the commons?',
    'Longitudinal studies tracking the economic impact of different licensing models on innovation, market concentration, and the growth of the open-source ecosystem.',
    'A high quantifiable cost would strengthen the argument for copyleft''s necessity and increase the measured extractiveness of permissive licenses; a low cost would weaken it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_proprietary_enclosure, empirical, 'Quantifying the impact of proprietary enclosure on the open-source commons.').

omega_variable(
    freedom_definition_ambiguity,
    'Is ''freedom'' in software primarily about the freedom to use and modify (as in permissive licenses) or the freedom to ensure future freedom for all users (as in copyleft)?',
    'This is a conceptual and preference-based question, resolvable only through philosophical debate and community consensus on the definition of ''software freedom''.',
    'If ''freedom to ensure future freedom'' is prioritized, this reading''s classification as extractive is strengthened; if ''freedom to use without restriction'' is prioritized, the ''commons_coordination_reading'' gains legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''software freedom'' between permissive and copyleft philosophies.').

omega_variable(
    enforcement_efficacy_of_copyleft,
    'How effective is the active enforcement of copyleft licenses in actually compelling reciprocity or deterring proprietary enclosure?',
    'Analysis of legal cases, compliance rates, and the actual ''viral'' spread of copylefted code into proprietary projects over time.',
    'Higher efficacy would increase the measured suppression and extractiveness of this constraint; lower efficacy would suggest it is more of a ''piton'' or ''theater'' than an actively enforced mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_efficacy_of_copyleft, empirical, 'Measuring the real-world impact and enforcement success of copyleft licenses.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__copyleft_counterfactual_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__copyleft_counterfactual_reading, theater_ratio, 25, 0.2).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 15, 0.74).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__copyleft_counterfactual_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 15, 0.62).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(perm_su_t25, permissive_license_text__copyleft_counterfactual_reading, suppression_requirement, 25, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__copyleft_counterfactual_reading, identity_coordination).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__copyleft_counterfactual_reading, permissive_license_text__corporate_moat_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'permissive_license_text' kernel. This 'copyleft_counterfactual_reading' focuses on the extractive potential of permissive licenses, contrasting with the 'commons_coordination_reading' (which emphasizes freedom of use) and the 'corporate_moat_reading' (which views permissive licenses as enabling corporate value capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
