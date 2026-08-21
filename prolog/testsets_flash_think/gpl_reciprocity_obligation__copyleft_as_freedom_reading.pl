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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft as Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'copyleft as freedom' reading of
 *   the GPL reciprocity obligation. From this perspective, the GPL is a vital
 *   mechanism to preserve user freedoms by legally preventing proprietary
 *   capture of software. It ensures that any modifications or derivative
 *   works distributed must also be free, creating a perpetually expanding
 *   commons. The constraint is seen as a 'rope' because it coordinates
 *   collective action for a shared benefit (software freedom), even though it
 *   imposes significant obligations on proprietary integrators. The high
 *   extractiveness and suppression metrics reflect the cost imposed on those
 *   who would privatize software, which is precisely the mechanism by which
 *   freedom is preserved for others.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.8).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.9).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft as Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'bb80fbb7-da69-4166-8410-23a22d25c04b').
narrative_ontology:cs_kernel_codification('bb80fbb7-da69-4166-8410-23a22d25c04b', fixed_text).
narrative_ontology:cs_authority_grounding('bb80fbb7-da69-4166-8410-23a22d25c04b', practice).
narrative_ontology:cs_interpretation_layer_present('bb80fbb7-da69-4166-8410-23a22d25c04b').
narrative_ontology:cs_reading_relation('bb80fbb7-da69-4166-8410-23a22d25c04b', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb80fbb7-da69-4166-8410-23a22d25c04b', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('bb80fbb7-da69-4166-8410-23a22d25c04b', foundational, software_must_be_free).
narrative_ontology:cs_axiom_status(software_must_be_free, holdable).
narrative_ontology:cs_axiom_grounding('bb80fbb7-da69-4166-8410-23a22d25c04b', software_must_be_free, deontological).
narrative_ontology:cs_axiom('bb80fbb7-da69-4166-8410-23a22d25c04b', foundational, proprietary_capture_is_harmful).
narrative_ontology:cs_axiom_status(proprietary_capture_is_harmful, holdable).
narrative_ontology:cs_axiom_grounding('bb80fbb7-da69-4166-8410-23a22d25c04b', proprietary_capture_is_harmful, instrumental).
narrative_ontology:cs_reference_frame('bb80fbb7-da69-4166-8410-23a22d25c04b', unrestricted_user_freedom_through_reciprocity).
narrative_ontology:cs_drift_state('bb80fbb7-da69-4166-8410-23a22d25c04b', contemporary_software_ecosystem, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('bb80fbb7-da69-4166-8410-23a22d25c04b', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, corporate_legal_departments).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_freedom_principle).
narrative_ontology:constraint_vindicates(gpl_reciprocity_obligation__copyleft_as_freedom_reading, anti_enclosure_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promotes, defends, and enforces the GPL to ensure software freedom and prevent proprietary capture. They see the license as a vital tool for collective software development and user empowerment.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_community, agenda_setter,
    organized, generational, mobile, global).

% Benefit from the freedom to use, study, share, and modify software, guaranteed by the GPL. They are protected from proprietary lock-in and have access to the source code of their tools.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users, beneficiary,
    moderate, biographical, mobile, global).

% Benefit from a large pool of freely available, modifiable code, fostering collaboration and innovation. They contribute to and build upon a shared commons, protected from proprietary appropriation.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, software_developers, beneficiary,
    moderate, biographical, mobile, global).

% Are compelled by the GPL to release their modifications to copylefted software under compatible free licenses if they distribute it. This prevents them from privatizing improvements and limits their business models based on proprietary extensions.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators, payer,
    powerful, biographical, constrained, global).

% Bear the cost of ensuring compliance with GPL terms, including legal review, open-sourcing modifications, and managing intellectual property. They face legal risks if non-compliance is detected.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, corporate_legal_departments, payer,
    institutional, biographical, constrained, global).

% Organizations like the Free Software Foundation (FSF) actively monitor, educate, and, when necessary, litigate to enforce the GPL's terms, ensuring its viral nature is maintained. They act as stewards of software freedom.
narrative_ontology:constraint_stakeholder(gpl_reciprocity_obligation__copyleft_as_freedom_reading, licensing_enforcement_bodies, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_reciprocity_obligation__copyleft_as_freedom_reading, diffuse).
narrative_ontology:fixing_cost_class(gpl_reciprocity_obligation__copyleft_as_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legal framework that coordinates software development around a shared, perpetually free code commons, ensuring that all users retain fundamental freedoms.
% TRANSFER_FUNCTION: Transfers the obligation to share source code and modifications from those who distribute GPL-licensed software to all subsequent distributors, preventing the privatization of collective work.
% ABSENT_VOICES: Proprietary software companies and business models that rely on integrating open-source components without contributing back their modifications are structurally excluded by the license's terms. They would argue for less restrictive licensing to enable proprietary innovation.
% DISAPPEARANCE_RATIONALE: If the GPL and its enforcement vanished overnight, proprietary forks of currently free software would proliferate rapidly. This would fragment the open-source ecosystem, reduce user freedom, and lead to widespread proprietary capture of collectively developed software, fundamentally reorganizing the software industry.
% FOUNDING_PROBLEM: Early software development saw a trend where proprietary vendors would take publicly available code, make improvements, and then distribute their enhanced versions under restrictive proprietary licenses, effectively privatizing collective work and eroding user freedoms.
% FOUNDING_PROBLEM_CORROBORATION: The open-source community and many independent developers attest that the threat of proprietary enclosure remains live, citing ongoing attempts to circumvent copyleft. Proprietary integrators would contest this, claiming it stifles innovation.
narrative_ontology:disappearance_verdict(gpl_reciprocity_obligation__copyleft_as_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_reciprocity_obligation__copyleft_as_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.8) is high because the GPL imposes a significant cost on proprietary integrators by forcing them to open-source their modifications, thereby 'extracting' their ability to create proprietary derivative works. Suppression (0.9) is also high, as the license actively suppresses alternative licensing models (i.e., proprietary ones) for derivative works, and its persistence relies on active legal enforcement. Theater ratio is low (0.1) because the license is highly functional and directly achieves its stated goal of preventing proprietary capture; there is little performative maintenance. Accessibility collapse (0.85) is high for proprietary integrators, as their preferred alternative (proprietary integration) is largely foreclosed. Resistance (0.7) is substantial, as proprietary interests continuously seek ways to circumvent or challenge the GPL.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the open-source community and downstream users, the GPL is a freedom-preserving 'rope' that enables a vibrant, collaborative ecosystem. From the perspective of proprietary integrators, it is a highly restrictive 'snare' that extracts their ability to monetize software through traditional proprietary means. The engine's classification will reflect this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream users, software developers, and the open-source community are clear beneficiaries (low directionality) as the constraint directly secures their freedoms and fosters their collaborative environment. Proprietary integrators and corporate legal departments are targets (high directionality) as the constraint extracts from them the right to privatize and imposes compliance costs. Licensing enforcement bodies act as agenda-setters, actively maintaining the constraint for the benefit of the community.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of the GPL, from this reading, is to prevent proprietary capture and preserve software freedom. This mandate is still very much 'live' (as indicated in the six_questions section), as proprietary interests continue to seek ways to enclose software. Therefore, the constraint is not suffering from mandatrophy; its function remains highly relevant and actively defended.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately represented as the ''copyleft_as_freedom_reading'' of the ''gpl_reciprocity_obligation'' kernel?',
    'Analysis of the specific arguments and legal interpretations used by proponents of software freedom, compared against the structural effects of the GPL.',
    'If this reading is not the dominant or intended interpretation, the classification of the constraint''s purpose and beneficiaries would shift, potentially altering its claimed type and the perceived legitimacy of its extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms the specific reading being instantiated for the GPL kernel.').

omega_variable(
    sibling_reading_delta_restriction,
    'How would the classification change if the ''copyleft_as_restriction_reading'' were adopted?',
    'Re-authoring the constraint story from the perspective of proprietary integrators, focusing on the constraint''s impact on their business models and freedom to innovate privately.',
    'The ''copyleft_as_restriction_reading'' would likely classify the constraint as a ''snare'' or ''tangled_rope'' from the perspective of proprietary integrators, with higher extractiveness and suppression values, and a different set of beneficiaries/victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_delta_restriction, conceptual, 'Examines the structural differences from the ''copyleft as restriction'' perspective.').

omega_variable(
    sibling_reading_delta_commons,
    'How would the classification change if the ''copyleft_as_commons_reading'' were adopted?',
    'Re-authoring the constraint story from the perspective of institutional design for commons governance, focusing on the GPL as a mechanism for preventing enclosure.',
    'The ''copyleft_as_commons_reading'' would likely emphasize the coordination function more strongly, potentially classifying it as a ''rope'' or ''tangled_rope'' with a focus on the collective benefit of a shared resource, and a different framing of beneficiaries (the commons itself, rather than individual users).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_commons, conceptual, 'Examines the structural differences from the ''copyleft as commons'' perspective.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 1991, 2021).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl__tr_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1991, 0.1).
narrative_ontology:measurement(gpl__tr_t1997, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 1997, 0.1).
narrative_ontology:measurement(gpl__tr_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2003, 0.1).
narrative_ontology:measurement(gpl__tr_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2009, 0.1).
narrative_ontology:measurement(gpl__tr_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(gpl__tr_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 2021, 0.1).

% Extraction over time
narrative_ontology:measurement(gpl__be_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1991, 0.6).
narrative_ontology:measurement(gpl__be_t1997, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 1997, 0.68).
narrative_ontology:measurement(gpl__be_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2003, 0.75).
narrative_ontology:measurement(gpl__be_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2009, 0.78).
narrative_ontology:measurement(gpl__be_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(gpl__be_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 2021, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(gpl__su_t1991, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1991, 0.7).
narrative_ontology:measurement(gpl__su_t1997, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 1997, 0.78).
narrative_ontology:measurement(gpl__su_t2003, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2003, 0.85).
narrative_ontology:measurement(gpl__su_t2009, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2009, 0.88).
narrative_ontology:measurement(gpl__su_t2015, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2015, 0.89).
narrative_ontology:measurement(gpl__su_t2021, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 2021, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_development_practices).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_software_market_dynamics).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, lgpl_licensing_obligation).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, agpl_licensing_obligation).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'gpl_reciprocity_obligation' kernel, each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
