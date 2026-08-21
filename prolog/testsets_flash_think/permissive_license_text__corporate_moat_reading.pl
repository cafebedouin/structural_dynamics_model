% ============================================================================
% CONSTRAINT STORY: permissive_license_text__corporate_moat_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__corporate_moat_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint describes the operation of permissive open-source
 *   licenses (e.g., MIT, Apache 2.0) from the perspective of their use by
 *   enterprise corporations to build proprietary derivative products. While
 *   these licenses are often framed as maximizing freedom and innovation,
 *   this reading highlights how they enable uncompensated extraction of value
 *   from individual maintainers and the open-source commons, effectively
 *   creating a 'corporate moat' around proprietary offerings built on shared
 *   foundations. This is one reading of the 'permissive_license_text' kernel.
 *
 * KEY AGENTS:
 *   - enterprise_corporations: Primary beneficiary and agenda-setter (institutional/arbitrage)
 *   - individual_maintainers: Primary payer (moderate/constrained)
 *   - open_source_commons: Excluded victim (organized/constrained)
 *   - legal_scholars_and_advocates: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.7).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.65).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '11e7d1c1-71ef-46a6-a804-3df9e791225e').
narrative_ontology:cs_kernel_codification('11e7d1c1-71ef-46a6-a804-3df9e791225e', fixed_text).
narrative_ontology:cs_authority_grounding('11e7d1c1-71ef-46a6-a804-3df9e791225e', extraction).
narrative_ontology:cs_interpretation_layer_present('11e7d1c1-71ef-46a6-a804-3df9e791225e').
narrative_ontology:cs_reading_relation('11e7d1c1-71ef-46a6-a804-3df9e791225e', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('11e7d1c1-71ef-46a6-a804-3df9e791225e', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('11e7d1c1-71ef-46a6-a804-3df9e791225e', foundational, innovation_requires_unrestricted_reuse).
narrative_ontology:cs_axiom_status(innovation_requires_unrestricted_reuse, holdable).
narrative_ontology:cs_axiom_grounding('11e7d1c1-71ef-46a6-a804-3df9e791225e', innovation_requires_unrestricted_reuse, instrumental).
narrative_ontology:cs_reference_frame('11e7d1c1-71ef-46a6-a804-3df9e791225e', unfettered_code_reuse).
narrative_ontology:cs_drift_state('11e7d1c1-71ef-46a6-a804-3df9e791225e', contemporary_corporate_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('11e7d1c1-71ef-46a6-a804-3df9e791225e', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, open_source_commons).
narrative_ontology:constraint_vindicates(permissive_license_text__corporate_moat_reading, free_market_innovation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These corporations leverage permissive open-source licenses to incorporate code into their proprietary products without obligation for reciprocal contribution. They benefit from reduced development costs and market dominance, actively shaping legal interpretations and industry norms to maintain this advantage.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, agenda_setter,
    institutional, generational, arbitrage, global).

% These are the primary creators and maintainers of open-source software released under permissive licenses. Their work is often incorporated into proprietary products without direct compensation or required reciprocal contribution, limiting their ability to build independent, sustainable projects or capture value from their innovations.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    moderate, biographical, constrained, global).

% The collective body of open-source software, knowledge, and community. It is diminished when contributions are siphoned into proprietary silos without reciprocal benefit, leading to a net outflow of value and potential fragmentation of the shared resource.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_commons, excluded,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_non_agent(permissive_license_text__corporate_moat_reading, open_source_commons).

% Academics and legal professionals who analyze the implications of software licensing, often advocating for policy changes or alternative licensing models to address perceived imbalances in value capture and contribution.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:fixing_cost_class(permissive_license_text__corporate_moat_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates broad adoption and integration of software components by minimizing legal friction for reuse, enabling a wide range of derivative works and fostering a large ecosystem of shared code.
% TRANSFER_FUNCTION: Transfers value (code, innovation, maintenance effort, and market share) from individual open-source contributors and the collective open-source commons to proprietary product developers, without requiring reciprocal contribution or direct compensation.
% ABSENT_VOICES: Advocates for strong copyleft licenses (e.g., GPL) or alternative economic models for open source are often marginalized. They would argue for mandatory reciprocity or direct compensation to ensure the commons is sustained and exploitation is prevented.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, the software ecosystem would undergo a fundamental reorganization. Corporations would face significantly higher costs for software components (either through licensing fees or in-house development), or be forced to adopt copyleft licenses, leading to a different distribution of value and innovation within the industry.
% FOUNDING_PROBLEM: The original problem was to maximize software reuse and interoperability by reducing legal friction associated with traditional copyright, thereby fostering a vibrant ecosystem of shared code and accelerating innovation.
% FOUNDING_PROBLEM_CORROBORATION: Corporate legal teams and business development departments attest to the ongoing need for frictionless reuse to drive innovation. However, open-source advocates and some legal scholars, from outside the benefiting corporations, argue that while reuse is still valuable, the founding problem is now largely solved, and the arrangement primarily serves as a mechanism for rent collection.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__corporate_moat_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(permissive_license_text__corporate_moat_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` (0.7) is high because corporations gain significant value from using open-source code without direct compensation or reciprocal contribution, effectively externalizing development costs. `Suppression` (0.65) is substantial, as the legal framework and market power of corporations limit the ability of individual maintainers to demand compensation or enforce reciprocal terms. `Theater_ratio` (0.4) reflects the narrative that permissive licenses are purely about 'freedom' and 'innovation,' which partially masks the underlying extractive dynamics. `Accessibility_collapse` (0.5) indicates that while alternatives exist (e.g., creating copyleft projects), the dominant market structure makes them less accessible or impactful for individual maintainers. `Resistance` (0.4) is present but often diffuse and difficult to organize against powerful corporate interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of enterprise corporations, permissive licenses are a 'rope' that facilitates innovation and broad adoption, minimizing legal friction. From the perspective of individual maintainers and open-source advocates, the same licenses function as a 'snare,' enabling uncompensated extraction and the enclosure of shared resources into proprietary products. The engine's classification will highlight this divergence based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are clear beneficiaries and agenda-setters, as they directly profit from the uncompensated use of code and influence the legal and market environment. Individual maintainers are payers, bearing the cost of their labor being incorporated into proprietary products without direct return. The 'open_source_commons' is a victim, as its collective value is diminished. Legal scholars and advocates serve as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of permissive licenses was to maximize software reuse and foster innovation by reducing legal friction. However, this reading argues that the constraint has drifted, and its function has atrophied into a mechanism for corporate extraction. The 'freedom' narrative now serves as a theatrical cover for what has become a snare, where the founding problem (fragmented software development) is largely solved, but the structure persists to benefit specific actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permissive_vs_copyleft_efficacy,
    'Is permissive licensing inherently extractive, or is it the lack of strong copyleft alternatives that enables the observed extraction?',
    'Comparative analysis of software ecosystems with dominant permissive vs. copyleft licensing, examining long-term value flows, contributor sustainability, and market concentration.',
    'If inherently extractive, policy interventions might focus on modifying permissive license terms or promoting alternative economic models. If the lack of copyleft is key, then strengthening copyleft enforcement or promoting its adoption would be the focus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permissive_vs_copyleft_efficacy, conceptual, 'Examines whether the extractive nature is intrinsic to permissive licenses or contingent on the broader licensing landscape.').

omega_variable(
    corporate_intent_vs_structural_outcome,
    'Is the uncompensated extraction by corporations a deliberate intent to exploit, or an emergent structural outcome of the permissive license terms and market dynamics?',
    'Analysis of corporate internal documents, public statements, and historical business strategies regarding open-source engagement, alongside economic modeling of market incentives.',
    'If deliberate, it suggests a need for stronger regulatory oversight and anti-monopoly measures. If emergent, it points to systemic issues in intellectual property law and market design that require structural reform.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corporate_intent_vs_structural_outcome, empirical, 'Distinguishes between intentional exploitation and systemic consequences of license design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(perm_tr_t4, permissive_license_text__corporate_moat_reading, theater_ratio, 4, 0.26).
narrative_ontology:measurement(perm_tr_t8, permissive_license_text__corporate_moat_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(perm_tr_t12, permissive_license_text__corporate_moat_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(perm_tr_t16, permissive_license_text__corporate_moat_reading, theater_ratio, 16, 0.39).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(perm_be_t4, permissive_license_text__corporate_moat_reading, base_extractiveness, 4, 0.56).
narrative_ontology:measurement(perm_be_t8, permissive_license_text__corporate_moat_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(perm_be_t12, permissive_license_text__corporate_moat_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement(perm_be_t16, permissive_license_text__corporate_moat_reading, base_extractiveness, 16, 0.69).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(perm_su_t4, permissive_license_text__corporate_moat_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(perm_su_t8, permissive_license_text__corporate_moat_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(perm_su_t12, permissive_license_text__corporate_moat_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(perm_su_t16, permissive_license_text__corporate_moat_reading, suppression_requirement, 16, 0.64).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__corporate_moat_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, open_source_funding_models).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, proprietary_software_market_concentration).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel, focusing on its extractive use by corporations. Sibling readings include 'commons_coordination_reading' (emphasizing universal implementation freedom) and 'copyleft_counterfactual_reading' (emphasizing the need for reciprocity to prevent exploitation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
