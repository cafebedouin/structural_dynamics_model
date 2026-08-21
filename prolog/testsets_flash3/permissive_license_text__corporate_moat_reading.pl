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
 *   constraint_id: permissive_license_text__corporate_moat_reading
 *   human_readable: Permissive License Text (Corporate Moat Reading)
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This constraint story analyzes permissive open-source license texts
 *   (e.g., MIT, Apache 2.0) from the 'corporate moat' reading. In this
 *   reading, the relaxation of copyright restrictions, while ostensibly
 *   promoting freedom, primarily enables enterprise corporations to extract
 *   value from publicly developed software without reciprocal obligations.
 *   This leads to uncompensated extraction for proprietary derivative
 *   products, effectively turning public goods into private profit centers.
 *   The constraint is claimed as a 'snare' because its coordination function
 *   (enabling reuse) is overshadowed by its extractive consequences for
 *   maintainers and the broader open-source community.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__corporate_moat_reading, 0.68).
domain_priors:suppression_score(permissive_license_text__corporate_moat_reading, 0.75).
domain_priors:theater_ratio(permissive_license_text__corporate_moat_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(permissive_license_text__corporate_moat_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__corporate_moat_reading, snare).
narrative_ontology:human_readable(permissive_license_text__corporate_moat_reading, "Permissive License Text (Corporate Moat Reading)").
narrative_ontology:topic_domain(permissive_license_text__corporate_moat_reading, "software_licensing/intellectual_property/technology_governance").

domain_priors:requires_active_enforcement(permissive_license_text__corporate_moat_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff').
narrative_ontology:cs_kernel_codification('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', fixed_text).
narrative_ontology:cs_authority_grounding('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', practice).
narrative_ontology:cs_interpretation_layer_present('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff').
narrative_ontology:cs_reading_relation('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', foundational, uncompensated_extraction_is_structural).
narrative_ontology:cs_axiom_status(uncompensated_extraction_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', uncompensated_extraction_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', foundational, corporate_leverage_distorts_commons).
narrative_ontology:cs_axiom_status(corporate_leverage_distorts_commons, holdable).
narrative_ontology:cs_axiom_grounding('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', corporate_leverage_distorts_commons, empirically_contingent).
narrative_ontology:cs_reference_frame('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', unfettered_commercial_reuse).
narrative_ontology:cs_drift_state('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', contemporary_open_source_economy, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('73df8a51-de8d-4bc3-85a9-d8f3e0cc51ff', '').
narrative_ontology:cs_kernel_id(permissive_license_text__corporate_moat_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__corporate_moat_reading, enterprise_corporations).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, individual_maintainers).
narrative_ontology:constraint_victim(permissive_license_text__corporate_moat_reading, open_source_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize permissively licensed open-source software in proprietary products without obligation to contribute back improvements or source code. They benefit from reduced development costs and accelerated time-to-market, effectively building commercial moats on public infrastructure.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, enterprise_corporations, beneficiary,
    institutional, generational, arbitrage, global).

% Invest significant unpaid labor into creating and maintaining permissively licensed software. They bear the costs of development, bug fixing, and community support, often seeing their work incorporated into commercial products without compensation or recognition beyond initial attribution.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, individual_maintainers, payer,
    powerless, biographical, identity_locked, global).

% Contributes to and relies on open-source projects. While benefiting from the availability of code, they collectively bear the cost of uncompensated labor and the risk of projects atrophying due to maintainer burnout, as corporate beneficiaries often do not contribute proportionally.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_community, payer,
    organized, generational, constrained, global).

% Analyze the long-term effects of permissive licensing on innovation, economic equity, and the sustainability of open-source ecosystems. They identify the structural mechanisms by which permissive licenses facilitate value transfer from public goods to private profit.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates widespread adoption and integration of software components by minimizing legal barriers to reuse, enabling diverse projects to build upon common foundations without complex licensing negotiations.
% TRANSFER_FUNCTION: Transfers the value of unpaid labor and intellectual property from individual and community open-source contributors to proprietary software developers and enterprise corporations, who then monetize derivative products without reciprocal obligations.
% ABSENT_VOICES: The original intent of some early permissive license authors, who may not have fully foreseen the scale of corporate extraction, is absent from contemporary discussions that focus on current economic realities. Also, the collective voice of future maintainers burdened by uncompensated work.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, replaced by copyleft or more restrictive terms, enterprise corporations would face significantly higher development costs or be forced to contribute back to the commons. The flow of value would reverse, leading to a major reorganization of the software industry's economic model.
% FOUNDING_PROBLEM: Proprietary software development was slow and costly, with legal friction hindering collaboration and reuse of code. Permissive licenses aimed to accelerate innovation by removing barriers to adoption.
% FOUNDING_PROBLEM_CORROBORATION: While the initial problem of legal friction was solved, the current status is 'dead' because the primary beneficiaries (enterprise corporations) now leverage the 'freedom' of permissive licenses to extract value without contributing proportionally, a problem not foreseen or intended by the original framers. Legal scholars and open-source advocates outside the benefiting corporations corroborate this shift, pointing to the unsustainability of uncompensated labor.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(permissive_license_text__corporate_moat_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__corporate_moat_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.68) is high because corporations gain significant economic value (reduced R&D, faster time-to-market) from using permissively licensed code without having to share their improvements or pay for the underlying labor. Suppression (0.75) is also high, as individual maintainers often feel compelled by community norms or career pressures to continue contributing, even when uncompensated, and lack the legal or economic power to demand reciprocity. The 'freedom' offered by permissive licenses effectively suppresses the ability of maintainers to capture value from their work. Theater ratio (0.20) is low, as the licenses are genuinely functional in enabling reuse, but the narrative of 'universal freedom' can be seen as a theatrical cover for the underlying extractive dynamic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of enterprise corporations, permissive licenses are a 'rope' or 'scaffold' that enables efficient innovation and collaboration. From the perspective of individual maintainers and the open-source community, the same licenses operate as a 'snare,' facilitating uncompensated labor and value transfer. The engine's classification will highlight this divergence based on the declared beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are clear beneficiaries (d near 0.0) as they gain significant value without direct cost. Individual maintainers and the open-source community are victims/payers (d near 1.0) as they bear the costs of development and maintenance without proportional compensation. The 'permissive' nature of the license, while appearing neutral, structurally directs benefits to one group and costs to another.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to foster collaboration and innovation by reducing legal friction. However, in this reading, the problem it was built to solve (slow, costly proprietary development) is 'dead' in the sense that the solution has created a new problem: uncompensated extraction. The classification as a 'snare' prevents mislabeling this as pure coordination, highlighting the shift from its original intent to its current extractive function. The persistence is due to the concentrated benefits for corporations and the diffuse, identity-locked costs for maintainers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    value_capture_mechanism,
    'To what extent is the ''uncompensated extraction'' a direct consequence of the license text itself, versus a result of broader economic structures and power asymmetries in the software industry?',
    'Comparative analysis of projects under permissive vs. copyleft licenses within similar market contexts, alongside economic studies on the distribution of value in open-source ecosystems.',
    'If primarily due to the license text, then license reform (e.g., stronger reciprocity clauses) would be a direct solution. If primarily due to broader economic structures, then policy interventions beyond licensing (e.g., antitrust, labor protections) would be necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(value_capture_mechanism, empirical, 'Distinguishing between license-driven and market-driven extraction.').

omega_variable(
    sustainability_of_uncompensated_labor,
    'Is the current model of uncompensated open-source labor, facilitated by permissive licenses, sustainable in the long term for critical infrastructure projects?',
    'Longitudinal studies of maintainer burnout rates, project abandonment, and security vulnerabilities in permissively licensed critical software, correlated with corporate contribution levels.',
    'If unsustainable, the ''snare'' classification is reinforced, indicating a systemic risk. If sustainable (e.g., through alternative funding models or intrinsic motivation), the extractive component might be lower than currently assessed, potentially shifting towards a ''tangled_rope'' or even ''rope'' if benefits are widely distributed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_of_uncompensated_labor, empirical, 'Assessing the long-term viability of the permissive license model for maintainers.').

omega_variable(
    framing_of_freedom,
    'Is the ''freedom to use'' promoted by permissive licenses primarily a freedom for all, or a freedom for those with capital to leverage public goods?',
    'Conceptual analysis of ''freedom'' in licensing contexts, examining its practical implications for different classes of actors (individual vs. corporate, resource-rich vs. resource-poor).',
    'If ''freedom'' is primarily for capital, it reinforces the ''snare'' classification by exposing the ideological cover for extraction. If genuinely universal, it would challenge the high extractiveness score.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_of_freedom, conceptual, 'Examining the beneficiaries of ''freedom'' in permissive licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__corporate_moat_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__corporate_moat_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__corporate_moat_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__corporate_moat_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__corporate_moat_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__corporate_moat_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__corporate_moat_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__corporate_moat_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__corporate_moat_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__corporate_moat_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t0, permissive_license_text__corporate_moat_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(perm_su_t5, permissive_license_text__corporate_moat_reading, suppression_requirement, 5, 0.45).
narrative_ontology:measurement(perm_su_t10, permissive_license_text__corporate_moat_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(perm_su_t15, permissive_license_text__corporate_moat_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(perm_su_t20, permissive_license_text__corporate_moat_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, commons_coordination_reading).
narrative_ontology:affects_constraint(permissive_license_text__corporate_moat_reading, copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel, focusing on its extractive aspects for corporations. It is linked to sibling readings that emphasize coordination or propose copyleft as an alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
