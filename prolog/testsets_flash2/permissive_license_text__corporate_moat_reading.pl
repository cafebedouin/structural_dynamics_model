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
 *   This constraint describes the operation of permissive open-source
 *   licenses (e.g., MIT, Apache 2.0) from the perspective of how they enable
 *   enterprise corporations to extract value without reciprocal contribution,
 *   effectively creating proprietary 'moats' around publicly developed
 *   software. While initially intended to foster collaboration, this reading
 *   highlights the asymmetric power dynamics that allow corporations to
 *   benefit disproportionately from the uncompensated labor of individual
 *   maintainers and the broader open-source community. The constraint is
 *   claimed as a 'snare' because its coordination story (reducing friction)
 *   serves as cover for a system that systematically extracts from
 *   identifiable victims.
 *
 * KEY AGENTS:
 *   - enterprise_corporations: Primary beneficiary (institutional/arbitrage) — extracts value without reciprocity.
 *   - individual_maintainers: Primary target (powerless/identity_locked) — provides uncompensated labor.
 *   - open_source_community: Secondary target (organized/constrained) — bears collective costs of free-riding.
 *   - legal_scholars: Analytical observer (analytical/analytical) — identifies structural extraction.
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
narrative_ontology:cs_story_uid(permissive_license_text__corporate_moat_reading, '35423db3-e2f4-4e87-9407-99c28980c518').
narrative_ontology:cs_kernel_codification('35423db3-e2f4-4e87-9407-99c28980c518', fixed_text).
narrative_ontology:cs_authority_grounding('35423db3-e2f4-4e87-9407-99c28980c518', practice).
narrative_ontology:cs_interpretation_layer_present('35423db3-e2f4-4e87-9407-99c28980c518').
narrative_ontology:cs_reading_relation('35423db3-e2f4-4e87-9407-99c28980c518', permissive_license_text__commons_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('35423db3-e2f4-4e87-9407-99c28980c518', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('35423db3-e2f4-4e87-9407-99c28980c518', foundational, unrestricted_reuse_maximizes_corporate_profit).
narrative_ontology:cs_axiom_status(unrestricted_reuse_maximizes_corporate_profit, holdable).
narrative_ontology:cs_axiom_grounding('35423db3-e2f4-4e87-9407-99c28980c518', unrestricted_reuse_maximizes_corporate_profit, instrumental).
narrative_ontology:cs_axiom('35423db3-e2f4-4e87-9407-99c28980c518', secondary, attribution_is_sufficient_compensation).
narrative_ontology:cs_axiom_status(attribution_is_sufficient_compensation, holdable).
narrative_ontology:cs_axiom_grounding('35423db3-e2f4-4e87-9407-99c28980c518', attribution_is_sufficient_compensation, conventional).
narrative_ontology:cs_reference_frame('35423db3-e2f4-4e87-9407-99c28980c518', minimal_friction_maximal_adoption).
narrative_ontology:cs_drift_state('35423db3-e2f4-4e87-9407-99c28980c518', contemporary_corporate_dominance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('35423db3-e2f4-4e87-9407-99c28980c518', '').
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

% Contributes to and relies on open-source projects. While benefiting from shared code, they collectively bear the cost of uncompensated labor and the risk of projects becoming unsustainable due to corporate free-riding. Their ability to 'exit' is limited by the network effects of existing projects.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, open_source_community, payer,
    organized, generational, constrained, global).

% Analyze the long-term effects of permissive licensing on innovation, competition, and the sustainability of open-source ecosystems. They identify the structural mechanisms by which permissive licenses enable extraction.
narrative_ontology:constraint_stakeholder(permissive_license_text__corporate_moat_reading, legal_scholars, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Facilitates widespread adoption and integration of software components by minimizing legal barriers to reuse and modification, enabling diverse projects to build upon a common foundation.
% TRANSFER_FUNCTION: Transfers the value of unpaid labor and intellectual property from individual maintainers and the open-source community to enterprise corporations, who convert it into proprietary commercial value without reciprocal obligation.
% ABSENT_VOICES: Advocates for stronger reciprocity in licensing (e.g., copyleft proponents) are present in the discourse but structurally marginalized in the adoption patterns driven by corporate interests. They would argue for licenses that ensure contributions flow back to the commons.
% DISAPPEARANCE_RATIONALE: If permissive licenses vanished overnight, replaced by more restrictive or reciprocal terms, enterprise corporations would face significantly higher development costs or be forced to contribute back to the commons. The software ecosystem would rebalance towards more equitable value distribution, fundamentally altering business models and open-source project sustainability.
% FOUNDING_PROBLEM: Proprietary software models created high barriers to entry and limited interoperability, hindering innovation and collaboration. Permissive licenses aimed to reduce these friction points, fostering a more open and collaborative development environment.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of permissive licenses (often corporations) argue the problem of friction remains live, justifying minimal restrictions. Critics (individual maintainers, some legal scholars) argue the original problem is largely solved, and the current arrangement has shifted to enable new forms of extraction, supported by empirical studies of corporate contributions to open source.
narrative_ontology:disappearance_verdict(permissive_license_text__corporate_moat_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__corporate_moat_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__corporate_moat_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) is high because the value transfer from maintainers to corporations is substantial and uncompensated. Suppression (0.75) is also high, as individual maintainers are often 'identity_locked' by their commitment to open source and lack the collective power or legal means to enforce reciprocity. The 'permissive' nature of the license itself acts as a form of structural suppression against demands for reciprocal contribution. Theater ratio (0.20) is low, indicating that the license's function is genuinely to enable widespread use, but this function is increasingly co-opted for extractive purposes rather than being purely performative. The rising extractiveness and suppression over time reflect the increasing commercialization of open source and the growing power imbalance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of enterprise corporations, permissive licenses are a 'rope' that facilitates efficient software development and innovation. From the perspective of individual maintainers and the open-source community, the same licenses operate as a 'snare' that enables uncompensated extraction and undermines the sustainability of the commons. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Enterprise corporations are clear beneficiaries (d=0.0-0.1) as they gain access to free, high-quality software without obligation. Individual maintainers are targets (d=0.9-1.0) due to their uncompensated labor and identity-locked commitment. The open-source community is also a target (d=0.7-0.8) as it collectively bears the costs of maintaining the commons while facing free-riding. Legal scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate was to foster open collaboration and reduce friction. This reading argues that while the 'friction reduction' function persists, it has been co-opted to enable extraction, leading to a form of mandatrophy where the original goal is overshadowed by unintended (or intentionally leveraged) extractive outcomes. The classification as a snare prevents mislabeling this as pure coordination by highlighting the asymmetric beneficiary structure and the suppression of reciprocal alternatives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainability_of_maintainer_labor,
    'Is the current model of uncompensated individual maintainer labor sustainable in the long term, given the increasing corporate reliance on permissive open-source software?',
    'Empirical studies tracking maintainer burnout rates, project abandonment, and the financial health of open-source foundations. Analysis of funding models for critical open-source infrastructure.',
    'If unsustainable, it strengthens the ''snare'' classification by demonstrating the long-term costs borne by victims. If sustainable, it might suggest a more complex ''tangled_rope'' where benefits to maintainers (e.g., reputation, learning) partially offset the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_of_maintainer_labor, empirical, 'Assesses the long-term viability of the current open-source labor model.').

omega_variable(
    corporate_contribution_reciprocity,
    'To what extent do enterprise corporations, benefiting from permissive licenses, contribute back to the open-source projects they utilize, either through direct code contributions, funding, or developer time?',
    'Audits of corporate open-source programs, analysis of contribution metrics (e.g., pull requests, bug reports, financial sponsorships) to projects they consume, and comparison with contributions to copyleft projects.',
    'Higher reciprocal contributions would weaken the ''snare'' classification, suggesting a more balanced ''tangled_rope''. Minimal or no contributions would reinforce the ''snare'' classification by confirming uncompensated extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_contribution_reciprocity, empirical, 'Measures the actual reciprocity from corporate beneficiaries.').

omega_variable(
    structural_vs_internalized_suppression,
    'Is the suppression of reciprocal demands primarily structural (e.g., corporate power, legal frameworks) or internalized (e.g., maintainers'' ideological commitment to ''freedom to use'')?',
    'Surveys of maintainer motivations and perceptions of exploitation, analysis of legal and economic barriers to adopting more restrictive licenses, and case studies of projects attempting to shift licensing models.',
    'If primarily structural, the suppression metric accurately reflects external barriers. If significantly internalized, the effective suppression on maintainers is higher, as they self-impose constraints even when external barriers might be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Distinguishes between external and internal mechanisms of suppressing demands for reciprocity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__corporate_moat_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t1990, permissive_license_text__corporate_moat_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(perm_tr_t1998, permissive_license_text__corporate_moat_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(perm_tr_t2006, permissive_license_text__corporate_moat_reading, theater_ratio, 2006, 0.15).
narrative_ontology:measurement(perm_tr_t2014, permissive_license_text__corporate_moat_reading, theater_ratio, 2014, 0.18).
narrative_ontology:measurement(perm_tr_t2020, permissive_license_text__corporate_moat_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement(perm_tr_t2024, permissive_license_text__corporate_moat_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(perm_be_t1990, permissive_license_text__corporate_moat_reading, base_extractiveness, 1990, 0.3).
narrative_ontology:measurement(perm_be_t1998, permissive_license_text__corporate_moat_reading, base_extractiveness, 1998, 0.45).
narrative_ontology:measurement(perm_be_t2006, permissive_license_text__corporate_moat_reading, base_extractiveness, 2006, 0.55).
narrative_ontology:measurement(perm_be_t2014, permissive_license_text__corporate_moat_reading, base_extractiveness, 2014, 0.62).
narrative_ontology:measurement(perm_be_t2020, permissive_license_text__corporate_moat_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(perm_be_t2024, permissive_license_text__corporate_moat_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(perm_su_t1990, permissive_license_text__corporate_moat_reading, suppression_requirement, 1990, 0.2).
narrative_ontology:measurement(perm_su_t1998, permissive_license_text__corporate_moat_reading, suppression_requirement, 1998, 0.35).
narrative_ontology:measurement(perm_su_t2006, permissive_license_text__corporate_moat_reading, suppression_requirement, 2006, 0.5).
narrative_ontology:measurement(perm_su_t2014, permissive_license_text__corporate_moat_reading, suppression_requirement, 2014, 0.65).
narrative_ontology:measurement(perm_su_t2020, permissive_license_text__corporate_moat_reading, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement(perm_su_t2024, permissive_license_text__corporate_moat_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__corporate_moat_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'permissive_license_text' kernel, focusing on its extractive aspects for corporate beneficiaries. It is structurally distinct from the 'commons_coordination_reading' (which emphasizes friction reduction) and the 'copyleft_counterfactual_reading' (which highlights the need for reciprocity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
