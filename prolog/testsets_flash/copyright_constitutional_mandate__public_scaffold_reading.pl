% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__public_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__public_scaffold_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public Domain Scaffold (Public Scaffold Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'public scaffold' reading of the US
 *   Constitutional Copyright Clause (Article I, Section 8, Clause 8), which
 *   holds that copyright's primary purpose is to 'promote the Progress of
 *   Science and useful Arts' by ensuring works eventually enter the public
 *   domain. The temporary monopoly granted to creators is a means to this
 *   public-good end, not an end in itself. This reading emphasizes shorter
 *   terms, robust fair use, and anti-enclosure norms. It is a scaffold
 *   because the temporary monopoly is meant to support creation and then
 *   recede, leaving the work in the public domain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.25).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.15).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public Domain Scaffold (Public Scaffold Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '2824fe25-ff2c-4811-a1a9-101a9184ec4f').
narrative_ontology:cs_kernel_codification('2824fe25-ff2c-4811-a1a9-101a9184ec4f', fixed_text).
narrative_ontology:cs_authority_grounding('2824fe25-ff2c-4811-a1a9-101a9184ec4f', lineage).
narrative_ontology:cs_interpretation_layer_present('2824fe25-ff2c-4811-a1a9-101a9184ec4f').
narrative_ontology:cs_reading_relation('2824fe25-ff2c-4811-a1a9-101a9184ec4f', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('2824fe25-ff2c-4811-a1a9-101a9184ec4f', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('2824fe25-ff2c-4811-a1a9-101a9184ec4f', foundational, copyright_serves_public_progress).
narrative_ontology:cs_axiom_status(copyright_serves_public_progress, holdable).
narrative_ontology:cs_axiom_grounding('2824fe25-ff2c-4811-a1a9-101a9184ec4f', copyright_serves_public_progress, deontological).
narrative_ontology:cs_axiom('2824fe25-ff2c-4811-a1a9-101a9184ec4f', foundational, monopoly_is_temporary_means).
narrative_ontology:cs_axiom_status(monopoly_is_temporary_means, holdable).
narrative_ontology:cs_axiom_grounding('2824fe25-ff2c-4811-a1a9-101a9184ec4f', monopoly_is_temporary_means, conventional).
narrative_ontology:cs_reference_frame('2824fe25-ff2c-4811-a1a9-101a9184ec4f', founding_era_public_purpose).
narrative_ontology:cs_drift_state('2824fe25-ff2c-4811-a1a9-101a9184ec4f', contemporary_corporate_lobbying_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2824fe25-ff2c-4811-a1a9-101a9184ec4f', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, future_creators).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_good_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_monopoly_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of copyright, receiving works into the public commons after a limited period, enabling new creation and cultural access. This reading prioritizes its expansion and accessibility.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    institutional, civilizational, analytical, universal).

% Benefit from a rich public domain as a source of inspiration and material for derivative works, without needing to license prior creations. This reading ensures a vibrant commons for their work.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, future_creators, beneficiary,
    moderate, generational, mobile, global).

% Benefits from broad access to cultural and informational works once they enter the public domain, fostering education, innovation, and cultural exchange. This reading supports their right to access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, general_public, beneficiary,
    organized, biographical, mobile, global).

% Granted a temporary monopoly to incentivize creation, but must accept that their works will eventually enter the public domain. This reading views their monopoly as a means, not an end, and limits its duration and scope.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Responsible for setting copyright terms and scope, guided by the constitutional mandate to promote progress. This reading interprets their role as balancing creator incentives with public benefit, favoring shorter terms and broader fair use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, legislature, agenda_setter,
    institutional, generational, analytical, national).

% Interprets copyright law and the constitutional clause, ensuring that legislative enactments serve the public good. This reading expects active judicial review to prevent excessive enclosure and uphold the public domain's role.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the temporary grant of exclusive rights to creators with the long-term goal of enriching the public domain, balancing incentives for creation with public access and future innovation.
% TRANSFER_FUNCTION: Temporarily transfers exclusive rights from the public to creators, in exchange for the eventual transfer of works into the public domain for all to use.
% ABSENT_VOICES: Lobbyists for maximal copyright extension and corporate interests seeking perpetual control over intellectual property are structurally opposed to this reading; they would argue for stronger, longer protections, but their arguments are subordinated to the public good in this framework.
% DISAPPEARANCE_RATIONALE: If this reading of copyright vanished, the balance would shift dramatically towards private enclosure, works would remain locked up longer, and the public domain would stagnate, fundamentally altering the landscape for future creation and public access.
% FOUNDING_PROBLEM: To incentivize the creation of new works for the benefit of society, while ensuring that these works eventually become part of a shared cultural and intellectual commons.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, public interest groups, and historians attest that the founding problem of balancing private incentive with public good remains live, and that this reading directly addresses it by prioritizing the public domain. This is corroborated by constitutional text and historical legislative debates.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).
:- end_tests(copyright_constitutional_mandate__public_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.25) because the temporary monopoly is seen as a necessary incentive, not a rent-seeking mechanism. Suppression is low (0.15) as this reading actively encourages access and reuse after the term. Theater ratio is low (0.1) because the stated public-good function is genuinely pursued. The slight increase in extractiveness and suppression over time (peaking around 2000) reflects historical legislative expansions of copyright terms, which this reading would view as deviations from its core principle, but the reading itself attempts to resist this drift.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the public domain and future creators, this reading is a pure rope or scaffold, facilitating progress. From the perspective of copyright holders seeking maximal protection, it is a constraint on their property rights, potentially appearing as a snare if their interests are prioritized over the public good. The engine's classification will reflect the structural data, not the claimed type, if the metrics indicate a different reality.
 *
 * DIRECTIONALITY LOGIC:
 *   The public domain, future creators, and the general public are the primary beneficiaries (d near 0.0), as this reading prioritizes their access and ability to build upon existing works. Copyright holders are payers (d near 1.0) in the sense that they must eventually relinquish their exclusive rights, which is the 'cost' of the temporary monopoly. The legislature and judiciary are agenda-setters, tasked with upholding this balance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_good_vs_private_property,
    'Is copyright primarily a mechanism to promote the public good through a rich public domain, or a fundamental private property right?',
    'Judicial rulings that explicitly prioritize the public domain over private enclosure, or legislative reforms that shorten terms and expand fair use based on public benefit analysis.',
    'If resolved towards public good, this reading''s classification as a scaffold would be strongly reinforced; if resolved towards private property, it would be reclassified as a tangled_rope or snare, reflecting greater extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(public_good_vs_private_property, conceptual, 'The fundamental conceptual framing of copyright''s purpose.').

omega_variable(
    term_length_efficacy,
    'What is the optimal copyright term length to incentivize creation without unduly delaying public domain entry, and does current law reflect this?',
    'Empirical economic studies correlating term length with creative output, and legislative action to adjust terms based on these findings.',
    'If current terms are found to be excessively long relative to incentive needs, the ''public scaffold'' reading would gain empirical support, potentially leading to a lower extractiveness score. If terms are found to be optimal, the current extractiveness would be justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(term_length_efficacy, empirical, 'Empirical basis for copyright term length.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 1787, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1787, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(copy_tr_t1850, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(copy_tr_t1900, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(copy_tr_t1950, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(copy_tr_t2000, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t1787, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(copy_be_t1850, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(copy_be_t1900, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(copy_be_t1950, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(copy_be_t2000, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1787, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1787, 0.05).
narrative_ontology:measurement(copy_su_t1850, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1850, 0.08).
narrative_ontology:measurement(copy_su_t1900, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(copy_su_t1950, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(copy_su_t2000, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, fair_use_doctrine).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, digital_rights_management_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'public scaffold' reading emphasizes the public domain as the ultimate beneficiary, contrasting with readings that prioritize private property or legislative discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
