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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public Domain Scaffold (Public-Good Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the US Constitutional
 *   Copyright Clause (Article I, Section 8, Clause 8), which grants Congress
 *   the power 'To promote the Progress of Science and useful Arts, by
 *   securing for limited Times to Authors and Inventors the exclusive Right
 *   to their respective Writings and Discoveries.' This 'public scaffold'
 *   reading emphasizes that the temporary monopoly granted to creators is a
 *   means to the end of enriching the public domain and promoting societal
 *   progress, not an end in itself. It views copyright as a transitional
 *   support mechanism (scaffold) for public benefit, with a clear sunset
 *   (limited times).
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
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public Domain Scaffold (Public-Good Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '57396ab2-5542-498c-a6df-401cdc0cab6c').
narrative_ontology:cs_kernel_codification('57396ab2-5542-498c-a6df-401cdc0cab6c', fixed_text).
narrative_ontology:cs_authority_grounding('57396ab2-5542-498c-a6df-401cdc0cab6c', lineage).
narrative_ontology:cs_interpretation_layer_present('57396ab2-5542-498c-a6df-401cdc0cab6c').
narrative_ontology:cs_reading_relation('57396ab2-5542-498c-a6df-401cdc0cab6c', copyright_constitutional_mandate__corporate_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('57396ab2-5542-498c-a6df-401cdc0cab6c', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('57396ab2-5542-498c-a6df-401cdc0cab6c', foundational, monopoly_is_means_to_public_end).
narrative_ontology:cs_axiom_status(monopoly_is_means_to_public_end, holdable).
narrative_ontology:cs_axiom_grounding('57396ab2-5542-498c-a6df-401cdc0cab6c', monopoly_is_means_to_public_end, deontological).
narrative_ontology:cs_axiom('57396ab2-5542-498c-a6df-401cdc0cab6c', foundational, public_domain_is_ultimate_beneficiary).
narrative_ontology:cs_axiom_status(public_domain_is_ultimate_beneficiary, holdable).
narrative_ontology:cs_axiom_grounding('57396ab2-5542-498c-a6df-401cdc0cab6c', public_domain_is_ultimate_beneficiary, deontological).
narrative_ontology:cs_reference_frame('57396ab2-5542-498c-a6df-401cdc0cab6c', original_constitutional_intent).
narrative_ontology:cs_drift_state('57396ab2-5542-498c-a6df-401cdc0cab6c', contemporary_copyright_extensions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('57396ab2-5542-498c-a6df-401cdc0cab6c', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, creators_and_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of copyright, receiving works into the common pool after the limited monopoly expires. This reading prioritizes its expansion and accessibility.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Receive a temporary, limited monopoly as an incentive to create, with the understanding that their works will eventually enrich the public domain. They benefit from the initial protection but are expected to contribute to the common good.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, creators_and_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Hold the temporary monopoly granted by copyright. Under this reading, they are expected to respect the 'limited times' and 'public good' aspects, bearing the 'cost' of eventual public domain entry and fair use limitations. Their power is constrained by the constitutional mandate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, copyright_holders, payer,
    powerful, biographical, constrained, global).

% Responsible for enacting copyright laws that adhere to the constitutional mandate of promoting progress and enriching the public domain. This reading holds them accountable for setting 'limited times' that are genuinely temporary and for balancing creator incentives with public access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets copyright law and the constitutional mandate. This reading expects the judiciary to actively enforce the 'limited times' and 'public good' principles, potentially striking down overly long terms or expanding fair use doctrines.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocate for maximal copyright protection and term extensions. Under this reading, their influence would be actively resisted or excluded from shaping policy, as their goals conflict with the public domain mandate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_lobbyists, excluded,
    organized, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the incentive for creation (temporary monopoly) with the long-term public good (enrichment of the public domain), ensuring a continuous flow of new works into the common intellectual heritage.
% TRANSFER_FUNCTION: Transfers a temporary, limited monopoly right from the public to creators, in exchange for the eventual transfer of the created work into the public domain.
% ABSENT_VOICES: Those who advocate for perpetual copyright or maximal enclosure of intellectual property are structurally excluded from the core premise of this reading; they would argue against any limitations on copyright duration or scope.
% DISAPPEARANCE_RATIONALE: If this constitutional mandate vanished, the balance between private incentive and public good would collapse. Copyright terms would likely extend indefinitely, fair use would diminish, and the public domain would stagnate, fundamentally altering the landscape of intellectual property and innovation.
% FOUNDING_PROBLEM: To incentivize the creation of new works for the benefit of society, while ensuring that these works eventually become part of the common intellectual heritage, preventing perpetual private control over public knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, public interest groups, and open-source advocates consistently corroborate that the founding problem of balancing private incentive with public access remains live, and that the 'limited times' clause is crucial to its resolution. This corroboration comes from outside the direct beneficiaries of copyright monopolies.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because this reading inherently limits the scope and duration of monopoly, prioritizing public access. Suppression is also low (0.15) as it actively encourages fair use and public domain entry, rather than suppressing alternatives. Theater ratio is low (0.1) because the stated purpose (public progress) is genuinely pursued, though often contested by other readings. The 'has_sunset_clause: true' reflects the 'limited Times' constitutional language, which is central to this reading's interpretation of copyright as a scaffold.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of copyright holders, this reading might feel extractive due to its emphasis on limitations and public domain entry. However, from the perspective of the public domain and future innovators, it is a beneficial coordination mechanism. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'public domain' (represented as a non-agent beneficiary) and 'creators and innovators' are the primary beneficiaries, as the system is designed to serve their interests. 'Copyright holders' are framed as payers, bearing the 'cost' of the temporary nature of their monopoly and the eventual entry of their works into the public domain. The 'legislature' and 'judiciary' are agenda-setters, tasked with upholding this balance. 'Corporate lobbyists' are excluded, as their agenda for maximal enclosure directly conflicts with this reading's core tenets.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_interpretation,
    'What constitutes ''limited Times'' in the constitutional mandate? Is it a fixed term, or a term subject to legislative extension?',
    'A Supreme Court ruling explicitly defining the outer bounds of ''limited Times'' or a constitutional amendment clarifying the intent.',
    'If ''limited Times'' is interpreted as a genuinely short, non-extendable term, this reading''s scaffold nature is strengthened. If it allows for indefinite extensions, the scaffold function is undermined, pushing the constraint towards a ''snare'' or ''tangled_rope'' from the public''s perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_times_interpretation, conceptual, 'Ambiguity in the ''limited Times'' clause and its impact on copyright duration.').

omega_variable(
    public_good_vs_private_profit,
    'How is ''Progress of Science and useful Arts'' measured? Is it primarily through the volume of new creations, or through the accessibility and diffusion of knowledge?',
    'Empirical studies correlating copyright term lengths and scope with innovation rates and public access metrics, or a legislative declaration of specific public-good metrics.',
    'If progress is measured by diffusion, this reading is reinforced. If it''s measured solely by creation volume (regardless of access), the ''corporate_enclosure_reading'' gains ground, potentially increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_good_vs_private_profit, empirical, 'The definition of ''progress'' and its impact on copyright policy.').

omega_variable(
    fair_use_scope,
    'What is the appropriate scope of ''fair use'' and other public exceptions to copyright? Should it be broadly interpreted to maximize public access, or narrowly to protect creator rights?',
    'Judicial precedents expanding or contracting fair use, or legislative action codifying specific public-interest exceptions.',
    'A broad interpretation of fair use strengthens this reading''s public-good orientation, reducing effective extractiveness. A narrow interpretation would shift the balance towards private enclosure, increasing extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_scope, preference, 'The balance between fair use and exclusive rights.').


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
narrative_ontology:measurement(copy_tr_t2000, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t1787, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1787, 0.1).
narrative_ontology:measurement(copy_be_t1850, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(copy_be_t1900, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(copy_be_t1950, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement(copy_be_t2000, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2000, 0.24).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1787, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1787, 0.05).
narrative_ontology:measurement(copy_su_t1850, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1850, 0.08).
narrative_ontology:measurement(copy_su_t1900, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(copy_su_t1950, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1950, 0.12).
narrative_ontology:measurement(copy_su_t2000, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2000, 0.14).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, intellectual_property_enforcement_regime).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'copyright_constitutional_mandate' kernel. This 'public scaffold' reading emphasizes the public domain as the ultimate beneficiary, contrasting with the 'corporate enclosure' reading's focus on maximal property rights and the 'judicial ambiguity' reading's deference to legislative discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
