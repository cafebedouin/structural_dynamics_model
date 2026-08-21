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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint story instantiates the 'public scaffold' reading of the
 *   U.S. Constitutional Copyright Clause (Article I, Section 8, Clause 8).
 *   Under this reading, copyright is understood as a temporary, instrumental
 *   grant of monopoly power, whose sole legitimate purpose is to incentivize
 *   the creation of works that ultimately enrich the public domain. The
 *   'limited times' provision is central, ensuring that the public eventually
 *   gains full access to and use of creative works, fostering further
 *   innovation and cultural development. This reading emphasizes the public
 *   good as the primary end, with private monopoly as a carefully
 *   circumscribed means.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__public_scaffold_reading, 0.2).
domain_priors:suppression_score(copyright_constitutional_mandate__public_scaffold_reading, 0.15).
domain_priors:theater_ratio(copyright_constitutional_mandate__public_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__public_scaffold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__public_scaffold_reading, scaffold).
narrative_ontology:human_readable(copyright_constitutional_mandate__public_scaffold_reading, "Copyright as Public Domain Scaffold (Public-Good Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__public_scaffold_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:has_sunset_clause(copyright_constitutional_mandate__public_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, 'f735b05e-7416-47ab-9167-2cd9cb98c382').
narrative_ontology:cs_kernel_codification('f735b05e-7416-47ab-9167-2cd9cb98c382', fixed_text).
narrative_ontology:cs_authority_grounding('f735b05e-7416-47ab-9167-2cd9cb98c382', lineage).
narrative_ontology:cs_interpretation_layer_present('f735b05e-7416-47ab-9167-2cd9cb98c382').
narrative_ontology:cs_reading_relation('f735b05e-7416-47ab-9167-2cd9cb98c382', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('f735b05e-7416-47ab-9167-2cd9cb98c382', copyright_constitutional_mandate__judicial_ambiguity_reading, coexists_with).
narrative_ontology:cs_axiom('f735b05e-7416-47ab-9167-2cd9cb98c382', foundational, copyright_serves_public_good).
narrative_ontology:cs_axiom_status(copyright_serves_public_good, holdable).
narrative_ontology:cs_axiom_grounding('f735b05e-7416-47ab-9167-2cd9cb98c382', copyright_serves_public_good, deontological).
narrative_ontology:cs_axiom('f735b05e-7416-47ab-9167-2cd9cb98c382', foundational, monopoly_is_temporary_incentive).
narrative_ontology:cs_axiom_status(monopoly_is_temporary_incentive, holdable).
narrative_ontology:cs_axiom_grounding('f735b05e-7416-47ab-9167-2cd9cb98c382', monopoly_is_temporary_incentive, instrumental).
narrative_ontology:cs_reference_frame('f735b05e-7416-47ab-9167-2cd9cb98c382', framers_original_intent).
narrative_ontology:cs_drift_state('f735b05e-7416-47ab-9167-2cd9cb98c382', contemporary_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f735b05e-7416-47ab-9167-2cd9cb98c382', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, users_and_consumers).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_good_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, limited_monopoly_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of copyright, receiving works after their temporary monopoly expires. Its 'interests' are represented by public interest advocates and constitutional scholars.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, the_public_domain, beneficiary,
    powerless, civilizational, analytical, universal).

% Receive temporary exclusive rights as an incentive to create and disseminate new works. They benefit from the limited monopoly, allowing them to profit from their creations before they enter the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, authors_and_creators, beneficiary,
    moderate, biographical, mobile, global).

% Administer and benefit from the temporary monopolies granted by copyright. Under this reading, their role is to facilitate the dissemination of works and manage the transition to the public domain, not to maximize enclosure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, publishers_and_distributors, agenda_setter,
    organized, generational, arbitrage, global).

% Benefit from access to a rich public domain and new creative works. They pay for copyrighted works during their monopoly period, but anticipate future free access and derivative use.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, users_and_consumers, beneficiary,
    moderate, biographical, constrained, global).

% Actively interpret and advocate for copyright law in line with its constitutional mandate to promote the progress of science and useful arts for the public good, emphasizing the temporary nature of monopoly and the importance of the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, constitutional_scholars_public_interest_advocates, observer,
    organized, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__public_scaffold_reading, constitutional_scholars_public_interest_advocates, agenda_setter).

% Responsible for enacting copyright statutes. Under this reading, the legislature is expected to set terms and conditions that balance creator incentives with the ultimate goal of public domain enrichment, adhering strictly to the 'limited times' clause.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, legislature, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To incentivize the creation and dissemination of new intellectual works by granting authors and inventors temporary exclusive rights, ensuring these works eventually enter the public domain for broader societal benefit and future innovation.
% TRANSFER_FUNCTION: Transfers temporary exclusive rights (a limited monopoly) from the public (who would otherwise have immediate free access) to creators, in exchange for the future enrichment of the public domain and the 'progress of science and useful arts'.
% ABSENT_VOICES: Future generations, who are the ultimate beneficiaries of a robust public domain, cannot directly advocate for shorter terms or broader fair use provisions. Their interests are represented by public interest groups and constitutional scholars.
% DISAPPEARANCE_RATIONALE: If copyright vanished overnight, the economic incentive for many forms of creative work would diminish, potentially leading to less professional creation. However, all existing works would immediately enter the public domain, leading to a massive rearrangement of cultural access and derivative work creation, with immediate benefits for remix culture and innovation.
% FOUNDING_PROBLEM: How to incentivize authors and inventors to create and disseminate new works for the public good, without granting perpetual monopolies that stifle future innovation and access, as seen with historical royal patents and monopolies.
% FOUNDING_PROBLEM_CORROBORATION: The writings of the U.S. Constitutional framers (e.g., Madison, Jefferson), early copyright statutes, and ongoing public policy debates and academic scholarship (e.g., Lawrence Lessig, James Boyle) consistently corroborate this original intent, distinct from industry lobbying efforts.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(copyright_constitutional_mandate__public_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__public_scaffold_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is low (0.20) because, under this reading, the temporary monopoly is seen as a necessary, minimal cost for a greater public benefit, not as a mechanism for rent extraction. Suppression is low (0.15) as the constraint's enforcement is primarily to secure the temporary incentive, not to prevent public access or alternative creative paths. The theater ratio is low (0.10) because the public-good function is genuinely central and actively pursued. Accessibility collapse is moderate (0.30) because while immediate access is restricted, the eventual entry into the public domain ensures alternatives are not permanently foreclosed. Resistance is low (0.10) because this reading aligns with the constitutional purpose and public interest.
 *
 * PERSPECTIVAL GAP:
 *   This 'public scaffold' reading stands in stark contrast to the 'corporate enclosure' reading, which views copyright primarily as a property right to be maximally extended for private gain. It also provides a specific interpretive lens for the 'judicial ambiguity' reading, arguing that legislative discretion must always be exercised within the bounds of the public-good mandate.
 *
 * DIRECTIONALITY LOGIC:
 *   The public domain and creators are the primary beneficiaries, as the system is designed to serve their interests (future access for the public, incentive for creators). Publishers and distributors act as agenda-setters, facilitating the system's operation. Users and consumers are also beneficiaries, gaining access to new works and eventually the public domain. There are no 'victims' in this reading, as the temporary monopoly is considered a fair exchange for the public good.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently guards against mandatrophy by emphasizing the 'limited times' clause and the ultimate goal of public domain enrichment. The constraint's mandate is explicitly tied to its temporary, instrumental function. Any attempt to extend terms indefinitely or to prioritize private gain over public access would be seen as a direct violation of its founding purpose, preventing the constraint from becoming a 'piton' or 'snare' by design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_interpretation,
    'What constitutes ''limited times'' in the context of copyright, and how should this be determined to best serve the public good?',
    'Empirical studies on optimal incentive duration, historical analysis of copyright terms, and legislative action informed by public interest rather than industry lobbying.',
    'A shorter, fixed term would reinforce the ''scaffold'' nature of copyright, ensuring quicker public domain entry. Longer terms, especially those extended retrospectively, would shift the constraint towards extraction and enclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_times_interpretation, conceptual, 'Ambiguity in the constitutional phrase ''limited times''.').

omega_variable(
    scope_of_progress_of_science_and_useful_arts,
    'What types of works and incentives truly promote ''the progress of science and useful arts'' for the public, and which primarily serve private interests without commensurate public benefit?',
    'Independent economic and cultural impact assessments of various copyrightable works and incentive structures, free from industry influence.',
    'A narrow interpretation would focus copyright on genuinely innovative and publicly beneficial works, reducing extractiveness. A broad interpretation risks extending monopoly to works with minimal public benefit, increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_progress_of_science_and_useful_arts, empirical, 'Defining the scope of works intended for public benefit.').

omega_variable(
    fair_use_doctrine_effectiveness,
    'Is the fair use doctrine, as currently interpreted and applied, effectively balancing creator rights with public access and transformative use, or is it being eroded by maximalist enforcement?',
    'Analysis of fair use litigation outcomes, legislative efforts to clarify or expand fair use, and the actual practice of creators in remixing and building upon existing works.',
    'If fair use is robust, it acts as a safety valve, reducing effective extraction and promoting public domain values. If it is weak or uncertain, it increases effective extraction and stifles creativity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fair_use_doctrine_effectiveness, empirical, 'Effectiveness of fair use in balancing rights and access.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(copy_tr_t6, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement(copy_tr_t12, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(copy_tr_t18, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(copy_tr_t24, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(copy_tr_t30, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(copy_be_t6, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 6, 0.19).
narrative_ontology:measurement(copy_be_t12, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 12, 0.2).
narrative_ontology:measurement(copy_be_t18, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 18, 0.2).
narrative_ontology:measurement(copy_be_t24, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(copy_be_t30, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 30, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t0, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(copy_su_t6, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 6, 0.13).
narrative_ontology:measurement(copy_su_t12, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 12, 0.14).
narrative_ontology:measurement(copy_su_t18, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 18, 0.15).
narrative_ontology:measurement(copy_su_t24, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 24, 0.15).
narrative_ontology:measurement(copy_su_t30, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 30, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel, emphasizing the public good and temporary nature of monopoly. It is linked to sibling readings that offer alternative interpretations of the same constitutional clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
