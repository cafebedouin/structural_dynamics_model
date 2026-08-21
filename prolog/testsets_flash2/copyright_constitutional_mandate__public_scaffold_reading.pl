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
 *   constraint_id: copyright_constitutional_mandate__public_scaffold_reading
 *   human_readable: Copyright as Public Domain Scaffold (Public-Good Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public scaffold' reading of the
 *   copyright constitutional mandate, where copyright exists primarily to
 *   enrich the public domain, and temporary monopoly is a means to that
 *   public-good end. This reading emphasizes shorter terms, robust fair use,
 *   and anti-enclosure norms. It is a scaffold because the temporary monopoly
 *   is a transitional support for public benefit, with a clear sunset (entry
 *   into the public domain). The metrics reflect a relatively low
 *   extractiveness and suppression, consistent with a system designed for
 *   coordination and public benefit, though historical drift has sometimes
 *   pushed these values higher.
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
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__public_scaffold_reading, '7f50d1f1-8037-4006-95d1-4b2afa9cad6b').
narrative_ontology:cs_kernel_codification('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', fixed_text).
narrative_ontology:cs_authority_grounding('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', lineage).
narrative_ontology:cs_interpretation_layer_present('7f50d1f1-8037-4006-95d1-4b2afa9cad6b').
narrative_ontology:cs_reading_relation('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', copyright_constitutional_mandate__corporate_enclosure_reading, forecloses).
narrative_ontology:cs_reading_relation('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', foundational, copyright_serves_public_domain).
narrative_ontology:cs_axiom_status(copyright_serves_public_domain, holdable).
narrative_ontology:cs_axiom_grounding('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', copyright_serves_public_domain, deontological).
narrative_ontology:cs_axiom('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', foundational, monopoly_is_temporary_means).
narrative_ontology:cs_axiom_status(monopoly_is_temporary_means, holdable).
narrative_ontology:cs_axiom_grounding('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', monopoly_is_temporary_means, instrumental).
narrative_ontology:cs_reference_frame('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', constitutional_utilitarian_balance).
narrative_ontology:cs_drift_state('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', contemporary_legislative_extensions, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7f50d1f1-8037-4006-95d1-4b2afa9cad6b', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__public_scaffold_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__public_scaffold_reading, creators_and_innovators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__public_scaffold_reading, corporate_rights_holders).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, utilitarian_theory_of_copyright).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__public_scaffold_reading, public_good_provision).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate beneficiary of copyright, receiving works into its commons after a limited period of exclusive rights, fostering future creativity and knowledge.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_domain, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__public_scaffold_reading, public_domain).

% Receive temporary exclusive rights as an incentive to create, with the understanding that their works will eventually enrich the public domain. They benefit from the initial market exclusivity.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, creators_and_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Actively champion shorter copyright terms, broader fair use, and anti-enclosure norms, seeking to align copyright law with its constitutional mandate to promote the progress of science and useful arts for the public good. They influence legislative and judicial interpretation.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, public_interest_advocates, agenda_setter,
    organized, generational, constrained, national).

% Has the constitutional power to set copyright terms and scope. Under this reading, its role is to balance creator incentives with public access, ensuring the 'limited times' provision is genuinely limited and serves the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, legislature, agenda_setter,
    institutional, generational, constrained, national).

% Interprets copyright statutes and the constitutional clause, with this reading emphasizing a strict construction of 'limited times' and a robust defense of fair use to prevent perpetual monopolies and promote public access.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Under this reading, they are constrained by the temporary nature of copyright and the robust fair use doctrine, preventing them from indefinitely enclosing works that should enter the public domain. They bear the 'cost' of eventual loss of exclusive rights.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__public_scaffold_reading, corporate_rights_holders, payer,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__public_scaffold_reading, public_domain).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__public_scaffold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the incentive for creation (temporary monopoly) with the ultimate goal of public access and future innovation (public domain enrichment), ensuring a continuous flow of knowledge and culture into the commons.
% TRANSFER_FUNCTION: Transfers temporary exclusive rights from the public to creators, in exchange for the eventual transfer of works into the public domain. It also transfers the cost of limited access during the monopoly period to the public.
% ABSENT_VOICES: Future generations, who are the ultimate beneficiaries of a rich public domain, are structurally absent from current legislative and judicial debates, making public interest advocates their proxy.
% DISAPPEARANCE_RATIONALE: If this reading of copyright vanished, the balance between private incentive and public good would collapse. Copyright terms would likely extend indefinitely, fair use would diminish, and the public domain would shrink, fundamentally altering the landscape of cultural and scientific progress.
% FOUNDING_PROBLEM: To incentivize the creation and dissemination of new works for the benefit of society, while preventing perpetual monopolies that would stifle future creativity and access to knowledge.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historians, and public interest organizations consistently corroborate this founding problem, citing the constitutional text ('To promote the Progress of Science and useful Arts, by securing for limited Times to Authors and Inventors the exclusive Right to their respective Writings and Discoveries') and early legislative history. This corroboration comes from outside the direct beneficiaries of extended copyright terms.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__public_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__public_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__public_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is low (0.25) because the primary goal is public benefit, not private rent-seeking. Suppression is low (0.15) as the system is meant to coordinate, not coerce, and alternatives (e.g., public domain works, fair use) are robust. Theater ratio is low (0.1) because the stated public-good function is genuinely pursued, though some performative elements exist around enforcement. The 'has_sunset_clause: true' reflects the 'limited Times' constitutional provision, which is central to this reading's scaffold nature. The slight increase in extractiveness and suppression around 2000 reflects historical legislative extensions of copyright terms, which this reading would view as a deviation from the core mandate, before a slight correction/stabilization.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of corporate rights holders (as in the 'corporate enclosure' reading), this constraint would appear highly extractive, as it limits their ability to maintain perpetual control over works. However, from the public-good perspective instantiated here, the limitation is a necessary coordination function, not extraction. The engine's per-seat classification would capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'public_domain' and 'creators_and_innovators' are the primary beneficiaries, as the system is designed to serve them. 'Public_interest_advocates', 'legislature', and 'judiciary' act as agenda-setters, shaping the constraint's application. 'Corporate_rights_holders' are framed as payers, bearing the cost of the temporary nature of their exclusive rights, which is a 'cost' from their perspective but a benefit from the public-good perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading inherently guards against mandatrophy by emphasizing the 'limited Times' and public domain enrichment. If the constraint were to drift towards perpetual monopoly, this reading would flag it as a failure of its core mandate, preventing mislabeling as mere coordination. The scaffold nature means its mandate is inherently temporary and transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_interpretation_ambiguity,
    'Is the ''limited Times'' clause a strict temporal limit or merely a prohibition on explicit perpetuity?',
    'A Supreme Court ruling explicitly defining the maximum permissible duration of copyright terms based on the original intent and public-good purpose, rather than deferring to legislative extensions.',
    'If interpreted as a strict limit, it would reinforce the scaffold nature, leading to shorter terms and lower extractiveness. If interpreted as a weak prohibition, it would allow for de facto perpetual monopolies, shifting the constraint towards a snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_interpretation_ambiguity, conceptual, 'Ambiguity in the constitutional interpretation of copyright duration.').

omega_variable(
    fair_use_scope_ambiguity,
    'What is the optimal scope of fair use to balance creator incentives and public access, and how does it align with the public-good mandate?',
    'Empirical studies on the impact of fair use expansion/contraction on creator output and public access, informing legislative adjustments and judicial precedent.',
    'A broader fair use doctrine would reduce extractiveness and suppression, reinforcing the public scaffold. A narrower doctrine would increase extractiveness, favoring rights holders over public access.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_scope_ambiguity, empirical, 'Uncertainty regarding the appropriate breadth of fair use doctrine.').

omega_variable(
    reading_natural_law_vs_construct,
    'Is this ''public scaffold'' reading of copyright a genuine reflection of natural law principles regarding intellectual commons, or a constructed legal framework that merely aligns with certain policy preferences?',
    'Philosophical and legal consensus on the inherent ''naturalness'' of an intellectual commons, or a clear demonstration that the framework''s benefits are entirely contingent on its specific legal construction.',
    'If a natural law, its legitimacy is inherent and its persistence more robust. If a construct, its persistence depends entirely on active enforcement and political will, making it more vulnerable to capture.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_natural_law_vs_construct, conceptual, 'Whether the public-good orientation of copyright is a natural or constructed feature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__public_scaffold_reading, 1790, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1790, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1790, 0.05).
narrative_ontology:measurement(copy_tr_t1850, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(copy_tr_t1900, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(copy_tr_t1950, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 1950, 0.09).
narrative_ontology:measurement(copy_tr_t2000, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(copy_tr_t2024, copyright_constitutional_mandate__public_scaffold_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(copy_be_t1790, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1790, 0.1).
narrative_ontology:measurement(copy_be_t1850, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1850, 0.15).
narrative_ontology:measurement(copy_be_t1900, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(copy_be_t1950, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(copy_be_t2000, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2000, 0.3).
narrative_ontology:measurement(copy_be_t2024, copyright_constitutional_mandate__public_scaffold_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1790, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1790, 0.1).
narrative_ontology:measurement(copy_su_t1850, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1850, 0.12).
narrative_ontology:measurement(copy_su_t1900, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1900, 0.14).
narrative_ontology:measurement(copy_su_t1950, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 1950, 0.16).
narrative_ontology:measurement(copy_su_t2000, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2000, 0.18).
narrative_ontology:measurement(copy_su_t2024, copyright_constitutional_mandate__public_scaffold_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__public_scaffold_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, corporate_enclosure_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__public_scaffold_reading, judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'copyright_constitutional_mandate' kernel. It emphasizes the public domain as the ultimate beneficiary, contrasting with readings that prioritize corporate property rights or legislative discretion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
