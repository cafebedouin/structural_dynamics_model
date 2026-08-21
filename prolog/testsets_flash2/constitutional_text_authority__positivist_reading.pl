% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Constitutional Text Authority (Positivist Reading)
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint represents the positivist reading of constitutional
 *   authority, where validity stems from formal enactment and institutional
 *   sources, strictly separating law from morality. It is one reading of the
 *   broader 'constitutional_text_authority' kernel, which also includes
 *   originalist and living constitutionalist interpretations. This reading
 *   emphasizes procedural fidelity and textualism, converging with
 *   originalism on the importance of text but diverging on any appeal to
 *   natural law or founding intent beyond the text itself.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.15).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.25).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Constitutional Text Authority (Positivist Reading)").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'bfca9cb3-777b-4316-aef2-04937340b837').
narrative_ontology:cs_kernel_codification('bfca9cb3-777b-4316-aef2-04937340b837', fixed_text).
narrative_ontology:cs_authority_grounding('bfca9cb3-777b-4316-aef2-04937340b837', lineage).
narrative_ontology:cs_interpretation_layer_present('bfca9cb3-777b-4316-aef2-04937340b837').
narrative_ontology:cs_reading_relation('bfca9cb3-777b-4316-aef2-04937340b837', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('bfca9cb3-777b-4316-aef2-04937340b837', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('bfca9cb3-777b-4316-aef2-04937340b837', foundational, law_morality_distinction).
narrative_ontology:cs_axiom_status(law_morality_distinction, holdable).
narrative_ontology:cs_axiom_grounding('bfca9cb3-777b-4316-aef2-04937340b837', law_morality_distinction, deontological).
narrative_ontology:cs_axiom('bfca9cb3-777b-4316-aef2-04937340b837', foundational, formal_enactment_validity).
narrative_ontology:cs_axiom_status(formal_enactment_validity, holdable).
narrative_ontology:cs_axiom_grounding('bfca9cb3-777b-4316-aef2-04937340b837', formal_enactment_validity, conventional).
narrative_ontology:cs_reference_frame('bfca9cb3-777b-4316-aef2-04937340b837', legal_positivism_framework).
narrative_ontology:cs_drift_state('bfca9cb3-777b-4316-aef2-04937340b837', contemporary_legal_discourse, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('bfca9cb3-777b-4316-aef2-04937340b837', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legal_profession).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, judicial_system).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, citizens).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, legislature).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, rule_of_law_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__positivist_reading, separation_of_powers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the clarity and predictability of law derived from formal sources, allowing for consistent legal education, practice, and adjudication. Requires adherence to established procedures.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legal_profession, beneficiary,
    organized, generational, constrained, national).

% Applies and interprets the Constitution based on its formal enactment and institutional sources, maintaining a clear distinction between legal validity and moral desirability. Its authority is grounded in this procedural fidelity.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, judicial_system, agenda_setter,
    institutional, generational, constrained, national).

% Must adhere to the formally enacted constitutional text and procedures for creating new laws or amending the Constitution. Cannot appeal to moral arguments alone to justify legislation that conflicts with established legal text.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Benefit from the stability and predictability of a legal system where constitutional validity is clear and not subject to shifting moral interpretations. Their rights and duties are defined by formally enacted law.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).

% Seek to infuse constitutional interpretation with contemporary moral values, but their arguments are deemed irrelevant to legal validity under this reading. They must pursue constitutional change through formal amendment procedures rather than judicial reinterpretation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, moral_advocates, excluded,
    powerless, generational, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal and political action by grounding constitutional validity in ascertainable formal procedures and institutional sources, rather than subjective moral judgments.
% TRANSFER_FUNCTION: Transfers interpretive authority from moral philosophy or evolving societal values to formal legal texts and established institutional processes, from moral advocates to the legal profession and judiciary.
% ABSENT_VOICES: Moral advocates and those who believe constitutional meaning should evolve with societal values are excluded from the primary discourse on legal validity, forced to operate outside the formal interpretive framework.
% DISAPPEARANCE_RATIONALE: If this positivist reading vanished, constitutional interpretation would immediately become more fluid, potentially leading to greater judicial activism based on moral reasoning, and a significant shift in the perceived legitimacy of legal outcomes. The legal system's stability would be profoundly altered.
% FOUNDING_PROBLEM: To establish a clear, objective basis for constitutional validity that is independent of individual moral beliefs, ensuring legal certainty and limiting judicial discretion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and practitioners within the positivist tradition attest to the ongoing need for a clear law/morality distinction to maintain the integrity and predictability of the legal system. Critics from other interpretive schools acknowledge the historical problem but dispute the positivist solution's contemporary relevance.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low because this reading primarily coordinates legal interpretation, providing clarity rather than extracting rents. Suppression is moderate, as it actively suppresses moral arguments from influencing legal validity, requiring adherence to formal procedures. Theater ratio is low, as the legal system genuinely operates on these principles. Accessibility collapse is high because once the positivist framework is adopted, alternative (e.g., moral) routes to constitutional validity are largely foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   While the positivist reading aims for objectivity, its application can still be contested. For instance, 'moral advocates' perceive the constraint as highly suppressive of their legitimate concerns, whereas the 'judicial system' views it as a necessary guardrail for legal integrity. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The legal profession and judicial system are beneficiaries, gaining clarity and authority from this reading. The legislature is a payer, constrained by formal procedures. Citizens are beneficiaries of legal predictability. Moral advocates are excluded, as their arguments are deemed outside the scope of legal validity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    law_morality_distinction_clarity,
    'How clear and consistently applicable is the distinction between law and morality in complex constitutional cases?',
    'Analysis of judicial opinions in hard cases: if judges consistently avoid moral reasoning or explicitly separate it from legal validity, the distinction is robust. If moral arguments implicitly or explicitly influence legal outcomes, the distinction is blurred.',
    'If the distinction is consistently blurred, the positivist reading''s claim to objective validity is weakened, potentially shifting its classification towards a more interpretive or even extractive type, as it would be seen as masking moral choices under legal formalism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(law_morality_distinction_clarity, empirical, 'Ambiguity in the practical application of the law/morality distinction.').

omega_variable(
    textualism_vs_positivism_convergence,
    'To what extent is strict textualism, as practiced by some originalists, functionally indistinguishable from this positivist reading?',
    'Comparative analysis of judicial outcomes and reasoning from self-identified textualist originalists and positivists in cases where moral content is at issue. If their reasoning and conclusions align on the irrelevance of moral content to legal validity, the convergence is strong.',
    'If the convergence is strong, the ''originalist_reading'' (at least its textualist wing) and this ''positivist_reading'' might be considered structurally very similar, potentially leading to a re-evaluation of their distinctness within the kernel. If they diverge significantly on the role of ''founding intent'' beyond the text, their distinctness is maintained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textualism_vs_positivism_convergence, conceptual, 'Overlap between strict textualist originalism and legal positivism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cons_tr_t10, constitutional_text_authority__positivist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(cons_tr_t20, constitutional_text_authority__positivist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__positivist_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(cons_tr_t50, constitutional_text_authority__positivist_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cons_be_t10, constitutional_text_authority__positivist_reading, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(cons_be_t20, constitutional_text_authority__positivist_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__positivist_reading, base_extractiveness, 30, 0.14).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cons_be_t50, constitutional_text_authority__positivist_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cons_su_t10, constitutional_text_authority__positivist_reading, suppression_requirement, 10, 0.22).
narrative_ontology:measurement(cons_su_t20, constitutional_text_authority__positivist_reading, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__positivist_reading, suppression_requirement, 30, 0.24).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.25).
narrative_ontology:measurement(cons_su_t50, constitutional_text_authority__positivist_reading, suppression_requirement, 50, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_text_authority__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text_authority' kernel, each representing a distinct interpretive approach to constitutional validity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
