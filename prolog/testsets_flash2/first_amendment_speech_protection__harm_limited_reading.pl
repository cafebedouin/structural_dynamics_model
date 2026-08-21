% ============================================================================
% CONSTRAINT STORY: first_amendment_speech_protection__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_amendment_speech_protection__harm_limited_reading, []).

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
 *   constraint_id: first_amendment_speech_protection__harm_limited_reading
 *   human_readable: First Amendment Speech Protection (Harm-Limited Reading)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   This constraint represents the 'harm-limited' reading of the First
 *   Amendment, where speech protection yields when it causes demonstrable,
 *   unconsented-to harm. This reading seeks to balance free expression with
 *   the protection of vulnerable individuals and groups from the direct
 *   negative impacts of speech. It is a contested interpretation within
 *   constitutional law and political philosophy, standing in contrast to
 *   absolutist and categorical balancing approaches. The constraint is
 *   claimed as a Tangled Rope because it genuinely coordinates the boundaries
 *   of speech while also extracting from speakers whose expression is deemed
 *   harmful.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, 0.6).
domain_priors:suppression_score(first_amendment_speech_protection__harm_limited_reading, 0.4).
domain_priors:theater_ratio(first_amendment_speech_protection__harm_limited_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(first_amendment_speech_protection__harm_limited_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_amendment_speech_protection__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(first_amendment_speech_protection__harm_limited_reading, "First Amendment Speech Protection (Harm-Limited Reading)").
narrative_ontology:topic_domain(first_amendment_speech_protection__harm_limited_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:requires_active_enforcement(first_amendment_speech_protection__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(first_amendment_speech_protection__harm_limited_reading, '915deec2-165f-41ca-b8fb-3e9e54548611').
narrative_ontology:cs_kernel_codification('915deec2-165f-41ca-b8fb-3e9e54548611', fixed_text).
narrative_ontology:cs_authority_grounding('915deec2-165f-41ca-b8fb-3e9e54548611', lineage).
narrative_ontology:cs_interpretation_layer_present('915deec2-165f-41ca-b8fb-3e9e54548611').
narrative_ontology:cs_reading_relation('915deec2-165f-41ca-b8fb-3e9e54548611', first_amendment_speech_protection__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('915deec2-165f-41ca-b8fb-3e9e54548611', first_amendment_speech_protection__categorical_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('915deec2-165f-41ca-b8fb-3e9e54548611', foundational, speech_causes_demonstrable_harm_is_not_protected).
narrative_ontology:cs_axiom_status(speech_causes_demonstrable_harm_is_not_protected, holdable).
narrative_ontology:cs_axiom_grounding('915deec2-165f-41ca-b8fb-3e9e54548611', speech_causes_demonstrable_harm_is_not_protected, empirically_contingent).
narrative_ontology:cs_axiom('915deec2-165f-41ca-b8fb-3e9e54548611', foundational, protection_of_vulnerable_groups_is_a_constitutional_value).
narrative_ontology:cs_axiom_status(protection_of_vulnerable_groups_is_a_constitutional_value, holdable).
narrative_ontology:cs_axiom_grounding('915deec2-165f-41ca-b8fb-3e9e54548611', protection_of_vulnerable_groups_is_a_constitutional_value, deontological).
narrative_ontology:cs_reference_frame('915deec2-165f-41ca-b8fb-3e9e54548611', post_brandenburg_incitement_test).
narrative_ontology:cs_drift_state('915deec2-165f-41ca-b8fb-3e9e54548611', contemporary_digital_speech_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('915deec2-165f-41ca-b8fb-3e9e54548611', '').
narrative_ontology:cs_kernel_id(first_amendment_speech_protection__harm_limited_reading, first_amendment_speech_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities).
narrative_ontology:constraint_beneficiary(first_amendment_speech_protection__harm_limited_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech).
narrative_ontology:constraint_victim(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ability to seek legal recourse or regulatory intervention against speech that causes demonstrable, unconsented-to harm, such as incitement to violence or severe harassment. Their ability to participate in public life is enhanced by this protection.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, vulnerable_minorities, beneficiary,
    powerless, generational, trapped, national).

% Receives protection from direct and severe harms caused by speech, allowing them to live and work without constant threat or degradation. This reading provides a mechanism for redress when speech crosses a harm threshold.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, targets_of_hate_speech, beneficiary,
    powerless, immediate, trapped, local).

% Faces legal or social consequences for speech deemed to cause demonstrable, unconsented-to harm. Their expressive freedom is curtailed at the point where it infringes on the safety or well-being of others. They bear the cost of enforcement actions.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, speakers_of_harmful_speech, payer,
    moderate, biographical, constrained, national).

% Advocates for a broader interpretation of the First Amendment, viewing any limitation based on harm as an unacceptable infringement on free expression. They bear the 'cost' of a narrower protected speech set and actively resist this reading through legal challenges and public discourse.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, free_speech_absolutists, payer,
    organized, generational, constrained, national).

% Are tasked with adjudicating specific instances of speech to determine if it crosses the harm threshold. They define and enforce the boundaries of protected speech, balancing expressive freedom against the prevention of harm. This involves complex legal and factual determinations.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, courts_and_regulators, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from a framework that seeks to prevent speech from causing direct and severe harm, theoretically fostering a more inclusive and less hostile environment for communication. This reading aims to improve the quality and safety of public deliberation.
narrative_ontology:constraint_stakeholder(first_amendment_speech_protection__harm_limited_reading, public_discourse, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(first_amendment_speech_protection__harm_limited_reading, public_discourse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the boundaries of free expression by establishing a clear, albeit complex, standard for when speech protection yields to the prevention of demonstrable, unconsented-to harm. This allows for a more orderly public discourse by setting limits on harmful expression.
% TRANSFER_FUNCTION: Transfers the burden of harm from vulnerable individuals and groups to speakers whose expression causes such harm, by permitting legal or regulatory intervention. It also transfers interpretive authority to courts and regulators to define and apply the harm standard.
% ABSENT_VOICES: Those who believe that any speech, regardless of its harmful impact, should be absolutely protected, are effectively marginalized in this framework. Their arguments for unfettered expression are not given equal weight when demonstrable harm is proven.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for speech would revert to a more absolutist or categorical balancing approach, potentially leaving vulnerable groups without recourse against harmful speech. Public discourse would likely become more hostile, and the role of courts in mediating speech disputes would fundamentally change.
% FOUNDING_PROBLEM: The problem of speech causing direct and severe harm to individuals and groups, particularly those historically marginalized, without adequate legal or social recourse.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, civil rights organizations, and victims' advocacy groups consistently attest to the ongoing problem of harmful speech and the necessity of this reading. Their corroboration comes from direct experience and analysis of societal impacts, outside the immediate beneficiaries of specific enforcement actions.
narrative_ontology:disappearance_verdict(first_amendment_speech_protection__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(first_amendment_speech_protection__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(first_amendment_speech_protection__harm_limited_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(first_amendment_speech_protection__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(first_amendment_speech_protection__harm_limited_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_amendment_speech_protection__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_amendment_speech_protection__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(first_amendment_speech_protection__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) reflects the cost borne by speakers whose speech is curtailed, and the resources expended in legal challenges and enforcement. Suppression (0.4) is moderate, as this reading requires active enforcement by courts and regulators to identify and penalize harmful speech, but it does not completely eliminate alternatives for expression. Theater ratio (0.2) is low, indicating that the primary function of preventing harm is genuinely pursued, though there can be performative aspects in high-profile cases. Resistance (0.7) is high due to ongoing challenges from free speech absolutists and those whose speech is restricted. Accessibility collapse (0.3) is moderate, as alternative forms of expression or legal challenges remain available, but the scope of protected speech is indeed narrowed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable minorities and targets of hate speech, this reading functions as a vital protection, enabling their participation in public life. From the perspective of speakers whose speech is curtailed, it is an extractive mechanism that limits their expressive freedom. Courts and regulators experience it as a complex coordination problem, balancing competing rights and interests.
 *
 * DIRECTIONALITY LOGIC:
 *   Vulnerable minorities and targets of hate speech are clear beneficiaries, as the constraint provides them with protection and recourse (low d). Speakers of harmful speech and free speech absolutists are targets, as their expressive freedom is curtailed or challenged (high d). Courts and regulators act as agenda-setters, defining and enforcing the boundaries, experiencing a more symmetric relationship (d near 0.5) as they bear the costs of adjudication while upholding a constitutional framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling coordination as pure extraction by acknowledging the genuine coordination function of defining speech boundaries for a more inclusive public sphere. However, it avoids mislabeling extraction as pure coordination by recognizing the real costs imposed on speakers and the active enforcement required. The 'live' status of the founding problem (harmful speech) and the 'world_rearranges' disappearance verdict indicate that the mandate is still relevant and not suffering from mandatrophy, though its application remains contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demonstrable_harm_definition,
    'What constitutes ''demonstrable unconsented-to harm'' in practice, and how consistently is it applied across different contexts and jurisdictions?',
    'Empirical analysis of court rulings and regulatory decisions, including cross-jurisdictional comparisons and studies on the actual impact of speech on individuals and groups.',
    'If the definition of harm is inconsistently applied or overly broad, the constraint''s effective extractiveness and suppression could be higher than intended, potentially reclassifying it closer to a Snare. If it is too narrow, the coordination function of protecting vulnerable groups would be undermined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrable_harm_definition, empirical, 'Ambiguity in the definition and application of the harm standard.').

omega_variable(
    absolutist_vs_harm_limited_framing,
    'Is the harm-limited reading a legitimate interpretation of the First Amendment''s original intent, or a modern re-framing that fundamentally alters its meaning?',
    'Historical-legal scholarship examining the evolution of free speech jurisprudence and the philosophical underpinnings of the First Amendment, alongside contemporary constitutional theory debates.',
    'If resolved as a fundamental alteration, the legitimacy of this reading would be challenged, potentially increasing resistance and decreasing its perceived coordination function for some stakeholders. If resolved as consistent with evolving constitutional principles, its legitimacy would be strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(absolutist_vs_harm_limited_framing, conceptual, 'Contest over the historical and philosophical legitimacy of the harm-limited interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_amendment_speech_protection__harm_limited_reading, 1969, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(firs_tr_t1969, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1969, 0.1).
narrative_ontology:measurement(firs_tr_t1980, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(firs_tr_t1990, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(firs_tr_t2000, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(firs_tr_t2010, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(firs_tr_t2024, first_amendment_speech_protection__harm_limited_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(firs_be_t1969, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1969, 0.5).
narrative_ontology:measurement(firs_be_t1980, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(firs_be_t1990, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(firs_be_t2000, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(firs_be_t2010, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(firs_be_t2024, first_amendment_speech_protection__harm_limited_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(firs_su_t1969, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1969, 0.3).
narrative_ontology:measurement(firs_su_t1980, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(firs_su_t1990, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(firs_su_t2000, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2000, 0.4).
narrative_ontology:measurement(firs_su_t2010, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2010, 0.42).
narrative_ontology:measurement(firs_su_t2024, first_amendment_speech_protection__harm_limited_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_amendment_speech_protection__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, absolutist_reading).
narrative_ontology:affects_constraint(first_amendment_speech_protection__harm_limited_reading, categorical_balancing_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'first_amendment_speech_protection' kernel. Its structural relationship to sibling readings (absolutist_reading, categorical_balancing_reading) is documented in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
