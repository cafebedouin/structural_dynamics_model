% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Competent Individual's Sovereign Authority Over Death (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint represents the 'autonomy_reading' of the contested
 *   'end_of_life_decision_authority' kernel. It asserts that competent
 *   individuals possess sovereign authority over their own death. While
 *   framed as a fundamental right, its realization is often contested and
 *   requires active enforcement against legal and institutional barriers. The
 *   constraint functions as a Tangled Rope: it offers a genuine coordination
 *   function (aligning individual will with medical practice) but also
 *   involves asymmetric extraction (prolonged suffering for those denied this
 *   authority). The metrics reflect the ongoing struggle for this authority,
 *   with rising extractiveness for those denied, and moderate suppression
 *   from opposing legal and ethical frameworks.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.65).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.55).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Competent Individual's Sovereign Authority Over Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'c4425414-e16a-4a89-846b-b1dd5cbb5e20').
narrative_ontology:cs_kernel_codification('c4425414-e16a-4a89-846b-b1dd5cbb5e20', formalized).
narrative_ontology:cs_authority_grounding('c4425414-e16a-4a89-846b-b1dd5cbb5e20', lineage).
narrative_ontology:cs_interpretation_layer_present('c4425414-e16a-4a89-846b-b1dd5cbb5e20').
narrative_ontology:cs_reading_relation('c4425414-e16a-4a89-846b-b1dd5cbb5e20', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4425414-e16a-4a89-846b-b1dd5cbb5e20', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('c4425414-e16a-4a89-846b-b1dd5cbb5e20', foundational, individual_autonomy_is_paramount).
narrative_ontology:cs_axiom_status(individual_autonomy_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('c4425414-e16a-4a89-846b-b1dd5cbb5e20', individual_autonomy_is_paramount, deontological).
narrative_ontology:cs_axiom('c4425414-e16a-4a89-846b-b1dd5cbb5e20', secondary, relief_of_suffering_is_moral_imperative).
narrative_ontology:cs_axiom_status(relief_of_suffering_is_moral_imperative, holdable).
narrative_ontology:cs_axiom_grounding('c4425414-e16a-4a89-846b-b1dd5cbb5e20', relief_of_suffering_is_moral_imperative, deontological).
narrative_ontology:cs_reference_frame('c4425414-e16a-4a89-846b-b1dd5cbb5e20', individual_self_determination).
narrative_ontology:cs_drift_state('c4425414-e16a-4a89-846b-b1dd5cbb5e20', contemporary_bioethics_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c4425414-e16a-4a89-846b-b1dd5cbb5e20', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_eold).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, facilitating_healthcare_professionals).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, competent_individuals_denied_eold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, opposing_healthcare_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who, being competent, wish to exercise their sovereign authority over their own death. They benefit when their wishes are respected and facilitated by the medical and legal systems, allowing for a dignified and self-determined end-of-life.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_eold, beneficiary,
    powerful, biographical, constrained, global).

% Competent individuals whose requests for end-of-life decisions (e.g., medical aid in dying) are denied due to legal, institutional, or conscientious objections. They bear the cost of prolonged suffering and loss of autonomy, effectively trapped by the system's refusal to acknowledge their authority.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_individuals_denied_eold, payer,
    powerless, immediate, trapped, global).

% Medical professionals who, in jurisdictions where it is legal, facilitate end-of-life decisions in accordance with patient autonomy. They benefit from practicing within a framework that respects patient rights, but are constrained by legal and ethical guidelines.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, facilitating_healthcare_professionals, agenda_setter,
    institutional, biographical, constrained, national).

% Medical professionals who, due to moral or religious objections, oppose participating in end-of-life decisions. While the autonomy reading primarily focuses on patient rights, these professionals may experience 'payer' dynamics if forced to participate or if their professional autonomy is curtailed by mandates to refer or facilitate.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, opposing_healthcare_professionals, payer,
    moderate, biographical, constrained, national).

% The legislative and judicial bodies that establish and interpret laws governing end-of-life decisions. They set the boundaries for individual authority, often balancing autonomy with other societal values. Their decisions directly enable or deny the exercise of this authority.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Scholars and practitioners who analyze the ethical implications of end-of-life decisions, contributing to policy debates and medical guidelines. They observe the unfolding dynamics without directly participating in the decision-making or enforcement.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethicists, observer,
    analytical, generational, analytical, global).

% Organizations that often hold strong theological positions on the sanctity of life and the morality of intentional death. While not directly involved in individual medical decisions, they exert significant influence on public opinion and legislative efforts, often advocating against the expansion of end-of-life autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions, excluded,
    organized, civilizational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, competent_individuals_seeking_eold).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the autonomous will of competent individuals regarding their end-of-life decisions with medical practice and legal frameworks, ensuring a dignified and self-determined death where desired and legally permissible.
% TRANSFER_FUNCTION: Transfers the ultimate decision-making authority over one's death from external authorities (state, family, medical paternalism) to the competent individual. When this authority is denied, it effectively transfers prolonged suffering and loss of dignity to the individual.
% ABSENT_VOICES: Individuals who are deemed incompetent but might have expressed wishes earlier; those who fear the 'slippery slope' of expanding end-of-life options but are not directly involved in individual cases; and those whose religious or moral objections are overridden by legal mandates to facilitate or refer.
% DISAPPEARANCE_RATIONALE: If the recognition of competent individuals' sovereign authority over their own death vanished overnight, it would lead to a profound loss of fundamental rights, increased prolonged suffering for many, a surge in legal battles over end-of-life care, and a significant shift back towards medical or state paternalism, fundamentally reorganizing end-of-life practices and societal values.
% FOUNDING_PROBLEM: The historical problem of individuals facing prolonged suffering and loss of dignity at the end of life, against their will, due to a lack of legal or medical recognition for their right to self-determination in death.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups, human rights organizations, and a significant portion of the medical and legal community corroborate the ongoing need for this authority, citing numerous cases of prolonged suffering and loss of dignity in the absence of recognized end-of-life autonomy. This is supported by legislative efforts in various jurisdictions to expand access to medical aid in dying.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate-to-high because the denial of this 'sovereign authority' directly leads to significant suffering and loss of dignity for individuals, which is a form of extraction. Suppression is moderate as legal and institutional barriers actively prevent the exercise of this authority in many places, though advocacy and legislative efforts provide some avenues for resistance. The theater ratio is low because the debate is genuinely about fundamental rights and values, not performative maintenance of an atrophied function. The claimed type is Tangled Rope because it has a clear coordination function (facilitating autonomous decisions) but also identifiable victims (those denied) and requires active enforcement to overcome resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of competent individuals seeking to exercise this authority, it is a fundamental right that should be a pure Rope. However, from the perspective of those denied, it operates as a Snare, trapping them in unwanted suffering. The legal system and opposing institutions often frame it as a complex ethical dilemma, highlighting the coordination challenges and potential for abuse. The engine's classification as Tangled Rope captures this hybrid nature, acknowledging both the coordination function and the asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent individuals seeking to exercise this authority are beneficiaries when their rights are recognized and facilitated (low d). Conversely, competent individuals denied this authority are clear targets/victims, bearing the full cost of prolonged suffering (high d). Facilitating healthcare professionals act as agenda-setters, enabling the exercise of this authority. Opposing healthcare professionals and religious institutions, while not direct targets of extraction in the same way as denied patients, bear costs if their own professional or moral autonomy is challenged by mandates to participate or refer.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_risk_assessment,
    'Does the expansion of individual sovereign authority over death inevitably lead to a ''slippery slope'' where vulnerable populations are coerced or pressured into end-of-life decisions?',
    'Longitudinal empirical studies from jurisdictions with legalized medical aid in dying, tracking rates of coercion, demographic shifts in recipients, and safeguards effectiveness.',
    'If a ''slippery slope'' is empirically validated, it would strengthen the ''vulnerability_protection_reading'' and potentially lead to reclassification of the autonomy reading as more extractive due to unintended victim sets. If disproven, it would bolster the autonomy reading''s claim to be a pure coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slippery_slope_risk_assessment, empirical, 'Empirical evidence for or against the ''slippery slope'' argument in end-of-life policy.').

omega_variable(
    competence_definition_ambiguity,
    'How is ''competence'' defined and assessed in practice, and does this definition adequately protect individuals while respecting autonomy?',
    'Analysis of legal precedents, medical guidelines, and psychiatric assessments across jurisdictions, focusing on consistency, reliability, and potential for bias or arbitrary denial.',
    'If competence definitions are found to be inconsistently applied or overly restrictive, it would increase the measured suppression and extractiveness for individuals denied access, potentially shifting the classification towards Snare. If robust and consistently applied, it would support the Tangled Rope classification''s coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_definition_ambiguity, conceptual, 'Ambiguity in the definition and assessment of ''competence'' for end-of-life decisions.').

omega_variable(
    sanctity_vs_autonomy_priority,
    'Which normative principle (individual autonomy or the sanctity of life) should take precedence in end-of-life decisions?',
    'This is a fundamental ethical and philosophical question, not resolvable by empirical data. Resolution depends on societal values, legal frameworks, and individual moral commitments.',
    'If sanctity of life is prioritized, the autonomy reading would be seen as fundamentally flawed and potentially extractive from a different moral framework. If autonomy is prioritized, the sanctity reading would be seen as imposing unwanted suffering. This omega highlights the preference-based nature of the core conflict between this reading and the ''sanctity_reading'' sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctity_vs_autonomy_priority, preference, 'The fundamental normative conflict between individual autonomy and the sanctity of life.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(end__tr_t1980, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(end__tr_t1990, end_of_life_decision_authority__autonomy_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(end__tr_t2000, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(end__tr_t2010, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(end__tr_t2020, end_of_life_decision_authority__autonomy_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(end__be_t1980, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(end__be_t1990, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(end__be_t2000, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(end__be_t2010, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(end__be_t2020, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1970, 0.45).
narrative_ontology:measurement(end__su_t1980, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement(end__su_t1990, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 1990, 0.53).
narrative_ontology:measurement(end__su_t2000, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2000, 0.54).
narrative_ontology:measurement(end__su_t2010, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(end__su_t2020, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
