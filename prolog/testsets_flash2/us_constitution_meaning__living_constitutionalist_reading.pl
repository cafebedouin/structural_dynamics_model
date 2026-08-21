% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__living_constitutionalist_reading, []).

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
 *   constraint_id: us_constitution_meaning__living_constitutionalist_reading
 *   human_readable: US Constitution (Living Constitutionalist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'living constitutionalist' reading of the
 *   US Constitution, where its enduring principles are applied in a manner
 *   that evolves with social attitudes and circumstances. This reading views
 *   the Constitution as a dynamic document whose meaning is not fixed at the
 *   moment of its ratification but adapts to contemporary society. It is one
 *   reading of the broader 'us_constitution_meaning' kernel, which also
 *   includes originalist and positivist interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__living_constitutionalist_reading, 0.35).
domain_priors:suppression_score(us_constitution_meaning__living_constitutionalist_reading, 0.2).
domain_priors:theater_ratio(us_constitution_meaning__living_constitutionalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(us_constitution_meaning__living_constitutionalist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__living_constitutionalist_reading, rope).
narrative_ontology:human_readable(us_constitution_meaning__living_constitutionalist_reading, "US Constitution (Living Constitutionalist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__living_constitutionalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__living_constitutionalist_reading, '84694d03-cd30-4f10-b07d-41e7e18c34d3').
narrative_ontology:cs_kernel_codification('84694d03-cd30-4f10-b07d-41e7e18c34d3', fixed_text).
narrative_ontology:cs_authority_grounding('84694d03-cd30-4f10-b07d-41e7e18c34d3', lineage).
narrative_ontology:cs_interpretation_layer_present('84694d03-cd30-4f10-b07d-41e7e18c34d3').
narrative_ontology:cs_reading_relation('84694d03-cd30-4f10-b07d-41e7e18c34d3', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('84694d03-cd30-4f10-b07d-41e7e18c34d3', us_constitution_meaning__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('84694d03-cd30-4f10-b07d-41e7e18c34d3', foundational, constitution_as_living_document).
narrative_ontology:cs_axiom_status(constitution_as_living_document, holdable).
narrative_ontology:cs_axiom_grounding('84694d03-cd30-4f10-b07d-41e7e18c34d3', constitution_as_living_document, conventional).
narrative_ontology:cs_axiom('84694d03-cd30-4f10-b07d-41e7e18c34d3', foundational, evolving_standards_of_decency).
narrative_ontology:cs_axiom_status(evolving_standards_of_decency, holdable).
narrative_ontology:cs_axiom_grounding('84694d03-cd30-4f10-b07d-41e7e18c34d3', evolving_standards_of_decency, deontological).
narrative_ontology:cs_reference_frame('84694d03-cd30-4f10-b07d-41e7e18c34d3', evolving_constitutional_consensus).
narrative_ontology:cs_drift_state('84694d03-cd30-4f10-b07d-41e7e18c34d3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('84694d03-cd30-4f10-b07d-41e7e18c34d3', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__living_constitutionalist_reading, judicial_branch).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, majoritarian_will).
narrative_ontology:constraint_victim(us_constitution_meaning__living_constitutionalist_reading, states_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies constitutional principles, adapting them to contemporary social attitudes and circumstances. This reading empowers judges to evolve constitutional meaning, balancing enduring principles with societal change. They are constrained by precedent but also by the need for the Constitution to remain relevant.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, judicial_branch, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the expansion of constitutional rights and protections to new social contexts and previously unrecognized groups. This reading provides a mechanism for their claims to be heard and potentially vindicated through judicial review, even against majoritarian opposition.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, rights_claimants_evolving_contexts, beneficiary,
    moderate, biographical, constrained, national).

% Bears the cost of judicial decisions that may override legislative outcomes reflecting contemporary popular sentiment. This can lead to frustration and accusations of judicial overreach, as the will of the majority is sometimes constrained by evolving constitutional interpretations.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, majoritarian_will, payer,
    organized, immediate, constrained, national).

% Experience judicial interpretations that may limit state autonomy and legislative power in favor of national constitutional standards. This reading often centralizes power at the federal level, particularly in the judiciary, which can be seen as an erosion of state sovereignty.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, states_rights_advocates, payer,
    organized, generational, constrained, national).

% Would argue that this reading undermines the fixed meaning of the Constitution and judicial legitimacy by allowing subjective interpretation. They are excluded from the interpretive framework of this reading, as their core premise of fixed meaning is rejected.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, originalist_scholars_and_judges, excluded,
    institutional, generational, identity_locked, national).

% Observe the evolution of constitutional meaning through judicial practice, but may critique the reliance on extra-legal moral principles as a basis for validity. They analyze the institutional mechanisms of change rather than endorsing the normative claims of either living constitutionalism or originalism.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__living_constitutionalist_reading, legal_positivists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures the Constitution remains a relevant and adaptable framework for governance across changing societal norms and technological advancements, preventing it from becoming anachronistic and preserving its legitimacy over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from a strictly historical understanding to a dynamic one, empowering the judiciary to adapt constitutional meaning. This can transfer rights and protections to new groups, while potentially limiting the immediate legislative power of the majority.
% ABSENT_VOICES: Strict originalists and textualists are structurally marginalized within this interpretive framework; they would argue for a fixed, historically determined meaning and against judicial discretion in evolving constitutional principles.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the Constitution would likely become rigid and unable to address modern challenges, leading to increased political instability, a crisis of legitimacy for the document, and potentially a breakdown in the rule of law as new social realities clash with an unyielding legal text. Rights would not expand to new contexts without explicit amendment.
% FOUNDING_PROBLEM: The framers understood that a constitution written in one era would need to adapt to unforeseen future circumstances and societal changes to remain viable and legitimate over centuries.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, political scientists, and a significant portion of the judiciary attest to the ongoing need for constitutional adaptability to address new technologies, social movements, and evolving understandings of justice, citing historical examples where rigid interpretation would have led to societal breakdown or injustice. This is corroborated by the long-term stability of the US constitutional system compared to more rigid frameworks.
narrative_ontology:disappearance_verdict(us_constitution_meaning__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__living_constitutionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__living_constitutionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(us_constitution_meaning__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__living_constitutionalist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__living_constitutionalist_reading_tests).
:- end_tests(us_constitution_meaning__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.35) because while it empowers judges, it also imposes a 'counter-majoritarian' cost by sometimes overriding the immediate will of the people. Suppression is low (0.20) as it generally expands, rather than suppresses, the scope of rights and political participation, though it can suppress certain forms of majoritarian legislative action. Theater ratio is low (0.10) as the interpretive function is genuine and actively engaged in adapting the law, not merely performing. The metrics reflect a system that, while not without costs, primarily functions to coordinate societal evolution with foundational legal principles.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rights claimants, this reading is a vital mechanism for justice and progress. From the perspective of majoritarian will or states' rights advocates, it can appear as an unaccountable imposition. The judicial branch, as the agenda-setter, experiences it as a necessary and legitimate exercise of its constitutional role. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The judicial branch acts as the agenda-setter, actively shaping the interpretation. Rights claimants in evolving contexts are clear beneficiaries, as their claims are more likely to be recognized. The majoritarian will and states' rights advocates are payers, as their immediate preferences or autonomy may be constrained by judicial decisions. Legal positivists act as observers, analyzing the system without directly participating in its normative claims.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_legitimacy_boundary,
    'At what point does judicial adaptation of constitutional meaning cross the line into judicial overreach, undermining the democratic legitimacy of the Constitution?',
    'Empirical analysis of public trust in the judiciary, legislative responses to judicial decisions, and the frequency of constitutional amendment attempts following controversial rulings.',
    'If judicial overreach is perceived as frequent and severe, the constraint''s legitimacy (and thus its effective suppression) could erode, leading to increased resistance and calls for structural reform of the judiciary. If adaptation is seen as legitimate, the constraint''s coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_boundary, conceptual, 'The tension between judicial adaptability and democratic accountability.').

omega_variable(
    contemporary_moral_consensus_definition,
    'How is ''contemporary moral consensus'' identified and measured, and whose consensus counts in constitutional interpretation?',
    'Sociological studies of public opinion, analysis of legislative trends across states, and philosophical arguments regarding the nature of moral progress. This is an ongoing debate within legal theory.',
    'A clear, broadly accepted definition would strengthen the coherence and predictability of this reading, potentially reducing perceived extractiveness from majoritarian will. Ambiguity leaves the interpretation open to accusations of judicial subjectivity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contemporary_moral_consensus_definition, conceptual, 'The ambiguity in defining and applying ''contemporary moral consensus''.').

omega_variable(
    reading_coexistence_stability,
    'Given the fundamental disagreements between living constitutionalism and originalism, can these readings truly ''coexist'' in a stable legal system, or does one inevitably seek to displace the other?',
    'Longitudinal study of legal discourse, judicial appointments, and legislative efforts to codify or overturn interpretive methodologies. Analysis of periods of relative interpretive dominance and backlash.',
    'If one reading consistently seeks to displace the other, the ''coexists_with'' relation might be reclassified to ''influences'' or even ''forecloses'' under certain conditions, indicating a more active contest for interpretive hegemony rather than a stable pluralism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_coexistence_stability, empirical, 'The stability of coexistence between competing constitutional readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__living_constitutionalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 10, 0.07).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 20, 0.08).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 30, 0.09).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__living_constitutionalist_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 40, 0.34).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__living_constitutionalist_reading, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 30, 0.19).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__living_constitutionalist_reading, suppression_requirement, 50, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__living_constitutionalist_reading, us_constitution_meaning__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'us_constitution_meaning' kernel. Its structural properties and classification are distinct from the originalist and positivist readings, which are modeled as separate constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
