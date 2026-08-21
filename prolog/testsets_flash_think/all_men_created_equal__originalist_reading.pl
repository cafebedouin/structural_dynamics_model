% ============================================================================
% CONSTRAINT STORY: all_men_created_equal__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_all_men_created_equal__originalist_reading, []).

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
 *   constraint_id: all_men_created_equal__originalist_reading
 *   human_readable: Equality Bounded by Originalist Intent
 *   domain: constitutional_law/political_philosophy/american_studies
 *
 * SUMMARY:
 *   This constraint represents the 'originalist reading' of the foundational
 *   American principle 'all men are created equal.' It interprets equality
 *   strictly according to the perceived intent of the 18th-century framers,
 *   bounding its scope by the social taxonomy of that era (e.g., excluding
 *   women, enslaved persons, and non-propertied men from full political
 *   equality). This reading is actively enforced through judicial
 *   interpretation and legal scholarship, benefiting those who align with or
 *   descend from the historical power structures, while systematically
 *   disadvantaging historically excluded and marginalized groups. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as a stable
 *   interpretive framework (a form of 'rope' or even 'mountain' by its
 *   proponents) while the authored metrics describe substantially extractive,
 *   actively enforced operation — the engine measures that divergence; do not
 *   reconcile the claim to the metrics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, 0.85).
domain_priors:suppression_score(all_men_created_equal__originalist_reading, 0.9).
domain_priors:theater_ratio(all_men_created_equal__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(all_men_created_equal__originalist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(all_men_created_equal__originalist_reading, snare).
narrative_ontology:human_readable(all_men_created_equal__originalist_reading, "Equality Bounded by Originalist Intent").
narrative_ontology:topic_domain(all_men_created_equal__originalist_reading, "constitutional_law/political_philosophy/american_studies").

domain_priors:requires_active_enforcement(all_men_created_equal__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(all_men_created_equal__originalist_reading, 'd120b0d7-b1cc-419f-bc96-6f94b9fd75a7').
narrative_ontology:cs_kernel_codification('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', fixed_text).
narrative_ontology:cs_authority_grounding('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', lineage).
narrative_ontology:cs_interpretation_layer_present('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7').
narrative_ontology:cs_reading_relation('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', all_men_created_equal__universalist_reading, forecloses).
narrative_ontology:cs_reading_relation('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', all_men_created_equal__textualist_paradox_reading, coexists_with).
narrative_ontology:cs_axiom('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', foundational, original_intent_supremacy).
narrative_ontology:cs_axiom_status(original_intent_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', original_intent_supremacy, conventional).
narrative_ontology:cs_axiom('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', foundational, fixed_meaning_of_constitutional_text).
narrative_ontology:cs_axiom_status(fixed_meaning_of_constitutional_text, holdable).
narrative_ontology:cs_axiom_grounding('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', fixed_meaning_of_constitutional_text, conventional).
narrative_ontology:cs_reference_frame('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', founding_era_social_order).
narrative_ontology:cs_drift_state('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', contemporary_civil_rights_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('d120b0d7-b1cc-419f-bc96-6f94b9fd75a7', '').
narrative_ontology:cs_kernel_id(all_men_created_equal__originalist_reading, all_men_created_equal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, originalist_jurists).
narrative_ontology:constraint_beneficiary(all_men_created_equal__originalist_reading, founding_elite_descendants).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, historically_excluded_groups).
narrative_ontology:constraint_victim(all_men_created_equal__originalist_reading, marginalized_communities).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, original_intent_doctrine).
narrative_ontology:constraint_vindicates(all_men_created_equal__originalist_reading, constitutional_conservatism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret constitutional texts, including 'all men are created equal,' strictly according to the perceived intent of the 18th-century framers. Their professional identity and authority are tied to this interpretive method, which often limits the scope of equality to a narrow, historical understanding. They actively enforce this interpretation through judicial rulings.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, originalist_jurists, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the preservation of social and economic hierarchies that were either explicit or implicit in the 18th-century social taxonomy. The originalist reading of equality helps maintain the legitimacy of these inherited advantages by limiting challenges based on broader interpretations of equality.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, founding_elite_descendants, beneficiary,
    powerful, generational, mobile, national).

% Bear the direct costs of a constrained definition of equality, experiencing ongoing systemic discrimination and denial of rights that would be recognized under a more expansive interpretation. Their historical exclusion is perpetuated by the originalist framework, making exit from their disadvantaged position extremely difficult.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, historically_excluded_groups, payer,
    powerless, generational, trapped, national).

% Experience the practical effects of limited equality in their daily lives, facing barriers in areas like housing, education, and employment. While they may organize locally, their ability to challenge the national constitutional framework is severely constrained.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, marginalized_communities, payer,
    powerless, biographical, constrained, local).

% Argue for an expansive, evolving understanding of equality that applies universally to all persons, regardless of historical context or framers' intent. Their arguments are often dismissed within originalist legal discourse as illegitimate or anachronistic, effectively excluding them from the dominant interpretive conversation.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, universalist_advocates, excluded,
    organized, generational, mobile, national).

% Analyze the historical development and contemporary application of constitutional principles, including the concept of equality. They critically evaluate different interpretive methodologies, including originalism, and document its effects on various social groups without directly participating in its enforcement or suffering its direct costs.
narrative_ontology:constraint_stakeholder(all_men_created_equal__originalist_reading, constitutional_scholars, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(all_men_created_equal__originalist_reading, originalist_jurists).
narrative_ontology:fixing_cost_class(all_men_created_equal__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, historically grounded framework for constitutional interpretation, aiming to prevent radical shifts in legal meaning and maintain fidelity to the perceived original design of the republic.
% TRANSFER_FUNCTION: Transfers interpretive authority and the benefits of a narrow definition of equality to those aligned with the founders' original intent (e.g., originalist jurists, descendants of the founding elite), at the cost of denying rights and full equality to historically excluded and marginalized groups.
% ABSENT_VOICES: Universalist advocates, civil rights organizations, and representatives of marginalized communities are often structurally excluded from the dominant interpretive process, their arguments for an evolving, expansive equality dismissed as not adhering to the originalist framework. Their perspectives are present in public discourse but absent from the authoritative legal interpretation.
% DISAPPEARANCE_RATIONALE: If the originalist constraint on 'all men are created equal' vanished overnight, the legal and political landscape regarding equality would undergo significant reinterpretation. Courts would likely adopt more expansive readings, leading to rapid expansion of rights, re-evaluation of historical injustices, and a fundamental shift in the balance of power and privilege within society.
% FOUNDING_PROBLEM: To establish a stable republic with a clear, limited scope of government and rights, as understood by the framers, preventing arbitrary expansion of power or rights beyond the original constitutional design.
% FOUNDING_PROBLEM_CORROBORATION: Originalist jurists and conservative legal scholars attest the problem of maintaining constitutional fidelity and preventing judicial overreach is still live. Civil rights advocates, critical race theorists, and many historians attest the founding problem (establishing a republic) is largely solved, but the constraint now primarily serves to maintain historical power structures and is no longer a legitimate solution to a live problem. Legislative hearing testimony and independent historical analysis from outside the benefiting parties support the shifted-function reading.
narrative_ontology:disappearance_verdict(all_men_created_equal__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(all_men_created_equal__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(all_men_created_equal__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(all_men_created_equal__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(all_men_created_equal__originalist_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(all_men_created_equal__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(all_men_created_equal__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(all_men_created_equal__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.85) because this reading systematically denies full equality and rights to large segments of the population, channeling benefits to a narrow group. Suppression is extremely high (0.90) as it requires active legal and institutional enforcement to resist continuous challenges from universalist interpretations and social movements. The accessibility collapse is high (0.88) because it actively forecloses alternative, broader interpretations of equality within the legal framework. Resistance is also high (0.75) due to ongoing social and legal challenges. Theater ratio is moderate (0.40): while there is a genuine effort to maintain a consistent interpretive methodology, a significant portion of the effort is performative, defending a narrow definition of equality against evolving societal norms under the guise of 'fidelity' to the past.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of originalist jurists and beneficiaries, this constraint provides essential stability and fidelity to the constitutional text, preventing arbitrary judicial activism. From the perspective of historically excluded groups and universalist advocates, it is a mechanism of ongoing oppression and denial of fundamental rights, leveraging historical context to maintain contemporary power imbalances. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist jurists and founding elite descendants are clear beneficiaries (low directionality), as the constraint legitimizes their interpretive authority and inherited advantages. Historically excluded groups and marginalized communities are clear targets (high directionality), bearing the direct costs of denied equality. Universalist advocates are excluded, their arguments systematically dismissed, making them targets of the constraint's suppressive function. Constitutional scholars act as analytical observers, neither directly benefiting nor suffering.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem of establishing a stable republic with limited government is arguably solved. However, the originalist constraint persists, with its function shifting from foundational stability to maintaining a specific, historically bounded social order. The contest over its 'founding problem status' (live vs. dead) highlights this mandatrophy: proponents argue it still prevents judicial overreach, while critics contend it now primarily serves to extract benefits by denying evolving conceptions of equality. The constraint's high extractiveness and suppression suggest it is no longer a pure coordination mechanism for a live problem, but rather an actively defended extractive structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, stable interpretation of ''all men are created equal,'' or one of several contested readings of a kernel?',
    'Analysis of legal and philosophical discourse across different interpretive communities; identification of alternative, coherent readings of the same foundational text.',
    'If it is one reading of a kernel, its classification is relative to other readings, and its ''naturalness'' is reduced. This story instantiates the ''originalist_reading'' of the ''all_men_created_equal'' kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the originalist reading of the ''all_men_created_equal'' kernel.').

omega_variable(
    original_intent_ambiguity,
    'Whose ''original intent'' governs the scope of equality, and how are conflicting intentions or evolving understandings within the founding era reconciled?',
    'Deep historical and textual analysis, including primary sources from diverse framers and contemporary commentators, to identify the range of views on equality during the founding period.',
    'If ''original intent'' is found to be internally inconsistent or ambiguous, the constraint''s claim to stable, fixed meaning is undermined, potentially reducing its perceived legitimacy and increasing its theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_ambiguity, empirical, 'Ambiguity in defining and applying ''original intent'' for equality.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal precedent, institutional power) or internalized (belief in the legitimacy of originalism by those it disadvantages)?',
    'Post-ruling trajectory of affected groups: if challenges persist and intensify after legal setbacks, suppression is primarily structural. If challenges wane, internalized suppression may be at play.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as targets carry the suppression with them. If purely structural, legal reforms could more readily alter the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for originalist interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(all_men_created_equal__originalist_reading, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(all__tr_t1776, all_men_created_equal__originalist_reading, theater_ratio, 1776, 0.2).
narrative_ontology:measurement(all__tr_t1865, all_men_created_equal__originalist_reading, theater_ratio, 1865, 0.25).
narrative_ontology:measurement(all__tr_t1954, all_men_created_equal__originalist_reading, theater_ratio, 1954, 0.3).
narrative_ontology:measurement(all__tr_t1980, all_men_created_equal__originalist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(all__tr_t2000, all_men_created_equal__originalist_reading, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(all__tr_t2024, all_men_created_equal__originalist_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(all__be_t1776, all_men_created_equal__originalist_reading, base_extractiveness, 1776, 0.6).
narrative_ontology:measurement(all__be_t1865, all_men_created_equal__originalist_reading, base_extractiveness, 1865, 0.7).
narrative_ontology:measurement(all__be_t1954, all_men_created_equal__originalist_reading, base_extractiveness, 1954, 0.78).
narrative_ontology:measurement(all__be_t1980, all_men_created_equal__originalist_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(all__be_t2000, all_men_created_equal__originalist_reading, base_extractiveness, 2000, 0.84).
narrative_ontology:measurement(all__be_t2024, all_men_created_equal__originalist_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(all__su_t1776, all_men_created_equal__originalist_reading, suppression_requirement, 1776, 0.7).
narrative_ontology:measurement(all__su_t1865, all_men_created_equal__originalist_reading, suppression_requirement, 1865, 0.8).
narrative_ontology:measurement(all__su_t1954, all_men_created_equal__originalist_reading, suppression_requirement, 1954, 0.85).
narrative_ontology:measurement(all__su_t1980, all_men_created_equal__originalist_reading, suppression_requirement, 1980, 0.88).
narrative_ontology:measurement(all__su_t2000, all_men_created_equal__originalist_reading, suppression_requirement, 2000, 0.89).
narrative_ontology:measurement(all__su_t2024, all_men_created_equal__originalist_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(all_men_created_equal__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, constitutional_interpretation_doctrine).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, civil_rights_legislation).
narrative_ontology:affects_constraint(all_men_created_equal__originalist_reading, voting_rights_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'all_men_created_equal' kernel. It represents the originalist interpretation, which bounds equality by 18th-century social taxonomy. It is linked to the 'universalist_reading' and 'textualist_paradox_reading' as part of a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
