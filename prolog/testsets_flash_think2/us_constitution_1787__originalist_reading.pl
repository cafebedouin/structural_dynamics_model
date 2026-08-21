% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: US Constitution: Originalist Reading (Fixed Meaning at Ratification)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the originalist reading of the US
 *   Constitution, asserting that its meaning was fixed at the time of
 *   ratification (1787) and is binding on all subsequent generations. This
 *   interpretation emphasizes the framers' intent and the original public
 *   meaning of the text, actively suppressing evolving interpretations. The
 *   claimed type is 'mountain' from the originalist perspective, reflecting
 *   its assertion of fixed, natural law. However, the authored metrics (high
 *   extractiveness, suppression, and resistance) describe its actual
 *   operation as a contested, actively enforced interpretive framework, which
 *   the engine will measure as a divergence from the claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, mountain).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "US Constitution: Originalist Reading (Fixed Meaning at Ratification)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).
domain_priors:emerges_naturally(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '2acb2390-4955-4d1e-817d-7fe6747f392b').
narrative_ontology:cs_kernel_codification('2acb2390-4955-4d1e-817d-7fe6747f392b', fixed_text).
narrative_ontology:cs_authority_grounding('2acb2390-4955-4d1e-817d-7fe6747f392b', lineage).
narrative_ontology:cs_interpretation_layer_present('2acb2390-4955-4d1e-817d-7fe6747f392b').
narrative_ontology:cs_reading_relation('2acb2390-4955-4d1e-817d-7fe6747f392b', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('2acb2390-4955-4d1e-817d-7fe6747f392b', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('2acb2390-4955-4d1e-817d-7fe6747f392b', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('2acb2390-4955-4d1e-817d-7fe6747f392b', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('2acb2390-4955-4d1e-817d-7fe6747f392b', foundational, framers_intent_is_binding).
narrative_ontology:cs_axiom_status(framers_intent_is_binding, holdable).
narrative_ontology:cs_axiom_grounding('2acb2390-4955-4d1e-817d-7fe6747f392b', framers_intent_is_binding, conventional).
narrative_ontology:cs_reference_frame('2acb2390-4955-4d1e-817d-7fe6747f392b', founding_era_meaning).
narrative_ontology:cs_drift_state('2acb2390-4955-4d1e-817d-7fe6747f392b', contemporary_society_complexities, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('2acb2390-4955-4d1e-817d-7fe6747f392b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_scholars).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_legal_movements).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, states_rights_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, proponents_of_evolving_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_groups_seeking_modern_protections).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, rule_of_law_principle).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, separation_of_powers_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propagate originalist methodology, influence judicial appointments, and shape legal discourse. Their professional identity is deeply tied to this interpretive framework.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_scholars, agenda_setter,
    powerful, generational, identity_locked, national).

% Benefit from originalism's success in limiting federal power and modern social rights, aligning with their policy goals. They actively promote originalist judges and legal theories.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_legal_movements, beneficiary,
    organized, generational, constrained, national).

% Bear the costs of having modern rights claims (e.g., privacy, equality) rejected or narrowly construed based on historical interpretations. They advocate for alternative interpretive methods.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, proponents_of_evolving_rights, payer,
    organized, generational, constrained, national).

% The primary institutional body responsible for interpreting the Constitution. Judges may adopt, apply, or reject originalist methods, shaping the legal landscape for generations.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federal_judiciary, agenda_setter,
    institutional, civilizational, constrained, national).

% Often find their claims for new or expanded rights (e.g., LGBTQ+ rights, environmental protections) dismissed by originalist interpretations that find no historical basis for them, leaving them with limited legal recourse.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_groups_seeking_modern_protections, payer,
    powerless, generational, trapped, national).

% Benefit from originalism's tendency to limit federal power and uphold a more constrained view of the Commerce Clause or other federal authorities, thereby preserving state autonomy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, states_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Develop and advocate for alternative interpretive methods, critiquing originalism's historical rigidity. They are structurally excluded from the originalist framework's definition of legitimate constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_scholars, excluded,
    powerful, generational, analytical, national).

% The kernel itself, the object of interpretation. It is a fixed document, but its meaning is subject to ongoing contestation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, constitutional_text, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(us_constitution_1787__originalist_reading, constitutional_text).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__originalist_reading, conservative_legal_movements).
narrative_ontology:fixing_cost_class(us_constitution_1787__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework for legal interpretation by anchoring constitutional meaning to a fixed historical point (ratification), aiming to reduce judicial discretion and ensure consistency with the framers' original design.
% TRANSFER_FUNCTION: Transfers interpretive authority from contemporary society and evolving norms to the historical intentions of the framers and the original public meaning of the text, thereby limiting the scope of modern legislative and judicial action.
% ABSENT_VOICES: Proponents of evolving rights, living constitutionalism, and those whose interests are not served by an 18th-century understanding are actively excluded from the originalist framework's legitimate interpretive community. The framers themselves, if they could speak, might also offer a different perspective on their intent or the adaptability of their work.
% DISAPPEARANCE_RATIONALE: If originalism vanished overnight, constitutional interpretation would immediately shift to other methods (e.g., living constitutionalism, textualism, pragmatism), leading to significantly different legal outcomes, especially in areas of social rights, federal power, and individual liberties. The composition of the judiciary and the nature of legal arguments would fundamentally change.
% FOUNDING_PROBLEM: To prevent judicial overreach and ensure that the Constitution's meaning remains consistent with the intent of its creators, thereby preserving democratic self-governance and the rule of law against subjective judicial preferences.
% FOUNDING_PROBLEM_CORROBORATION: Originalist scholars and conservative legal groups attest that the problem of judicial overreach and interpretive instability is still live. Living constitutionalist scholars, many legal historians, and civil rights advocates argue that the founding problem is substantially solved or that a rigid adherence to 18th-century intent creates new problems for a modern, diverse society, making the originalist solution itself problematic.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, ExtMetricName, E),
    domain_priors:suppression_score(us_constitution_1787__originalist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(us_constitution_1787__originalist_reading),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading imposes significant costs on those seeking to adapt the Constitution to modern social realities, effectively extracting contemporary interpretive flexibility. Suppression is high due to the active intellectual and institutional effort to delegitimize and exclude alternative interpretive methods. Theater ratio is moderate, reflecting genuine scholarly effort in historical inquiry, but also the performative aspect of asserting a singular, discoverable 'original intent' in complex historical contexts. Resistance is high because this interpretive method is constantly challenged by other legal theories and social movements.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist's perspective, this constraint is a 'mountain' – the unchangeable, true meaning of the Constitution. From the perspective of those advocating for evolving rights, it operates as a 'snare' or 'tangled_rope,' actively extracting from modern society by binding it to historical norms and suppressing alternatives. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist scholars and conservative legal movements are beneficiaries, as this reading aligns with their ideological and policy goals, preserving certain power structures. Proponents of evolving rights and marginalized groups are victims, as their claims are often curtailed by this fixed interpretation. The federal judiciary acts as an agenda-setter, as its adoption or rejection of originalism directly shapes the constraint's impact.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_intent_discoverability,
    'Is ''the framers'' intent'' a singular, discoverable historical fact, or a complex, often contradictory set of individual and collective understandings that is inherently ambiguous?',
    'Further historical and textual scholarship, including analysis of debates, private correspondence, and contemporary dictionaries. However, complete resolution may be impossible due to the nature of historical evidence.',
    'If intent is largely undiscoverable or contradictory, the originalist method''s claim to objectivity weakens, potentially reducing its perceived legitimacy and extractiveness. If it is largely discoverable, the method''s claims are strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_intent_discoverability, empirical, 'Ambiguity of ''framers'' intent'' as a fixed interpretive anchor.').

omega_variable(
    natural_law_vs_constructed_interpretation,
    'Is the fixed meaning of the Constitution a ''natural law'' of constitutionalism (a Mountain), or is originalism a constructed interpretive methodology (a Snare or Tangled Rope) that benefits identifiable agents?',
    'Analysis of the historical development of originalism as a legal theory, its proponents, and its impact on legal outcomes, particularly in areas where it diverges from other interpretive methods.',
    'If originalism is primarily a constructed methodology, its ''mountain'' claim is a false summit, and its classification would shift to reflect its active enforcement and beneficiary structure (e.g., Tangled Rope or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_interpretation, conceptual, 'Whether originalism is a genuine natural law or a constructed interpretive framework.').

omega_variable(
    framing_under_determination,
    'Does the choice between originalist, living, and positivist framings represent an irreducible conceptual under-determination in constitutional interpretation, or can one framing be definitively shown to be superior?',
    'Ongoing philosophical and legal debate, potentially influenced by shifts in societal values or the perceived efficacy of each approach in maintaining constitutional order and justice. No definitive resolution is expected.',
    'If under-determination is irreducible, the contest between readings is a permanent feature of constitutional law, and the ''truth'' of any single reading remains contested. If one is superior, the others would lose legitimacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Irreducible conceptual under-determination in constitutional interpretive framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1970, us_constitution_1787__originalist_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(us_c_tr_t1980, us_constitution_1787__originalist_reading, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(us_c_tr_t1990, us_constitution_1787__originalist_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__originalist_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__originalist_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(us_c_tr_t2020, us_constitution_1787__originalist_reading, theater_ratio, 2020, 0.4).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1970, us_constitution_1787__originalist_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(us_c_be_t1980, us_constitution_1787__originalist_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(us_c_be_t1990, us_constitution_1787__originalist_reading, base_extractiveness, 1990, 0.58).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__originalist_reading, base_extractiveness, 2000, 0.62).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__originalist_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement(us_c_be_t2020, us_constitution_1787__originalist_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1970, us_constitution_1787__originalist_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(us_c_su_t1980, us_constitution_1787__originalist_reading, suppression_requirement, 1980, 0.63).
narrative_ontology:measurement(us_c_su_t1990, us_constitution_1787__originalist_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__originalist_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__originalist_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(us_c_su_t2020, us_constitution_1787__originalist_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, identity_coordination).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787), each with its own structural properties and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
