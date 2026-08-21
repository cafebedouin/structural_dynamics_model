% ============================================================================
% CONSTRAINT STORY: constitutional_text__popular_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__popular_sovereignty_reading, []).

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
 *   constraint_id: constitutional_text__popular_sovereignty_reading
 *   human_readable: Constitutional Authority from Popular Sovereignty
 *   domain: constitutional_theory/political_philosophy/comparative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'popular sovereignty' reading of
 *   the constitutional text, where the ultimate authority for constitutional
 *   interpretation resides with the constituent power of the people, rather
 *   than with judicial or legislative bodies. It posits that institutions are
 *   subordinate to extra-institutional democratic expression, with democratic
 *   participation as the primary beneficiary and institutional
 *   stability/expertise as the primary victim. The classification as a
 *   Tangled Rope reflects the genuine coordination function of legitimizing
 *   and adapting the constitution, coupled with the asymmetric extraction
 *   from established institutional power structures.
 *
 * KEY AGENTS:
 *   - the_demos: Primary agenda_setter and beneficiary (organized/civilizational) — benefits from ultimate authority and adaptability.
 *   - courts: Primary payer (institutional/generational) — bears the cost of potential popular override and delegitimization.
 *   - legislature: Primary payer (institutional/biographical) — bears the cost of potential popular override of its acts.
 *   - constitutional_scholars: Analytical observer (analytical/biographical) — analyzes the dynamics, but their expertise is subordinate.
 *   - revolutionary_movements: Excluded from formal process, but potential agenda_setter (organized/immediate) — represents a direct, extra-institutional assertion of popular will.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, 0.7).
domain_priors:suppression_score(constitutional_text__popular_sovereignty_reading, 0.65).
domain_priors:theater_ratio(constitutional_text__popular_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text__popular_sovereignty_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__popular_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__popular_sovereignty_reading, "Constitutional Authority from Popular Sovereignty").
narrative_ontology:topic_domain(constitutional_text__popular_sovereignty_reading, "constitutional_theory/political_philosophy/comparative_law").

domain_priors:requires_active_enforcement(constitutional_text__popular_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__popular_sovereignty_reading, 'f99d0ab6-3883-45f0-a9f3-acc89c664960').
narrative_ontology:cs_kernel_codification('f99d0ab6-3883-45f0-a9f3-acc89c664960', fixed_text).
narrative_ontology:cs_authority_grounding('f99d0ab6-3883-45f0-a9f3-acc89c664960', practice).
narrative_ontology:cs_interpretation_layer_present('f99d0ab6-3883-45f0-a9f3-acc89c664960').
narrative_ontology:cs_reading_relation('f99d0ab6-3883-45f0-a9f3-acc89c664960', constitutional_text__judicial_supremacy_reading, forecloses).
narrative_ontology:cs_reading_relation('f99d0ab6-3883-45f0-a9f3-acc89c664960', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_axiom('f99d0ab6-3883-45f0-a9f3-acc89c664960', foundational, popular_will_is_supreme).
narrative_ontology:cs_axiom_status(popular_will_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('f99d0ab6-3883-45f0-a9f3-acc89c664960', popular_will_is_supreme, deontological).
narrative_ontology:cs_axiom('f99d0ab6-3883-45f0-a9f3-acc89c664960', foundational, constituent_power_is_unlimited).
narrative_ontology:cs_axiom_status(constituent_power_is_unlimited, holdable).
narrative_ontology:cs_axiom_grounding('f99d0ab6-3883-45f0-a9f3-acc89c664960', constituent_power_is_unlimited, deontological).
narrative_ontology:cs_reference_frame('f99d0ab6-3883-45f0-a9f3-acc89c664960', founding_moment_of_popular_ratification).
narrative_ontology:cs_drift_state('f99d0ab6-3883-45f0-a9f3-acc89c664960', contemporary_institutional_entrenchment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f99d0ab6-3883-45f0-a9f3-acc89c664960', '').
narrative_ontology:cs_kernel_id(constitutional_text__popular_sovereignty_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, the_demos).
narrative_ontology:constraint_beneficiary(constitutional_text__popular_sovereignty_reading, democratic_participation).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, institutional_stability).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, judicial_expertise).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislative_prerogative).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, courts).
narrative_ontology:constraint_victim(constitutional_text__popular_sovereignty_reading, legislature).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__popular_sovereignty_reading, right_of_revolution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate source of constitutional authority, retaining interpretive power through various mechanisms. Benefits from the legitimacy and adaptability of the constitution. Bears the cost of vigilance and potential revolutionary action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, the_demos, agenda_setter,
    organized, civilizational, mobile, national).

% Institutional interpreters of the constitution whose authority is subordinate to the people's ultimate will. Bears the cost of potential popular override or delegitimization of their rulings.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, courts, payer,
    institutional, generational, constrained, national).

% The primary law-making body, but whose constitutional interpretations are also subordinate to the people's ultimate will. Bears the cost of potential popular override of their legislative acts or constitutional amendments.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Analyze and debate the theoretical and practical implications of popular sovereignty, often advocating for or against its active exercise. Their expertise is a resource for the demos but is also challenged by direct popular action.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, constitutional_scholars, observer,
    analytical, biographical, analytical, global).

% Groups that embody the most direct, extra-institutional expression of popular sovereignty, often challenging existing institutional arrangements. While formally excluded from the constitutional process, they represent a potential mechanism for the demos to assert its ultimate authority.
narrative_ontology:constraint_stakeholder(constitutional_text__popular_sovereignty_reading, revolutionary_movements, excluded,
    organized, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__popular_sovereignty_reading, revolutionary_movements, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the ultimate interpretive authority of the people (the demos) in constitutional matters, ensuring the constitution remains legitimate and adaptable to changing societal values through mechanisms like amendment, convention, or revolution.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from institutional actors (courts, legislature) to the constituent power of the people; transfers the risk of constitutional stagnation to the potential for institutional instability.
% ABSENT_VOICES: Those who prioritize institutional stability, judicial finality, or legislative supremacy would object, arguing that popular sovereignty, when directly exercised, risks instability, tyranny of the majority, or erosion of expert interpretation. They are often found within established institutional structures or academic circles that emphasize institutional safeguards.
% DISAPPEARANCE_RATIONALE: If the principle of popular sovereignty as ultimate constitutional authority vanished, the entire political system would fundamentally reorganize. Courts or legislatures would likely claim final interpretive authority, leading to a shift in the source of legitimacy and potentially a more rigid, less adaptable constitutional order. The very idea of 'the people' as a constituent power would cease to be a grounding for the state.
% FOUNDING_PROBLEM: The constraint was built to solve the problem of ensuring governmental legitimacy, preventing tyranny, and allowing for the adaptation of fundamental law to evolving societal norms, by vesting ultimate authority in the people rather than any single institutional branch.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists, historical movements, and international human rights bodies attest to the ongoing need for popular checks on institutional power and the dynamic nature of constitutional meaning. This supports the claim that the founding problem of preventing tyranny and ensuring legitimacy remains live, even as its mechanisms are debated.
narrative_ontology:disappearance_verdict(constitutional_text__popular_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__popular_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__popular_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(constitutional_text__popular_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__popular_sovereignty_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__popular_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__popular_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__popular_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because this reading fundamentally reallocates power, subordinating established institutions and extracting their claims to finality. Suppression (0.65) is also high, as institutions actively resist direct popular challenges to their authority. Theater ratio is low (0.1) because when popular sovereignty is genuinely invoked, it is a direct and often disruptive force, not a performance. Accessibility collapse is moderate (0.4) as alternative institutional supremacy claims persist, but this reading asserts their normative collapse. Resistance is high (0.75) due to the inherent conflict with entrenched institutional power.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'the demos', this constraint is a necessary mechanism for self-governance and constitutional vitality, ensuring the constitution remains a living document. From the perspective of 'courts' and 'legislature', it represents a threat to institutional order, stability, and the rule of law, potentially leading to chaos or majoritarian tyranny. The engine's classification captures this inherent tension.
 *
 * DIRECTIONALITY LOGIC:
 *   'The demos' and 'democratic_participation' are beneficiaries (low d) as they gain ultimate interpretive authority and constitutional adaptability. 'Institutional_stability', 'judicial_expertise', and 'legislative_prerogative' are victims (high d) as their claims to finality and autonomy are subordinated. 'Courts' and 'legislature' are payers, bearing the costs of this subordination. 'Revolutionary_movements' are excluded from formal processes but act as a potential mechanism for the demos, making their directionality complex but generally aligned with the demos when active.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure 'Rope' (simple coordination) by highlighting the significant extraction from institutional stability and expertise. It also avoids mislabeling it as a 'Snare' (pure extraction) by acknowledging the genuine coordination function of ensuring constitutional legitimacy and adaptability through popular will. The 'Tangled Rope' accurately captures the dual nature of coordination and asymmetric extraction inherent in this reading of constitutional authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    popular_sovereignty_mechanism_ambiguity,
    'What specific mechanisms (amendment, convention, revolution) legitimately constitute ''the people''s'' ultimate interpretive authority, and how are they invoked without descending into anarchy?',
    'Historical analysis of constitutional crises, comparative study of constitutional amendment processes, and philosophical debate on the nature and limits of constituent power.',
    'If the mechanisms are unclear or practically inaccessible, the claim of popular sovereignty becomes rhetorical, increasing effective extraction by institutions; if clear and accessible, it acts as a genuine check on institutional power, reducing effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(popular_sovereignty_mechanism_ambiguity, conceptual, 'Ambiguity in the practical exercise of popular sovereignty.').

omega_variable(
    institutional_stability_vs_popular_will,
    'What is the optimal balance between the stability provided by institutional constitutional interpretation and the dynamism of popular sovereignty?',
    'Empirical study of constitutional systems that prioritize one over the other, and normative philosophical debate on the trade-offs between stability, legitimacy, and adaptability.',
    'If institutional stability is deemed paramount, the ''victim'' status of institutional expertise is re-evaluated, potentially shifting the constraint towards a more ''rope''-like coordination of institutional roles; if popular will is paramount, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_stability_vs_popular_will, preference, 'Normative trade-off between institutional stability and popular interpretive authority.').

omega_variable(
    judicial_supremacy_foreclosure,
    'Does the popular sovereignty reading truly foreclose judicial supremacy, or do they coexist as competing claims within the same constitutional order?',
    'Analysis of constitutional crises where popular will directly challenged judicial rulings, and the subsequent legal and political outcomes. Conceptual analysis of the logical compatibility of their core premises.',
    'If judicial supremacy is not truly foreclosed, the relationship shifts to ''coexists_with'', indicating a persistent, unresolved tension rather than a structural subordination, potentially lowering the effective suppression metric from the popular sovereignty perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(judicial_supremacy_foreclosure, conceptual, 'Logical compatibility of popular sovereignty and judicial supremacy.').

omega_variable(
    legislative_sovereignty_foreclosure,
    'Does the popular sovereignty reading truly foreclose legislative sovereignty, or do they coexist as competing claims within the same constitutional order?',
    'Analysis of constitutional crises where popular will directly challenged legislative enactments, and the subsequent legal and political outcomes. Conceptual analysis of the logical compatibility of their core premises.',
    'If legislative sovereignty is not truly foreclosed, the relationship shifts to ''coexists_with'', indicating a persistent, unresolved tension rather than a structural subordination, potentially lowering the effective suppression metric from the popular sovereignty perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legislative_sovereignty_foreclosure, conceptual, 'Logical compatibility of popular sovereignty and legislative sovereignty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__popular_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__popular_sovereignty_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t6, constitutional_text__popular_sovereignty_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(cons_tr_t12, constitutional_text__popular_sovereignty_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(cons_tr_t18, constitutional_text__popular_sovereignty_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(cons_tr_t24, constitutional_text__popular_sovereignty_reading, theater_ratio, 24, 0.1).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__popular_sovereignty_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__popular_sovereignty_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(cons_be_t6, constitutional_text__popular_sovereignty_reading, base_extractiveness, 6, 0.67).
narrative_ontology:measurement(cons_be_t12, constitutional_text__popular_sovereignty_reading, base_extractiveness, 12, 0.68).
narrative_ontology:measurement(cons_be_t18, constitutional_text__popular_sovereignty_reading, base_extractiveness, 18, 0.69).
narrative_ontology:measurement(cons_be_t24, constitutional_text__popular_sovereignty_reading, base_extractiveness, 24, 0.7).
narrative_ontology:measurement(cons_be_t30, constitutional_text__popular_sovereignty_reading, base_extractiveness, 30, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__popular_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cons_su_t6, constitutional_text__popular_sovereignty_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(cons_su_t12, constitutional_text__popular_sovereignty_reading, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(cons_su_t18, constitutional_text__popular_sovereignty_reading, suppression_requirement, 18, 0.64).
narrative_ontology:measurement(cons_su_t24, constitutional_text__popular_sovereignty_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(cons_su_t30, constitutional_text__popular_sovereignty_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__popular_sovereignty_reading, identity_coordination).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__judicial_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_text__popular_sovereignty_reading, constitutional_text__legislative_sovereignty_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'constitutional_text' kernel, each representing a distinct claim about the ultimate source of constitutional authority and interpretation. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
