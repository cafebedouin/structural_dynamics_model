% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/political_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the 'judicial supremacy' reading of
 *   constitutional interpretive authority, where courts, through their
 *   specialized legal expertise and independence, hold the final say on
 *   constitutional meaning. This reading positions the judiciary as the
 *   ultimate arbiter, often at the expense of legislative and popular will.
 *   It is one of several competing readings of how constitutional authority
 *   should be exercised.
 *
 * KEY AGENTS:
 *   - judiciary: Primary agenda_setter (institutional/identity_locked) — asserts and enforces final interpretive authority.
 *   - legal_profession: Primary beneficiary (organized/constrained) — benefits from the specialized nature of constitutional law.
 *   - legislature: Primary payer (institutional/constrained) — bears costs of judicial review overturning legislation.
 *   - electoral_majorities: Primary payer (powerless/trapped) — experience democratic will constrained by judicial rulings.
 *   - executive_branch: Secondary payer (institutional/constrained) — must operate within judicially defined constitutional limits.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.7).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/political_theory/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '77942e4e-acd9-426a-b26b-c513c98b2c16').
narrative_ontology:cs_kernel_codification('77942e4e-acd9-426a-b26b-c513c98b2c16', fixed_text).
narrative_ontology:cs_authority_grounding('77942e4e-acd9-426a-b26b-c513c98b2c16', lineage).
narrative_ontology:cs_interpretation_layer_present('77942e4e-acd9-426a-b26b-c513c98b2c16').
narrative_ontology:cs_reading_relation('77942e4e-acd9-426a-b26b-c513c98b2c16', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('77942e4e-acd9-426a-b26b-c513c98b2c16', basic_law_interpretive_authority__popular_constitutionalism_reading, coexists_with).
narrative_ontology:cs_axiom('77942e4e-acd9-426a-b26b-c513c98b2c16', foundational, judicial_expertise_supremacy).
narrative_ontology:cs_axiom_status(judicial_expertise_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('77942e4e-acd9-426a-b26b-c513c98b2c16', judicial_expertise_supremacy, conventional).
narrative_ontology:cs_axiom('77942e4e-acd9-426a-b26b-c513c98b2c16', foundational, constitutional_stability_through_judicial_finality).
narrative_ontology:cs_axiom_status(constitutional_stability_through_judicial_finality, holdable).
narrative_ontology:cs_axiom_grounding('77942e4e-acd9-426a-b26b-c513c98b2c16', constitutional_stability_through_judicial_finality, instrumental).
narrative_ontology:cs_reference_frame('77942e4e-acd9-426a-b26b-c513c98b2c16', marbury_v_madison_precedent).
narrative_ontology:cs_drift_state('77942e4e-acd9-426a-b26b-c513c98b2c16', contemporary_political_polarization, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('77942e4e-acd9-426a-b26b-c513c98b2c16', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The courts, particularly supreme and constitutional courts, assert and exercise the final authority to interpret the constitution. This grants them significant institutional power and shapes the legal landscape. Their independence from direct political pressure is a core tenet of this authority.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefits from the complexity and specialized nature of constitutional law, which judicial supremacy entrenches. Expertise in this domain becomes highly valued, creating career paths and influence for lawyers, scholars, and advocates who navigate the judicial system.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legal_profession, beneficiary,
    organized, biographical, constrained, national).

% Bears the cost of having its legislative output subject to judicial review. Laws passed by elected representatives can be struck down, leading to policy gridlock, frustration of democratic will, and the need to craft legislation to anticipate judicial interpretation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, legislature, payer,
    institutional, biographical, constrained, national).

% Experience the frustration of having their democratically expressed preferences overturned or constrained by unelected judges. Their ability to effect change through the ballot box is limited by judicial interpretations, leading to a sense of disempowerment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    powerless, immediate, trapped, national).

% Must implement and enforce laws within the bounds set by judicial interpretations. Executive actions and policies can be challenged and overturned by courts, limiting their discretion and requiring careful legal navigation.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, executive_branch, payer,
    institutional, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, authoritative interpretation of the constitution, preventing conflicting interpretations from different branches of government and ensuring legal consistency across jurisdictions. It aims to protect fundamental rights and minority interests from majoritarian overreach.
% TRANSFER_FUNCTION: Transfers final interpretive power over constitutional meaning from elected branches and popular will to the judiciary. This transfers institutional authority, prestige, and the power to shape policy outcomes.
% ABSENT_VOICES: Advocates of parliamentary sovereignty and popular constitutionalism are structurally excluded from the final interpretive process. They would argue for greater democratic control over constitutional meaning and challenge the judiciary's claim to unique expertise or independence.
% DISAPPEARANCE_RATIONALE: If judicial supremacy vanished, the legislative and executive branches would immediately assert their own interpretive authority, leading to a period of intense inter-branch conflict over constitutional meaning. The stability of law would be challenged, and new mechanisms for resolving constitutional disputes would emerge, fundamentally reorganizing the political system.
% FOUNDING_PROBLEM: To prevent arbitrary government, protect individual liberties, and ensure a consistent application of the supreme law of the land, especially in a system with separated powers and federalism.
% FOUNDING_PROBLEM_CORROBORATION: The judiciary and legal profession assert the problem is live, citing the need for checks on power and protection of rights. Political scientists, some legal scholars, and advocates of other constitutional theories argue the problem is either over-solved (leading to judicial overreach) or that the 'solution' itself creates new problems of democratic deficit. Public opinion is often divided, with support for judicial review fluctuating based on specific rulings.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because the judiciary's interpretive authority can significantly alter policy outcomes and limit democratic action, effectively extracting policy space from elected branches. Suppression (0.70) is high due to the institutional and legal barriers to challenging judicial decisions, making alternatives difficult to pursue. The theater ratio (0.10) is low, as the judiciary's interpretive function is largely genuine, though its claims of pure 'neutrality' may have performative elements. The historical measurements show a rise in extractiveness and suppression as judicial review became more entrenched and expansive, with a slight recent dip reflecting increased political contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's perspective, this is a necessary 'rope' for constitutional order and rights protection. From the legislature and electoral majorities, it can feel like a 'snare' that thwarts democratic will. The engine's computation of per-seat classification will reflect this divergence based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary and legal profession are beneficiaries (low d) as they gain institutional power and professional prestige. The legislature, electoral majorities, and executive branch are targets (high d) as their actions and will are constrained by judicial rulings. The 'identity_locked' exit for the judiciary reflects the deep professional and institutional commitment to its role as final arbiter, making it difficult to conceive of an alternative framework.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a 'tangled_rope' because it genuinely coordinates constitutional meaning (preventing chaos from multiple interpretations) but does so with significant asymmetric extraction from the legislative and popular seats. The classification prevents mislabeling it as a 'rope' (ignoring extraction) or a 'snare' (ignoring coordination). The 'contested' status of the founding problem highlights the ongoing debate about whether the original mandate (preventing tyranny, ensuring consistency) is still being met or if the mechanism has drifted into overreach.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_judicial_independence,
    'To what extent is judicial independence from political pressure a genuine structural feature versus a performative claim that masks political influence?',
    'Empirical studies on judicial decision-making, analysis of judicial appointments processes, and comparative legal studies across different political systems.',
    'If independence is largely performative, the ''judicial supremacy'' reading''s legitimacy is undermined, increasing its effective extractiveness and potentially reclassifying it closer to a ''snare'' due to a weaker coordination justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_judicial_independence, empirical, 'Assessing the true nature of judicial independence.').

omega_variable(
    democratic_legitimacy_deficit,
    'Does judicial supremacy create an unacceptable democratic legitimacy deficit by allowing unelected officials to overturn the will of elected representatives?',
    'Conceptual analysis of democratic theory, public opinion surveys on trust in institutions, and political science studies on policy responsiveness.',
    'If the deficit is deemed unacceptable, the ''judicial supremacy'' reading would be seen as more extractive and suppressive from the perspective of democratic principles, potentially shifting its classification towards a ''snare'' or a highly extractive ''tangled_rope''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(democratic_legitimacy_deficit, conceptual, 'The tension between judicial review and democratic principles.').

omega_variable(
    alternative_interpretive_mechanisms,
    'Are there viable alternative mechanisms for constitutional interpretation that could provide stability and rights protection without judicial supremacy?',
    'Comparative studies of constitutional systems with parliamentary sovereignty or strong popular constitutionalism, and theoretical proposals for new institutional designs.',
    'If viable alternatives exist, the ''suppression'' metric for this constraint would decrease, as the perceived lack of alternatives is a key component of its persistence. This could shift its classification towards a less entrenched ''tangled_rope'' or even a ''rope'' if the coordination function is genuinely separable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_interpretive_mechanisms, empirical, 'Feasibility of non-judicial constitutional interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t1900, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(basi_tr_t1930, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(basi_tr_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(basi_tr_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(basi_tr_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(basi_be_t1900, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement(basi_be_t1930, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1930, 0.55).
narrative_ontology:measurement(basi_be_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(basi_be_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(basi_be_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t1900, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(basi_su_t1930, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1930, 0.6).
narrative_ontology:measurement(basi_su_t1960, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1960, 0.65).
narrative_ontology:measurement(basi_su_t1990, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(basi_su_t2024, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'basic_law_interpretive_authority' kernel. Its ε value differs significantly from the 'parliamentary_sovereignty_reading' and 'popular_constitutionalism_reading' siblings due to differing allocations of interpretive power and associated costs/benefits.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
