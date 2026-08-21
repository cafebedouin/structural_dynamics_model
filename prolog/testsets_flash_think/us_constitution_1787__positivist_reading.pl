% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: US Constitution (Positivist Reading)
 *   domain: legal/political_philosophy
 *
 * SUMMARY:
 *   This constraint story describes the positivist reading of the US
 *   Constitution, which holds that constitutional meaning is derived from the
 *   explicit text and its democratic amendments. Judicial interpretation is
 *   strictly constrained to this textual basis, limiting judicial activism
 *   and emphasizing the amendment process as the primary legitimate mechanism
 *   for constitutional change. This reading is one of several competing
 *   interpretations of the US Constitution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.6).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "US Constitution (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, 'b44d5eaa-41f8-44b1-99d3-ba75e01a8be2').
narrative_ontology:cs_kernel_codification('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', fixed_text).
narrative_ontology:cs_authority_grounding('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', lineage).
narrative_ontology:cs_interpretation_layer_present('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2').
narrative_ontology:cs_reading_relation('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', foundational, constitutional_text_is_supreme).
narrative_ontology:cs_axiom_status(constitutional_text_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', constitutional_text_is_supreme, conventional).
narrative_ontology:cs_axiom('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', foundational, amendment_is_sole_legitimate_change_mechanism).
narrative_ontology:cs_axiom_status(amendment_is_sole_legitimate_change_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', amendment_is_sole_legitimate_change_mechanism, conventional).
narrative_ontology:cs_reference_frame('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', textual_supremacy_framework).
narrative_ontology:cs_drift_state('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', contemporary_legal_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b44d5eaa-41f8-44b1-99d3-ba75e01a8be2', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, democratic_majority).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, judicial_branch).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, minority_groups_seeking_judicial_remedy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the constraint on judicial power, as it reinforces the legislative role in lawmaking and constitutional change via amendment. Sets the agenda for statutory law and initiates amendments.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legislative_branch, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, legislative_branch, beneficiary).

% Bears the cost of constrained interpretation, limited to the text and democratic amendments, rather than broader historical or evolving societal norms. Still acts as an agenda-setter by interpreting the text, but within strict boundaries.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, judicial_branch, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, judicial_branch, agenda_setter).

% Benefits from the emphasis on democratic processes (amendments) as the primary legitimate means of constitutional change, reinforcing popular sovereignty.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, democratic_majority, beneficiary,
    organized, biographical, mobile, national).

% Bears the cost of limited judicial recourse if the constitutional text does not explicitly protect their interests or if amendments are difficult to achieve. Their ability to secure rights or protections is tightly bound by the written word.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, minority_groups_seeking_judicial_remedy, payer,
    powerless, generational, constrained, national).

% Analyze and debate the implications of this interpretive method, its consistency, and its effects on governance and rights. They do not directly benefit or pay but shape the intellectual discourse around the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, legal_scholars, observer,
    analytical, generational, analytical, global).

% Advocate for a different interpretive method (originalism) that would prioritize the framers' intent. While their views are part of the broader legal discourse, this positivist reading structurally excludes their core premise as a legitimate interpretive tool.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_scholars, excluded,
    moderate, generational, constrained, national).

% Advocate for a different interpretive method (living constitutionalism) that would allow the Constitution's meaning to evolve with society. This positivist reading structurally excludes their core premise as a legitimate interpretive tool.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalists, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, legislative_branch).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, text-based framework for constitutional governance, coordinating the powers of government branches and ensuring that fundamental changes occur through a clear, democratic amendment process.
% TRANSFER_FUNCTION: Transfers interpretive authority from the judiciary (beyond the text) to the legislative branch and the democratic majority (via amendments), limiting judicial discretion and reinforcing popular sovereignty.
% ABSENT_VOICES: Originalist scholars and living constitutionalists are present in the broader debate but are structurally excluded from the core interpretive methodology of this reading. They would argue for different sources of constitutional meaning (framers' intent or societal evolution, respectively).
% DISAPPEARANCE_RATIONALE: If this positivist reading vanished, the US constitutional system would fundamentally rearrange. Judicial interpretation would likely become either more originalist (seeking historical intent) or more 'living' (adapting to contemporary norms), leading to different legal outcomes, shifts in power dynamics between branches, and potentially a less stable or predictable legal landscape.
% FOUNDING_PROBLEM: The US Constitution was established to create a stable framework for government, define the limits of power, and provide a mechanism for legitimate change, avoiding arbitrary rule and ensuring popular consent.
% FOUNDING_PROBLEM_CORROBORATION: The ongoing need for stable governance, clear limits on power, and legitimate change mechanisms is widely attested by political scientists, legal historians, and public discourse, even by those who disagree with the positivist reading's specific solutions.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.60) is moderate because while it provides a stable framework, it imposes significant costs on those seeking broader or more flexible interpretations, particularly the judiciary and minority groups who might rely on judicial innovation. Suppression (0.70) is high as it actively suppresses alternative interpretive methodologies in favor of textualism and the amendment process. Theater ratio (0.20) is low; while legal formalism can have performative aspects, the core function of textual interpretation is genuine. Accessibility collapse (0.60) is moderate, as it collapses alternatives to textual and amendment-based change, but other interpretive avenues still exist in academic and political discourse. Resistance (0.50) is moderate, as this reading is constantly challenged by proponents of originalism and living constitutionalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the legislative branch and the democratic majority, this reading functions as a legitimate rope, coordinating governance and ensuring popular sovereignty. From the perspective of the judicial branch and minority groups, it can feel more like a tangled rope or snare, imposing significant constraints and limiting avenues for justice or adaptation. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The legislative branch and the democratic majority are beneficiaries, gaining power and legitimacy from the emphasis on textualism and the amendment process. The judicial branch is a payer, as its interpretive power is constrained. Minority groups seeking judicial remedies are also payers, as their ability to secure rights may be limited by the explicit text. Legal scholars are observers, analyzing the effects of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_meaning_ambiguity,
    'What constitutes ''what the text says''? Is it plain meaning, original public meaning, or a historically informed plain meaning?',
    'Judicial consensus on interpretive canons, or a constitutional amendment explicitly defining interpretive methodology.',
    'If ''plain meaning'' is strictly applied, it could lead to different outcomes than ''original public meaning'', potentially shifting the burden of change to the amendment process or altering the scope of judicial constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_meaning_ambiguity, conceptual, 'Ambiguity in the definition of ''textual meaning'' within the positivist framework.').

omega_variable(
    amendment_process_efficacy,
    'Is the amendment process truly an accessible and effective democratic mechanism for constitutional change, or is it practically prohibitive?',
    'Empirical analysis of amendment success rates, political science studies on supermajority requirements, and comparative constitutional law.',
    'If the amendment process is practically prohibitive, the constraint''s democratic legitimacy is weakened, and the extraction from those seeking change without judicial recourse becomes more severe, potentially pushing the classification closer to a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_efficacy, empirical, 'The practical efficacy and democratic accessibility of the constitutional amendment process.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint a genuine, self-contained interpretive framework, or is its structural integrity dependent on its contestation with originalist and living constitutionalist readings?',
    'Conceptual analysis of the internal coherence of the positivist reading in isolation, and its historical development in response to other theories.',
    'If its integrity is highly dependent on contestation, its claimed stability and democratic grounding are partly performative, and its classification might shift towards a higher theater_ratio or even piton if its core function atrophies without the external pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'The fundamental ambiguity of being one reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1900, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1900, us_constitution_1787__positivist_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement(us_c_tr_t1925, us_constitution_1787__positivist_reading, theater_ratio, 1925, 0.19).
narrative_ontology:measurement(us_c_tr_t1950, us_constitution_1787__positivist_reading, theater_ratio, 1950, 0.2).
narrative_ontology:measurement(us_c_tr_t1975, us_constitution_1787__positivist_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_1787__positivist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement(us_c_tr_t2023, us_constitution_1787__positivist_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1900, us_constitution_1787__positivist_reading, base_extractiveness, 1900, 0.55).
narrative_ontology:measurement(us_c_be_t1925, us_constitution_1787__positivist_reading, base_extractiveness, 1925, 0.57).
narrative_ontology:measurement(us_c_be_t1950, us_constitution_1787__positivist_reading, base_extractiveness, 1950, 0.58).
narrative_ontology:measurement(us_c_be_t1975, us_constitution_1787__positivist_reading, base_extractiveness, 1975, 0.59).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_1787__positivist_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(us_c_be_t2023, us_constitution_1787__positivist_reading, base_extractiveness, 2023, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1900, us_constitution_1787__positivist_reading, suppression_requirement, 1900, 0.65).
narrative_ontology:measurement(us_c_su_t1925, us_constitution_1787__positivist_reading, suppression_requirement, 1925, 0.67).
narrative_ontology:measurement(us_c_su_t1950, us_constitution_1787__positivist_reading, suppression_requirement, 1950, 0.68).
narrative_ontology:measurement(us_c_su_t1975, us_constitution_1787__positivist_reading, suppression_requirement, 1975, 0.69).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_1787__positivist_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(us_c_su_t2023, us_constitution_1787__positivist_reading, suppression_requirement, 2023, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the US Constitution (us_constitution_1787 kernel). Each reading represents a different structural constraint with unique beneficiaries, victims, and metric profiles, linked by their common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
