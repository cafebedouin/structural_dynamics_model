% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Binding Backward Constraint
 *   domain: legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates the strict_stare_decisis reading of the
 *   common_law_precedent_corpus kernel: the doctrine that judicial precedent
 *   binds as a backward constraint and that departure from accumulated
 *   holdings requires extraordinary justification. It is distinguished from
 *   sibling readings (evolutionary_framework and pluralist_balancing) by high
 *   rigidity, narrow pathways for normative challenge, and the structural
 *   constraint of the judiciary by its own prior decisions. The claim is
 *   tangled_rope because the arrangement carries a genuine coordination
 *   function (legal predictability and stability) while also asymmetrically
 *   extracting decisional autonomy from subordinate judges and litigants to
 *   protect incumbent legal arrangements.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Primary agenda_setter (institutional/constrained) â administers the doctrine and determines what counts as extraordinary justification, though itself bound by accumulated holdings
 *   - subordinate_judges: Primary target (moderate/constrained) â bear the cost of constrained interpretive autonomy under hierarchical precedent
 *   - precedent_challengers: Primary target (powerless/trapped) â litigants seeking normative change who face extraordinary barriers to overturning precedent
 *   - incumbent_rights_holders: Primary beneficiary (powerful/mobile) â parties whose settled legal positions are protected against challenge
 *   - legal_scholars: Analytical observer (analytical/analytical) â external critics who analyze the doctrine without bearing its costs
 *   - marginalized_litigants: Excluded voice (powerless/trapped) â absent from the docket due to high barriers to mounting precedent challenges
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.72).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.68).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.72).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Binding Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudential").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, '7fb8dcbb-4728-46ca-a9eb-7e9327aa3184').
narrative_ontology:cs_kernel_codification('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', formalized).
narrative_ontology:cs_authority_grounding('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', lineage).
narrative_ontology:cs_interpretation_layer_present('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184').
narrative_ontology:cs_reading_relation('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', foundational, precedent_as_temporal_binding_norm).
narrative_ontology:cs_axiom_status(precedent_as_temporal_binding_norm, holdable).
narrative_ontology:cs_axiom_grounding('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', precedent_as_temporal_binding_norm, conventional).
narrative_ontology:cs_axiom('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', foundational, extraordinary_justification_requirement).
narrative_ontology:cs_axiom_status(extraordinary_justification_requirement, holdable).
narrative_ontology:cs_axiom_grounding('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', extraordinary_justification_requirement, conventional).
narrative_ontology:cs_reference_frame('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', binding_precedent_continuity).
narrative_ontology:cs_drift_state('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', contemporary_jurisprudence, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7fb8dcbb-4728-46ca-a9eb-7e9327aa3184', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, incumbent_rights_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, subordinate_judges).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, precedent_challengers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the doctrine of stare decisis, determining when precedent may be overruled and what constitutes extraordinary justification. Benefits from institutional continuity and hierarchical control, but is itself constrained by its own accumulated holdings and legitimacy considerations.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Are bound by superior court precedent; must distinguish or apply holdings even when they believe them wrong. Bear the cost of constrained interpretive autonomy and face reversal if they depart.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, subordinate_judges, payer,
    moderate, biographical, constrained, national).

% Litigants seeking to overturn existing precedent bear the burden of demonstrating extraordinary justification for departure. Their normative claims are filtered through accumulated holdings that narrow viable challenges.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, precedent_challengers, payer,
    powerless, immediate, trapped, national).

% Parties whose legal rights and expectations rest on existing precedent benefit from stability and resistance to change. The constraint protects their holdings against normative challenge.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, incumbent_rights_holders, beneficiary,
    powerful, biographical, mobile, national).

% Analyze and critique the doctrine's coherence, historical accuracy, and normative effects. Neither collect rents from the constraint nor bear its direct costs.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, national).

% Would challenge precedent if present but lack resources, standing, or institutional voice to mount claims requiring extraordinary justification. Their absence from the docket is produced by the high barrier to entry, not by satisfaction with the status quo.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, marginalized_litigants, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, incumbent_rights_holders).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable framework of legal rules so that actors can coordinate their conduct and plan with confidence that established norms will not shift arbitrarily with each new judicial panel.
% TRANSFER_FUNCTION: Transfers decisional authority from present litigants and subordinate courts to past judicial majorities and superior courts; transfers the cost of legal innovation onto challengers who must surmount an extraordinary justification barrier.
% ABSENT_VOICES: Future litigants and marginalized communities whose claims are foreclosed by existing precedent but who lack the resources to mount challenges requiring extraordinary justification; dissenting judges whose interpretive frameworks are suppressed by accumulated holdings.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight, subordinate judges would regain interpretive autonomy, precedent challengers would face lower barriers, established rights holders would face uncertainty, and the legal system would shift toward more fluid normative evolution.
% FOUNDING_PROBLEM: How to maintain legal stability and predictability amid changing social conditions and varying judicial panels, preventing arbitrary jurisprudential whipsaw.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative jurists attest that legal systems without strict stare decisis maintain stability through codification and hierarchical review; rule-of-law scholars attest that precedent constraint prevents arbitrary judicial power. The contested status is corroborated by ongoing academic debate from seats outside the benefiting parties, not merely self-asserted by the judiciary.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) is high because the constraint systematically transfers the burden of legal change onto challengers and forecloses normative pathways. Suppression (0.68) reflects the active enforcement of hierarchical precedent and the doctrinal barriers to departure. Theater ratio (0.25) captures the partial performativity of precedent rhetoric, which exceeds the actual rate of doctrinal change but remains subordinate to genuine coordination functions. Accessibility collapse (0.78) is high because once a litigant understands the doctrine, the pathway to overturning precedent appears nearly closed. Resistance (0.55) captures persistent academic and occasional judicial dissent without effective institutional reversal. The temporal series show accumulation: extraction and enforcement harden as the precedent corpus deepens over the interval.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary experiences the constraint as necessary coordination for legal stability and institutional legitimacy. Subordinate judges and precedent challengers experience it as an external bind that forecloses preferred outcomes. Incumbent rights holders experience it as protective stability. The engine computes this divergence from the structural data: agenda_setter and beneficiary seats face low directionality, while payer seats with trapped or constrained exit face high directionality.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent_rights_holders are declared beneficiaries with mobile exit (operating confidently within stable precedent), placing their directionality near the subsidy end. Subordinate_judges and precedent_challengers are declared victims with constrained or trapped exit, placing their directionality near the full-target end. The appellate_judiciary sits in an intermediate position: it administers the constraint (agenda_setter) but is also bound by it; its exit is constrained rather than fully open, so its derived directionality sits between beneficiary and symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was legal instability and arbitrary judicial variation. Its status is contested: some argue it remains live (rule-of-law values require stability), while others argue it is substantially solved by codification, constitutional text, and hierarchical review, such that strict stare decisis now functions primarily to protect entrenched errors and incumbent rights. This contested genealogy prevents mislabeling the constraint as pure rope (there are identifiable victims paying extraordinary costs) or pure snare (there is a genuine coordination function in legal predictability). The tangled_rope classification captures both the coordination good and the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_coordination_or_capture,
    'Does strict stare decisis solve a genuine coordination problem (legal predictability and stability) or does it primarily function to capture the judiciary and protect incumbent legal and political arrangements?',
    'Comparative institutional analysis of jurisdictions with weaker stare decisis norms, measuring legal stability, incumbent protection, and rates of rights expansion.',
    'If primarily capture, the extractiveness metric understates the constraint''s asymmetric extraction and the coordination function is smaller than claimed; if genuine coordination, the tangled_rope classification is properly balanced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_coordination_or_capture, conceptual, 'Ambiguity between coordination function and incumbent protection').

omega_variable(
    reading_independence_verdict,
    'Does the strict_stare_decisis reading instantiate a distinct constraint with its own epsilon, or is it merely a high-intensity parameter setting of a single precedent-constraint shared with its siblings?',
    'Structural decomposition test: if changing the reading changes epsilon, victim/beneficiary structure, and classification, the epsilon-invariance decomposition is warranted; if only intensity changes, the kernel should be modeled as one constraint with variable strictness.',
    'If the latter, the corpus should merge the sibling stories; if the former, the current decomposition stands and the network edges are validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_independence_verdict, conceptual, 'Whether strict stare decisis is a distinct constraint or parameter variant').

omega_variable(
    extraordinary_justification_asymmetry,
    'Is the ''extraordinary justification'' standard for departing precedent applied symmetrically across conservative and progressive precedents, or does it functionally vary by ideological alignment and political salience?',
    'Quantitative analysis of overruling rates and justification rhetoric across issue areas, court compositions, and precedent ideological direction.',
    'Asymmetric application would reveal the constraint as more extractive (protecting certain precedents more than others) and less coordinative, shifting the effective epsilon upward for targeted domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_asymmetry, empirical, 'Empirical asymmetry in the application of the extraordinary justification standard').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.1).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.15).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.18).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.2).
narrative_ontology:measurement(comm_tr_t80, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 80, 0.23).
narrative_ontology:measurement(comm_tr_t100, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 100, 0.25).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.67).
narrative_ontology:measurement(comm_be_t80, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 80, 0.7).
narrative_ontology:measurement(comm_be_t100, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 100, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(comm_su_t80, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 80, 0.67).
narrative_ontology:measurement(comm_su_t100, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 100, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_law_precedent_corpus kernel, decomposed per the epsilon-invariance principle because the strict_stare_decisis, evolutionary_framework, and pluralist_balancing readings produce structurally distinct epsilon values, beneficiary/victim structures, and classification profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
