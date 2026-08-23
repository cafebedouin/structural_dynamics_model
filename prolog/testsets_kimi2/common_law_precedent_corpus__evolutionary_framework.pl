% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__evolutionary_framework, []).

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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Adaptive Framework
 *   domain: legal/jurisprudence/constitutional
 *
 * SUMMARY:
 *   This constraint instantiates the evolutionary_framework reading of the
 *   common_law_precedent_corpus kernel. Under this reading, precedent
 *   functions not as a rigid backward-looking binding force but as an
 *   adaptive framework that permits contemporary normative evolution through
 *   reinterpretation, distinguishing, and overruling. The constraint treats
 *   judicial authority to update doctrine as inherent to the common law
 *   method. This reading stands in contrast to strict_stare_decisis (which it
 *   forecloses as a comprehensive framework) and pluralist_balancing (with
 *   which it coexists as a domain-specific competitor). The kernel decomposes
 *   into multiple constraints because each reading assigns a different
 *   epsilon to the same precedent corpus: the evolutionary reading sees
 *   moderate extraction through concentrated judicial authority and
 *   reliance-cost externalization, while the strict reading sees higher
 *   extraction through rigid bindingness.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: Agenda-setter (institutional/constrained) â empowered as normative updater, captures authority gain
 *   - litigants_normative_challenge: Beneficiary (moderate/constrained) â gains pathways to challenge precedent
 *   - reliance_interest_holders: Target/payer (moderate/constrained) â bears costs of overturned precedent
 *   - lower_courts: Target/payer (institutional/constrained) â bears administrative costs of tracking evolution
 *   - formalist_jurists: Excluded voice (organized/constrained) â structurally marginalized methodological dissenters
 *   - comparative_legal_scholars: Analytical observer (analytical/analytical) â evaluates across jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.58).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.5).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Adaptive Framework").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal/jurisprudence/constitutional").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__evolutionary_framework).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'b138eb09-ea94-4daf-82ad-248cb9b3da6c').
narrative_ontology:cs_kernel_codification('b138eb09-ea94-4daf-82ad-248cb9b3da6c', distributed).
narrative_ontology:cs_authority_grounding('b138eb09-ea94-4daf-82ad-248cb9b3da6c', lineage).
narrative_ontology:cs_interpretation_layer_present('b138eb09-ea94-4daf-82ad-248cb9b3da6c').
narrative_ontology:cs_reading_relation('b138eb09-ea94-4daf-82ad-248cb9b3da6c', common_law_precedent_corpus__strict_stare_decisis, forecloses).
narrative_ontology:cs_reading_relation('b138eb09-ea94-4daf-82ad-248cb9b3da6c', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('b138eb09-ea94-4daf-82ad-248cb9b3da6c', foundational, precedent_adaptive_legitimacy).
narrative_ontology:cs_axiom_status(precedent_adaptive_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b138eb09-ea94-4daf-82ad-248cb9b3da6c', precedent_adaptive_legitimacy, conventional).
narrative_ontology:cs_axiom('b138eb09-ea94-4daf-82ad-248cb9b3da6c', foundational, judicial_normative_updating_authority).
narrative_ontology:cs_axiom_status(judicial_normative_updating_authority, holdable).
narrative_ontology:cs_axiom_grounding('b138eb09-ea94-4daf-82ad-248cb9b3da6c', judicial_normative_updating_authority, conventional).
narrative_ontology:cs_reference_frame('b138eb09-ea94-4daf-82ad-248cb9b3da6c', adaptive_precedent_framework).
narrative_ontology:cs_drift_state('b138eb09-ea94-4daf-82ad-248cb9b3da6c', contemporary_originalist_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('b138eb09-ea94-4daf-82ad-248cb9b3da6c', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_normative_challenge).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_courts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises interpretive authority to update legal norms through precedent, selecting which precedents to follow, distinguish, or overrule based on perceived contemporary normative requirements. Frames overruling as corrective evolution rather than departure from settled law.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Bring claims seeking to overturn existing precedent on normative grounds, gaining access to judicial pathways that treat precedent as revisable rather than fixed. Benefit from the doctrinal openness that permits challenges to longstanding rules.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_normative_challenge, beneficiary,
    moderate, biographical, constrained, national).

% Parties, investors, and institutions who structured conduct around existing precedent and bear adjustment costs when appellate courts overrule or reinterpret established doctrine. Cannot opt out of the legal system's retroactive normative shifts.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, reliance_interest_holders, payer,
    moderate, biographical, constrained, national).

% Bound to follow appellate precedent but must track evolving interpretations, distinguishments, and implicit overrulings. Bear the administrative and legitimacy costs of applying shifting doctrine while maintaining the fiction of continuity.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_courts, payer,
    institutional, generational, constrained, national).

% Legal theorists and jurists who argue for strict adherence to precedent as a constraint on judicial discretion. Their methodological objections are structurally marginalized in courts adopting the evolutionary framework, which treats their position as primitive or rigid rather than as a live interpretive option.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, formalist_jurists, excluded,
    organized, generational, constrained, global).

% Study precedent systems across jurisdictions, evaluating how different interpretive frameworks affect legal stability and adaptation. Neither bound by nor directly benefiting from any single jurisdiction's precedent methodology.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for legal norms to adapt to evolving social values and contemporary conditions without requiring constant legislative revision, maintaining systemic coherence while permitting doctrinal correction.
% TRANSFER_FUNCTION: Transfers normative updating authority from the accumulated weight of past decisions to the contemporary judiciary, and transfers the costs of legal instability from the updating court to affected parties and lower courts.
% ABSENT_VOICES: Strict formalists and originalist jurists who would argue that the evolutionary framework licenses unchecked judicial discretion; legislative bodies whose normative prerogatives are encroached upon by judicial updating; and losing litigants from overturned precedents who bear reliance costs without being heard in the reinterpretation moment.
% DISAPPEARANCE_RATIONALE: If the evolutionary framework vanished and precedent became strictly binding, courts would lose the primary mechanism for doctrinal correction, litigants would lose pathways to challenge outdated norms, and the legal landscape would ossify around past errors; conversely, if precedent binding dissolved entirely, coordination through stare decisis would collapse. The arrangement is load-bearing.
% FOUNDING_PROBLEM: How to maintain a stable legal system that can nevertheless correct errors and adapt to fundamentally changed social conditions without relying solely on legislative action, which may be slow, deadlocked, or non-responsive to minority grievances.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative constitutional scholars outside the immediate beneficiary judiciary attest that common law systems faced adaptation crises before the evolutionary framework matured; legislative bodies and public law scholars corroborate that statutory revision alone cannot address the volume of doctrinal adaptation required.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-high because the evolutionary framework concentrates normative updating authority in appellate judiciaries and systematically externalizes reliance costs to past-dependent parties. Suppression (0.50) reflects the active judicial and professional effort required to maintain the evolutionary methodology against formalist and originalist alternatives. Theater ratio (0.45) captures the performative dimension: courts routinely present doctrinal innovation as faithful continuity with past precedent, masking the normative update as discovery. Accessibility collapse (0.48) is moderate because alternatives (strict textualism, legislative override, constitutional amendment) remain structurally available but are professionally and politically costly to activate. Resistance (0.55) reflects sustained methodological opposition from originalist and formalist quarters.
 *
 * PERSPECTIVAL GAP:
 *   The appellate judiciary experiences this constraint as professional empowerment and interpretive flexibility â a coordination mechanism that preserves institutional legitimacy while permitting necessary correction. Reliance-interest holders and lower courts experience the same structure as instability and unfunded mandates: the former suffer retroactive disruption of settled expectations, while the latter must administer shifting doctrine without corresponding authority to resist it. The engine computes this divergence from identical structural data through power and exit modulation.
 *
 * DIRECTIONALITY LOGIC:
 *   Appellate judiciary and norm-challenging litigants are declared beneficiaries, producing low directionality (subsidized by the constraint's authority structure). Reliance holders and lower courts are declared victims/payers, producing high directionality (extracted from). The formalist jurists are excluded â their exclusion is the suppression mechanism that keeps the evolutionary framework stable. Comparative scholars observe from outside the directionality derivation entirely (analytical exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â legal adaptation without legislative bottleneck â remains live (R5 status: live). This prevents piton misclassification: the constraint is not an atrophied relic but an actively functioning coordination mechanism. However, the presence of identifiable beneficiaries (judiciary, progressive litigants) alongside identifiable payers (reliance holders, lower courts) prevents rope misclassification: the coordination function is real but asymmetrically distributed, yielding tangled rope rather than pure coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evolutionary_vs_lawmaking,
    'Does the evolutionary framework disguise judicial law-making as normative discovery, or is reinterpretation genuinely constrained by the precedent corpus?',
    'Comparative analysis of overruling rates and reasoning in evolutionary-framework courts versus courts with stricter interpretive methodologies, measuring the predictability of outcomes from the precedent corpus alone.',
    'If reinterpretation is unconstrained law-making, extraction is higher than measured and the constraint is closer to snare; if genuinely constrained by precedent, it is more rope-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evolutionary_vs_lawmaking, conceptual, 'Whether evolutionary reinterpretation is discovery or creation').

omega_variable(
    reliance_cost_quantification,
    'How quantifiable are the costs to reliance-interest holders when precedent is overruled under the evolutionary framework?',
    'Empirical studies of behavioral adjustment costs, contract renegotiation, and investment disruption following major overrulings.',
    'Would calibrate the victim-side extraction more precisely and test whether reliance costs are diffuse or concentrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reliance_cost_quantification, empirical, 'Measuring reliance costs of precedent overruling').

omega_variable(
    kernel_reading_boundary,
    'Is the evolutionary framework reading structurally distinct from the pluralist balancing reading, or do they collapse into a single constraint at fine grain?',
    'Examine whether domain-specific balancing is a distinct mechanism or merely an application of general evolutionary adaptation across different legal fields.',
    'If collapse, one sibling constraint should merge into the other; if distinct, the constraint family link is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether evolutionary and pluralist readings are distinct constraints').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.28).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.32).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.38).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.42).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.45).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 40, 0.52).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 50, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, pluralist_balancing).

% DUAL FORMULATION NOTE:
% The common_law_precedent_corpus kernel decomposes into three structurally distinct constraints because the colloquial label 'precedent' conflates incompatible claims about rigidity, adaptability, and domain-specificity. Each reading assigns a different epsilon to the same corpus: strict_stare_decisis treats precedent as high-extraction binding, evolutionary_framework treats it as moderate-extraction adaptive coordination, and pluralist_balancing treats domain variance as primary. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
