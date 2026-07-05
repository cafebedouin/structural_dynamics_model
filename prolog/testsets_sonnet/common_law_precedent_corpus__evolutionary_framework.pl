% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__evolutionary_framework
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: common_law_precedent_corpus__evolutionary_framework
 *   human_readable: Common Law Precedent as Evolutionary Framework (Reinterpretation-Permissive Reading)
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the evolutionary_framework reading of the
 *   common_law_precedent_corpus kernel: precedent is treated as an adaptive
 *   scaffold that can and should yield to contemporary normative development,
 *   with overruling normalized as a corrective mechanism rather than an
 *   extraordinary event. This is deliberately NOT a story about
 *   precedent-as-binding-rule (strict_stare_decisis) or
 *   precedent-as-context-weighted-balance (pluralist_balancing) — those are
 *   separate constraints with different beneficiary/victim structures and
 *   different epsilon values, linked here only through
 *   network.affects_constraints and cs_structure.reading_relations. Under
 *   this reading, litigants challenging entrenched doctrine and groups
 *   disadvantaged by historically-decided rules gain a real pathway to legal
 *   change through the judiciary rather than the legislature; those who
 *   relied on the old rule's stability bear the reversal cost without formal
 *   doctrinal protection for their reliance interest.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__evolutionary_framework, 0.38).
domain_priors:suppression_score(common_law_precedent_corpus__evolutionary_framework, 0.24).
domain_priors:theater_ratio(common_law_precedent_corpus__evolutionary_framework, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, extractiveness, 0.38).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_law_precedent_corpus__evolutionary_framework, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__evolutionary_framework, rope).
narrative_ontology:human_readable(common_law_precedent_corpus__evolutionary_framework, "Common Law Precedent as Evolutionary Framework (Reinterpretation-Permissive Reading)").
narrative_ontology:topic_domain(common_law_precedent_corpus__evolutionary_framework, "legal_theory/jurisprudence/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__evolutionary_framework, 'b0438bd7-1f62-4af3-9174-7167399797b9').
narrative_ontology:cs_kernel_codification('b0438bd7-1f62-4af3-9174-7167399797b9', distributed).
narrative_ontology:cs_authority_grounding('b0438bd7-1f62-4af3-9174-7167399797b9', practice).
narrative_ontology:cs_interpretation_layer_present('b0438bd7-1f62-4af3-9174-7167399797b9').
narrative_ontology:cs_reading_relation('b0438bd7-1f62-4af3-9174-7167399797b9', common_law_precedent_corpus__strict_stare_decisis, coexists_with).
narrative_ontology:cs_reading_relation('b0438bd7-1f62-4af3-9174-7167399797b9', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('b0438bd7-1f62-4af3-9174-7167399797b9', foundational, precedent_revisability_as_legitimate_correction).
narrative_ontology:cs_axiom_status(precedent_revisability_as_legitimate_correction, holdable).
narrative_ontology:cs_axiom_grounding('b0438bd7-1f62-4af3-9174-7167399797b9', precedent_revisability_as_legitimate_correction, conventional).
narrative_ontology:cs_axiom('b0438bd7-1f62-4af3-9174-7167399797b9', foundational, contemporary_normative_consensus_supersedes_historical_settlement).
narrative_ontology:cs_axiom_status(contemporary_normative_consensus_supersedes_historical_settlement, holdable).
narrative_ontology:cs_axiom_grounding('b0438bd7-1f62-4af3-9174-7167399797b9', contemporary_normative_consensus_supersedes_historical_settlement, instrumental).
narrative_ontology:cs_reference_frame('b0438bd7-1f62-4af3-9174-7167399797b9', common_law_stability_through_stare_decisis).
narrative_ontology:cs_drift_state('b0438bd7-1f62-4af3-9174-7167399797b9', contemporary_rights_jurisprudence_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('b0438bd7-1f62-4af3-9174-7167399797b9', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_norm_reversal).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_under_outdated_precedent).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__evolutionary_framework, legal_reform_advocates).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, settled_expectation_holders).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, long_term_contractual_parties).
narrative_ontology:constraint_victim(common_law_precedent_corpus__evolutionary_framework, lower_court_judges_facing_instability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads precedent as an adaptive framework rather than a binding chain, treating changed social conditions, evolving normative consensus, and doctrinal incoherence as sufficient grounds to overrule. Sets the interpretive posture that determines how much weight prior decisions carry against present-day reasoning. Gains institutional authority as the recognized site of normative updating rather than mere rule-application.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, appellate_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Bring cases explicitly arguing that a controlling precedent is outdated, unjust, or inconsistent with contemporary values. Under the evolutionary reading, they have a genuine pathway to reversal that would be foreclosed or heavily disfavored under strict stare decisis. Their exit from an unfavorable precedent depends entirely on this framework remaining dominant in the courts hearing their case.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, litigants_seeking_norm_reversal, beneficiary,
    moderate, biographical, constrained, national).

% Live under precedents originally decided against their interests (discriminatory doctrines, outdated status categories). The evolutionary framework is their principal legal avenue for change, since they typically lack the political power to secure legislative reversal. They cannot exit the legal system itself, so the interpretive posture of the judiciary materially determines whether change is possible at all.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, marginalized_groups_under_outdated_precedent, beneficiary,
    powerless, generational, trapped, national).

% Litigation organizations and academic movements that strategically bring test cases designed to invite reconsideration of precedent. They benefit directly from a judiciary willing to treat precedent as revisable, and they actively shape which cases reach courts in a posture favorable to reversal.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_reform_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__evolutionary_framework, legal_reform_advocates, agenda_setter).

% Individuals and entities who structured contracts, property arrangements, business models, or life decisions around the existing precedent's stability. When precedent is overruled under the evolutionary framework, their reliance interests are not protected by the doctrine itself — they absorb the cost of legal change they had no notice would come, since the very premise of the reading is that stability is not owed.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, settled_expectation_holders, payer,
    moderate, biographical, trapped, national).

% Commercial actors who negotiated long-duration agreements under a known interpretation of governing precedent. An evolutionary reading raises the probability that governing rules shift mid-contract, and while they have resources to litigate or renegotiate, they cannot fully price or hedge against a doctrine that explicitly treats prior interpretation as provisional.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, long_term_contractual_parties, payer,
    powerful, biographical, constrained, national).

% Trial and intermediate appellate judges must apply precedent while anticipating that higher courts may reinterpret it under evolving norms. They bear the operational cost of doctrinal instability — inconsistent rulings, reversal risk, and difficulty giving litigants predictable guidance — without controlling the interpretive posture set above them.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, lower_court_judges_facing_instability, payer,
    institutional, biographical, constrained, national).

% Practitioners and scholars committed to precedent as a binding constraint would object that the evolutionary framework licenses judges to substitute contemporary policy preference for settled law under the guise of interpretation. Their objection is a matter of persistent public record in dissents and legal scholarship but does not control which reading a given court adopts in a given case.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, strict_constructionist_bar, excluded,
    organized, generational, constrained, national).

% Study how doctrines of precedent themselves have shifted across eras, documenting when courts have invoked evolutionary reasoning to justify reversal and tracing the downstream effects on legal stability and social change alike.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__evolutionary_framework, legal_historians, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__evolutionary_framework, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__evolutionary_framework, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows the legal system to correct precedents that have become factually obsolete, morally indefensible, or doctrinally incoherent without requiring legislative action, keeping law responsive to normative change over time.
% TRANSFER_FUNCTION: Moves normative authority from the accumulated weight of prior decisions and the reliance interests built on them toward the present judiciary's assessment of contemporary values; correspondingly shifts risk from beneficiaries of the old rule to those who had structured expectations around it.
% ABSENT_VOICES: The strict_constructionist_bar objects that this reading destabilizes the rule-of-law function of precedent, but their objection is registered in dissents and scholarship rather than controlling doctrine in courts that have adopted the evolutionary posture. Settled_expectation_holders rarely get a formal hearing on the reliance costs of reversal — courts weigh doctrinal coherence and contemporary values, not the sunk costs of the losing side's past planning.
% DISAPPEARANCE_RATIONALE: If courts abandoned the evolutionary reading entirely and reverted strictly to stare decisis, litigants challenging outdated precedent would lose their primary pathway to judicial reversal, forcing them toward slower and more difficult legislative routes; conversely, settled_expectation_holders and long_term_contractual_parties would gain materially greater predictability. The mix of pending and future litigation strategy would reorganize substantially around whichever framework controls.
% FOUNDING_PROBLEM: Rigid adherence to precedent can entrench factually superseded, morally repudiated, or doctrinally incoherent rules; the evolutionary framework was built to give courts a legitimate mechanism to correct such rules without waiting for legislatures.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and comparative scholars outside the litigation-advocacy community corroborate that landmark reversals (e.g., overruling doctrines once used to justify segregation or coverture) addressed genuine and live founding problems. However, the same historians also document instances where evolutionary reasoning was invoked instrumentally to reach a preferred outcome absent a genuinely obsolete rule, which is why status is marked contested rather than simply live.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__evolutionary_framework, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__evolutionary_framework, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__evolutionary_framework, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__evolutionary_framework, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__evolutionary_framework, 0.38, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__evolutionary_framework_tests).
:- end_tests(common_law_precedent_corpus__evolutionary_framework_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38) and rises slowly over the interval: the evolutionary framework is not primarily an extraction mechanism, but a live judiciary willing to overrule creates a persistent low-grade transfer of settled expectations toward whichever normative reassessment prevails in a given era. Suppression is low (0.24) because the framework does not foreclose alternative readings by coercion — strict_constructionist advocates remain free to argue their position in briefs, dissents, and scholarship; the framework wins by persuading, not by silencing. Theater ratio stays low (0.22, rising slightly) because the coordination function (correcting genuinely obsolete rules) remains substantially real throughout, though some drift toward invoking 'evolving values' language for outcome-driven reversals is documented by legal historians. Accessibility_collapse is moderate (0.35): alternative doctrinal postures (strict stare decisis, pluralist balancing) remain fully available and are actively practiced by other courts and eras, so this reading has not foreclosed its siblings as live options in the broader legal culture. Resistance is comparatively high (0.55) reflecting the active, organized, persistent objection from the strict-constructionist bar.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, the framework is coordination: keeping law responsive to a changing society is the whole point of having judicial interpretation rather than static codification. From the settled_expectation_holders' seat, the identical structure functions as extraction: their planning was invalidated by a standard that explicitly denies them a right to relied-upon stability. The engine's per-seat computation should reflect this asymmetry without either seat's framing overriding the other.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary is the agenda-setting seat: it decides, case by case, how much interpretive latitude to exercise, and its institutional standing is enhanced by being recognized as a normative updater rather than a mechanical rule-applier. Litigants seeking reversal and marginalized groups under outdated precedent are structural beneficiaries — the framework is their primary or only realistic route to change, so they sit near the target-of-benefit end. Settled_expectation_holders, long_term_contractual_parties, and lower_court_judges are payers: they bear the cost of doctrinal instability without having chosen it and without a formal doctrine of protected reliance to shield them, so their effective extraction rises with the frequency and scope of overruling.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (correcting entrenched, obsolete, or unjust precedent) remains genuinely live in some domains (civil rights doctrine, criminal procedure) and is contested in others (commercial and property law, where reliance costs are higher and normative consensus less obviously shifted). Marking founding_problem_status as contested rather than dead or live prevents mislabeling the entire framework as either pure ongoing coordination or pure obsolete extraction — it is doing genuine corrective work in some areas while functioning as outcome-oriented instrumentalism in others, and the six_questions corroboration deliberately names both patterns rather than collapsing them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three sibling readings (evolutionary_framework, strict_stare_decisis, pluralist_balancing) actually controls in a given jurisdiction or era, and how would we know if courts silently shifted between readings without acknowledging it?',
    'Systematic coding of appellate opinions'' explicit and implicit treatment of precedent-weight language across decades, cross-referenced with reversal rates and citation of prior-era authority as binding versus merely persuasive.',
    'If courts oscillate between readings opportunistically depending on desired outcome rather than committing to one interpretive posture, the evolutionary framework''s legitimacy as a principled doctrine (rather than a post-hoc justification vehicle) is undermined, and its structural resemblance to strict_stare_decisis-with-exceptions increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the three kernel readings are stable interpretive commitments or fluid, outcome-driven labels.').

omega_variable(
    reliance_interest_protection_gap,
    'Should the evolutionary framework incorporate a formal reliance-interest doctrine (e.g., prospective-only overruling) to protect settled_expectation_holders, and does the absence of such a doctrine make the reading structurally extractive rather than merely adaptive?',
    'Comparative analysis of jurisdictions that have adopted prospective overruling versus pure retroactive reversal, measuring litigation volume and reliance-cost complaints in each.',
    'If the absence of reliance protection is itself a policy choice rather than a doctrinal necessity, the extraction borne by settled_expectation_holders is closer to a deliberate cost-allocation decision than an unavoidable feature of adaptive precedent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliance_interest_protection_gap, preference, 'Whether uncompensated reliance loss is intrinsic to the evolutionary reading or a separable policy add-on.').

omega_variable(
    instrumentalism_vs_genuine_correction,
    'In any given overruling decision, is the invocation of ''contemporary normative evolution'' a genuine response to changed social facts and values, or an instrumental justification for a court reaching a result it independently preferred?',
    'Case-by-case examination of whether the cited normative shift predates or postdates the litigation strategy that produced the test case, and whether comparable normative claims were rejected in contemporaneous cases with different outcomes.',
    'If instrumentalism dominates, the framework''s coordination function is substantially theatrical and its extraction from settled_expectation_holders is better characterized as outcome-driven judicial power rather than principled doctrinal adaptation — pushing the classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(instrumentalism_vs_genuine_correction, empirical, 'Whether normative-evolution invocations are genuinely responsive or outcome-justifying.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__evolutionary_framework, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 10, 0.16).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 20, 0.18).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 30, 0.19).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 40, 0.2).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 50, 0.21).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__evolutionary_framework, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 20, 0.33).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 40, 0.36).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 50, 0.37).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__evolutionary_framework, base_extractiveness, 60, 0.38).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(common_law_precedent_corpus__evolutionary_framework, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__evolutionary_framework, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__strict_stare_decisis).
narrative_ontology:affects_constraint(common_law_precedent_corpus__evolutionary_framework, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the common_law_precedent_corpus kernel (evolutionary_framework, strict_stare_decisis, pluralist_balancing). Each reading is authored as its own constraint with its own epsilon, beneficiary/victim structure, and classification, per the epsilon-invariance principle. Strict_stare_decisis would show lower extractiveness toward settled_expectation_holders and higher accessibility_collapse (precedent nearly foreclosed as a target of challenge); pluralist_balancing would show intermediate values with victim/beneficiary sets varying by legal domain rather than uniformly. The three are linked here rather than merged because measuring 'the doctrine of precedent' along a single epsilon would conflate structurally distinct claims about how binding precedent actually is.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
