% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__overdetermined_composite_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__overdetermined_composite_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: dueling_disappearance_mechanism__overdetermined_composite_reading
 *   human_readable: Overdetermined Composite Account of Dueling's Decline
 *   domain: historical sociology / legal history / cultural anthropology
 *
 * SUMMARY:
 *   From roughly the 1960s onward, historiography of the duel converged on
 *   attributing its disappearance to four conditions treated as jointly
 *   sufficient: statutory prohibition, institutional modernization (courts,
 *   banking, libel law, and insurance displacing honor arbitration), the
 *   cultural shift from honor to dignity norms, and the Civil War's
 *   destruction of the Southern honor economy. The composite functions as
 *   disciplinary infrastructure: it closed a bitter specialist standoff, made
 *   general synthesis writable, and gave every camp a permanent place in the
 *   story. Its costs are quieter: comparative causal-weight questions go
 *   unasked, adjudication-framed work is reviewed as reductionism, and the
 *   account's joint-sufficiency premise is essentially never tested. KEY
 *   AGENTS (by structural relationship): survey textbook authors administer
 *   the canon and distribute its rents; four specialist camps
 *   (legal-prohibition, institutional-substitution, Southern-honor,
 *   honor-culture theory) are simultaneously sheltered and confined; doctoral
 *   students inherit the frame as an untestable ceiling;
 *   causal-methodologists stand excluded outside the reproduction loop. The
 *   claim/metrics gap is deliberate: claimed_type states what this reading
 *   takes the structure to be (coordination with extraction through the same
 *   arrangement, actively enforced); the metrics describe its observed
 *   operation independently.
 *
 * KEY AGENTS:
 *   - survey_textbook_authors: agenda-setter and principal canon-holder (institutional/arbitrage) — controls which causal claims reach print
 *   - legal_prohibition_specialists: coordinated-and-partially-confined camp (organized/constrained)
 *   - institutional_substitution_researchers: dual-positioned camp, nominally covered, denied adjudication (powerful/arbitrage)
 *   - southern_honor_historians: dual-positioned regional camp with identity-fused exit (organized/identity_locked)
 *   - honor_culture_theorists: dual-positioned theoretical camp (moderate/identity_locked)
 *   - doctoral_students_in_historical_sociology: clearest target seat (powerless/trapped) — inherits an untestable frame at maximum career exposure
 *   - comparative_causality_methodologists: excluded voice — would impose adjudication standards (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58).
domain_priors:suppression_score(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.5).
domain_priors:theater_ratio(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__overdetermined_composite_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__overdetermined_composite_reading, tangled_rope).
narrative_ontology:human_readable(dueling_disappearance_mechanism__overdetermined_composite_reading, "Overdetermined Composite Account of Dueling's Decline").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__overdetermined_composite_reading, "historical sociology / legal history / cultural anthropology").

domain_priors:requires_active_enforcement(dueling_disappearance_mechanism__overdetermined_composite_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__overdetermined_composite_reading, '5c1f8ed9-73ab-4160-a099-5db110e5f761').
narrative_ontology:cs_kernel_codification('5c1f8ed9-73ab-4160-a099-5db110e5f761', distributed).
narrative_ontology:cs_authority_grounding('5c1f8ed9-73ab-4160-a099-5db110e5f761', expertise).
narrative_ontology:cs_interpretation_layer_present('5c1f8ed9-73ab-4160-a099-5db110e5f761').
narrative_ontology:cs_reading_relation('5c1f8ed9-73ab-4160-a099-5db110e5f761', dueling_disappearance_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c1f8ed9-73ab-4160-a099-5db110e5f761', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_axiom('5c1f8ed9-73ab-4160-a099-5db110e5f761', foundational, joint_independent_sufficiency_of_decline_conditions).
narrative_ontology:cs_axiom_status(joint_independent_sufficiency_of_decline_conditions, holdable).
narrative_ontology:cs_axiom_grounding('5c1f8ed9-73ab-4160-a099-5db110e5f761', joint_independent_sufficiency_of_decline_conditions, empirically_contingent).
narrative_ontology:cs_axiom('5c1f8ed9-73ab-4160-a099-5db110e5f761', secondary, causal_weight_adjudication_unnecessary_for_settlement).
narrative_ontology:cs_axiom_status(causal_weight_adjudication_unnecessary_for_settlement, holdable).
narrative_ontology:cs_axiom_grounding('5c1f8ed9-73ab-4160-a099-5db110e5f761', causal_weight_adjudication_unnecessary_for_settlement, conventional).
narrative_ontology:cs_reference_frame('5c1f8ed9-73ab-4160-a099-5db110e5f761', plural_joint_sufficiency_baseline).
narrative_ontology:cs_drift_state('5c1f8ed9-73ab-4160-a099-5db110e5f761', post_cliometric_methods_era, gap(axiom_overriding, minor, false)).
narrative_ontology:cs_created_at('5c1f8ed9-73ab-4160-a099-5db110e5f761', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__overdetermined_composite_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, survey_textbook_authors).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_specialists).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_substitution_researchers).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_historians).
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_theorists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_substitution_researchers).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_historians).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_theorists).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, doctoral_students_in_historical_sociology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_specialists).
narrative_ontology:constraint_vindicates(dueling_disappearance_mechanism__overdetermined_composite_reading, causal_pluralism_in_historical_explanation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and periodically revise the standard survey textbooks, encyclopedia entries, and course frameworks in which the four-cause composite appears as settled background. They choose which adjudication claims receive airtime in print, and their products define what each incoming cohort learns. They collect distribution rents (adoptions, royalties, citation centrality) and lose almost nothing if any single mechanism later proves decisive, because the chapter already lists every candidate.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, survey_textbook_authors, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, survey_textbook_authors, beneficiary).

% Document anti-dueling statutes, prosecutions, and their famously uneven enforcement. The composite grants their legislative record permanent sufficiency standing alongside the other three causes, protecting the specialty from demotion to a footnote. The cost is that their evidence is never asked to carry the whole account, so the sharpest open questions in their archive (why bans failed for generations while honored in breach) stay peripheral to the mainstream narrative. Leaving means abandoning a well-developed archival specialty.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_specialists, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, legal_prohibition_specialists, payer).

% Historians working in a law-and-economics register, arguing that courts, banking, libel litigation, and casualty insurance made the duel redundant as dispute resolution. The composite acknowledges their mechanism as sufficient, which shields the camp from outright defeat, but blocks the comparative-weight studies that would let their account win outright; manuscripts framed as adjudication get reviewed as reductionism. They can migrate into the broader violence-decline literature, which blunts the pressure they feel and keeps their resistance vocal.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_substitution_researchers, payer,
    powerful, biographical, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_substitution_researchers, beneficiary).

% Reconstruct the nineteenth-century Southern honor economy whose destruction in the Civil War ended the duel's social substrate. The composite preserves their case as a sufficient cause rather than a regional aside under a European-style modernization narrative, and it is the composite's most vivid exhibit. Their professional identities are fused with the honor-culture framework, so conceding the dominance question feels like recanting; meanwhile the independence of their factor from the other three is never tested.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_historians, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, southern_honor_historians, payer).

% Anthropologically oriented scholars tracing the displacement of honor codes by dignity norms across the Atlantic world. Cultural shift sits in the composite's list, which shields the paradigm from dismissal, but the process questions their research program exists to answer (pace, reversibility, generality of the norm transition) are crowded out by the settled four-part story. Recanting the framework would unravel careers built inside it, so exit from the frame is effectively unavailable even where publication access is not.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_theorists, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dueling_disappearance_mechanism__overdetermined_composite_reading, honor_culture_theorists, beneficiary).

% Enter a field where the composite is presented as settled background knowledge. A dissertation testing any single pathway returns ambiguous results because the other three causes are assumed to carry the residual variance; committees trained on the composite steer candidates toward descriptive synthesis. Years of training are sunk before the bind is visible, and degree timelines make waiting out the frame impossible.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, doctoral_students_in_historical_sociology, payer,
    powerless, immediate, trapped, continental).

% Causal-inference and counterfactual-methods scholars who would require the composite to specify joint predictions, necessity tests, and adjudication criteria before accepting sufficiency claims. They sit outside the history-field review loop that reproduces the composite; their objections surface intermittently in methodological venues and alter nothing in the narrative's day-to-day operation.
narrative_ontology:constraint_stakeholder(dueling_disappearance_mechanism__overdetermined_composite_reading, comparative_causality_methodologists, excluded,
    moderate, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dueling_disappearance_mechanism__overdetermined_composite_reading, diffuse).
narrative_ontology:fixing_cost_class(dueling_disappearance_mechanism__overdetermined_composite_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ends the mid-twentieth-century specialist standoff over dueling's decline by declaring each camp's mechanism independently sufficient, allowing legal, economic-institutional, anthropological, and regional-case scholarship to coexist in one account and enabling cross-regional synthesis.
% TRANSFER_FUNCTION: Moves explanatory certainty and citation centrality away from adjudication research toward synthesis narratives; moves publication access away from monocausal claims toward inclusive accounts; and transfers the risk of being decisively wrong from any individual camp to the collective four-part narrative.
% ABSENT_VOICES: Causal-inference methodologists and counterfactual historians would demand necessity tests and joint predictions; cliometricians would ask for quantified mechanism weights. Both stand outside the history-journal review loop that reproduces the composite. Inside the field, junior scholars internalize the frame early enough that adjudication ambitions are rarely voiced at all.
% DISAPPEARANCE_RATIONALE: Textbook chapters, review norms, dissertation templates, and the four specialist camps' publication strategies are organized around the composite; remove it overnight and the monocausal standoff reopens, existing syntheses lose their spine, and adjudication becomes mandatory rather than optional. Against that, the historical record itself is untouched, and scholars who regard the composite as a list of known forces rather than a tested claim argue the field would lose little. The parties dispute which loss, if any, is real.
% FOUNDING_PROBLEM: By the mid-twentieth century, legal historians, institutional economists, anthropologists of honor, and Southern regional specialists each claimed their mechanism alone explained dueling's disappearance, producing an unresolvable monocausal standoff that blocked all general synthesis of the phenomenon.
% FOUNDING_PROBLEM_CORROBORATION: External corroboration exists but is thin and methodological: the causal-inference and philosophy-of-history literature explicitly treats narrative overdetermination claims as unadjudicated, and cliometric venues continue commissioning single-mechanism tests, which is behavioral attestation from outside the beneficiary perimeter that the adjudication problem remains open. The historical participants themselves are extinct and can corroborate nothing; the four specialist camps, though they pursue dominance questions, sit inside the beneficiary set and therefore do not count as external witnesses.
narrative_ontology:disappearance_verdict(dueling_disappearance_mechanism__overdetermined_composite_reading, contested).
narrative_ontology:founding_problem_status(dueling_disappearance_mechanism__overdetermined_composite_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dueling_disappearance_mechanism__overdetermined_composite_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dueling_disappearance_mechanism__overdetermined_composite_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dueling_disappearance_mechanism__overdetermined_composite_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dueling_disappearance_mechanism__overdetermined_composite_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the composite's takings are epistemic rather than material: denied adjudication, dissertation designs doomed to ambiguity, and citation rents flowing to inclusive synthesis. It is not extreme because no camp loses publication access, income, or standing outright. Suppression (0.50) is soft gatekeeping — review norms, committee steering, textbook selection — rather than coercive exclusion; the excluded methodologists are outside the room, not silenced inside it. Theater (0.30) reflects a growing share of the composite's circulation being ritual citation ('multiple interacting causes were at work') rather than engagement, while genuine integrative historiography continues underneath. Accessibility_collapse (0.40) is low-to-moderate: the alternative accounts remain visibly published and citable; the composite marginalizes rather than erases them. Resistance (0.60) is sustained and real — cliometric challenges, counterfactual proposals, persistent monocausal monographs — which is itself evidence that enforcement is doing ongoing work. The measurement series run on one shared time grid ({0,10,20,30,40,50,60}) so every tracked metric is authored at every examined point; all three series rise together, consistent with a coordination arrangement accreting extractive overhead as it canonizes.
 *
 * PERSPECTIVAL GAP:
 *   From the survey_textbook_authors seat the composite is a serviceable consensus product they distribute and profit from; from the four specialist camps' seats it is shelter plus confinement — inclusion without victory; from the doctoral_students seat it is an inherited ceiling discovered only after years are sunk; from the excluded methodologists' seat it is a narrative that predicts nothing and forbids the tests that would matter. Same artifact, four incompatible experiences; the engine computes this divergence from the role/power/exit data rather than from the authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Pure beneficiaries derive low d; pure payers derive high d. The interesting seats are the three dual-positioned camps, who appear in BOTH the beneficiaries and victims arrays: nominal sufficiency-standing damps their directionality toward the subsidized end, while adjudication-denial and untested premises push it toward the target end. Net positioning differs by exit atom: southern_honor_historians sit nearest symmetric (their Civil War case is the composite's showpiece, and identity_locked exit raises effective targeting of whatever denial they suffer); institutional_substitution_researchers sit slightly target-side (their adjudication program is the most directly blocked, but arbitrage-grade exit into the violence-decline literature damps effective extraction); honor_culture_theorists sit target-side (identity_locked exit amplifies the cost of the frame they cannot recant). Doctoral students derive near-full-target d from the victims declaration plus trapped exit; textbook authors derive near-full-beneficiary d. No directionality_overrides are authored: the dual-role declarations plus differentiated exit atoms supply the derivation chain everything it needs, and override entries keyed only by power atom would misfire across seats sharing atoms (two camps share 'organized', two share 'moderate').
 *
 * MANDATROPHY ANALYSIS:
 *   A naive read sorts this arrangement as rope — it ended a real conflict, everyone signed on, nobody is coerced out. The tangled_rope classification forces the analyst to locate the extraction riding the same rails: the sufficiency-listing that shelters every camp also forecloses the comparative question each camp's deeper program needs answered, and the costs concentrate on those least able to refuse (juniors, students). Calling it a snare would misattribute symmetrically: no camp is expelled, coerced, or impoverished, and the enforcement apparatus suppresses a question, not a person. On lifecycle: the founding problem (ending the monocausal standoff) is live rather than dead — adjudication remains genuinely open — so this is not yet mandatrophy; the arrangement has frozen its founding problem, not outlived it. No sunset clause exists and none is plausible, so scaffold is excluded; theater remains subordinate to function, so piton is excluded. The measurement trajectory (rising extraction, rising theater, hardening enforcement) marks this as a tangled_rope drifting further from its coordination origin, not a degraded remnant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victim_set_dominance_dependence,
    'The composite''s victim set depends on which mechanism actually dominated historically — does the cost structure shift if institutional substitution carried more of the real-world decline than Civil War disruption, or vice versa?',
    'Regionally weighted counterfactual reconstruction: estimate each mechanism''s contribution to dueling''s decline by region and decade, then map which specialist camps'' adjudication ambitions were blocked hardest where their mechanism mattered most.',
    'Evenly spread load supports the tangled_rope profile (symmetrically denied camps, diffuse gains); concentrated load converts one camp''s denial into targeted extraction and drifts the story snare-ward with a sharper victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_dominance_dependence, empirical, 'Victim-set composition tracks the unknown dominance weights of the four mechanisms.').

omega_variable(
    epsilon_pathway_separability,
    'Is the extractiveness authored here for the composite-as-a-whole separable into per-mechanism components, or do the causal pathways interact such that only a joint epsilon is meaningful?',
    'Per-strand decomposition attempt: classify each mechanism''s historiographical handling as its own constraint story and compare summed component extraction against the joint figure.',
    'If separable, this story as authored violates epsilon-invariance and should decompose into linked per-mechanism stories; if inseparable, the joint epsilon stands and decomposition would fabricate precision the structure does not support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_pathway_separability, conceptual, 'Whether the composite''s epsilon admits decomposition along its four causal strands.').

omega_variable(
    scientific_conclusion_vs_truce,
    'Is the overdetermination composite a genuine scientific finding (the four conditions really were independently sufficient) or a disciplinary truce that suspends adjudication without resolving it?',
    'Adversarial-engagement audit: examine whether composite proponents respond to necessity tests and counterfactual adjudication attempts with new analysis or with restatement of the four-cause list.',
    'Genuine finding shifts weight to the coordination side and excess extraction toward rope territory; truce means the enforcement apparatus is maintaining a suspension, and extraction and enforcement intensity rise accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scientific_conclusion_vs_truce, conceptual, 'Composite thesis as settled science versus negotiated suspension of adjudication.').

omega_variable(
    falsifiability_cost_by_career_stage,
    'Who bears the practical cost of the composite''s weak falsifiability — junior scholars locked into ambiguous dissertation designs, or established camps shielded from decisive tests?',
    'Career-stage cohort study: track adjudication-framed dissertations and their publication outcomes against synthesis-framed work across the interval.',
    'If juniors absorb disproportionate cost, effective extraction concentrates on the powerless/trapped seat and sharpens computed seat divergence; if costs are symmetric, the tangled_rope reading stabilizes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(falsifiability_cost_by_career_stage, empirical, 'Distribution of the composite''s falsifiability costs across career stages.').

omega_variable(
    sibling_reading_structural_delta,
    'How would this story''s structure change if the composite''s joint-sufficiency premise yielded to a single dominant mechanism, as either sibling reading of the dueling_disappearance_mechanism kernel asserts?',
    'Adjudication outcome: if one mechanism proves dominant, reclassify against that sibling''s constraint — the contraction_reading implies cultural-specialist capture of the account; the institutional_displacement_reading implies institutional-camp capture.',
    'The composite''s tangled_rope with diffuse gains would resolve into the winning sibling''s beneficiary/victim geometry; the disagreement between readings is located precisely in the sufficiency-versus-dominance premise, nowhere else.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Structural consequences of kernel contest resolution on this reading''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__overdetermined_composite_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(duel_tr_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(duel_tr_t0, observed).
narrative_ontology:measurement(duel_tr_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(duel_tr_t10, observed).
narrative_ontology:measurement(duel_tr_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement_basis(duel_tr_t20, observed).
narrative_ontology:measurement(duel_tr_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement_basis(duel_tr_t30, observed).
narrative_ontology:measurement(duel_tr_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(duel_tr_t40, observed).
narrative_ontology:measurement(duel_tr_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(duel_tr_t50, observed).
narrative_ontology:measurement(duel_tr_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, theater_ratio, 60, 0.3).
narrative_ontology:measurement_basis(duel_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(duel_be_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(duel_be_t0, observed).
narrative_ontology:measurement(duel_be_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 10, 0.36).
narrative_ontology:measurement_basis(duel_be_t10, observed).
narrative_ontology:measurement(duel_be_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(duel_be_t20, observed).
narrative_ontology:measurement(duel_be_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement_basis(duel_be_t30, observed).
narrative_ontology:measurement(duel_be_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 40, 0.51).
narrative_ontology:measurement_basis(duel_be_t40, observed).
narrative_ontology:measurement(duel_be_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(duel_be_t50, observed).
narrative_ontology:measurement(duel_be_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, base_extractiveness, 60, 0.58).
narrative_ontology:measurement_basis(duel_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t0, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement_basis(duel_su_t0, observed).
narrative_ontology:measurement(duel_su_t10, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement_basis(duel_su_t10, observed).
narrative_ontology:measurement(duel_su_t20, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(duel_su_t20, observed).
narrative_ontology:measurement(duel_su_t30, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement_basis(duel_su_t30, observed).
narrative_ontology:measurement(duel_su_t40, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(duel_su_t40, observed).
narrative_ontology:measurement(duel_su_t50, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 50, 0.48).
narrative_ontology:measurement_basis(duel_su_t50, observed).
narrative_ontology:measurement(duel_su_t60, dueling_disappearance_mechanism__overdetermined_composite_reading, suppression_requirement, 60, 0.5).
narrative_ontology:measurement_basis(duel_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__overdetermined_composite_reading, resource_allocation).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, contraction_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__overdetermined_composite_reading, institutional_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the kernel dueling_disappearance_mechanism. The colloquial label 'why dueling disappeared' covers three structurally distinct claims, each warranting its own story with its own epsilon, beneficiary/victim geometry, and type: the contraction_reading (pure cultural-mechanism account), the institutional_displacement_reading (mechanism-substitution account), and this overdetermined_composite_reading (joint-sufficiency account operating as disciplinary infrastructure). This story exerts absorption pressure on the displacement reading (its claim is pre-included in the composite's list, weakening its distinctive stakes — hence the influences edge) while coexisting with the contraction reading across different factions of the profession. The upstream empirical anchors of the composite (anti-dueling statutes, court/banking expansion, honor-to-dignity norm transition, Civil War social disruption) are separate constraints and are not folded into this story's epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
