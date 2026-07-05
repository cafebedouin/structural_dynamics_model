% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Backward-Binding Constraint
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the strict stare decisis reading of the
 *   common-law precedent kernel: precedent binds as a genuinely
 *   backward-looking constraint, and departure from it requires an
 *   extraordinarily high threshold justification (workability failure,
 *   doctrinal erosion, changed factual predicate, or reliance-interest
 *   imbalance so severe that adherence itself becomes unjust). This is one of
 *   three structurally distinct readings of the same kernel — the
 *   evolutionary_framework reading treats precedent as an adaptive scaffold
 *   open to contemporary reinterpretation, and the pluralist_balancing
 *   reading treats precedent weight as domain- and context-variable, balanced
 *   case by case. Each reading is a separate constraint with its own ε,
 *   beneficiary/victim structure, and classification; this file models only
 *   the strict reading. Under strict stare decisis, the coordination function
 *   (predictable adjudication) is real, but so is the asymmetric extraction:
 *   settled interest holders and commercial actors capture the reliance
 *   dividend while litigants seeking doctrinal correction and historically
 *   disadvantaged claimants bear the cost of an extraordinarily high
 *   departure threshold, sustained by active appellate enforcement of the
 *   overruling standard.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: agenda_setter (institutional/arbitrage) — administers the extraordinary-justification standard and controls interpretive apparatus
 *   - settled_interest_holders: beneficiary (powerful/mobile) — captures predictability dividend
 *   - litigants_seeking_doctrinal_change: payer (moderate/trapped) — blocked at threshold before merits review
 *   - historically_disadvantaged_claimants: payer (powerless/trapped) — bears compounding cost of precedent inertia across generations
 *   - lower_court_judges_bound_by_erroneous_precedent: payer/agenda_setter (institutional/constrained) — administers locally, cannot revise
 *   - legislature: excluded (institutional/constrained) — no voice over constitutional precedent correction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.44).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.58).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.44).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Backward-Binding Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'c69e335a-397d-48b2-b345-7d491392b3bc').
narrative_ontology:cs_kernel_codification('c69e335a-397d-48b2-b345-7d491392b3bc', distributed).
narrative_ontology:cs_authority_grounding('c69e335a-397d-48b2-b345-7d491392b3bc', lineage).
narrative_ontology:cs_interpretation_layer_present('c69e335a-397d-48b2-b345-7d491392b3bc').
narrative_ontology:cs_reading_relation('c69e335a-397d-48b2-b345-7d491392b3bc', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('c69e335a-397d-48b2-b345-7d491392b3bc', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('c69e335a-397d-48b2-b345-7d491392b3bc', foundational, departure_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(departure_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('c69e335a-397d-48b2-b345-7d491392b3bc', departure_requires_extraordinary_justification, conventional).
narrative_ontology:cs_axiom('c69e335a-397d-48b2-b345-7d491392b3bc', secondary, uniform_threshold_across_legal_domains).
narrative_ontology:cs_axiom_status(uniform_threshold_across_legal_domains, holdable).
narrative_ontology:cs_axiom_grounding('c69e335a-397d-48b2-b345-7d491392b3bc', uniform_threshold_across_legal_domains, instrumental).
narrative_ontology:cs_reference_frame('c69e335a-397d-48b2-b345-7d491392b3bc', vertical_and_horizontal_binding_precedent).
narrative_ontology:cs_drift_state('c69e335a-397d-48b2-b345-7d491392b3bc', contemporary_doctrinal_reform_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('c69e335a-397d-48b2-b345-7d491392b3bc', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, settled_interest_holders).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, commercial_actors_relying_on_predictability).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_doctrinal_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, historically_disadvantaged_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_erroneous_precedent).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the doctrine of horizontal and vertical stare decisis: decides which precedents bind, articulates the extraordinary-justification standard for overruling, and controls the interpretive apparatus (distinguishing, narrowing, dicta-classification) that determines whether a prior holding controls a new case. Insulated from most direct costs of rigidity; benefits from reduced case-by-case scrutiny of its own reasoning and from institutional legitimacy narratives tied to consistency.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Parties who structured transactions, contracts, property arrangements, or legal strategy around existing precedent. They collect the predictability dividend directly — the doctrine's rigidity is what protects their reliance interests from being unsettled by new argument or evolving norms.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, settled_interest_holders, beneficiary,
    powerful, generational, mobile, national).

% Businesses and financial institutions that price risk and structure contracts on the assumption that governing precedent will not shift absent extraordinary justification. They lobby for and cite stare decisis to defeat novel claims that would disturb settled commercial doctrine.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, commercial_actors_relying_on_predictability, beneficiary,
    organized, biographical, mobile, national).

% Parties whose claims require a court to revisit or overturn governing precedent to succeed. They must clear an extraordinarily high threshold (special justification, workability failure, doctrinal erosion, reliance-interest balancing) even where the precedent is demonstrably wrong or outdated. Most such litigants lose not on the merits of their underlying claim but on the threshold question of whether departure is permitted at all.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_doctrinal_change, payer,
    moderate, biographical, trapped, national).

% Groups whose exclusion or subordination was encoded into precedent decades or centuries earlier under different social and evidentiary conditions. Strict stare decisis makes correction of these holdings dependent on courts finding the departure standard met, which historically has taken generations even where the injustice is well documented. They bear the compounding cost of precedent inertia most acutely because they had no voice in the precedent's formation.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, historically_disadvantaged_claimants, payer,
    powerless, generational, trapped, national).

% Bound by vertical stare decisis to apply controlling precedent from higher courts even where they can identify its reasoning as unsound or its factual predicate as obsolete. Their only lawful recourse is distinguishing on narrow facts or flagging the issue for appellate reconsideration; they cannot depart directly. They administer the constraint locally while bearing no power to revise it.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_erroneous_precedent, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_erroneous_precedent, agenda_setter).

% Study the doctrine's operation, catalog overruling rates, and debate whether strict stare decisis serves rule-of-law values or ossifies error. Their critiques circulate in law reviews and occasionally reach courts as amicus argument, but they do not control outcomes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars_and_treatise_writers, observer,
    analytical, civilizational, analytical, national).

% Can statutorily override some precedents interpreting statutes, but cannot touch constitutional precedent, which remains exclusively within judicial control under strict stare decisis. Where the precedent at issue is constitutional, the legislature has no voice at all in correcting it — this is precisely the domain the doctrine reserves to the judiciary.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legislature, excluded,
    institutional, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stare decisis solves a genuine coordination problem: it lets courts, litigants, and third parties predict how a given legal question will be resolved without relitigating first principles in every case, and it constrains judicial discretion so outcomes do not turn on which panel or judge hears a dispute.
% TRANSFER_FUNCTION: Moves the cost of legal certainty from settled interest holders and commercial actors (who receive predictability) onto litigants whose claims depend on doctrinal correction and onto historically disadvantaged groups whose exclusion is encoded in older holdings — the transfer is the difference between a merits hearing and a threshold departure-standard hearing.
% ABSENT_VOICES: Litigants who could show a precedent is wrong but cannot meet the extraordinary-justification threshold never get their substantive argument heard on the merits — the doctrine forecloses the conversation at the threshold stage. Groups whose subordination was encoded in precedent decades ago were never parties to the cases that bound them and have no formal channel to reopen those holdings except by convincing a court, generations later, that the very high bar for departure has been met.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight and courts treated all precedent as merely persuasive, settled commercial and property arrangements built on predictability would face renewed litigation risk, appellate courts would see a surge in cases re-arguing settled doctrine, and the predictability premium currently captured by well-resourced repeat litigants would erode; simultaneously the threshold barrier currently blocking correction of erroneous or outdated holdings would fall, giving previously foreclosed claims a path to a merits hearing.
% FOUNDING_PROBLEM: Common law systems needed a mechanism to prevent judicial decisions from being arbitrary or personality-dependent — deciding like cases alike, so that the law would be knowable in advance rather than reinvented by each judge from first principles.
% FOUNDING_PROBLEM_CORROBORATION: Predictability and rule-of-law scholars outside the judiciary (comparative law academics studying overruling rates across common-law jurisdictions) attest the coordination problem remains partially live but note the doctrine's strict form now blocks correction well past the point where the underlying uncertainty problem would justify it; civil rights historians and dissenting jurists document cases where the extraordinary-justification standard shielded holdings for decades after their factual and moral premises had been discredited, which is corroboration from outside the judiciary that benefits from the doctrine's own operation.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.44, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).
:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.44 at interval end) and rising slowly: the coordination function of predictability is genuine and substantial, so the doctrine is not purely extractive, but the extraordinary-justification threshold increasingly functions as a gate that forecloses merits review for a growing share of doctrinal-change claims as precedent accumulates. Suppression is higher (0.58) because the constraint's persistence depends on active appellate enforcement of a demanding overruling standard, not on litigant preference — departure is possible in principle but structurally rare by design. Theater ratio is modest (0.28) and slowly rising: courts do perform genuine departure analysis in most cases, but a growing share of 'extraordinary justification' opinions recite the standard as ritual affirmation of non-departure rather than substantive engagement with whether the precedent remains sound. Accessibility collapse (0.62) reflects that once a precedent is understood as controlling, the practical paths to challenge it (distinguishing, waiting for a circuit split, awaiting a change in court composition) are narrow and slow. Resistance (0.55) captures ongoing scholarly and litigant pressure against rigid adherence, particularly from historically disadvantaged claimant groups and doctrinal reformers.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, strict stare decisis is virtuous judicial restraint protecting the rule of law from personality-driven adjudication. From the seat of a claimant whose case requires overturning outdated precedent, the identical doctrine operates as a nearly impassable procedural wall that forecloses the merits entirely. The engine computes these as different per-seat classifications from the same structural data; the claimed_type (tangled_rope) is authored as the story's own analytical judgment, independent of either seat's self-report.
 *
 * DIRECTIONALITY LOGIC:
 *   The appellate judiciary sits nearest the beneficiary end: it administers the standard, is insulated from the costs of rigidity, and gains legitimacy and reduced re-litigation burden from consistency. Settled interest holders and commercial actors are direct beneficiaries — their d sits low because the constraint subsidizes their reliance interests. Litigants seeking doctrinal change and historically disadvantaged claimants sit near the full-target end — trapped exit options, no capacity to relitigate outside the system, and the extraordinary-justification threshold operates specifically against their interests. Lower court judges are structurally payers of the constraint's rigidity (bound to apply precedent they may see as wrong) while simultaneously being local agenda-setters who administer it — hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing arbitrary, personality-dependent adjudication — remains partially live (predictability is a real ongoing need), which is why this is not classified as a pure snare. But the founding problem's status is contested: for holdings whose factual or moral premises have been discredited, the coordination rationale has been substantially satisfied for correction, yet the extraordinary-justification threshold continues to block departure years or generations past that point. Classifying this as tangled_rope rather than snare or rope prevents both mislabeling errors: it does not treat the genuine predictability function as if it did not exist (which a snare label would), and it does not treat the accumulated extraction on foreclosed claimants as incidental friction (which a rope label would).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predictability_vs_correction_tradeoff_location,
    'At what point does the marginal predictability gain from an additional generation of precedent-adherence fall below the marginal cost of continuing to bind litigants to a precedent whose factual or moral predicate has been substantially undermined?',
    'Comparative empirical study of overruling rates and outcomes across jurisdictions that use strict stare decisis versus pluralist-balancing or evolutionary frameworks — tracking whether the more flexible regimes show measurably worse predictability outcomes or merely different correction timelines.',
    'If flexible regimes show comparable predictability with faster correction of discredited precedent, the extraordinary-justification threshold in the strict reading is revealed as excess extraction beyond what the coordination function requires; if flexible regimes show materially worse predictability, the strict threshold''s rigidity is closer to necessary coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(predictability_vs_correction_tradeoff_location, empirical, 'Whether strict stare decisis''s departure threshold exceeds what the predictability function requires.').

omega_variable(
    kernel_reading_selection_is_contested,
    'Is strict stare decisis the structurally correct reading of the common-law precedent kernel, or is it one contested reading among the evolutionary_framework and pluralist_balancing siblings, each held by different judicial philosophies and jurisdictions?',
    'This is not resolvable by further data within a single reading — it is a live jurisprudential dispute. Track which reading a given court or jurisdiction actually applies (via citation patterns and overruling frequency) rather than asking which reading is ''true.''',
    'Different jurisdictions and different eras of the same court plausibly operate under different readings simultaneously; the classification of any given precedent dispute depends on which reading is operative in that forum, not on a single answer for the kernel as a whole.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_selection_is_contested, conceptual, 'The kernel itself is contested across three coexisting readings; this story models only the strict reading.').

omega_variable(
    historically_disadvantaged_correction_lag_causal_mechanism,
    'Is the multi-generational lag in correcting precedents that encoded historical subordination best explained by the strict-stare-decisis threshold itself, or by independent social and political factors (composition of the bench, availability of counsel, litigation funding) that would have produced similar lag under any precedent regime?',
    'Compare correction timelines for comparable discredited holdings in jurisdictions with different stare decisis strength, controlling for bench composition and case funding, to isolate the doctrine''s independent contribution to the lag.',
    'If the doctrine is the primary driver, the victim classification for historically_disadvantaged_claimants is strongly supported; if independent factors dominate, the doctrine''s causal contribution to the extraction is smaller than the metrics assume, though the structural relationship (trapped exit, no formal correction channel) would still hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historically_disadvantaged_correction_lag_causal_mechanism, empirical, 'Whether the doctrine itself or independent social factors drive correction lag for historically disadvantaged claimants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.18).
narrative_ontology:measurement(comm_tr_t12, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 12, 0.2).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 24, 0.22).
narrative_ontology:measurement(comm_tr_t36, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 36, 0.24).
narrative_ontology:measurement(comm_tr_t48, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 48, 0.26).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(comm_be_t12, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 12, 0.36).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(comm_be_t36, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 36, 0.41).
narrative_ontology:measurement(comm_be_t48, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 48, 0.43).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(comm_su_t12, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(comm_su_t36, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 36, 0.55).
narrative_ontology:measurement(comm_su_t48, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 48, 0.57).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the common_law_precedent_corpus kernel (strict_stare_decisis, evolutionary_framework, pluralist_balancing), each authored as a separate constraint story with its own epsilon, stakeholders, and classification per the epsilon-invariance principle. The strict reading is distinguished from evolutionary_framework by treating departure as requiring extraordinary justification rather than routine contemporary reinterpretation, and from pluralist_balancing by applying a uniform high threshold rather than domain-variable weighting. All three are linked via affects_constraints because a shift toward or away from any one reading in a given jurisdiction structurally changes the operating environment (litigation strategy, forum shopping incentives, doctrinal stability expectations) for the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
