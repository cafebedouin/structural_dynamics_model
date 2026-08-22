% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction — Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   The Rome Statute created a permanent international criminal court whose
 *   jurisdiction is triggered only when national systems are 'unwilling or
 *   unable' to prosecute — the complementarity principle. Under the hybrid
 *   reading, this is neither pure sovereign veto nor pure universal mandate:
 *   the Court possesses genuine standing authority grounded partly in a
 *   natural-law-adjacent claim (that certain crimes offend humanity as such,
 *   independent of any state's consent) and partly in ordinary treaty consent
 *   (states opted in and can opt out). The mechanism functions as
 *   coordination — a shared forum avoiding ad hoc tribunals — layered with
 *   asymmetric extraction: victims and defendants in weak or non-cooperative
 *   states bear costs that victims and defendants in cooperative, capable
 *   states do not, and the pattern of who actually faces prosecution tracks
 *   geopolitical cooperation more than crime severity.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: institutional agenda-setter administering the admissibility gate
 *   - cooperative_state_parties: institutional beneficiaries with real exit (withdrawal) who gain a stabilizing external forum
 *   - atrocity_victims_in_noncooperative_states: powerless, trapped payers who absorb the enforcement gap
 *   - weak_states_lacking_domestic_capacity: moderate-power payers penalized for having ratified while lacking capacity
 *   - non_party_powerful_states: excluded institutional actors who retain Security Council leverage without treaty exposure
 *   - international_law_scholarship: analytical observer documenting the selective-enforcement pattern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.38).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction — Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'db63e0c6-d893-4d18-b3cb-a4cefbe2d607').
narrative_ontology:cs_kernel_codification('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', formalized).
narrative_ontology:cs_authority_grounding('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', lineage).
narrative_ontology:cs_interpretation_layer_present('db63e0c6-d893-4d18-b3cb-a4cefbe2d607').
narrative_ontology:cs_reading_relation('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', foundational, jurisdiction_is_consent_gated_but_normatively_universal_in_aspiration).
narrative_ontology:cs_axiom_status(jurisdiction_is_consent_gated_but_normatively_universal_in_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', jurisdiction_is_consent_gated_but_normatively_universal_in_aspiration, conventional).
narrative_ontology:cs_axiom('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', foundational, complementarity_is_genuine_substantive_limit_not_mere_formality).
narrative_ontology:cs_axiom_status(complementarity_is_genuine_substantive_limit_not_mere_formality, holdable).
narrative_ontology:cs_axiom_grounding('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', complementarity_is_genuine_substantive_limit_not_mere_formality, instrumental).
narrative_ontology:cs_reference_frame('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', post_nuremberg_ad_hoc_tribunal_synthesis).
narrative_ontology:cs_drift_state('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', post_african_union_withdrawal_threats_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('db63e0c6-d893-4d18-b3cb-a4cefbe2d607', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperative_state_parties).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_where_states_cooperate).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_noncooperative_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, weak_states_lacking_domestic_capacity).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, defendants_subject_to_forum_uncertainty).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, complementarity_doctrine).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, state_sovereignty_qualified_by_treaty_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines admissibility by assessing whether a state is 'unwilling or unable genuinely' to investigate or prosecute. Controls the gate that decides whether the Court steps in, which gives it enormous discretionary reach while formally deferring to states. Its authority to act depends on state parties' continued acceptance of the Statute and on Security Council referrals or state cooperation for arrest and evidence.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% States that ratified the Statute and generally cooperate gain a credible external forum that stabilizes their own transitional justice processes, deters domestic backsliding, and lets them prosecute domestically first to retain control. They can withdraw from the Statute if the arrangement becomes disadvantageous, giving them real exit leverage.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperative_state_parties, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, cooperative_state_parties, agenda_setter).

% Benefit when their state's domestic system or a functioning ICC referral actually produces accountability. They have no direct standing to compel jurisdiction themselves and depend entirely on state cooperation or Court action to see any redress.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_where_states_cooperate, beneficiary,
    powerless, biographical, trapped, national).

% Live under regimes that are non-parties, that block Security Council referral through veto-holding allies, or that simply refuse cooperation. The complementarity mechanism's deference to sovereignty means the Court cannot act without an arrest that never comes; these victims absorb the full cost of the gap between the Statute's aspirational reach and its consent-dependent enforcement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_noncooperative_states, payer,
    powerless, biographical, trapped, national).

% States that ratified in good faith but lack functioning judiciaries find themselves nominally 'unable' under the complementarity test, triggering ICC jurisdiction they cannot resist procedurally, while wealthier non-party states with equal or worse records remain outside the Court's reach entirely. They bear the asymmetric cost of having joined.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, weak_states_lacking_domestic_capacity, payer,
    moderate, generational, constrained, national).

% Face prosecution in whichever forum — domestic or international — the admissibility determination lands on, often years after the alleged conduct, with the choice of forum shaped by geopolitics as much as by the 'unwilling or unable' test. Cannot predict or contest which system will try them until the determination is made.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, defendants_subject_to_forum_uncertainty, payer,
    powerless, biographical, trapped, national).

% Major military powers that never ratified sit entirely outside the jurisdictional reach the hybrid reading claims for the Court, while retaining Security Council votes that can trigger or block referrals against others. Their absence from the treaty is the load-bearing fact the universalist reading has to explain away and the sovereigntist reading treats as vindication.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_party_powerful_states, excluded,
    institutional, civilizational, arbitrage, global).

% Studies whether complementarity functions as principled deference to sovereignty or as a structural excuse for selective, geopolitically-patterned enforcement. Produces the doctrinal literature that both sovereigntist and universalist readings cite against each other.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_law_scholarship, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, diffuse).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international forum and set of substantive crime definitions so states do not have to build ad hoc tribunals after every atrocity, while preserving each state's first right to prosecute its own nationals — solving the genuine problem of impunity gaps without requiring states to surrender primary criminal jurisdiction.
% TRANSFER_FUNCTION: Moves adjudicative authority from domestic courts to the ICC only in cases of proven unwillingness or inability, and moves cooperation costs (arrest, evidence-sharing, extradition) from the Court onto state parties who must expend political capital to comply.
% ABSENT_VOICES: Non-party powerful states are structurally outside the framework yet influence it via Security Council referral and veto power; victims in blocked jurisdictions have no procedural voice to compel action and their absence is invisible in admissibility hearings that focus on state conduct, not victim need.
% DISAPPEARANCE_RATIONALE: Cooperative states and the ICC bureaucracy would lose a functioning (if partial) forum and international law would lose its clearest standing accountability mechanism — that arrangement would visibly rearrange. But for victims in non-cooperative states, the Statute's disappearance would change little in practice, since the complementarity deference already leaves them without effective recourse; whether the world 'rearranges' depends entirely on which seat is asked.
% FOUNDING_PROBLEM: After Nuremberg and the ad hoc tribunals for Yugoslavia and Rwanda, states sought a permanent, standing court to prosecute genocide, crimes against humanity, and war crimes without needing to negotiate a new tribunal's mandate and legitimacy from scratch after each atrocity — while avoiding a supranational court that could simply override national courts and sovereignty at will.
% FOUNDING_PROBLEM_CORROBORATION: The ICC and cooperative state parties attest the founding problem remains live and the complementarity design is working as intended. Independent scholarship (e.g. analyses from the International Crisis Group and academic commentators outside the Court's own registry) and several African Union member states — themselves signatories, not merely outside critics — attest that in practice the mechanism has produced selective enforcement concentrated on weaker states, suggesting the founding problem has partially mutated into a legitimacy problem the framers did not anticipate.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) and theater ratio (0.48, rising over the interval) reflect a coordination mechanism whose function has been increasingly consumed by geopolitically patterned selective enforcement — the Court's actual docket has concentrated on weaker African and non-aligned states while remaining structurally unable to reach non-party powers, a drift the temporal series traces from 1998 (low extraction, mechanism newly operative) to 2024 (moderate extraction, substantial performative overhead from cases that never reach trial). Suppression (0.38) is moderate: states retain a genuine, low-cost exit (withdrawal, as Burundi and the Philippines exercised), which caps how coercive the arrangement can be, but weak states that ratified face de facto lock-in once domestic incapacity triggers admissibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Cooperative state parties and the ICC's own office sit near the beneficiary end: they retain agenda control, and states can exit via withdrawal (mobile). Powerless victims in non-cooperative states and defendants facing forum uncertainty sit near the full-target end — trapped exit, no standing to compel action, bearing the complementarity gap's full cost. Weak states occupy an intermediate but genuinely disadvantaged position: they lack the exit leverage of powerful non-parties and the capacity of cooperative developed states, so the same nominal treaty terms produce different effective extraction depending on domestic capacity — a same-level (state-to-state) divergence the derivation captures via exit_options (mobile vs. constrained) rather than power_atom alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (impunity gaps after ad hoc tribunals) is contested as live vs. mutated: cooperative, capable states report the mechanism functioning as designed, while independent scholarship and even some signatory states report the practical effect has drifted toward selective enforcement concentrated on the weak. Classifying this as tangled_rope rather than snare or rope preserves both halves: there is a genuine, non-fake coordination function (a standing forum with defined crimes, avoiding re-litigating tribunal legitimacy each time) AND asymmetric extraction running through the identical complementarity mechanism, which is exactly the hybrid structure the tangled_rope classification exists to name — collapsing it to pure extraction would erase the real coordination benefit cooperative victims and states receive; collapsing it to pure coordination would erase the documented selective-enforcement pattern.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_authority_source_ambiguity,
    'Does the ICC''s jurisdictional authority under this reading rest genuinely on a hybrid of natural-law-adjacent universal claims and treaty consent, or is the ''natural law'' component rhetorical cover for what is structurally pure treaty consent with aspirational language attached?',
    'Examine whether the Court or its judges have ever asserted jurisdiction over conduct by a non-party state absent Security Council referral on grounds other than the Statute''s text — a genuine hybrid-authority claim would predict at least attempted assertions of this kind; their absence would support the reading that this is treaty consent alone, dressed in universalist language.',
    'If purely treaty-consent, this story collapses toward the sovereigntist_reading structurally, even while retaining different rhetorical framing; if genuine hybrid authority is exercised, the reading is structurally distinct and the coordination/extraction balance shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_authority_source_ambiguity, conceptual, 'Whether the hybrid reading''s claimed dual authority source is structurally real or rhetorical.').

omega_variable(
    kernel_disagreement_locus,
    'Where exactly do the sovereigntist, universalist, and hybrid readings of the Rome Statute kernel disagree — is it about what the treaty text says, about what legitimate international authority requires, or about empirical facts regarding how the Court has actually behaved?',
    'Systematic comparison of admissibility rulings against each reading''s predicted outcomes: the sovereigntist reading predicts near-total deference to any state assertion of domestic proceedings; the universalist reading predicts the Court would override weak domestic proceedings even absent a clear unwillingness finding; the hybrid reading predicts a middle pattern keyed to genuine capacity/willingness findings. Docket analysis could adjudicate which pattern actually obtains.',
    'If Court practice tracks the sovereigntist prediction, this hybrid story overstates the Court''s independent authority; if it tracks the universalist prediction, this story understates it. The disagreement is located in the empirical operation of complementarity, not merely in normative framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_disagreement_locus, conceptual, 'Locating where the three sibling readings of the kernel actually diverge — text, normative theory, or empirical practice.').

omega_variable(
    selective_enforcement_causal_mechanism,
    'Is the documented concentration of ICC prosecutions on weaker, non-aligned states caused by the complementarity mechanism''s structure itself, or by independent factors (referral politics, evidentiary access, state cooperation willingness) that would produce the same pattern under any of the three kernel readings?',
    'Comparative institutional analysis: would a strict sovereigntist regime or a strong universalist regime have produced a materially different case distribution, holding referral and cooperation patterns constant?',
    'If the selective pattern is intrinsic to complementarity''s design, it strengthens the tangled_rope classification (extraction running through the coordination mechanism itself). If it is caused by exogenous factors common to all three readings, the extraction should be partly attributed to the broader international system rather than to this specific reading''s structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_enforcement_causal_mechanism, empirical, 'Whether selective enforcement is intrinsic to the complementarity design or exogenous to it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(rome_tr_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2014, 0.42).
narrative_ontology:measurement(rome_tr_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2019, 0.46).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.22).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.28).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.33).
narrative_ontology:measurement(rome_be_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2014, 0.37).
narrative_ontology:measurement(rome_be_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2019, 0.4).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.24).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.28).
narrative_ontology:measurement(rome_su_t2014, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2014, 0.32).
narrative_ontology:measurement(rome_su_t2019, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2019, 0.35).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints sharing the rome_statute_jurisdiction kernel. universalist_reading treats complementarity as a procedural formality subordinate to a transcendent mandate (predicted lower deference to sovereignty, higher claimed authority, likely lower measured suppression but higher claimed universal reach). sovereigntist_reading treats complementarity as the substantive, near-total limit on jurisdiction (predicted higher deference, lower claimed authority, likely classified closer to rope or scaffold given the emphasis on consent and exit). This hybrid_complementarity_reading occupies the structurally contested middle, authored here as tangled_rope given the coexistence of genuine coordination function with documented asymmetric extraction. Each sibling has its own ε, beneficiary/victim structure, and stakeholders — they are not variations of one measurement but three distinct constraints linked through this network field.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
