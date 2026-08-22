% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__diplomatic_capital_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__diplomatic_capital_reading, []).

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
 *   constraint_id: eu_council_unanimity__diplomatic_capital_reading
 *   human_readable: EU Council Unanimity Requirement — Diplomatic Capital Reading
 *   domain: political/institutional/international_relations
 *
 * SUMMARY:
 *   In the policy domains where it still governs — foreign and security
 *   policy, taxation, treaty change — the EU Council's unanimity requirement
 *   conditions every collective act on the assent of all member governments.
 *   This story instantiates the diplomatic-capital reading of that
 *   arrangement: unanimity as a consensus-forcing device whose negotiation
 *   costs purchase legitimacy and durability, on the theory that iterated
 *   bargaining converts latent defection into buy-in and that unanimously
 *   adopted positions survive longer than majority-imposed ones. Per the
 *   epsilon-invariance principle, the sibling readings
 *   (sovereignty-guarantor, veto-trap) are separate constraint files over the
 *   same standing arrangement; this file authors only this reading's
 *   assessment and does not hedge across them. The claim/metric gap is
 *   deliberate and independent: the constraint is CLAIMED as rope (a genuine
 *   coordination solution with net-beneficial participation), while the
 *   metrics describe what this reading actually observes — bounded but real
 *   costs that creep upward with each enlargement, a slowly inflating share
 *   of performative unity, and a gradually decaying normative grip.
 *
 * KEY AGENTS:
 *   - rotating_council_presidency: agenda-setting broker ([institutional]/[constrained]) — chairs the search for consensus on a six-month clock
 *   - european_commission: proposal author and secondary beneficiary ([institutional]/[constrained]) — assembles consent before texts reach the table
 *   - smaller_member_states: primary beneficiary ([moderate]/[constrained]) — veto parity multiplies their voice
 *   - large_member_states: beneficiary and principal cost-bearer ([institutional]/[constrained]) — pays the highest concession bill for durability
 *   - national_parliaments: excluded seat ([moderate]/[trapped]) — ratifies bargains it did not shape
 *   - affected_domestic_constituencies: excluded seat ([powerless]/[trapped]) — inherits unanimous outcomes without a channel
 *   - integration_scholars: analytical observer ([analytical]/[analytical]) — tests the durability premise from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__diplomatic_capital_reading, 0.28).
domain_priors:suppression_score(eu_council_unanimity__diplomatic_capital_reading, 0.28).
domain_priors:theater_ratio(eu_council_unanimity__diplomatic_capital_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(eu_council_unanimity__diplomatic_capital_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__diplomatic_capital_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__diplomatic_capital_reading, "EU Council Unanimity Requirement — Diplomatic Capital Reading").
narrative_ontology:topic_domain(eu_council_unanimity__diplomatic_capital_reading, "political/institutional/international_relations").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__diplomatic_capital_reading, 'c81b521a-ae84-4922-b02a-a878f904861b').
narrative_ontology:cs_kernel_codification('c81b521a-ae84-4922-b02a-a878f904861b', formalized).
narrative_ontology:cs_authority_grounding('c81b521a-ae84-4922-b02a-a878f904861b', lineage).
narrative_ontology:cs_interpretation_layer_present('c81b521a-ae84-4922-b02a-a878f904861b').
narrative_ontology:cs_reading_relation('c81b521a-ae84-4922-b02a-a878f904861b', eu_council_unanimity__sovereignty_guarantor_reading, coexists_with).
narrative_ontology:cs_reading_relation('c81b521a-ae84-4922-b02a-a878f904861b', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_axiom('c81b521a-ae84-4922-b02a-a878f904861b', foundational, consent_produces_durability).
narrative_ontology:cs_axiom_status(consent_produces_durability, holdable).
narrative_ontology:cs_axiom_grounding('c81b521a-ae84-4922-b02a-a878f904861b', consent_produces_durability, empirically_contingent).
narrative_ontology:cs_axiom('c81b521a-ae84-4922-b02a-a878f904861b', foundational, negotiation_cost_is_legitimacy_investment).
narrative_ontology:cs_axiom_status(negotiation_cost_is_legitimacy_investment, holdable).
narrative_ontology:cs_axiom_grounding('c81b521a-ae84-4922-b02a-a878f904861b', negotiation_cost_is_legitimacy_investment, instrumental).
narrative_ontology:cs_reference_frame('c81b521a-ae84-4922-b02a-a878f904861b', consensus_as_legitimacy_source).
narrative_ontology:cs_drift_state('c81b521a-ae84-4922-b02a-a878f904861b', post_lisbon_enlargement_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c81b521a-ae84-4922-b02a-a878f904861b', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__diplomatic_capital_reading, european_commission).
narrative_ontology:constraint_victim(eu_council_unanimity__diplomatic_capital_reading, large_member_states).
narrative_ontology:constraint_vindicates(eu_council_unanimity__diplomatic_capital_reading, consensus_legitimacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Chairs Council meetings for a six-month rotation, drafts compromise texts, and brokers package deals among twenty-seven delegations. Gains diplomatic visibility from successfully closed summits but hands the chair to the next member before long-term consequences of its bargains arrive. Cannot decline the consensus obligation while holding the chair.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, rotating_council_presidency, agenda_setter,
    institutional, immediate, constrained, continental).

% Drafts the proposals that require every government's assent in the covered domains, so it invests heavily in pre-negotiation, technical accommodation, and side-payments to assemble consent before a text ever reaches the table. Unanimous adoption confers extra legitimacy on its acts; a single delegation's objection kills a proposal outright. It lives inside the system it services and has nowhere else to propose.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, european_commission, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, european_commission, beneficiary).

% Hold a formal veto equal to the largest members', which multiplies their voice far beyond their market size or population. They extract accommodations during package bargaining that a population-weighted vote would never grant them. Leaving the Union would forfeit market access and transfers, so they stay and bargain instead.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, smaller_member_states, beneficiary,
    moderate, generational, constrained, continental).

% Contribute the largest budget shares and absorb the longest negotiation timelines; they concede the most in package deals to hold the union together. In return they receive commitments immune to reversal by shifting majorities and avoid being outvoted on core national interests. Their size gives them bilateral leverage outside the room that small members cannot match.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, large_member_states, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__diplomatic_capital_reading, large_member_states, payer).

% Learn the terms of Council bargains after executives have shaken hands; ratification votes frequently present take-it-or-leave-it choices on texts they did not shape. They would insist on prior scrutiny of the positions their governments take in Brussels. They cannot unbind their governments from an agreed text short of treaty-level confrontation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, national_parliaments, excluded,
    moderate, biographical, trapped, national).

% Bear the consequences of unanimous decisions — tax arrangements, sanctions regimes, foreign-policy stances — filtered entirely through their national government's negotiating position. They have no seat in the room and no procedural channel through which to object to a bargain struck between governments.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, affected_domestic_constituencies, excluded,
    powerless, biographical, trapped, national).

% Track whether unanimously adopted decisions endure longer than majority-made ones, whether successive enlargements have made consensus unattainable, and whether blocking has shifted from expression of dissent to bargaining instrument. Their published assessments feed treaty-reform debates, but they hold no vote and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__diplomatic_capital_reading, integration_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(eu_council_unanimity__diplomatic_capital_reading, diffuse).
narrative_ontology:fixing_cost_class(eu_council_unanimity__diplomatic_capital_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the commitment problem of twenty-seven sovereign states taking common action without a coercive center: requiring every government's assent guarantees that no state is bound against its will, converting would-be defection into negotiated buy-in and making adopted positions materially harder to reverse.
% TRANSFER_FUNCTION: Moves concession and time: agenda proponents trade side-payments, exemptions, and patience to the last reluctant government; in exchange the adopted act carries every member's consent. Negotiating attention and diplomatic effort flow from the capitals into iterative bargaining rounds; legitimacy and durability flow back out with the decision.
% ABSENT_VOICES: National parliaments and affected domestic constituencies stand outside the room: bargains are struck among executives, and publics encounter unanimous outcomes as settled facts filtered through their own government's position. Candidate countries and third states targeted by unanimous foreign-policy decisions likewise object from outside with no procedural channel open to them.
% DISAPPEARANCE_RATIONALE: Replace unanimity with qualified-majority voting overnight and small-state voice collapses immediately: leverage shifts to population-weighted coalitions, blocking ends, decisions accelerate, and the consent premium built into every adopted act disappears. Governments that accepted deep integration partly for veto protection would reopen the treaty settlement, and several domains — taxation, foreign policy — would begin producing outputs that some members refuse to carry.
% FOUNDING_PROBLEM: After the 1965 empty-chair crisis, the Community needed a way to keep taking joint decisions among states that would not accept being outvoted on what they deemed vital interests; the Luxembourg Compromise entrenched effective unanimity as the standing price of continued integration.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: sovereigntist parties campaigning to preserve national vetoes attest that the consent demand is real even while opposing integration's aims; international-relations scholarship on credible commitment among sovereigns independently identifies the same problem; and the historical record of majority-rule failures in the League of Nations supplies the negative case. No attestation relies solely on the governments that benefit from the rule.
narrative_ontology:disappearance_verdict(eu_council_unanimity__diplomatic_capital_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__diplomatic_capital_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__diplomatic_capital_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(eu_council_unanimity__diplomatic_capital_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__diplomatic_capital_reading, 0.28, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__diplomatic_capital_reading_tests).
:- end_tests(eu_council_unanimity__diplomatic_capital_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is low (0.28 at interval end) because the arrangement's costs — negotiating time, concession, delayed action — are bounded and, on this reading, repaid in durability; the measurement series shows the cost creeping upward with each enlargement (six members to twenty-seven) as the price of assembling consent grows with N. Suppression (0.28) is a raw, unscaled structural property: the rule blocks action rather than coercing agents, and its normative grip has visibly decayed — hence the tracked suppression_requirement series, which falls from 0.48 to 0.28 as workarounds proliferated (constructive abstention, passerelle clauses, enhanced cooperation) and blocking acquired reputational cost. Theater (0.22) is modest but rising: summit communiques increasingly declare unity that the bargaining record qualifies, yet the core activity remains substantive negotiation, not performance. Accessibility_collapse (0.45) reflects that alternatives — qualified-majority voting in adjacent domains, enhanced cooperation, coalitions outside the Union — remain partly available; resistance (0.30) reflects recurring reform pressure to widen majority voting, met by broad acquiescence from the states the rule protects. All three tracked series run on one shared time grid (1966, 1979, 1992, 2004, 2009, 2017, 2024) so no metric's end-state value is silently substituted into earlier periods.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same rule. Smaller member states experience unanimity as voice protection approaching pure benefit: the veto is the only institution where their formal weight equals Germany's. Large member states sit nearest symmetric — they pay the largest concession bills and the longest timelines, repaid in commitments no shifting majority can reverse; their net position is positive but the closest to break-even of any seated party. The rotating presidency experiences the arrangement as brokerage workload under a six-month horizon, collecting visibility and handing on consequences. The Commission experiences it as a consent-assembly tax on every proposal, repaid in the legitimacy premium of unanimous adoption. The excluded domestic seats experience the whole structure as distant executive dealing. Inter-institutionally, the Commission and the presidency both administer the rule but neither controls it; same-level laterally, small and large member states hold nominally identical sovereign standing while their exit options and concession burdens differ sharply — the differentiation the engine reads from power and exit data, not from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party is declared a beneficiary and no party is declared a victim, because on this reading the arrangement has no fixed extraction asymmetry: participants net out positive, which is precisely what distinguishes this reading from its veto-trap sibling. Derived directionality therefore sits low for all seated agents. The one refinement the derivation misses is that large member states sit nearer symmetric than small ones (their concession burden approaches their durability gain); no directionality override is authored because the override mechanism keys on power atom, and an institutional-atom override would misfire across the three institutional seats (presidency, Commission, large states) whose relationships differ. The excluded seats — parliaments and domestic constituencies — bear diffuse costs with no formal position; their exclusion is this reading's principal blind spot and is carried in the omegas rather than forced into the beneficiary/victim structure, which belongs to this reading's own lights.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding sovereign states into common action without a coercive center — remains live, and the founding-problem status (live) is consistent with the disappearance verdict (world_rearranges), so no zombie mismatch arises. Mandatrophy discipline cuts both ways here. Against mislabeling as pure extraction: the negotiation cost is not rent collection, because no seat captures the gains — the receipt surface is affirmatively diffuse, checked across every named seat (small states collect voice, large states collect durability, the presidency collects episodic visibility, the Commission collects passage legitimacy, and none of these is the arrangement's surplus accruing to one place). Against romanticizing: the receipt surface also records that fixing is prohibitive — extending majority voting requires the unanimous consent of the very governments whose veto the reform would remove, so the rule's protective feature and its reform-lock are the same mechanism. The prohibitive-plus-diffuse cell is nominally the degraded-institution cell, and the divergence is worth stating plainly: theater_ratio stays low (0.22) and the founding problem stays live, which argues the function is intact rather than performed. The engine weighs the full structure; this story authors the facts and lets the computation speak.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading characterizes the dominant function of the Council''s unanimity requirement — legitimacy investment (this story), sovereignty protection, or minoritarian leverage?',
    'Comparative institutional analysis across the three sibling stories over the identical referent: survival rates of unanimous versus qualified-majority decisions, frequency and success rate of blocking threats, and the distributional incidence of negotiation delay.',
    'If leverage dominates, effective extraction rises sharply and the arrangement computes toward enforced extraction; if sovereignty protection dominates, the beneficiary structure reorganizes around defense against majoritarian coercion. Cross-reading divergence is signal about the frames, not an error to be reconciled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This story is one reading of the eu_council_unanimity kernel; the sibling readings instantiate different constraints over the same standing arrangement.').

omega_variable(
    epsilon_reading_indexicality,
    'Is the low epsilon authored here a property of the unanimity arrangement itself, or an artifact of the diplomatic-capital frame''s charitable accounting of negotiation cost?',
    'Reading-indexed comparison under a fixed referent: hold the standing unanimity requirement constant and compare the epsilon each sibling reading authors over it; systematic divergence locates the disagreement in the frames rather than in the world.',
    'Confirms that epsilon is a property of a reading over a shared referent and forecloses averaging across readings, which would fabricate a constraint that none of the contending parties actually holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_reading_indexicality, conceptual, 'Whether the reading''s low-extraction assessment survives frame-independent scrutiny.').

omega_variable(
    durability_premise_testability,
    'Does the reading''s load-bearing empirical premise hold — that unanimous decisions are more durable and defect less than qualified-majority impositions?',
    'Survival analysis of EU legislative and foreign-policy decisions by voting rule, controlling for policy salience and domain, comparing reversal and non-implementation rates.',
    'Refutation collapses the reading''s justification for the coordination cost, shifting evidential weight toward the veto-trap sibling and raising the assessed extraction of the same arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durability_premise_testability, empirical, 'Testability of the consent-produces-durability claim on which this reading rests.').

omega_variable(
    enlargement_cost_scaling,
    'Does the buy-in dividend continue to cover the coordination cost at twenty-seven members, or has the membership crossed the threshold beyond which consensus is unattainable and delay is pure cost?',
    'Time-to-decision and concession-distribution data across successive enlargements, plus uptake of enhanced cooperation as revealed preference for exiting the unanimity requirement.',
    'Past the threshold, the legitimacy payoff thins, the rising extractiveness series steepens, and the arrangement drifts toward inertial maintenance of a form its members can no longer operate as designed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enlargement_cost_scaling, empirical, 'Whether coordination cost scaling with membership has outrun the legitimacy dividend.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__diplomatic_capital_reading, 1966, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t1966, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 1966, 0.08).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t1966, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t1979, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 1979, 0.1).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t1979, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t1992, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 1992, 0.13).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t1992, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t2004, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2004, 0.16).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t2004, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t2009, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2009, 0.18).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t2009, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t2017, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2017, 0.2).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t2017, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_tr_t2024, eu_council_unanimity__diplomatic_capital_reading, theater_ratio, 2024, 0.22).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(ecu_diplomatic_capital_be_t1966, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1966, 0.14).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t1966, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t1979, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1979, 0.17).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t1979, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t1992, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 1992, 0.21).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t1992, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t2004, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2004, 0.24).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t2004, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t2009, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2009, 0.26).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t2009, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t2017, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2017, 0.27).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t2017, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_be_t2024, eu_council_unanimity__diplomatic_capital_reading, base_extractiveness, 2024, 0.28).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(ecu_diplomatic_capital_su_t1966, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1966, 0.48).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t1966, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t1979, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1979, 0.44).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t1979, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t1992, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 1992, 0.4).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t1992, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t2004, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2004, 0.36).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t2004, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t2009, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2009, 0.33).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t2009, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t2017, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2017, 0.3).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t2017, observed).
narrative_ontology:measurement(ecu_diplomatic_capital_su_t2024, eu_council_unanimity__diplomatic_capital_reading, suppression_requirement, 2024, 0.28).
narrative_ontology:measurement_basis(ecu_diplomatic_capital_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__diplomatic_capital_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__sovereignty_guarantor_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__diplomatic_capital_reading, eu_council_unanimity__veto_trap_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'EU unanimity' decomposes into three structurally distinct constraints sharing one referent (the standing unanimity requirement) and differing in the reading that assesses it. This file authors the diplomatic-capital reading (low epsilon, net-beneficial participation, no victim class); the sovereignty-guarantor sibling authors a protective beneficiary structure centered on sovereign consent; the veto-trap sibling authors substantially higher epsilon with identifiable targets of blocking leverage. The siblings are linked here per the family rule; each carries its own epsilon, beneficiaries, and claimed type, and cross-reading divergence is the measurement the family exists to take.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
