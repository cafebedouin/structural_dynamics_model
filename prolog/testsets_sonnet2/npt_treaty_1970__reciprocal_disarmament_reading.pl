% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__reciprocal_disarmament_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__reciprocal_disarmament_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_treaty_1970__reciprocal_disarmament_reading
 *   human_readable: NPT Article VI as Binding Reciprocal Disarmament Bargain
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the reciprocal-disarmament reading of the NPT
 *   kernel: Article VI is treated as a temporally urgent, legally binding
 *   obligation on nuclear weapon states to negotiate disarmament in good
 *   faith, standing in a genuine reciprocal bargain with the horizontal
 *   nonproliferation commitments of Articles I-II. Under this reading, the
 *   treaty's coordination function (freezing proliferation at five states) is
 *   real, but it is yoked to an extraction dynamic: NNWS permanently
 *   renounced weapons capability in exchange for a disarmament trajectory
 *   that fifty-plus years of NWS modernization programs have not honored. The
 *   enforcement gap — robust IAEA verification for Articles I-III, none for
 *   Article VI — is read here not as an implementation detail but as the
 *   structural mechanism of injustice: one side of the bargain is legally
 *   enforceable and the other is not, and that asymmetry is itself
 *   extractive. This is a distinct constraint from the
 *   oligopoly_enforcement_reading (which treats Articles I-II as the primary
 *   binding obligation and Article VI as contingent/aspirational, inverting
 *   which side of the bargain is enforceable) and from the
 *   withdrawal_sovereignty_reading (which treats Article X as legitimate
 *   sovereign exit rather than treaty breach). All three share the same
 *   treaty text but diverge on which clause is binding law and which is
 *   aspiration — they are three constraints, not three measurements of one.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, 0.68).
domain_priors:suppression_score(npt_treaty_1970__reciprocal_disarmament_reading, 0.55).
domain_priors:theater_ratio(npt_treaty_1970__reciprocal_disarmament_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__reciprocal_disarmament_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__reciprocal_disarmament_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__reciprocal_disarmament_reading, "NPT Article VI as Binding Reciprocal Disarmament Bargain").
narrative_ontology:topic_domain(npt_treaty_1970__reciprocal_disarmament_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__reciprocal_disarmament_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__reciprocal_disarmament_reading, '379c7430-53dd-4eb6-8ace-f64bd571f994').
narrative_ontology:cs_kernel_codification('379c7430-53dd-4eb6-8ace-f64bd571f994', fixed_text).
narrative_ontology:cs_authority_grounding('379c7430-53dd-4eb6-8ace-f64bd571f994', extraction).
narrative_ontology:cs_interpretation_layer_present('379c7430-53dd-4eb6-8ace-f64bd571f994').
narrative_ontology:cs_reading_relation('379c7430-53dd-4eb6-8ace-f64bd571f994', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('379c7430-53dd-4eb6-8ace-f64bd571f994', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('379c7430-53dd-4eb6-8ace-f64bd571f994', foundational, article_vi_creates_time_bound_enforceable_duty).
narrative_ontology:cs_axiom_status(article_vi_creates_time_bound_enforceable_duty, holdable).
narrative_ontology:cs_axiom_grounding('379c7430-53dd-4eb6-8ace-f64bd571f994', article_vi_creates_time_bound_enforceable_duty, conventional).
narrative_ontology:cs_axiom('379c7430-53dd-4eb6-8ace-f64bd571f994', foundational, nonproliferation_and_disarmament_are_indivisible_reciprocal_terms).
narrative_ontology:cs_axiom_status(nonproliferation_and_disarmament_are_indivisible_reciprocal_terms, holdable).
narrative_ontology:cs_axiom_grounding('379c7430-53dd-4eb6-8ace-f64bd571f994', nonproliferation_and_disarmament_are_indivisible_reciprocal_terms, deontological).
narrative_ontology:cs_reference_frame('379c7430-53dd-4eb6-8ace-f64bd571f994', id_1968_negotiating_bargain_equilibrium).
narrative_ontology:cs_drift_state('379c7430-53dd-4eb6-8ace-f64bd571f994', post_cold_war_modernization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('379c7430-53dd-4eb6-8ace-f64bd571f994', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__reciprocal_disarmament_reading, npt_secretariat_institutional_apparatus).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy_interests).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five treaty-recognized nuclear powers accepted Article VI's disarmament language to secure NNWS accession to Articles I-II, locking in horizontal nonproliferation while retaining their arsenals under a legally ambiguous 'good faith negotiate' clause. Under this reading they are bound by a real, enforceable, temporally urgent obligation to disarm — their continued modernization programs (submarine fleets, warhead life-extension, new delivery systems) are read as breaches, not sovereign prerogative. Their exit is arbitrage-grade in practice (no enforcement mechanism reaches them) but the reading treats that impunity itself as the injustice, not as evidence the obligation was never binding.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states, payer).

% The roughly 185 states that renounced nuclear weapons acquisition did so on the explicit promise, embedded in Article VI, that the nuclear powers would negotiate in good faith toward disarmament. Fifty-plus years on, they have foregone weapons capability permanently (accession is treated as irreversible under this reading's normative logic) while receiving no corresponding drawdown. They are trapped: having renounced the technology and accepted safeguards regimes, they cannot credibly re-threaten proliferation as leverage without incurring sanctions and reputational costs the NWS never risk symmetrically. Their remedy is entirely normative — NPT Review Conference statements, humanitarian-consequences initiatives, the TPNW — none of which binds the NWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, payer,
    organized, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__reciprocal_disarmament_reading, non_nuclear_weapon_states_coalition, excluded).

% Within the nuclear weapon states, the military-strategic establishments that plan force modernization, extended deterrence guarantees, and alliance commitments experience this reading as a direct constraint on doctrine and budget. If Article VI is binding law rather than aspirational language, modernization programs require legal justification against a disarmament obligation, arms-control diplomacy becomes compulsory rather than discretionary, and strategic flexibility narrows. This reading places their institutional latitude in the victim set: what defense planners treat as sovereign prerogative, the reading treats as a constrained, legally encumbered position.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, nws_strategic_autonomy_interests, payer,
    powerful, generational, constrained, global).

% The IAEA safeguards architecture and Review Conference process administer verification for Articles I-III with real inspection and reporting machinery, but no equivalent verification mechanism exists for Article VI — there is no inspection regime for disarmament negotiation 'good faith.' The apparatus perpetuates the treaty's legitimacy by running full review cycles every five years while lacking any structural capacity to enforce the obligation this reading treats as central. It benefits from the treaty's continued existence and moral authority regardless of whether Article VI is honored.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, npt_secretariat_institutional_apparatus, agenda_setter,
    institutional, civilizational, analytical, global).

% Civil society coalitions and the states behind the Treaty on the Prohibition of Nuclear Weapons argue the humanitarian consequences of nuclear weapons make any arsenal illegitimate now, not contingent on NWS good faith. They are structurally outside the NPT's formal amendment and enforcement process — they can produce normative pressure and a parallel treaty regime but cannot compel NWS compliance through the NPT's own machinery, which has no seat for non-state or non-NWS-recognized voices in enforcement decisions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, humanitarian_disarmament_movement, excluded,
    organized, generational, mobile, global).

% Issued the 1996 Advisory Opinion holding that Article VI creates an obligation to pursue in good faith and bring to a conclusion negotiations leading to nuclear disarmament — the strongest available third-party corroboration of this reading's binding-obligation claim, though advisory opinions are non-binding and no enforcement followed.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__reciprocal_disarmament_reading, international_court_of_justice, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__reciprocal_disarmament_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__reciprocal_disarmament_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The treaty solves a real collective action problem: without a bargain, universal proliferation was a plausible trajectory in the 1960s, and any state's acquisition pressures neighbors toward acquisition. The NPT coordinates near-universal renunciation of weapons acquisition in exchange for a reciprocal commitment to eventual disarmament and access to peaceful nuclear technology.
% TRANSFER_FUNCTION: Moves permanent renunciation of nuclear weapons capability from non-nuclear states to the regime, in exchange for a promised (and under this reading, legally binding) trajectory toward disarmament from nuclear weapon states — a trajectory that has not materialized in proportion to the renunciation received.
% ABSENT_VOICES: The humanitarian disarmament coalition and TPNW signatory states argue nuclear weapons are illegitimate independent of NWS reciprocity, but they hold no seat in NPT Review Conference enforcement decisions, which remain a state-consensus process dominated by the same NWS whose compliance is in question. Populations living under extended-deterrence umbrellas, who bear the doctrinal costs of continued reliance on nuclear weapons, are also absent from the bargain's negotiating table.
% DISAPPEARANCE_RATIONALE: NWS would argue the world stays largely unchanged — extended deterrence, national security doctrine, and modernization programs would continue much as they do now since Article VI has never been enforced. NNWS coalition members and disarmament advocates would argue the world rearranges substantially: the normative floor against explicit rearmament claims, the political cost of open renunciation of Article VI, and the entire safeguards architecture that Article VI's promise legitimizes would collapse, likely triggering proliferation cascades as the reciprocal bargain's remaining moral cover disappears.
% FOUNDING_PROBLEM: In the 1960s, the spread of nuclear weapons technology threatened to produce a multipolar nuclear world with dramatically higher accident and war risk; the treaty was built to freeze the number of nuclear weapon states at five while establishing a legal pathway toward their eventual elimination as the price non-nuclear states would accept for permanent renunciation.
% FOUNDING_PROBLEM_CORROBORATION: The International Court of Justice's 1996 Advisory Opinion, issued by a body outside both the NWS and NNWS blocs, corroborates that Article VI constitutes a binding legal obligation to pursue negotiations in good faith to a conclusion — supporting the founding problem's continued live status as a matter of law. However, the ICJ opinion is advisory and carries no enforcement mechanism; NWS governments and their own defense establishments (the primary benefiting parties) characterize the obligation as aspirational and contingent on the broader security environment, which is precisely the corroboration gap this reading identifies as structural injustice rather than mere non-compliance.
narrative_ontology:disappearance_verdict(npt_treaty_1970__reciprocal_disarmament_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_1970__reciprocal_disarmament_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__reciprocal_disarmament_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__reciprocal_disarmament_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__reciprocal_disarmament_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__reciprocal_disarmament_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 to 0.68) tracking the growing gap between NNWS renunciation (fixed and irreversible from 1970) and NWS modernization (continuing and in several respects accelerating post-2010 with new delivery systems and warhead life-extension programs). Theater ratio also rises (0.30 to 0.62): Review Conferences continue to produce consensus final documents referencing Article VI while substantive disarmament steps (New START notwithstanding) have stalled or reversed, and the ratio of ceremonial reaffirmation to actual arms reduction has grown. Suppression is moderate rather than high (0.55) because NNWS retain formal treaty exit rights (Article X) and a parallel normative track (TPNW) — this is a bargain enforced more by reputational and alliance-structure lock-in than direct coercion, but exit from renunciation itself is treated as effectively foreclosed once safeguards infrastructure and non-proliferation identity are established.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS agenda-setting seat, the modernization programs are legitimate exercises of sovereign security policy under a general, non-time-bound aspiration; from the NNWS payer seat and under this reading's own logic, those same programs are treaty breaches with a specific legal character established by the ICJ's 1996 opinion. The engine will compute these seats differently from the same structural data — that divergence is exactly what the reciprocal-disarmament reading exists to surface, in contrast to a reading that would find no divergence because Article VI was never binding in the first place.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are the structural beneficiaries under this reading: they receive the security benefit of a frozen proliferation landscape (no new nuclear rivals) while the reciprocal obligation binding them remains unenforced — this is treated as a subsidy captured at NNWS expense. NNWS sit at the target end: they made an irreversible concession (renunciation, verified by intrusive safeguards) for an unfulfilled promise. NWS strategic-autonomy interests occupy an unusual position — they are payers under this specific reading (their modernization latitude is constrained by treating Article VI as binding law) even though the NWS as states are beneficiaries; this bifurcation is the story's structural delta from the oligopoly_enforcement_reading, which would place NWS uniformly as beneficiaries with no internal victim seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (freeze proliferation, establish a legal pathway to elimination) is contested as live or dead: the NWS treat the freeze function as sufficient and permanent while treating disarmament as separately negotiable and indefinitely deferrable; the NNWS coalition treats the bargain as a single indivisible transaction whose disarmament half has gone unperformed for over five decades, converting what was framed as transitional scaffold-toward-elimination into a permanent tangled rope. This reading resists treating the arrangement as a simple rope (pure coordination) precisely because the coordination benefit (freeze) is being retained by NWS without paying the reciprocal disarmament cost the same clause obligates them to pay.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_status_kernel_contest,
    'Is Article VI a legally binding obligation with an enforceable timeline (this reading), a contingent/aspirational commitment subordinate to Articles I-II (oligopoly_enforcement_reading), or an obligation whose force is conditioned on the security environment that Article X permits states to exit (withdrawal_sovereignty_reading)?',
    'No single resolution mechanism exists because this is a genealogical/interpretive dispute over treaty text rather than an empirical question — the 1996 ICJ Advisory Opinion supports the binding reading but is non-binding itself and has not produced enforcement; state practice by NWS since 1970 is consistent with treating the clause as aspirational. Resolution would require either a binding ICJ contentious-case ruling or NWS acceptance of a verification protocol for Article VI, neither of which has occurred.',
    'If this reading is correct, NWS modernization programs constitute ongoing treaty breaches and the enforcement gap is a structural injustice with the NNWS coalition as a genuine victim class; if the oligopoly_enforcement_reading is correct, no such breach exists and the constraint is closer to a stable, if unequal, rope; if the withdrawal_sovereignty_reading is correct, the entire disarmament-bargain framing dissolves into a contingent, renegotiable security arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_status_kernel_contest, conceptual, 'Which of three competing readings of Article VI''s legal status is authoritative — the central kernel contest this story is one reading of.').

omega_variable(
    modernization_as_breach_or_prerogative,
    'Do ongoing NWS nuclear modernization programs constitute breaches of a binding Article VI obligation, or legitimate maintenance of a sovereign deterrent capability that Article VI does not restrict in the interim before eventual disarmament?',
    'Comparative analysis of NWS modernization spending trajectories against any credible timeline toward elimination; assessment of whether ''good faith negotiation'' as an ICJ-recognized standard has been met by NPT Review Conference participation absent substantive reductions.',
    'If modernization is breach, this reading''s placement of NWS strategic-autonomy interests in the victim set (constrained by binding law) is validated as a structural fact rather than rhetorical framing. If modernization is legitimate prerogative under Article VI''s actual (non-binding-timeline) text, the victim-set placement over-claims and NWS autonomy should instead be read as unconstrained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernization_as_breach_or_prerogative, conceptual, 'Whether NWS modernization is a treaty breach or legitimate interim conduct — determines whether the NWS victim-seat in this story is structurally warranted.').

omega_variable(
    enforcement_gap_natural_or_constructed,
    'Is the absence of Article VI verification machinery (in contrast to the robust IAEA safeguards regime for Articles I-III) a natural consequence of disarmament being harder to verify than proliferation, or a constructed asymmetry that NWS negotiating power produced deliberately during treaty drafting and subsequent Review Conferences?',
    'Historical analysis of 1968 negotiating record and subsequent Review Conference proposals for Article VI verification mechanisms (e.g., proposed reporting requirements) and why they were not adopted; comparison to verification regimes negotiated in other arms control treaties (START, INF) that did achieve mutual verification despite comparable technical difficulty.',
    'If natural/technical, the enforcement gap is a Mountain-like feature and this reading''s framing of it as ''structural injustice'' over-attributes agency; if constructed through NWS negotiating leverage, it corroborates the tangled_rope classification and the beneficiary/victim asymmetry authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_gap_natural_or_constructed, empirical, 'Whether the Article VI verification gap reflects genuine technical difficulty or negotiated asymmetric power — bears directly on whether the enforcement gap is natural or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__reciprocal_disarmament_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1980, 0.38).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2000, 0.5).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2020, 0.6).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__reciprocal_disarmament_reading, theater_ratio, 2025, 0.62).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2020, 0.66).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__reciprocal_disarmament_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1980, 0.4).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 1995, 0.45).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2000, 0.48).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2020, 0.53).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__reciprocal_disarmament_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__reciprocal_disarmament_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__reciprocal_disarmament_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__reciprocal_disarmament_reading, tpnw_treaty_prohibition_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the npt_treaty_1970 kernel, decomposed per the ε-invariance principle because the natural-language label 'the NPT' conflates structurally distinct claims about which article is binding law. reciprocal_disarmament_reading (this story) authors a substantially extractive tangled_rope (ε=0.68) where NWS retain a coordination benefit without discharging the reciprocal obligation. oligopoly_enforcement_reading would author a lower-ε, closer-to-rope classification from the NWS-favorable premise that Article VI was never binding. withdrawal_sovereignty_reading would author yet another structure centered on Article X's exit right rather than on breach/compliance at all. Each carries its own ε, beneficiary/victim structure, and stakeholder surface; they are linked here rather than merged. The TPNW is linked as a downstream constraint whose legitimacy and negotiating leverage this reading's account of unfulfilled Article VI directly feeds.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
