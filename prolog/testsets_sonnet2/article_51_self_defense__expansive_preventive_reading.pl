% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__expansive_preventive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__expansive_preventive_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: article_51_self_defense__expansive_preventive_reading
 *   human_readable: Article 51 Self-Defense — Expansive Preventive/Preemptive Reading
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   This story instantiates the expansive preventive/preemptive reading of
 *   the Article 51 self-defense kernel: the claim that self-defense extends
 *   to force against non-state actors or emerging threats whenever the acting
 *   state itself demonstrates necessity, without requiring a prior actual or
 *   imminent armed attack attributable to a state. This is one reading among
 *   several live readings of the same textual kernel — it is not a synthesis
 *   or an average of readings, and its ε (0.78, substantial and rising)
 *   belongs to this reading's own account of the standing arrangement it
 *   authorizes, not to the narrower or hybrid readings, which are separate
 *   constraints with their own ε values in their own files.
 *
 * KEY AGENTS:
 *   - militarily_capable_states: Primary agenda-setter and beneficiary (institutional/arbitrage) — self-certifies necessity, retains freedom of unilateral action
 *   - target_region_civilian_populations: Primary victim (powerless/trapped) — bears the physical cost of strikes premised on inchoate threats
 *   - weaker_states_facing_preventive_strikes: Secondary victim (powerless/trapped) — sovereignty violated without meeting the narrow reading's threshold
 *   - un_security_council_multilateral_authority: Institutional victim (institutional/constrained) — its ex ante authorization role is structurally bypassed
 *   - domestic_defense_industrial_sector: Secondary beneficiary (organized/arbitrage) — captures budget from standing preventive posture
 *   - international_law_scholars_and_icj: Analytical observer (analytical) — assesses whether state practice has crystallized this reading into custom
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, 0.78).
domain_priors:suppression_score(article_51_self_defense__expansive_preventive_reading, 0.72).
domain_priors:theater_ratio(article_51_self_defense__expansive_preventive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(article_51_self_defense__expansive_preventive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__expansive_preventive_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__expansive_preventive_reading, "Article 51 Self-Defense — Expansive Preventive/Preemptive Reading").
narrative_ontology:topic_domain(article_51_self_defense__expansive_preventive_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__expansive_preventive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__expansive_preventive_reading, '8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b').
narrative_ontology:cs_kernel_codification('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', fixed_text).
narrative_ontology:cs_authority_grounding('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', distributed).
narrative_ontology:cs_reading_relation('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', article_51_self_defense__narrow_armed_attack_reading, forecloses).
narrative_ontology:cs_reading_relation('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', foundational, necessity_is_self_judged_by_acting_state).
narrative_ontology:cs_axiom_status(necessity_is_self_judged_by_acting_state, holdable).
narrative_ontology:cs_axiom_grounding('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', necessity_is_self_judged_by_acting_state, conventional).
narrative_ontology:cs_axiom('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', foundational, anticipatory_defense_permissible_absent_prior_attack).
narrative_ontology:cs_axiom_status(anticipatory_defense_permissible_absent_prior_attack, holdable).
narrative_ontology:cs_axiom_grounding('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', anticipatory_defense_permissible_absent_prior_attack, instrumental).
narrative_ontology:cs_reference_frame('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', un_charter_collective_security_primacy).
narrative_ontology:cs_drift_state('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', post_9_11_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8ad2c1d2-35f7-4c29-a891-b0e8dbe32c6b', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__expansive_preventive_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, militarily_capable_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, domestic_defense_industrial_sector).
narrative_ontology:constraint_beneficiary(article_51_self_defense__expansive_preventive_reading, executive_war_powers_apparatus).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, weaker_states_facing_preventive_strikes).
narrative_ontology:constraint_victim(article_51_self_defense__expansive_preventive_reading, un_security_council_multilateral_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes Article 51 to justify strikes against non-state actors or emerging threats before an armed attack occurs, self-certifying necessity and imminence through internal legal opinions rarely subjected to external review. Maintains the reading through state practice, legal memoranda, and diplomatic pressure on international bodies, and benefits from the freedom of action the reading confers.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, militarily_capable_states, beneficiary).

% Supplies the platforms, munitions, and intelligence infrastructure that preventive and preemptive operations consume at a materially higher rate than reactive defense doctrine would require. Lobbies for doctrine that normalizes standing readiness and recurring strikes, and captures budget allocations tied to the expansive reading's operational tempo.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, domestic_defense_industrial_sector, beneficiary,
    organized, biographical, arbitrage, national).

% Live in the territory where preventive or preemptive strikes occur, absorbing civilian casualties, displacement, and infrastructure destruction from operations premised on threats that have not yet materialized into an armed attack. Have no standing to contest the necessity determination, no access to the acting state's internal legal reasoning, and no meaningful exit from the territory being struck.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, target_region_civilian_populations, payer,
    powerless, immediate, trapped, regional).

% Host territory or actors that a militarily capable state designates as an emerging threat, and bear the sovereignty violation of strikes on their soil without having committed or being on the verge of an armed attack by the narrow reading's standard. Lack the military capacity to deter preventive action and lack diplomatic leverage to compel Security Council review before the fact.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, weaker_states_facing_preventive_strikes, payer,
    powerless, biographical, trapped, national).

% Holds the Charter-designated authority to authorize force outside genuine self-defense, but the expansive reading routes force through unilateral self-certification before or instead of Council deliberation, sidelining the collective security mechanism the Charter was built around. Can condemn after the fact but cannot compel ex ante review, and permanent members with veto power on the acting-state side can shield the action from binding censure.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, un_security_council_multilateral_authority, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, un_security_council_multilateral_authority, excluded).

% Domestic executive branches gain expanded unilateral authority to order force without prior legislative or judicial authorization when the legal threshold is self-judged necessity against an inchoate threat rather than a demonstrable armed attack. Accrues discretionary power that narrower readings would require sharing with legislatures or international bodies.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, executive_war_powers_apparatus, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(article_51_self_defense__expansive_preventive_reading, executive_war_powers_apparatus, agenda_setter).

% Analyze state practice and opinio juris to assess whether the expansive reading has crystallized into customary law or remains a contested unilateral assertion. Produce scholarship and advisory opinions that shape, but do not bind, subsequent state behavior.
narrative_ontology:constraint_stakeholder(article_51_self_defense__expansive_preventive_reading, international_law_scholars_and_icj, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_51_self_defense__expansive_preventive_reading, diffuse).
narrative_ontology:fixing_cost_class(article_51_self_defense__expansive_preventive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides militarily capable states a rapid-response mechanism against genuinely emerging non-state threats (e.g., transnational terrorist networks) where waiting for an actual armed attack or Security Council authorization could mean absorbing catastrophic harm first.
% TRANSFER_FUNCTION: Moves the burden of proof for the use of force from the acting state (which must justify itself to a multilateral body) to the target population and target state (which must prove after the fact that no genuine necessity existed), while moving military-industrial budget allocation and executive discretion toward the acting state's institutions.
% ABSENT_VOICES: Target-region civilian populations and the states struck as 'emerging threats' have no seat in the acting state's internal necessity determination and no reliable forum to contest it before harm occurs; the UN Security Council is structurally bypassed rather than consulted, so its collective judgment is absent from the operative decision.
% DISAPPEARANCE_RATIONALE: If the expansive preventive reading were repudiated and replaced by the narrow armed-attack standard, militarily capable states would lose their primary unilateral legal cover for anticipatory strikes, defense budgets tied to standing preventive-strike readiness would face scrutiny, and weaker states/non-state-actor host territories would regain a meaningful (if imperfect) shield of sovereign non-intervention pending Security Council authorization.
% FOUNDING_PROBLEM: Article 51 was drafted to preserve a residual right of self-defense for states facing an actual armed attack in the gap before the Security Council could act, without licensing unilateral war-making that the Charter's collective security system was built to displace.
% FOUNDING_PROBLEM_CORROBORATION: Acting states and their legal advisers attest the founding problem — the danger of catastrophic non-state attacks with no state to hold accountable in advance — remains live and justifies the expansive reading. Independent international law scholars, the ICJ's Nicaragua and Wall advisory jurisprudence, and diplomats from targeted states attest that the expansive reading departs from the Charter's textual and drafting-history constraints and that its 'necessity' determinations are rarely corroborated by evidence available outside the acting state's own classified assessments.
narrative_ontology:disappearance_verdict(article_51_self_defense__expansive_preventive_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_51_self_defense__expansive_preventive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_51_self_defense__expansive_preventive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_51_self_defense__expansive_preventive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_51_self_defense__expansive_preventive_reading, 0.78, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__expansive_preventive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_51_self_defense__expansive_preventive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_51_self_defense__expansive_preventive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.55 to 0.78) because the reading's operative mechanism — self-judged necessity with no binding external check — has been invoked with increasing frequency and doctrinal confidence over the interval, each invocation reinforcing the precedent for the next. Suppression (0.72) reflects that alternatives (Security Council authorization, narrower self-defense claims) are not merely disfavored but structurally sidelined: once an acting state asserts this reading, the target state or population has essentially no forum, and the Security Council's censure power is blunted by veto alignment. Theater ratio (0.40) captures that a meaningful share of legal justification activity — invoking 'unwilling or unable' language, citing state practice — functions as post hoc legitimation rather than genuine ex ante constraint. Accessibility collapse (0.60) is moderate rather than near-total because the narrow reading and multilateral fora remain rhetorically available, just not operatively binding on the acting state.
 *
 * PERSPECTIVAL GAP:
 *   From the acting state's seat, this reading is coordination: closing a genuine gap in collective security architecture. From the target population's seat, the identical structure is unreviewable extraction: force visited upon them by a determination they cannot see, contest, or appeal. The engine should compute these as structurally different experiences of the same arrangement, not reconcile them to a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Militarily capable states and their executive war-powers apparatus sit at the beneficiary end: the reading expands their unilateral discretion and is self-enforced by their own legal opinions, so directionality derives low d. The defense industrial sector benefits indirectly through budget capture tied to operational tempo. Target-region civilian populations and weaker states facing preventive strikes sit at the target end — trapped exit options, no standing to contest necessity, bearing the full cost of a determination made elsewhere. The UN Security Council occupies an unusual institutional-power/constrained-exit position: it retains formal authority but the reading routes around its exercise, producing high effective extraction from an institutional seat that would ordinarily be expected to hold high bargaining power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a genuine gap where waiting for an actual armed attack could mean absorbing catastrophic harm from a non-state actor with no attributable state to hold accountable — was real at the doctrine's origin and arguably remains partially live (contested status, not dead). This prevents flattening the reading into pure snare: there is a genuine coordination problem (rapid response to catastrophic non-state threats) that the doctrine addresses. But the tangled_rope classification is warranted because the same structure that solves that coordination problem is also the mechanism by which asymmetric extraction occurs — the self-judging necessity standard that protects against catastrophic surprise is identical to the standard that removes external check on abuse. No sunset clause, no external adjudication requirement, and increasing reliance over time (T17-relevant drift) indicate the arrangement is not confined to its founding emergency but has become a standing feature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_crystallization_status,
    'Has the expansive preventive/preemptive reading crystallized into binding customary international law through consistent state practice and opinio juris, or does it remain a contested unilateral assertion that most states reject as a matter of law even while some states act on it?',
    'Systematic survey of state practice (frequency, geographic spread, and the reactions of non-acting states) combined with tracking of General Assembly resolutions, ICJ dicta, and formal legal objections lodged by other states against specific invocations.',
    'If crystallized as custom, the reading''s classification shifts toward a more settled (though still tangled) coordination mechanism with reduced illegitimacy; if it remains a persistent objector-style unilateral claim rejected by the majority of states, the extraction is better characterized as ongoing norm violation dressed in legal language, pushing the classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_crystallization_status, empirical, 'Whether the expansive reading has become binding custom or remains contested unilateral practice.').

omega_variable(
    genuine_versus_pretextual_necessity,
    'In the population of actual invocations of this reading, what fraction reflect genuine, verifiable imminent threats versus threats that are pretextual, exaggerated, or retrospectively unsupported by evidence?',
    'Post hoc declassification and independent investigative review (parliamentary inquiries, UN fact-finding missions, investigative journalism) comparing the acting state''s contemporaneous necessity claim against subsequently available evidence.',
    'A high genuine-necessity fraction would support the coordination-function half of the tangled_rope reading; a low fraction would indicate the necessity requirement functions mainly as legal cover for extraction, pushing the classification toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(genuine_versus_pretextual_necessity, empirical, 'Whether self-judged necessity claims are typically substantively grounded or typically pretextual.').

omega_variable(
    committer_framing_ambiguity,
    'Is the correct unit of analysis the acting-state''s declared legal doctrine (a formalized kernel reading) or the aggregate pattern of unilateral strikes it authorizes (an emergent practice with no single coherent doctrine)?',
    'Compare doctrinal consistency across the acting states'' own legal memoranda over time; if the stated legal standard shifts opportunistically case-by-case, the ''reading'' framing understates the arrangement''s ad hoc character.',
    'If the doctrine is genuinely stable and self-consistent across invocations, this reading is well-modeled as a single kernel reading; if the legal standard is invoked inconsistently and adjusted post hoc to fit whatever action was already taken, this constraint may better be modeled as an even more diffuse family of snare-like ad hoc justifications rather than one coherent expansive reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_framing_ambiguity, conceptual, 'Whether the expansive reading is a coherent doctrine or a retrospective label for ad hoc unilateral practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__expansive_preventive_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_51_self_defense__expansive_preventive_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(arti_tr_t4, article_51_self_defense__expansive_preventive_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(arti_tr_t8, article_51_self_defense__expansive_preventive_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(arti_tr_t12, article_51_self_defense__expansive_preventive_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(arti_tr_t16, article_51_self_defense__expansive_preventive_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(arti_tr_t20, article_51_self_defense__expansive_preventive_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(arti_tr_t24, article_51_self_defense__expansive_preventive_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(arti_be_t4, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 4, 0.62).
narrative_ontology:measurement(arti_be_t8, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(arti_be_t12, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 12, 0.71).
narrative_ontology:measurement(arti_be_t16, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 16, 0.74).
narrative_ontology:measurement(arti_be_t20, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 20, 0.76).
narrative_ontology:measurement(arti_be_t24, article_51_self_defense__expansive_preventive_reading, base_extractiveness, 24, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(arti_su_t4, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement(arti_su_t8, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(arti_su_t12, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 12, 0.64).
narrative_ontology:measurement(arti_su_t16, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(arti_su_t20, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(arti_su_t24, article_51_self_defense__expansive_preventive_reading, suppression_requirement, 24, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__expansive_preventive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_51_self_defense__expansive_preventive_reading, 0.1).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_narrow_armed_attack_reading).
narrative_ontology:affects_constraint(article_51_self_defense__expansive_preventive_reading, article_51_unable_unwilling_doctrine_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposing the natural-language 'Article 51 self-defense' kernel per the ε-invariance principle. The narrow_armed_attack_reading carries a much lower ε (tight constraint, state-attribution requirement, minimal self-judging). The unable_unwilling_doctrine_reading carries an intermediate ε (hybrid trigger requiring host-state failure as a threshold condition, providing some external anchoring absent here). This reading (expansive_preventive) carries the highest ε because necessity is wholly self-judged by the acting state with no attribution or host-state-failure threshold. All three are linked via affects_constraints because state practice and legal argument under one reading shapes the legitimacy terrain available to advocates of the others — an expansion of the expansive reading's acceptance erodes the practical force of the narrow reading's constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__expansive_preventive_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
