% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI as Binding Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story authors the NNWS (non-nuclear-weapon-state) reading of the NPT
 *   kernel: Article VI's disarmament language is a binding legal obligation,
 *   and non-proliferation is the conditional restraint NNWS accepted in
 *   exchange for NWS movement toward elimination. Under this reading, the
 *   treaty is a rope with moderate and rising extractiveness — a genuine
 *   coordination mechanism (preventing a multipolar nuclear world) whose
 *   reciprocal consideration has been progressively under-delivered,
 *   producing a widening gap between the compliance NNWS actually bear and
 *   the disarmament progress NWS actually deliver. This is NOT the same
 *   constraint as the NWS reading (which treats disarmament as aspirational
 *   and non-proliferation as the operative binding law) or the
 *   withdrawal-threshold reading (which concerns Article X exit costs). Each
 *   is authored as its own constraint with its own ε; they are linked via
 *   network.affects_constraints and cs_structure.reading_relations, not
 *   merged.
 *
 * KEY AGENTS:
 *   - non_nuclear_weapon_states: primary payer (moderate/constrained) — bears the compliance and forbearance costs
 *   - nuclear_weapon_states: agenda-setter and structural beneficiary (institutional/arbitrage) — controls Review Conference interpretation with no binding disarmament deadline
 *   - global_civilian_population: diffuse beneficiary (powerless/trapped) — benefits from restraint but has no enforcement standing
 *   - tpnw_states_parties and civil_society_disarmament_advocates: excluded voices pressing the disarmament-obligation reading from outside the NPT's own consensus machinery
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.42).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.35).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI as Binding Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, '07fdac8d-bfd6-4bb6-abc5-105c17d6df33').
narrative_ontology:cs_kernel_codification('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', fixed_text).
narrative_ontology:cs_authority_grounding('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', distributed).
narrative_ontology:cs_reading_relation('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', foundational, article_vi_creates_binding_result_obligation).
narrative_ontology:cs_axiom_status(article_vi_creates_binding_result_obligation, holdable).
narrative_ontology:cs_axiom_grounding('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', article_vi_creates_binding_result_obligation, conventional).
narrative_ontology:cs_axiom('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', foundational, nonproliferation_consideration_is_conditional_not_absolute).
narrative_ontology:cs_axiom_status(nonproliferation_consideration_is_conditional_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', nonproliferation_consideration_is_conditional_not_absolute, conventional).
narrative_ontology:cs_reference_frame('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', grand_bargain_reciprocity_framework).
narrative_ontology:cs_drift_state('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', post_2015_review_conference_breakdown, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('07fdac8d-bfd6-4bb6-abc5-105c17d6df33', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_civilian_population).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, grand_bargain_reciprocity_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, disarmament_as_legal_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepted a permanent legal prohibition on acquiring nuclear weapons in exchange for a treaty text promising NWS would pursue good-faith disarmament negotiations 'at an early date' and would share peaceful nuclear technology. Fifty-plus years on, they hold the compliance burden (safeguards inspections, export controls, forgone weapons options) while the reciprocal obligation remains textually asserted but practically unenforced. Their exit is constrained: withdrawal under Article X carries severe diplomatic and security costs, and the alternative (the TPNW) does not bind NWS at all.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% The five recognized NWS (US, Russia, UK, France, China) administer the Review Conference process, control the interpretive record through consensus-blocking, and have modernized rather than reduced arsenals over the treaty's life. They treat Article VI as a process obligation (negotiate) rather than a result obligation (disarm), and can absorb Review Conference criticism without binding consequence. Their exit option is effectively arbitrage: they retain the treaty's non-proliferation benefits (freezing the weapons club at five) while facing no comparably enforceable disarmament deadline.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits diffusely from any restraint on nuclear proliferation and any genuine disarmament progress, bearing the existential risk of nuclear war or accident. Has no seat at Review Conferences and no mechanism to enforce the bargain; their interest is represented only derivatively through NNWS delegations and civil society.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, global_civilian_population, beneficiary,
    powerless, civilizational, trapped, universal).

% Administers safeguards verification and convenes the five-year Review Conferences where the disarmament-obligation dispute is aired. Documents NWS arsenal modernization and NNWS compliance but has no enforcement authority over Article VI itself — its mandate is verification of non-proliferation, not disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, iaea_and_review_conference_secretariat, observer,
    institutional, generational, analytical, global).

% A coalition of NNWS and civil society actors who concluded the NPT's disarmament bargain had failed and negotiated the Treaty on the Prohibition of Nuclear Weapons as a parallel, more categorical instrument. They are structurally excluded from NPT Review Conference decision-making influence proportional to their numbers because NWS and NWS-allied states boycott and delegitimize the TPNW process; their objection — that Article VI has been read out of the bargain — is exactly what this reading asserts, yet they operate outside rather than inside the NPT's own institutional machinery.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_states_parties, excluded,
    organized, generational, mobile, global).

% NGOs, scientific bodies, and disarmament campaigners (e.g. ICAN) who document arsenal trends and lobby at Review Conferences but hold no vote and no formal standing in the treaty's amendment or interpretation process.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, civil_society_disarmament_advocates, excluded,
    moderate, generational, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT's original bargain coordinates a genuine collective-action problem: preventing horizontal proliferation while creating a credible path toward eventual disarmament, avoiding a world of dozens of nuclear-armed states.
% TRANSFER_FUNCTION: Moves a permanent, legally enforceable restraint (forgoing nuclear weapons acquisition, submitting to safeguards) from NNWS to the non-proliferation regime, in exchange for a textually asserted but weakly enforced NWS obligation to negotiate disarmament in good faith and share peaceful nuclear technology.
% ABSENT_VOICES: TPNW states parties and disarmament NGOs would insist the bargain has been broken and that Article VI creates a present, not merely aspirational, legal obligation — they raise this at Review Conferences but are excluded from the consensus-based decision structure NWS effectively control.
% DISAPPEARANCE_RATIONALE: NNWS argue that if the disarmament obligation vanished as a live legal claim, the non-proliferation regime would lose its normative legitimacy and NNWS compliance would erode over a generation as the reciprocity rationale collapsed; NWS argue the non-proliferation architecture would continue essentially unchanged since it already operates as if the disarmament clause were aspirational, making the world largely unchanged from the NWS reading's own vantage. The verdict genuinely depends on which reading of the kernel is applied — hence contested rather than resolved here.
% FOUNDING_PROBLEM: In 1968, the founding problem was a rapidly proliferating nuclear world (multiple states pursuing weapons programs) combined with a demand from non-nuclear states that they not be asked to accept a permanent inferior legal status without a reciprocal commitment from the weapons states to eventually disarm.
% FOUNDING_PROBLEM_CORROBORATION: The 1995 and 2000 Review Conference final documents (consensus texts including NWS signatures) explicitly reaffirmed disarmament as a binding Article VI commitment, and the ICJ's 1996 advisory opinion described an obligation to pursue negotiations to a conclusion in good faith — corroboration from outside the NNWS beneficiary set. However, NWS delegations at subsequent Review Conferences (2015, 2022) blocked consensus final documents partly over disarmament-pace disputes, indicating the corroboration itself has weakened over time even among signatories who once affirmed it.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at a moderate 0.42 (rising from 0.20 in 1970) because the coordination function (avoiding proliferation cascade) remains genuinely operative and valued by most NNWS, but the reciprocal consideration (verifiable disarmament) has thinned over five decades of arsenal modernization programs, making the bargain increasingly one-directional. Suppression is moderate (0.35) — NNWS are not coerced into the NPT by force, but face severe diplomatic, security-guarantee, and reputational costs for withdrawal, and the consensus rule at Review Conferences structurally suppresses the disarmament-obligation reading from becoming binding practice. Theater ratio rises sharply (0.25 → 0.58) reflecting increasing performative content in the Review Conference cycle: elaborate final-document negotiations, 'action plans' (2010), and NWS statements of commitment that have not translated into verified reductions proportional to the rhetoric — a classic Goodhart substitution where the negotiating process becomes the measured deliverable instead of arsenal reduction itself.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are declared as both payer and (secondary) beneficiary: they pay through permanent forgoing of weapons options and intrusive safeguards, and they benefit from the non-proliferation regime's stabilizing effect on their own security environment (a nuclear-armed neighbor is worse than a non-proliferation treaty with weak enforcement). NWS are the structural beneficiary/agenda-setter: they retain nuclear status, control the interpretive venue, and bear no comparably enforceable cost. Global civilian population benefits diffusely with no standing to enforce. This directionality reflects the NNWS reading specifically — under the sibling NWS reading, the beneficiary/victim assignment inverts (NNWS become the bound party, disarmament becomes non-binding aspiration), which is exactly why that must be a separate story.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (proliferation cascade risk) remains partly live — the non-proliferation function still coordinates real value — but the disarmament half of the bargain shows classic mandatrophy symptoms: the process (Review Conferences, action plans, PrepComs) has become self-sustaining even as its substantive deliverable (verified arsenal reduction toward elimination) stalls or reverses. The rope classification (rather than tangled_rope) is authored because, from the NNWS reading's own lights, the coordination function still substantially dominates and enforcement remains negotiation-based rather than coercive — but the rising theater_ratio and extractiveness trend are the leading indicators that this reading's own actors increasingly experience the arrangement as tangled_rope-adjacent, which is a live and explicit internal contest documented in the omegas below rather than resolved by fiat here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Does Article VI''s ''good faith negotiation'' language create a binding obligation of result (disarmament must occur) or merely a binding obligation of process (negotiations must be pursued), and which reading does customary international law now support?',
    'The ICJ''s 1996 advisory opinion leaned toward an obligation to pursue negotiations to a conclusion, but has not been tested in binding contentious litigation. A future ICJ contentious case, or a Review Conference consensus document explicitly adopting one reading, would resolve this for treaty-law purposes.',
    'If the obligation-of-result reading prevails, NWS arsenal modernization becomes an actionable treaty breach and this reading''s extractiveness score should rise sharply toward tangled_rope; if the obligation-of-process reading prevails, this reading converges toward the NWS reading''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Whether Article VI is a result-obligation or process-obligation, and which reading has stronger legal standing.').

omega_variable(
    reciprocity_bargain_still_operative,
    'Was the 1968 grand bargain (non-proliferation for disarmament) ever a legally cognizable quid pro quo, or is this a retrospective NNWS narrative imposed on a treaty text that always separated the two obligations into independent articles?',
    'Historical treaty-negotiation record analysis (travaux préparatoires) and comparison with how NWS delegations characterized the bargain during 1965-68 negotiations versus how they characterize it in current Review Conference statements.',
    'If the bargain was genuinely quid pro quo at drafting, the NNWS reading has strong originalist grounding and this reading''s classification is robust; if the linkage is a later interpretive gloss, the nws_reading''s aspirational framing gains ground and this constraint''s beneficiary/victim structure becomes more contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_bargain_still_operative, empirical, 'Whether the disarmament-for-nonproliferation linkage was drafted as binding reciprocity or is a later NNWS interpretive construction.').

omega_variable(
    tpnw_regime_competition_effect,
    'Does the TPNW''s emergence as a parallel, more categorical prohibition regime strengthen the NNWS reading''s leverage within the NPT (by demonstrating a credible exit/alternative) or weaken the NPT''s coordination function (by fragmenting the non-proliferation consensus and giving NWS an excuse to dismiss NNWS grievances as adequately addressed elsewhere)?',
    'Track NPT Review Conference outcomes and NWS negotiating positions before and after TPNW entry into force (2021) for evidence of either increased NWS engagement with disarmament demands or increased NWS dismissal of the NPT''s disarmament pillar as ''covered'' by TPNW critics outside the treaty.',
    'If TPNW strengthens NNWS leverage, this reading''s extractiveness trend may reverse; if it fragments consensus, the rising extractiveness trend documented in measurements is likely to continue or accelerate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_regime_competition_effect, empirical, 'Whether TPNW regime competition helps or harms the NNWS reading''s practical leverage inside the NPT.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.32).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_text__nnws_reading, theater_ratio, 2005, 0.45).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nnws_reading, theater_ratio, 2015, 0.52).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.2).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_text__nnws_reading, base_extractiveness, 2005, 0.36).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nnws_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_treaty_text__nnws_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the npt_treaty_text kernel. npt_treaty_text__nws_reading inverts the beneficiary/victim structure (non-proliferation binding on NNWS; disarmament aspirational) and should show lower extractiveness from the NWS-favorable vantage. npt_treaty_text__withdrawal_threshold_reading addresses the structurally distinct Article X exit-threshold question, linked here because disillusionment with this reading's disarmament grievance is a primary driver of withdrawal-threshold contests. All three share the same underlying treaty text but instantiate different ε values, different stakeholder structures, and potentially different classifications — per the ε-invariance principle they are authored as separate stories rather than one story with a reading parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
