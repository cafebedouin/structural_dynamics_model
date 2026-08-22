% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: article_27_veto_power__coordination_reading
 *   human_readable: UN Charter Article 27(3) P5 Veto — Great-Power War Prevention Reading
 *   domain: international_relations/institutional_design/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the coordination reading of the Article 27(3) P5
 *   veto kernel: the veto is read as a necessary structural safety valve
 *   preventing the Security Council itself from becoming an institutional
 *   pathway to great-power war. Under this reading, no resolution can compel
 *   a nuclear-armed permanent member into a military confrontation it
 *   rejects, because doing so would either force submission (destabilizing to
 *   the confronted power's deterrence posture) or force defiance (destroying
 *   the Council's authority and setting a precedent for great-power conflict
 *   conducted under a veneer of collective legitimacy). This is one of three
 *   readings of the same kernel; the sibling oligopoly_reading and
 *   sovereignty_reading are separate constraint stories with their own ε and
 *   structure — this file does not average across them, hedge between them,
 *   or describe their contest internally.
 *
 * KEY AGENTS:
 *   - p5_member_states: agenda_setter/beneficiary (institutional/arbitrage) — administer and are shielded by the veto
 *   - non_p5_member_states: beneficiary (moderate/constrained) — lose specific votes but gain systemic war-avoidance
 *   - global_civilian_populations: beneficiary (powerless/trapped) — ultimate stakeholders in avoided nuclear confrontation, no procedural voice
 *   - international_system_stability: non-agent collective good — the object this reading holds is truly served
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__coordination_reading, 0.18).
domain_priors:suppression_score(article_27_veto_power__coordination_reading, 0.22).
domain_priors:theater_ratio(article_27_veto_power__coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(article_27_veto_power__coordination_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__coordination_reading, rope).
narrative_ontology:human_readable(article_27_veto_power__coordination_reading, "UN Charter Article 27(3) P5 Veto — Great-Power War Prevention Reading").
narrative_ontology:topic_domain(article_27_veto_power__coordination_reading, "international_relations/institutional_design/constitutional_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__coordination_reading, '2643f8da-c5f8-45e3-92ee-30a8eb86ba97').
narrative_ontology:cs_kernel_codification('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', formalized).
narrative_ontology:cs_authority_grounding('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', lineage).
narrative_ontology:cs_interpretation_layer_present('2643f8da-c5f8-45e3-92ee-30a8eb86ba97').
narrative_ontology:cs_reading_relation('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', article_27_veto_power__oligopoly_reading, coexists_with).
narrative_ontology:cs_reading_relation('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', article_27_veto_power__sovereignty_reading, influences).
narrative_ontology:cs_axiom('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', foundational, council_process_must_not_become_war_pathway).
narrative_ontology:cs_axiom_status(council_process_must_not_become_war_pathway, holdable).
narrative_ontology:cs_axiom_grounding('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', council_process_must_not_become_war_pathway, instrumental).
narrative_ontology:cs_axiom('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', secondary, great_power_unanimity_prevents_escalation_by_design).
narrative_ontology:cs_axiom_status(great_power_unanimity_prevents_escalation_by_design, holdable).
narrative_ontology:cs_axiom_grounding('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', great_power_unanimity_prevents_escalation_by_design, empirically_contingent).
narrative_ontology:cs_reference_frame('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', san_francisco_charter_drafting_consensus).
narrative_ontology:cs_drift_state('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', post_cold_war_multipolarity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2643f8da-c5f8-45e3-92ee-30a8eb86ba97', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__coordination_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, international_system_stability).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, non_p5_member_states).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, global_civilian_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(article_27_veto_power__coordination_reading, p5_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent seats and can block any resolution that would authorize action against their vital interests, including action that would put them in direct military confrontation. They administer the mechanism and are also its direct beneficiaries in the narrow sense that they cannot be compelled — but the coordination reading holds that the deeper beneficiary is the avoidance of a resolution that forces two nuclear powers into a Council-mandated collision.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, p5_member_states, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__coordination_reading, p5_member_states, beneficiary).

% Cannot block resolutions themselves and often see action they favor stalled by a single veto. Under this reading they are still net beneficiaries: the same mechanism that blocks resolutions they want also guarantees no Council vote can drag the system into a great-power war that would be catastrophically worse for every state, themselves included.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, non_p5_member_states, beneficiary,
    moderate, generational, constrained, global).

% Have no seat at the table and no capacity to influence Council votes, but are the ultimate beneficiaries of avoided nuclear confrontation. Their situation under the coordination reading is that a mechanism they cannot touch nonetheless holds open the single most consequential exit ramp available in the system — the ability of a nuclear state to refuse a Council-authorized path to war.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, global_civilian_populations, beneficiary,
    powerless, civilizational, trapped, universal).

% Not an actor but the collective good the coordination reading identifies as the true recipient of the mechanism's function: an international order in which no formal collective decision can force a nuclear power's hand, reducing the probability that institutional process itself becomes an escalation pathway.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, international_system_stability, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(article_27_veto_power__coordination_reading, international_system_stability).

% Administers Council procedure and records vote outcomes without power to compel or override a veto. Observes the mechanism operate as designed — as a hard stop on any resolution lacking P5 unanimity — without itself gaining or losing from any particular outcome.
narrative_ontology:constraint_stakeholder(article_27_veto_power__coordination_reading, security_council_secretariat, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(article_27_veto_power__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(article_27_veto_power__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the Security Council from becoming a formal mechanism by which a coalition of states could vote to authorize collective military action against a nuclear-armed permanent member, which would force that member either to submit to a coalition-authorized confrontation or to defy the UN Charter outright — either outcome being more destabilizing than the veto's blocking function.
% TRANSFER_FUNCTION: The mechanism does not principally transfer resources; it withholds a capability (binding collective authorization of force against a P5 state) that would otherwise exist, redistributing decision authority over great-power military engagement away from majority-vote Council process and toward unanimous P5 consent.
% ABSENT_VOICES: States and populations subject to Council action who are not P5 members have no veto and no guaranteed voice when a P5 member's interests are engaged; the coordination reading holds their absence from the blocking power is the price of the same mechanism that protects them from Council-mandated great-power war, but this is a claim, not a demonstrated trade audited from their seat.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that removing the veto would materially increase the risk that a Council majority attempts to authorize action against a nuclear state's core interests, precipitating either Charter defiance or armed confrontation with an institutional pedigree behind it. Critics (see sibling readings) hold the world would rearrange differently — toward redistributed authority rather than toward war — so the verdict is contested between readings of the same kernel, not settled within this one.
% FOUNDING_PROBLEM: The League of Nations lacked any mechanism to prevent collective security votes from being used, or threatened, against a great power, and its collapse was read by the Charter's drafters as partly a consequence of attempting collective enforcement without great-power buy-in, risking war among the powers whose cooperation the system most needed.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians of the 1945 San Francisco Conference and Charter drafters' own recorded statements (contemporaneous, prior to any P5 state's later self-interested defense of the veto) attest the founding problem was live at drafting. Non-P5 states and reform advocates at the UN General Assembly's periodic 'Uniting for Consensus' and Article 27 reform debates attest that whether this problem remains equally live for all five current holders, versus having become a rent-preserving artifact for some, is disputed — that dispute is the subject of the sibling readings, not resolved here.
narrative_ontology:disappearance_verdict(article_27_veto_power__coordination_reading, contested).
narrative_ontology:founding_problem_status(article_27_veto_power__coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(article_27_veto_power__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__coordination_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__coordination_reading_tests).
:- end_tests(article_27_veto_power__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.18) because under this reading's own lights the veto's operation does not principally transfer value from an identifiable victim class to an identifiable beneficiary class — it withholds a capability (Council-authorized coercion of a P5 state) whose exercise would be catastrophic for the whole system, P5 included. Suppression is moderate-low (0.22): the mechanism does foreclose certain Council outcomes entirely, but it does not require active policing beyond the vote itself — there is no enforcement apparatus compelling compliance beyond the formal voting rule. Theater ratio is low and drifts only slightly upward (0.10 to 0.15) reflecting the reading's own account that the mechanism has continued to perform its core function (no Council-authorized action against a P5 state's core interests has occurred) without becoming primarily performative, though the slow upward drift acknowledges post-Cold-War criticism that veto use has increasingly served narrower national interests than pure war-avoidance.
 *
 * PERSPECTIVAL GAP:
 *   From the P5 agenda-setter seat, the veto is straightforwardly protective — it prevents exactly the scenario the founding problem names. From the non-P5 beneficiary seat, the same mechanism is experienced ambivalently: specific resolutions they favor are blocked, yet this reading holds they still net-benefit from the systemic guarantee. The engine will compute these seats independently from the structural data; this reading does not assert they will compute identically, only that under its own metrics neither seat should compute as a clear extraction target.
 *
 * DIRECTIONALITY LOGIC:
 *   No victim class is declared under this reading because its central claim is that avoided great-power war benefits all states, including those who lose individual votes to a veto. P5 states are agenda_setter/beneficiary jointly — they administer the block and are shielded by it, but the deeper beneficiary named is the systemic good (international_system_stability), which is authored as a non-agent to avoid conflating a collective good with an actor capturing rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The coordination reading treats the founding problem (preventing Council process from becoming a war-authorization pathway against a great power) as substantially still live given continued nuclear multipolarity, which is why founding_problem_status is authored contested rather than dead — this reading's own position is closer to 'live,' but honest corroboration requires acknowledging that reform advocates and non-P5 states contest this. If the mandate had genuinely died (no nuclear-armed P5 state's core interests were ever again at stake in Council deliberation) while the veto persisted, this reading would expect the constraint to drift toward piton; the current low, slowly rising theater_ratio is monitored for exactly that signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_use_pattern_discriminates_readings,
    'Does the empirical historical pattern of P5 veto use — how often it has blocked action that would have precipitated great-power military confrontation, versus how often it has blocked action unrelated to great-power war risk (e.g. blocking humanitarian intervention resolutions, sanctions regimes, or membership applications) — support the coordination reading''s war-prevention account, or does it better fit the oligopoly reading''s rent-extraction account?',
    'Systematic coding of all Security Council vetoes since 1946 by subject matter and by whether the blocked resolution, if passed, would plausibly have created a direct military-confrontation pathway against the vetoing P5 state versus an unrelated policy matter (trade, membership, human rights, third-party conflicts not involving the vetoing power directly).',
    'If the large majority of vetoes concern matters unrelated to direct great-power war risk (protecting allies, blocking unrelated Council action, shielding the vetoing state''s own conduct in third-country conflicts), the coordination reading''s ε of 0.18 substantially understates the mechanism''s actual extractive function and the oligopoly reading becomes the better-fitting account of the same formal mechanism. If a large share concerns genuine great-power confrontation avoidance, this reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_use_pattern_discriminates_readings, empirical, 'Whether historical veto-use patterns support the war-prevention function this reading claims versus the rent-extraction function the sibling oligopoly reading claims.').

omega_variable(
    kernel_framing_choice_between_readings,
    'Is the coordination reading the correct primary framing for the Article 27(3) veto, or is it a legitimating narrative layered over a structure whose primary function (as the oligopoly reading holds) is entrenching P5 authority against institutional evolution — with war-prevention as a genuine but secondary effect?',
    'Compare Charter drafting history (which emphasizes war-prevention explicitly in San Francisco Conference records) against the amendment-immutability structure (Article 108/109''s own supermajority-plus-P5-unanimity requirement for Charter amendment, which the oligopoly reading identifies as evidence the mechanism was also designed to be self-entrenching).',
    'If the drafting record and amendment-immutability structure are read as two faces of one design choice, all three readings may be simultaneously true at different structural levels rather than competing single-truth accounts — this would argue for treating the kernel as genuinely multi-stable rather than adjudicable to one dominant reading. This omega documents that the choice of THIS reading as the primary lens is itself contestable, not resolved by the metrics authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_between_readings, conceptual, 'Whether the coordination reading is separable from, or is a legitimating gloss on, the oligopoly reading of the identical formal mechanism.').

omega_variable(
    nuclear_confrontation_counterfactual,
    'In the counterfactual world without the veto, would Security Council majorities have actually attempted to authorize military action against a P5 nuclear state''s core interests, or would other checks (nuclear deterrence itself, bilateral diplomacy, non-Council mechanisms) have prevented such confrontation regardless of the veto''s existence?',
    'Historical and game-theoretic analysis of whether any non-P5 Council majority has, in fact, come close to attempting authorization of action directly confronting a P5 nuclear state militarily, absent the veto as a backstop — i.e., whether the veto has ever been the operative constraint versus a redundant one given deterrence.',
    'If deterrence alone would have prevented such votes from ever reaching the point of passage, the veto''s war-prevention function claimed by this reading is largely redundant with nuclear deterrence, and the ε attributed to genuine coordination function should be lower still, with more of the mechanism''s real-world effect better explained by the sibling readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nuclear_confrontation_counterfactual, empirical, 'Whether the veto is doing genuine causal work in preventing great-power war or is redundant with nuclear deterrence''s own restraining effect.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__coordination_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_27_veto_power__coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(arti_tr_t16, article_27_veto_power__coordination_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(arti_tr_t32, article_27_veto_power__coordination_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(arti_tr_t48, article_27_veto_power__coordination_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement(arti_tr_t64, article_27_veto_power__coordination_reading, theater_ratio, 64, 0.14).
narrative_ontology:measurement(arti_tr_t80, article_27_veto_power__coordination_reading, theater_ratio, 80, 0.15).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_27_veto_power__coordination_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(arti_be_t16, article_27_veto_power__coordination_reading, base_extractiveness, 16, 0.15).
narrative_ontology:measurement(arti_be_t32, article_27_veto_power__coordination_reading, base_extractiveness, 32, 0.16).
narrative_ontology:measurement(arti_be_t48, article_27_veto_power__coordination_reading, base_extractiveness, 48, 0.17).
narrative_ontology:measurement(arti_be_t64, article_27_veto_power__coordination_reading, base_extractiveness, 64, 0.18).
narrative_ontology:measurement(arti_be_t80, article_27_veto_power__coordination_reading, base_extractiveness, 80, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(article_27_veto_power__coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(article_27_veto_power__coordination_reading, 0.12).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__oligopoly_reading).
narrative_ontology:affects_constraint(article_27_veto_power__coordination_reading, article_27_veto_power__sovereignty_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the article_27_veto_power kernel, decomposed per the ε-invariance principle because the natural-language concept 'the P5 veto' conflates structurally distinct claims with distinct ε values: the coordination reading (this file, ε=0.18, Rope, no victim class), the oligopoly reading (ε expected substantially higher, Tangled Rope or Snare, victims = states/populations excluded from Council authority redistribution), and the sovereignty reading (ε expected low-moderate, framed around consent rather than coordination or extraction, likely Rope or Mountain-adjacent depending on how consent-based international law is treated). All three share the identical Charter text (Article 27(3)) and identical formal voting mechanism; they diverge entirely on beneficiary/victim structure and coordination-vs-extraction characterization. Each is linked to the other two via affects_constraints rather than merged into a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
