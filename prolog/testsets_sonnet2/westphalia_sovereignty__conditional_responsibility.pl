% ============================================================================
% CONSTRAINT STORY: westphalia_sovereignty__conditional_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalia_sovereignty__conditional_responsibility, []).

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
 *   constraint_id: westphalia_sovereignty__conditional_responsibility
 *   human_readable: Responsibility to Protect (R2P) — Conditional Sovereignty Doctrine
 *   domain: international_law/political_theory/state_systems
 *
 * SUMMARY:
 *   This story instantiates the conditional_responsibility reading of the
 *   Westphalian sovereignty kernel: the doctrine, formalized as
 *   Responsibility to Protect (R2P) after the 2005 World Summit, holds that
 *   territorial inviolability is not absolute but conditional on a state
 *   adequately protecting its population from genocide, war crimes, ethnic
 *   cleansing, and crimes against humanity. Failure to meet that
 *   responsibility transfers a residual protective duty to the international
 *   community, which gains adjudicative and potentially interventionist
 *   authority. This reading is authored on its own terms — the standing
 *   arrangement it describes is R2P as actually practiced (selective,
 *   veto-gated, coalition-administered), not an idealized
 *   universal-protection regime.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, 0.58).
domain_priors:suppression_score(westphalia_sovereignty__conditional_responsibility, 0.62).
domain_priors:theater_ratio(westphalia_sovereignty__conditional_responsibility, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(westphalia_sovereignty__conditional_responsibility, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalia_sovereignty__conditional_responsibility, tangled_rope).
narrative_ontology:human_readable(westphalia_sovereignty__conditional_responsibility, "Responsibility to Protect (R2P) — Conditional Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalia_sovereignty__conditional_responsibility, "international_law/political_theory/state_systems").

domain_priors:requires_active_enforcement(westphalia_sovereignty__conditional_responsibility).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalia_sovereignty__conditional_responsibility, '2016001c-d95a-4763-9389-3ec0977d6838').
narrative_ontology:cs_kernel_codification('2016001c-d95a-4763-9389-3ec0977d6838', distributed).
narrative_ontology:cs_authority_grounding('2016001c-d95a-4763-9389-3ec0977d6838', distributed).
narrative_ontology:cs_reading_relation('2016001c-d95a-4763-9389-3ec0977d6838', westphalia_sovereignty__absolute_non_intervention, forecloses).
narrative_ontology:cs_reading_relation('2016001c-d95a-4763-9389-3ec0977d6838', westphalia_sovereignty__graded_sovereignty, influences).
narrative_ontology:cs_axiom('2016001c-d95a-4763-9389-3ec0977d6838', foundational, sovereignty_is_forfeitable_upon_atrocity_failure).
narrative_ontology:cs_axiom_status(sovereignty_is_forfeitable_upon_atrocity_failure, holdable).
narrative_ontology:cs_axiom_grounding('2016001c-d95a-4763-9389-3ec0977d6838', sovereignty_is_forfeitable_upon_atrocity_failure, conventional).
narrative_ontology:cs_axiom('2016001c-d95a-4763-9389-3ec0977d6838', secondary, international_community_holds_residual_protective_duty).
narrative_ontology:cs_axiom_status(international_community_holds_residual_protective_duty, holdable).
narrative_ontology:cs_axiom_grounding('2016001c-d95a-4763-9389-3ec0977d6838', international_community_holds_residual_protective_duty, instrumental).
narrative_ontology:cs_reference_frame('2016001c-d95a-4763-9389-3ec0977d6838', pre_r2p_absolute_westphalian_order).
narrative_ontology:cs_drift_state('2016001c-d95a-4763-9389-3ec0977d6838', post_libya_2011_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2016001c-d95a-4763-9389-3ec0977d6838', '').
narrative_ontology:cs_kernel_id(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, global_governance_institutions).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, targeted_state_governments).
narrative_ontology:constraint_victim(westphalia_sovereignty__conditional_responsibility, weak_states_without_veto_shielded_allies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, responsibility_to_protect_doctrine).
narrative_ontology:constraint_vindicates(westphalia_sovereignty__conditional_responsibility, individual_criminal_accountability_for_atrocity_crimes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Civilians facing genocide, ethnic cleansing, war crimes, or crimes against humanity by their own state or by armed factions within it. The doctrine exists in their name — its entire coordination justification is protecting them. But intervention is inconsistently triggered, often arrives too late or not at all where no major power has interest, and when it does arrive can produce prolonged conflict, occupation, or destabilization that makes their situation worse. They bear the costs of both action and inaction without any vote in either.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, populations_under_atrocity_regimes, payer).

% Governments accused of atrocity conduct, whose territorial inviolability is suspended once the international community deems the threshold crossed. They lose the Westphalian shield that every other recognized state still enjoys, face sanctions, arms embargoes, ICC referral, or armed intervention, and have no symmetric procedure to contest the atrocity determination before losing sovereign protection.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, targeted_state_governments, payer,
    moderate, biographical, trapped, national).

% Ad hoc or standing coalitions of intervening states that invoke the doctrine to authorize action inside another state's territory. They select which atrocities cross the threshold, assemble the coalition, and conduct the intervention — gaining strategic access, resource positioning, or regional influence as a byproduct of the humanitarian mandate, with no external body auditing whether their selection of cases tracks atrocity severity or tracks their own interests.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, agenda_setter,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions, beneficiary).

% The five veto-holding states control whether the Security Council authorizes intervention under the doctrine. They and their close allies are functionally exempt from having the doctrine invoked against them regardless of conduct, because any resolution can be vetoed — the conditional-sovereignty rule binds every state except the ones that write and enforce it.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, un_security_council_permanent_members, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The UN system, ICC, and affiliated bodies gain adjudicative authority and institutional relevance from the doctrine's existence — it is the legal hook that lets them assert jurisdiction over what would otherwise be purely domestic conduct. Their continued authority and funding depend partly on the doctrine remaining invoked and contested rather than settled.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, global_governance_institutions, beneficiary,
    institutional, civilizational, analytical, global).

% States with internal instability but no permanent-member patron are structurally exposed to intervention in a way that similarly-conducted states with a shielding ally are not. Their sovereignty is conditional in practice; a shielded state's sovereignty is conditional only on paper.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, weak_states_without_veto_shielded_allies, payer,
    powerless, generational, trapped, national).

% States and blocs (many post-colonial states, several regional organizations) that hold the sovereignty-as-inviolable reading would object that the conditional-responsibility framework legitimizes selective great-power intervention under humanitarian language. They participate in UN votes but the doctrine's adjudicative machinery is built and administered largely without their design input.
narrative_ontology:constraint_stakeholder(westphalia_sovereignty__conditional_responsibility, absolute_non_intervention_states, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalia_sovereignty__conditional_responsibility, humanitarian_intervention_coalitions).
narrative_ontology:fixing_cost_class(westphalia_sovereignty__conditional_responsibility, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international standard for recognizing when mass atrocity conduct is severe enough that the normal presumption against external interference in a state's internal affairs should lift, allowing coordinated response instead of each state acting on unilateral, ad hoc justification.
% TRANSFER_FUNCTION: Moves adjudicative authority over a state's internal conduct from that state to the international community (in practice, to coalitions capable of assembling political and military will), and moves the cost of that determination's application asymmetrically onto states without veto-holding protectors.
% ABSENT_VOICES: States holding the absolute non-intervention reading, and the atrocity-affected populations themselves, are structurally absent from the threshold-setting process — the determination of when sovereignty lapses is made by Security Council members and intervening coalitions, not by the people the doctrine is invoked to protect or by symmetric procedure available to the accused state.
% DISAPPEARANCE_RATIONALE: If the conditional-responsibility reading vanished overnight, the legal basis currently cited for interventions like Libya 2011 or the ICC's atrocity jurisdiction claims would lose its doctrinal anchor; states facing internal crises would revert to the pure non-intervention default, altering both when interventions are attempted and what legal cover exists for refusing to intervene.
% FOUNDING_PROBLEM: Built to resolve the post-Cold War and post-Rwanda/Srebrenica problem of the international system having no legitimate mechanism to act against mass atrocity when the perpetrating state was the recognized sovereign of the territory — inaction in Rwanda 1994 was the proximate trigger for the 2005 World Summit's R2P adoption.
% FOUNDING_PROBLEM_CORROBORATION: UN member states adopted R2P by consensus at the 2005 World Summit, and independent scholars of humanitarian law and several UN Secretary-General reports attest the atrocity-prevention gap the doctrine targets remains partly live. But independent critics — including scholars from non-Western international law traditions, African Union commentary following Libya 2011, and BRICS-aligned diplomatic statements — attest from outside the intervening coalitions that the doctrine's actual application tracks great-power interest more than atrocity severity, making its founding problem only partially addressed and its selective enforcement the operative reality.
narrative_ontology:disappearance_verdict(westphalia_sovereignty__conditional_responsibility, world_rearranges).
narrative_ontology:founding_problem_status(westphalia_sovereignty__conditional_responsibility, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalia_sovereignty__conditional_responsibility, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalia_sovereignty__conditional_responsibility, 'none', 1).
narrative_ontology:epsilon_provenance(westphalia_sovereignty__conditional_responsibility, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalia_sovereignty__conditional_responsibility_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalia_sovereignty__conditional_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalia_sovereignty__conditional_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rose from 0.32 (1990, pre-Rwanda, doctrine barely theorized) to 0.58 by 2011 (post-Libya, the high-water mark of interventionist application) and has plateaued since as the doctrine's invocation became more visibly correlated with great-power interest than atrocity severity. Suppression (0.62) reflects both the coercive force applied to targeted states once the threshold is invoked and the veto mechanism that suppresses the doctrine's use against permanent-member-shielded states — suppression here operates in two directions at once. Theater ratio (0.42) captures the growing gap between the doctrine's protective rhetoric and its actual invocation pattern: Syria, Yemen, and Xinjiang show the doctrine's atrocity-recognition machinery running (UN reports, special rapporteurs, resolutions) without triggering intervention, i.e., performative process without the substantive protective outcome the doctrine promises.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a humanitarian coalition or the UN system, this is a genuine coordination achievement solving the Rwanda/Srebrenica problem of legal paralysis in the face of atrocity. From the seat of a targeted government or an unshielded weak state, the same structure operates as an asymmetric extraction of sovereign protection that is available to some states and not others based on alliance structure rather than conduct. The engine should compute a tangled-rope classification at the aggregate level precisely because both readings are structurally correct from their respective seats — the coordination function is real (atrocity prevention has some legal hook now) and the extraction is real (enforcement tracks power, not atrocity severity).
 *
 * DIRECTIONALITY LOGIC:
 *   Populations under atrocity regimes are declared both beneficiary (the doctrine's stated purpose) and payer (they bear the costs of inconsistent or absent enforcement, and sometimes of intervention itself) — this dual role is intentional and reflects the doctrine's actual structure, not an error. Humanitarian intervention coalitions and Security Council permanent members are structural beneficiaries: they hold agenda-setting power with arbitrage-grade exit (they choose when to invoke the doctrine and are never subject to it themselves via the veto). Targeted state governments and veto-unshielded weak states are targets: their sovereignty becomes genuinely conditional while permanent members' sovereignty remains absolute in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legal paralysis against atrocity) is only partially resolved — some invocations (Libya 2011, ICC referrals) show the mechanism functioning; many others (Syria, Myanmar, Xinjiang) show it stalling at the same veto point that predated R2P. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine, if imperfect, coordination achievement as pure extraction; classifying it as tangled_rope rather than rope prevents treating veto-driven selectivity as incidental noise rather than as the constitutive asymmetry it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    atrocity_threshold_objectivity,
    'Is the atrocity threshold that triggers sovereignty forfeiture an objective, consistently-applied legal standard, or is it a post-hoc justification selectively invoked to authorize interventions already desired for other strategic reasons?',
    'Comparative case analysis of atrocity severity metrics (casualty counts, documented crimes against humanity findings, UN Commission of Inquiry reports) against actual intervention/non-intervention decisions, controlling for the intervening or vetoing states'' independent strategic interests in each case.',
    'If threshold application correlates strongly with atrocity severity independent of great-power interest, this reading is closer to genuine rope; if it correlates strongly with strategic interest and only weakly with severity, the coordination story is substantially cover for extraction, pushing toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atrocity_threshold_objectivity, empirical, 'Whether the R2P threshold tracks atrocity severity or great-power interest.').

omega_variable(
    veto_shield_structural_necessity,
    'Is permanent-member veto immunity from the doctrine a structural necessity of the UN Charter system (without which no doctrine could be adopted at all), or is it a designed asymmetry that makes the entire conditional-sovereignty framework selectively enforceable by construction?',
    'Historical analysis of the 2005 World Summit negotiation record and subsequent Charter reform proposals to determine whether veto-exempt application was a known, accepted design feature or an unintended consequence tolerated for lack of alternative.',
    'If structurally necessary, the asymmetry is a coordination cost rather than pure extraction; if it was a known, accepted design choice serving permanent-member interest, the tangled_rope classification understates the extractive component relative to a reading that would weight this more heavily toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_shield_structural_necessity, conceptual, 'Whether veto immunity is inherent to UN coordination or a designed extraction feature.').

omega_variable(
    reading_framing_underdetermination,
    'Does the conditional_responsibility reading and the graded_sovereignty reading actually pick out different legal claims, or are they two descriptions of the same practice at different levels of abstraction (binary-conditional as the legal fiction, scalar-capacity as the sociological reality it operationalizes)?',
    'Doctrinal comparison of ICJ and ICC jurisprudence: does case law treat sovereignty forfeiture as a discrete legal event (supports binary-conditional) or as admitting of degree (supports graded)?',
    'If the two readings converge doctrinally, they should be merged or the network edge between them strengthened to influences rather than treated as fully independent; if they diverge in actual case outcomes, they remain properly separate constraints.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_framing_underdetermination, conceptual, 'Whether the binary-conditional and scalar-capacity readings are genuinely distinct in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalia_sovereignty__conditional_responsibility, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(west_tr_t1999, westphalia_sovereignty__conditional_responsibility, theater_ratio, 1999, 0.28).
narrative_ontology:measurement(west_tr_t2005, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2005, 0.3).
narrative_ontology:measurement(west_tr_t2011, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2011, 0.34).
narrative_ontology:measurement(west_tr_t2016, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2016, 0.4).
narrative_ontology:measurement(west_tr_t2024, westphalia_sovereignty__conditional_responsibility, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(west_be_t1999, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 1999, 0.4).
narrative_ontology:measurement(west_be_t2005, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2005, 0.46).
narrative_ontology:measurement(west_be_t2011, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2011, 0.55).
narrative_ontology:measurement(west_be_t2016, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2016, 0.58).
narrative_ontology:measurement(west_be_t2024, westphalia_sovereignty__conditional_responsibility, base_extractiveness, 2024, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(west_su_t1999, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 1999, 0.48).
narrative_ontology:measurement(west_su_t2005, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2005, 0.52).
narrative_ontology:measurement(west_su_t2011, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2011, 0.6).
narrative_ontology:measurement(west_su_t2016, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(west_su_t2024, westphalia_sovereignty__conditional_responsibility, suppression_requirement, 2024, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalia_sovereignty__conditional_responsibility, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__absolute_non_intervention).
narrative_ontology:affects_constraint(westphalia_sovereignty__conditional_responsibility, westphalia_sovereignty__graded_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalia_sovereignty kernel. absolute_non_intervention holds sovereignty is categorically inviolable regardless of internal conduct (ε near zero for the standing pre-R2P Westphalian order under that reading's own lights). conditional_responsibility (this story) holds sovereignty is binary-conditional on atrocity-prevention performance, with ε = 0.58 reflecting substantial selective-enforcement extraction. graded_sovereignty holds sovereignty is continuously scalar with capacity, producing a different victim set (weak/failed states generally, not only atrocity-committing states) and a different extraction profile. The three are not the same constraint measured differently — each authors a distinct beneficiary/victim structure and a distinct ε, linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
