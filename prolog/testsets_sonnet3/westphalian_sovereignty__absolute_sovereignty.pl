% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__absolute_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__absolute_sovereignty, []).

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
 *   constraint_id: westphalian_sovereignty__absolute_sovereignty
 *   human_readable: Absolute Sovereignty Reading: Unconditional Non-Interference Norm
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This story instantiates the absolute-sovereignty reading of the contested
 *   Westphalian sovereignty kernel: the claim that sovereignty confers
 *   unconditional authority over domestic affairs and that any external
 *   interference is categorically illegitimate, regardless of what occurs
 *   within the state's borders. This is NOT the conditional-sovereignty
 *   reading (which ties legitimacy to human-rights performance) nor the
 *   graduated-sovereignty reading (which scales authority to state capacity)
 *   — those are separate constraints with their own ε and stakeholder sets,
 *   linked here only via network edges and the cs_structure.reading_relations
 *   block. Under this reading's own lights, the standing arrangement is the
 *   near-universal diplomatic and legal deference to the non-interference
 *   norm as applied in UN Security Council practice, treaty interpretation,
 *   and customary international law since 1945.
 *
 * KEY AGENTS:
 *   - authoritarian_state_apparatuses: Primary beneficiary (institutional/arbitrage) — uses the shield to block accountability
 *   - veto_wielding_security_council_members: Selective enforcer (institutional/arbitrage) — invokes or waives the norm strategically
 *   - domestic_populations_under_repression: Primary victim (powerless/trapped) — bears the cost of foreclosed external remedy
 *   - international_law_scholars: Analytical observer — traces the norm's dual defensive/abusive application across cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, 0.55).
domain_priors:suppression_score(westphalian_sovereignty__absolute_sovereignty, 0.68).
domain_priors:theater_ratio(westphalian_sovereignty__absolute_sovereignty, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, extractiveness, 0.55).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(westphalian_sovereignty__absolute_sovereignty, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__absolute_sovereignty, tangled_rope).
narrative_ontology:human_readable(westphalian_sovereignty__absolute_sovereignty, "Absolute Sovereignty Reading: Unconditional Non-Interference Norm").
narrative_ontology:topic_domain(westphalian_sovereignty__absolute_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__absolute_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__absolute_sovereignty, 'f9a57402-b308-4344-93bb-01713a6c29c0').
narrative_ontology:cs_kernel_codification('f9a57402-b308-4344-93bb-01713a6c29c0', formalized).
narrative_ontology:cs_authority_grounding('f9a57402-b308-4344-93bb-01713a6c29c0', practice).
narrative_ontology:cs_interpretation_layer_present('f9a57402-b308-4344-93bb-01713a6c29c0').
narrative_ontology:cs_reading_relation('f9a57402-b308-4344-93bb-01713a6c29c0', westphalian_sovereignty__conditional_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('f9a57402-b308-4344-93bb-01713a6c29c0', westphalian_sovereignty__graduated_sovereignty, forecloses).
narrative_ontology:cs_axiom('f9a57402-b308-4344-93bb-01713a6c29c0', foundational, sovereignty_is_unconditional_on_internal_conduct).
narrative_ontology:cs_axiom_status(sovereignty_is_unconditional_on_internal_conduct, holdable).
narrative_ontology:cs_axiom_grounding('f9a57402-b308-4344-93bb-01713a6c29c0', sovereignty_is_unconditional_on_internal_conduct, conventional).
narrative_ontology:cs_axiom('f9a57402-b308-4344-93bb-01713a6c29c0', foundational, external_interference_is_categorically_illegitimate_regardless_of_cause).
narrative_ontology:cs_axiom_status(external_interference_is_categorically_illegitimate_regardless_of_cause, holdable).
narrative_ontology:cs_axiom_grounding('f9a57402-b308-4344-93bb-01713a6c29c0', external_interference_is_categorically_illegitimate_regardless_of_cause, conventional).
narrative_ontology:cs_reference_frame('f9a57402-b308-4344-93bb-01713a6c29c0', post_westphalian_non_interference_baseline).
narrative_ontology:cs_drift_state('f9a57402-b308-4344-93bb-01713a6c29c0', post_cold_war_r2p_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f9a57402-b308-4344-93bb-01713a6c29c0', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_apparatuses).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, ruling_elites_of_weak_states).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__absolute_sovereignty, veto_wielding_security_council_members).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities).
narrative_ontology:constraint_victim(westphalian_sovereignty__absolute_sovereignty, victims_of_state_mass_atrocity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invokes the absolute non-interference norm to shield internal repression, mass detention, ethnic cleansing, or electoral fraud from external response. Cites the norm in UN forums to block resolutions, sanctions, or investigative mandates targeting its own conduct. Faces essentially no structural cost from invoking the norm and substantial benefit in continued unchallenged rule.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_apparatuses, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, authoritarian_state_apparatuses, agenda_setter).

% Uses sovereignty claims defensively against stronger neighbors and former colonial powers, and offensively to insulate governance failures, corruption, or crackdowns on dissent from scrutiny. Benefits from the norm's symmetry: the same shield that protects against neo-imperial intervention also protects against accountability for domestic abuse.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, ruling_elites_of_weak_states, beneficiary,
    moderate, biographical, mobile, national).

% Selectively enforces or waives the absolute-sovereignty reading depending on whether intervention serves its own strategic interests, while relying on the norm's rhetorical force to block interventions against itself or its clients. Sets the practical boundaries of when the norm is invoked versus overridden.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, veto_wielding_security_council_members, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__absolute_sovereignty, veto_wielding_security_council_members, agenda_setter).

% Lives under the governance the sovereignty shield protects from external scrutiny or intervention. Cannot appeal to any body above the state because the norm categorically forecloses that appeal as illegitimate interference. Exit is typically unavailable — flight is dangerous, internal dissent is suppressed by the same state the norm insulates.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, domestic_populations_under_repression, payer,
    powerless, immediate, trapped, local).

% Bears the concentrated cost when the sovereignty shield is invoked to block outside response to targeted violence, forced assimilation, or discriminatory law, precisely because international bodies treat the state's domestic-affairs claim as dispositive.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, ethnic_and_religious_minorities, payer,
    powerless, generational, trapped, local).

% Suffer direct physical harm during episodes where the absolute-sovereignty reading is cited to delay, dilute, or block intervention (peacekeeping mandates, humanitarian corridors, tribunal referrals) until harm is largely irreversible.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, victims_of_state_mass_atrocity, payer,
    powerless, immediate, trapped, local).

% Documents violations and advocates for intervention but has no standing to compel action; the norm treats their findings as, at most, persuasive input that the invoking state can dismiss as interference in its internal affairs.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, human_rights_monitoring_bodies, excluded,
    organized, generational, constrained, global).

% Analyzes how the absolute-sovereignty reading of the Westphalian norm coexists in tension with the UN Charter's human rights provisions and the post-1990s responsibility-to-protect doctrine, tracing which invoking pattern dominates in specific cases.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__absolute_sovereignty, international_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__absolute_sovereignty, diffuse).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__absolute_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable baseline rule preventing arbitrary cross-border military and political intervention, which genuinely reduces great-power war risk and protects newly independent and weaker states from renewed colonial-style domination.
% TRANSFER_FUNCTION: Moves protection from external scrutiny and accountability to the state apparatus currently in power, at the cost of remedy, protection, and voice for populations that apparatus is actively harming.
% ABSENT_VOICES: The populations living under the regimes invoking the shield are structurally absent from the forums (UN Security Council, bilateral diplomacy) where the norm's application is decided; human rights bodies that document their situation have no vote and no standing, only advisory input a state can reject as interference.
% DISAPPEARANCE_RATIONALE: If the absolute non-interference norm vanished overnight, the entire architecture of what counts as a legitimate international response to internal state conduct would restructure — sanctions regimes, humanitarian intervention doctrine, UN Charter interpretation, and the diplomatic cover currently available to repressive governments would all shift; both defensive uses (protecting weak states from domination) and abusive uses (shielding atrocity) would end simultaneously.
% FOUNDING_PROBLEM: Post-1648 Europe needed to end centuries of wars fought partly over the right to impose religious and political order on neighboring territories; a firm non-interference baseline stopped states from treating each other's internal governance as a legitimate casus belli.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the Peace of Westphalia and mainstream international-relations realists attest the original problem (interstate war over internal religious/political order) is substantially resolved among major powers. Human rights monitoring bodies, genocide scholars, and R2P advocates — outside the beneficiary set of invoking states — attest that the norm's absolute reading has been redirected from preventing interstate war to shielding intrastate atrocity, a use the founding problem never contemplated.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__absolute_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__absolute_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__absolute_sovereignty, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(westphalian_sovereignty__absolute_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__absolute_sovereignty, 0.55, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__absolute_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__absolute_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__absolute_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.55, within the expected 0.45-0.60 band for this reading: the norm genuinely coordinates a real interstate problem (preventing pretextual invasion dressed as moral concern) but simultaneously and inseparably shields governments actively harming their own populations from any external check. Suppression (0.68) reflects that the norm's persistence depends on active diplomatic and legal enforcement — states must continually reassert non-interference in Security Council debate, treaty negotiation, and customary practice, and dissenting voices (human rights bodies, victim populations) are structurally locked out of the fora where the norm's application is decided. Theater ratio rises over the interval (0.20 to 0.40) as the gap between the norm's stated coordination purpose (preventing great-power domination) and its dominant contemporary use (shielding atrocity from accountability) widens post-Cold War, when interstate war among great powers became rarer even as the norm's atrocity-shielding invocations became more visible and contested.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a state invoking the norm, the arrangement is straightforward coordination — respecting borders, avoiding war, honoring self-determination. From the seat of a population under that state's repression, the identical structure operates as a categorical denial of any external recourse. The engine computes these as different seat-level classifications from the same structural data; the divergence is the finding, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian state apparatuses and veto-wielding Security Council members are coded as beneficiaries with high power and mobile-to-arbitrage exit: they invoke the norm when convenient and are never structurally bound by its costs. Domestic populations under repression, minorities, and atrocity victims are coded as victims with powerless status and trapped exit: the norm's entire function, from their position, is to ensure no external actor can legitimately intervene on their behalf, regardless of what is done to them. This is the asymmetry the tangled_rope classification requires: real coordination function (preventing interstate war) plus asymmetric extraction (domestic populations pay through the same structure that protects interstate peace) plus active enforcement (the norm must be continually reasserted against R2P-style challengers).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing interstate war over internal religious/political order) is substantially resolved among major powers, yet the absolute reading of the norm persists at full strength and has been redirected toward a use — shielding intrastate atrocity — the founding framework never addressed. This is a mandatrophy candidate: the mandate (categorical non-interference) has partially outlived its founding function while retaining full enforcement force, now serving a different beneficiary set (incumbent repressive regimes) than the one that justified its creation (post-Westphalian European states seeking to avoid religious wars).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_inseparability,
    'Is the interstate-peace coordination function of absolute sovereignty separable from its atrocity-shielding function, or are they the same mechanism operating in two contexts?',
    'Comparative analysis of cases where the norm was invoked defensively (blocking genuine imperial aggression) versus offensively (blocking intervention against domestic mass atrocity) — if the same legal and rhetorical mechanism produces both outcomes with no structural distinction available ex ante, the functions are inseparable.',
    'If inseparable, no reform can preserve the coordination benefit while eliminating the extraction — the reading would have to be replaced wholesale by conditional or graduated sovereignty rather than reformed internally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_inseparability, conceptual, 'Whether the peace-preserving and atrocity-shielding functions of absolute sovereignty are mechanically separable.').

omega_variable(
    selective_invocation_pattern,
    'Is the absolute-sovereignty reading applied consistently, or do powerful states invoke it against themselves rarely while enforcing it strictly against weaker states (i.e., is the norm itself power-asymmetric in application even though it is framed as universal)?',
    'Empirical review of Security Council voting and intervention patterns 1945-2025, coded by whether the target state was a P5 member, ally of a P5 member, or neither.',
    'If invocation is systematically asymmetric, the ''categorical'' framing is itself part of the extraction — the norm functions as a shield available differentially to states with veto-power patronage, not as a neutral rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selective_invocation_pattern, empirical, 'Whether the categorical framing masks asymmetric real-world application favoring powerful states and their clients.').

omega_variable(
    kernel_framing_choice,
    'Is the choice to treat ''absolute sovereignty'' as a distinct reading (rather than as the default/residual state when conditional or graduated criteria are simply unmet) itself a framing decision that affects classification?',
    'Compare classification outcomes if absolute sovereignty were instead modeled as the null hypothesis (no criteria for intervention exist) versus as an affirmative doctrine (interference is categorically barred) — the affirmative framing used here treats non-interference as an actively asserted claim, which is what generates the tangled_rope reading; a residual-state framing might read closer to a pure default with lower authored suppression.',
    'Under the residual-state framing, suppression might be authored lower (nothing is being actively defended, it is merely the absence of an alternative rule) and the classification could shift toward piton (inertial rather than actively defended). This story adopts the affirmative-doctrine framing because states demonstrably invoke and litigate the norm rather than merely defaulting to it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice, conceptual, 'Whether treating absolute sovereignty as an affirmatively asserted doctrine versus a residual default changes the suppression measurement and resulting classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__absolute_sovereignty, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1945, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(west_tr_t1960, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(west_tr_t1975, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1975, 0.3).
narrative_ontology:measurement(west_tr_t1994, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 1994, 0.28).
narrative_ontology:measurement(west_tr_t2005, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(west_tr_t2015, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(west_tr_t2025, westphalian_sovereignty__absolute_sovereignty, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(west_be_t1945, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1945, 0.35).
narrative_ontology:measurement(west_be_t1960, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(west_be_t1975, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1975, 0.45).
narrative_ontology:measurement(west_be_t1994, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 1994, 0.5).
narrative_ontology:measurement(west_be_t2005, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2005, 0.48).
narrative_ontology:measurement(west_be_t2015, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(west_be_t2025, westphalian_sovereignty__absolute_sovereignty, base_extractiveness, 2025, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1945, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1945, 0.5).
narrative_ontology:measurement(west_su_t1960, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1960, 0.55).
narrative_ontology:measurement(west_su_t1975, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1975, 0.58).
narrative_ontology:measurement(west_su_t1994, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 1994, 0.6).
narrative_ontology:measurement(west_su_t2005, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2005, 0.62).
narrative_ontology:measurement(west_su_t2015, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(west_su_t2025, westphalian_sovereignty__absolute_sovereignty, suppression_requirement, 2025, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__absolute_sovereignty, enforcement_mechanism).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__conditional_sovereignty).
narrative_ontology:affects_constraint(westphalian_sovereignty__absolute_sovereignty, westphalian_sovereignty__graduated_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the westphalian_sovereignty kernel, decomposed per the ε-invariance principle: absolute_sovereignty (this story, tangled_rope, ε=0.55), conditional_sovereignty (rights-conditioned intervention legitimacy), and graduated_sovereignty (capacity/legitimacy-scaled authority). Each reading has its own ε, beneficiary/victim structure, and classification because measuring 'sovereignty' under each reading's own operative rule yields structurally different extraction profiles — they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
