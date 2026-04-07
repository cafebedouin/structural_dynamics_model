% ============================================================================
% CONSTRAINT STORY: sotu_2002_bush_unilateral_military_intervention_doctrine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_2002_bush_unilateral_military_intervention_doctrine, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_2002_bush_unilateral_military_intervention_doctrine
 *   human_readable: Unilateral U.S. Military Intervention Doctrine (SOTU 2002)
 *   domain: foreign_policy/security_doctrine
 *
 * SUMMARY:
 *   The 2002 State of the Union address articulates a structural constraint
 *   on military decision-making authority: the United States reserves the
 *   right to conduct unilateral military operations against terrorist groups
 *   when other nations are deemed 'timid in the face of terror' and fail to
 *   act independently. This doctrine transfers security decision-making from
 *   multilateral consensus (requiring UN Security Council authorization or
 *   treaty alliance coordination) to unilateral U.S. executive judgment. The
 *   constraint exhibits mixed characteristics: it solves a genuine
 *   coordination problem (identifying and acting against transnational
 *   terrorist networks) while simultaneously concentrating extraction
 *   authority in the U.S. executive, imposing sovereignty costs on target
 *   nations, and suspending international legal constraints on first-strike
 *   military intervention. The doctrine's theater_ratio reflects the
 *   sophisticated legal arguments for preemption doctrine (based on
 *   self-defense interpretation of UN Charter Article 51) alongside the
 *   performative nature of these arguments — they rationalize what is
 *   substantively an extraordinary power claim. Extractiveness rises over the
 *   interval as the doctrine is invoked repeatedly, establishing precedent
 *   and normalizing the expansion of unilateral action authority beyond the
 *   original counter-terrorism framing.
 *
 * KEY AGENTS:
 *   - U.S. Executive Branch: Primary beneficiary (institutional/arbitrage) — gains unilateral authority to intervene without coalition consensus; retains strategic flexibility
 *   - Target Nations: Primary victim (powerless/trapped) — face unilateral military intervention threat with no meaningful exit or appeal mechanism; sovereign authority stripped
 *   - Coalition Partner Nations: Secondary victim (moderate/constrained) — face pressure to cooperate or risk isolation; mixed extraction from burden-sharing pressure
 *   - International Legal Constraint Regime: Partial victim (organized/mobile) — UN Charter Article 51 constraints are reinterpreted; multilateral consensus mechanism is bypassed
 *   - United Nations System: Institutional degradation (institutional/constrained) — persists as legitimacy theater while functional authority migrates to U.S. unilateral judgment
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent doctrinal choice as inevitable consequence of international anarchy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_2002_bush_unilateral_military_intervention_doctrine, 0.58).
domain_priors:suppression_score(sotu_2002_bush_unilateral_military_intervention_doctrine, 0.72).
domain_priors:theater_ratio(sotu_2002_bush_unilateral_military_intervention_doctrine, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_2002_bush_unilateral_military_intervention_doctrine, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_2002_bush_unilateral_military_intervention_doctrine, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sotu_2002_bush_unilateral_military_intervention_doctrine, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_2002_bush_unilateral_military_intervention_doctrine, tangled_rope).
narrative_ontology:human_readable(sotu_2002_bush_unilateral_military_intervention_doctrine, "Unilateral U.S. Military Intervention Doctrine (SOTU 2002)").
narrative_ontology:topic_domain(sotu_2002_bush_unilateral_military_intervention_doctrine, "foreign_policy/security_doctrine").

domain_priors:requires_active_enforcement(sotu_2002_bush_unilateral_military_intervention_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_2002_bush_unilateral_military_intervention_doctrine, united_states_executive).
narrative_ontology:constraint_beneficiary(sotu_2002_bush_unilateral_military_intervention_doctrine, u_s_strategic_interests).
narrative_ontology:constraint_victim(sotu_2002_bush_unilateral_military_intervention_doctrine, national_sovereignty_of_target_states).
narrative_ontology:constraint_victim(sotu_2002_bush_unilateral_military_intervention_doctrine, international_legal_constraint_regime).
narrative_ontology:constraint_victim(sotu_2002_bush_unilateral_military_intervention_doctrine, non_aligned_nation_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TARGET NATION (SNARE) — A nation harboring or suspected of harboring terrorist groups faces unilateral military intervention threat with no meaningful exit. Cannot influence the U.S. judgment of whether they are sufficiently vigorous in counter-terrorism. No appeal mechanism; no veto right; sovereignty stripped. Full extraction: loss of territorial control, civilian casualties, institutional disruption. Suppression is total — no structural alternative exists.
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COALITION PARTNER NATION (TANGLED ROPE) — A nation technically allied with the U.S. but lacking resources for independent counter-terrorism operations faces pressure to cooperate or risk unilateral intervention. Mixed extraction: genuine coordination benefit (shared intelligence, training, partial burden-shifting) alongside coercive pressure (if deemed insufficiently vigorous, faces unilateral action). Exit cost is high (diplomatic isolation, potential military action) but not absolute — partial compliance may suffice.
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: U.S. EXECUTIVE BRANCH (ROPE) — The doctrine grants unilateral authority to act without coalition consensus, framing the constraint as solving a coordination problem: other nations are 'timid'; the U.S. must act. From this perspective, the mechanism is coordination — defining when action is justified and who has authority. Net beneficiary of the doctrine through expanded strategic flexibility. Experiences minimal suppression of its own options; maximum agency.
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL LEGAL REGIME (SCAFFOLD) — The doctrine temporarily suspends multi-lateral constraints on first-strike military action (UN Charter Article 51 self-defense language reinterpreted; preemption doctrine). From the lens of international law, this is a temporary override: justified by extraordinary terrorism threat; intended to expire when the threat is contained. Theater_ratio reflects that legal arguments for preemption are sophisticated but substantially performative — the underlying claim is 'existential threat justifies suspension of ordinary law,' which is inherently temporary framing. Sunset clause implicit: when terrorism is 'defeated' or threat perception declines, ordinary law should resume. Low chi because the regime maintains agency (can renegotiate, can re-establish norms).
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNITED NATIONS SYSTEM (PITON) — The doctrine marginalizes the UN Security Council consensus requirement. The UN system persists as a forum for coordination and legitimacy theater, but effective decision-making has migrated to U.S. unilateral judgment. The constraint maintains performative UN institutional role (statements, resolutions, debates) while functional authority is relocated. High theater_ratio: UN appears to decide, but decisions are post-hoc legitimation of U.S. choices. Piton classification reflects degradation through inertia — the institution persists because alternatives haven't fully replaced it, not because it functions as designed.
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, unilateral military doctrine appears as an immutable consequence of anarchic international relations: no global authority exists; therefore strong actors must reserve the right to act unilaterally in existential threats. International relations 'anarchy' makes unilateral preemption structurally inevitable. However, this perspective risks naturalizing what is a contingent doctrinal choice. The structural data reveals beneficiaries and victims, identifying this as a false summit candidate: the 'anarchic inevitability' framing naturalizes an institutional arrangement that benefits identifiable agents (U.S. executive) at cost to others (target nation sovereignty). Engine false summit detector will flag this as naturalization rather than natural law.
constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_2002_bush_unilateral_military_intervention_doctrine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_2002_bush_unilateral_military_intervention_doctrine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_2002_bush_unilateral_military_intervention_doctrine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_2002_bush_unilateral_military_intervention_doctrine, TR),
    TR >= 0.70.

:- end_tests(sotu_2002_bush_unilateral_military_intervention_doctrine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The doctrine generates significant extraction through unilateral authority concentration and suspension of multilateral constraints. However, it is not maximal (0.80+) because genuine coordination problems exist (transnational terrorism requires rapid response; multilateral consensus can be slow), and the doctrine partially solves these problems alongside extracting authority. The rise from 0.35 to 0.58 reflects doctrine normalization: initially presented as extraordinary response to 9/11 threat; over time becomes established precedent for unilateral action, increasing extraction efficiency. Suppression (0.72): High. Target nations face multiple suppression mechanisms: no legal veto (UN Charter authority concentrated in Security Council, which the U.S. can influence); no diplomatic exit (refusal to cooperate risks unilateral action); no military deterrent (U.S. dominance is overwhelming for most target nations). Suppression is not absolute (1.0) because some target nations maintain limited diplomatic agency and can negotiate terms of cooperation rather than pure unilateral action. Theater_ratio (0.65): Moderate-high. The doctrine is rationalized through sophisticated legal arguments (preemption doctrine, self-defense interpretation), which constitute theatrical legitimation. The underlying mechanism is power concentration, not genuinely new legal principle. The initial lower theater_ratio (0.48) reflects relative novelty of the doctrine; as it becomes established practice, theater rises (0.62 → 0.68) as legal arguments become standardized. The slight decline at T=10 (0.65) reflects early contestation and pushback from international law scholars, reducing theater slightly without fundamentally challenging the doctrine.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence across structural positions. The U.S. executive sees the doctrine as solving a coordination problem (Rope) — defining when multilateral consensus is insufficient and granting authority to act. Coalition partners see mixed benefits and extraction (Tangled Rope) — genuine counter-terrorism coordination alongside coercive pressure. Target nations see pure extraction with no exit (Snare) — stripped sovereignty with no appeal mechanism. The UN system experiences institutional degradation (Piton) — persists as legitimacy theater while functional authority has migrated. The international legal regime perceives temporary suspension of ordinary law (Scaffold) — extraordinary circumstances framework implies sunset clause when terrorism threat declines. The civilizational analytical observer risks naturalizing the power concentration as inevitable consequence of international anarchy (Mountain), but structural data reveals this as false summit — identifiable beneficiaries (U.S. executive) and victims (target nations) indicate constructed institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from the agent's structural position relative to the extraction flow. The U.S. executive benefits from the doctrine (d ≈ 0.05, low directionality, negative f(d)) — extraction flows toward them; they are beneficiary. Target nations suffer from the doctrine (d ≈ 0.92, high directionality, high f(d)) — extraction flows away from them; they are full victims. Coalition partners occupy middle position (d ≈ 0.60, moderate directionality) — they benefit from threat elimination and burden-sharing but bear extraction pressure through coercive compliance. The UN system (d ≈ 0.65) experiences extraction through institutional marginalization but maintains some functional role, preventing complete suppression. The key structural distinction: the doctrine is enforced by military dominance (U.S. can execute unilateral action without coalition consent), not by legal consensus (UN Charter constraints are reinterpreted rather than formally amended). This enforcement mechanism drives high suppression — target nations cannot veto or exit through legal channels.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by showing that the apparent contradiction between 'coordination' (U.S. executive perspective) and 'extraction' (target nation perspective) reflects a genuine structural asymmetry, not a measurement error. Both readings are correct from their respective positions. The coordination function is real (the doctrine does solve the problem of ineffective counter-terrorism by slow multilateral consensus), but so is the extraction mechanism (the doctrine does concentrate decision authority in the U.S. executive, imposing sovereignty costs on target nations). The Tangled Rope classification at the primary analytical perspective captures this hybrid: genuine coordination function (counter-terrorism capacity) + asymmetric extraction (sovereignty + legal constraint costs). The mandatrophy that appears to exist at the mountain level (naturalizing the doctrine as inevitable consequence of anarchy) is explicitly resolved by the false summit detection: the structural data shows beneficiaries and victims, confirming that the doctrine is constructed institutional arrangement, not natural law. The analytical observer's mountain classification is flagged as false summit; the true classification is tangled_rope (from the analytical, civilizational, global perspective) or snare (from the target nation perspective). No single 'correct' type exists; the presheaf of perspectives IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threat_threshold_ambiguity,
    'What constitutes ''timidity in the face of terror'' sufficient to justify unilateral intervention? How is the U.S. executive judgment on this threshold constrained or reviewable?',
    'Analysis of cases where the doctrine was invoked: were intervening nations'' counter-terrorism efforts genuinely insufficient, or were they deemed insufficient because their sovereignty was inconvenient? Post-hoc review of classification criteria used by U.S. decision-makers.',
    'If threshold is objective and reviewable: constraint is hybrid with genuine coordination function (snare for targets becomes constrained). If threshold is subjective and unreviewable: constraint is pure extraction mechanism (snare for targets, with suppression approaching 1.0).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threat_threshold_ambiguity, empirical, 'Objectivity and review-ability of terrorism threat threshold').

omega_variable(
    coalition_burden_shifting,
    'Does the unilateral doctrine reduce costs for coalition partners by shifting burden to U.S., or does it increase costs by pressuring partners to participate or face isolation?',
    'Comparative analysis: military burden-sharing (troops deployed, casualties incurred, funding contributed) by partner nations before and after doctrine articulation. Interview data on partner perception of coercion.',
    'If burden reduction: doctrine has rope characteristics even for partners (genuine coordination benefit). If burden increase: doctrine is tangled rope with asymmetric extraction confirmed (partners bear costs while U.S. retains strategic authority).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_burden_shifting, empirical, 'Whether doctrine reduces or increases coalition partner military burden').

omega_variable(
    international_law_suspension_permanence,
    'Is the suspension of UN Charter Article 51 constraints temporary and reversible, or has the doctrine created a permanent shift in international legal practice?',
    'Longitudinal analysis of UN voting patterns, Security Council veto usage, and subsequent military doctrines post-2002. Track whether the preemption doctrine precedent has become normalized or contested across administrations.',
    'If temporary: scaffold classification confirmed (sunset clause implicit in extraordinary circumstances framing). If permanent: scaffold becomes tangled_rope or snare (extraction via permanent legal norm shift masked as temporary emergency measure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_suspension_permanence, empirical, 'Permanence of the suspension of multilateral legal constraints').

omega_variable(
    terrorism_definition_drift,
    'How malleable is the definition of ''terrorism'' and ''terrorist groups'' that trigger the unilateral intervention doctrine? Can the definition be expanded to include political opponents?',
    'Semantic analysis of ''terrorism'' definitions in policy documents pre- and post-2002. Case analysis: which organizations have been designated terrorist vs. political groups under the doctrine. Examination of mission creep from counter-terrorism to regime change.',
    'If definition is stable and narrowly applied: suppression is legitimate security measure. If definition drifts: suppression becomes instrument of political control (doctrine becomes pure snare masked as counter-terrorism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(terrorism_definition_drift, conceptual, 'Semantic stability of ''terrorism'' as trigger for unilateral intervention').

omega_variable(
    countervailing_power_emergence,
    'Can other major powers develop credible counter-doctrines or deterrent mechanisms to constrain U.S. unilateral action, converting the constraint from snare to negotiated tangled_rope?',
    'Analysis of emerging military doctrines from China, Russia, EU that explicitly reserve reciprocal unilateral action rights. Track development of non-intervention alliance blocs. Examine whether multipolar military capacity reduces U.S. freedom of action.',
    'If countervailing power emerges: constraint becomes mutually binding (snare converts toward tangled_rope as suppression mechanism becomes bilateral rather than unilateral). If U.S. maintains dominance: snare persists unchanged.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(countervailing_power_emergence, empirical, 'Emergence of countervailing military doctrines constraining U.S. unilateral authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_2002_bush_unilateral_military_intervention_doctrine, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu2002_tr_t0, sotu_2002_bush_unilateral_military_intervention_doctrine, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu2002_tr_t3, sotu_2002_bush_unilateral_military_intervention_doctrine, theater_ratio, 3, 0.62).
narrative_ontology:measurement(sotu2002_tr_t6, sotu_2002_bush_unilateral_military_intervention_doctrine, theater_ratio, 6, 0.68).
narrative_ontology:measurement(sotu2002_tr_t10, sotu_2002_bush_unilateral_military_intervention_doctrine, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(sotu2002_be_t0, sotu_2002_bush_unilateral_military_intervention_doctrine, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu2002_be_t3, sotu_2002_bush_unilateral_military_intervention_doctrine, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(sotu2002_be_t6, sotu_2002_bush_unilateral_military_intervention_doctrine, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(sotu2002_be_t10, sotu_2002_bush_unilateral_military_intervention_doctrine, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_2002_bush_unilateral_military_intervention_doctrine, enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_2002_bush_unilateral_military_intervention_doctrine, multilateral_military_coalition_consensus).
narrative_ontology:affects_constraint(sotu_2002_bush_unilateral_military_intervention_doctrine, un_charter_article_51_interpretation).
narrative_ontology:affects_constraint(sotu_2002_bush_unilateral_military_intervention_doctrine, national_sovereignty_constraints).
narrative_ontology:affects_constraint(sotu_2002_bush_unilateral_military_intervention_doctrine, preemption_doctrine_precedent).

% DUAL FORMULATION NOTE:
% The doctrine operates at the intersection of three structurally distinct constraints: (1) the general coordination problem of transnational terrorism requiring rapid response (network constraint: real coordination need), (2) the institutional question of who has authority to act unilaterally (network constraint: US executive power concentration), (3) the legal question of whether unilateral preemptive action violates international law (network constraint: reinterpretation of UN Charter). The doctrine simultaneously 'solves' the coordination problem while extracting authority. Upstream constraints (multilateral consensus requirements, UN Charter constraints) are bypassed by this doctrine. Downstream constraints (precedent effects, normalization of unilateral action) are affected by this doctrine.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
