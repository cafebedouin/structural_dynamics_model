% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__p5_veto_gatekeeper
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__p5_veto_gatekeeper, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_2_7_chapter_vii_tension__p5_veto_gatekeeper
 *   human_readable: P5 Veto Power as Institutional Gatekeeper in UN Chapter VII
 *   domain: international_law/security_studies/geopolitics
 *
 * SUMMARY:
 *   The P5 veto mechanism in UN Security Council Chapter VII enforcement
 *   represents a fundamental structural tension in post-WWII international
 *   governance: a collective security system designed to enable multilateral
 *   action requires consent from the most powerful military actors, yet that
 *   requirement grants those actors a unilateral veto that routinely prevents
 *   the system from functioning. This constraint demonstrates how the same
 *   institutional mechanism appears as natural law (necessary feature of
 *   great-power governance), coordinated fairness (legitimate protection of
 *   major powers), extraction mechanism (gatekeeping that prevents global
 *   south from accessing enforcement), and inertial theater (ritualized
 *   voting that masks real authority migration to ad-hoc coalitions). The P5
 *   veto exemplifies what we might call 'naturalized extraction' — a power
 *   distribution that benefits the veto holders is framed as inevitable,
 *   technical, or neutral coordination mechanism. The extractiveness has
 *   increased over the 80-year interval (0.48 → 0.62) as the gap between
 *   Security Council authority and actual enforcement capacity has widened.
 *   Theater has increased modestly (0.42 → 0.55) as the ritual of formal
 *   voting and veto statements has become more performative relative to
 *   actual collective action capacity.
 *
 * KEY AGENTS:
 *   - Permanent Security Council Members (P5 — US, Russia, China, UK, France): Primary beneficiaries (institutional/arbitrage) — hold veto power that guarantees protection of their security interests; can leverage veto for side agreements and exemptions
 *   - Non-Permanent Council Members: Primary victims (powerless/trapped) — excluded from veto power; bound by P5 decisions with zero enforcement agency
 *   - General Assembly: Secondary victim (powerless/trapped under Chapter VII) — structurally excluded from enforcement mechanisms; rendered advisory on security matters where it has formal equality
 *   - Targeted States (especially non-P5 major powers and developing nations): Victims with constrained options (organized/constrained) — face chapter VII enforcement; can organize blocking coalitions but at high diplomatic/military cost
 *   - UN Reform Coalition: Organized agents (organized/constrained) — developing nations, humanitarian advocates, and Uniting for Peace proponents seeking veto reform or alternative enforcement mechanisms
 *   - UN Security Council as Institutional Apparatus: Institutional actor (institutional/arbitrage) — maintains formal authority through ritual and legitimacy; actual enforcement authority has migrated to ad-hoc coalitions (NATO, regional powers)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, 0.62).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, 0.68).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, extractiveness, 0.62).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, tangled_rope).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, "P5 Veto Power as Institutional Gatekeeper in UN Chapter VII").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, "international_law/security_studies/geopolitics").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__p5_veto_gatekeeper).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, '0fb45ced-f466-4e7f-b601-cee3880f468e').
narrative_ontology:cs_kernel_codification('0fb45ced-f466-4e7f-b601-cee3880f468e', formalized).
narrative_ontology:cs_authority_grounding('0fb45ced-f466-4e7f-b601-cee3880f468e', extraction).
narrative_ontology:cs_interpretation_layer_present('0fb45ced-f466-4e7f-b601-cee3880f468e').
narrative_ontology:cs_axiom('0fb45ced-f466-4e7f-b601-cee3880f468e', foundational, great_power_security_autonomy_paramount).
narrative_ontology:cs_axiom_status(great_power_security_autonomy_paramount, holdable).
narrative_ontology:cs_axiom_grounding('0fb45ced-f466-4e7f-b601-cee3880f468e', great_power_security_autonomy_paramount, deontological).
narrative_ontology:cs_axiom('0fb45ced-f466-4e7f-b601-cee3880f468e', foundational, stability_requires_veto).
narrative_ontology:cs_axiom_status(stability_requires_veto, holdable).
narrative_ontology:cs_axiom_grounding('0fb45ced-f466-4e7f-b601-cee3880f468e', stability_requires_veto, empirically_contingent).
narrative_ontology:cs_reference_frame('0fb45ced-f466-4e7f-b601-cee3880f468e', post_war_great_power_consensus).
narrative_ontology:cs_drift_state('0fb45ced-f466-4e7f-b601-cee3880f468e', contemporary_multipolarity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0fb45ced-f466-4e7f-b601-cee3880f468e', '').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, permanent_security_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, non_permanent_council_members).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, general_assembly).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, targeted_states).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, international_collective_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PERMANENT COUNCIL MEMBER (SNARE) — Structurally excluded from veto power. Cannot exit the UN system without surrendering all global governance participation. Bound by P5 decisions with zero agency to block them. Maximum experienced extraction with no countervailing coordination benefit.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERAL ASSEMBLY UNDER CHAPTER VII (SNARE) — Formally excluded from Chapter VII enforcement mechanisms. The General Assembly, representing all states equally, cannot authorize military action, economic sanctions, or enforcement measures. Trapped in advisory role; extraction runs toward P5 permanent veto holders.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: TARGETED STATE WITH ALLIED COALITION (TANGLED ROPE) — If a state faces Chapter VII enforcement, it experiences extraction (military action, sanctions). But organized states with allies can coordinate blocking coalitions (e.g., Russia-China vetoes). Constrained, not trapped — coordination capacity exists though costly. The constraint provides coordination function (collective security mechanism) alongside asymmetric extraction (P5 gate control).
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PERMANENT SECURITY COUNCIL MEMBER (ROPE) — Veto power is experienced as coordination function. P5 members use the veto to guarantee their interests are protected in collective security decisions, enabling their participation in the system. Arbitrage exit: can leverage veto for side agreements, exemptions, or bilateral accommodations. Net beneficiary — extraction flows inward.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: UN REFORM ADVOCATES (SCAFFOLD) — Organized actors (developing nations, humanitarian coalitions, Uniting for Peace proponents) see veto gatekeeping as temporary institutional dysfunction with a potential sunset. Proposals for veto reform (two-thirds consensus, Uniting for Peace revival, Security Council expansion) represent scaffolding toward alternative enforcement mechanisms. Theater is moderate — reform rhetoric masks the structural difficulty of P5 consensus, but the goal is genuine institutional redesign.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SECURITY COUNCIL AS INSTITUTIONAL THEATER (PITON) — The veto mechanism persists largely through inertial legitimacy: the Security Council maintains its authority over Chapter VII enforcement despite frequent veto deadlock that prevents action. The ritual of vetoing (formal votes, diplomatic protests, strategic statements) is performative — it preserves the appearance of collective decision-making while actual enforcement authority has migrated to ad-hoc coalitions (NATO, regional powers). Theater ratio reflects that the formal mechanism (veto) is maintained more for legitimacy than for actual governance function.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, the veto mechanism appears as an immutable structural feature of great-power politics: any global collective security system requires the consent of the most militarily powerful actors, or they will opt out. This perspective sees the veto as a natural law of geopolitical coordination — no stable international order is possible without accommodating great-power veto rights. However, the engine will detect this as a false summit: the beneficiary structure (P5 members benefit from veto lock) reveals that naturalizing veto power as inevitable serves those who hold it.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__p5_veto_gatekeeper_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, TR),
    TR >= 0.70.

:- end_tests(article_2_7_chapter_vii_tension__p5_veto_gatekeeper_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The P5 veto enables asymmetric extraction through gate control: permanent members can block enforcement against themselves and their allies, while non-members cannot. However, extractiveness is not maximal (snare-level 0.66+) because legitimate coordination functions exist — the veto does guarantee that great powers will remain in the system rather than opting out, which enables the system's existence. The increase from 0.48 (1945) to 0.62 (2025) reflects accumulating extraction: more veto instances (deadlock in Syria, Ukraine, Palestine enforcement actions) without corresponding increase in actual enforcement capacity or reform mechanism, increasing the gap between promise and function. Suppression (0.68): Moderate-high and stable. The Charter explicitly excludes non-P5 members from veto power and structurally subordinates the General Assembly to Council authority on Chapter VII matters. This is formal prohibition with implicit coercion from P5 military capacity. Suppression is stable because neither the Charter nor the power distribution has fundamentally shifted, though efforts to invoke Uniting for Peace represent attempts to reduce it. Theater ratio (0.55): Moderate and rising. The Security Council voting ritual is highly performative — formal votes, diplomatic statements, strategic vetoes — yet actual enforcement authority has migrated to ad-hoc coalitions (NATO in Balkans, coalition forces in Iraq/Afghanistan, regional powers in Syria). The increase reflects growing divergence between the formal mechanism (veto voting) and actual governance capacity (coalitions operating outside Security Council authorization). Tangled Rope classification: The constraint provides genuine coordination (ensures great-power participation in collective security) alongside asymmetric extraction (veto gatekeeping). The active enforcement requirement is met: the veto mechanism requires active P5 participation and diplomatic negotiation. Beneficiaries are clear (P5 members); victims are clear (non-P5 members, General Assembly, targeted states).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a dramatic perspectival inversion: the same veto mechanism is experienced as coordination fairness by its beneficiaries and as pure extraction by its victims. P5 members frame veto as necessary (mountain) or coordinating (rope); non-P5 members frame it as gatekeeping (snare). The analytical observer must choose: does naturalization of veto serve the P5's interests (false summit) or does it describe an immutable feature of great-power systems (genuine mountain)? The constraint family effect: this reading (veto gatekeeper) coexists with alternative readings (general_assembly_primacy, supermajority_collective_security) that would produce different ε values and different type classifications. The kernel tension is irresolvable within a single framework — it requires commitment to a specific reading of Article 2(7) and Charter intent.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary structure: P5 members benefit from veto lock (preserved strategic autonomy, protection against hostile enforcement). Victim structure: non-P5 members, General Assembly, and targeted states bear extraction costs (exclusion from enforcement decisions, subjection to veto deadlock, inability to access collective security even when threatened). The directionality derivation from beneficiary/victim declarations and exit options produces the observed perspectival gap: P5 institutional beneficiaries with arbitrage exit → d ≈ 0.15 → negative χ → rope classification. Trapped victims with no exit → d ≈ 0.95 → high χ → snare classification. The same base properties (extractiveness 0.62, suppression 0.68) produce opposite types when directionality changes. This illustrates why declaring structural relationships (who benefits, who bears costs) is the primary input to the engine: the beneficiary/victim declarations fully determine directionality, which then determines effective extraction χ via the sigmoid function, which then determines classification type.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolves by recognizing that this constraint simultaneously provides collective security coordination AND asymmetric extraction gatekeeping. The coordination function (ensuring great-power participation) is genuine — without veto security, the most powerful actors would opt out. The extraction function (gatekeeping) is also genuine — the mechanism prevents non-P5 action even when majorities support it. The tangled rope classification accepts both functions as real. Mandatrophy would emerge if we tried to claim the veto is pure coordination (rope, ignoring victims) or pure extraction (snare, ignoring coordination benefits). Tangled rope avoids the trap by declaring both beneficiaries (coordination function) and victims (gatekeeping cost). The constraint is not incoherent — it is genuinely hybrid. The reform scaffold perspective shows that the hybrid can theoretically be resolved by redesign (Uniting for Peace, expansion of enforcement mechanisms), but currently it remains tangled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_necessity_vs_contingency,
    'Is the P5 veto a necessary structural feature of any stable great-power collective security arrangement, or a contingent institutional choice?',
    'Comparative analysis of historical and hypothetical collective security systems; examination of whether veto-free enforcement mechanisms (e.g., supermajority voting, rotating chair authority) have failed in practice or failed only because great powers refused participation',
    'If necessary: veto is immutable (mountain becomes legitimate). If contingent: veto is a negotiated power distribution that could be reformed (mountain is false summit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_necessity_vs_contingency, conceptual, 'Whether veto is a structural necessity or contingent choice').

omega_variable(
    extraction_magnitude_p5_vs_enforcement,
    'How much of the measured extraction (0.62) represents legitimate coordination cost of consensual great-power governance versus asymmetric extraction through veto gatekeeping?',
    'Counterfactual analysis: what would extractiveness be under a supermajority-vote Chapter VII system vs. current veto system? Measurement of actual harm from vetoed enforcement actions (deaths, humanitarian costs, failed interventions) attributable to veto deadlock.',
    'If coordination cost > 0.35: constraint is primarily Rope with enforcement overhead. If asymmetric extraction > 0.50: constraint is primarily Snare masquerading as Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_magnitude_p5_vs_enforcement, empirical, 'Proportion of extractiveness from coordination vs. gatekeeping').

omega_variable(
    uniting_for_peace_viability,
    'Does the Uniting for Peace mechanism (General Assembly authorization of enforcement) constitute a genuine alternative to P5 veto gatekeeping, or is it a Potemkin exit route?',
    'Historical case analysis: frequency of Uniting for Peace invocation, success rate of General Assembly-authorized enforcement, P5 responses (do they respect or circumvent GA authorizations?), contemporary likelihood of mobilizing two-thirds GA consensus vs. P5 consensus',
    'If viable: scaffold classification is accurate, sunset path exists. If Potemkin: scaffold is aspirational (should classify as piton or snare), exit routes are illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(uniting_for_peace_viability, empirical, 'Whether Uniting for Peace provides genuine alternative enforcement authority').

omega_variable(
    kernel_reading_interpretation_gap,
    'Does the UN Charter Article 2(7) collective security framework support the veto gatekeeper reading, or does it support a different reading emphasizing General Assembly equality and supermajority decision-making?',
    'Textual analysis: Charter language on Security Council vs. General Assembly authority; travaux préparatoires (drafting history) examining P5 intentions; jurisprudence of ICJ and UN practice interpretations',
    'If Charter text supports veto reading: this reading''s authority is strong. If Charter text supports alternative reading: this reading instantiates one possible but contested interpretation; sibling readings (general_assembly_primacy, supermajority_collective_security) are coexisting alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_interpretation_gap, conceptual, 'Whether Article 2(7) framework supports veto gatekeeper reading').

omega_variable(
    suppression_mechanism_voluntary_vs_coercive,
    'Is the suppression of non-P5 enforcement capacity a result of formal prohibition (Chapter VII structural design) or of implicit coercion (P5 military capacity override)?',
    'Analysis of UN practice: do non-P5 states attempt to invoke supermajority enforcement pathways (Uniting for Peace) and face active P5 obstruction, or do they self-suppress without P5 needing to intervene?',
    'If formal prohibition: suppression is structural/intentional (high legitimacy claim). If implicit coercion: suppression depends on P5 military credibility (more contingent; vulnerable if military balance shifts).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_voluntary_vs_coercive, empirical, 'Whether suppression is formal or implicit coercion-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(p5veto_tr_t0, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, theater_ratio, 0, 0.42).
narrative_ontology:measurement(p5veto_tr_t5, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, theater_ratio, 5, 0.5).
narrative_ontology:measurement(p5veto_tr_t10, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(p5veto_be_t0, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(p5veto_be_t5, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(p5veto_be_t10, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, base_extractiveness, 10, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(p5veto_su_t0, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(p5veto_su_t5, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(p5veto_su_t10, article_2_7_chapter_vii_tension__p5_veto_gatekeeper, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, general_assembly_primacy).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, supermajority_collective_security).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__p5_veto_gatekeeper, unilateral_security_action_legitimacy).

% DUAL FORMULATION NOTE:
% The veto gatekeeper reading coexists with alternative readings of the Article 2(7) framework. All readings share the same base structural facts (P5 formal veto power, Charter text) but interpret the framework's intent and legitimacy differently. The sibling readings would produce different ε values and different type classifications: GA-primacy reading would show veto as contingent institutional choice (higher ε, different beneficiary structure); supermajority reading would show veto as circumventable (different victims, scaffold-type sunset path already encoded in Uniting for Peace). This story represents the veto gatekeeper reading only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
