% ============================================================================
% CONSTRAINT STORY: article_51_self_defense__narrow_armed_attack_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_51_self_defense__narrow_armed_attack_reading, []).

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
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_51_self_defense__narrow_armed_attack_reading
 *   human_readable: Article 51 Narrow Armed Attack Reading: Self-Defense Constrained to State Attribution
 *   domain: international_law/security_studies/constitutional_interpretation
 *
 * SUMMARY:
 *   Article 51 of the UN Charter ('Nothing in the present Charter shall
 *   impair the inherent right of individual or collective self-defence if an
 *   armed attack occurs against a Member of the United Nations') is the
 *   foundational text for the legality of unilateral force in international
 *   law. However, the clause is fundamentally contested: what counts as an
 *   'armed attack'? Who must conduct it? Under what conditions is response
 *   'necessary'? This constraint story instantiates ONE reading — the
 *   narrow_armed_attack_reading — which interprets Article 51 to require that
 *   self-defense responses be limited to: (1) actual or imminent armed
 *   attacks (not preventive strikes against future threats), and (2) armed
 *   attacks attributable under international law to a state (not non-state
 *   actors, even if operating from a host state's territory). This reading
 *   preserves UN authority, protects weaker states from preemptive attack,
 *   and forces powerful states to seek Security Council authorization for
 *   broader interventions. It does so by constraining the strategic freedom
 *   of powerful states — they experience extraction. Weaker states and
 *   multilateral institutions experience coordination and benefit. Non-state
 *   actors and victims of non-attributable attacks experience a security gap
 *   (snare). The constraint's theater_ratio has risen from 0.32 to 0.48 over
 *   the interval as states increasingly engage in elaborate attribution
 *   theater and 'imminence' claims to justify actions while maintaining
 *   formal compliance with the narrow reading. This suggests the constraint
 *   is functioning — states feel compelled to justify their actions within
 *   its bounds — but the justifications are becoming more performative and
 *   contested, indicating incipient erosion toward the piton classification.
 *
 * KEY AGENTS:
 *   - Weaker States: Primary beneficiaries (institutional/constrained) — gain protection from preemptive attack and security guarantees through multilateral mechanisms
 *   - Powerful States: Primary victims (institutional/arbitrage) — lose unilateral strategic freedom to respond to threats they perceive as imminent without Security Council approval
 *   - UN Security Council and Multilateral Institutions: Secondary beneficiary (institutional/constrained) — preserve authority over use-of-force decisions; also face extraction through pressure from powerful states to authorize exceptions
 *   - Non-State Armed Actors: Secondary victim (powerless/trapped) — the legal constraint creates a security gap that benefits them operationally (victims cannot invoke Article 51 self-defense against them unless state-attributed)
 *   - Victims of Non-Attributable Attacks: Tertiary victim (powerless/trapped) — civilians and armed forces killed by non-state actors face a legal bind: their killers are not legally framed as 'armed attackers' (no state attribution), so Article 51 does not apply
 *   - Intelligence and Security Establishments: Gaming agents (organized/arbitrage) — develop attribution theater and imminence claims to justify actions within the reading's bounds while advancing their own strategic preferences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_51_self_defense__narrow_armed_attack_reading, 0.38).
domain_priors:suppression_score(article_51_self_defense__narrow_armed_attack_reading, 0.52).
domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_51_self_defense__narrow_armed_attack_reading, tangled_rope).
narrative_ontology:human_readable(article_51_self_defense__narrow_armed_attack_reading, "Article 51 Narrow Armed Attack Reading: Self-Defense Constrained to State Attribution").
narrative_ontology:topic_domain(article_51_self_defense__narrow_armed_attack_reading, "international_law/security_studies/constitutional_interpretation").

domain_priors:requires_active_enforcement(article_51_self_defense__narrow_armed_attack_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_51_self_defense__narrow_armed_attack_reading, '816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b').
narrative_ontology:cs_kernel_codification('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', fixed_text).
narrative_ontology:cs_authority_grounding('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', lineage).
narrative_ontology:cs_interpretation_layer_present('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b').
narrative_ontology:cs_reading_relation('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', article_51_self_defense__expansive_preventive_reading, coexists_with).
narrative_ontology:cs_reading_relation('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', article_51_self_defense__unable_unwilling_doctrine_reading, influences).
narrative_ontology:cs_axiom('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', foundational, armed_attack_requires_state_attribution).
narrative_ontology:cs_axiom_status(armed_attack_requires_state_attribution, holdable).
narrative_ontology:cs_axiom_grounding('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', armed_attack_requires_state_attribution, conventional).
narrative_ontology:cs_axiom('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', foundational, self_defense_excludes_preventive_force_absent_imminence).
narrative_ontology:cs_axiom_status(self_defense_excludes_preventive_force_absent_imminence, holdable).
narrative_ontology:cs_axiom_grounding('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', self_defense_excludes_preventive_force_absent_imminence, conventional).
narrative_ontology:cs_reference_frame('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', state_centric_collective_security_order).
narrative_ontology:cs_drift_state('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', contemporary_non_state_threat_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('816eecf7-9bbd-4fc2-8f0b-ce6084b0c02b', '').
narrative_ontology:cs_kernel_id(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, weaker_states).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, multilateral_institutions).
narrative_ontology:constraint_beneficiary(article_51_self_defense__narrow_armed_attack_reading, international_legal_order).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, powerful_states_strategic_freedom).
narrative_ontology:constraint_victim(article_51_self_defense__narrow_armed_attack_reading, targeted_state_security_discretion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUAL LITERALIST / NATURAL LAW VIEW (MOUNTAIN) — Article 51's text is read as a fixed legal fact: self-defense is limited to 'armed attack by a state.' This constraint emerges as natural law of the written Charter — immutable, unchangeable, the literal boundary of permissible force. No degrees of freedom; all contexts must fit within this frame. However, structural analysis reveals beneficiaries (weaker states gaining protection) and victims (powerful states losing strategic freedom), signaling potential false summit.
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: POWERFUL STATE UNDER CONSTRAINT (TANGLED ROPE) — A powerful state experiences this reading as extraction: it genuinely coordinates international security (deterrence, arms control verification, burden-sharing through multilateral frameworks), but simultaneously loses unilateral strategic freedom to respond to non-state threats, terrorist networks, or gray-zone actors. The constraint both enables collective security AND denies the powerful state rapid response to threats it perceives as existential. Mixed extraction and coordination — the state benefits from the global stability the constraint provides but bears significant cost from its own constrained options.
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: WEAKER STATE BENEFICIARY (ROPE) — A weaker state experiences this reading as pure coordination: the narrow Article 51 rule protects it from preemptive or preventive attacks by stronger powers claiming self-defense against alleged future threats. The constraint enables burden-sharing through UN mechanisms, regional alliances, and the normative prohibition on unilateral force. Exit options are constrained (cannot abandon international law wholesale) but the agent benefits from the coordination mechanism without experiencing significant extraction. The constraint solves the collective action problem of arms control and mutual security.
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NON-STATE ACTOR / UNATTRIBUTABLE THREAT (SNARE) — A non-state network or terrorist organization that does not operate under direct state attribution experiences this reading as a snare: they can conduct armed attacks that kill civilians and soldiers, but the victims cannot invoke Article 51 self-defense because the attacker is not 'a state.' The constraint creates a security gap that non-state actors exploit. The victims are trapped — high suppression because the legal constraint prevents response despite actual violence. No exit: the victims must either absorb the attacks, conduct counterterrorism outside the legal framework (violating international law), or escalate to attack a neighboring state to justify state attribution. Maximum experienced extraction from the powerless perspective (civilians, attacked armed forces who cannot legally retaliate).
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: UN AND MULTILATERAL INSTITUTIONS (TANGLED ROPE) — The UN Security Council and multilateral legal order benefit from the narrow reading (it preserves UN authority and multilateralism), but also face extraction: powerful states under threat pressure the Security Council to authorize action, and the constraint creates constant gaming about attribution and pretext. Institutions must both uphold the rule AND accommodate powerful states' security concerns. Constrained exit (institutions cannot simply rewrite the Charter unilaterally) but genuine coordination function (preventing great-power wars) and genuine extraction (pressure, politicization, manipulation of attribution criteria).
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR ATTRIBUTION DOCTRINE (PITON) — The narrow reading's attribution requirement was designed for the Cold War: clear, state-based proxy conflicts (Soviet-proxy states in Africa, Chinese-proxy insurgencies). In the contemporary era of non-state networks, transnational terrorism, and distributed attacks, the attribution mechanism has degraded — determining whether an attack is 'attributable under international law' to a state is now deeply contested and often performative (intelligence agencies claim attribution; other states dispute it; the UN remains deadlocked). The theater_ratio is high because states engage in elaborate attribution theater (public intelligence claims, technical forensics, diplomatic assertions) to justify responses while maintaining the appearance of staying within Article 51's bounds. The function has atrophied; the form persists.
constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_51_self_defense__narrow_armed_attack_reading, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_51_self_defense__narrow_armed_attack_reading, TR),
    TR >= 0.70.

:- end_tests(article_51_self_defense__narrow_armed_attack_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The narrow reading genuinely constrains powerful states' strategic freedom — they cannot unilaterally strike non-state actors or conduct preventive wars without legal cover that this reading requires (state attribution, actual or imminent attack, no clear alternative). However, the constraint is not maximal extraction because: (1) powerful states retain arbitrage options (invoking regional security arrangements, collective defense treaties, Security Council authorization via great-power consensus), (2) the attribution requirement is ambiguous enough to enable strategic justification, and (3) powerful states have built substantial workarounds (private military contractors, intelligence operations, cyber attribution ambiguity). The rise from 0.28 to 0.38 over 30 years reflects rising pressure on the constraint as non-state threat lethality has increased, forcing states to invest in more elaborate justification theater. Suppression (0.52): Moderate-high. The constraint carries real enforcement mechanisms (UN delegitimization, sanctions, regional alignment costs, International Criminal Court potential), but enforcement is patchy — the Security Council is deadlocked on major interventions, and powerful states absorb the reputational cost. Suppression would be higher (0.65+) if enforcement were consistent; it is lower than maximum because weaker states cannot force compliance from powerful actors unilaterally. Theater ratio (0.48): Moderate, rising. Early in the interval (1995-2005), the narrow reading was relatively functional — states did defer to the constraint and sought Security Council authorization for major interventions. Contemporary state practice shows rising theater: states now routinely claim state attribution (sometimes thin), assert imminence (increasingly contestable), and frame interventions within Article 51's bounds despite broader strategic intent. The rise reflects both the constraint's actual force (states feel compelled to justify) and its incipient degradation (justifications are becoming more performative and less credible). This trajectory — stable constraint with rising theater — is characteristic of a Tangled Rope approaching Piton degradation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival diversity. The powerful state sees it as pure extraction with coordination benefits (mixed experience). The weaker state sees it as pure coordination (rope). The non-state actor and attack victims see it as a snare with no exit. The UN and multilateral institutions see mixed coordination (their authority is preserved) and extraction (political pressure and gaming). The textual literalist sees a natural law of written text (mountain, but false summit because beneficiaries exist). The degraded attribution doctrine sees a ritual that has lost its functional force but persists through institutional inertia (piton). The perspectival spread — from mountain to snare to rope — indicates that all six constraint types coexist in the same institutional fact, distinguished by observer position and structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (chi) is derived from their structural directionality (d) — their position as beneficiary or victim, their power level, and their exit options. Powerful states occupying the victim position with arbitrage options have moderate d (~0.50-0.55) because they are victims with some exit, yielding moderate f(d) and moderate chi. Weaker states occupying the beneficiary position with constrained exit have low d (~0.20-0.30) because they benefit while bearing real costs (constrained exit), yielding near-zero or slightly negative chi. Non-state actors as powerless victims with no legal exit have maximum d (~0.95), yielding maximum f(d) and experienced chi. The narrow reading's spatial scope (global) applies σ(S) = 1.2, amplifying extractiveness at the global level. This is appropriate: the constraint's force comes from its universal applicability and the inability of any single state to opt out of the international legal order. The piton and mountain perspectives derive from theater/accessibility gates rather than from directionality recalculation — they are emergent classifications from structural degradation and textual naturalization, not from shifted (P,T,E,S) tuples.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT exhibit mandatrophy in the classical sense (simultaneity of pure coordination and pure extraction). Instead, it exemplifies what might be called 'perspectival mandatrophy': the same structural fact (Article 51's state-attribution requirement) is experienced as pure coordination (rope) from the weaker-state perspective, pure extraction (snare) from the non-state-actor perspective, and mixed (tangled rope) from the powerful-state and institutional perspectives. The resolution is NOT to assign one 'correct' type but to recognize that the constraint is genuinely multi-typed across the perspectival domain. The narrow reading's tangled rope classification (from the powerful-state perspective) is the appropriate global classification: it exhibits both coordination function (enables multilateral security architecture, deters great-power wars) and extraction (constrains unilateral response options). The mountain classification is a false summit: the textual reading naturalizes what is actually a political choice about how to interpret Article 51, and that choice benefits identifiable agents (weaker states, multilateral institutions). The piton classification (from the degraded-attribution perspective) reflects the rising theater — the constraint is becoming performative as states learn to claim attribution and imminence more effectively, shifting the binding mechanism from the rule itself toward the politics of attribution claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_attribution_threshold_ambiguity,
    'What level of state involvement constitutes ''attribution under international law'' — direct command-and-control, material support, organizational tolerance, or passive non-prevention?',
    'ICJ precedent analysis (Nicaragua v. US, Armed Activities on the Territory of Congo); ILC Responsibility of States framework; systematic review of Security Council attribution determinations 2001-2026',
    'Low threshold (material support): expands what counts as ''armed attack by a state,'' moves this reading toward expansive_preventive_reading territory, narrows victims to powerful states only. High threshold (direct control): tightens constraint, reinforces narrow reading, expands victims to include non-state actors harmed by the gap. Attribution ambiguity is THE omega for this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(state_attribution_threshold_ambiguity, conceptual, 'Definitional ambiguity in what constitutes state attribution for armed attack accountability').

omega_variable(
    imminent_threat_vs_preventive_force_slip,
    'Does ''actual or imminent armed attack'' prevent preventive strikes against gathering threats, or do assessments of ''imminence'' function as cover for preventive logic?',
    'Temporal analysis of claimed imminence vs actual time-to-attack in case studies (Israel 1967, US 2003, India 2019); forensic review of intelligence assessments ex-post; behavioral patterns in Security Council acquiescence to claims of imminence',
    'If imminence assessments are reliable: narrow reading is functional, prevents preventive wars. If unreliable: the constraint is performative theater (moves toward piton classification), or erodes into expansive_preventive_reading operationally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imminent_threat_vs_preventive_force_slip, empirical, 'Whether ''imminence'' criteria prevent or enable preventive force expansion').

omega_variable(
    non_state_actor_security_gap_persistence,
    'As non-state armed groups increase in lethality and scope (drone swarms, cyber weapons, private military contractors), does the state-attribution requirement create a persistent legal gap that forces states to violate international law or tolerate attacks?',
    'Trend analysis of non-state attack lethality vs state-attribution-based legal responses 2001-2026; institutional documentation of state law-of-war violations under pressure from non-attributable threats; survey of state practice in gray zones (cyber, CBRN, private contractors)',
    'If gap persists and widens: pressure accumulates for unable_unwilling_doctrine_reading or expansive_preventive_reading, delegitimizing narrow reading. If states develop workarounds: narrow reading survives but via performative attribution theater (piton pathway).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_security_gap_persistence, empirical, 'Whether state-attribution requirement creates persistent security gap driving law violation').

omega_variable(
    kernel_reading_identity_question,
    'Is the narrow_armed_attack_reading a living, enforced legal constraint or a formal text that has eroded operationally through state practice divergence?',
    'Inventory of Security Council acquiescence to actions claimed outside Article 51 strict bounds; gap analysis between Charter text and actual state interventions 1990-2026; institutional statements from UN Legal Counsel and regional bodies on Article 51 scope',
    'If living constraint: this reading''s extractiveness and suppression are accurately pitched at 0.38/0.52 — real constraint on actual behavior. If eroded: the narrow reading is aspirational/Piton-degraded (theater), extractiveness may be lower (~0.20), rendering this classification potentially inaccurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity_question, empirical, 'Whether narrow reading is an enforced constraint or a formally preserved but operationally eroded text').

omega_variable(
    committer_reading_displacement_risk,
    'Which sibling reading (expansive_preventive or unable_unwilling doctrine) does this narrow reading most directly displace in contemporary state practice, and what are the material consequences of the displacement?',
    'Comparative analysis of how often states invoke narrow Article 51 vs expansive justifications in their intervention announcements; tracking of which reading dominates across regional groupings (Africa, Asia, Americas, Europe) and power tiers (P5 vs non-P5)',
    'If displacement is real and directional: the narrow reading''s survival depends on continued great-power commitment to multilateralism. If reading is already operationally displaced: this constraint''s extractiveness should be lower, or the constraint should be reclassified as degraded (Piton) or as coexisting with its siblings under observer-specific selection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_displacement_risk, empirical, 'Which sibling reading the narrow reading actually displaces in state practice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_51_self_defense__narrow_armed_attack_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(a51narrow_theater_t0, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement(a51narrow_theater_t15, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(a51narrow_theater_t30, article_51_self_defense__narrow_armed_attack_reading, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(a51narrow_ext_t0, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(a51narrow_ext_t15, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(a51narrow_ext_t30, article_51_self_defense__narrow_armed_attack_reading, base_extractiveness, 30, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(a51narrow_supp_t0, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(a51narrow_supp_t15, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(a51narrow_supp_t30, article_51_self_defense__narrow_armed_attack_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_51_self_defense__narrow_armed_attack_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__expansive_preventive_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, article_51_self_defense__unable_unwilling_doctrine_reading).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, un_security_council_authorization_requirement).
narrative_ontology:affects_constraint(article_51_self_defense__narrow_armed_attack_reading, international_humanitarian_law_targeting_constraints).

% DUAL FORMULATION NOTE:
% The narrow_armed_attack_reading is one reading of the article_51_self_defense kernel. The sibling readings (expansive_preventive and unable_unwilling) constitute alternative interpretations of the same Charter text, each with different ε values, beneficiary/victim structures, and perspectival distributions. All three constraint stories are necessary to model the full space of Article 51 contestation. This story models the narrow reading's constraint force; the siblings model how that constraint is challenged and eroded in state practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(article_51_self_defense__narrow_armed_attack_reading, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
