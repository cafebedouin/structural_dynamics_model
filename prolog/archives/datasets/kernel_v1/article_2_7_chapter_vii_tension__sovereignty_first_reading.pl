% ============================================================================
% CONSTRAINT STORY: article_2_7_chapter_vii_tension__sovereignty_first_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_2_7_chapter_vii_tension__sovereignty_first_reading, []).

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
 *   constraint_id: article_2_7_chapter_vii_tension__sovereignty_first_reading
 *   human_readable: Article 2(7)/Chapter VII Tension: Sovereignty-First Reading
 *   domain: international_law/political_philosophy/security_studies
 *
 * SUMMARY:
 *   The sovereignty-first reading of the Article 2(7)/Chapter VII tension
 *   treats state sovereignty as foundational and absolute except in cases of
 *   explicit interstate aggression. This reading maintains that humanitarian
 *   intervention, even in response to atrocity, requires either the target
 *   state's explicit consent or a narrowly defined Chapter VII authorization
 *   limited to threats arising from interstate conflict. Populations
 *   suffering domestic atrocity have no standing to invoke international
 *   humanitarian intervention based on their suffering alone; their
 *   protection depends entirely on their state's willingness to accept
 *   external help or on Great Powers' geopolitical calculation that
 *   intervention serves their interests. This reading privileges the
 *   post-Westphalian international system's commitment to non-interference
 *   and mutual recognition of territorial authority. It benefits
 *   authoritarian and post-colonial states by providing immunity from
 *   external accountability for internal repression, while extracting from
 *   populations under atrocity by denying them any independent right to
 *   international protection. The constraint's extractiveness has risen from
 *   0.52 to 0.74 over the interval (roughly 1945–1995), driven by
 *   accumulating precedent (Korean War, Vietnam, Afghanistan Soviet invasion,
 *   various proxy conflicts) that solidified the interpretation that Chapter
 *   VII cannot be invoked for pure humanitarian grounds. Theater ratio has
 *   also risen (0.35 to 0.55) as diplomatic framing increasingly uses
 *   humanitarian language while maintaining the legal position that Article
 *   2(7) blocks action — creating performative humanitarian concern without
 *   functional intervention capacity.
 *
 * KEY AGENTS:
 *   - Populations Under Atrocity: Primary victim (powerless/trapped) — trapped in territorial jurisdiction with no right to external appeal; experience maximum extraction as doctrine provides immunity for their persecutors
 *   - Post-Colonial/Authoritarian States: Primary beneficiary (institutional/arbitrage) — capture absolute immunity from external intervention; experience the constraint as pure coordination of mutual non-interference
 *   - Humanitarian/Human Rights Coalition: Organized actors (organized/constrained) — experience mixed coordination (prevents Great Power wars) and extraction (their advocacy is suppressed by Article 2(7) barrier)
 *   - Great Powers (especially P5): Secondary beneficiary (institutional/arbitrage) — maintain veto authority and geopolitical flexibility; can invoke Article 2(7) when convenient and override it when aligned
 *   - UN Institutional Structure: Institutional actor (institutional/arbitrage) — maintains the doctrine through diplomatic ritual while acknowledging degradation of functional intervention capacity
 *   - Would-be Intervening States: Secondary victim (moderate/constrained) — face suppression from Article 2(7) barrier, legal risk, and veto threat even when motivated by humanitarian concern
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.68).
domain_priors:suppression_score(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.72).
domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare).
narrative_ontology:human_readable(article_2_7_chapter_vii_tension__sovereignty_first_reading, "Article 2(7)/Chapter VII Tension: Sovereignty-First Reading").
narrative_ontology:topic_domain(article_2_7_chapter_vii_tension__sovereignty_first_reading, "international_law/political_philosophy/security_studies").

domain_priors:requires_active_enforcement(article_2_7_chapter_vii_tension__sovereignty_first_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_2_7_chapter_vii_tension__sovereignty_first_reading, '78f54de9-2352-4fd5-81d1-736403617c98').
narrative_ontology:cs_kernel_codification('78f54de9-2352-4fd5-81d1-736403617c98', formalized).
narrative_ontology:cs_authority_grounding('78f54de9-2352-4fd5-81d1-736403617c98', extraction).
narrative_ontology:cs_interpretation_layer_present('78f54de9-2352-4fd5-81d1-736403617c98').
narrative_ontology:cs_reading_relation('78f54de9-2352-4fd5-81d1-736403617c98', article_2_7_chapter_vii_tension__r2p_reading, coexists_with).
narrative_ontology:cs_axiom('78f54de9-2352-4fd5-81d1-736403617c98', foundational, territorial_sovereignty_is_foundational).
narrative_ontology:cs_axiom_status(territorial_sovereignty_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('78f54de9-2352-4fd5-81d1-736403617c98', territorial_sovereignty_is_foundational, deontological).
narrative_ontology:cs_axiom('78f54de9-2352-4fd5-81d1-736403617c98', foundational, humanitarian_access_requires_explicit_consent).
narrative_ontology:cs_axiom_status(humanitarian_access_requires_explicit_consent, holdable).
narrative_ontology:cs_axiom_grounding('78f54de9-2352-4fd5-81d1-736403617c98', humanitarian_access_requires_explicit_consent, conventional).
narrative_ontology:cs_reference_frame('78f54de9-2352-4fd5-81d1-736403617c98', westphalian_state_system_foundational).
narrative_ontology:cs_drift_state('78f54de9-2352-4fd5-81d1-736403617c98', post_cold_war_humanitarian_pressure, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('78f54de9-2352-4fd5-81d1-736403617c98', '2026-02-26T14:22:33Z').
narrative_ontology:cs_kernel_id(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, post_colonial_authoritarian_states).
narrative_ontology:constraint_beneficiary(article_2_7_chapter_vii_tension__sovereignty_first_reading, great_power_veto_holders).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, populations_under_atrocity).
narrative_ontology:constraint_victim(article_2_7_chapter_vii_tension__sovereignty_first_reading, humanitarian_access_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POPULATION UNDER ATROCITY (SNARE) — Faces high suppression from domestic regime. International legal framework explicitly locks them into territorial jurisdiction (Article 2(7)) with no exit option. Cannot appeal to intervention under Chapter VII unless state-to-state aggression occurs. Extraction is maximal: sovereignty doctrine provides immunity for domestic perpetrators while blocking external remedy pathways.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POST-COLONIAL/AUTHORITARIAN STATE (ROPE) — Benefits from absolute sovereignty protection against external intervention. Article 2(7) provides a hard barrier to humanitarian claims. Experiences the constraint as pure coordination: the sovereignty doctrine coordinates non-interference and mutual recognition among states. Net beneficiary — the constraint subsidizes regime stability and shields internal repression from external accountability.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HUMANITARIAN/HUMAN RIGHTS COALITION (TANGLED ROPE) — Organized actors (NGOs, advocacy networks, donor states) experience genuine coordination function: sovereignty doctrine prevents Great Power military competition on humanitarian pretexts. But they also experience extraction: their advocacy capacity is suppressed by the Article 2(7) barrier, forcing them into indirect channels (ICC, fact-finding, advocacy campaigns) with limited enforcement. Mixed coordination (preventing Great Power wars) and extraction (silencing humanitarian voice).
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: UN INSTITUTIONAL STRUCTURE (PITON) — The UN system treats Article 2(7) as foundational law, but its enforcement mechanisms have atrophied. The Security Council's humanitarian intervention authority (Chapter VII, Article 42) is routinely blocked by veto, rendering the provision performative rather than functional. The institutional apparatus maintains the sovereignty doctrine through diplomatic ritual and charter citation while acknowledging that its intervention capacity has degraded. Theater-heavy maintenance of a once-functional coordination device.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / WESTPHALIAN INEVITABILITY (MOUNTAIN) — From a civilizational view, state sovereignty is treated as an irreducible logical/historical necessity: the Westphalian system and the principle of non-interference are presented as natural outcomes of international relations theory, necessarily emerging from the anarchic structure of the international system. This perspective risks naturalizing what is actually a contingent institutional choice. The engine's false summit detector will identify this as misclassification if beneficiaries are declared (which they are).
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: INTERVENING STATE (WITHOUT CONSENT) (SNARE) — A state seeking to intervene on humanitarian grounds faces maximum suppression: legal doctrine (Article 2(7)), veto threat from Security Council, reputational costs, potential counter-intervention. Exit options are severely constrained — intervention requires either explicit consent (undermining sovereignty of intervening action) or finding a Great Power that will block veto (dependency on external authorization). High extraction from the sovereignty framework even for actors who might justify intervention.
constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_2_7_chapter_vii_tension__sovereignty_first_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_2_7_chapter_vii_tension__sovereignty_first_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_2_7_chapter_vii_tension__sovereignty_first_reading, TR),
    TR >= 0.70.

:- end_tests(article_2_7_chapter_vii_tension__sovereignty_first_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The sovereignty-first reading creates asymmetric protection: authoritarian states capture immunity while populations under atrocity have no legal standing to invoke external protection. The extraction is not total (0.74 or higher) because some humanitarian crises do trigger Chapter VII authorization when Great Powers align (current precedent: ~10–15% of documented atrocities receive intervention consideration). The extractiveness also reflects that the doctrine is actively enforced through legal doctrine, Security Council precedent, and state practice — not merely an absence of assistance but an active suppression of the normative claim that atrocity victims have standing. Rising extractiveness over the interval reflects accumulation of precedent making the doctrine's enforcement more rigid and more explicitly justified. Suppression (0.72): High. Barriers to humanitarian intervention include the Article 2(7) legal requirement, the high bar of 'threat to the peace' under Chapter VII, the Security Council veto threat, the sovereignty principle itself, and the absence of any independent right for affected populations to trigger intervention. These are structural barriers backed by legal doctrine. Theater ratio (0.55): Moderate. The constraint has an intermediate theater ratio because diplomatic practice increasingly uses humanitarian language and R2P framing, but the underlying legal doctrine (Article 2(7)) remains the operative gating mechanism. UN statements about humanitarian concern are performative while the actual authorization structure (Chapter VII requiring 'threat to peace') filters atrocity concerns through geopolitical calculation. Rising theater ratio over the interval reflects increasing gap between rhetorical humanitarian commitment and actual intervention capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival divide. Populations under atrocity see a snare with maximum extraction and no exit (mountain immutability from their perspective, but in fact a snare with high suppression). Post-colonial/authoritarian states see pure coordination — the doctrine simply formalizes mutual recognition and non-interference. The humanitarian coalition sees tangled rope — genuine coordination benefit (preventing Great Power wars) mixed with extraction (silencing their advocacy). The UN institution sees a piton — once-functional system (enforcement capacity during Cold War détente) now performative (humanitarian language without intervention capacity). The analytical observer's mountain view risks naturalizing Westphalian sovereignty as immutable rather than recognizing it as a contingent institutional choice with specific beneficiaries. The perspectival gap is widest between the victim (snare, powerless, trapped) and the beneficiary (rope, institutional, arbitrage) — they are reading the same legal doctrine as fundamentally opposite constraint types.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural position: whether they benefit or bear costs, their exit options, and their power level. Populations under atrocity are victims with no exit (trapped, d=0.95, f(d)≈1.42); post-colonial authoritarian states are beneficiaries with high exit/arbitrage (d=0.05, f(d)≈-0.12); the humanitarian coalition is organized but constrained (d=0.50–0.60, f(d)≈0.65–0.85). The Great Powers occupy a unique position: they appear as both beneficiaries (veto authority) and secondary beneficiaries (geopolitical flexibility), but their high institutional power and arbitrage exit options keep their d low (0.10–0.20). The UN structure is beneficiary-aligned (it maintains the doctrine) with arbitrage exit (d=0.12). Would-be intervening states are secondary victims (constrained exit, no direct atrocity responsibility) with d=0.65–0.75. No directionality overrides are needed — the beneficiary/victim declarations and exit options drive the standard derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does NOT resolve mandatrophy in the classical sense because it is ONE READING of a contested kernel, not a neutral policy question. Mandatrophy resolution would require comparing this sovereignty-first reading to the R2P reading to show that BOTH readings are coherent instantiations of the same structural fact (Article 2(7) + Chapter VII + atrocity reality), each with different ε values, different beneficiary/victim structures, and different implications for intervention. The sovereignty-first reading claims ε=0.68 (high extraction, snare for atrocity victims). The R2P reading would claim lower ε (high coordination, low extraction, maybe rope or tangled rope with different beneficiary/victim weights). The mandatrophy is 'which reading is correct?' — and the answer is both are structurally defensible, chosen by different institutional actors (post-colonial states vs humanitarian advocates) based on their interests and values, not by appeal to facts alone. The false summit detection on the analytical observer's mountain perspective reveals that 'Westphalian necessity' is not immutable — it is a reading choice with beneficiaries.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_kernel_identity,
    'Is this constraint one reading of a contested kernel (Article 2(7) + Chapter VII tension) or a foundational natural law of international relations?',
    'Comparative historical/institutional analysis: does Article 2(7) describe an inherent logical structure of sovereign states, or does it describe a specific institutional choice made at San Francisco 1945 that could have been (and has been attempted to be) otherwise? Did pre-UN international law have the same non-intervention absolute? Did the UN Charter''s own creation show alternatives to Article 2(7)?',
    'If reading: the constraint is contingent, contested, and one of multiple coherent framings. If natural law: the constraint is immutable and the R2P sibling reading would be logically foreclosed. Current classification assumes reading status — the false summit detection on the mountain perspective confirms this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Whether Article 2(7) is a contingent institutional reading or immutable natural law').

omega_variable(
    domestic_atrocity_threshold_underspecification,
    'What severity of domestic atrocity justifies Chapter VII authorization? Is genocide required, or do systematic human rights violations suffice?',
    'Text analysis of Chapter VII (''threat to the peace, breach of the peace, or act of aggression'') vs. contemporary precedent (Kosovo 1999, Libya 2011 — both invoked humanitarian emergency; Rwanda 1994 — no intervention despite genocide). Does the threshold exist in the legal text or only in state practice?',
    'If threshold is underspecified: sovereignty doctrine provides absolute Article 2(7) immunity except in state-to-state conflict, making domestic atrocity irrelevant to Chapter VII eligibility. If threshold is emergent from practice: humanitarian emergency can meet ''threat to peace'' standard, making this reading''s absoluteness contestable at the margin.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_atrocity_threshold_underspecification, conceptual, 'The legal threshold for humanitarian emergency under Chapter VII').

omega_variable(
    r2p_institutional_traction,
    'Has the R2P (Responsibility to Protect) reading achieved sufficient institutional codification to constitute a competing reading with holdable status, or is it still a challenger narrative?',
    'Institutional adoption count: UN General Assembly endorsement (2005), regional organization adoption (African Union, ASEAN), invocations in Security Council votes and state statements. Does R2P appear in state practice enough to constitute a live alternative, or is it largely NGO/scholar advocacy without institutional substrate?',
    'If R2P is holdable: two genuinely live readings coexist with different axioms and different implications for intervention authorization. This reading (sovereignty-first) and R2P are both rational institutional positions. If R2P is still challenger: this reading remains dominant but under pressure, influencing but not yet overriding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(r2p_institutional_traction, empirical, 'Institutional codification status of the R2P reading').

omega_variable(
    veto_enforcement_mechanism,
    'Is the Security Council veto (allowing permanent members to block Chapter VII authorization) part of the sovereignty-first reading''s structure, or a separate constraint?',
    'Textual analysis: Article 2(7) and Chapter VII are distinct mechanisms. Does a state''s Article 2(7) immunity depend on Security Council composition? Could a non-blocked Chapter VII vote overcome Article 2(7), or does Article 2(7) have independent blocking force?',
    'If veto is structural: the extractiveness of this constraint is partly indexed to Great Power politics, and a different Security Council composition could alter the constraint''s practical force. If Article 2(7) is independent: the constraint''s extraction is stable regardless of veto politics. Current model treats veto as a separate enforcement layer, not intrinsic to this constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(veto_enforcement_mechanism, empirical, 'Relationship between Article 2(7) and Security Council veto enforcement').

omega_variable(
    post_colonial_beneficiary_specificity,
    'Do all post-colonial states benefit equally from Article 2(7) protection, or does the constraint''s extraction flow preferentially to authoritarian regimes while constraining democratic post-colonial states?',
    'Analysis of state behavior: do democratic post-colonial states invoke Article 2(7) more or less frequently than authoritarian counterparts? Do they seek to reform the doctrine? Do they support humanitarian intervention in other states? Directionality may differ by regime type even within the post-colonial category.',
    'If benefit is regime-dependent: the beneficiary group is more narrowly authoritarian states, not post-colonial states generically. The constraint then targets a narrower extraction flow and benefits a more specific coalition. This would refine the directionality model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_colonial_beneficiary_specificity, empirical, 'Regime-dependent beneficiary specificity of Article 2(7)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_2_7_chapter_vii_tension__sovereignty_first_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_first_theater_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sov_first_theater_t25, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(sov_first_theater_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(sov_first_extract_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(sov_first_extract_t25, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(sov_first_extract_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, base_extractiveness, 50, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(sov_first_supp_t0, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(sov_first_supp_t25, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement(sov_first_supp_t50, article_2_7_chapter_vii_tension__sovereignty_first_reading, suppression_requirement, 50, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_2_7_chapter_vii_tension__sovereignty_first_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, article_2_7_chapter_vii_tension__r2p_reading).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, security_council_veto_enforcement).
narrative_ontology:affects_constraint(article_2_7_chapter_vii_tension__sovereignty_first_reading, great_power_geopolitical_arbitrage).

% DUAL FORMULATION NOTE:
% The Article 2(7)/Chapter VII tension decomposes into two readings with different ε values and different structural implications. This sovereignty-first reading (ε=0.68, snare) treats Article 2(7) as foundational and Chapter VII as narrowly limited to interstate conflict. The R2P reading (sibling constraint, not this file) would claim lower ε and different beneficiary/victim structure, treating humanitarian emergency as sufficient for Chapter VII authorization. Both readings operate on the same legal text (the UN Charter) but instantiate different constraint types because they weight coordination vs extraction differently. Decomposition is semantically and epistemically necessary — the contested kernel cannot be modeled as a single constraint with measurement-dependent classification. The readings are linked via network.affects_constraints as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
