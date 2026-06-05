% ============================================================================
% CONSTRAINT STORY: empirical_social_substrate_split
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_empirical_social_substrate_split, []).

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
 *   constraint_id: empirical_social_substrate_split
 *   human_readable: Empirical-Social Substrate Split in Epistemic Contests
 *   domain: epistemology/philosophy_of_science/cognitive_science
 *
 * SUMMARY:
 *   The empirical-social substrate split describes a structural ambiguity in
 *   epistemic contests: participants may be engaged in an empirical dispute
 *   (testing claims about reality) or a social dispute (negotiating status,
 *   coalition membership, or resource allocation), and the deepest damage
 *   occurs when one party misidentifies which substrate the interaction is
 *   on. A good-faith truth seeker who brings empirical arguments to a status
 *   contest wastes cognitive resources and cedes social ground; conversely, a
 *   participant who treats an empirical question as a social negotiation
 *   corrupts the epistemic commons. The constraint exhibits both coordination
 *   and extraction: the substrate distinction is genuinely useful (it enables
 *   specialized epistemic tools), but the ambiguity between substrates
 *   enables systematic exploitation by actors who can shift frames
 *   strategically while their opponents remain locked into one substrate. The
 *   constraint is downstream of parable-as-transmission-layer: narrative
 *   knowledge transfer inherently blurs empirical and social substrates
 *   because stories encode both factual claims and social signals. The
 *   theater_ratio (0.58) reflects that much academic discourse maintains the
 *   appearance of empirical rigor (peer review, citation norms,
 *   methodological standards) while actually operating on the social
 *   substrate (status games, coalition signaling, resource competition).
 *   Methodological reforms (registered reports, adversarial collaboration)
 *   represent an attempt to force substrate discipline, but their
 *   effectiveness remains uncertain.
 *
 * KEY AGENTS:
 *   - Good-Faith Truth Seekers: Primary victims (powerless/identity_locked) — cannot recognize substrate shifts without abandoning their identity as truth-seekers; invest cognitive resources in empirical arguments while opponents play status games
 *   - Strategic Frame Shifters: Primary beneficiaries (institutional/arbitrage) — can shift between empirical and social substrates strategically, deploying empirical arguments when convenient and status moves when advantageous
 *   - Reflective Participants: Secondary victims (moderate/constrained) — have learned to recognize substrate shifts but face high social costs to exit or call out frame confusion
 *   - Methodological Reform Coalition: Organized agents (organized/mobile) — building explicit substrate-tagging protocols to make frame shifts visible and costly
 *   - Academic Peer Review System: Institutional actor (institutional/constrained) — supposed to enforce substrate discipline but has degraded into theater; cannot distinguish genuine confusion from strategic shifting
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both coordination function (substrate distinction enables specialized tools) and extraction mechanism (ambiguity enables strategic exploitation)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(empirical_social_substrate_split, 0.48).
domain_priors:suppression_score(empirical_social_substrate_split, 0.62).
domain_priors:theater_ratio(empirical_social_substrate_split, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(empirical_social_substrate_split, extractiveness, 0.48).
narrative_ontology:constraint_metric(empirical_social_substrate_split, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(empirical_social_substrate_split, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(empirical_social_substrate_split, tangled_rope).
narrative_ontology:human_readable(empirical_social_substrate_split, "Empirical-Social Substrate Split in Epistemic Contests").
narrative_ontology:topic_domain(empirical_social_substrate_split, "epistemology/philosophy_of_science/cognitive_science").

domain_priors:requires_active_enforcement(empirical_social_substrate_split).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(empirical_social_substrate_split, actors_exploiting_frame_confusion).
narrative_ontology:constraint_beneficiary(empirical_social_substrate_split, strategic_frame_shifters).
narrative_ontology:constraint_victim(empirical_social_substrate_split, participants_assuming_epistemic_contest).
narrative_ontology:constraint_victim(empirical_social_substrate_split, good_faith_truth_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GOOD-FAITH TRUTH SEEKER (SNARE) — Identity-locked into epistemic contest framing. Cannot recognize when the game has shifted to social substrate without abandoning their identity as a truth-seeker. Bears maximum extraction: invests cognitive resources in empirical arguments while opponent plays status games. The identity lock is cognitive — structurally could exit but cannot see the substrate shift from within their epistemic frame.
constraint_indexing:constraint_classification(empirical_social_substrate_split, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: REFLECTIVE PARTICIPANT (TANGLED ROPE) — Has learned to recognize substrate shifts but faces high costs to exit: calling out frame confusion is socially costly and risks being labeled paranoid or uncollegial. Benefits from the coordination function (shared epistemic norms enable productive discourse when both parties are on empirical substrate) but also bears extraction when opponents exploit the ambiguity. Mixed experience — some agency, some entrapment.
constraint_indexing:constraint_classification(empirical_social_substrate_split, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STRATEGIC FRAME SHIFTER (ROPE) — Institutional actor with arbitrage exit. Experiences the substrate ambiguity as a coordination resource: can shift between empirical and social substrates strategically, deploying empirical arguments when convenient and status moves when advantageous. Net beneficiary — the frame confusion is a tool, not a trap.
constraint_indexing:constraint_classification(empirical_social_substrate_split, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: METHODOLOGICAL REFORM COALITION (SCAFFOLD) — Organized agents building explicit substrate-tagging protocols: adversarial collaboration frameworks, registered reports, pre-commitment to empirical tests, and social norm enforcement mechanisms that make substrate shifts visible. See the split as a temporary coordination failure with a sunset: as methodological reforms mature, substrate confusion becomes harder to exploit. Estimated sunset: 15-25 years for norms to propagate across academic disciplines.
constraint_indexing:constraint_classification(empirical_social_substrate_split, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC PEER REVIEW SYSTEM (PITON) — Traditional peer review is supposed to enforce substrate discipline (empirical claims require empirical evidence; social claims require social evidence) but has degraded into theater. Reviewers cannot distinguish genuine substrate confusion from strategic frame-shifting without access to author intent. The ritual persists through institutional inertia: journals maintain the review process because no alternative has fully replaced it, not because it effectively polices substrate boundaries.
constraint_indexing:constraint_classification(empirical_social_substrate_split, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the substrate split serves a genuine coordination function: distinguishing empirical from social claims enables specialized epistemic tools (experiment vs negotiation, evidence vs rhetoric). But the ambiguity also enables extraction: actors who can shift substrates strategically extract from those who cannot. The analytical classification is Tangled Rope because both functions coexist — the split is not purely extractive (it solves a real coordination problem) and not purely coordinative (it enables systematic exploitation).
constraint_indexing:constraint_classification(empirical_social_substrate_split, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(empirical_social_substrate_split_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(empirical_social_substrate_split, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(empirical_social_substrate_split, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(empirical_social_substrate_split, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(empirical_social_substrate_split, TR),
    TR >= 0.70.

:- end_tests(empirical_social_substrate_split_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. Strategic frame shifters capture significant advantages by exploiting substrate ambiguity — they can deploy empirical arguments when those are winning and shift to social moves when empirical arguments are losing, while good-faith participants remain locked into one substrate. The extraction is not maximal because the substrate distinction also serves a genuine coordination function: distinguishing empirical from social claims enables specialized epistemic tools. Suppression (0.62): High. Significant barriers to recognizing and exiting substrate confusion include: identity fusion (truth-seekers cannot see the shift without abandoning their self-concept), social costs (calling out frame confusion is labeled paranoid or uncollegial), lack of explicit substrate-tagging protocols, and the inherent ambiguity of narrative knowledge transfer. Theater ratio (0.58): Moderate-high. Much academic discourse maintains the appearance of empirical rigor while actually operating on the social substrate. Peer review is supposed to enforce substrate discipline but cannot distinguish genuine confusion from strategic shifting without access to author intent. The theater has increased over the interval as academic competition has intensified and strategic frame-shifting has become more sophisticated.
 *
 * PERSPECTIVAL GAP:
 *   The good-faith truth seeker sees a snare: they are trapped in an epistemic contest while their opponent plays a status game, and they cannot recognize the substrate shift without abandoning their identity. The strategic frame shifter sees a rope: the substrate ambiguity is a coordination resource that enables flexible deployment of arguments. The reflective participant sees a tangled rope: benefits from the coordination function when both parties are on the same substrate, but bears extraction when opponents exploit the ambiguity. The methodological reform coalition sees a scaffold: the substrate confusion is a temporary coordination failure being solved by explicit tagging protocols. The academic peer review system sees a piton: the ritual of enforcing substrate discipline persists through inertia despite being largely performative. The analytical observer sees a tangled rope: the substrate distinction serves a genuine coordination function, but the ambiguity also enables systematic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The good-faith truth seeker is identity-locked: structurally mobile (could learn to recognize substrate shifts) but cognitively trapped by identity fusion. Their identity as a truth-seeker is constituted through the assumption that epistemic contests are about reality, not status. Recognizing substrate shifts would require abandoning this identity frame. The strategic frame shifter is a beneficiary with arbitrage exit: can shift substrates at will and experiences the ambiguity as a tool rather than a trap. The reflective participant is constrained: has learned to see substrate shifts but faces high social costs to exit or call out frame confusion. The methodological reform coalition is organized and mobile: building alternative pathways (registered reports, adversarial collaboration) that make substrate shifts visible and costly. The academic peer review system is institutional but constrained: supposed to enforce substrate discipline but cannot do so effectively without access to author intent. The analytical observer sees the full structure: both coordination function and extraction mechanism coexist.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the substrate split is neither purely coordinative nor purely extractive. The coordination function is real: distinguishing empirical from social claims enables specialized epistemic tools (experiment vs negotiation, evidence vs rhetoric). But the extraction mechanism is also real: actors who can shift substrates strategically extract from those who cannot. The constraint is a tangled rope from the analytical perspective because both functions coexist structurally. The good-faith truth seeker's snare classification is their genuine experience — they are trapped by identity fusion and cannot see the substrate shift. The strategic frame shifter's rope classification is their genuine experience — they benefit from the ambiguity. The scaffold perspective is a real structural feature — methodological reforms are building alternative pathways. The piton perspective is a real observation — peer review has degraded into theater. No single type is 'the' answer; the presheaf over observation sites captures the full structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substrate_detection_threshold,
    'What observable threshold distinguishes genuine substrate confusion (epistemic error) from strategic frame-shifting (social extraction)?',
    'Longitudinal analysis of argument patterns: does the actor maintain substrate consistency when it''s costly to do so, or only when it''s advantageous? Adversarial collaboration protocols that force pre-commitment to substrate.',
    'If threshold is low (easy to detect): most frame confusion is strategic, and the constraint is more extractive than coordinative. If threshold is high (hard to detect): most confusion is genuine, and the constraint is more coordinative than extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substrate_detection_threshold, empirical, 'Observable threshold for distinguishing genuine confusion from strategic shifting').

omega_variable(
    identity_lock_mechanism,
    'Is the good-faith truth seeker''s inability to recognize substrate shifts due to cognitive architecture (humans are bad at meta-level reasoning) or identity fusion (recognizing the shift would require abandoning self-concept as truth-seeker)?',
    'Experimental intervention: can explicit substrate-tagging training break the lock, or does it require identity work (therapy, deprogramming, community support)? Comparison of debiasing success rates for cognitive vs identity-based interventions.',
    'If cognitive: the lock is a bug that can be patched with better epistemic hygiene. If identity-based: the lock is a feature of how humans construct meaning, and breaking it requires deeper transformation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism, conceptual, 'Whether substrate blindness is cognitive or identity-based').

omega_variable(
    methodological_reform_sufficiency,
    'Do registered reports, adversarial collaboration, and pre-commitment protocols actually prevent substrate confusion, or do they just move the exploitation to a different layer (gaming the pre-commitment process)?',
    'Longitudinal tracking of disputes in fields that have adopted vs not adopted these protocols. Do registered reports reduce substrate confusion rates, or do strategic actors learn to game the registration process?',
    'If sufficient: scaffold perspective confirmed — the sunset is real. If insufficient: the reforms are themselves theater, and the piton perspective applies to the reform movement itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methodological_reform_sufficiency, empirical, 'Whether methodological reforms prevent or merely displace substrate exploitation').

omega_variable(
    parable_transmission_dependency,
    'Is the substrate split downstream of parable-as-transmission-layer (the ambiguity is inherent to narrative knowledge transfer) or independent (the split would exist even with non-narrative transmission)?',
    'Cross-cultural comparison: do cultures with non-narrative epistemic traditions (formal logic, mathematical proof, ritual transmission) exhibit the same substrate confusion patterns? Historical analysis: did the split emerge with literacy and narrative dominance, or does it predate written language?',
    'If dependent: the split is a contingent feature of how humans encode knowledge in stories, and alternative transmission layers could eliminate it. If independent: the split is a deeper feature of social cognition, and narrative is just one manifestation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(parable_transmission_dependency, conceptual, 'Whether substrate split is contingent on narrative transmission or independent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(empirical_social_substrate_split, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ess_tr_t0, empirical_social_substrate_split, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ess_tr_t8, empirical_social_substrate_split, theater_ratio, 8, 0.48).
narrative_ontology:measurement(ess_tr_t16, empirical_social_substrate_split, theater_ratio, 16, 0.58).

% Extraction over time
narrative_ontology:measurement(ess_be_t0, empirical_social_substrate_split, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ess_be_t8, empirical_social_substrate_split, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(ess_be_t16, empirical_social_substrate_split, base_extractiveness, 16, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(empirical_social_substrate_split, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is downstream of parable_as_transmission_layer (mountain): narrative knowledge transfer inherently blurs empirical and social substrates because stories encode both factual claims and social signals. The substrate split is a contingent feature of how humans encode knowledge in narratives, not an immutable law of cognition. If alternative transmission layers (formal logic, mathematical proof, ritual transmission) were dominant, the split might not exist or might take a different form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
