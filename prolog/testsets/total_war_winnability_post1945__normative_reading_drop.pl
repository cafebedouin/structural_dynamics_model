% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__normative_reading_drop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__normative_reading_drop, []).

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
 *   constraint_id: total_war_winnability_post1945__normative_reading_drop
 *   human_readable: Total War Winnability Post-1945: Normative Illegitimacy Reading
 *   domain: international_relations/strategic_studies/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE reading of the contested kernel
 *   'total war winnability post-1945': the normative delegitimacy reading.
 *   The kernel describes a single persisting commitment—whether total war
 *   remains a viable strategic option after 1945—that different analytical
 *   traditions read differently. This normative reading frames the constraint
 *   as a Tangled Rope: Article 2(4) of the UN Charter and the development of
 *   humanitarian law coordinate state behavior and protect civilian
 *   populations while simultaneously extracting compliance costs from
 *   revisionist actors. The constraint combines genuine coordination (all
 *   states benefit from protection of their own civilians) with asymmetric
 *   extraction (revisionist states bear disproportionate cost of norm
 *   enforcement). The reading's core claim is that total war became
 *   normatively illegitimate through formal and soft law mechanisms—through
 *   the commitment system of international law—rather than through structural
 *   impossibility (nuclear deterrence) or cultural drift. This reading
 *   coexists with the structural contraction reading (nuclear weapons made
 *   total war strategically irrational) and the strategic culture reading
 *   (norms evolved through institutional practice), but each reading produces
 *   different beneficiary/victim distributions and different epsilon values.
 *   Sibling readings are separate constraint stories linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - Global Civilian Populations: Primary victim (powerless/trapped) — protected in principle by Article 2(4) and IHL, but remain vulnerable through enforcement gaps and dual-use infrastructure
 *   - International Humanitarian Law Coalition: Organized beneficiary (organized/constrained) — UN state coalitions, ICRC, human rights organizations that benefit from norm coordination while bearing enforcement costs
 *   - Status Quo Great Powers: Primary beneficiary (institutional/arbitrage) — Article 2(4) protects their territorial integrity; can arbitrage between enforcement and non-enforcement depending on interests
 *   - Revisionist Military Planners: Primary victim (powerful/constrained) — face delegitimization of classical total war doctrine; constrained exit through ICC threat and sanctions
 *   - Post-Colonial Norm-Builders: Organized agent (organized/mobile) — emerging consensus coalition building stronger normative consensus; mobile exit between frameworks
 *   - International Legal Order: Meta-institutional actor (institutional/arbitrage) — coordinates state behavior through normative delegation; arbitrages enforcement gaps
 *   - Analytical Observer: Civilizational observer (analytical/analytical) — risks naturalizing institutional choice as immutable physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__normative_reading_drop, 0.32).
domain_priors:suppression_score(total_war_winnability_post1945__normative_reading_drop, 0.48).
domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, extractiveness, 0.32).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__normative_reading_drop, tangled_rope).
narrative_ontology:human_readable(total_war_winnability_post1945__normative_reading_drop, "Total War Winnability Post-1945: Normative Illegitimacy Reading").
narrative_ontology:topic_domain(total_war_winnability_post1945__normative_reading_drop, "international_relations/strategic_studies/commitment_systems").

domain_priors:requires_active_enforcement(total_war_winnability_post1945__normative_reading_drop).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__normative_reading_drop, '6afd7e05-c8b5-4df6-8657-1e1841d78fde').
narrative_ontology:cs_kernel_codification('6afd7e05-c8b5-4df6-8657-1e1841d78fde', formalized).
narrative_ontology:cs_authority_grounding('6afd7e05-c8b5-4df6-8657-1e1841d78fde', extraction).
narrative_ontology:cs_interpretation_layer_present('6afd7e05-c8b5-4df6-8657-1e1841d78fde').
narrative_ontology:cs_reading_relation('6afd7e05-c8b5-4df6-8657-1e1841d78fde', total_war_winnability_post1945__structural_contraction_reading, influences).
narrative_ontology:cs_reading_relation('6afd7e05-c8b5-4df6-8657-1e1841d78fde', total_war_winnability_post1945__strategic_culture_drift, coexists_with).
narrative_ontology:cs_axiom('6afd7e05-c8b5-4df6-8657-1e1841d78fde', foundational, article_2_4_delegitimizes_total_war_doctrine).
narrative_ontology:cs_axiom_status(article_2_4_delegitimizes_total_war_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('6afd7e05-c8b5-4df6-8657-1e1841d78fde', article_2_4_delegitimizes_total_war_doctrine, conventional).
narrative_ontology:cs_axiom('6afd7e05-c8b5-4df6-8657-1e1841d78fde', foundational, humanitarian_law_internalization_enforced_through_institutions).
narrative_ontology:cs_axiom_status(humanitarian_law_internalization_enforced_through_institutions, holdable).
narrative_ontology:cs_axiom_grounding('6afd7e05-c8b5-4df6-8657-1e1841d78fde', humanitarian_law_internalization_enforced_through_institutions, conventional).
narrative_ontology:cs_reference_frame('6afd7e05-c8b5-4df6-8657-1e1841d78fde', collective_security_framework_1945).
narrative_ontology:cs_drift_state('6afd7e05-c8b5-4df6-8657-1e1841d78fde', contemporary_enforcement_gap, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6afd7e05-c8b5-4df6-8657-1e1841d78fde', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, global_civilian_populations).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__normative_reading_drop, international_legal_order).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, revisionist_states).
narrative_ontology:constraint_victim(total_war_winnability_post1945__normative_reading_drop, military_planners_classical_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Face absolute prohibition on being targeted as combatants, yet remain structurally vulnerable through precision degradation, dual-use infrastructure, and siege/starvation mechanisms. No exit; bearing full extraction through vulnerability despite normative shield. Powerless agents trapped in the constraint's target zone.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERNATIONAL HUMANITARIAN LAW COALITION (TANGLED ROPE) — Organized state and NGO actors that benefit from the normative order (coordination function: protects their own civilians in their wars) while bearing costs of constraint (must forgo certain military efficiencies). Genuine coordination mixed with asymmetric extraction against actors who violate norms.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: STATUS QUO GREAT POWERS (ROPE) — Primary beneficiaries of the normative order. Article 2(4) and IHL frameworks protect their territorial integrity and military dominance while simultaneously constraining potential challengers. Arbitrage exit: can withdraw from specific instruments while maintaining institutional pressure through UN/ICC mechanisms. Experience the constraint as coordination—preserving international order that preserves their position.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REVISIONIST MILITARY PLANNERS (SNARE) — Face asymmetric constraint from the normative order. Classical total war doctrine (decisive victory through civilian targeting) is delegitimized without being physically eliminated. Must pursue objectives under normative handicap while status quo powers retain institutional enforcement capacity. Constrained exit (can violate but face ICC/sanctions).
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: POST-COLONIAL NORM-BUILDERS (SCAFFOLD) — Organized coalitions (Global South states, human rights networks) perceive the constraint as temporary coordination failure transitioning toward stronger normative consensus. Mobile exit: can retreat to older frameworks (Westphalian sovereignty) or advance to new ones (humanitarian intervention doctrine). Scaffold classification: sunset is internal to this reading—strengthening of normative illegitimacy itself is the enforcement mechanism replacing military capacity.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL ORDER (META-INSTITUTIONAL) (TANGLED ROPE) — The legal order itself coordinates state behavior through normative delegation (Article 2(4), IHL) while extracting legitimacy authority from states. Benefits through institutionalization; bears cost of enforcement gaps and norm violations. Arbitrage: can cite enforcement failures to justify institutional strengthening (ICC expansion) or retreat to state sovereignty (treaty withdrawal).
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: DETERRENCE THEATER (PITON) — The deterrence framework (Mutually Assured Destruction, nuclear taboo extension) persists as performative constraint despite its mechanism being decoupled from conventional war planning. Theater ratio high: extensive doctrinal literature, strategic studies investment, and institutional maintenance far exceed the actual behavioral suppression of total war doctrine. Piton reflects degradation—the constraint is maintained through institutional inertia and ritual rather than active enforcement.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational universal perspective, nuclear weapons have made total war strategically irrational (first-strike vulnerability eliminates decisive victory). This reading frames the normative illegitimacy as downstream effect of physics: nuclear deterrence creates an immutable constraint that normative orders merely formalize. However, this risks naturalizing what is actually an institutional choice—other states retain total war as doctrine despite understanding nuclear risk.
constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__normative_reading_drop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(total_war_winnability_post1945__normative_reading_drop, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(total_war_winnability_post1945__normative_reading_drop, TR),
    TR >= 0.70.

:- end_tests(total_war_winnability_post1945__normative_reading_drop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. This reading's core claim is that normative illegitimacy rather than structural impossibility constrains total war. Measured from baseline 1945 through 2026, extractiveness has risen (0.18→0.32) as enforcement mechanisms have strengthened (ICC, targeted sanctions, norm internalization in military academies). The moderate value reflects that the constraint is real but has significant compliance gaps—many actors retain total war doctrine despite delegitimization, enforcement is selective (P5 vetoes), and the normative prohibition functions partly through institutional leverage rather than internalized consensus. If the constraint operated through pure institutional power, extractiveness would be higher (0.50+). The intermediate value captures the constraint's true character: a coordination mechanism backed by selective enforcement. Suppression (0.48): Moderate-high. The constraint suppresses alternatives (total war doctrine) through multiple mechanisms: formal prohibition (Article 2(4)), institutional sanctions (UN, ICC), reputational costs, and battlefield enforcement (IHL compliance monitors). However, suppression is not absolute—revisionist actors retain total war as latent doctrine, strategic ambiguity persists at conflict margins (hybrid warfare, asymmetric tactics), and enforcement gaps are exploited. Theater ratio (0.55): Moderate-high. Significant performative element: deterrence doctrine, strategic studies investment, and treaty ceremonies far exceed the actual behavioral constraint. Many actors pay nominal compliance (sign IHL instruments, train on humanitarian law) while maintaining covert doctrine. Rising theater ratio over the interval reflects increasing gap between formal norm adoption and actual operational doctrine.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival divergence central to the committer frame. Status quo powers experience it as Rope (coordination protecting their civilians). Revisionist planners experience it as Snare (normative delegitimization without internal acceptance, bearing extraction from enforcement mechanisms). Organized international coalitions experience it as Tangled Rope (both coordinating civilian protection and extracting enforcement costs from violators). Civilian populations experience it as Snare (theoretical protection without guarantee). The post-colonial coalition experiences it as Scaffold (temporary coordination failure being resolved through strengthening norms). The deterrence theater experiences it as Piton (performative constraint maintained through ritual). The analytical observer risks Mountain classification (naturalizing institutional choice as physical law). The perspectival gaps reveal that the constraint is fundamentally institutional—its classification depends on structural position relative to Article 2(4) and IHL enforcement mechanisms, not on objective facts about total war winnability.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's power level, exit capacity, and structural position in the extraction flow. Status quo great powers are beneficiaries with arbitrage exit (low d, negative χ). Revisionist planners are victims with constrained exit (high d, high χ). International legal order institutions are beneficiaries with arbitrage (low d). Civilian populations are victims with trapped exit (maximum d, maximum χ). The organized coalition has mixed position—beneficiary of norm coordination, victim of enforcement costs, with constrained exit (moderate d, moderate χ). The derivation chain follows: beneficiary + arbitrage → low d → negative f(d) → negative χ; victim + trapped → high d → high f(d) → high χ. The perspectival gap arises from the same base constraint having different directionality values for different actors—status quo powers and revisionists experience opposite extraction flows.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves mandatrophy by clarifying that the normative frame produces a Tangled Rope: genuine coordination (all states benefit from their own civilians being protected) coexists with asymmetric extraction (revisionist states bear disproportionate cost of norm enforcement). The constraint is NOT pure coordination (Rope) because enforcement mechanisms target revisionist actors asymmetrically. It is NOT pure extraction (Snare) because the coordination function (protecting civilians) is real and benefits all states, including those that violate norms when their own civilians are threatened. The Tangled Rope classification is stable across the analytical observer's perspectives because the structural features are invariant: coordination function + asymmetric enforcement + active enforcement mechanisms are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_internalization_depth,
    'Has the normative illegitimacy of total war become genuinely internalized in strategic culture, or does it remain enforced primarily through external institutional pressure and material disincentives?',
    'Analysis of strategic doctrine in non-aligned states (India, Brazil, Turkey); tracking of military academies'' treatment of total war doctrine; examination of strategic culture textbooks and official war colleges. Compare internalization rates across nuclear-armed vs non-nuclear states.',
    'If internalized: the constraint is primarily Rope (coordination problem solved via shared understanding). If externally enforced: the constraint remains Snare for revisionist actors (normative illegitimacy without internal acceptance). Classification shifts upward in extractiveness if external enforcement dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_internalization_depth, empirical, 'Depth of normative internalization of total war illegitimacy in strategic culture').

omega_variable(
    article_2_4_enforcement_gap,
    'How large is the gap between the stated prohibition on force in Article 2(4) and actual enforcement capacity through UN mechanisms?',
    'Catalog of UN vetoes blocking enforcement (P5 blocking patterns), comparison of Security Council responses to interventions by permanent members vs non-permanent members, analysis of enforcement action latency.',
    'If gap is severe (enforcement <20% of violations): the constraint is de-legitimizing without suppression—a pure signaling mechanism (Rope class). If enforcement is effective (>60%): the constraint is backed by institutional power (Tangled Rope to Snare). Classification and suppression metric depend on this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_2_4_enforcement_gap, empirical, 'Enforcement gap between Article 2(4) statement and Security Council capacity').

omega_variable(
    revisionist_doctrine_persistence,
    'To what extent do revisionist military doctrines (Russia, China, non-state actors) retain total war objectives despite normative delegitimization?',
    'Doctrine analysis: examine official military manuals, strategic concepts, and operational planning documents for references to civilian targeting, unlimited objectives, or unconditional victory. Compare pre-1945 total war doctrine with contemporary revisionist planning.',
    'If total war doctrine survives in revisionist planning: the constraint is actively resisted (Snare for revisionists, extraction from planners attempting to pursue illegitimate strategies). If doctrine has been abandoned: the constraint has achieved genuine behavioral shift (Rope or Scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revisionist_doctrine_persistence, empirical, 'Persistence of total war objectives in contemporary revisionist military doctrine').

omega_variable(
    humanitarian_law_compliance_variance,
    'Does compliance with IHL norms correlate with strategic culture (internalized norms) or with enforcement capacity and material incentives?',
    'Multivariate analysis: track IHL violation rates against (a) enforcement capacity (distance to ICC, Security Council composition), (b) strategic culture indicators (UN voting patterns, treaty ratification), (c) material factors (military capacity, conflict intensity). Partial regression to identify which predicts compliance.',
    'If strategic culture dominates: norms have been internalized; constraint is cooperative (Rope). If enforcement/incentives dominate: constraint is coercive (Snare). Classification revises based on explanatory dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_law_compliance_variance, empirical, 'Compliance drivers for IHL norms: internalization vs enforcement vs incentives').

omega_variable(
    kernel_reading_distinctness,
    'Is this normative reading (illegitimacy through treaty and soft law) distinct from the structural contraction reading (total war winnability eliminated by nuclear weapons) and strategic culture reading (norms drifted through institutional evolution)?',
    'Structural analysis: examine whether the three readings produce different policy implications, different ε values under the same empirical conditions, and different beneficiary/victim distributions. This omega documents the committer frame''s core claim—that one kernel (total war winnability post-1945) instantiates three distinct constraints.',
    'If readings are genuinely distinct: the normative reading focuses on Article 2(4) and IHL as commitment system kernels; the structural reading focuses on nuclear deterrence physics; the culture reading focuses on institutional evolution. Different constraint stories, different ε, different network relationships. If readings collapse: the kernel is not meaningful; author fewer stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_distinctness, conceptual, 'Distinctness of normative reading from structural and cultural siblings in the total-war-winnability kernel family').

omega_variable(
    false_summit_natural_law_risk,
    'Does the mountain perspective''s frame (total war becomes irrational because physics eliminates victory) naturalize what is actually an institutional/normative choice?',
    'Counterfactual analysis: examine non-nuclear-armed states and non-aligned actors. If they retain total war doctrine despite the same physics, the mountain frame fails—the constraint is not immutable natural law. If they also adopt normative illegitimacy, investigate whether through physics or through institutional socialization (UN socialization, treaty pressure).',
    'If natural law frame fails: mountain classification is a false summit (engine triggers FSM override). True constraint is Tangled Rope or Snare (institutional, not physical). Extractiveness may increase if physicalist framing masks coercive institutional enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law_risk, empirical, 'False summit risk: whether mountain perspective naturalizes institutional choice as physical law').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__normative_reading_drop, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(twn_tr_t0, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 0, 0.42).
narrative_ontology:measurement(twn_tr_t15, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 15, 0.48).
narrative_ontology:measurement(twn_tr_t30, total_war_winnability_post1945__normative_reading_drop, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(twn_be_t0, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(twn_be_t15, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 15, 0.25).
narrative_ontology:measurement(twn_be_t30, total_war_winnability_post1945__normative_reading_drop, base_extractiveness, 30, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(twn_su_t0, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(twn_su_t15, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(twn_su_t30, total_war_winnability_post1945__normative_reading_drop, suppression_requirement, 30, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__normative_reading_drop, enforcement_mechanism).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__structural_contraction_reading).
narrative_ontology:affects_constraint(total_war_winnability_post1945__normative_reading_drop, total_war_winnability_post1945__strategic_culture_drift).

% DUAL FORMULATION NOTE:
% The total-war-winnability kernel admits three readings, each producing a distinct constraint with different ε values. The normative reading focuses on Article 2(4) and IHL as commitment-system kernels (ε≈0.32, Tangled Rope). The structural reading focuses on nuclear deterrence eliminating strategic rationality for total war (ε≈0.08, Mountain). The culture reading focuses on institutional practice and military academy evolution (ε≈0.42, Tangled Rope). These are not alternative measurements of one constraint—they are different constraints instantiated by the same kernel under different readings. Network links capture the family relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(total_war_winnability_post1945__normative_reading_drop, powerful, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
