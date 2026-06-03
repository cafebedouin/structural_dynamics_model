% ============================================================================
% CONSTRAINT STORY: constitutional_interpretive_authority__coordinate_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_interpretive_authority__coordinate_construction_reading, []).

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
 *   constraint_id: constitutional_interpretive_authority__coordinate_construction_reading
 *   human_readable: Constitutional Interpretive Authority — Coordinate Construction Reading
 *   domain: constitutional_law/political_theory/jurisprudence
 *
 * SUMMARY:
 *   The coordinate construction reading of constitutional interpretive
 *   authority posits that no single branch possesses final authority to
 *   determine constitutional meaning; instead, constitutional interpretation
 *   emerges through inter-branch dialogue, contestation, and bargaining, with
 *   disputes resolved through political mechanisms (amendment, appointment,
 *   budget control) rather than singular adjudication. This reading sits in
 *   structural tension with two sibling readings: the judicial supremacy
 *   reading (courts possess final authority via guardianship of fundamental
 *   rights) and the parliamentary supremacy reading (legislatures possess
 *   final authority, with no judicial override power). All three readings
 *   coexist as live positions within contemporary democratic
 *   constitutionalism, held by different institutional actors, legal
 *   theorists, and political movements. The coordinate construction reading
 *   dominates in federal systems with robust separation of powers and
 *   multiple veto points (United States, Australia, Canada, Germany); the
 *   judicial supremacy reading has gained ground in post-WWII constitutional
 *   courts and rights-based frameworks; parliamentary supremacy persists in
 *   Westminster traditions and some Nordic systems. The constraint exhibits
 *   genuine coordination function (branches constrain each other to prevent
 *   domination) and genuine extraction (institutional contestation imposes
 *   costs on litigants and ordinary subjects seeking clear legal rules). The
 *   theater ratio (0.64) reflects that the formal doctrine of
 *   separated-but-coordinate powers increasingly masks the reality of
 *   inter-branch bargaining and political contestation, rendering the
 *   doctrine increasingly performative as actual power distribution drifts
 *   away from formal separation.
 *
 * KEY AGENTS:
 *   - Litigants seeking settled law: powerless/trapped — bear full cost of interpretive uncertainty without voice
 *   - Institutional actors within each branch (Congress, courts, executive agencies): moderate-institutional/constrained — constrained by other branches, benefit from participation in interpretive contests
 *   - Constitutional reform coalitions: organized/mobile — can mobilize supermajorities for amendment or convention
 *   - Legislative branch: institutional/constrained — constrained by amendment/judicial/executive threats, benefits from budgetary and electoral leverage
 *   - Judicial branch: institutional/constrained — constrained by appointment politics and jurisdiction-stripping threats, benefits from independent interpretation authority and precedent-binding
 *   - Executive branch: institutional/constrained — constrained by appropriations and impeachment, benefits from broad executive power interpretation
 *   - Separation of powers doctrine: institutional/arbitrage — maintains legitimacy cover for political contestation (piton perspective)
 *   - Analytical observer: analytical/analytical — sees coordinate construction as genuine coordination achievement (rope perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, 0.48).
domain_priors:suppression_score(constitutional_interpretive_authority__coordinate_construction_reading, 0.58).
domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_interpretive_authority__coordinate_construction_reading, "Constitutional Interpretive Authority — Coordinate Construction Reading").
narrative_ontology:topic_domain(constitutional_interpretive_authority__coordinate_construction_reading, "constitutional_law/political_theory/jurisprudence").

domain_priors:requires_active_enforcement(constitutional_interpretive_authority__coordinate_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_interpretive_authority__coordinate_construction_reading, 'deae88ef-0a7b-47f1-9fb4-6fdfa04f0006').
narrative_ontology:cs_kernel_codification('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', distributed).
narrative_ontology:cs_authority_grounding('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', lineage).
narrative_ontology:cs_interpretation_layer_present('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006').
narrative_ontology:cs_reading_relation('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', constitutional_interpretive_authority__parliamentary_supremacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', constitutional_interpretive_authority__judicial_supremacy_reading, coexists_with).
narrative_ontology:cs_axiom('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', foundational, no_final_authority_branch).
narrative_ontology:cs_axiom_status(no_final_authority_branch, holdable).
narrative_ontology:cs_axiom_grounding('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', no_final_authority_branch, deontological).
narrative_ontology:cs_axiom('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', foundational, political_mechanisms_resolve_disputes).
narrative_ontology:cs_axiom_status(political_mechanisms_resolve_disputes, holdable).
narrative_ontology:cs_axiom_grounding('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', political_mechanisms_resolve_disputes, instrumental).
narrative_ontology:cs_reference_frame('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', coordinate_multipart_authority).
narrative_ontology:cs_drift_state('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', contemporary_polarized_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('deae88ef-0a7b-47f1-9fb4-6fdfa04f0006', '').
narrative_ontology:cs_kernel_id(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, political_compromise_agents).
narrative_ontology:constraint_beneficiary(constitutional_interpretive_authority__coordinate_construction_reading, incremental_constitutional_development).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, certainty_seekers).
narrative_ontology:constraint_victim(constitutional_interpretive_authority__coordinate_construction_reading, stable_legal_rule_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITIGANT SEEKING SETTLED LAW (SNARE) — Cannot exit constitutional uncertainty; bears full cost of interpretive contestation without remedy. The individual litigant lacks standing to resolve inter-branch disputes and faces extraction through legal unpredictability, inconsistent application of constitutional norms, and the burden of navigating multiple coordinate authorities offering incompatible interpretations. Maximum experienced extraction — no exit, no voice in interpretation, bearing costs of instability.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INSTITUTIONAL ACTOR WITHIN ONE BRANCH (TANGLED ROPE) — Constrained by need to coordinate with other branches and by threat of override (budget cuts, appointments, constitutional amendment). Also benefits from participation in interpretive contests — can advance institutional position and policy preferences through inter-branch dialogue. Significant extraction but not maximal — genuine agency within constraints and real benefits from the coordination mechanism.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSTITUTIONAL REFORM COALITION (ROPE) — Mobile agents (amendment advocates, institutional reformers, supermajority coalitions) see coordinate construction as a genuine coordination problem that they can solve through amendment or constitutional convention. Benefits from the mechanism itself (pluralistic input) and from their ability to achieve structural change. Effective extraction is low — these agents have agency and exit paths (coalition-building toward amendment).
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGISLATIVE BRANCH (TANGLED ROPE) — Constrained by potential constitutional amendment, judicial nullification of its acts, executive veto, and budget-dependent agencies. Also benefits from coordinate construction: can advance legislative policy through interpretation, use budgetary power to constrain judicial or executive action, and claim legitimacy as elected representative of the people. Mixed coordination-extraction dynamic — genuine coercive constraint alongside real benefit from participation in the dialogue.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUDICIAL BRANCH (TANGLED ROPE) — Constrained by threat of constitutional amendment, legislative jurisdiction-stripping, executive non-compliance, and appointment politics. Also benefits: can shape constitutional meaning through precedent, claim institutional independence and expertise, and advance judicial institutional interests through interpretive position-taking. Mixed coordination-extraction dynamic — constrained but not dominated, with real benefits from the dialogue.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: EXECUTIVE BRANCH (TANGLED ROPE) — Constrained by legislative appropriations, judicial oversight of executive action, and impeachment threats. Also benefits: can interpret constitutional executive power broadly, advance policy through administrative action pending judicial review, and leverage appointment power to influence judiciary and agencies. Mixed coordination-extraction dynamic — powerful institutional actor with real constraints and real benefits.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: SEPARATION OF POWERS DOCTRINE (PITON) — The formal doctrine of separated-but-coordinate powers persists through institutional inertia, but its functional coordination capacity has degraded over centuries of institutional specialization and power concentration. The doctrine is maintained theatrically as legitimacy cover for what is actually inter-branch contestation and bargaining. Theater_ratio is high (0.64) because the formal separation masks the reality that power is distributed through political mechanisms rather than structural boundaries.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, coordinate construction represents a genuine coordination achievement: the constraint solves the problem of how multiple power centers can negotiate shared governance without any single authority dictating outcomes. The mechanism works through institutional checks, electoral cycles, amendment procedures, and bargaining. This perspective does not claim the mechanism is frictionless (it is not), but sees the friction itself as the coordination function — power centers constrain each other to prevent domination.
constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(constitutional_interpretive_authority__coordinate_construction_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_interpretive_authority__coordinate_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(constitutional_interpretive_authority__coordinate_construction_reading, TR),
    TR >= 0.70.

:- end_tests(constitutional_interpretive_authority__coordinate_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, slightly elevated from simple average due to the constraint's structural feature that institutional actors can maintain interpretive instability strategically to preserve power options. Judicial actors may favor ambiguity in precedent to maintain flexibility; legislative actors may favor competing interpretations to preserve action space; executive actors may favor broad executive power readings. The measurement shows rising extractiveness from 0.35 (early period with stronger shared constitutional culture) to 0.48 (contemporary, with more polarized inter-branch contestation). Suppression (0.58): Moderate-high. The constraint suppresses clarity-seeking agents' alternatives through the simple fact that all three branches participate in interpretation — no single agent can exit to a unified rule-making process. Supermajority amendment requirements create high barriers to formal constitutional revision, forcing parties to accept inter-branch contestation as the only available adjustment mechanism. Suppression rises over the interval as amendment has become rarer and inter-branch contestation more intense. Theater ratio (0.64): Moderate-high. The formal doctrine of separated-but-coordinate powers is increasingly ceremonial. Actual power distribution reflects institutional bargaining, appointment politics, and budget leverage more than formal role boundaries. Inter-branch dialogue is real, but the performative framing of it as 'constitutional interpretation' masks its fundamentally political character.
 *
 * PERSPECTIVAL GAP:
 *   The litigant sees snare (no exit, no voice, bearing pure costs). Institutional actors see tangled_rope (constrained but with real benefits from bargaining capacity). Reform coalitions see rope (amendment pathway available, genuine coordination mechanism). The separation of powers doctrine sees itself as rope but is functionally piton (performative). The analytical observer sees rope (genuine coordination of multiple power centers). The central perspectival gap lies between agents seeking legal clarity (who experience the constraint as snare, bearing costs of instability) and institutional actors who benefit from strategic ambiguity (who experience constraint as tangled_rope or rope, deriving benefit from interpretive flexibility). The coordinate construction reading distributes authority among branches and resolves this gap through the argument that pluralistic input (all branches participating) is a legitimacy value that outweighs clarity. But the measurement shows rising extractiveness — suggesting the constraint is drifting toward snare (pure institutional self-interest maintaining instability) rather than remaining as rope (genuine coordination mechanism).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's power level, exit options, and structural relationship to the interpretive authority flow. Litigants (powerless/trapped) experience high d (0.95) → high f(d) (1.42) → high experienced extraction. Institutional actors constrained by other branches experience moderate d (0.50–0.65) → moderate f(d) (0.65–1.00) → moderate extraction, with benefits from bargaining power. Reform coalitions with amendment pathways experience lower d (0.40) → lower f(d) → lower extraction. The analytical observer at civilizational scope experiences d around 0.72 (observer position on contestation) → f(d) of 1.15, but classified as rope because the perspective sees the mechanism as genuinely coordinating multiple power sources. The constraint's chi (effective extraction) varies significantly across perspectives due to both directionality differences and scope modifiers: national scope (σ=1.0) for domestic constituencies, but global scope (σ=1.2) for comparative constitutional analysis makes extraction appear amplified internationally.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that coordinate construction is genuine tangled_rope (mixed coordination and extraction) rather than pure rope or pure snare. The coordination function is real: branches do constrain each other, preventing any single authority from dominating; inter-branch dialogue does produce some constitutional development through political negotiation. But extraction is also real: institutional actors strategically maintain interpretive ambiguity to preserve power options; litigants and ordinary subjects bearing costs of uncertainty have no voice; the clarity-seeking common good is suppressed. The rising measurements (extractiveness 0.35→0.48, theater_ratio 0.48→0.64, suppression 0.45→0.58) suggest the constraint is drifting from genuine tangled_rope (coordination still functioning) toward snare (extraction dominating) or piton (doctrine performative). The mandatrophy is resolved by recognizing that all three readings (coordinate construction, judicial supremacy, parliamentary supremacy) are structurally plausible interpretations of the same constitutional kernel, but the coordinate construction reading's actual practice increasingly deviates from its theory. The reading coexists with judicial and parliamentary supremacy readings — neither forecloses the other — but the contemporary drift shows de facto drift toward judicial supremacy (appellate review as final arbiter) in many domains, undercutting the coordinate construction reading's lived plausibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_instability_tolerance,
    'How much constitutional interpretive instability can a political system tolerate before the coordinate construction mechanism degrades into pure contestation (Snare) or collapses into judicial/parliamentary supremacy?',
    'Comparative constitutional analysis: correlation between volatility in constitutional interpretation and institutional legitimacy loss; longitudinal studies of democracies with coordinate construction vs. supremacy regimes; measurement of litigant-perceived legal certainty under each regime',
    'If tolerance is high (>60% instability tolerated): coordinate construction remains rope/tangled_rope. If tolerance is low (<30% instability): the reading devolves into snare or foreclosure of coordinate construction by supremacy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_instability_tolerance, empirical, 'Constitutional system tolerance threshold for interpretive instability').

omega_variable(
    de_facto_judicial_supremacy_creep,
    'Does the coordinate construction reading''s actual practice tend toward de facto judicial supremacy through repeated appellate review and precedent-binding? I.e., is the reading theoretically coordinate but structurally drift toward judicial dominance over time?',
    'Institutional analysis of outcome reversals: how often does legislature override Court interpretation through amendment vs. legislative workaround vs. acceptance of judicial reading? How often does executive refuse judicial order? Multi-generational tracking of which branch''s interpretation of contested clauses (executive power, commerce power, individual rights) tends to prevail long-term.',
    'If drift is substantial: the coordinate construction reading is aspirational/performative (Piton) rather than structural (Rope). If no drift: reading is sustained as genuine coordinate authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(de_facto_judicial_supremacy_creep, empirical, 'Whether coordinate construction drifts toward judicial supremacy in practice').

omega_variable(
    amendment_as_coordination_mechanism_viability,
    'Is constitutional amendment a viable mechanism for resolving interpretive disputes, or are supermajority requirements so high that amendment effectively freezes the constitution while inter-branch contestation becomes the only real adjustment mechanism?',
    'Empirical analysis of amendment frequency and amendment success rates for interpretively contested provisions; comparison of amending-through-interpretation vs. formal amendment timelines; study of institutional strategies to bypass amendment through creative interpretation or executive/legislative power expansion',
    'If amendment is viable: coordinate construction is genuine rope (agent can reach exits). If amendment is effectively frozen: coordinate construction collapses into bargaining without exit (degraded snare or tangled_rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(amendment_as_coordination_mechanism_viability, empirical, 'Viability of formal constitutional amendment as dispute-resolution mechanism').

omega_variable(
    contested_kernel_reading_selection,
    'Which reading of the constitutional interpretive authority kernel is this constraint instantiating? The coordinate construction reading coexists with parliamentary supremacy and judicial supremacy readings. Are these readings genuinely held by different parties (coexists_with), or does one reading logically foreclose the others within a single constitutional framework?',
    'Comparative constitutional law: study constitutions that formally enshrine one reading (e.g., Canadian Charter of Rights and Freedoms with strong judicial review vs. Westminster parliamentary sovereignty vs. EU treaty-based coordinate construction). Examine whether practitioners within each system could coherently hold the alternative reading, or whether the alternative is logically excluded by that system''s foundational commitments.',
    'If coexists_with: all three readings remain live; the constraint classifies as tangled_rope across all three stories. If forecloses: at least one reading is structurally excluded; reclassify the excluded reading or collapse the kernel. If influences: one reading creates pressure on others without fully excluding them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_reading_selection, conceptual, 'Nature of relationship between coordinate construction, parliamentary, and judicial supremacy readings').

omega_variable(
    political_contestation_as_legitimacy_mechanism,
    'In the coordinate construction reading, does inter-branch contestation serve as a legitimacy mechanism (ensuring multiple constituencies get voice) or as an extractive mechanism (preventing clear rules and extracting uncertainty costs from powerless agents)?',
    'Institutional ethnography: interviews with litigants, institutional actors, and constitutional scholars about whether contestation feels legitimate (pluralistic voice) or illegitimate (arbitrary instability). Measurement of perceived constitutional authority and institutional trust across systems with different readings. Analysis of which agents benefit from instability vs. which agents demand clarity.',
    'If legitimacy: coordinate construction remains tangled_rope (genuine coordination function). If extraction dominates: reclassify toward snare (contestation as pure extraction mechanism, clarification as common good that benefits powerless but is suppressed by institutional actors maintaining instability for strategic advantage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_contestation_as_legitimacy_mechanism, conceptual, 'Whether inter-branch contestation functions as legitimacy or extraction mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_interpretive_authority__coordinate_construction_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(const_coord_tr_t0, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(const_coord_tr_t50, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 50, 0.56).
narrative_ontology:measurement(const_coord_tr_t100, constitutional_interpretive_authority__coordinate_construction_reading, theater_ratio, 100, 0.64).

% Extraction over time
narrative_ontology:measurement(const_coord_be_t0, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(const_coord_be_t50, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(const_coord_be_t100, constitutional_interpretive_authority__coordinate_construction_reading, base_extractiveness, 100, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(const_coord_su_t0, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(const_coord_su_t50, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement(const_coord_su_t100, constitutional_interpretive_authority__coordinate_construction_reading, suppression_requirement, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_interpretive_authority__coordinate_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__parliamentary_supremacy_reading).
narrative_ontology:affects_constraint(constitutional_interpretive_authority__coordinate_construction_reading, constitutional_interpretive_authority__judicial_supremacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional interpretive authority kernel decomposes into three structurally distinct constraint stories, each modeling a different reading of the same foundational ambiguity: who decides what the constitution means? The coordinate construction reading models authority as dispersed among branches, resolved through political mechanisms; the parliamentary supremacy reading models authority as concentrated in the legislature; the judicial supremacy reading models authority as concentrated in the courts. Each story has its own epsilon, its own beneficiary/victim structure, its own measurement profile. They are linked through kernel relationships (reading_relations) that specify how each reading relates to its siblings: coexists_with (live in different frameworks simultaneously), forecloses (logically rules out in the same framework), or influences (creates structural pressure). All three readings coexist in contemporary constitutional practice — different democracies instantiate different readings, and within single democracies, different institutional actors advocate for different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
