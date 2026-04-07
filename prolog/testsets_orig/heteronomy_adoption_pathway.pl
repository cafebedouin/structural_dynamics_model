% ============================================================================
% CONSTRAINT STORY: heteronomy_adoption_pathway
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_heteronomy_adoption_pathway, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: heteronomy_adoption_pathway
 *   human_readable: Heteronomy Adoption Pathway: Internalization of Externally-Given Standards
 *   domain: moral_philosophy/psychology_of_agency/social_epistemology
 *
 * SUMMARY:
 *   The heteronomy adoption pathway describes the mechanism by which
 *   externally-given moral and epistemic standards are internalized by
 *   individual agents and subsequently misrecognized as self-authored. This
 *   constraint operates at the intersection of moral philosophy (autonomy vs
 *   heteronomy), psychology of agency (identity formation and cognitive
 *   closure), and social epistemology (peer effects on belief formation). The
 *   primary observable is the gap between agents' stated commitment to
 *   autonomous value formation and the genealogical evidence of peer-group
 *   conformity: individuals claim to have rationally derived or authentically
 *   chosen their values, but statistical analysis reveals that their values
 *   cluster tightly with their reference groups and shift predictably when
 *   reference groups change. The constraint exhibits genuine coordination
 *   benefits (shared standards enable social cooperation, mutual legibility,
 *   and collective action) alongside asymmetric extraction (suppression of
 *   genuine preference formation, status hierarchy maintenance, epistemic
 *   closure). The theater ratio (0.68) reflects that much of the discourse
 *   around rational autonomy and self-legislation functions performatively:
 *   agents justify their values with reasons and perform deliberation, but
 *   the genealogy reveals that the values were adopted through social
 *   transmission rather than rational derivation. The constraint is
 *   downstream of the recognition dependency mechanism (the
 *   mountain-classified need for social recognition) but adds a specific
 *   pathology: not just that agents need recognition, but that they
 *   internalize the standards by which recognition is granted and
 *   misrecognize those standards as their own.
 *
 * KEY AGENTS:
 *   - Identity-Fused Individual: Primary victim (powerless/identity_locked) — agent whose self-concept is constituted through internalized standards; cannot recognize external origin without identity dissolution; provides legitimacy to hierarchy through authentic endorsement
 *   - Reflective Agent: Secondary victim (moderate/constrained) — partial meta-cognitive awareness; sees tension between autonomy claims and conformity patterns; faces high exit costs (social penalty, identity disruption) but retains some agency
 *   - Genuine Self-Authorship: Abstract victim (powerless/trapped) — the epistemic capacity for authentic value formation; suppressed by the heteronomy mechanism; no advocate and no exit option
 *   - Status Hierarchies: Primary beneficiary (institutional/arbitrage) — the mechanism maintains hierarchical stability by producing agents who authentically endorse the values that justify the hierarchy; experiences constraint as pure coordination
 *   - Norm Enforcers: Secondary beneficiary (organized/mobile) — agents who police conformity and punish deviation; benefit from the constraint's suppression function while also being partly subject to it
 *   - Counter-Cultural Coalition: Organized agents (organized/mobile) — attempt to expose and exit the heteronomy mechanism; face the rebel uniform problem (risk reproducing the same internalization mechanism with different content)
 *   - Developmental Psychology Framework: Organized epistemic community (organized/mobile) — sees heteronomy as temporary developmental stage with sunset clause; models transition to autonomy as maturational process
 *   - Enlightenment Autonomy Ideal: Institutional discourse (institutional/constrained) — the legitimating narrative of rational self-legislation; persists theatrically despite empirical evidence of social transmission; sees own ideal as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the constraint as inevitable feature of social learning rather than recognizing it as contingent mechanism with variable intensity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heteronomy_adoption_pathway, 0.48).
domain_priors:suppression_score(heteronomy_adoption_pathway, 0.62).
domain_priors:theater_ratio(heteronomy_adoption_pathway, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heteronomy_adoption_pathway, extractiveness, 0.48).
narrative_ontology:constraint_metric(heteronomy_adoption_pathway, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(heteronomy_adoption_pathway, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(heteronomy_adoption_pathway, tangled_rope).
narrative_ontology:human_readable(heteronomy_adoption_pathway, "Heteronomy Adoption Pathway: Internalization of Externally-Given Standards").
narrative_ontology:topic_domain(heteronomy_adoption_pathway, "moral_philosophy/psychology_of_agency/social_epistemology").

domain_priors:requires_active_enforcement(heteronomy_adoption_pathway).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(heteronomy_adoption_pathway, status_hierarchies).
narrative_ontology:constraint_beneficiary(heteronomy_adoption_pathway, norm_enforcers).
narrative_ontology:constraint_beneficiary(heteronomy_adoption_pathway, institutional_gatekeepers).
narrative_ontology:constraint_victim(heteronomy_adoption_pathway, genuine_self_authorship).
narrative_ontology:constraint_victim(heteronomy_adoption_pathway, epistemic_autonomy).
narrative_ontology:constraint_victim(heteronomy_adoption_pathway, individual_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IDENTITY-FUSED INDIVIDUAL (SNARE) — The agent whose identity is constituted through the internalized standards. Cannot recognize the standards as externally given because doing so would require abandoning the self-concept built around them. Structurally mobile (could adopt different values) but cognitively trapped by identity fusion. Experiences the constraint as natural and self-authored despite genealogical evidence of peer-group uniformity. Maximum extraction: bears full cost of misrecognition while providing legitimacy to the hierarchy.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 2: REFLECTIVE AGENT (TANGLED ROPE) — Agent with partial meta-cognitive awareness. Recognizes some tension between stated autonomy and actual conformity but faces high costs to exit: social penalty, identity disruption, loss of status markers. Benefits from the coordination function (shared standards enable social legibility and cooperation) while bearing extraction costs (suppression of genuine preference formation). Mixed experience: the constraint both enables and constrains.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATUS HIERARCHY (ROPE) — Institutional beneficiary. The mechanism by which externally-given standards are internalized and misrecognized as self-authored is precisely what maintains hierarchical stability. Agents who believe they autonomously chose the values that justify the hierarchy are more reliable enforcers than agents who recognize the values as imposed. Experiences the constraint as pure coordination: a solution to the problem of maintaining social order without visible coercion. Net beneficiary with arbitrage exit options.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: COUNTER-CULTURAL COALITION (TANGLED ROPE) — Organized agents attempting to build alternative value formation pathways. See the heteronomy mechanism clearly and work to expose it, but also depend on shared counter-norms that may themselves be heteronomous (the rebel uniform problem). Mobile exit options (can leave mainstream institutions) but constrained by the need for coalition coherence. Mixed extraction: benefits from collective identity while risking reproduction of the same internalization mechanism with different content.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: DEVELOPMENTAL PSYCHOLOGY FRAMEWORK (SCAFFOLD) — Organized epistemic community (developmental psychologists, moral development researchers, critical pedagogues) that sees heteronomy as a necessary developmental stage with a sunset clause. Kohlberg's stages, Kegan's orders of consciousness, and critical pedagogy all model the transition from heteronomous to autonomous moral reasoning as a maturational process. The constraint is temporary support: externally-given standards are scaffolding for agents who have not yet developed capacity for genuine self-authorship. Suppression is tolerated because it declines over the developmental horizon. Low effective extraction because the organized community has exit options and sees a resolution path.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ENLIGHTENMENT AUTONOMY IDEAL (PITON) — The institutional discourse of rational autonomy and self-legislation (Kant's categorical imperative, liberal individualism, rational choice theory) persists as a legitimating narrative despite empirical evidence that most moral standards are socially transmitted and internalized rather than rationally derived. The ideal functions theatrically: agents perform autonomy (justify their values with reasons) while the genealogy reveals peer-group conformity. Theater ratio is high because the autonomy discourse is maintained through institutional inertia (philosophy curricula, political theory, legal doctrine) rather than because it accurately describes value formation. The institution sees its own ideal as degraded but cannot abandon it without losing legitimacy.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational/universal perspective, some degree of heteronomy may appear inevitable: humans are social animals, value formation occurs in social contexts, and complete independence from external influence is incoherent. This perspective risks naturalizing what is actually a contingent mechanism with variable intensity. The structural data contradicts the mountain classification — the engine's false summit detector will identify this as naturalization of a contingent social-psychological process. The gap between stated autonomy and actual conformity is not a law of nature but a product of specific socialization practices, status incentives, and epistemic closure mechanisms that vary across cultures and institutions.
constraint_indexing:constraint_classification(heteronomy_adoption_pathway, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heteronomy_adoption_pathway_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(heteronomy_adoption_pathway, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heteronomy_adoption_pathway, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(heteronomy_adoption_pathway, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(heteronomy_adoption_pathway, TR),
    TR >= 0.70.

:- end_tests(heteronomy_adoption_pathway_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from individual epistemic autonomy and genuine self-authorship while benefiting status hierarchies and norm enforcers. The extraction is substantial but not maximal because the constraint also provides genuine coordination benefits: shared standards enable cooperation, and some degree of social learning is necessary for functioning agents. The value reflects that the heteronomy mechanism is a mixed phenomenon — partly legitimate socialization, partly extractive suppression of autonomy. Suppression (0.62): High. Significant barriers to recognizing and exiting the heteronomy mechanism include identity fusion (the self-concept is built around the internalized standards), social penalty for deviation (status loss, exclusion), epistemic closure (the standards themselves define what counts as good reasoning), and the performative autonomy discourse (agents are taught to justify their values as self-authored, which obscures the genealogy). The suppression is not total — some agents achieve meta-cognitive awareness and exit — but it is substantial and operates partly through internalized mechanisms that persist after removal of external penalties. Theater ratio (0.68): High. Much of the discourse around autonomy, rational deliberation, and self-legislation is performative. Agents justify their values with reasons and perform the ritual of deliberation, but the genealogy reveals that the values were adopted through social transmission (peer effects, authority figures, institutional socialization) rather than rational derivation. The Enlightenment ideal of autonomous self-legislation persists as a legitimating narrative despite empirical evidence that most moral reasoning is post-hoc rationalization of socially transmitted intuitions. The theater has increased over the interval as the gap between the autonomy ideal and the conformity reality has become more visible through social psychology research, yet the ideal persists through institutional inertia.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of DR classification from a single structural phenomenon. The identity-fused individual sees nothing (the constraint is invisible because it constitutes their identity) or, if they see it, experiences it as a snare (cannot exit without self-dissolution). The reflective agent sees tangled rope (mixed coordination and extraction, high exit costs but some agency). Status hierarchies see rope (pure coordination — the mechanism solves the problem of maintaining order without visible coercion). The counter-cultural coalition sees tangled rope (exposes the mechanism but risks reproducing it). The developmental psychology framework sees scaffold (temporary stage with a maturational sunset). The Enlightenment autonomy ideal sees piton (its own degraded discourse, maintained through inertia). The analytical observer risks seeing mountain (heteronomy is inevitable given human social nature) but the structural data reveals this as a false summit: the intensity and mechanisms of heteronomy vary across cultures and institutions, indicating contingency rather than natural law. The perspectival gap is diagnostic: agents at different structural positions experience the same mechanism as invisible, inescapable, mixed, beneficial, temporary, degraded, or natural.
 *
 * DIRECTIONALITY LOGIC:
 *   The identity-fused individual is a victim with identity_locked exit options, yielding high directionality (d ≈ 0.89) and high experienced extraction. The agent is structurally mobile (could adopt different values) but cognitively trapped by identity fusion — exit would require abandoning the self-concept built around the internalized standards. The reflective agent is a victim with constrained exit options, yielding moderate-high directionality (d ≈ 0.75). This agent has partial meta-cognitive awareness and could exit at significant cost (social penalty, identity disruption), producing a tangled rope classification. Status hierarchies are beneficiaries with arbitrage exit options, yielding low directionality (d ≈ 0.05) and negative experienced extraction — they benefit from the mechanism and can exit costlessly if it stops serving their interests. The counter-cultural coalition is organized with mobile exit options but mixed beneficiary/victim status (benefits from collective identity, bears cost of potential reproduction of heteronomy with different content), yielding moderate directionality. The developmental psychology framework is organized with mobile exit options and primarily beneficiary status (the framework itself is not subject to the constraint it studies), yielding low directionality and scaffold classification. The Enlightenment autonomy ideal is institutional with constrained exit options (cannot abandon the autonomy narrative without losing legitimacy) and mixed status (benefits from the legitimating function, bears cost of the gap between ideal and reality), yielding moderate directionality and piton classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that heteronomy is neither pure coordination (rope) nor pure extraction (snare) but a structurally mixed phenomenon whose classification depends on the observer's position. From the status hierarchy's perspective, the mechanism is genuine coordination: it solves the problem of maintaining social order by producing agents who authentically endorse the hierarchy's values. From the identity-fused individual's perspective, the mechanism is extraction: it suppresses genuine self-authorship and produces epistemic closure. Both perspectives are structurally correct. The tangled rope classification at the analytical level captures this duality: the constraint has both a coordination function (enabling social cooperation through shared standards) and an extraction function (suppressing autonomy and maintaining hierarchy). The mandatrophy is resolved by recognizing that the question 'is heteronomy good or bad?' is indexical — it depends on which agent you are measuring from and what values you use to evaluate. The framework does not adjudicate this normative question; it maps the structural positions from which different answers emerge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_heteronomy_boundary,
    'Where is the boundary between legitimate social learning (acquiring language, norms, skills from others) and heteronomous internalization (misrecognizing external standards as self-authored)?',
    'Longitudinal studies tracking value genealogy; cross-cultural comparison of value formation processes; experimental manipulation of norm exposure and subsequent endorsement patterns',
    'If boundary is narrow: most socialization is heteronomous, and genuine autonomy is rare or impossible. If boundary is wide: heteronomy is a specific pathology rather than a general feature of social learning.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(autonomy_heteronomy_boundary, conceptual, 'Boundary between social learning and heteronomous internalization').

omega_variable(
    recognition_vs_constitution,
    'Does recognizing the external origin of one''s values dissolve their normative force, or can values remain binding after genealogical awareness?',
    'Philosophical analysis of the relationship between genealogy and normativity; empirical studies of value stability after consciousness-raising interventions; phenomenological accounts from agents who have undergone deconversion or deprogramming',
    'If recognition dissolves force: genealogical awareness is inherently liberating, and the constraint has a natural sunset. If values persist after recognition: the constraint is more durable, and meta-cognitive awareness is insufficient for exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(recognition_vs_constitution, conceptual, 'Whether genealogical awareness dissolves normative force').

omega_variable(
    peer_uniformity_threshold,
    'What degree of peer-group uniformity in stated values constitutes evidence of heteronomy rather than convergence on objective moral truths?',
    'Statistical analysis of within-group vs between-group variance in moral judgments; comparison of value distributions in isolated vs connected populations; tracking value change in response to peer-group transitions',
    'If threshold is low: even modest conformity indicates heteronomy, and most moral consensus is suspect. If threshold is high: only extreme uniformity indicates heteronomy, and most consensus reflects genuine agreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_uniformity_threshold, empirical, 'Peer uniformity threshold for diagnosing heteronomy').

omega_variable(
    developmental_stage_universality,
    'Are the developmental stages from heteronomy to autonomy (Kohlberg, Kegan) universal and inevitable, or are they culturally specific and contingent?',
    'Cross-cultural developmental psychology; historical analysis of moral reasoning patterns in different civilizations; longitudinal studies in non-WEIRD populations',
    'If universal: the scaffold perspective is structurally correct, and heteronomy has a natural sunset. If culturally specific: the developmental framework itself may be a Western heteronomous standard, and the scaffold perspective is aspirational rather than descriptive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_stage_universality, empirical, 'Universality of developmental stages toward autonomy').

omega_variable(
    suppression_mechanism_ratio,
    'What proportion of the measured suppression is structural (social penalty, status loss, institutional barriers) vs internalized (identity fusion, cognitive closure, fear of self-examination)?',
    'Post-exit suppression trajectory: if suppression persists after removal of external penalties, reclassify as partially internalized. Comparison of exit difficulty in high-penalty vs low-penalty environments with similar identity fusion levels.',
    'If mostly structural: reducing external penalties enables exit. If mostly internalized: the constraint''s effective suppression is higher than structural measures suggest, and the target carries the suppression with them after exit from the immediate social context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ratio, empirical, 'Structural vs internalized suppression mechanism ratio').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(heteronomy_adoption_pathway, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hetero_theater_initial, heteronomy_adoption_pathway, theater_ratio, 0, 0.52).
narrative_ontology:measurement(hetero_theater_early, heteronomy_adoption_pathway, theater_ratio, 3, 0.58).
narrative_ontology:measurement(hetero_theater_mid, heteronomy_adoption_pathway, theater_ratio, 6, 0.64).
narrative_ontology:measurement(hetero_theater_final, heteronomy_adoption_pathway, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(hetero_extract_initial, heteronomy_adoption_pathway, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hetero_extract_early, heteronomy_adoption_pathway, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(hetero_extract_mid, heteronomy_adoption_pathway, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(hetero_extract_final, heteronomy_adoption_pathway, base_extractiveness, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(heteronomy_adoption_pathway, identity_coordination).
narrative_ontology:boltzmann_floor_override(heteronomy_adoption_pathway, 0.08).

% DUAL FORMULATION NOTE:
% The heteronomy adoption pathway is downstream of the recognition dependency mechanism (mountain-classified need for social recognition). The recognition dependency is a structural feature of human social cognition; the heteronomy pathway is a specific mechanism by which that dependency produces internalization and misrecognition of externally-given standards. The upstream constraint (recognition dependency) has low extractiveness (ε ≈ 0.08, mountain) because the need for recognition is treated as a natural law. The downstream constraint (heteronomy pathway) has moderate extractiveness (ε = 0.48, tangled rope) because the specific mechanism by which recognition needs produce internalization is contingent on socialization practices, status incentives, and epistemic closure mechanisms that vary across contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
