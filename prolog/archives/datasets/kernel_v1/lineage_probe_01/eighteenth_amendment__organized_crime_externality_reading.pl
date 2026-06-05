% ============================================================================
% CONSTRAINT STORY: eighteenth_amendment__organized_crime_externality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eighteenth_amendment__organized_crime_externality_reading, []).

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
 *   constraint_id: eighteenth_amendment__organized_crime_externality_reading
 *   human_readable: Eighteenth Amendment: Organized Crime Externality Reading
 *   domain: legal/constitutional/criminal_organization
 *
 * SUMMARY:
 *   This constraint instantiates the organized-crime-externality reading of
 *   the Eighteenth Amendment: Prohibition created a constitutional monopoly
 *   rent that criminal syndicates captured, consolidated, and defended
 *   through violence and political infiltration. The reading distinguishes
 *   itself from sibling readings (constitutional_overreach_lesson and
 *   enforcement_collapse) by focusing on who benefited and what structural
 *   mechanisms enabled their consolidation. The key agents are: (1) organized
 *   crime syndicates as primary beneficiaries, capturing the illegal alcohol
 *   market monopoly that law enforcement could not suppress; (2) urban
 *   populations as primary victims, bearing extraction through violence,
 *   graft, and territory control; (3) law enforcement as a secondary victim,
 *   tasked with enforcing an unenforceable law against a better-funded
 *   adversary; (4) the temperance movement as maintaining a performative
 *   enforcement regime (piton) despite mounting evidence of failure. The
 *   constraint exhibits a false summit at the civilizational analytical
 *   level—the claim that prohibition inevitably creates organized crime
 *   operates as a natural law of incentive structures, but the beneficiary
 *   structure (identifiable syndicates, not market forces) reveals this as a
 *   constructed rather than natural constraint. The measurement arc shows
 *   extractiveness rising as syndicate consolidation deepens (1920–1927, the
 *   violent consolidation period) and then plateauing as the regime becomes
 *   inert (1927–1933, enforcement exhaustion). Suppression rises
 *   monotonically—the enforcement machinery must intensify as syndicates
 *   become more sophisticated—until by 1933 the regime is unsustainable.
 *
 * KEY AGENTS:
 *   - Organized Crime Syndicates (Capone, Lansky networks, etc.): Primary beneficiary (institutional/arbitrage) — capture the monopoly rent created by Prohibition; experience the constraint as coordination mechanism enabling their market consolidation
 *   - Urban populations (Chicago, New York, Detroit, etc.): Primary victim (powerless/trapped) — subject to syndicate violence, extortion, graft; no exit option; maximum extraction experience
 *   - Federal Enforcement Apparatus (Volstead Administration): Secondary beneficiary and victim (institutional/constrained) — benefits from budgetary authority and jurisdictional expansion; victimized by incompatibility between mandate (enforce nationwide) and resources (chronically insufficient). Transitional from beneficiary (early 1920s, new agency authority) to victim (mid-1920s onward, captured by syndicates).
 *   - Local Law Enforcement: Victim (moderate/constrained) — caught between federal mandate (enforce) and local reality (syndicates have superior firepower and organization). High suppression (arrest risk, assassination risk) with zero benefit. Partially captured by syndicate graft.
 *   - Temperance Movement and Moral Reform Coalition: Institutional beneficiary with declining returns (institutional/constrained) — the Amendment achieved their primary goal, but the moral outcome (reformed society) failed to materialize. Theater increases as functional success decreases.
 *   - Political Repeal Coalition (Wets): Organized victim (organized/constrained) — experienced suppression through political opposition and enforcement of the regime they oppose. Transitioned to a scaffold perspective by early 1930s as repeal momentum built.
 *   - Analytical Observer: Sees false summit (analytical/analytical) — risks naturalizing a contingent institutional arrangement as an immutable law of political economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eighteenth_amendment__organized_crime_externality_reading, 0.68).
domain_priors:suppression_score(eighteenth_amendment__organized_crime_externality_reading, 0.78).
domain_priors:theater_ratio(eighteenth_amendment__organized_crime_externality_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eighteenth_amendment__organized_crime_externality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eighteenth_amendment__organized_crime_externality_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eighteenth_amendment__organized_crime_externality_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eighteenth_amendment__organized_crime_externality_reading, snare).
narrative_ontology:human_readable(eighteenth_amendment__organized_crime_externality_reading, "Eighteenth Amendment: Organized Crime Externality Reading").
narrative_ontology:topic_domain(eighteenth_amendment__organized_crime_externality_reading, "legal/constitutional/criminal_organization").

domain_priors:requires_active_enforcement(eighteenth_amendment__organized_crime_externality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eighteenth_amendment__organized_crime_externality_reading, 'd0b367ee-989d-4866-a7dd-aaf53031213c').
narrative_ontology:cs_kernel_codification('d0b367ee-989d-4866-a7dd-aaf53031213c', formalized).
narrative_ontology:cs_authority_grounding('d0b367ee-989d-4866-a7dd-aaf53031213c', extraction).
narrative_ontology:cs_interpretation_layer_present('d0b367ee-989d-4866-a7dd-aaf53031213c').
narrative_ontology:cs_reading_relation('d0b367ee-989d-4866-a7dd-aaf53031213c', eighteenth_amendment__constitutional_overreach_lesson_reading, influences).
narrative_ontology:cs_reading_relation('d0b367ee-989d-4866-a7dd-aaf53031213c', eighteenth_amendment__enforcement_collapse_reading, influences).
narrative_ontology:cs_axiom('d0b367ee-989d-4866-a7dd-aaf53031213c', foundational, monopoly_rent_capture_by_organized_crime).
narrative_ontology:cs_axiom_status(monopoly_rent_capture_by_organized_crime, holdable).
narrative_ontology:cs_axiom_grounding('d0b367ee-989d-4866-a7dd-aaf53031213c', monopoly_rent_capture_by_organized_crime, empirically_contingent).
narrative_ontology:cs_axiom('d0b367ee-989d-4866-a7dd-aaf53031213c', foundational, suppression_transfer_not_elimination).
narrative_ontology:cs_axiom_status(suppression_transfer_not_elimination, holdable).
narrative_ontology:cs_axiom_grounding('d0b367ee-989d-4866-a7dd-aaf53031213c', suppression_transfer_not_elimination, empirically_contingent).
narrative_ontology:cs_reference_frame('d0b367ee-989d-4866-a7dd-aaf53031213c', legal_monopoly_rent_extraction_via_prohibition).
narrative_ontology:cs_drift_state('d0b367ee-989d-4866-a7dd-aaf53031213c', id_1933_repeal_and_aftermath, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('d0b367ee-989d-4866-a7dd-aaf53031213c', '').
narrative_ontology:cs_kernel_id(eighteenth_amendment__organized_crime_externality_reading, eighteenth_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eighteenth_amendment__organized_crime_externality_reading, organized_crime_syndicates).
narrative_ontology:constraint_victim(eighteenth_amendment__organized_crime_externality_reading, urban_populations).
narrative_ontology:constraint_victim(eighteenth_amendment__organized_crime_externality_reading, law_enforcement_capacity).
narrative_ontology:constraint_victim(eighteenth_amendment__organized_crime_externality_reading, legitimate_alcohol_commerce).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: URBAN RESIDENTS UNDER SYNDICATE CONTROL (SNARE) — Trapped in territories where organized crime extracts through violence, graft, and protection rackets. The Amendment created a monopoly rent; syndicates captured it. Residents experience maximum suppression (violence, arrest risk, extortion) with zero benefit. No exit option — geographic, legal, or economic. The constraint is unambiguously extractive and coercive.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL LAW ENFORCEMENT (SNARE) — Tasked with enforcing an unenforceable law against a better-funded and better-organized adversary. Constrained by inadequate budget, jurisdictional limits, and syndicate infiltration of the enforcement apparatus itself. The constraint extracts enforcement effort (budgetary and human cost) while the syndicate captures the rent. Law enforcement is a victim, not a beneficiary — they bear the cost of an enforcement regime that benefits syndicates.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL ENFORCEMENT APPARATUS / VOLSTEAD ADMINISTRATION (TANGLED ROPE) — The Volstead Act created a new enforcement bureaucracy with real coordination function (attempting to suppress the alcohol trade nationwide) AND asymmetric extraction. The federal apparatus is constrained by limited reach and resources; syndicates outcompete them. Yet the apparatus also benefits from budgetary allocation and jurisdictional authority. This is coordination (building a national enforcement system) layered atop extraction (from users, distributors, and local authorities). Suppression is high (enforcement authority) but the apparatus itself fails to achieve its suppressive goal and becomes partially captured.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORGANIZED CRIME SYNDICATES (ROPE) — The Amendment created a legal monopoly rent: alcohol prohibition outlawed the legitimate supply chain and handed the market to whoever could enforce supply outside the law. Syndicates experience the constraint as pure coordination: they coordinate production, distribution, and protection of their market share. From their perspective, the Amendment is a coordination mechanism that transfers suppression of competitors to a third party (law enforcement) while they capture the rent. Zero extraction from their position — maximum benefit. The constraint is a rope because it solves their collective action problem (monopoly maintenance without legal standing).
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TEMPERANCE MOVEMENT / MORAL ENFORCEMENT REGIME (PITON) — The Amendment promised moral transformation of the nation through constitutional law. By the 1920s midway through Prohibition, the theater persisted (continued raids, arrests, moral rhetoric) but the functional outcome (reduced alcohol consumption, moral reform) had largely failed. The constraint persisted through institutional inertia — the temperance movement could not admit failure, and the enforcement apparatus could not be dismantled without admitting that the constitutional text had been captured by its opponents. Theater ratio is moderate-to-high: the performative enforcement operations (raids, arrests) continued at high cost with minimal effect. By the 1933 repeal, the regime was mostly theater—institutional commitment to an unachievable goal.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational perspective, one might naturalize Prohibition as a law of political thermodynamics: any attempt to suppress a market demand that large will automatically create a higher-profit black market and drive sophistication of organized crime. The constraint appears immutable—a consequence of incentive structures, not policy design. However, this reading naturalizes what is actually a contingent institutional choice (the decision to prohibit via constitutional Amendment rather than tax and regulate). The beneficiary structure (organized syndicates) reveals this as a false summit: the 'law' is not natural, it is constructed in a way that benefits identifiable agents.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: POLITICAL REFORM COALITION / WET FORCES (TANGLED ROPE) — Organized agents (repeal advocates, civil libertarians, urban politicians) experienced the constraint as a hybrid: they coordinated to repeal it (genuine coordination function) while also suffering extraction through the constraint's enforcement regime. They faced suppression (political opposition, institutional resistance to repeal) but also maintained agency and exit paths (electoral pressure, legislative strategy). By 1933, their perspective on the constraint shifted from snare (Prohibition as an imposed evil) to scaffold (a temporary regime with a visible sunset via repeal). The constraint's classification from this perspective depends on when in the generational timeline the observation occurs—earlier, snare; later, tangled rope transitioning to scaffold.
constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eighteenth_amendment__organized_crime_externality_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eighteenth_amendment__organized_crime_externality_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eighteenth_amendment__organized_crime_externality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eighteenth_amendment__organized_crime_externality_reading, TR),
    TR >= 0.70.

:- end_tests(eighteenth_amendment__organized_crime_externality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, reflecting the rent capture by syndicates and the multi-layered extraction from urban populations, law enforcement, and the federal budget. The value is not at the maximum (0.75+) because Prohibition did reduce overall alcohol consumption (not zero extraction effectiveness), and the regime retained some functional enforcement capacity. The upward trajectory from 0.35 (1920, initial Volstead implementation) to 0.72 (1931, peak syndicate control) reflects the acceleration of rent consolidation as syndicates organized. Suppression (0.78): Very high. The constraint operates through violence, legal prohibition, and infiltration of enforcement apparatus. The suppression floor is set by the constitutional prohibition itself (legal impossibility of legitimate supply) and the institutional violence required to maintain syndicate monopoly. This is not maximal suppression because some agents (wealthy, connected) could obtain alcohol; suppression was stratified by geography and class. Theater ratio (0.55): Moderate-to-moderate-high. The Volstead enforcement operations had genuine functional content (they did seize some alcohol, arrest some distributors, disrupt some supply chains) but increasingly operated as spectacle—high-profile raids that rarely prevented the trade from functioning. By 1932, the theater ratio rises to 0.68 as the regime becomes exhausted but persists through institutional inertia. The moderate starting value (0.40 in 1920) reflects that early enforcement was more functional than theatrical; it evolved into theater as syndicates adapted.
 *
 * PERSPECTIVAL GAP:
 *   The original research group (syndicates) sees Rope—the constraint coordinates their market and suppresses competitors. Urban residents see Snare—pure extraction, no exit. Law enforcement sees an impossible mandate (tangled_rope transitioning to snare). The temperance movement sees successful policy (piton, not yet visible as theater). The political repeal coalition sees a temporary injustice (tangled_rope with sunset clause). The civilizational observer risks seeing a natural law (false summit). The perspectival gaps are deep: syndicates experience benefit where residents experience victimization; federal apparatus experiences mandate expansion where locals experience impossible pressure; the regime persists as theater long after its functional death. This is not perspectival disagreement about facts but structurally rooted differences in position.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is derived from base extractiveness (0.68), the sigmoid function f(d) of directionality, and scope modifier sigma(national=1.0). The syndicates get d ≈ 0.10 (beneficiaries with arbitrage options) → f(d) ≈ -0.01 → chi ≈ -0.01 (negative, they benefit). Urban residents get d ≈ 0.95 (victims with trapped exit) → f(d) ≈ 1.42 → chi ≈ 0.97 (maximum extraction). Law enforcement gets d ≈ 0.65 (mixed victim/beneficiary with constrained exit) → f(d) ≈ 1.00 → chi ≈ 0.68 (high extraction). The federal apparatus gets d ≈ 0.40 (weak beneficiary with constrained options) → f(d) ≈ 0.40 → chi ≈ 0.27 (moderate). Temperance movement gets d ≈ 0.15 (institutional beneficiary with constrained options given political opposition) → f(d) ≈ -0.01 → chi ≈ -0.01 (they see the constraint as serving their values, not extracting from them). The political repeal coalition gets d ≈ 0.70 (victims with organized agency and exit paths) → f(d) ≈ 1.15 → chi ≈ 0.78 (high experienced extraction). The directionality logic is a pure function of structural position, not of opinion about whether Prohibition is good policy.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy between snare, tangled_rope, and rope classifications by showing they are all structurally correct at different positions. The syndicates genuinely experience rope (coordination without extraction for them). The residents genuinely experience snare (extraction without coordination). The federal apparatus genuinely experiences tangled_rope (both coordination—attempting to enforce—and extraction—the cost and partial capture of the effort). No single classification is 'the truth'—the presheaf over the observation map is the truth. The false summit at the analytical level reveals that naturalizing Prohibition as an immutable law prevents seeing the contingent institutional arrangements (beneficiary structure) that make it what it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    supply_elasticity_counterfactual,
    'Would a heavily taxed but legal alcohol market have generated comparable syndicate profits, or is the black-market monopoly rent uniquely extractive?',
    'Comparative historical analysis: alcohol tax regimes in countries that never prohibited (Scandinavia, France) vs Prohibition-era U.S. black market profits. Model elasticity of supply under legal/taxed regime.',
    'If legal market would have captured most of the rent: Prohibition is a policy lever, not a natural law (supports false summit reclassification). If black market genuinely generates higher profit through monopoly scarcity: the natural law perspective has structural support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_elasticity_counterfactual, empirical, 'Whether legal regulation would have reduced syndicate profitability').

omega_variable(
    syndicate_consolidation_mechanism,
    'Did Prohibition uniquely enable rapid syndicate consolidation and centralization, or would organized crime have consolidated anyway under illegal supply?',
    'Historical analysis of organized crime structure pre-Prohibition vs during Prohibition. Examine whether pre-Prohibition criminal organizations were comparably sophisticated, violence-intensive, and geographically centralized. Compare with other national prohibition regimes.',
    'If Prohibition uniquely enabled consolidation: the constitutional text is the structural lever (this reading is strong). If syndicates would have consolidated regardless: Prohibition accelerated pre-existing trends but was not the primary cause (weaken this reading).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syndicate_consolidation_mechanism, empirical, 'Whether Prohibition uniquely enabled syndicate consolidation').

omega_variable(
    kernel_reading_ambiguity,
    'Is Prohibition an exemplar of how prohibition-as-policy creates the conditions for organized crime, or an exemplar of how constitutional overreach creates legal vacuums?',
    'Jurisdictional analysis: was the error in choosing to prohibit (policy), or in constitutionalizing the prohibition (legal form)? Compare with other constitutional amendments (13th, 14th, 15th) and their implementation. Compare with non-constitutional prohibitions in other countries.',
    'If policy error (could have used taxation/regulation instead): this reading (organized_crime_externality_reading) is the primary driver. If legal form error (constitutionalizing is the mistake): the constitutional_overreach_lesson_reading becomes primary. Both readings coexist; the ambiguity is in causal attribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the error is policy (prohibition) or legal form (constitutionalization)').

omega_variable(
    reading_kernel_distinctness,
    'This reading focuses on the organized crime externality—syndicates as beneficiaries, urban violence as the cost. Is this a distinct reading of the Eighteenth Amendment kernel, or is it subsumed by the enforcement_collapse_reading?',
    'Textual and structural differentiation: the enforcement_collapse_reading focuses on law-enforcement institutional failure and citizen contempt for law. This reading focuses on rent capture and criminal organization consolidation. These are distinct causal mechanisms and distinct victim sets. Confirm that the syndicates-as-beneficiaries frame is genuinely novel relative to the enforcement-collapse frame.',
    'If distinct: three separable readings stand (constitutional_overreach, enforcement_collapse, organized_crime_externality). If subsumed: this reading is an artifact of the enforcement_collapse frame and should be consolidated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_distinctness, conceptual, 'Whether organized_crime_externality_reading is distinct from enforcement_collapse_reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eighteenth_amendment__organized_crime_externality_reading, 1920, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1920_genuine_enforcement, eighteenth_amendment__organized_crime_externality_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement(theater_1926_mixed_enforcement_spectacle, eighteenth_amendment__organized_crime_externality_reading, theater_ratio, 1926, 0.55).
narrative_ontology:measurement(theater_1932_enforcement_exhaustion, eighteenth_amendment__organized_crime_externality_reading, theater_ratio, 1932, 0.68).

% Extraction over time
narrative_ontology:measurement(extract_1920_volstead_ratified, eighteenth_amendment__organized_crime_externality_reading, base_extractiveness, 1920, 0.35).
narrative_ontology:measurement(extract_1923_syndicate_consolidation, eighteenth_amendment__organized_crime_externality_reading, base_extractiveness, 1923, 0.52).
narrative_ontology:measurement(extract_1927_peak_violence, eighteenth_amendment__organized_crime_externality_reading, base_extractiveness, 1927, 0.68).
narrative_ontology:measurement(extract_1931_repeal_imminent, eighteenth_amendment__organized_crime_externality_reading, base_extractiveness, 1931, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(suppress_1920_initial_enforcement, eighteenth_amendment__organized_crime_externality_reading, suppression_requirement, 1920, 0.45).
narrative_ontology:measurement(suppress_1926_violence_escalation, eighteenth_amendment__organized_crime_externality_reading, suppression_requirement, 1926, 0.78).
narrative_ontology:measurement(suppress_1932_peak_enforcement, eighteenth_amendment__organized_crime_externality_reading, suppression_requirement, 1932, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eighteenth_amendment__organized_crime_externality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eighteenth_amendment__organized_crime_externality_reading, eighteenth_amendment__constitutional_overreach_lesson_reading).
narrative_ontology:affects_constraint(eighteenth_amendment__organized_crime_externality_reading, eighteenth_amendment__enforcement_collapse_reading).
narrative_ontology:affects_constraint(eighteenth_amendment__organized_crime_externality_reading, organized_crime_consolidation_1920s).
narrative_ontology:affects_constraint(eighteenth_amendment__organized_crime_externality_reading, urban_violence_cartel_wars).

% DUAL FORMULATION NOTE:
% This reading is part of a three-way kernel family decomposed from the Eighteenth Amendment. Each reading instantiates a different causal story about Prohibition: constitutional form error (overreach), institutional failure (enforcement), and market structure (rent capture / crime). The three stories have different epsilon values and different beneficiary/victim structures. They are linked via network.affects_constraints to show causal dependency: constitutional overreach created conditions for enforcement failure; enforcement failure created conditions for syndicate monopoly. The three readings coexist as live interpretive positions in legal/policy discourse, not as temporally sequential phases.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eighteenth_amendment__organized_crime_externality_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
