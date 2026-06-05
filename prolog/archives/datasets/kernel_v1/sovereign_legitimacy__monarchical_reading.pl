% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Monarchical Bloodline Legitimacy (Sovereign Legitimacy Kernel)
 *   domain: political_philosophy/constitutional_theory
 *
 * SUMMARY:
 *   The monarchical reading of sovereign legitimacy grounds authority
 *   exclusively in inherited bloodline: the sovereign is sovereign because
 *   they are born into the correct lineage, and this legitimacy is
 *   inalienable, unrevisable, and independent of performance, consent, or
 *   periodic validation. This reading constitutes one point in a three-way
 *   contested kernel alongside the republican reading (legitimacy derives
 *   from popular will, expressed through elections) and the constitutional
 *   hybrid reading (legitimacy derives from a written constitution that may
 *   preserve monarchy while constraining it via law and representation). The
 *   monarchical reading exhibits Tangled Rope structure: it contains a
 *   genuine coordination function (solving the succession problem through
 *   fixed, predictable inheritance) layered with asymmetric extraction (the
 *   populace cannot revise or contest the authority, while the beneficiary
 *   lineage extracts continued power regardless of performance). The
 *   constraint's theater ratio has risen over the interval (0.55 → 0.68) as
 *   the underlying legitimacy mechanism has become increasingly performative
 *   and ceremonial, particularly in contexts where de facto power has shifted
 *   to representative institutions while hereditary authority persists as
 *   constitutional form. Suppression has remained high and stable (0.68 →
 *   0.72), indicating that the constraint's coercive apparatus (legal status
 *   hierarchies, exclusionary succession rules, suppression of challenge
 *   mechanisms) has not weakened despite modernization pressures.
 *
 * KEY AGENTS:
 *   - Dynastic Lineage: Primary beneficiary (institutional/arbitrage) — captures authority and succession privilege; experiences constraint as pure coordination
 *   - Hereditary Aristocracy: Secondary beneficiary (institutional/constrained) — protected by the same inheritance principle that secures the crown; mixed experience of benefit and stability advantage
 *   - Disenfranchised Populace: Primary victim (powerless/trapped) — excluded from legitimacy tests, succession participation, or authority challenge; no exit options; bears full suppressive burden
 *   - Excluded Constituencies: Secondary victim (powerless/trapped) — subordinated by fixed birth-based hierarchy; cannot rise through merit or mobilization; trapped in assigned status
 *   - Constitutional Reform Movement: Organized challenger (organized/constrained) — sees hereditary legitimacy as replaceable through amendment or revolution; building alternative pathways
 *   - Historian/Analyst: Civilizational observer (analytical/analytical) — risks naturalizing contingent institutional arrangement as inevitable law; detects false summit signature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.58).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.72).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Monarchical Bloodline Legitimacy (Sovereign Legitimacy Kernel)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political_philosophy/constitutional_theory").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '52bf2385-d318-4c3e-8eb7-573d7178a61b').
narrative_ontology:cs_kernel_codification('52bf2385-d318-4c3e-8eb7-573d7178a61b', fixed_text).
narrative_ontology:cs_authority_grounding('52bf2385-d318-4c3e-8eb7-573d7178a61b', lineage).
narrative_ontology:cs_interpretation_layer_present('52bf2385-d318-4c3e-8eb7-573d7178a61b').
narrative_ontology:cs_reading_relation('52bf2385-d318-4c3e-8eb7-573d7178a61b', sovereign_legitimacy__republican_reading, coexists_with).
narrative_ontology:cs_reading_relation('52bf2385-d318-4c3e-8eb7-573d7178a61b', sovereign_legitimacy__constitutional_hybrid_reading, influences).
narrative_ontology:cs_axiom('52bf2385-d318-4c3e-8eb7-573d7178a61b', foundational, legitimacy_via_birth_immutable).
narrative_ontology:cs_axiom_status(legitimacy_via_birth_immutable, holdable).
narrative_ontology:cs_axiom_grounding('52bf2385-d318-4c3e-8eb7-573d7178a61b', legitimacy_via_birth_immutable, conventional).
narrative_ontology:cs_axiom('52bf2385-d318-4c3e-8eb7-573d7178a61b', secondary, dynastic_succession_natural_order).
narrative_ontology:cs_axiom_status(dynastic_succession_natural_order, overridden).
narrative_ontology:cs_axiom_grounding('52bf2385-d318-4c3e-8eb7-573d7178a61b', dynastic_succession_natural_order, empirically_contingent).
narrative_ontology:cs_reference_frame('52bf2385-d318-4c3e-8eb7-573d7178a61b', hereditary_divine_right).
narrative_ontology:cs_drift_state('52bf2385-d318-4c3e-8eb7-573d7178a61b', constitutional_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52bf2385-d318-4c3e-8eb7-573d7178a61b', '2026-02-26T14:33:22Z').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, dynastic_line).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_aristocracy).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, popular_will).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, excluded_constituencies).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISENFRANCHISED POPULACE (SNARE) — Trapped by birth into subject status with no exit mechanism. Legitimate authority is declared unrevisable regardless of consent or performance. Cannot exit, cannot challenge through periodic legitimacy tests, cannot appeal to alternative authority source. Maximum extraction with minimal coordination benefit — the constraint serves only to stabilize the beneficiary's position against challenge.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LESSER NOBILITY AND GENTRY (TANGLED ROPE) — Constrained by career dependence on the crown and legal status contingent on the hierarchy. Also benefits from the stable succession principle — their own hereditary claims and estates are protected by the same legitimacy rule. Mixed experience: significant extraction via hierarchy but genuine coordination benefit from succession stability and clear authority lines.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REIGNING MONARCH (ROPE) — Pure beneficiary with exit options (abdication, migration, institutional capture). Experiences the constraint as pure coordination: the inherited bloodline rule solves the succession problem, provides legitimacy narrative, stabilizes expectations about authority. Zero extraction cost; full benefit. The constraint coordinates around the sovereign's position without requiring coercion from the sovereign's perspective.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSTITUTIONAL REFORM MOVEMENT (SCAFFOLD) — Organized actors (reform legislatures, liberal constitutionalists, nationalist movements) see hereditary legitimacy as a temporary coordination mechanism being actively replaced by electoral and representative systems. The movement has agency and sees an exit path via constitutional amendment or revolution. The constraint appears as a sunset mechanism that is being deliberately dismantled. Theater is moderate (the legitimacy ritual persists during transition) but the reform coalition has clear agency.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORIAN'S VIEW (PITON) — From a long-term institutional perspective, hereditary legitimacy functions primarily through performative ceremony (coronation rituals, succession pageantry, dynastic symbolism) rather than genuine coordination. The mechanism persists due to institutional inertia and cultural habit despite contradicting modern legitimacy principles (consent, representation, accountability). Theater ratio is high (0.68) — the ritual is maintained by the institution for its own perpetuation, not for the legitimacy problem it ostensibly solves.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some argue that hereditary succession reflects natural law: the family is the fundamental political unit, inheritance is natural to human society, and the chain of legitimate descent through bloodline mirrors the natural order. However, this classification is a FALSE SUMMIT: the constraint's ε value, suppression profile, and beneficiary/victim structure contradict the mountain gates. The natural law framing naturalizes what is actually a constructed institutional arrangement.
constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sovereign_legitimacy__monarchical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, TR),
    TR >= 0.70.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The monarchical reading benefits the dynastic line with unrevisable authority — they extract continued power regardless of performance. However, this is not maximum extraction because the constraint does provide genuine succession coordination: it solves the problem of 'who rules next' through fixed, predictable rules. This coordination benefit is asymmetric (concentrated on the beneficiary, not the populace), but real. The constraint prevents succession wars and provides legitimacy narrative that reduces the enforcement cost of authority. If the constraint were pure extraction (Snare), this coordination function would be absent. The ε=0.58 reflects the hybrid: significant extraction (the power asymmetry is extreme and irrevisable) mixed with genuine coordination (the succession problem is solved). Suppression (0.72): High. The constraint requires substantial coercive apparatus: legal status hierarchies that lock individuals into subject status by birth, exclusion of non-lineage members from succession regardless of merit, suppression of challenge mechanisms (denial of voting, assembly, petition rights during pre-constitutional periods, or severe constraints in constitutional monarchies). This suppression is both structural (laws that enforce hierarchy) and cultural (norms that treat heredity as natural). Theater ratio (0.68): Moderately high. The constraint's legitimacy mechanism is increasingly performative in modern contexts: coronation rituals, succession pageantry, dynastic symbolism, and appeals to tradition carry the legitimacy narrative even where de facto power has shifted to representative or administrative institutions. The theater has risen over the interval as the underlying functional necessity of hereditary authority has diminished in industrial/post-industrial societies, yet the institution persists.
 *
 * PERSPECTIVAL GAP:
 *   The monarchical reading generates a striking perspectival gap between the beneficiary and victim positions. The reigning monarch (institutional, arbitrage) sees the constraint as pure Rope — it solves the succession problem, provides legitimacy narrative, stabilizes expectations. Zero experienced extraction because they are the beneficiary. The disenfranchised populace (powerless, trapped) sees it as pure Snare — they bear all costs (subject status, exclusion, suppression), with no exit and no benefit. The lesser nobility (moderate, constrained) sees Tangled Rope — they benefit from the succession stability but are also constrained by the hierarchy. The constitutional reform movement (organized, constrained) sees Scaffold — the constraint is temporary, being actively dismantled through constitutional amendment and electoral systems. The historian (analytical) risks seeing Mountain (naturalizing it as inevitable), but the false summit detector reveals this as a manufactured naturalization — the structural data shows beneficiaries and suppression, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply by agent position. The dynastic lineage (beneficiary, institutional power, arbitrage exit) has d ≈ 0.05 — they are the extraction target in reverse, the power holder, experiencing negative effective extractiveness (they extract from the system). The disenfranchised populace (victim, powerless, trapped) has d ≈ 0.95 — they are the maximum extraction target with no exit, experiencing f(d) ≈ 1.42 (maximum effective extractiveness). The constitutional reform movement (organized, constrained exit) has d ≈ 0.50 — they are neither pure beneficiary nor pure victim but rather positioned at the pivot point where the constraint's legitimacy is contested and the exit strategy is being constructed. The perspective-specific chi values flow directly from these directionality positions and the corresponding agent power levels; the Rope classification from the monarch's perspective reflects their position as beneficiary, while the Snare from the populace's perspective reflects their position as victim-with-no-exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the monarchical reading contains a genuine coordination function (succession stability, predictable authority lines, legitimacy narrative that reduces enforcement costs) layered with asymmetric extraction (the populace cannot participate in or revise the authority structure). This is the defining signature of Tangled Rope: both genuine coordination and irreversible asymmetry must be present. The constraint is NOT pure Rope (which would lack the asymmetry and suppression) and NOT pure Snare (which would lack the coordination function and beneficiary organization). The three-way perspectival structure (beneficiary sees Rope, victim sees Snare, analyst sees Tangled Rope hybrid) correctly maps the constraint's actual structure: it contains both elements, and which is salient depends on the observer's structural position and interests. The mandatrophy is resolved by privileging the analytical perspective as the structural truth — the constraint genuinely coordinates AND genuinely extracts; the beneficiary's Rope perception is partial (ignores extraction), and the victim's Snare perception is partial (ignores coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bloodline_necessity_vs_contingency,
    'Is hereditary succession a necessary feature of monarchical legitimacy, or is it contingent on specific historical contexts?',
    'Comparative analysis of monarchical systems: which rely on strict bloodline rules, which permit adoption or collateral succession, which have shifted from hereditary to elective succession within a single lineage framework. Examination of legitimacy narratives in each system.',
    'If necessary: the monarchical reading''s core axiom (legitimacy via birth) is foundational to the type. If contingent: alternative monarchical forms (elective monarchy, merit-based succession within dynastic framework) become structurally possible, potentially creating sub-types or rendering the distinction between monarchical and other readings less sharp.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bloodline_necessity_vs_contingency, conceptual, 'Whether bloodline succession is necessary or contingent to monarchical legitimacy').

omega_variable(
    extraction_vs_stability_tradeoff,
    'Does the hereditary legitimacy constraint extract value from the populace, or does it provide genuine stability benefit that outweighs the extraction cost?',
    'Historical comparison of stability metrics (frequency of violent succession disputes, civil wars, regime changes) in hereditary vs non-hereditary systems controlling for region and era. Welfare analysis comparing extractive burden against conflict reduction.',
    'If stability benefit outweighs extraction: reclassify from Tangled Rope toward Rope (coordination dominates). If extraction dominates: reclassify toward Snare. Current classification (Tangled Rope, ε=0.58) assumes mixed dynamics; resolution would shift the balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_stability_tradeoff, empirical, 'Whether hereditary legitimacy provides stability benefit sufficient to justify extraction burden').

omega_variable(
    kernelized_vs_naturalized_framing,
    'Is hereditary legitimacy presented as a deliberate political choice (kernelized commitment) or as an inevitable natural law?',
    'Textual and institutional analysis of how legitimacy is asserted: appeal to divine right, natural order, historical continuity, or explicit contract/constitution. Examination of whether the constraint is treated as revisable or immutable within its own framework.',
    'If kernelized (presented as chosen, revisable): the constraint''s epistemic status is clearer and the reading''s relationship to siblings becomes sharper. If naturalized: the false summit risk is higher — the reading disguises a contingent choice as necessity, confounding the analysis with the natural law sibling reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernelized_vs_naturalized_framing, conceptual, 'Whether hereditary legitimacy is framed as deliberate commitment or natural inevitability').

omega_variable(
    competing_authority_sources,
    'Can a system hold both hereditary legitimacy and an alternative legitimacy source (e.g., popular consent, religious authority) simultaneously, or do they necessarily conflict?',
    'Analysis of hybrid constitutional monarchies: how do they reconcile hereditary succession with representative government and popular sovereignty? Examination of which legitimacy source prevails in cases of conflict.',
    'If compatible: Tangled Rope classification confirmed (genuine coordination hybrid). If incompatible: the constraint forecloses other readings, and the three-way committer dispute (monarchical vs republican vs constitutional_hybrid) contains genuine logical foreclosure, not mere coexistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competing_authority_sources, conceptual, 'Whether hereditary legitimacy can coexist with alternative authority sources').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sov_mon_tr_t0, sovereign_legitimacy__monarchical_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(sov_mon_tr_t50, sovereign_legitimacy__monarchical_reading, theater_ratio, 50, 0.61).
narrative_ontology:measurement(sov_mon_tr_t100, sovereign_legitimacy__monarchical_reading, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(sov_mon_be_t0, sovereign_legitimacy__monarchical_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(sov_mon_be_t50, sovereign_legitimacy__monarchical_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(sov_mon_be_t100, sovereign_legitimacy__monarchical_reading, base_extractiveness, 100, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(sov_mon_su_t0, sovereign_legitimacy__monarchical_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(sov_mon_su_t50, sovereign_legitimacy__monarchical_reading, suppression_requirement, 50, 0.7).
narrative_ontology:measurement(sov_mon_su_t100, sovereign_legitimacy__monarchical_reading, suppression_requirement, 100, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, identity_coordination).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The sovereign_legitimacy kernel contains three structurally distinct constraints, each representing a different reading with different ε values and beneficiary/victim structures. The monarchical_reading (this file) has ε=0.58 and grounds legitimacy in bloodline. The republican_reading has ε that reflects legitimacy derived from popular will (expect higher ε due to electoral machinery overhead and higher volatility). The constitutional_hybrid_reading has ε that reflects legitimacy derived from written law constraining hereditary succession (expect lower ε than monarchical due to reduced suppression, or higher ε if the written constraint is used to legitimize hereditary privilege). All three constraints affect each other via authority legitimacy competition — the salience and enforcement of each reading depends on which is institutionally dominant and which political actors control interpretation of the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sovereign_legitimacy__monarchical_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
