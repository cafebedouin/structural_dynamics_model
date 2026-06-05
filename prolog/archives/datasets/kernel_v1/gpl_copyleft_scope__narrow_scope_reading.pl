% ============================================================================
% CONSTRAINT STORY: gpl_copyleft_scope__narrow_scope_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_copyleft_scope__narrow_scope_reading, []).

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
 *   constraint_id: gpl_copyleft_scope__narrow_scope_reading
 *   human_readable: GPL Section 2(b) Narrow Copyleft Scope (Traditional Copyright Doctrine Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   GPL Section 2(b) establishes a copyleft obligation: any work that copies
 *   or modifies the licensed code must itself be distributed under the GPL.
 *   The narrow reading interprets 'derivative work' according to traditional
 *   copyright law doctrine, which excludes aggregation, plugin architectures,
 *   and dynamic linking — these are treated as separate works under
 *   copyright, not modifications of the GPL'd code. This reading generates
 *   moderate-extraction tangled rope dynamics. Commercial integrators gain
 *   coordination benefits (legal certainty for mixed-license products) and an
 *   extraction benefit (flexibility to wrap GPL components in proprietary
 *   layers without triggering copyleft). The GPL advocacy community suffers:
 *   their foundational assumption — that copyleft would induce universal
 *   code-sharing — is structurally weakened by a copyright-doctrine
 *   interpretation they cannot easily challenge. Enforcement machinery
 *   persists but has decayed in effectiveness (piton). Organized reform
 *   coalitions (GPLv3, AGPL, Commons Clause) are building layer-by-layer
 *   alternatives that work within the narrow reading's constraints
 *   (scaffold). The constraint is one reading of a contested kernel: how
 *   should Section 2(b) be interpreted when copyright doctrine's 'derivative
 *   work' category is narrower than the drafters' apparent intent? The narrow
 *   reading coexists with stronger-copyleft alternatives without logically
 *   foreclosing them; rather, it creates structural pressure on the advocacy
 *   community to build explicit workarounds.
 *
 * KEY AGENTS:
 *   - Commercial Integrators (institutional/arbitrage): Primary beneficiary — gains legal flexibility to combine GPL components with proprietary layers; experiences constraint as coordination mechanism
 *   - GPL Copyleft Advocacy Community (powerless/trapped): Primary victim — foundational assumption of universal code-sharing is structurally undermined; trapped within GPL framework they cannot easily exit or rewrite
 *   - Secondary GPL Contributors (powerful/mobile): Mixed position — benefit from GPL's coordination mechanism but suffer from narrow derivative-work boundary that reduces their leverage
 *   - GPL Enforcement Machinery (institutional/arbitrage): Institutional actor experiencing piton degradation — enforcement actions against linking/plugin architectures fail under copyright doctrine; apparatus persists through inertia
 *   - License Drafting Reform Coalition (organized/constrained): Organized actors building structured alternatives (GPLv3, AGPL, Commons Clause) that work around the narrow reading
 *   - Analytical Observer (analytical/analytical): Civilizational view at risk of naturalizing contingent copyright-doctrine framing as immutable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_copyleft_scope__narrow_scope_reading, 0.38).
domain_priors:suppression_score(gpl_copyleft_scope__narrow_scope_reading, 0.42).
domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_copyleft_scope__narrow_scope_reading, tangled_rope).
narrative_ontology:human_readable(gpl_copyleft_scope__narrow_scope_reading, "GPL Section 2(b) Narrow Copyleft Scope (Traditional Copyright Doctrine Reading)").
narrative_ontology:topic_domain(gpl_copyleft_scope__narrow_scope_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_copyleft_scope__narrow_scope_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_copyleft_scope__narrow_scope_reading, '53be1e1d-1b45-4da4-9920-cdabf29291d2').
narrative_ontology:cs_kernel_codification('53be1e1d-1b45-4da4-9920-cdabf29291d2', fixed_text).
narrative_ontology:cs_authority_grounding('53be1e1d-1b45-4da4-9920-cdabf29291d2', extraction).
narrative_ontology:cs_interpretation_layer_present('53be1e1d-1b45-4da4-9920-cdabf29291d2').
narrative_ontology:cs_reading_relation('53be1e1d-1b45-4da4-9920-cdabf29291d2', gpl_copyleft_scope__strong_copyleft_reading, coexists_with).
narrative_ontology:cs_reading_relation('53be1e1d-1b45-4da4-9920-cdabf29291d2', gpl_copyleft_scope__enforcement_vacuum_reading, influences).
narrative_ontology:cs_axiom('53be1e1d-1b45-4da4-9920-cdabf29291d2', foundational, copyright_doctrine_is_controlling_framework).
narrative_ontology:cs_axiom_status(copyright_doctrine_is_controlling_framework, holdable).
narrative_ontology:cs_axiom_grounding('53be1e1d-1b45-4da4-9920-cdabf29291d2', copyright_doctrine_is_controlling_framework, conventional).
narrative_ontology:cs_axiom('53be1e1d-1b45-4da4-9920-cdabf29291d2', secondary, aggregation_and_dynamic_linking_are_separate_works).
narrative_ontology:cs_axiom_status(aggregation_and_dynamic_linking_are_separate_works, holdable).
narrative_ontology:cs_axiom_grounding('53be1e1d-1b45-4da4-9920-cdabf29291d2', aggregation_and_dynamic_linking_are_separate_works, empirically_contingent).
narrative_ontology:cs_reference_frame('53be1e1d-1b45-4da4-9920-cdabf29291d2', copyright_doctrine_interpretation_of_derivative_work).
narrative_ontology:cs_drift_state('53be1e1d-1b45-4da4-9920-cdabf29291d2', contemporary_post_appellate_precedent, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('53be1e1d-1b45-4da4-9920-cdabf29291d2', '').
narrative_ontology:cs_kernel_id(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, commercial_integrators).
narrative_ontology:constraint_beneficiary(gpl_copyleft_scope__narrow_scope_reading, proprietary_layer_vendors).
narrative_ontology:constraint_victim(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_advocacy_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GPL COPYLEFT ADVOCACY (SNARE) — Trapped by the narrow reading's legal entrenchment. The community's foundational assumption — that Section 2(b) mandates universal code-sharing for all derivative works — is structurally undermined by a copyright-doctrine interpretation that excludes aggregation, plugins, and dynamic linking. No viable exit: the courts have adopted the narrow reading, enforcement actions fail, and rewriting the license requires consensus that will not materialize. Full extraction: the commercial integrators gain flexibility while the advocacy community's coordination mechanism (copyleft obligation) loses force. Zero degrees of freedom for reorienting strategy within the existing license framework.
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMERCIAL INTEGRATORS (ROPE) — Experiences the narrow reading as coordination: GPL components can be integrated with proprietary layers without triggering copyleft obligations. The constraint solves a genuine collective action problem — how to mix open and closed code in a single product. Legal certainty around derivative work boundaries enables product architecture. The integrator benefits from the clarity (arbitrage: can exit GPL use entirely if needed, or structure code to avoid derivative-work triggers). Coordination without maximal coercion.
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SECONDARY GPL CONTRIBUTORS (TANGLED ROPE) — Benefit from the license's attribution and attribution-share clauses, but suffer extraction via the narrow derivative-work boundary. A contributor to a GPL library that gets wrapped in a proprietary plugin cannot enforce copyleft against the plugin vendor. Mixed dynamics: genuine coordination (the GPL library is maintained as shared commons) coexists with structural asymmetry (the narrower the derivative-work boundary, the less secondary contributors' leverage over downstream use).
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: GPL ENFORCEMENT MACHINERY (PITON) — The formal enforcement mechanism (licensing audits, cease-and-desist letters, litigation threats) has largely decayed in effectiveness for the narrow-scope boundaries. Enforcement actions against dynamic linking or plugin architectures consistently fail under copyright law doctrine. The enforcement apparatus persists through institutional inertia — Software Freedom Conservancy, FSF, and individual copyright holders maintain the machinery — but its functional goal (inducing universal code-sharing) has atrophied. Theater ratio is high because enforcement claims rest on interpretations of 'derivative work' that courts reject; the ritual persists but produces few victories.
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LICENSE DRAFTING REFORM COALITION (SCAFFOLD) — Organized actors (GPLv3 drafters, AGPL advocates, Commons Clause projects) recognize that the narrow reading is a temporary institutional arrangement and are building explicit exit paths. GPLv3 strengthened language around linking and aggregation; AGPL added network-use provisions; Commons Clause wraps GPL with proprietary-style restrictions. These represent structured sunset alternatives: copyleft advocates accept the narrow reading as a constraint they must work around rather than a settled law. The sunset is a strategy (accept the narrow reading, build layer-by-layer alternatives) rather than a formal license term, but it functions as scaffold logic.
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COPYRIGHT DOCTRINE (MOUNTAIN) — From the civilizational perspective, the narrow reading appears as an immutable consequence of traditional copyright law. Derivative work is a fixed legal category rooted in the Copyright Act; modification, translation, and preparation of adaptations are enumerated; the enumeration is exhaustive; plugin architectures and dynamic linking do not fit the statutory language. This perspective naturalizes the narrow reading as a necessary consequence of copyright doctrine. However, the structural data reveals this as a false summit: the 'immutability' is contingent on the choice to apply copyright doctrine rather than trade secret or technology law frameworks; different jurisdictions apply different doctrinal lenses; the GPL could be rewritten to extend beyond derivative work; the narrowness is a strategic choice, not a law of nature.
constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_copyleft_scope__narrow_scope_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_copyleft_scope__narrow_scope_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_copyleft_scope__narrow_scope_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Commercial integrators benefit from legal clarity and flexibility to structure code with proprietary layers. But the extraction is not maximal (0.7+) because: (a) GPL projects still capture significant benefit from being used in commercial products — user base, funding, development attention; (b) the narrow reading has been judicially established, reducing uncertainty for both GPL advocates and integrators; (c) secondary contributors retain attribution and copyleft benefits within the GPL-licensed portions. The 0.38 value reflects genuine coordination benefits for integrators (solving the mixed-license problem) alongside extraction from the advocacy community (weakened enforcement). Suppression (0.42): Moderate. Barriers to escape the narrow reading include: copyright law's established doctrine (difficult to challenge through GPL rewriting); judicial precedent against broader derivative-work interpretations; the GPL's foundational license-text framing (rewording to extend beyond copyright categories requires consensus). But suppression is not total: GPLv3, AGPL, and Commons Clause demonstrate structured alternatives; jurisdictions apply doctrine differently; projects can migrate to stronger copyleft frameworks. Theater ratio (0.58): Moderate-high. Rising over the interval. Enforcement actions against linking and plugin architectures rest on interpretations of 'derivative work' that courts consistently reject. The enforcement machinery (Conservancy, FSF litigation threats, copyright audits) persists despite low success rates, suggesting performative character — the ritual of enforcement maintains the illusion of copyleft strength even as court decisions narrow its scope. Theater increases as the gap widens between copyleft advocates' expectations and enforcement outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme. The commercial integrator sees coordination (Rope) — the narrow reading solves a genuine problem of mixed-license architecture. The advocacy community sees pure extraction (Snare) — their foundational copyleft mechanism has been structurally undermined by copyright doctrine in a way they cannot escape. The reform coalition sees a problem to work around with structured alternatives (Scaffold). The enforcement machinery sees its own decay (Piton) — the ritual persists but produces few victories. The analytical observer risks naturalizing copyright doctrine as immutable law (Mountain), when in fact the narrowness is a choice to apply one legal framework rather than others. The gap reveals that all six types are legitimate readings of the same constraint from different structural positions — the narrow reading's nature depends on where you stand relative to it.
 *
 * DIRECTIONALITY LOGIC:
 *   The narrow reading's directionality differs sharply by agent perspective. Commercial integrators (institutional/arbitrage) experience low d — they are net beneficiaries who can exit GPL use entirely if needed, giving them high flexibility. The advocacy community (powerless/trapped) experiences high d — they are net victims facing legal entrenchment of their loss through copyright doctrine; trapped by their own license choice and the courts' interpretation of copyright law. Secondary contributors (powerful/mobile) experience moderate d — they retain some benefit from the GPL's coordination function but suffer extraction through the narrow derivative-work boundary. These differences are not derived from a single power level but from the agent's structural position relative to THIS specific constraint: the beneficiary's exit flexibility, the victim's inability to escape copyright-doctrine interpretation, the secondary actor's split position. The narrow reading's extraction runs toward integrators and away from advocates — a clear directionality gradient that prevents uniform classification across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how a single institutional interpretation (copyright-doctrine reading of Section 2(b)) can simultaneously be coordination mechanism for one agent (commercial integrator), extraction mechanism for another (advocacy community), degraded ritual for a third (enforcement machinery), and work-around target for a fourth (reform coalition). The mandatrophy is resolved by accepting that the constraint's classification depends on observational position and that all six types are structural facts rather than errors. The narrow reading creates genuine coordination value for mixed-license software while extracting from copyleft advocates' capability to enforce universal code-sharing. Both are real. The apparent contradiction dissolves when the perspectival nature of the classification is made explicit. No single type is 'correct' — the presheaf over the full set of positions is the complete description.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    derivative_work_boundary_contest,
    'Does ''derivative work'' under copyright law structurally include dynamic linking, plugin loading, and aggregation in linked binaries, or are these legally separable?',
    'Appellate precedent on linking architectures (currently split: some jurisdictions recognize derivative-work status for dynamic linking; others treat it as separate distribution). Test case: jurisdiction-by-jurisdiction survey of GPL enforcement outcomes and court findings on linking scope.',
    'If derivative work scope widens: narrow reading collapses into strong copyleft reading; extraction flows toward advocacy community. If scope narrows further: narrow reading entrenches; GPL loses effective enforcement against integration strategies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(derivative_work_boundary_contest, empirical, 'Whether copyright derivative-work doctrine encompasses dynamic linking and plugin architectures').

omega_variable(
    doctrine_versus_intention_gap,
    'Is the narrow reading a faithful application of GPL drafters'' original intent (Stallman, Moglen), or has it diverged from the drafters'' expectations through judicial interpretation of copyright doctrine?',
    'Textual analysis of GPL preamble, Section 2(b) drafting history, and founding FSF documentation. Comparison with GPLv2 vs GPLv3 language evolution. Interviews with drafters on linking scope.',
    'If faithful: narrow reading is intentional constraint design, not interpretive drift. If diverged: the narrow reading is an unintended side effect of applying copyright law to a license designed with stronger assumptions about code-sharing. This omega flags the identity-lock mechanism: GPL advocates may be ''trapped'' by their own license''s vulnerability to copyright-doctrine interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_versus_intention_gap, empirical, 'Alignment between GPL drafters'' intent and current copyright-doctrine interpretation of Section 2(b)').

omega_variable(
    commercial_integration_necessity,
    'Do commercial integrators actually require the narrow reading to build mixed-license products, or could they structure code to comply with stronger copyleft obligations at acceptable cost?',
    'Cost-benefit analysis: survey of proprietary software vendors on compliance costs for universal copyleft vs. current narrow-boundary compliance. Measurement of architectural flexibility lost under universal copyleft.',
    'If necessity is real: the narrow reading solves a genuine coordination problem and the rope classification is accurate. If necessity is exaggerated: the extraction is larger than claimed, the beneficiary''s escape hatch is less justified, and the constraint shifts toward snare territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_integration_necessity, empirical, 'Whether commercial integrators need the narrow derivative-work boundary to feasibly build mixed-license software').

omega_variable(
    reading_collapse_mechanism,
    'Which conditions would cause this narrow reading to coalesce with or diverge from the strong copyleft reading or enforcement vacuum reading?',
    'Scenario analysis: (a) Court decision widening derivative-work scope → narrow reading collapses into strong copyleft reading. (b) Adoption of GPLv4 with explicit linking/plugin language → narrow reading becomes obsolete but re-emerges in software using GPLv2/v3. (c) Shift in commercial practice toward stronger copyleft (Affero, Commons Clause) → narrow reading becomes enforcement-vacuum reading (agreement without mechanism). (d) Jurisdictional fragmentation (EU stronger protections, US narrower scope) → narrow reading persists in some jurisdictions, collapses in others.',
    'Determines long-term stability and stability of this reading as a kernel interpretation. High-impact omega for strategic planning by advocacy community.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_collapse_mechanism, conceptual, 'Conditions under which this narrow reading would merge with or split from sibling readings').

omega_variable(
    identity_lock_via_license_choice,
    'Are GPL adopters identity-locked to Section 2(b)''s copyright-doctrine interpretation by their foundational commitment to copyleft, even when the copyright framework undermines their stated goals?',
    'Interview and historical analysis: Do GPL project maintainers perceive the narrow reading as a changeable institutional artifact or as an immutable feature of ''how GPL works''? Measure the cognitive cost of shifting to alternative licenses (AGPL, Commons Clause, custom copyleft) vs. accepting the narrow reading.',
    'If identity-locked: the constraint''s binding mechanism is cognitive (developers have internalized the narrow reading as ''GPL''s nature'') rather than structural (the law forces it). Reclassifies the trapped perspective as identity_locked, revealing that the advocacy community''s binding is self-imposed through license commitment rather than external legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_via_license_choice, conceptual, 'Whether GPL adopters are identity-locked to narrow-reading interpretation through their license commitment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_copyleft_scope__narrow_scope_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_narrow_tr_t0, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gpl_narrow_tr_t10, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(gpl_narrow_tr_t20, gpl_copyleft_scope__narrow_scope_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(gpl_narrow_be_t0, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(gpl_narrow_be_t10, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(gpl_narrow_be_t20, gpl_copyleft_scope__narrow_scope_reading, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_copyleft_scope__narrow_scope_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__strong_copyleft_reading).
narrative_ontology:affects_constraint(gpl_copyleft_scope__narrow_scope_reading, gpl_copyleft_scope__enforcement_vacuum_reading).

% DUAL FORMULATION NOTE:
% The narrow reading, strong reading, and enforcement vacuum reading are three interpretations of the same kernel (GPL Section 2(b)). They are not independent constraints but rather three ways of reading the same license text. The network edges indicate that the narrow reading's entrenchment creates structural pressure on both sibling readings: it reduces the feasibility of strong copyleft enforcement (influences strong reading) and it concentrates the enforcement-vacuum effect by narrowing the scope where enforcement is even theoretically attempted (influences enforcement vacuum reading). The three stories must be treated as a reading family — they share a kernel and presheaf over different interpretive positions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_copyleft_scope__narrow_scope_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
