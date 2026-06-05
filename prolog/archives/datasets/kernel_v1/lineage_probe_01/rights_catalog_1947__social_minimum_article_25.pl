% ============================================================================
% CONSTRAINT STORY: rights_catalog_1947__social_minimum_article_25
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rights_catalog_1947__social_minimum_article_25, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: rights_catalog_1947__social_minimum_article_25
 *   human_readable: Article 25: Social Minimum as Constitutional Right (1947 Catalog Reading)
 *   domain: constitutional_law/doctrinal_interpretation
 *
 * SUMMARY:
 *   Article 25 of the 1947 rights catalog promises all persons a right to
 *   'wholesome and cultured living' and obligates the state to social welfare
 *   and public assistance. This constraint embodies a distinctive doctrinal
 *   innovation: the constitutionalization of welfare obligation. Yet the
 *   innovation is paired with structural gaps — justiciability is notoriously
 *   thin, and the clause has generated decades of doctrine that invokes the
 *   promise while restraining enforcement. This story traces ONE reading of
 *   the contested rights catalog kernel: the social minimum reading that
 *   treats destitution suppression as a constitutional duty. The sibling
 *   readings (Article 24's family equality and Article 13's individual
 *   dignity) represent alternative doctrinal pathways that ground legitimacy
 *   differently. This reading specifically establishes welfare as a state
 *   obligation, not as charity or discretionary benefit. The constraint
 *   exhibits hybrid character: genuine coordination function (the clause
 *   legitimized welfare state construction) paired with systematic extraction
 *   (justiciability thinness, means-testing burden, administrative
 *   discretion) that permits indefinite underfunding despite the promise.
 *   Extractiveness rises over the measurement interval (0.22 → 0.38) as
 *   welfare programs mature and the gap between promised adequacy and actual
 *   benefit levels widens. Theater ratio rises similarly (0.45 → 0.68) as the
 *   doctrine of justiciability restraint becomes more elaborate — the clause
 *   is invoked more frequently but enforcement more carefully circumscribed.
 *
 * KEY AGENTS:
 *   - Destitute Claimants: Primary victims (powerless/trapped) — promised welfare rights but denied enforceable remedies due to justiciability doctrine
 *   - Welfare Claimants & Statutory Programs: Beneficiaries and constrained actors (moderate/constrained) — benefit from constitutional legitimacy but face administrative burden, stigma, and discretionary denial
 *   - Welfare State Apparatus: Institutional beneficiary (institutional/arbitrage) — gains authority and legitimacy to allocate welfare; experiences constraint as pure coordination
 *   - Minimal-State Constitutional Reading: Victim of doctrinal shift (institutional/constrained) — the social minimum reading locks the nation into welfare obligation, foreclosing minimal-state governance
 *   - Justiciability Doctrine: Institutional mediator (institutional/constrained) — performs the commitment while preserving judicial restraint; increasingly ossified theater
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees hybrid coordination-extraction structure; recognizes both innovation (welfare legitimization) and persistent extraction (thinness, underfunding)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rights_catalog_1947__social_minimum_article_25, 0.38).
domain_priors:suppression_score(rights_catalog_1947__social_minimum_article_25, 0.52).
domain_priors:theater_ratio(rights_catalog_1947__social_minimum_article_25, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rights_catalog_1947__social_minimum_article_25, extractiveness, 0.38).
narrative_ontology:constraint_metric(rights_catalog_1947__social_minimum_article_25, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(rights_catalog_1947__social_minimum_article_25, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rights_catalog_1947__social_minimum_article_25, tangled_rope).
narrative_ontology:human_readable(rights_catalog_1947__social_minimum_article_25, "Article 25: Social Minimum as Constitutional Right (1947 Catalog Reading)").
narrative_ontology:topic_domain(rights_catalog_1947__social_minimum_article_25, "constitutional_law/doctrinal_interpretation").

domain_priors:requires_active_enforcement(rights_catalog_1947__social_minimum_article_25).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rights_catalog_1947__social_minimum_article_25, '4398e8f1-5aca-4fd7-927c-d3776af4720f').
narrative_ontology:cs_kernel_codification('4398e8f1-5aca-4fd7-927c-d3776af4720f', formalized).
narrative_ontology:cs_authority_grounding('4398e8f1-5aca-4fd7-927c-d3776af4720f', lineage).
narrative_ontology:cs_interpretation_layer_present('4398e8f1-5aca-4fd7-927c-d3776af4720f').
narrative_ontology:cs_reading_relation('4398e8f1-5aca-4fd7-927c-d3776af4720f', rights_catalog_1947__equality_and_family_article_24, influences).
narrative_ontology:cs_reading_relation('4398e8f1-5aca-4fd7-927c-d3776af4720f', rights_catalog_1947__individual_dignity_article_13, influences).
narrative_ontology:cs_axiom('4398e8f1-5aca-4fd7-927c-d3776af4720f', foundational, destitution_suppression_state_obligation).
narrative_ontology:cs_axiom_status(destitution_suppression_state_obligation, holdable).
narrative_ontology:cs_axiom_grounding('4398e8f1-5aca-4fd7-927c-d3776af4720f', destitution_suppression_state_obligation, deontological).
narrative_ontology:cs_axiom('4398e8f1-5aca-4fd7-927c-d3776af4720f', secondary, justiciability_restraint_doctrine).
narrative_ontology:cs_axiom_status(justiciability_restraint_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('4398e8f1-5aca-4fd7-927c-d3776af4720f', justiciability_restraint_doctrine, conventional).
narrative_ontology:cs_reference_frame('4398e8f1-5aca-4fd7-927c-d3776af4720f', constitutional_welfare_obligation_framework).
narrative_ontology:cs_drift_state('4398e8f1-5aca-4fd7-927c-d3776af4720f', contemporary_50_year_mark, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('4398e8f1-5aca-4fd7-927c-d3776af4720f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(rights_catalog_1947__social_minimum_article_25, rights_catalog_1947).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rights_catalog_1947__social_minimum_article_25, welfare_claimants).
narrative_ontology:constraint_beneficiary(rights_catalog_1947__social_minimum_article_25, social_welfare_programs).
narrative_ontology:constraint_beneficiary(rights_catalog_1947__social_minimum_article_25, statutory_entitlement_infrastructure).
narrative_ontology:constraint_victim(rights_catalog_1947__social_minimum_article_25, minimal_state_readings).
narrative_ontology:constraint_victim(rights_catalog_1947__social_minimum_article_25, justiciability_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DESTITUTE CLAIMANTS (SNARE) — Article 25 promises a right to 'wholesome and cultured living,' yet justiciability is notoriously thin. The clause offers rhetorical protection but no accessible remedy. Trapped between the promise and the absence of enforceable channels, destitute agents experience maximum extraction: the constitution names them as rights-bearers while the doctrine denies them recourse. The suppression is structural — the clause itself creates legitimacy for welfare but simultaneously bars enforcement.
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELFARE CLAIMANTS & STATUTORY PROGRAMS (TANGLED ROPE) — The Article 25 reading created the legitimacy foundation for a welfare state. Claimants benefit from this doctrinal move: statutory schemes (unemployment, disability, old-age pensions) were built on the premise that destitution suppression is constitutional obligation, not mere charity. But the benefit is constrained by implementation gaps and means-testing regimes that extract administrative burden and stigma. The constraint both enables and suppresses — genuine coordination function (welfare legitimacy) alongside asymmetric extraction (bureaucratic gatekeeping, discretionary denial).
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: WELFARE STATE ADMINISTRATIVE APPARATUS (ROPE) — From the perspective of welfare agencies, pension boards, and statutory infrastructure, Article 25 is pure coordination. The clause justifies the existence and scope of welfare programs. Agencies extract administrative authority (power to allocate benefits, set criteria, manage caseloads) but experience this as legitimate coordination function. No serious exit option — the apparatus IS the implementation of the promise. Beneficiary with arbitrage capacity (can reallocate within the welfare budget, can set implementation standards).
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MINIMAL-STATE CONSTITUTIONAL READING (MOUNTAIN VICTIM) — The minimal-state reading of the catalog treats welfare as private responsibility, not state obligation. Article 25's social minimum reading inverts this premise: destitution suppression becomes a constitutional duty, not a discretionary matter. From the minimal-state view, this constitutionalization of welfare is an immutable legal transformation — the nation is locked into a welfare-oriented constitution regardless of economic or ideological preference. The minimal-state agent experiences the constraint as unchangeable at the constitutional level, though litigation and legislative amendment remain theoretically open (biographical time). The classification as mountain reflects the perceived immutability of the doctrinal commitment, but the strategic beneficiaries (welfare apparatus) and victims (anti-welfare ideologies) structure the extraction asymmetry.
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, mountain,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: JUSTICIABILITY DOCTRINE (PITON) — The Article 25 guarantee comes with notorious doctrinal thinness on enforcement. Courts invoke the clause for narrative and aspirational purposes but rarely grant enforceable remedies based on Article 25 alone. The doctrine is substantially performative: it performs the state's commitment to welfare while preserving judicial restraint. Over decades, justiciability doctrine has ossified into theater — the clause is cited, respected, but functionally degraded. The piton classification reflects the gap between theatrical invocation (high) and functional enforcement (low): the doctrine persists through institutional inertia and because alternatives (relying only on statutory schemes without constitutional foundation) are incomplete, not because it delivers real justiciable rights.
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, Article 25 represents a genuine structural innovation in constitutional vocabulary: welfare obligation has shifted from optional charity to named state duty. This is authentic coordination progress — the clause established the conceptual foundation for mid-20th-century welfare regimes across jurisdictions that adopted similar language. But the innovation is paired with systematic extraction: doctrinal thinness on justiciability preserves state discretion, means-testing extracts administrative burden from claimants, and the right-without-remedy structure permits indefinite underfunding. The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is hybrid, with genuine doctrinal progress and persistent structural gaps maintained by design.
constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rights_catalog_1947__social_minimum_article_25_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rights_catalog_1947__social_minimum_article_25, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(rights_catalog_1947__social_minimum_article_25, TR),
    TR >= 0.70.

:- end_tests(rights_catalog_1947__social_minimum_article_25_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate, rising over time. At the moment of constitutional adoption (t=0, ε=0.22), Article 25 primarily functioned as coordination — it legitimized welfare as state duty rather than private responsibility. This was genuine progress, shifting the normative foundation. Over 50 years, extractiveness rises as welfare systems mature and the gap widens between the promise ('wholesome and cultured living') and actual statutory benefits (often subsistence-level, means-tested, administratively burdensome). The rise reflects accumulation of extraction through justiciability doctrine — the clause itself remains unchanged, but its functional extraction increases as the doctrine insulates implementation choices from constitutional review. Suppression (0.52): Moderate-high. The constraint suppresses alternatives through the justiciability doctrine — the thin enforcement pathway makes it difficult to challenge welfare inadequacy on constitutional grounds. Claimants are formally promised a right but practically denied remedy. This suppression is doctrinal, not legal prohibition; the doctrine has force because courts treat justiciability restraint as binding. Theater ratio (0.68): High, rising. The clause is increasingly invoked in preambles, policy statements, and judicial rhetoric, but actual enforcement remains thin. The rise reflects growing divergence between invocation and remedy — the more courts elaborate the doctrine of restraint, the more theatrical the clause becomes. At t=50, the constraint functions as piton from the justiciability perspective: maintained through institutional inertia and because alternatives (relying only on statutes, without constitutional foundation) feel incomplete, not because enforcement works.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits sharp perspectival divergence across power positions. Destitute claimants (powerless/trapped) experience maximum extraction — the promise creates legitimacy that amplifies the betrayal of non-enforcement, locking them in psychological and structural dependence on a right they cannot vindicate. Welfare apparatus (institutional/arbitrage) experiences rope — pure coordination with no extraction. Welfare claimants (moderate/constrained) experience the hybrid: benefit from legitimacy but constrained by implementation gaps. Minimal-state reading (institutional/constrained) experiences mountain — the clause as an immutable constitutional lock. Justiciability doctrine (institutional/constrained) experiences piton — the constraint as degraded ritual. The analytical observer (civilizational/analytical) sees the full structure: genuine coordination progress paired with systematic extraction through doctrinal design. The perspectival gap reveals that the constraint's type depends entirely on the observer's structural position — their power, exit options, and beneficiary/victim status relative to welfare obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural relationship to the constraint. Destitute claimants are victims with no exit (trapped) → high d (0.92) → high f(d) → high experienced extractiveness. Welfare apparatus are beneficiaries with arbitrage capacity → low d (0.12) → low/negative f(d) → negative experienced extractiveness (pure benefit). Welfare claimants are partial beneficiaries with constrained exit → moderate d (0.55) → moderate f(d) ≈ 0.75 → moderate experienced extractiveness. Minimal-state reading is a victim of the doctrinal lock → moderately high d (0.70) → elevated f(d) → perceived immutability at biographical horizon. Justiciability doctrine is an institutional actor maintaining the constraint → moderate d (0.60) → f(d) ≈ 0.85. No directionality overrides needed — the derivation from beneficiary/victim status + exit options captures the structural relationships accurately.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    justiciability_thinness_origin,
    'Is the doctrinal thinness on Article 25 enforcement a deliberate constitutional choice, or an accident of judicial interpretation?',
    'Historical analysis of drafting records, committee debates, and early judicial commentary; comparison with jurisdictions that adopted stronger enforcement mechanisms for equivalent clauses',
    'If deliberate choice: the constraint is designed as hybrid (coordination + extraction) by constitutional intent. If interpretive accident: the constraint is hybrid by doctrine, not design — the foundational commitment is stronger than the implementations reveal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(justiciability_thinness_origin, empirical, 'Origin of justiciability doctrine thinness').

omega_variable(
    welfare_program_necessity,
    'Do statutory welfare schemes depend on Article 25''s constitutional foundation, or would they exist absent the constitutional promise?',
    'Counterfactual analysis of other jurisdictions without equivalent constitutional clauses; historical tracking of legislative welfare expansion in relation to Article 25 invocation',
    'If dependent: Article 25 provides genuine coordination function for welfare legitimacy (rope at institutional level). If independent: welfare programs would exist on statutory grounds alone, and Article 25 is rhetorical overlay (piton).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_program_necessity, empirical, 'Causal role of Article 25 in welfare program architecture').

omega_variable(
    minimal_state_reading_survivability,
    'Is a minimal-state constitutional reading logically compatible with Article 25''s explicit welfare obligation, or does Article 25 foreclose it at the level of commitment?',
    'Doctrinal analysis of interpretive techniques (constitutional amendment, narrowing construction, delegitimization of precedent) that minimal-state theorists use to undermine Article 25''s force',
    'If logically incompatible: Article 25 forecloses the minimal-state reading (relation = forecloses). If coexistent through interpretive techniques: both readings survive in different jurisdictional or ideological spaces (relation = coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimal_state_reading_survivability, conceptual, 'Logical compatibility between minimal-state and Article 25 readings').

omega_variable(
    destitution_suppression_scope,
    'What constitutes ''suppression of destitution'' under Article 25 — bare subsistence, adequacy for participation, or cultivation of human capability?',
    'Doctrinal comparison across jurisdictions: constitutional courts'' interpretations of ''wholesome and cultured living''; empirical mapping of benefit adequacy thresholds',
    'If bare subsistence: extractiveness lower, theater lower. If adequacy for participation: extractiveness higher, welfare programs closer to functional. If capability cultivation: extractiveness depends on how courts value non-economic rights (dignity, cultural participation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(destitution_suppression_scope, conceptual, 'Semantic scope of destitution suppression').

omega_variable(
    reading_boundary_with_article_24,
    'Does Article 25''s social minimum reading logically depend on Article 24''s family equality reading, or are they independent doctrinal foundations?',
    'Doctrinal analysis of how Article 24 (family rights, equal marriage) is used to ground Article 25 claims (spousal welfare, child benefit eligibility); jurisdictional comparison of whether Article 25 interpretation differs in societies with weak Article 24 implementation',
    'If dependent: the readings influence each other structurally. If independent: each has its own doctrinal sufficiency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_with_article_24, conceptual, 'Logical dependence between Article 25 and Article 24 readings').

omega_variable(
    kernel_reading_ambiguity,
    'Is this constraint (Article 25 as social minimum) a stable reading of the constitutional kernel, or a contested interpretation still subject to reversal?',
    'Jurisdictional survey of constitutional courts'' fidelity to Article 25 welfare interpretation over 75+ years; documentation of rollback attempts, narrowing constructions, or reaffirmations',
    'If stable: the reading has achieved interpretive closure (holdable axioms). If contested: the axiom status is uncertain, and the constraint is subject to foreclosure if political conditions shift.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Stability and closure of the Article 25 reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rights_catalog_1947__social_minimum_article_25, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sm25_theater_t0, rights_catalog_1947__social_minimum_article_25, theater_ratio, 0, 0.45).
narrative_ontology:measurement(sm25_theater_t25, rights_catalog_1947__social_minimum_article_25, theater_ratio, 25, 0.62).
narrative_ontology:measurement(sm25_theater_t50, rights_catalog_1947__social_minimum_article_25, theater_ratio, 50, 0.68).

% Extraction over time
narrative_ontology:measurement(sm25_extract_t0, rights_catalog_1947__social_minimum_article_25, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sm25_extract_t25, rights_catalog_1947__social_minimum_article_25, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(sm25_extract_t50, rights_catalog_1947__social_minimum_article_25, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(sm25_suppress_t0, rights_catalog_1947__social_minimum_article_25, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(sm25_suppress_t25, rights_catalog_1947__social_minimum_article_25, suppression_requirement, 25, 0.48).
narrative_ontology:measurement(sm25_suppress_t50, rights_catalog_1947__social_minimum_article_25, suppression_requirement, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rights_catalog_1947__social_minimum_article_25, resource_allocation).
narrative_ontology:boltzmann_floor_override(rights_catalog_1947__social_minimum_article_25, 0.18).
narrative_ontology:affects_constraint(rights_catalog_1947__social_minimum_article_25, rights_catalog_1947__equality_and_family_article_24).
narrative_ontology:affects_constraint(rights_catalog_1947__social_minimum_article_25, rights_catalog_1947__individual_dignity_article_13).
narrative_ontology:affects_constraint(rights_catalog_1947__social_minimum_article_25, welfare_justiciability_doctrine).
narrative_ontology:affects_constraint(rights_catalog_1947__social_minimum_article_25, means_testing_administrative_extraction).

% DUAL FORMULATION NOTE:
% This constraint is part of the rights_catalog_1947 kernel family. Article 25 (social minimum) is one reading; Article 24 (family equality) and Article 13 (individual dignity) are sibling readings with different ε values and beneficiary/victim structures. The extractiveness values differ because each reading grounds legitimacy differently and has different enforcement mechanisms. Network links show how the readings influence each other: welfare claims invoke family and dignity protections; equality doctrine affects family benefit structures; dignity protections inform adequacy standards for welfare. Decomposition follows the ε-invariance principle: measuring welfare obligation (Article 25) yields ε=0.38; measuring family equality (Article 24) yields different ε (lower, because family equality is more justiciable); measuring individual dignity (Article 13) yields yet another ε (foundational, lower suppression). Each reading is a distinct constraint with stable ε; network edges show structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
