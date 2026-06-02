% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative Right (Statutory Exception Reading)
 *   domain: intellectual_property_law/information_economics
 *
 * SUMMARY:
 *   The 'transformative right' reading of fair use holds that copyright's
 *   statutory exception for fair use exists fundamentally to enable
 *   transformative cultural production and innovation, with the corollary
 *   that courts must facilitate such innovation by broadly interpreting
 *   transformative use and skeptically scrutinizing licensing markets as a
 *   basis for fair use denial. This reading crystallized in the Supreme
 *   Court's Harper & Row and Sony decisions and matured in Campbell v.
 *   Acuff-Rose (2 Live Crew parody case) and subsequent precedent,
 *   particularly Google Books (Authors Guild v. Google) and Andy Warhol
 *   Foundation cases. Under this reading, the transformative test becomes the
 *   dominant fair use factor, and commercial availability of licensing is not
 *   dispositive of fair use denial. This constraint story instantiates ONE
 *   reading of the contested fair use kernel — the kernel being the Copyright
 *   Act §107 statutory exception itself, which requires courts to balance
 *   four factors without specifying which should dominate. The transformative
 *   right reading privileges innovation/cultural production; sibling readings
 *   (narrow defense reading, market licensing reading) privilege copyright
 *   holder incentives or licensing markets as the primary coordination
 *   function. These readings coexist in contemporary jurisprudence, with
 *   different judicial coalitions and different doctrinal emphasis. The
 *   analytical task is to model THIS reading as a coherent constraint with
 *   its own ε, beneficiary/victim structure, and internal logic, while
 *   routing the kernel contest to omega variables and cs_structure fields.
 *
 * KEY AGENTS:
 *   - Transformative Creators: Primary beneficiary (institutional/arbitrage) — authors, artists, filmmakers, remixers, parody producers; benefit from broad fair use exemption enabling unlicensed access to source material
 *   - Copyright Holders / Licensing Intermediaries: Primary victim (powerless/trapped) — original rights holders (publishers, record labels, film studios, photographers); suppressed from licensing negotiations and licensing revenue by statutory exemption
 *   - Educational Institutions: Secondary victim (moderate/constrained) — universities, K-12 schools; benefit from fair use exemption for educational use but face litigation risk and uncertain standards
 *   - Open Culture Coalition: Organized beneficiary (organized/mobile) — Creative Commons advocates, open licensing supporters; see fair use as transitional pathway to explicit permission norms
 *   - Courts/Legal Doctrine: Institutional maintainer (institutional/constrained) — judicial system applying four-factor test with high variance in transformativeness application; doctrine is performative in application despite coherent statement
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees the constraint as a genuine tension between two coordination functions (creator incentives vs. cultural evolution) rather than a pathology to be eliminated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.38).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.52).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative Right (Statutory Exception Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__transformative_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '9c9d8b96-3c6c-4fcd-98a7-e837570af443').
narrative_ontology:cs_kernel_codification('9c9d8b96-3c6c-4fcd-98a7-e837570af443', fixed_text).
narrative_ontology:cs_authority_grounding('9c9d8b96-3c6c-4fcd-98a7-e837570af443', lineage).
narrative_ontology:cs_interpretation_layer_present('9c9d8b96-3c6c-4fcd-98a7-e837570af443').
narrative_ontology:cs_reading_relation('9c9d8b96-3c6c-4fcd-98a7-e837570af443', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('9c9d8b96-3c6c-4fcd-98a7-e837570af443', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('9c9d8b96-3c6c-4fcd-98a7-e837570af443', foundational, transformative_purpose_overrides_market).
narrative_ontology:cs_axiom_status(transformative_purpose_overrides_market, holdable).
narrative_ontology:cs_axiom_grounding('9c9d8b96-3c6c-4fcd-98a7-e837570af443', transformative_purpose_overrides_market, deontological).
narrative_ontology:cs_axiom('9c9d8b96-3c6c-4fcd-98a7-e837570af443', foundational, cultural_evolution_requires_unlicensed_access).
narrative_ontology:cs_axiom_status(cultural_evolution_requires_unlicensed_access, holdable).
narrative_ontology:cs_axiom_grounding('9c9d8b96-3c6c-4fcd-98a7-e837570af443', cultural_evolution_requires_unlicensed_access, empirically_contingent).
narrative_ontology:cs_reference_frame('9c9d8b96-3c6c-4fcd-98a7-e837570af443', copyright_system_with_balanced_incentives).
narrative_ontology:cs_drift_state('9c9d8b96-3c6c-4fcd-98a7-e837570af443', contemporary_digital_remix_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9c9d8b96-3c6c-4fcd-98a7-e837570af443', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, transformative_creators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, cultural_producers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, educational_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, original_copyright_holders).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, market_licensing_intermediaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COPYRIGHT HOLDER / LICENSING INTERMEDIARY (SNARE) — Faces extraction through the fair use exemption, which permits reuse without licensing negotiation or revenue sharing. Cannot exit or organize effectively; suppressed by statutory override that forecloses licensing markets. Experiences maximum asymmetric extraction from the reading's core premise that transformative use trumps licensing rights.
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRANSFORMATIVE CREATOR / INSTITUTIONAL BENEFICIARY (ROPE) — Benefits from fair use exemption as a coordination mechanism that enables cultural production and remix without licensing friction. Experiences the constraint as pure coordination: fair use solves the collective action problem of accessing source material for new creative work. Can arbitrage between institutional funding and access-without-licensing models.
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: EDUCATIONAL INSTITUTION (TANGLED ROPE) — Experiences both genuine coordination (fair use enables educational access without licensing) and asymmetric extraction (cannot use material for commercial purposes; faces fair use litigation risk; must prove transformative purpose). Constrained by uncertain legal standards and resource barriers to litigation. Genuine coordination function exists (education is recognized as transformative) but enforcement overhead and litigation risk create extraction asymmetry.
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LEGAL DOCTRINE / FAIR USE STANDARD (PITON) — The transformative test has become substantially performative: courts deploy 'transformative' as a legitimacy label with high variance in application. The doctrine persists through institutional inertia (precedent weight, institutional incentives) despite low functional coherence across cases. Theater ratio reflects that the four-factor test is ritually performed but outcome variance suggests the doctrine is not reliably capturing its stated function (protecting cultural innovation).
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN CULTURE COALITION / CREATIVE COMMONS MOVEMENT (SCAFFOLD) — Organized agents (Creative Commons, open licensing advocates, remix culture) see fair use as a temporary scaffold enabling broader access norms. The coalition has structured alternative pathways (open licenses, explicit permission frameworks, commons-based production) that reduce dependence on fair use litigation. Experiences fair use as transitional coordination with a sunset clause: as open licensing norms mature and creators adopt explicit permission frameworks, the need for fair use exemptions should decline.
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational analytical perspective, this reading instantiates a tension between two legitimate coordination functions: (1) protecting creators' incentives to produce new work (through copyright monopoly), and (2) enabling transformative reuse necessary for cultural evolution and innovation. The reading prioritizes (2) over (1) through the transformative test. The classification as tangled_rope reflects that both coordination functions are genuine, but the reading's architecture creates asymmetric extraction from copyright holders in favor of transformative creators. The tension is structural, not pathological — the two coordination functions are in genuine tension at the boundary cases.
constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_statutory_exception__transformative_right_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, TR),
    TR >= 0.70.

:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The transformative reading creates asymmetric extraction from copyright holders by exempting transformative uses from licensing negotiation. However, the extraction is not extreme (ε < 0.46) because: (1) licensing markets coexist with fair use in many sectors, suggesting adaptation rather than foreclosure; (2) copyright incentives for original creation remain intact; (3) the exemption applies only to transformative uses, not wholesale copying; (4) court application is uncertain enough that some licensing negotiations still occur even for potentially fair uses. The trajectory shows modest increase from 0.28 to 0.38 over the interval, reflecting gradual expansion of what courts recognize as transformative (from parody in Campbell to search engines in Google Books to transformative use in contemporary visual art). Suppression (0.52): Moderate-high. Copyright holders are structurally suppressed from licensing negotiations and cannot exit the exemption; the statutory exception forecloses private ordering. However, suppression is not extreme (≤ 0.60, the snare floor) because licensing markets still function in many domains, and the four-factor test still requires case-by-case judgment — it is not blanket exemption. Theater ratio (0.58): Moderate-high. The transformative test functions both as a genuine analytical tool (identifying uses that add new creative meaning) and as a legitimacy ritual (courts deploy 'transformative' language to justify outcomes determined by other factors). The increasing trajectory (0.42 → 0.58) reflects the doctrine's growing reliance on transformative language as the primary analytical frame, with corresponding increase in performative deployment.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the copyright holder's Snare (trapped, powerless, zero exit) and the beneficiary's Rope (institutional, arbitrage, pure coordination). The copyright holder experiences the exemption as pure extraction with no coordination benefit — their income stream is suppressed without receiving any offsetting benefit. The transformative creator experiences the exemption as pure coordination — it solves the transaction cost problem of licensing and enables creative innovation. The educational institution occupies a middle ground (Tangled Rope) — genuine coordination function (education is legitimate transformative purpose) combined with asymmetric legal risk (must prove transformative purpose; litigation costs suppress smaller educational uses). The legal doctrine (Piton) reveals that courts apply the transformative standard inconsistently, with high variance in outcomes despite consistent rhetoric. The analytical observer (Tangled Rope) recognizes that the reading encodes a genuine tension between two coordination functions (creator incentives and cultural evolution) without resolving which should dominate at boundary cases.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position: copyright holders face suppression from statutory override (trapped exit, victim status) → high d → high f(d) → high experienced extraction. Transformative creators face low barriers to access and benefit from exemption (institutional power, arbitrage exit, beneficiary status) → low d → low/negative f(d) → low/negative experienced extraction. The scope modifier (national/global) amplifies the extractiveness by σ(1.0 to 1.2), reflecting that fair use interpretation at the national level (U.S. courts) increasingly influences global licensing practices through the weight of U.S. jurisprudence and platform policy. Educational institutions occupy a middle position (moderate power, constrained exit, mixed victim/beneficiary status) → medium d → medium f(d) → medium χ. The analytical observer with arbitrage-adjacent institutional status derives a higher d (≈0.72) reflecting the observer's position at the margin of the extraction structure — seeing both the beneficiary and victim positions from an equidistant analytical vantage.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resolves the mandatrophy by accepting the genuine tension between copyright holder protection and transformative creator enablement. The reading does not claim both functions can be fully optimized — it prioritizes transformative cultural production over licensing market optimization. The extractiveness from copyright holder perspective (Snare, χ ≈ 0.75-0.85 accounting for scope amplification) is not denied or minimized; it is accepted as the structural consequence of privileging innovation over copyright holder revenue. The mandatrophy is resolved not by denying extraction but by asserting that the extraction serves a higher coordination function (cultural evolution enabling civilization-scale progress) than the denied coordination function (licensing market efficiency). This moves the classification from 'unresolved tension between competing claims' to 'resolved asymmetry with explicit value prioritization.' The open culture coalition perspective (Scaffold) offers a further resolution pathway: if open licensing norms mature sufficiently, the scaffold enables sunset of the fair use exemption itself, as creators voluntarily adopt explicit permission frameworks and licensing markets adapt. This would eliminate the asymmetric extraction by making licensing frictionless and voluntary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_definition_variance,
    'Does ''transformative use'' have coherent semantic boundaries, or does it function as an open-ended legitimacy label that courts apply inconsistently?',
    'Systematic analysis of fair use case law: code holdings by transformativeness reasoning; measure inter-judge agreement on transformation classification for similar fact patterns; identify whether transformation doctrine predicts outcomes or is post-hoc justification',
    'If coherent: the reading''s architecture is functional (Rope from creator perspective, Tangled Rope from copyright holder perspective). If inconsistent: the doctrine is performative (Piton classification), and the reading''s legitimacy claim is degraded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_definition_variance, empirical, 'Whether transformative test has coherent semantic boundaries or functions as post-hoc justification').

omega_variable(
    licensing_market_suppression_extent,
    'To what degree does fair use exemption suppress actual licensing markets that would otherwise function?',
    'Comparative licensing market analysis: sectors/content types where licensing is robust vs. where fair use substitutes; counterfactual analysis of licensing revenue if fair use restrictions were narrowed; study adoption of licensing models in creative industries',
    'If suppression is severe and market-substituting: fair use reading is high-extraction Snare from copyright holder perspective. If suppression is minimal and markets coexist with fair use: reading is lower-extraction Tangled Rope. If licensing markets adapted to fair use rather than being foreclosed: reading is Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(licensing_market_suppression_extent, empirical, 'Extent to which fair use suppresses actual licensing markets').

omega_variable(
    open_licensing_adoption_trajectory,
    'Are open licensing norms (Creative Commons, explicit permission frameworks) actually reducing dependence on fair use litigation, or is fair use litigation increasing in parallel?',
    'Longitudinal data: fair use litigation rates over time; Creative Commons license adoption trends; creator survey on whether open licenses reduce or supplement fair use reliance',
    'If adoption is high and litigation declining: scaffold perspective is structural (real sunset pathway). If adoption is niche and litigation growing: scaffold is aspirational, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_licensing_adoption_trajectory, empirical, 'Whether open licensing adoption is reducing fair use dependence').

omega_variable(
    one_reading_of_contested_kernel,
    'Is this reading (transformative right reading) logically compatible with the narrow_defense_reading and market_licensing_reading, or do the readings foreclose each other within a unified copyright framework?',
    'Jurisprudential analysis: trace how courts have held both this reading and alternatives in succession; identify whether the readings represent different values or different empirical claims about markets/innovation; identify whether one reading''s truth would logically entail the falsity of another',
    'If foreclosing relations exist: the kernel is genuinely contested and cannot be held consistently by a single framework. If coexisting: they are alternative policy choices, not incompatible epistemic claims. Different legal implications for how doctrine can evolve.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(one_reading_of_contested_kernel, conceptual, 'Structural relationships between fair use readings within copyright kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fairuse_transf_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fairuse_transf_tr_t5, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 5, 0.51).
narrative_ontology:measurement(fairuse_transf_tr_t10, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(fairuse_transf_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fairuse_transf_be_t5, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 5, 0.33).
narrative_ontology:measurement(fairuse_transf_be_t10, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 10, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(fairuse_transf_su_t0, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(fairuse_transf_su_t5, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(fairuse_transf_su_t10, fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, identity_coordination).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, copyright_term_extension__incentive_claim).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, digital_millennium_copyright_act__circumvention_exception).

% DUAL FORMULATION NOTE:
% The transformative right reading is one of three structurally distinct interpretations of the fair use kernel. The other readings (narrow_defense_reading, market_licensing_reading) have different ε values, different beneficiary/victim structures, and different classifications from the copyright holder perspective. All three readings affect downstream constraints (copyright term extension, DMCA exceptions) because courts deploying one reading of fair use tend to extend that interpretive framework to related constraints. The network linkage enables analysis of doctrine coherence across fair use and neighboring IP constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
