% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanafi_reading, []).

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
 *   constraint_id: usul_al_fiqh_method__hanafi_reading
 *   human_readable: Hanafi Method: Jurisprudential Authority via Qiyas, Ra'y, and Istihsan
 *   domain: islamic_jurisprudence/commitment_systems/legal_authority
 *
 * SUMMARY:
 *   The Hanafi jurisprudential method, grounded in systematic qiyas (analogy)
 *   and ra'y (juristic opinion), with istihsan (juristic preference to
 *   prevent hardship) as an override mechanism, constitutes one reading of
 *   the broader kernel of usul al-fiqh (Islamic legal methodology). This
 *   reading instantiates a commitment-system constraint that coordinates
 *   Islamic jurisprudence while simultaneously extracting authority and
 *   resources toward a specific constellation of beneficiaries: the merchant
 *   class, administrative officials with interpretive power, and jurists
 *   positioned as authorized reasoners. The constraint exhibits all six DR
 *   types from different perspectives. The Hanafi method is experienced as
 *   pure coordination (Rope) by merchants operating at scale, as flexible
 *   equity (Scaffold-like) by those invoking hardship exceptions, as a mixed
 *   coordination-extraction system (Tangled Rope) by those under
 *   merchant-favorable rules who benefit from occasional istihsan relief, as
 *   performative justification (Piton) when viewed at the civilizational
 *   scale, as constraining authority (Snare) by textualist jurists, and as an
 *   immutable feature of legal necessity (Mountain) by analytical observers.
 *   The constraint's theater ratio has risen over 400 years from 0.40 to
 *   0.55, reflecting increasing elaboration of justificatory apparatus
 *   without proportional change in actual jurisprudential outcomes. The
 *   extractiveness has also risen, from 0.22 to 0.38, indicating that
 *   merchant favoritism has accumulated as the school matured. The
 *   suppression requirement has grown modestly (0.35→0.42), suggesting that
 *   maintaining the istihsan override system requires increasing enforcement
 *   effort and theoretical justification.
 *
 * KEY AGENTS:
 *   - Merchant Class and Trading Networks: Primary beneficiary (institutional/arbitrage) — gains legal instruments for credit, deferred payment, partnerships, commercial innovations unavailable under stricter readings. Can arbitrage between Hanafi and Hanbali zones.
 *   - Administrative Officials and Qadi: Secondary beneficiary (institutional/constrained) — gains flexibility to prevent chaos and serve governance stability through istihsan, but constrained by need to justify deviations and maintain appearance of consistency.
 *   - Textualist Jurists: Primary victim (powerless/trapped) — trapped within the madhhab yet systematically subordinated; their literal Qur'an-hadith reading is overrideable by istihsan at will.
 *   - Non-Merchant Constituencies (peasants, artisans, subsistence farmers): Secondary victim (moderate/constrained) — experience mixed: benefit from occasional hardship relief via istihsan, but framework tilted toward merchant interests; lack voice in interpretation.
 *   - Jurisprudential Authority Structure: Institutional actor (institutional/arbitrage) — distributes authority to qualified jurists (mujtahidun) capable of ra'y and istihsan reasoning; jurists with interpretive capacity benefit enormously.
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent choice of merchant-favorable jurisprudence as inherent to law itself.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanafi_reading, 0.38).
domain_priors:suppression_score(usul_al_fiqh_method__hanafi_reading, 0.42).
domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanafi_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanafi_reading, "Hanafi Method: Jurisprudential Authority via Qiyas, Ra'y, and Istihsan").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanafi_reading, "islamic_jurisprudence/commitment_systems/legal_authority").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanafi_reading, 'c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k').
narrative_ontology:cs_kernel_codification('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', formalized).
narrative_ontology:cs_authority_grounding('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', lineage).
narrative_ontology:cs_interpretation_layer_present('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k').
narrative_ontology:cs_reading_relation('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', usul_al_fiqh_method__hanbali_reading, coexists_with).
narrative_ontology:cs_axiom('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', foundational, istihsan_essential_for_equity).
narrative_ontology:cs_axiom_status(istihsan_essential_for_equity, holdable).
narrative_ontology:cs_axiom_grounding('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', istihsan_essential_for_equity, deontological).
narrative_ontology:cs_axiom('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', foundational, ra_y_qualified_jurists_legitimate).
narrative_ontology:cs_axiom_status(ra_y_qualified_jurists_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', ra_y_qualified_jurists_legitimate, conventional).
narrative_ontology:cs_reference_frame('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', qiyas_ra_y_istihsan_methodology).
narrative_ontology:cs_drift_state('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', contemporary_reform_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c4d7e1f2-8a9b-4c2d-9e5a-1b3d5f7g9h2k', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, merchant_class).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, administrative_officials).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanafi_reading, jurists_with_interpretive_authority).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, textualist_constraint_holders).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, non_merchant_constituencies).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanafi_reading, judicial_consistency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TEXTUALIST JURIST (SNARE) — Trapped within the Hanafi framework yet systemically disadvantaged by the istihsan override. Cannot exit without abandoning juridical legitimacy within the madhhab. The textualist reading is formally subordinated: qiyas establishes the rule, but istihsan can suspend it whenever a jurist perceives hardship. Experiences maximum extraction — loses authority to interpret while bearing the burden of maintaining textual precision.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NON-MERCHANT CONSTITUENCY (TANGLED ROPE) — Benefits from istihsan's hardship prevention in edge cases affecting rural populations, artisans, and subsistence economies. But bears extraction through merchant-favoring commercial jurisprudence. Constrained by lack of juridical voice in interpretation—can petition for hardship relief but cannot shape the framework itself. Mixed experience: some genuine coordination benefit (harm prevention), significant extraction (framework tilted toward merchant interests).
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MERCHANT CLASS (ROPE) — Primary beneficiary. The Hanafi method's permissiveness via ra'y and istihsan creates legal instruments for credit transactions, sales with deferred payment, partnership contracts, and commercial instruments absent or forbidden under stricter readings. Merchants experience the constraint as coordination: the legal framework enables commerce at scale. Arbitrage options abundant—can operate in Hanafi-governed territories and arbitrage against stricter juridical zones. Net positive experience; extraction runs toward this agent.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: ADMINISTRATIVE OFFICIAL/QADI (TANGLED ROPE) — Institutional beneficiary with significant constraint. Qadi benefits from istihsan's flexibility—can adjust harsh textual outcomes to serve governance stability and social peace. But constrained by the need to justify deviations through elaborate ra'y reasoning and to maintain consistency across cases. Active enforcement required: must continually distinguish when istihsan applies vs when qiyas governs. High theater cost in producing juridical justifications for discretionary decisions. Mixed: genuine coordination function (preventing chaos from rigid rules) alongside institutional extraction (qadi's authority expands unchecked).
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: FORMALIST JURISPRUDENTIAL TRADITION (PITON) — Civilization-scale view: the elaborate meta-jurisprudence of Hanafi usul al-fiqh (theory of sources) is largely performative at this scale. The canonical formulas—qiyas defined as analogy with four elements, istihsan as juristic preference for equity—function as scripts that legitimate what are fundamentally political/economic choices about whose interests the law serves. The theatrical apparatus (formal definitions, ranked hierarchies of reasoning types) persists through institutional inertia. Functional degradation: the apparatus does not actually constrain merchant favoritism; it provides the performance through which merchant favoritism is justified as jurisprudential principle rather than interest capture.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / JURISPRUDENTIAL NECESSITY (MOUNTAIN) — From a universal civilizational perspective, some mechanism for adjusting strict textual rules to prevent catastrophic outcomes is inherent to any legal system. The tension between literal rule and equitable exception is immutable—no mature legal tradition can function with zero flexibility. This perspective sees istihsan as an inevitable structural feature of law itself, not a contingent institutional choice. The engine's false summit detector will identify this as naturalization: the structural data reveals that the specific form of istihsan (merchant-favoring, discretionary, theater-laden) is contingent, not the necessity of some flexibility mechanism.
constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanafi_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(usul_al_fiqh_method__hanafi_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(usul_al_fiqh_method__hanafi_reading, TR),
    TR >= 0.70.

:- end_tests(usul_al_fiqh_method__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The Hanafi method creates genuine coordination functions—merchants genuinely can conduct commerce at larger scales because of available legal instruments. But the distribution of benefit is asymmetric: merchants gain liquidity and credit access; non-merchants gain only occasional hardship relief. The 0.38 value reflects that extraction exists (merchant-favorable commercial jurisprudence) but is not total (hardship prevention mechanism does benefit non-merchants). The rise from 0.22 to 0.38 over 400 years indicates that as the school matured and merchant patronage consolidated, commercial jurisprudence became more elaborate and more systematically merchant-favorable. Suppression (0.42): Moderate-high. Barriers to exit include: institutional authority of qualified jurists is non-negotiable within the madhhab; textualist readings are formally subordinated within Hanafi theory; non-merchants lack juridical voice and cannot contest interpretations; those dissatisfied must either adopt a different school (costly, involves abandoning community) or accept the framework. Suppression is not total because istihsan does provide a mechanism to appeal hardship, and because the Hanafi school itself competes with Maliki, Shafi'i, and Hanbali schools (some geographic/sectarian choice exists). Theater ratio (0.55): Moderate-high. The elaborate meta-jurisprudence of usul al-fiqh—canonical definitions of qiyas with four elements, hierarchical ranking of reasoning types, formal criteria for istihsan—functions partly as justification apparatus. The theater has increased over time as the school developed more elaborate reasoning scripts to legitimize what were fundamentally political choices about whose interests the law serves.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. The merchant class sees nearly pure coordination (Rope)—the method solves their collective action problem of how to conduct complex commerce while maintaining Islamic legal legitimacy. The Qadi sees mixed coordination and authority expansion (Tangled Rope)—flexibility enables governance but also expands discretionary power requiring constant justification. The textualist jurist sees pure extraction (Snare)—their literal reading is formally subordinated and overrideable at will by any jurist claiming istihsan. The non-merchant constituency sees tangled coordination and extraction (Tangled Rope)—they benefit from hardship relief but within a framework fundamentally shaped to merchant advantage. The civilizational view risks seeing immutable legal necessity (Mountain)—flexibility is essential to law. The analytical observer's mountain is a false summit: the structural data reveals that the specific form of istihsan (merchant-favoring, discretionary, theater-laden) is historically contingent, not an inevitable feature of jurisprudence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: power level, exit options, and beneficiary/victim status. Merchants with arbitrage options (high mobility, tangible benefit from the system) experience low d and therefore low effective extractiveness. Textualist jurists with trapped options (cannot leave the madhhab without losing legitimacy) and victim status experience high d and maximum chi. Non-merchants with constrained options and mixed beneficiary/victim status experience moderate d. Administrative officials with institutional power and constrained exit experience moderate d but with high negotiating power. The piton perspective derives from theater exceeding function, not from high experienced extraction. The mountain perspective at the analytical level uses the canonical d for analytical contexts (0.73→f(d)≈1.15) but the false summit detector will flag it as naturalization when structural data shows beneficiaries and contingent institutional arrangements.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    istihsan_boundaries_underspecified,
    'What objective criteria distinguish legitimate istihsan (preventing genuine hardship) from illicit tawassu'' (stretching rules to accommodate preference)?',
    'Comparative analysis of Hanafi jurisprudential texts: cataloging istihsan applications and identifying the implicit rule system Hanafi jurists actually use vs formally stated criteria. Cross-checking against later reformist critiques (Shatibi''s maqasid framework, modern jurisprudential theory).',
    'If criteria are robust: istihsan is a principled flexibility mechanism within bounds (Tangled Rope from moderate perspectives). If criteria are fundamentally fuzzy: istihsan is discretionary authority with jurisprudential theater (Snare from textualist perspective, Piton from civilizational perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(istihsan_boundaries_underspecified, empirical, 'Whether istihsan boundaries are formally specified or deliberately indeterminate').

omega_variable(
    merchant_class_dominance_contingency,
    'Is the Hanafi school''s merchant-favorable jurisprudence an inherent consequence of the qiyas-istihsan method, or a historical contingency of the merchant-dominated social coalition that funded and promoted Hanafi jurisprudence?',
    'Historical analysis: comparison of Hanafi jurisprudential development across different economic zones (high-commerce areas vs subsistence economies). Analysis of patronage networks and who funded hadith collection, jurisprudential schools, and legal education. Counterfactual: would the Shafi''i or Maliki schools show different economic profiles if merchant patronage had flowed differently?',
    'If inherent: the merchant favoritism is baked into the istihsan method itself, confirming Snare for textualists. If contingent: the method is neutral and merchant dominance reflects power distribution, making the Hanafi reading structurally similar to sibling readings but historically deployed differently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merchant_class_dominance_contingency, empirical, 'Whether merchant favoritism follows from the method or from historical patronage').

omega_variable(
    kernel_reading_foreclosure_possibility,
    'Does the Hanafi reading''s core commitment to istihsan as essential for preventing hardship logically foreclose the Hanbali reading''s core commitment to textual constraint as essential for preventing drift? Or do both readings coexist as viable frameworks held by different factions?',
    'Formal logical analysis: extracting the foundational axioms of each reading and testing for contradiction within a single commitment framework. Historical analysis of whether Hanbali and Hanafi jurists acknowledged each other''s frameworks as internally coherent vs illegitimate.',
    'If foreclosed: the readings are locked in zero-sum competition; the Hanafi dominance is a matter of institutional power, not logical coherence. If coexist: both readings remain live options; the Hanafi ascendance reflects patronage and political utility, not jurisprudential superiority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_possibility, conceptual, 'Whether Hanafi and Hanbali readings foreclose each other or coexist').

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the necessity of some legal flexibility mechanism (istihsan-like capacity) a genuine immutable property of law (supporting Mountain classification from the analytical perspective), or is it a naturalization of historically contingent institutional arrangements?',
    'Comparative law analysis: systems that attempted to function with zero flexibility (strict statutory regimes, algorithmic law). Analysis of failure modes: did systems lacking istihsan-like mechanisms collapse, or did they evolve functional equivalents? Historical analysis of the analytical observer''s own tradition''s handling of equity (English common law, Roman law, modern statutory interpretation).',
    'If immutable: the mountain classification is justified; istihsan necessity is inherent to law. If contingent: the mountain is a false summit; the Hanafi reading naturalizes what is a constructed institutional choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, conceptual, 'Whether istihsan necessity is immutable law or constructed naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanafi_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_hanafi_tr_t0, usul_al_fiqh_method__hanafi_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(usul_hanafi_tr_t200, usul_al_fiqh_method__hanafi_reading, theater_ratio, 200, 0.48).
narrative_ontology:measurement(usul_hanafi_tr_t400, usul_al_fiqh_method__hanafi_reading, theater_ratio, 400, 0.55).

% Extraction over time
narrative_ontology:measurement(usul_hanafi_be_t0, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(usul_hanafi_be_t200, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 200, 0.31).
narrative_ontology:measurement(usul_hanafi_be_t400, usul_al_fiqh_method__hanafi_reading, base_extractiveness, 400, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(usul_hanafi_su_t0, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(usul_hanafi_su_t200, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 200, 0.39).
narrative_ontology:measurement(usul_hanafi_su_t400, usul_al_fiqh_method__hanafi_reading, suppression_requirement, 400, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanafi_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanafi_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__shafii_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, usul_al_fiqh_method__hanbali_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, hanafi_commercial_jurisprudence__credit_and_sale).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanafi_reading, ijma_consensus_constraint__merchant_class_dominance).

% DUAL FORMULATION NOTE:
% The Hanafi usul al-fiqh method is one constraint family with four structurally distinct readings (Hanafi, Maliki, Shafi'i, Hanbali). Each reading is instantiated in a separate constraint story with different epsilon values, different beneficiary sets, and different perspectival classifications. The Hanafi reading (this story) has ε=0.38 and merchant-favorable extraction. The Hanbali reading constrains extractiveness through textual rigor. The Maliki reading balances merchant interests with public interest (maslaha). The Shafi'i reading attempts to limit both merchant favoritism and juristic discretion. All four readings coexist as live jurisprudential options in contemporary Islamic law. The family is linked via network.affects_constraints to enable contamination analysis: if the Hanafi reading's legitimacy degrades (e.g., through successful reform critiques demonstrating merchant capture), the pressure waves through the entire family as alternative readings become more attractive.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanafi_reading, institutional, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
