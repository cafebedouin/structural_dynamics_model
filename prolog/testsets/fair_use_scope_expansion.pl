% ============================================================================
% CONSTRAINT STORY: fair_use_scope_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_scope_expansion, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fair_use_scope_expansion
 *   human_readable: Fair Use Scope Expansion in Copyright Law
 *   domain: intellectual_property/copyright_law
 *
 * SUMMARY:
 *   Fair use doctrine in copyright law creates a structural tension between
 *   incentivizing original creation and enabling transformative use,
 *   knowledge access, and innovation. The doctrine's scope has expanded
 *   significantly over the past two decades through landmark cases (Campbell
 *   v. Acuff-Rose, Google Books, Sony Music v. Titus) and the emergence of
 *   AI-assisted creation, but this expansion has benefited institutional
 *   actors (large publishers, educational institutions, tech platforms) far
 *   more than independent creators and small publishers. The constraint
 *   exhibits classical tangled-rope structure: a genuine coordination
 *   function (enabling secondary markets, remix culture, research, education)
 *   layered atop asymmetric extraction (powerful institutions capture value
 *   from expansive fair use interpretation while small creators lose
 *   licensing control). The theater ratio (0.55) reflects that fair use
 *   enforcement largely consists of cease-and-desist letters and settlement
 *   threats rather than actual litigation—parties invoke fair use doctrine
 *   strategically, but predictability has degraded as transformativeness
 *   doctrine has become increasingly flexible.
 *
 * KEY AGENTS:
 *   - Independent Creators: Primary victims (powerless/trapped) — photographers, musicians, writers who cannot afford litigation and lose licensing control to fair use assertions
 *   - Small Publishers: Secondary victims (moderate/constrained) — face high litigation costs and licensing revenue loss but retain some negotiating capacity
 *   - Large Copyright Holders / Media Conglomerates: Primary beneficiaries (institutional/arbitrage) — benefit from licensing market while asserting fair use for their own derivative uses
 *   - Educational Institutions: Secondary beneficiaries (institutional/arbitrage) — university and school use of copyrighted material for teaching/research benefits from broad fair use
 *   - AI Training Companies / Transformative Creators: Mixed agents (powerful/mobile) — benefit from broad fair use for training data but vulnerable to legislative backlash; also extract value from original creators
 *   - Fair Use Reform Coalition: Organized advocates (organized/constrained) — EFF, internet rights groups seeking statutory clarification and policy reform
 *   - Courts Applying Doctrine: Institutional enforcers (institutional/arbitrage) — apply four-factor test inconsistently; performative ritual maintains precedent structure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional power asymmetries as inherent to information economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_scope_expansion, 0.58).
domain_priors:suppression_score(fair_use_scope_expansion, 0.62).
domain_priors:theater_ratio(fair_use_scope_expansion, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_scope_expansion, extractiveness, 0.58).
narrative_ontology:constraint_metric(fair_use_scope_expansion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fair_use_scope_expansion, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_scope_expansion, tangled_rope).
narrative_ontology:human_readable(fair_use_scope_expansion, "Fair Use Scope Expansion in Copyright Law").
narrative_ontology:topic_domain(fair_use_scope_expansion, "intellectual_property/copyright_law").

domain_priors:requires_active_enforcement(fair_use_scope_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_scope_expansion, original_copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_scope_expansion, educational_institutions).
narrative_ontology:constraint_beneficiary(fair_use_scope_expansion, research_organizations).
narrative_ontology:constraint_beneficiary(fair_use_scope_expansion, transformative_creators).
narrative_ontology:constraint_victim(fair_use_scope_expansion, independent_creators).
narrative_ontology:constraint_victim(fair_use_scope_expansion, small_publishers).
narrative_ontology:constraint_victim(fair_use_scope_expansion, licensing_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CREATOR (SNARE) — Small-scale creators (photographers, musicians, writers) cannot exit the fair use regime. They face uncertainty about whether derivative uses of their work constitute fair use, cannot afford litigation to establish boundaries, and bear the extraction cost of unpredictable licensing loss. Zero degrees of freedom — trapped in an asymmetric legal regime that extracts their control without compensation.
constraint_indexing:constraint_classification(fair_use_scope_expansion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL PUBLISHER (TANGLED ROPE) — Faces high cost to challenge fair use assertions and loses licensing revenue, but also coordinates with distribution networks and gains from transformative content ecosystem. Suppression is real (litigation expense, market uncertainty) but exit cost, while high, is not insurmountable. Genuine hybrid: both extraction and coordination.
constraint_indexing:constraint_classification(fair_use_scope_expansion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE COPYRIGHT HOLDERS (ROPE) — Institutional beneficiaries (major studios, publishers, tech platforms) experience fair use scope expansion as coordination: the doctrine enables secondary markets (licensing, derivative adaptation) while their own use of copyrighted material falls under institutional fair use claims. Arbitrage exit: they can license or assert fair use as strategically advantageous. Net beneficiary position.
constraint_indexing:constraint_classification(fair_use_scope_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EDUCATIONAL INSTITUTIONS (ROPE) — Universities and schools benefit substantially from expansive fair use doctrine (classroom copying, course materials, research use). They have arbitrage exit: can negotiate licenses or rely on fair use depending on cost-benefit. Their organizational position enables them to influence fair use doctrine through amicus briefs and policy channels. Net coordination benefit.
constraint_indexing:constraint_classification(fair_use_scope_expansion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: TRANSFORMATIVE CREATORS / AI TRAINING (TANGLED ROPE) — Tech platforms and AI companies use fair use doctrine expansively (dataset scraping, training data incorporation, generative outputs). They have structural power and can afford litigation but are vulnerable to legislative backlash. They both benefit from expansive fair use AND extract value from original creators whose work trains their systems. Mixed: genuine coordination function (enabling innovation) with asymmetric extraction (uncompensated use of original work).
constraint_indexing:constraint_classification(fair_use_scope_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FAIR USE REFORM COALITION (SCAFFOLD) — Organized advocates (EFF, internet rights groups, academic consortia) see the current regime as temporary and subject to revision through legislation and jurisprudential development. They have agency through collective action and identify concrete exit pathways (statutory carve-outs for research/education, compulsory licensing, clarity via Congressional codification). Sunset logic: current uncertainty is being addressed through emerging legal frameworks. Suppression is tolerated because the coalition perceives a near-term solution.
constraint_indexing:constraint_classification(fair_use_scope_expansion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: COPYRIGHT DOCTRINE (PITON) — Fair use jurisprudence has become substantially performative. Courts apply four-factor tests (purpose, nature, amount, market effect) that yield unpredictable results. Parties cite precedent strategically rather than applying predictive doctrine. The theater ratio is high (0.55) because enforcement consists largely of cease-and-desist letters and settlement threats, not actual litigation. The doctrine persists through institutional inertia: all legal players depend on the precedent structure even though its predictive power has degraded.
constraint_indexing:constraint_classification(fair_use_scope_expansion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the tension between copyright incentive and knowledge access is inherent to information goods: you cannot simultaneously maximize creator compensation and maximize access. This perspective naturalizes the scope expansion as a permanent equilibrium problem. However, the structural data contradicts this: the scope expansion is driven by power asymmetries (large institutions benefit from broad fair use; small creators bear costs), not by invariant information economics. The engine detects this as a false summit.
constraint_indexing:constraint_classification(fair_use_scope_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_scope_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fair_use_scope_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fair_use_scope_expansion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_scope_expansion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fair_use_scope_expansion, TR),
    TR >= 0.70.

:- end_tests(fair_use_scope_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Fair use scope expansion creates measurable value asymmetry — institutional actors (platforms, publishers, educational institutions) benefit from broad interpretation; independent creators lose licensing control. The extraction is not total (creators can still negotiate, smaller uses are clearly permitted, the doctrine has some predictive power) but has grown measurably over the 20-year interval. Suppression (0.62): High. Barriers to independent action are substantial: litigation costs make contesting fair use claims prohibitive (average copyright litigation exceeds $300k); uncertainty about which uses will be deemed fair use chills licensing negotiations; market power concentration means small creators cannot exert countervailing pressure. Theater ratio (0.55): Moderate. Courts apply four-factor transformativeness test, but outcomes are unpredictable, suggesting the doctrine has become substantially performative. Cease-and-desist letters invoke doctrine rhetorically without genuine legal prediction; settlement patterns reflect power asymmetries more than doctrinal clarity. The theater has increased over the interval as transformativeness became more elastic (compare Sony v. Universal in 1984—fairly clear rule against commercial copying—to Campbell v. Acuff-Rose in 1994 allowing commercial parody—to Google Books and beyond, where near-total copying can be fair use if transformative). Claimed type: Tangled Rope. The constraint coordinates genuine functions (enabling research, education, secondary markets, remix culture) while asymmetrically extracting from small creators who lack litigation resources and institutional support.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Independent creators experience snare-level extraction (doctrine limits their control, litigation is prohibitively expensive, they cannot exit). Educational institutions experience rope-level coordination (broad fair use enables teaching/research mission; they have institutional power to negotiate or rely on fair use as beneficial). Large publishers experience rope-level coordination (they assert fair use when advantageous while licensing to others). The pivotal gap: both independent creators and large publishers are technically subject to the same doctrine, but its application differs radically due to power asymmetry. An independent photographer cannot effectively assert fair use against a large platform; a large media company can assert it strategically or negotiate from strength. The reform coalition sees this as temporary (scaffold perspective—legislative solution is feasible). The copyright doctrine itself appears as a degraded ritual (piton—courts apply tests that yield unpredictable results; parties invoke doctrine more as negotiating tactic than predictive law). The naïve analytical observer risks seeing fair use as an eternal equilibrium problem in information economics (mountain—the tension between incentives and access is built-in to knowledge goods), naturalizing what is actually a contingent institutional arrangement shaped by lobbying, litigation costs, and power concentration.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value is computed from structural position in the extraction flow: beneficiaries with arbitrage exit (large copyright holders, educational institutions) receive low d → negative chi → rope classification. Victims without exit (independent creators) receive high d → high chi → snare classification. Mixed agents with some power and constrained exit (small publishers, AI companies) receive moderate d → moderate chi → tangled_rope classification. Organized advocates with collective agency and perceived exit path (reform coalition) receive moderate d with low chi due to scaffold framing (sunset clause, agency). The institutional copyright doctrine itself receives high d (positioned as the enforcement mechanism) with high theater_ratio → piton classification (degraded ritual). The analytical observer receives the 'observer' d value which typically yields rope-or-mountain depending on how natural law is framed; in this case the engine detects false summit because the structural data contradicts the naturalized framing.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is not fully resolved in this constraint—the false summit risk (mountain classification at analytical level contradicted by structural data) is genuine. The constraint resolves as tangled_rope at the consensus level (genuine coordination with asymmetric extraction), but the mandate to classify it as a 'natural equilibrium in information economics' (mountain) collides with evidence that the extraction is institutional (power-driven, not law-driven). The omega variables around transformativeness definition ambiguity and market harm measurement are central: if fair use doctrine were actually predictive and objective, it would rank lower in theater and might approach rope-only classification. But the perspectival gap (independent creators see snare; institutions see rope) suggests the doctrine is doing institutional work (legitimizing unequal power) rather than legal work (predicting outcomes). The 'natural law' reading is the mandate that courts (and policymakers) invoke to avoid redistribution questions: 'Fair use scope is determined by the inherent nature of copyright incentives,' not 'Fair use scope is the outcome of relative institutional power and litigation costs.' The engine's false summit detection is diagnostically appropriate here—the analysis should flag that the mountain framing conceals institutional extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_definition_ambiguity,
    'What degree of transformation makes a use ''transformative'' under fair use doctrine?',
    'Case law corpus analysis; comparison of judicial decisions (Campbell v. Acuff-Rose, Google Books, Andy Warhol Foundation) to identify whether ''transformativeness'' has consistent operative definition or functions as a rhetorical term that conceals power asymmetries',
    'If consistent definition exists: doctrine has predictive power (piton classification is wrong; should be rope). If definition is context-dependent or power-influenced: transformativeness functions as a legalized extraction cover story, and the snare classification for powerless creators is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(transformativeness_definition_ambiguity, empirical, 'Whether transformativeness has consistent operative definition in fair use doctrine').

omega_variable(
    market_harm_measurement_framework,
    'Does the ''market harm'' factor in the four-factor test actually measure harm to the licensing market, or does it implicitly favor incumbent copyright holders?',
    'Analysis of how courts assess market harm in cases involving: educational use (does harm to textbook sales count?), derivative adaptation (does transformation eliminate harm?), AI training (does generative output compete with original?). Cross-reference with economic impact studies.',
    'If market harm is objectively measured: suppression reflects real market asymmetry (legitimate extraction). If courts weight incumbent licensing markets more heavily than emerging use cases: fair use scope expansion is genuine but covers institutional extraction, not liberalization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_measurement_framework, empirical, 'Measurement of market harm in fair use factor analysis').

omega_variable(
    ai_training_doctrinal_stability,
    'Will AI training data use remain under fair use, or will courts and legislatures restrict this via statutory amendment or new case law?',
    'Legislative tracking (Section 1201 amendments, pending copyright reform bills); judicial forecasting (outcomes in pending AI copyright cases: Getty v. Stability AI, New York Times v. OpenAI, Authors Guild v. Google continuation); industry licensing trends (whether AI companies move toward licensed training datasets)',
    'If AI training stays under fair use: scope expansion continues (tangled_rope classification holds). If legislated out: scope contracts, and the scaffold perspective is validated—temporary regime under revision. If courts narrow transformativeness for AI: the piton classification is revealed as accurate—doctrine becomes more performative as courts apply doctrinal tests inconsistently.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_training_doctrinal_stability, empirical, 'Future stability of AI training data use under fair use doctrine').

omega_variable(
    licensing_market_counterfactual,
    'Would independent creators and small publishers have higher revenue if fair use scope were narrower, or is fair use expansion revenue-neutral for them (extraction is driven by institutional power, not doctrinal scope)?',
    'Econometric analysis comparing creator revenue across jurisdictions with different fair use doctrine (US vs EU Copyright Directive). Licensing data from rights-clearance intermediaries. Surveys of independent creators on licensing demand elasticity.',
    'If narrower fair use increases independent creator revenue: the extraction is real and doctrinal. If revenue change is minimal: extraction is institutional (large platforms and publishers benefit; small creators'' revenue depends on platform algorithms and market power, not doctrine). If revenue decreases: fair use expansion enables secondary markets that compensate creators indirectly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(licensing_market_counterfactual, empirical, 'Revenue impact of fair use scope on independent creators').

omega_variable(
    statutory_clarity_feasibility,
    'Can Congress codify fair use doctrine with greater clarity without implicitly narrowing it or making it subject to lobbying capture by incumbent copyright holders?',
    'Analysis of proposed legislation (PRO Act, CREATE Act, etc.) against fair use jurisprudence; assessment of whether statutory language narrows scope or clarifies it. Public comment records and legislative history to identify lobbying influence.',
    'If statutory clarity is feasible: scaffold perspective is vindicated—exit path via legislation is real. If statute becomes lobbied to benefit incumbents: fairness regime is more extractive post-codification than current doctrine. If Congress cannot achieve agreement: piton classification is validated—regime persists through inertia because alternatives are politically impossible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(statutory_clarity_feasibility, conceptual, 'Feasibility of statutory fair use clarification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_scope_expansion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fairuse_tr_t0, fair_use_scope_expansion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fairuse_tr_t10, fair_use_scope_expansion, theater_ratio, 10, 0.48).
narrative_ontology:measurement(fairuse_tr_t20, fair_use_scope_expansion, theater_ratio, 20, 0.55).
narrative_ontology:measurement(fairuse_tr_t5, fair_use_scope_expansion, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(fairuse_be_t0, fair_use_scope_expansion, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fairuse_be_t10, fair_use_scope_expansion, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(fairuse_be_t20, fair_use_scope_expansion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(fairuse_be_t5, fair_use_scope_expansion, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_scope_expansion, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_scope_expansion, copyright_licensing_market_efficiency).
narrative_ontology:affects_constraint(fair_use_scope_expansion, platform_content_moderation_liability).
narrative_ontology:affects_constraint(fair_use_scope_expansion, ai_training_data_acquisition).

% DUAL FORMULATION NOTE:
% Fair use scope expansion can be decomposed into two structurally distinct constraints: (1) fair use doctrine as general coordination mechanism for knowledge access and secondary markets (low ε, rope-predominant), and (2) fair use doctrine as institutional extraction mechanism benefiting powerful actors over independent creators (high ε, snare-predominant for powerless). This story integrates both via tangled_rope classification, but alternative analysis could decompose into family of related constraints with different ε values measuring doctrine's coordination vs extractive functions separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fair_use_scope_expansion, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
