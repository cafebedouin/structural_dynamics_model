% ============================================================================
% CONSTRAINT STORY: egypt_three_millennia_persistence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_egypt_three_millennia_persistence, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: egypt_three_millennia_persistence
 *   human_readable: Pharaonic System's Three-Millennia Persistence (3100-30 BCE)
 *   domain: ancient_religion/institutional_longevity
 *
 * SUMMARY:
 *   The pharaonic system persisted as a governing and cosmological framework
 *   for approximately 3,070 years (3100 BCE to 30 BCE), encompassing roughly
 *   30-31 dynasties, multiple intermediate periods of state fragmentation,
 *   foreign invasions (Hyksos 1630-1523 BCE, Persian 525-404 BCE, renewed
 *   341-332 BCE, Ptolemaic Macedonian 305-30 BCE), major theological
 *   innovations (Atenism, Amun ascendancy, syncretism), and significant
 *   administrative restructuring. This longevity exceeded the Spartan
 *   system's roughly 3-century persistence (founded ca. 900 BCE, functional
 *   collapse ca. 371 BCE at Leuctra and final authority loss ca. 195 BCE),
 *   demonstrating that the anchored-kernel institutional model produces
 *   different outcome lifespan depending on the depth of
 *   theological-interpretive accretion beneath the kernel. The pharaonic
 *   system exhibits mid-range flexibility: enough to absorb foreign rule and
 *   theological reframing without losing the Ma'at-Amun cosmological core,
 *   but insufficient to achieve the near-total interpretive pluralism of the
 *   Hindu system (4000+ years through simultaneous sectarian diversity and
 *   Vedic kernel). The constraint serves as the comparative case supporting
 *   the composability thesis: the axis profile (anchored religious kernel,
 *   traditional authority, low formal bandwidth for law-making) appears
 *   insufficient to explain 3000-year persistence; instead, persistence
 *   correlates with institutional depth (capacity to reinterpret and
 *   integrate innovations while preserving core legitimacy). The system
 *   functioned as a tangled-rope institution for 2,900 years
 *   (elite-priesthood coordination alongside peasant extraction), degraded
 *   into Piton status during late Ptolemaic and Roman periods (ritual theater
 *   without functional authority), and underwent functional termination in 30
 *   BCE when Roman conquest integrated Egypt into imperial administration and
 *   emperors adopted only token pharaonic titles.
 *
 * KEY AGENTS:
 *   - Pharaonic Elite (Dynastic Rulers): Primary beneficiary (institutional/arbitrage) — captures legitimacy, resource control, succession stability through Ma'at framework; able to reinterpret theology while maintaining authority
 *   - Priesthood (Temple Institutions): Primary beneficiary & enforcer (institutional/arbitrage) — maintains religious apparatus, justifies extraction, coordinates with elite through mutual legitimacy reinforcement; controls interpretive reinterpretation capacity
 *   - Peasant Labor Base: Primary victim (powerless/trapped) — bears corvée obligations, agricultural surplus extraction, temple service; geographic immobility and legal status create permanent entrenchment
 *   - Subject Peoples & Vassal States: Secondary victim (moderate/constrained) — navigating extraction through tribute and conscription; constrained but able to negotiate within framework during periods of central weakness
 *   - Theological Reform Movements: Organized actors (organized/constrained) — Atenism, regional priestly schools, Amun syncretism — attempting to modify system through reinterpretation; succeed when framed within Ma'at but fail if rejected as heterodoxy
 *   - Foreign Rulers (Hyksos, Persians, Ptolemies): Institutional adapters (institutional/mobile) — initially integrated into pharaonic framework through adoption of pharaonic titles and Ma'at language; structural integration capacity degraded with Roman rule
 *   - Roman Imperial Authority: Institutional successor (institutional/arbitrage) — terminated functional pharaonic authority by refusing full integration into Ma'at framework; reduced pharaonic legitimacy to ceremonial residue within imperial administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(egypt_three_millennia_persistence, 0.38).
domain_priors:suppression_score(egypt_three_millennia_persistence, 0.52).
domain_priors:theater_ratio(egypt_three_millennia_persistence, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(egypt_three_millennia_persistence, extractiveness, 0.38).
narrative_ontology:constraint_metric(egypt_three_millennia_persistence, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(egypt_three_millennia_persistence, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(egypt_three_millennia_persistence, accessibility_collapse, 0.0).
narrative_ontology:constraint_metric(egypt_three_millennia_persistence, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(egypt_three_millennia_persistence, tangled_rope).
narrative_ontology:human_readable(egypt_three_millennia_persistence, "Pharaonic System's Three-Millennia Persistence (3100-30 BCE)").
narrative_ontology:topic_domain(egypt_three_millennia_persistence, "ancient_religion/institutional_longevity").

domain_priors:requires_active_enforcement(egypt_three_millennia_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(egypt_three_millennia_persistence, pharaonic_elite).
narrative_ontology:constraint_beneficiary(egypt_three_millennia_persistence, priestly_class).
narrative_ontology:constraint_victim(egypt_three_millennia_persistence, peasant_labor_base).
narrative_ontology:constraint_victim(egypt_three_millennia_persistence, subject_peoples).
narrative_ontology:constraint_victim(egypt_three_millennia_persistence, theological_heterodoxy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEASANT LABOR BASE (SNARE) — Trapped across generations in corvée obligations, agricultural surplus extraction, and temple service. Suppression is structural and total: geographic immobility (Nile valley dependency), legal status (permanent serf-like binding), religious justification (Ma'at divine order). No exit mechanism exists within the system; exit costs are civilizational displacement. Maximum experienced extractiveness — the constraint extracts labor, surplus, and identity.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SUBJECT PEOPLES & VASSAL STATES (TANGLED ROPE) — Constrained by military capacity and resource dependency on Egypt but also coordinating through the pharaonic legitimacy framework. Foreign invasions (Hyksos, Persians) and integrations (Ptolemaic Hellenization) show that subject peoples can negotiate within the system. Benefits include trade access, legal protection, and participation in shared cosmological order; costs include tribute, conscription, and religious subordination. Mixed extraction and coordination — the system genuinely solves the collective action problem of coordinating multiethnic states but does so via asymmetric extraction from conquered populations.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PHARAONIC ELITE & PRIESTHOOD (ROPE) — Experiences the constraint as genuine coordination mechanism. The Ma'at framework enables continuity of governance, legitimacy transmission across dynasties, and durable allocation of resources and authority. Elite actors benefit from the system and see it as solving the problem of succession stability and inter-institutional coordination (pharaoh-priesthood). Arbitrage exit: elite can reframe themselves within Ma'at doctrine (Akhenaten's theological innovation, Ptolemaic Hellenization) without breaking the system. Net beneficiary with interpretive flexibility.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: REFORM MOVEMENTS & THEOLOGICAL INNOVATION (SCAFFOLD) — Organized agents (Atenism, Amun priesthood, regional theology shifts) perceive the constraint as temporary institutional problem solvable through controlled reinterpretation. Each major theological reform (Middle Kingdom shift to democratized afterlife, New Kingdom Aten deviation, Third Intermediate Period priestly dominance) represents an attempt to modify the system while preserving its core. Theater ratio shows these reforms are partly performative (sustaining legitimacy narrative) but also partly functional (redistributing power). Constrained because reforms must work within Ma'at framework to succeed, but agency exists in theological reframing.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-PTOLEMAIC REMNANTS (PITON) — After 30 BCE Roman conquest, the pharaonic system persists in vestigial form: temple priesthoods continue ritual performance, demotic scribal traditions maintain formal continuity, but functional governance authority has transferred to Greco-Roman administration. The theatrical maintenance (temple building, ritual cycle, administrative titles) persists without the coordination function that made the system functionally necessary. Mobile exit: Roman emperors can abandon pharaonic legitimacy claims after internal consolidation (Christianity's rise reduces ritual value further). Piton classification reflects degraded functionality maintained through institutional inertia and cultural continuity rather than active enforcement.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — The pharaonic system appears to civilizational analysis as a natural feature of Egyptian civilization, an immutable product of geography (Nile flooding cycle), theology (inherent cosmological order), and cultural DNA. The 3000-year persistence suggests the system is a law of nature rather than contingent institutional arrangement. However, the structural data contradicts this: identified beneficiaries (pharaonic elite, priesthood), measurable extraction (corvée, surplus, suppressed heterodoxy), and finite lifespan (30 BCE) all indicate a constructed constraint falsely naturalized. Engine's false summit detector will reclassify as tangled rope, revealing that 'eternal Egypt' is a narrativization of extractive institutional persistence, not a natural law.
constraint_indexing:constraint_classification(egypt_three_millennia_persistence, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(egypt_three_millennia_persistence_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(egypt_three_millennia_persistence, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(egypt_three_millennia_persistence, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(egypt_three_millennia_persistence, TR),
    TR >= 0.70.

:- end_tests(egypt_three_millennia_persistence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate-high. The pharaonic system continuously extracted peasant labor (corvée duty), agricultural surplus (taxes), and religious obligation (temple service) across all three millennia. The rate appears stable (~0.32-0.41 across the interval, mean 0.38) rather than accumulating, suggesting the system reached an equilibrium extraction level that the peasant base could sustain without triggering systemic rebellion. This is consistent with the constraint being functionally stable rather than predatory. The extraction is substantial (peasants bear real costs) but not maximal (the system had capacity for higher extraction without economic collapse, as evidenced by Roman administrators' ability to extract higher taxes after conquest). Suppression (0.52): Moderate-high. Suppression is structural and comprehensive: (1) geographic immobility — peasants tied to Nile valley dependencies and village-level settlement patterns; (2) legal status — permanent serf-like obligation to state and temple; (3) religious justification — Ma'at theology frames extraction as natural cosmic order; (4) organizational barriers — peasant organization explicitly prevented by administrative structure (no intermediaries between village and state); (5) exit suppression — departure from system triggers legal punishment, religious condemnation, and loss of subsistence access. Suppression is not total (some peasants could flee to Nubia or desert margins, some local craft producers could accumulate minor autonomy) but high enough to make exit structurally costly. Theater ratio (0.45 → 0.68): Rising across the interval. Early pharaonic period (Old Kingdom) shows relatively low theater because the system's core function (coordinating monumental construction, managing flood-dependent agriculture, maintaining elite succession) is actively performed. Theater ratio rises across the interval, suggesting degrading functionality: Middle Kingdom shows increased administrative theater (bureaucratic elaboration without proportional output increase); New Kingdom imperial administration adds religious ceremony load; Third Intermediate Period shows sharp theater increase as priestly authority concentrates and pharaonic administrative capacity declines; Late Period and Ptolemaic era show theater ratio approaching 0.68 as the system becomes increasingly performative. By Roman period, the constraint becomes near-pure Piton (theater ≥ 0.70) as pharaonic ritual persists without functional governance authority.
 *
 * PERSPECTIVAL GAP:
 *   The constraint's six-perspective range from Snare to Mountain to Rope reveals how the same institutional structure generates opposite experienced classification depending on structural position. The peasant sees immutable, unchangeable extraction (Snare at trapped horizon = Mountain-like perceived immutability). The elite see a manageable coordination problem (Rope, with arbitrage flexibility to innovate within bounds). The reformers see a temporary institutional friction point (Scaffold, with agency to reframe). The piton observer sees degraded functionality sustained by ritual (Piton, theater > 0.70). The analytical observer risks naturalizing the whole thing (Mountain — 'eternal Egypt'). This perspectival fan across a single constraint demonstrates why indexical classification is necessary: the question 'Is the pharaonic system a Snare or a Rope?' has no single answer. It is a Snare from the trapped peasant's perspective, a Rope from the elite's perspective, a Scaffold from the reformer's perspective, and a Mountain (false summit) from the analyst's uncritical perspective. The constraint's three-millennia persistence reflects that the elite's Rope experience (genuine coordination value, arbitrage flexibility) enabled them to continuously renew commitment to the system, while the peasant's Snare experience (entrenchment, high suppression) prevented exit-driven collapse. When the elite's arbitrage opportunity shifted to Roman integration (higher status in imperial hierarchy), their commitment dissolved within two centuries, revealing that the 3000-year persistence was contingent on continuous elite choice (Rope classification justifies staying) rather than immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation for each perspective proceeds from beneficiary/victim status and exit_options. Pharaonic Elite: declared beneficiary + arbitrage exit → d ≈ 0.10-0.20 → f(d) ≈ -0.05 to 0.05 → near-zero or negative χ (beneficiary experiences minimal or negative extraction, which is correct — the system benefits the elite). Peasant Base: declared victim + trapped exit → d ≈ 0.93 → f(d) ≈ 1.38 → high χ (victim with no exit bears maximum extraction, which is structurally accurate). Subject Peoples: declared victim + constrained exit → d ≈ 0.62 → f(d) ≈ 0.95 → moderate-high χ (victims with high-cost exit experience significant but not maximal extraction, reflecting negotiation capacity within the framework). Theological Reformers: mixed beneficiary-victim (beneficiary if innovation succeeds, victim if suppressed) + constrained exit → d ≈ 0.45-0.55 (bifurcated) → f(d) ≈ 0.55-0.75 → mixed χ reflecting contingency on innovation success. Late-Period Priesthood transitioning to Piton: nominal beneficiary + degraded arbitrage (ritual maintenance value declining) → d rising over time (~0.20 → 0.45) as functional coordination capacity declines and theater dominates. The derivation chain fully explains why each perspective produces its classification without overrides — the beneficiary/victim declarations and exit_options contain all necessary information.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the same institutional structure exhibits different coordination-extraction balances from different perspectives, and that these balances explain the constraint's actual historical trajectory. The system was neither pure coordination (Rope) nor pure extraction (Snare) but a functional hybrid (Tangled Rope) from the elite and moderate agent perspectives, with genuine coordination benefits (elite continuity, multiethnic state stability) paired with asymmetric extraction (peasant entrenchment). The three-millennia persistence reflects that the tangled-rope structure proved stable because: (1) elite benefited enough to renew commitment (coordination function was real), (2) peasants could not exit (suppression was high), (3) periodic theological reinterpretation prevented legitimacy collapse (interpretive accretion depth enabled institutional flexibility). The system collapsed functional authority in 30 BCE not because extraction became insupportable but because the elite's arbitrage opportunity shifted: integration into Roman imperial hierarchy offered higher status and security with lower coordination cost (Rome provided centralized enforcement that Egyptian state had to provide for itself). This was rational elite choice, demonstrating that Rope classification (elite beneficiary perspective) correctly identified that the system was sustainable only so long as elite perceived net coordination benefit. The Snare perspective (peasant victim) was always accurate about peasant structural entrenchment, but peasant structural position did not determine system outcome — elite choice did. Mandatrophy is resolved by the comparative case logic: Spartan system (more rigid kernel, less interpretive accretion depth) collapsed in 3 centuries; pharaonic system (mid-range rigidity, moderate interpretive accretion) persisted 3000 years; Hindu system (flexible kernel, nearly total interpretive accretion) persisted 4000+ years and remains institutionally continuous. The variable enabling different longevity is interpretive-accretion depth, demonstrating that the composability thesis is correct — institutional persistence is multiply-realizable through different axis profiles combined with different accretion depths. The Piton observation (late-period degradation, theater rising above 0.65) is consistent with the historical record and shows that the system remained functional as long as elite commitment justified the coordination cost; when commitment shifted, the Piton classification correctly identified that the system was already operationally terminal. The false-summit observation (mountain classification is naturalization, not law) is validated by the system's actual termination — natural laws do not terminate when political elites change their preferences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_accretion_depth,
    'What depth of theological-interpretive reinterpretation capacity explains the pharaonic system''s three-millennia persistence versus the Spartan system''s three-century collapse or the Hindu system''s 4000+ year durability?',
    'Comparative analysis of theological flexibility: count successful major reinterpretations without system collapse (Aten deviation, Amun syncretism, Ptolemaic Hellenization) vs failed innovations that triggered legitimacy crisis. Measure against Spartan rigidity (Lycurgan code resistance to reinterpretation) and Hindu adaptive theology (Vedic to Puranic, sect pluralism).',
    'If accretion depth is determinative: pharaonic persistence is explained by mid-range flexibility — enough to absorb foreign rule and theological innovation, but not so much that the kernel lost coherence (unlike Hindu system''s near-total theological pluralism). If depth is not determinative: the three-millennia span is contingent on other factors (geographic isolation, absence of equivalent civilizational competitors, resource abundance).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_accretion_depth, conceptual, 'Depth of interpretive theological accretion enabling system persistence').

omega_variable(
    extraction_sustainability_threshold,
    'What extraction rate (measured as peasant labor duty, surplus tax, and religious obligation burden) can the peasant base sustain across generations before triggering systemic rebellion that breaks the pharaonic framework?',
    'Historical analysis of extraction burden (estimated from temple tax records, labor corvée registers, administrative papyri) correlated with recorded unrest frequency (tomb robbery, labor strike narratives, regional defection). Compare against documented population stress markers (nutritional archaeology, settlement patterns during Intermediate Periods).',
    'If extraction was at sustainability ceiling: the system''s three-millennia longevity reflects operating at the boundary of what peasant base can bear — any increase would trigger collapse. If extraction was below ceiling: the system had unused coercive capacity and persisted due to factors beyond extraction tolerance (legitimacy, coordination benefits, absence of viable alternatives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_sustainability_threshold, empirical, 'Maximum sustainable extraction rate for peasant labor base').

omega_variable(
    foreign_rule_integration_mechanism,
    'How did foreign rule (Hyksos, Persian, Ptolemaic conquest) integrate into the pharaonic system without breaking it, whereas Roman conquest appears to have terminated the system''s functional authority within two centuries?',
    'Comparative institutional analysis: measure continuity of administrative structures, religious legitimacy claims, and elite coalition formation under each foreign regime. Identify the threshold where integration capacity failed (Roman Christian theology vs Ma''at incompatibility, centralized imperial authority vs pharaonic decentralization, emperor-worship vs pharaonic theocracy).',
    'If integration capacity is the key variable: the system''s persistence depended on foreign rulers'' willingness to adopt pharaonic legitimacy claims and preserve priestly-elite coalition. Roman refusal to fully integrate into Ma''at framework triggered functional termination. If other variables dominate: the system was already degraded (Piton perspective) by Late Period, and Roman conquest simply accelerated already-terminal decline.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_rule_integration_mechanism, empirical, 'Foreign rule integration capacity and Roman conquest termination').

omega_variable(
    false_summit_naturalization,
    'Is the three-millennia persistence a natural feature of Egyptian civilization or a contingent institutional arrangement sustained through coordinated elite action and suppressed alternatives?',
    'Epistemological cross-position analysis: compare how the constraint appears from powerless (trapped in system), elite (coordinating through system), and analytical (seeing system as natural) perspectives. Presence of identified beneficiaries and measurable extraction indicates false summit. Absence of documented alternative frameworks (other available governance models) vs absence of technological capacity to imagine alternatives is diagnostic.',
    'If mountain classification is correct: the pharaonic system is an immutable feature of Egyptian civilization, and its collapse in 30 BCE requires explanation of sudden geological/climatic shift. If tangled rope classification is correct (false summit): the system was sustained through elite coordination and peasant suppression, explaining both its longevity (benefits to elite) and its termination (elite''s ability to exit into Roman administration).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalization, conceptual, 'Natural law vs contingent institutional arrangement (false summit detection)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(egypt_three_millennia_persistence, 0, 3070).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(egypt_tr_t0, egypt_three_millennia_persistence, theater_ratio, 0, 0.45).
narrative_ontology:measurement(egypt_tr_t750, egypt_three_millennia_persistence, theater_ratio, 750, 0.52).
narrative_ontology:measurement(egypt_tr_t1500, egypt_three_millennia_persistence, theater_ratio, 1500, 0.58).
narrative_ontology:measurement(egypt_tr_t2250, egypt_three_millennia_persistence, theater_ratio, 2250, 0.65).
narrative_ontology:measurement(egypt_tr_t3000, egypt_three_millennia_persistence, theater_ratio, 3000, 0.68).

% Extraction over time
narrative_ontology:measurement(egypt_be_t0, egypt_three_millennia_persistence, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(egypt_be_t750, egypt_three_millennia_persistence, base_extractiveness, 750, 0.38).
narrative_ontology:measurement(egypt_be_t1500, egypt_three_millennia_persistence, base_extractiveness, 1500, 0.41).
narrative_ontology:measurement(egypt_be_t2250, egypt_three_millennia_persistence, base_extractiveness, 2250, 0.39).
narrative_ontology:measurement(egypt_be_t3000, egypt_three_millennia_persistence, base_extractiveness, 3000, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(egypt_three_millennia_persistence, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(egypt_three_millennia_persistence, 0.2).
narrative_ontology:affects_constraint(egypt_three_millennia_persistence, spartan_institutional_collapse).
narrative_ontology:affects_constraint(egypt_three_millennia_persistence, hindu_system_interpretive_pluralism).
narrative_ontology:affects_constraint(egypt_three_millennia_persistence, ptolemaic_hellenization_integration).
narrative_ontology:affects_constraint(egypt_three_millennia_persistence, maat_theological_kernel_stability).

% DUAL FORMULATION NOTE:
% The pharaonic system's three-millennia persistence is explained as a composite of: (1) anchored theological kernel (Ma'at-Amun cosmology, immutable legitimacy source), (2) mid-range interpretive accretion capacity (theological flexibility enabling absorption of Atenism, Amun syncretism, Ptolemaic Hellenization without kernel collapse), and (3) high suppression of peasant base (structural entrenchment preventing exit-driven destabilization). This constraint family links three comparative cases: Spartan system (low accretion depth, 3 centuries), Pharaonic system (mid-range accretion depth, 3000 years), Hindu system (high accretion depth, 4000+ years). Each story measures the same institutional axis profile under different accretion depths, supporting the composability thesis that institutional longevity is multiply-realizable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
