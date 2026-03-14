% ============================================================================
% CONSTRAINT STORY: chemical_regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_chemical_regulatory_capture, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: chemical_regulatory_capture
 *   human_readable: Chemical Industry Regulatory Capture
 *   domain: industrial_regulation/environmental_governance
 *
 * SUMMARY:
 *   Chemical regulatory capture represents a structural constraint in which
 *   the regulated industry systematically influences the regulatory agencies
 *   ostensibly designed to constrain it. The mechanism operates through
 *   multiple channels: industry funding of safety studies, participation in
 *   standard-setting committees, technical complexity that creates
 *   information asymmetries favoring incumbent firms, political access and
 *   lobbying, and the threat of regulatory arbitrage (relocation to
 *   less-regulated jurisdictions). The constraint exhibits tangled_rope
 *   structure: genuine coordination functions exist (complex hazard
 *   assessment requires technical expertise, international harmonization
 *   prevents regulatory fragmentation), yet these functions are
 *   asymmetrically captured to benefit incumbent manufacturers while
 *   extracting costs from exposed communities and regulatory agencies.
 *   Theater_ratio increases over time as risk assessment procedures become
 *   more elaborate and technically sophisticated while remaining
 *   operationally biased toward incumbent interests. Extractiveness increases
 *   as regulatory agency capture deepens and safer alternative pathways are
 *   systematically disadvantaged. The constraint is not a pure snare because
 *   coordination functions are real, but it is not a rope because the
 *   coordination benefits are asymmetrically distributed.
 *
 * KEY AGENTS:
 *   - Chemical Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture regulatory processes to maintain market position and delay substitution; can relocate production to less-regulated jurisdictions
 *   - Exposed Communities: Primary victims (powerless/trapped) — face health and environmental costs with no exit option or meaningful regulatory participation; generational consequences from bioaccumulative toxins
 *   - Environmental Regulatory Agencies: Secondary victim (moderate/constrained) — must coordinate hazard assessment while being captured; bear accountability without enforcement authority
 *   - Safer Alternative Producers: Organized secondary victim (organized/constrained) — face asymmetric regulatory burdens; market entry constrained by incumbent regulatory advantages
 *   - Risk Assessment Bureaucracy: Institutional performer (institutional/arbitrage) — maintains elaborate procedures with operationalized bias toward industry; theater persists through institutional inertia
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory capture as inevitable feature of political economy rather than contingent institutional outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(chemical_regulatory_capture, 0.58).
domain_priors:suppression_score(chemical_regulatory_capture, 0.65).
domain_priors:theater_ratio(chemical_regulatory_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(chemical_regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(chemical_regulatory_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(chemical_regulatory_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(chemical_regulatory_capture, tangled_rope).
narrative_ontology:human_readable(chemical_regulatory_capture, "Chemical Industry Regulatory Capture").
narrative_ontology:topic_domain(chemical_regulatory_capture, "industrial_regulation/environmental_governance").

domain_priors:requires_active_enforcement(chemical_regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(chemical_regulatory_capture, chemical_manufacturers).
narrative_ontology:constraint_beneficiary(chemical_regulatory_capture, agrochemical_producers).
narrative_ontology:constraint_victim(chemical_regulatory_capture, public_health).
narrative_ontology:constraint_victim(chemical_regulatory_capture, environmental_commons).
narrative_ontology:constraint_victim(chemical_regulatory_capture, regulatory_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXPOSED COMMUNITIES (SNARE) — Communities near manufacturing facilities or agricultural regions have no exit option. They bear full health and environmental costs with no meaningful participation in regulatory decisions. Regulatory capture ensures their exposure persists despite available safer alternatives. Maximum extraction with zero agency.
constraint_indexing:constraint_classification(chemical_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PERSISTENT BIOACCUMULATIVE TOXINS VICTIMS (SNARE) — Communities exposed to legacy chemicals (PCBs, DDT, PFOA/PFOS) face generational health consequences. Regulatory capture delayed restrictions for decades. Even after formal bans, environmental persistence means continued exposure. Trapped across biological and temporal scales.
constraint_indexing:constraint_classification(chemical_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ENVIRONMENTAL REGULATORY AGENCIES (TANGLED ROPE) — Agencies must coordinate complex hazard assessment (genuine coordination function) while being constrained by captured decision processes, political pressure, and resource dependency on regulated industry for technical data. Asymmetric extraction: agencies bear accountability for health outcomes while lacking effective enforcement authority.
constraint_indexing:constraint_classification(chemical_regulatory_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CHEMICAL MANUFACTURERS (ROPE) — Primary beneficiaries experience the constraint as efficient coordination: industry participation in standard-setting, tiered testing requirements, and regulatory flexibility create profitable pathways. Arbitrage options allow manufacturers to relocate production to less-regulated jurisdictions if constraints tighten. Net beneficiary position.
constraint_indexing:constraint_classification(chemical_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: SAFER ALTERNATIVE PRODUCERS (TANGLED ROPE) — Firms developing safer chemical substitutes are constrained by regulatory timelines that favor incumbent products. They have some agency (market entry, licensing) but face extraction: regulatory approval requires expensive safety studies while incumbents operate under grandfathered exemptions. Mixed coordination-extraction: market coordination exists but rules are asymmetrically applied.
constraint_indexing:constraint_classification(chemical_regulatory_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: RISK ASSESSMENT BUREAUCRACY (PITON) — Formal risk assessment procedures (hazard characterization, dose-response, exposure quantification) are heavily performative. Industry-funded studies dominate the evidence base; endpoints are chosen to minimize detected harm; uncertainty factors are systematically reduced. The bureaucratic theater persists through institutional inertia despite systematic bias toward industry interests. Theater_ratio high because the assessment ritual appears rigorous but operationalizes a captured frame.
constraint_indexing:constraint_classification(chemical_regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational view, regulatory capture can appear as an inevitable law of political economy: industries will always seek favorable regulation, and large corporate entities will always have advantages in regulatory processes. This perspective naturalizes what is actually a contingent institutional outcome. The engine's false summit detector will flag this as naturalization of a structural arrangement that could be reformed.
constraint_indexing:constraint_classification(chemical_regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(chemical_regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(chemical_regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(chemical_regulatory_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(chemical_regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(chemical_regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(chemical_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from exposed communities and alternative producers while also capturing regulatory agency resources and attention. The extraction is substantial but not as severe as a pure snare because manufacturers must still meet minimum safety standards and face some regulatory constraints. Suppression (0.65): High. Barriers to exit are significant: exposed communities cannot relocate; regulatory agencies cannot abandon their mandate; safer alternatives face long approval timelines; public awareness is systematically limited by industry PR and regulatory framing. Theater ratio (0.68): High and increasing. Risk assessment procedures are elaborate and appear rigorous but operationalize captured assumptions about what counts as evidence, what endpoints matter, what uncertainty factors apply. As procedures become more complex, their performative character increases — the appearance of rigor obscures systematic bias. The trajectory reflects Goodhart's Law: as metrics become targets, they lose their value. Risk assessment was designed to assess risk; now it is used to certify predetermined industry-favorable conclusions.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Manufacturers see coordination (Rope) — industry participation in regulatory design efficiently incorporates technical expertise. Safer alternatives see asymmetric extraction (Tangled Rope) — the system coordinates benefits for incumbents while constraining challengers. Exposed communities see irreversible harm (Snare) — trapped with no exit option and no effective regulatory protection. Regulatory agencies see capture and degradation (Tangled Rope at moderate power, Piton at institutional view of the system itself). The analytical observer risks seeing a natural law (Mountain) — 'industries always influence their regulators' — but structural data shows this is a contingent outcome: jurisdictions with stronger conflict-of-interest rules, independent testing requirements, and stakeholder representation show measurably different outcomes. The divergence is not just about different time horizons or power levels — it reflects fundamentally different structural relationships to the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Manufacturers occupy the extracted-from position in this constraint despite their beneficiary status because they experience the regulatory process as coordination: they participate in design, provide technical input, and shape outcomes. Their d value is low (derived from institutional power + arbitrage exit + beneficiary status), yielding low or negative f(d). Exposed communities occupy the extraction-target position: trapped exit + victim status yields high d (~0.95), high f(d) (~1.42), maximum experienced extractiveness. Safer alternative producers occupy an intermediate position: organized power but constrained exit + victim status yields moderate d (~0.55), moderate f(d) (~0.75). Regulatory agencies experience capture and constraint: moderate power, constrained exit, but victim status of the regulatory system's independence yields d (~0.60), f(d) (~0.88). The directionality gradient explains why the same structural constraint produces snare classification (high d targets), tangled_rope (moderate d agents), and rope classification (low d beneficiaries) from different perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: Regulatory capture resolves mandatrophy by showing that the constraint genuinely coordinates technical assessment (satisfying rope requirements) while simultaneously extracting from powerless agents and degrading regulatory independence (satisfying snare requirements). The tangled_rope classification is not a compromise between two types — it is the structural reality. The coordination function (hazard assessment, standard-setting) is real and necessary; the asymmetric distribution of benefits and costs is also real and structural. Neither function can be removed without destroying the other. What can change is the degree of capture: stronger agency independence, mandatory independent testing, stakeholder representation, and international regulatory harmonization can reduce extraction while preserving coordination. The mandatrophy is resolved by recognizing that 'pure' rope-like coordination at this scale has never existed — the institutional structure of chemical regulation has always involved some degree of industry influence. The question is not whether to coordinate but how to coordinate in a way that protects powerless agents from bearing the full cost of uncertainty. The analytical observer's mountain (regulation as inevitably captured) is a false summit: alternative institutional designs exist and produce measurably different outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    industry_data_dominance,
    'Does industry funding and generation of safety study data constitute necessary technical expertise or systematic bias in the evidence base?',
    'Comparison of health outcomes and environmental persistence in jurisdictions with industry-generated vs independent testing data; analysis of publication bias in industry-funded vs independent studies; detection of selective endpoint reporting.',
    'If industry data is biased: regulatory capture is structural and severe (ε upward toward 0.70). If industry data is necessary and reliable: extraction mechanism is weaker (ε downward toward 0.40).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(industry_data_dominance, empirical, 'Whether industry-funded safety data introduces systematic bias').

omega_variable(
    precautionary_principle_applicability,
    'Should regulatory approval require proof of safety (precautionary) or proof of harm (incumbent default)? Does this choice constitute regulatory policy or a natural constraint?',
    'Historical analysis of jurisdictions with precautionary vs incumbent-burden standards; correlation with detected adverse health outcomes; economic analysis of innovation costs under each regime.',
    'If precautionary is feasible and reduces harm: current system is a policy choice favoring incumbents (confirms tangled_rope extraction). If precautionary stifles beneficial innovation: current system reflects unavoidable tradeoff (weakens extraction narrative).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precautionary_principle_applicability, conceptual, 'Feasibility and consequences of shifting burden of proof in chemical approval').

omega_variable(
    regulatory_independence_restoration,
    'Can regulatory agencies regain independence through structural reforms (funding separation, conflict-of-interest rules, independent testing requirements, stakeholder representation)?',
    'Case studies of regulatory reforms (EU REACH directive, California Prop 65, restrictions on per- and polyfluoroalkyl substances); measurement of health outcome improvements post-reform; tracking of industry influence metrics pre/post reform.',
    'If reforms restore independence: capture is not inherent; sunset clauses could apply. If industry influence persists despite reforms: capture is deeply structural (supports snare classification for victims, suggests lower likelihood of scaffold pathway).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_independence_restoration, empirical, 'Whether structural reforms can interrupt regulatory capture mechanisms').

omega_variable(
    international_regulatory_arbitrage,
    'Do multinational chemical firms routinely relocate production and sales to lower-regulation jurisdictions, and does this arbitrage actually constrain regulatory tightening?',
    'Analysis of production location decisions correlating with regulatory stringency; measurement of chemical availability across jurisdictions; tracking of manufacturer lobbying against harmonized standards.',
    'If arbitrage is effective: it is a genuine structural constraint on regulation (supports manufacturer arbitrage exit classification). If manufacturers stay despite tighter regulation: constraints can be tightened without capital flight (weakens arbitrage justification, suggests higher regulatory capture severity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_regulatory_arbitrage, empirical, 'Effectiveness of regulatory arbitrage as constraint on tightening rules').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(chemical_regulatory_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chem_reg_tr_t0, chemical_regulatory_capture, theater_ratio, 0, 0.45).
narrative_ontology:measurement(chem_reg_tr_t10, chemical_regulatory_capture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(chem_reg_tr_t20, chemical_regulatory_capture, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(chem_reg_be_t0, chemical_regulatory_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(chem_reg_be_t10, chemical_regulatory_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(chem_reg_be_t20, chemical_regulatory_capture, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(chemical_regulatory_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(chemical_regulatory_capture, 0.25).
narrative_ontology:affects_constraint(chemical_regulatory_capture, petrochemical_subsidization).
narrative_ontology:affects_constraint(chemical_regulatory_capture, agriculture_pesticide_monoculture).
narrative_ontology:affects_constraint(chemical_regulatory_capture, drinking_water_contamination).

% DUAL FORMULATION NOTE:
% Chemical regulatory capture is downstream of industry influence mechanisms (lobbying, funding, revolving door employment) and upstream of specific chemical harms (pesticide exposure, water contamination, worker health). The ε value reflects the degree to which the regulatory apparatus systematically favors incumbent manufacturers. Related constraints with distinct ε values: petrochemical price support (higher ε, pure extraction), agricultural pesticide lock-in (moderate ε, coordination-dependent), drinking water PFAS contamination (higher ε, victim concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(chemical_regulatory_capture, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
