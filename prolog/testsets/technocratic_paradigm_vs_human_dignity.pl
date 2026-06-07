% ============================================================================
% CONSTRAINT STORY: technocratic_paradigm_vs_human_dignity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technocratic_paradigm_vs_human_dignity, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: technocratic_paradigm_vs_human_dignity
 *   human_readable: Technocratic Paradigm vs Human Dignity in AI Systems
 *   domain: technology_ethics/political_theology/labor
 *
 * SUMMARY:
 *   The technocratic paradigm in AI systems embeds an
 *   efficiency-control-profit logic that treats persons as optimizable data
 *   rather than ends-in-themselves with infinite ontological dignity. This
 *   constraint operates through design choices: what AI systems measure
 *   (productivity metrics, risk scores, engagement time), what they optimize
 *   (profit, control, efficiency), and what they exclude (worker agency,
 *   contextual judgment, dignity-respecting alternatives). The encyclical
 *   Antiqua et Nova (hypothetical) identifies this as a structural challenge
 *   to Catholic Social Doctrine's core commitments: human dignity, common
 *   good, subsidiarity, solidarity, and justice. The constraint exhibits
 *   tangled rope dynamics: AI genuinely solves coordination problems
 *   (matching supply and demand, optimizing logistics, personalizing
 *   services) while simultaneously extracting value through asymmetric power
 *   (algorithmic management strips worker agency, surveillance capital
 *   monetizes attention, predictive systems exclude vulnerable populations).
 *   The theater ratio (0.58) reflects the gap between AI systems' claimed
 *   neutrality ('objective algorithms') and their actual operation (embedding
 *   the values and interests of their designers and owners). Measurements
 *   show extraction accumulation (0.42 → 0.68) and enforcement
 *   intensification (suppression 0.48 → 0.72) over the 2010-2022 interval as
 *   AI systems penetrated labor markets, credit allocation, policing, and
 *   social services.
 *
 * KEY AGENTS:
 *   - Algorithmically Managed Workers: Primary victims (powerless/trapped) — gig workers, warehouse employees, call center staff subjected to real-time algorithmic control with no exit except unemployment
 *   - Excluded Populations: Primary victims (powerless/identity_locked) — communities categorized as high-risk by credit scores, predictive policing, hiring algorithms; identity constituted through data profiles
 *   - Tech Monopolies: Primary beneficiaries (institutional/arbitrage) — platform firms extracting value through network effects, data accumulation, and regulatory capture
 *   - Small Businesses on Platforms: Mixed position (moderate/constrained) — benefit from market access, bear extraction through fees and algorithmic control; can exit at high cost
 *   - Digital Rights Coalition: Organized resistance (organized/constrained) — labor unions, privacy advocates, algorithmic accountability groups building alternative frameworks with sunset logic (GDPR, AI Act, platform cooperatives)
 *   - Captured Regulators: Institutional actors (institutional/constrained) — formal authority undermined by revolving-door dynamics and expertise asymmetry; see the capture but cannot break it
 *   - Magisterial Teaching Authority: Analytical observer (analytical/analytical) — Catholic Social Doctrine applied to AI; sees both coordination function and structural extraction; constrained by need to engage secular frameworks while maintaining doctrinal coherence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technocratic_paradigm_vs_human_dignity, 0.68).
domain_priors:suppression_score(technocratic_paradigm_vs_human_dignity, 0.72).
domain_priors:theater_ratio(technocratic_paradigm_vs_human_dignity, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_dignity, extractiveness, 0.68).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_dignity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_dignity, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_dignity, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(technocratic_paradigm_vs_human_dignity, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technocratic_paradigm_vs_human_dignity, tangled_rope).
narrative_ontology:human_readable(technocratic_paradigm_vs_human_dignity, "Technocratic Paradigm vs Human Dignity in AI Systems").
narrative_ontology:topic_domain(technocratic_paradigm_vs_human_dignity, "technology_ethics/political_theology/labor").

domain_priors:requires_active_enforcement(technocratic_paradigm_vs_human_dignity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technocratic_paradigm_vs_human_dignity, 'ced49b29-7cfd-4312-acce-d7653b43e814').
narrative_ontology:cs_kernel_codification('ced49b29-7cfd-4312-acce-d7653b43e814', formalized).
narrative_ontology:cs_authority_grounding('ced49b29-7cfd-4312-acce-d7653b43e814', lineage).
narrative_ontology:cs_interpretation_layer_present('ced49b29-7cfd-4312-acce-d7653b43e814').
narrative_ontology:cs_created_at('ced49b29-7cfd-4312-acce-d7653b43e814', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_dignity, tech_monopolies).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_dignity, surveillance_capital_firms).
narrative_ontology:constraint_beneficiary(technocratic_paradigm_vs_human_dignity, algorithmic_management_platforms).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_dignity, algorithmically_managed_workers).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_dignity, excluded_populations).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_dignity, communities_subjected_to_predictive_policing).
narrative_ontology:constraint_victim(technocratic_paradigm_vs_human_dignity, human_dignity_as_collective_good).
narrative_ontology:constraint_vindicates(technocratic_paradigm_vs_human_dignity, efficiency_maximization_doctrine).
narrative_ontology:constraint_vindicates(technocratic_paradigm_vs_human_dignity, data_extractivism_legitimacy).
narrative_ontology:constraint_vindicates(technocratic_paradigm_vs_human_dignity, technocratic_solutionism).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ALGORITHMICALLY MANAGED WORKER (SNARE) — Trapped by economic necessity in systems that treat labor as optimizable input. No exit from gig platforms or warehouse management systems without losing livelihood. Experiences maximum extraction: agency stripped, dignity reduced to productivity metrics, surveillance constant. The coordination story (efficient matching, performance feedback) is cover for asymmetric control.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXCLUDED POPULATION (SNARE) — Identity-locked by algorithmic categorization: credit scores, risk profiles, predictive policing models constitute identity through data. Structurally could relocate or contest scores, but identity frame (internalized as 'high-risk', 'unbanked', 'flagged') makes exit unthinkable. Experiences extraction through exclusion from housing, credit, employment, public space. The system's claim to neutral optimization masks structural violence.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS ON PLATFORM (TANGLED ROPE) — Constrained by network effects and switching costs but genuinely benefits from platform access to markets. Experiences both coordination (customer reach, payment processing, logistics) and extraction (algorithmic demotion, fee escalation, data asymmetry). Can exit at high cost. Mixed experience: the platform solves real problems while extracting rents.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TECH MONOPOLY (ROPE) — Benefits from network effects, data accumulation, and regulatory capture. Experiences the constraint as pure coordination: AI systems solve the legitimate problem of matching supply and demand at scale, optimizing logistics, personalizing services. Extraction flows toward this agent. Arbitrage-level exit: can shift jurisdictions, lobby for favorable regulation, acquire competitors.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL RIGHTS COALITION (SCAFFOLD) — Organized agents (labor unions, privacy advocates, algorithmic accountability groups, Catholic social justice networks) see the technocratic paradigm as a temporary coordination failure with a sunset: GDPR, AI Act, worker data rights, platform cooperatives, and dignity-centered design frameworks are building alternative pathways. Constrained by capital's structural power but has agency and sees an exit path through regulation and alternative institutions.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: CAPTURED REGULATOR (TANGLED ROPE) — Institutional actor with formal authority but constrained by revolving-door dynamics, industry expertise asymmetry, and political pressure. Experiences both coordination (genuine need to regulate emerging technology, protect consumers) and extraction (regulatory frameworks shaped by industry lobbying, enforcement underfunded, expertise concentrated in firms being regulated). Not identity-locked — sees the capture — but structurally constrained from breaking it.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, tangled_rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: MAGISTERIAL TEACHING AUTHORITY (TANGLED ROPE) — Analytical observer grounded in Catholic Social Doctrine. Sees both genuine coordination function (AI can serve common good, enhance human capabilities, solve collective problems) and structural extraction (technocratic paradigm treats persons as means, concentrates power, erodes subsidiarity, violates dignity). Classification as Tangled Rope reflects the encyclical's core claim: AI is not inherently evil (mountain of technology) nor purely beneficial (rope of neutral tool), but a contested terrain where design choices embed either dignity-respecting or dignity-violating logics. The teaching authority's analytical position is itself constrained by the need to engage secular frameworks while maintaining doctrinal coherence.
constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technocratic_paradigm_vs_human_dignity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(technocratic_paradigm_vs_human_dignity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(technocratic_paradigm_vs_human_dignity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technocratic_paradigm_vs_human_dignity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Tech monopolies capture substantial value through network effects, data asymmetry, and algorithmic control. Workers experience wage suppression, intensified surveillance, and stripped agency. Excluded populations bear costs through denial of credit, housing, employment. The extraction is not total (some workers benefit from platform access, some users gain from personalized services), but the asymmetry is severe and growing. Suppression (0.72): High. Exit options are structurally constrained: workers face unemployment if they leave platforms; small businesses face loss of market access; excluded populations cannot escape algorithmic categorization. Network effects and switching costs create lock-in. Regulatory capture suppresses political alternatives. The suppression has intensified as AI systems became infrastructure. Theater ratio (0.58): Moderate-high. AI systems claim neutrality ('objective algorithms', 'data-driven decisions') while embedding the values and interests of their designers. The gap between claimed objectivity and actual operation is substantial but not total — some optimization is genuinely functional, not purely performative. The theater has grown as 'AI ethics' initiatives proliferate without changing underlying extraction mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates indexical classification across power and exit differentials. Tech monopolies see pure coordination (Rope) — AI solves legitimate problems of scale and complexity. Small businesses see mixed coordination and extraction (Tangled Rope) — platforms enable and constrain simultaneously. Digital rights coalition sees a temporary problem with a sunset (Scaffold) — alternative institutions and regulations are being built. Captured regulators see coordination and extraction with structural constraints (Tangled Rope) — cannot break the expertise asymmetry. Algorithmically managed workers and excluded populations see pure extraction (Snare) — the coordination story is cover for control and exclusion. The Magisterial teaching authority sees Tangled Rope from the analytical position: AI has genuine coordination potential (can serve common good) but current implementations embed dignity-violating logics (treat persons as means). The perspectival gap is the core of the encyclical's argument: what appears as neutral optimization from the beneficiary's seat appears as structural violence from the victim's seat, and the analytical observer must hold both truths simultaneously to avoid either technological determinism (mountain) or naive optimism (rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Tech monopolies are primary beneficiaries with arbitrage exit → low d → negative or low chi (experience the constraint as coordination). Algorithmically managed workers are victims with trapped exit → high d → high chi (experience maximum extraction). Excluded populations are victims with identity_locked exit → high d → high chi (extraction through exclusion, identity constituted by algorithmic categorization). Small businesses are mixed: beneficiaries (market access) and victims (fee extraction, algorithmic control) with constrained exit → moderate d → moderate chi. Digital rights coalition is organized with constrained exit → moderate-low d → moderate-low chi (has agency, sees exit path through regulation and alternatives). Captured regulators are institutional with constrained exit → moderate d → moderate chi (formal authority undermined by structural constraints). Magisterial teaching authority is analytical → d derived from doctrinal position as critic of technocratic paradigm → moderate-high d (experiences the constraint as a challenge to core commitments, not as a benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by identifying the structural ambiguity: AI systems are BOTH genuine coordination mechanisms (solving real problems of scale, complexity, information asymmetry) AND extraction mechanisms (concentrating power, stripping agency, violating dignity). The mandate (coordinate economic activity efficiently) has not outlived its function — the coordination is real. But the extraction layered onto the coordination has grown severe enough that the mixed character (tangled rope) is now the dominant experience for most agents except the primary beneficiaries. The Magisterial teaching does not claim AI is inherently evil (mountain of technology's limits) or purely beneficial (rope of neutral tool). It claims AI is a contested terrain where design choices matter: dignity-respecting AI is structurally possible (hence scaffold perspective is coherent) but not currently dominant (hence snare perspective from victims is accurate). The mandatrophy question 'Is this coordination or extraction?' is answered: it is both, and the ratio is shifting toward extraction as monopolization intensifies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dignity_operationalization_ambiguity,
    'Can ''human dignity'' as infinite ontological worth be operationalized in AI design constraints, or does any formalization reduce dignity to measurable attributes (thus contradicting the claim)?',
    'Theological-technical dialogue: Can dignity-preserving design be specified without reducing dignity to a metric? Case studies of systems claiming dignity-centered design vs. their actual operation.',
    'If dignity is formalizable: Scaffold perspective confirmed — alternative AI is structurally possible. If dignity resists formalization: the technocratic paradigm may be an inescapable feature of computational systems, and the Magisterial critique identifies a mountain (technology''s inherent limits) rather than a tangled rope (contestable design choices).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dignity_operationalization_ambiguity, conceptual, 'Whether human dignity can be operationalized in AI design without self-contradiction').

omega_variable(
    platform_cooperative_viability,
    'Can platform cooperatives (worker-owned, dignity-centered alternatives) achieve scale and network effects sufficient to compete with extractive monopolies, or do network dynamics structurally favor concentration?',
    'Longitudinal tracking of cooperative platforms (e.g., driver cooperatives, Fairbnb, Resonate) vs. monopoly platforms on user growth, capital access, and survival rates over 10-20 years.',
    'If cooperatives can scale: Scaffold sunset is real — alternative institutions are viable. If network effects structurally favor monopolies: the extraction is closer to a snare (exit suppressed by economic structure) than a tangled rope (exit costly but possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_cooperative_viability, empirical, 'Whether dignity-centered platform cooperatives can achieve competitive scale').

omega_variable(
    regulatory_capture_reversibility,
    'Is regulatory capture of AI governance a reversible political failure or a structural feature of expertise asymmetry between states and tech firms?',
    'Comparative analysis of regulatory outcomes in jurisdictions with different institutional designs (EU vs. US vs. China); correlation between regulator independence, technical capacity, and enforcement effectiveness.',
    'If reversible: the captured regulator perspective is tangled rope (coordination + extraction, fixable through institutional reform). If structural: the regulator is closer to powerless/trapped (cannot exit the expertise asymmetry), and the constraint is closer to snare from more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_reversibility, empirical, 'Whether regulatory capture is reversible or structurally inevitable').

omega_variable(
    theological_naturalization_risk,
    'Does the Magisterial teaching risk naturalizing contingent technological arrangements by framing AI''s dignity-violations as inherent to ''the technocratic paradigm'' rather than to specific capitalist property relations and power structures?',
    'Comparative theological analysis: Does the encyclical''s critique apply equally to worker-owned AI, state-directed AI, and capitalist AI? If the critique is paradigm-level (technology''s inherent logic) rather than structure-level (who owns and controls), it may be naturalizing.',
    'If naturalizing: the Magisterial perspective is a false summit (mountain claim masking a tangled rope). If the critique is structure-specific: the teaching correctly identifies contingent extraction mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_naturalization_risk, conceptual, 'Whether the encyclical naturalizes capitalist AI as ''the technocratic paradigm''').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technocratic_paradigm_vs_human_dignity, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_parad_theater_2010, technocratic_paradigm_vs_human_dignity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(tech_parad_theater_2013, technocratic_paradigm_vs_human_dignity, theater_ratio, 3, 0.44).
narrative_ontology:measurement(tech_parad_theater_2016, technocratic_paradigm_vs_human_dignity, theater_ratio, 6, 0.5).
narrative_ontology:measurement(tech_parad_theater_2019, technocratic_paradigm_vs_human_dignity, theater_ratio, 9, 0.55).
narrative_ontology:measurement(tech_parad_theater_2022, technocratic_paradigm_vs_human_dignity, theater_ratio, 12, 0.58).

% Extraction over time
narrative_ontology:measurement(tech_parad_extract_2010, technocratic_paradigm_vs_human_dignity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(tech_parad_extract_2013, technocratic_paradigm_vs_human_dignity, base_extractiveness, 3, 0.51).
narrative_ontology:measurement(tech_parad_extract_2016, technocratic_paradigm_vs_human_dignity, base_extractiveness, 6, 0.61).
narrative_ontology:measurement(tech_parad_extract_2019, technocratic_paradigm_vs_human_dignity, base_extractiveness, 9, 0.66).
narrative_ontology:measurement(tech_parad_extract_2022, technocratic_paradigm_vs_human_dignity, base_extractiveness, 12, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tech_parad_suppress_2010, technocratic_paradigm_vs_human_dignity, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(tech_parad_suppress_2016, technocratic_paradigm_vs_human_dignity, suppression_requirement, 6, 0.63).
narrative_ontology:measurement(tech_parad_suppress_2022, technocratic_paradigm_vs_human_dignity, suppression_requirement, 12, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technocratic_paradigm_vs_human_dignity, resource_allocation).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_dignity, gig_economy_labor_precarity).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_dignity, surveillance_capitalism_attention_extraction).
narrative_ontology:affects_constraint(technocratic_paradigm_vs_human_dignity, algorithmic_bias_in_criminal_justice).

% DUAL FORMULATION NOTE:
% The technocratic paradigm is a family-level constraint affecting multiple downstream domains (labor, finance, policing, healthcare). Each domain has its own extractiveness reflecting specific power asymmetries, but all share the common logic: persons treated as optimizable data. Decompose into domain-specific stories when analyzing particular implementations (e.g., Amazon warehouse management vs. predictive policing vs. credit scoring).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technocratic_paradigm_vs_human_dignity, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
