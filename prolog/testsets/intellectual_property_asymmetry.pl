% ============================================================================
% CONSTRAINT STORY: intellectual_property_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_intellectual_property_asymmetry, []).

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
 *   constraint_id: intellectual_property_asymmetry
 *   human_readable: Intellectual Property Asymmetry in Knowledge Markets
 *   domain: economic/institutional/knowledge_governance
 *
 * SUMMARY:
 *   Intellectual property asymmetry represents a structural extraction
 *   mechanism embedded in global knowledge governance regimes. The constraint
 *   operates across pharmaceutical access, software development, agricultural
 *   innovation, and academic research — wherever knowledge goods have been
 *   assigned property rights enforceable by state monopoly on force. This
 *   constraint exhibits genuine coordination function (IP protection does
 *   incentivize some innovation investment and enables licensing
 *   arrangements) alongside severe extraction (artificial scarcity restricts
 *   access to life-saving medications, constrains development-economy
 *   innovation, extracts rents from knowledge commons). The asymmetry is not
 *   in innovation incentives per se but in who bears costs of enforcement:
 *   wealthy corporations benefit from protection and have arbitrage options;
 *   resource-constrained agents (generic drug manufacturers,
 *   developing-economy researchers, poor patients) bear extraction costs
 *   without meaningful coordination benefit. The extractiveness trajectory
 *   shows monotonic increase from 0.35 to 0.58 over 20 years, driven by
 *   patent scope creep (business method patents, evergreening strategies,
 *   software patents on trivial logic) and enforcement intensity
 *   amplification via TRIPS harmonization and trade leverage. Theater ratio
 *   rises from 0.48 to 0.62, reflecting the gap between stated IP purpose
 *   (incentivize innovation) and actual mechanism (protect incumbent rents):
 *   innovation rates have decoupled from patent protection across multiple
 *   domains; enforced scarcity persists despite reduced justification.
 *
 * KEY AGENTS:
 *   - Patent-Holding Corporations: Primary beneficiary (institutional/arbitrage) — capture rents from monopoly enforcement; have exit options via cross-licensing, patent pools, strategic acquisition
 *   - Generic Drug Manufacturers: Primary victim (powerless/trapped) — cannot produce medications without legal liability; face trade sanctions and market exclusion
 *   - Developing-Economy Innovators: Secondary victim (powerless/constrained) — cannot build on patented knowledge; costly licensing fees create barriers to entry
 *   - Global Knowledge Commons: Tertiary victim (powerless/trapped) — abstract collective; cannot organize or exit; bears cost of artificial scarcity
 *   - Resource-Constrained Researchers: Mixed position (moderate/constrained) — benefit from IP ownership but constrained by access costs to others' research
 *   - Patent Office / IP Regulator: Institutional actor (institutional/constrained) — bound by TRIPS obligations; holds enforcement lever but constrained by trade agreements
 *   - Open Science Coalition: Organized agents (organized/constrained) — building exit pathways via open-source software, open-access publishing, compulsory licensing frameworks
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent policy as immutable law of innovation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(intellectual_property_asymmetry, 0.58).
domain_priors:suppression_score(intellectual_property_asymmetry, 0.68).
domain_priors:theater_ratio(intellectual_property_asymmetry, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(intellectual_property_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(intellectual_property_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(intellectual_property_asymmetry, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(intellectual_property_asymmetry, tangled_rope).
narrative_ontology:human_readable(intellectual_property_asymmetry, "Intellectual Property Asymmetry in Knowledge Markets").
narrative_ontology:topic_domain(intellectual_property_asymmetry, "economic/institutional/knowledge_governance").

domain_priors:requires_active_enforcement(intellectual_property_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(intellectual_property_asymmetry, patent_holders).
narrative_ontology:constraint_beneficiary(intellectual_property_asymmetry, pharmaceutical_corporations).
narrative_ontology:constraint_beneficiary(intellectual_property_asymmetry, technology_incumbents).
narrative_ontology:constraint_victim(intellectual_property_asymmetry, knowledge_access_commons).
narrative_ontology:constraint_victim(intellectual_property_asymmetry, developing_economy_innovators).
narrative_ontology:constraint_victim(intellectual_property_asymmetry, generic_drug_manufacturers).
narrative_ontology:constraint_victim(intellectual_property_asymmetry, academic_researchers_resource_constrained).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GENERIC DRUG MANUFACTURER IN DEVELOPING ECONOMY (SNARE) — Trapped by patent enforcement: cannot produce life-saving medications at scale without violating IP law. Faces legal liability, trade sanctions, and exclusion from international markets. No exit option except accepting dependency on expensive patent-holders. Maximum experienced extraction — bears full cost of IP protection regime without coordination benefit.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL KNOWLEDGE COMMONS (SNARE) — Cannot organize or exit; bears structural cost of artificial scarcity imposed by IP enforcement. Prevents coordination on disease prevention, agricultural adaptation, and scientific advancement. The commons has no agent to advocate for it. Extraction occurs through restriction of beneficial knowledge diffusion.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: RESOURCE-CONSTRAINED ACADEMIC RESEARCHER (TANGLED ROPE) — Constrained by prohibitive journal access fees and patent licensing costs, but benefits from IP regime through ownership of research outputs. Experiences mixed coordination (IP incentivizes research funding, patent licensing enables collaboration) and extraction (high access costs, licensing friction). Moderate power; surmountable but costly exit paths (open-access publication, institutional repositories).
constraint_indexing:constraint_classification(intellectual_property_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PATENT-HOLDING CORPORATION (ROPE) — Primary beneficiary with arbitrage options (cross-licensing, patent pools, strategic IP acquisition). Experiences IP regime as pure coordination: legal certainty enables investment in R&D, patent sales generate revenue, licensing arrangements coordinate technology access. Net beneficiary with organizational exit capacity — can arbitrage between enforcement intensity and market access.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PATENT OFFICE / IP REGULATOR (TANGLED ROPE) — Constrained by international trade obligations (TRIPS, patent harmonization agreements) but holds institutional power to shape enforcement intensity. Experiences genuine coordination function (property rights enable innovation investment) alongside extraction (overly broad patents, evergreening strategies, enforcement against legitimate competitors). Required to enforce regime regardless of distributed harm; trapped between innovation incentives and access justice.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OPEN SCIENCE COALITION (SCAFFOLD) — Organized actors (Wikipedia, Linux Foundation, Creative Commons, open-access publishers, global generic drug initiatives) are building alternative knowledge production pathways that bypass traditional IP extraction. Low effective extraction because coalition has agency and sunset logic: open-source software, open-access research, and compulsory licensing mechanisms represent exit pathways. Estimated maturation: 15-30 years for norms to displace IP regime in core domains.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: IP PHILOSOPHY / INSTITUTIONAL INERTIA (PITON) — The claim that 'IP protection incentivizes innovation' is substantially performative at current enforcement levels. Evidence shows innovation rates in software (before/after IP enforcement), pharmaceutical development (patent life vs actual investment), and academia (open-access acceleration) decoupled from patent protection. IP regime persists through institutional inertia, TRIPS treaty lock-in, and incumbent corporate dependence on legacy patents — not because it optimally incentivizes innovation. Theater ratio 0.62 reflects the gap between IP regime's stated coordination function and its actual effect on innovation.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — This perspective risks classifying IP asymmetry as an immutable law of knowledge economics: 'property rights are necessary to incentivize creation; scarcity is inherent to innovation.' This framing naturalizes a contingent institutional arrangement. The base properties reveal this as a false summit: IP enforcement is a designed system with leverage points (patent scope, enforcement intensity, compulsory licensing). The mountain classification masks policy choices as natural necessity.
constraint_indexing:constraint_classification(intellectual_property_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intellectual_property_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(intellectual_property_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intellectual_property_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(intellectual_property_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(intellectual_property_asymmetry, TR),
    TR >= 0.70.

:- end_tests(intellectual_property_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. IP enforcement transfers wealth from knowledge-users to knowledge-holders beyond what innovation incentives justify. The 0.23-point increase over 20 years reflects patent scope creep and enforcement amplification (TRIPS harmonization, trade leverage, evergreening litigation). Baseline 0.35 (early regime, narrower scope) vs current 0.58 (expanded scope, harmonized enforcement) shows the constraint has become more extractive. Suppression (0.68): High. Barriers to knowledge access include: (1) legal prohibition on generic production, (2) prohibitive licensing fees, (3) trade sanctions against compulsory licensing, (4) lack of alternative production pathways for complex pharmaceuticals, (5) international agreement lock-in (TRIPS). Suppression has both structural (legal barriers) and geopolitical components (trade coercion). Theater ratio (0.62): Moderate-high. IP regime claims to incentivize innovation, but empirical decoupling is visible: (1) software innovation accelerated after IP enforcement weakened (Linux era), (2) pharmaceutical development timelines unchanged post-patent expansion, (3) academic research accelerates in open-access domains, (4) venture capital investment in tech follows network effects and team quality more than patent protection. The theatrical component reflects the gap between stated purpose and observed mechanism: enforcement justifies itself through innovation mythology rather than demonstrable causation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival divergence. Patent-holders see Rope: coordination mechanism enabling investment, licensing revenue, strategic partnerships. Resource-constrained researchers see Tangled Rope: genuine coordination benefit (IP ownership of outputs) alongside extraction (access costs). Developing-economy generic manufacturers see Snare: pure extraction with no coordination benefit — trapped by legal prohibition with no arbitrage options. Open science coalition sees Scaffold: temporary constraint with sunset logic — open-source and open-access pathways are maturing and will displace IP extraction in measurable domains. Patent office sees Tangled Rope from constrained position: coordinating innovation incentives while bearing political cost of access restrictions and trade leverage. The global knowledge commons sees Snare: cannot exit or organize; bears full cost of artificial scarcity. The false natural law (Mountain) perspective risks naturalizing IP as inherent to innovation, masking policy choices (patent scope, enforcement intensity, compulsory licensing allowance) as immutable economic laws. The perspectival gap reveals this: IP regime is designed, has leverage points, and exhibits counterfactual evidence (innovation in open-source domains, low-IP jurisdictions) that contradicts the mountain classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) derives from structural position relative to extraction flow. Patent-holders (d ≈ 0.05–0.15): beneficiaries with arbitrage options → low/negative f(d) → receive negative χ (constraint subsidizes them). Developing-economy generic manufacturers (d ≈ 0.90–1.00): trapped victims with no exit options → high f(d) ≈ 1.40 → maximum experienced extraction χ. Patent office / regulator (d ≈ 0.65–0.75): constrained institutional actor bearing enforcement costs and political pressure → moderate-high f(d) ≈ 1.00–1.15 → moderate extraction. Open science coalition (d ≈ 0.50–0.60): organized agents with emerging exit pathways → moderate f(d) ≈ 0.65–0.75 → moderate-low experienced extraction. The global knowledge commons (d ≈ 0.95): abstract victim without exit or representation → maximum extraction. Scope modifier σ(S) = 1.2 (global) amplifies effective extraction by 20% for all agents, reflecting that enforcement reaches worldwide and no jurisdiction can fully escape IP regime without trade sanctions.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY STRUCTURE: IP asymmetry likely decomposes into multiple distinct constraints with different ε values, revealing why mandatrophy resolution is complex. Candidate decomposition: (1) Patent-as-Coordination (ε ≈ 0.20, Rope) — genuine coordination benefit of property rights for large-scale innovation investment. (2) Patent-Scope Creep (ε ≈ 0.65, Snare) — expansion of patentability to trivial logic, business methods, natural sequences — pure extraction with minimal innovation incentive. (3) Evergreening Litigation Strategy (ε ≈ 0.72, Snare) — strategic patent refiling to extend monopoly without new innovation — pure extraction. (4) Access Restriction to Essential Medicines (ε ≈ 0.78, Snare) — patent enforcement preventing life-saving generic production — pure extraction. The aggregated story (ε = 0.58) is a weighted mixture of these structurally distinct constraints. Resolving mandatrophy requires identifying which ε value is primary (for the single-story case) or decomposing into the family. For this story, the 0.58 value captures the empirical reality: current global IP regime mixes genuine coordination (0.20 baseline) with serious scope creep and enforcement intensity extraction (0.35–0.78 in specific domains). The Tangled Rope classification holds this hybrid: coordination function persists (requisite beneficiaries), asymmetric extraction evident (victims present), active enforcement required (TRIPS, trade leverage). The rising extractiveness trajectory (0.35 → 0.58) reflects scope creep, not loss of coordination function — the regime is becoming more extractive while maintaining coordination pretense (theater ratio rising). Theater ratio trajectory (0.48 → 0.62) indicates the constraint's functional gap is widening: enforcement rhetoric emphasizes innovation but behavioral evidence (evergreening success, generic suppression, open-source acceleration) shows extraction mechanism dominant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    innovation_incentive_decoupling,
    'Is IP protection causally necessary for innovation, or is the correlation between IP regimes and innovation investment driven by incumbent corporation dependence on legacy patents and regulatory path-dependence?',
    'Historical comparison of innovation rates pre/post IP enforcement (software 1990s vs pre-IP era; pharmaceutical development before/after TRIPS; open-source contribution rates). Controlled comparison of innovation in high-IP vs low-IP jurisdictions accounting for research funding levels.',
    'If causally necessary: IP regime is coordination mechanism justified by efficiency (Rope classification confirmed). If decoupled: regime is pure incumbent protection with innovation benefit as secondary effect (Snare/Tangled Rope classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_incentive_decoupling, empirical, 'Whether IP protection is causally necessary for innovation or epiphenomenal').

omega_variable(
    patent_breadth_optimal_scope,
    'What patent claim breadth maximizes innovation while minimizing access barriers? Is current enforcement breadth (biotechnology patents on gene sequences, software patents on business logic) justified by innovation incentives or driven by incumbent rent-seeking?',
    'Analysis of patent litigation outcomes; comparison of innovation metrics before/after patent scope restriction; survey of innovators on whether current scope increases or decreases R&D investment; examination of evergreening litigation success rates.',
    'If current scope is optimal: IP enforcement is equilibrated (Rope/Tangled Rope classification confirmed). If scope exceeds optimal: extraction is hidden behind innovation mythology (Snare classification strengthened; extractiveness rises 0.15-0.25).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_breadth_optimal_scope, empirical, 'Whether current patent scope maximizes innovation or enables rent-seeking').

omega_variable(
    compulsory_licensing_sufficiency,
    'Do compulsory licensing mechanisms for essential medicines and green technologies adequately address IP asymmetry, or do they function as theatrical relief while preserving core extraction?',
    'Analysis of compulsory licensing outcomes: how often invoked, what remuneration patent-holders receive, impact on generic drug access and cost, adoption rates in different jurisdictions. Comparison of outcomes for jurisdictions with aggressive vs passive compulsory licensing.',
    'If sufficient: theater_ratio drops 0.15-0.25, classification shifts toward Rope/Scaffold. If theatrical: theater_ratio maintained, suppression persists, extraction confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compulsory_licensing_sufficiency, empirical, 'Whether compulsory licensing adequately addresses IP asymmetry').

omega_variable(
    open_commons_sustainability,
    'Can open-source, open-access, and commons-based peer production models sustain at scale for knowledge production at the complexity level of pharmaceutical development and biotechnology?',
    'Trajectory analysis of open-source software (Linux, Apache, Kubernetes) innovation rates and code quality. Examination of open-access publishing growth and research quality metrics. Analysis of failure/success rates for commons-based drug development initiatives (Drugs for Neglected Diseases, malaria research). Comparison of development timelines and costs: open vs proprietary models for equivalent complexity.',
    'If sustainable: scaffold perspective validated — open-science sunset is structurally possible. Extractiveness ceiling in those domains drops 0.20-0.30 as alternatives mature. If unsustainable: scaffold is aspirational; IP extraction remains necessary for high-complexity innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_commons_sustainability, empirical, 'Whether commons-based production can scale to pharmaceutical complexity').

omega_variable(
    trade_obligation_lock_in,
    'How much of current global IP enforcement intensity is driven by TRIPS agreement and bilateral trade leverage vs genuine consensus on optimal IP policy? What would countries choose absent trade coercion?',
    'Analysis of IP-related trade dispute settlements (WTO cases on pharmaceutical patents, generic drug bans). Survey of developing economy government preferences on IP policy absent trade pressure. Historical analysis of pre-TRIPS IP regimes and their documented outcomes on innovation and access. Examination of carve-outs and compulsory licensing attempts (Thailand generic antiretrovirals, India pharmaceutical generics) and trade retaliation consequences.',
    'If TRIPS is largely consensual optimum: regime is legitimated coordination (Rope). If TRIPS is coerced extraction: extractiveness rises 0.15-0.25, suppression confirmed as partially structural (trade sanctions), classification solidifies as Snare/Tangled Rope. Identifies suppression as having a geopolitical mechanism beyond intellectual property itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trade_obligation_lock_in, empirical, 'How much IP enforcement is trade-coerced vs consensual optimal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(intellectual_property_asymmetry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_asym_tr_t0, intellectual_property_asymmetry, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ip_asym_tr_t10, intellectual_property_asymmetry, theater_ratio, 10, 0.55).
narrative_ontology:measurement(ip_asym_tr_t20, intellectual_property_asymmetry, theater_ratio, 20, 0.62).

% Extraction over time
narrative_ontology:measurement(ip_asym_be_t0, intellectual_property_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ip_asym_be_t10, intellectual_property_asymmetry, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ip_asym_be_t20, intellectual_property_asymmetry, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(intellectual_property_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(intellectual_property_asymmetry, 0.22).
narrative_ontology:affects_constraint(intellectual_property_asymmetry, pharmaceutical_access_disparity).
narrative_ontology:affects_constraint(intellectual_property_asymmetry, agricultural_adaptation_barrier).
narrative_ontology:affects_constraint(intellectual_property_asymmetry, software_innovation_fragmentation).
narrative_ontology:affects_constraint(intellectual_property_asymmetry, academic_knowledge_concentration).

% DUAL FORMULATION NOTE:
% IP asymmetry decomposes into domain-specific constraints: patent-scope-creep (Snare, ε ≈ 0.65), evergreening-litigation (Snare, ε ≈ 0.72), access-restriction-essential-medicines (Snare, ε ≈ 0.78), and genuine-coordination-baseline (Rope, ε ≈ 0.20). The 0.58 aggregate value reflects weighted distribution across these structurally distinct mechanisms. Each downstream constraint exhibits its own extractiveness; the network structure captures how scope creep in one domain (e.g., biotech patents on gene sequences) affects others (agricultural adaptation, pharmaceutical access).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(intellectual_property_asymmetry, institutional, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
