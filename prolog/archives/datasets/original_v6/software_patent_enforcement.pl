% ============================================================================
% CONSTRAINT STORY: software_patent_enforcement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_patent_enforcement, []).

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
 *   constraint_id: software_patent_enforcement
 *   human_readable: Software Patent Enforcement Regime
 *   domain: intellectual_property/technology/economics
 *
 * SUMMARY:
 *   Software patent enforcement creates a structural extraction mechanism
 *   that bifurcates the technology sector into patent-holding incumbents
 *   (beneficiaries) and developers/open-source communities (victims). The
 *   regime exhibits characteristics of both coordination (enabling
 *   cross-licensing, portfolio management, and standards negotiation among
 *   large actors) and pure extraction (litigation threats against independent
 *   developers and open-source projects). The extractiveness has increased
 *   over the measurement interval (0.35 → 0.58) as patent litigation volumes
 *   increased and assertion strategies matured. Theater ratio has also
 *   increased (0.52 → 0.68), reflecting that examination quality has not kept
 *   pace with assertion volumes — patents are issued with lower scrutiny and
 *   then enforced aggressively through litigation rather than resolved
 *   through examination clarity. The constraint represents a classic case
 *   where a coordination mechanism (intellectual property rights, enabling
 *   inventors to capture returns) has degraded into an extraction regime
 *   (litigation threats and submarine patents that do not solve any problem
 *   for the targets).
 *
 * KEY AGENTS:
 *   - Independent Developers: Primary victim (powerless/trapped) — face litigation threats with no resources to defend; cannot predict patent landscape or design around unknown claims
 *   - Open Source Communities: Primary victim (powerless/trapped) — millions of unpaid contributors exposed to asymmetric enforcement; cannot be sued individually but projects face shutdown risk
 *   - Patent Holding Corporations: Primary beneficiary (institutional/arbitrage) — benefit from enforcement actions, licensing revenue, and portfolio value; can exit through sales or licensing deals
 *   - Patent Litigation Firms: Secondary beneficiary (institutional/arbitrage) — revenue directly dependent on enforcement regime; profit from both plaintiff and defendant work
 *   - Mid-Size Software Companies: Mixed position (moderate/constrained) — constrained by licensing costs and litigation risk but also use patents defensively; benefit from cross-licensing coordination
 *   - Large Technology Companies: Mixed position (powerful/constrained) — constrained by licensing obligations and cross-license negotiations but also use patents to block competitors; powerful enough to negotiate favorable terms
 *   - Patent Office: Institutional actor (institutional/constrained) — tasked with examining and issuing patents; degraded function due to time constraints and prior art complexity; constrained by statute to continue despite low-quality outcomes
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the regime as an immutable feature of innovation incentives rather than a contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_patent_enforcement, 0.58).
domain_priors:suppression_score(software_patent_enforcement, 0.65).
domain_priors:theater_ratio(software_patent_enforcement, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_patent_enforcement, extractiveness, 0.58).
narrative_ontology:constraint_metric(software_patent_enforcement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(software_patent_enforcement, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_patent_enforcement, tangled_rope).
narrative_ontology:human_readable(software_patent_enforcement, "Software Patent Enforcement Regime").
narrative_ontology:topic_domain(software_patent_enforcement, "intellectual_property/technology/economics").

domain_priors:requires_active_enforcement(software_patent_enforcement).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_patent_enforcement, patent_holding_corporations).
narrative_ontology:constraint_beneficiary(software_patent_enforcement, patent_litigation_firms).
narrative_ontology:constraint_victim(software_patent_enforcement, independent_developers).
narrative_ontology:constraint_victim(software_patent_enforcement, open_source_communities).
narrative_ontology:constraint_victim(software_patent_enforcement, software_innovation_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT DEVELOPER (SNARE) — Small developer faces existential threat from patent assertions. Cannot afford litigation costs ($2M+ defense). Cannot design around thousands of submarine patents. Cannot predict which features violate which claims. Exit means abandoning career or relocating to jurisdictions without enforcement. Maximum suppression: legal system's complexity itself is weaponized. Zero coordination benefit — the patent system does not solve any problem the developer needs solved.
constraint_indexing:constraint_classification(software_patent_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: OPEN SOURCE COMMUNITY (SNARE) — Millions of unpaid developers contribute code that may infringe unknown patents. Enforcement action targets not individual developers but projects (Linux, Apache, etc.). Community has no litigation budget, no exit option except project shutdown. Suppression is total: the threat landscape is opaque (submarines), the enforcement is asymmetric (large corporates sue, not vice versa), and coordination around the problem is legally risky (discussing patent workarounds can constitute inducement to infringe).
constraint_indexing:constraint_classification(software_patent_enforcement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SIZE COMPANY (TANGLED ROPE) — Constrained by licensing costs and litigation risk but also uses patent system defensively (cross-licenses, patent portfolios as negotiation tools). The system genuinely coordinates: patent pools, cross-licensing agreements, and portfolio-based 'freedom to operate' negotiations reduce risk. But extraction is asymmetric: large patent holders extract more favorable licensing terms. Net experience is mixed coordination and extraction.
constraint_indexing:constraint_classification(software_patent_enforcement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PATENT HOLDING CORPORATION (ROPE) — Experiences patent enforcement as pure coordination: standardizes licensing, enables cross-deals, creates portfolio value. No suppression from this actor's view. Benefits from asymmetric litigation costs (can afford defense). Exit option is arbitrage: licensing, litigation threats, portfolio sales. Net experience is coordination without extraction — the system works as designed from this position.
constraint_indexing:constraint_classification(software_patent_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LITIGATION FIRM (ROPE) — Experiences enforcement as pure coordination: orchestrates claims, manages discovery, structures settlements. Revenue depends on the regime existing and functioning. Exit option is arbitrage: can litigate elsewhere or in different domains. No suppression; zero coordination problem to solve — the system generates work. Benefits directly from enforcement.
constraint_indexing:constraint_classification(software_patent_enforcement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PATENT OFFICE (PITON) — Performs the function of examining and issuing software patents despite widespread recognition that software patents are difficult to quality-assess, prone to overbreadth, and harmful to innovation. Theater ratio (0.68): examination procedures are performative — examiners have limited time (~20 hours per patent), software prior art is scattered across repositories and academic literature, and obviousness standards are contested. The office persists through institutional inertia and statutory mandate, not because the function works well. Metrics show declining examination rigor and increasing litigation rates.
constraint_indexing:constraint_classification(software_patent_enforcement, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: LARGE TECH COMPANY (TANGLED ROPE) — Constrained by licensing obligations and cross-license negotiations but also uses patent system to capture markets and block smaller competitors. Genuine coordination function (standards setting, portfolio management) exists alongside extractive blocking. Can afford litigation, build patent moats, and negotiate favorable terms. Asymmetric position: powerful enough to extract, but still constrained by large peers.
constraint_indexing:constraint_classification(software_patent_enforcement, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risks viewing software patent enforcement as an immutable feature of innovation incentive systems: 'Intellectual property protection is necessary for technology investment.' However, this naturalizes a contingent institutional arrangement. Historical fact: major software platforms (Unix, World Wide Web, Linux, most cloud infrastructure) developed with minimal or no patent protection. The 'necessity' claim confuses correlation (some patent-heavy tech companies exist) with causation (patents caused their innovation). The mountain classification fails the accessibility collapse and resistance gates when examined empirically.
constraint_indexing:constraint_classification(software_patent_enforcement, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_patent_enforcement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(software_patent_enforcement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(software_patent_enforcement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(software_patent_enforcement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(software_patent_enforcement, TR),
    TR >= 0.70.

:- end_tests(software_patent_enforcement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts value from independent developers and open-source communities (litigation threats, licensing fees, project shutdown risk) and transfers it to patent holders. However, extraction is not total because large technology companies also benefit from coordination mechanisms (cross-licensing, portfolio management) and some innovation does occur within the regime. The measurement shows extraction increasing over time as assertion strategies matured (from 0.35 to 0.58), indicating that the regime has shifted from coordination-dominant to extraction-dominant. Suppression (0.65): Moderate-high. Barriers to exit include the opacity of the patent landscape (thousands of patents in any software domain, many with overlapping claims), litigation costs ($2M+ to defend against a single patent assertion), and the legal complexity that makes workarounds unpredictable. Suppression is asymmetric: well-resourced actors can afford litigation and navigate licensing; poorly-resourced actors face existential threats. Theater ratio (0.68): Moderate-high. Patent examination for software is substantially performative. Examiners have limited time (~20 hours per patent) to assess novelty and obviousness across scattered prior art (academic papers, open-source code, implementation details). Patents are issued based on plausibility rather than rigorous assessment, then enforced through litigation where the real claims evaluation occurs. The theater has increased as examination backlogs grew and assertion litigation became more aggressive.
 *
 * PERSPECTIVAL GAP:
 *   The gap between patent holder perception (Rope: pure coordination) and developer perception (Snare: pure extraction) reveals the regime's bifurcation. The beneficiaries experience the system as solving a genuine problem (allocating returns from innovation, enabling licensing negotiations). The victims experience the system as imposing a pure threat with no problem it solves for them. The large technology company perspective (Tangled Rope) represents an intermediate case where genuine coordination (cross-licensing, portfolio management) coexists with asymmetric extraction (blocking competitors through patent assertions). The patent office perspective (Piton) reveals institutional degradation: the examination function is performative, quality is low, but the office persists through inertia. The analytical observer perspective (false Mountain) naturalizes a contingent regime, claiming that patent protection is inherent to innovation incentives — but historical evidence (Unix, WWW, Linux, cloud infrastructure) developed without strong patent protection contradicts this claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are determined by the agent's structural position relative to the enforcement mechanism. Patent holders and litigation firms have low d values (high beneficiary position, arbitrage exit) — they experience negative or very low effective extraction because the regime flows toward them. Large technology companies have moderate d values (some benefit, some cost, constrained exit) — they experience mixed extraction. Mid-size companies have slightly higher d values (more costs than benefits, higher suppression) — they experience moderate extraction. Independent developers and open-source communities have high d values (pure victim status, trapped exit) — they experience maximum extraction. The patent office has a moderate d value (constrained exit, mixed experience) — it sees the regime as degraded but cannot exit. The sigmoid function f(d) maps these to experienced extractiveness chi. Beneficiaries with low d get low/negative chi; victims with high d get high chi. The analytical observer's d is set to neutral (0.72) per canonical fallback, reflecting that the observer claims no structural stake.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by revealing that the 'correct' type depends on the structural position of the observer. From the beneficiary's view, the regime is Rope (pure coordination). From the victim's view, it is Snare (pure extraction). From the large-company view, it is Tangled Rope (mixed). No single type is wrong — each captures a real aspect of the regime from a specific structural position. The false Mountain perspective (analytical observer naturalizing the regime as immutable) is correctly flagged as a false summit: the regime is contingent, not inherent. The mandatrophy is resolved by recognizing that the presheaf of perspectives over the constraint site — not a single type — captures the full structure. The increasing extractiveness over the measurement interval (0.35 → 0.58) indicates that the regime has drifted from coordination-dominant to extraction-dominant, which explains why Rope perspectives are less defensible over time.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    patent_quality_measurement,
    'Can software patent quality be measured independently, or is perceived quality always relative to enforcement incentives?',
    'Longitudinal analysis of software patents: claim clarity, prior art coverage, examination time, and post-issuance invalidation rates. Correlation with enforcement trends.',
    'If quality is measurable and declining: extraction is increasing (suppression through low-quality patent proliferation). If quality assessment is inherently captured by enforcement actors: theater is higher than measured (0.68 → 0.75+).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_quality_measurement, empirical, 'Whether software patent quality can be measured independently').

omega_variable(
    innovation_attribution,
    'How much of software innovation is attributable to patent incentives vs. other drivers (reputation, skill development, platform lock-in, network effects)?',
    'Historical analysis of open-source innovation rates vs. patent-intensive sectors; surveys of developer motivation; econometric attribution studies.',
    'If patents contribute <20% of innovation incentive: classification shifts toward snare (extraction without coordination benefit). If patents contribute >40%: coordination function is stronger, classification tilts toward rope for large actors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_attribution, empirical, 'Attribution of software innovation to patent incentives').

omega_variable(
    enforcement_cascade_risk,
    'Does patent enforcement create a self-reinforcing cycle where litigation threats drive frivolous assertions, which then require more litigation to defend against?',
    'Time-series analysis of patent assertions vs. invalidation rates; correlation between litigation volumes and subsequent assertion filings; case complexity trends.',
    'If cascade exists: suppression and theater increase over time (measurements should show rising values). Extraction mechanism is self-sustaining. If no cascade: enforcement is more random, less systematic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_cascade_risk, empirical, 'Whether patent enforcement creates self-reinforcing litigation cascade').

omega_variable(
    jurisdictional_arbitrage,
    'Do different patent enforcement regimes (US vs. EU vs. Asia) create systemic arbitrage opportunities that benefit only the most mobile (large corporates) and hurt the immobile (open source, developing-world developers)?',
    'Mapping of enforcement action by jurisdiction; correlation with developer mobility; case studies of cross-border enforcement outcomes.',
    'If strong arbitrage: suppression is asymmetric (high for immobile, low for mobile actors). Classification remains tangled_rope for global-scope perspectives but snare for locally-trapped actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_arbitrage, empirical, 'Whether enforcement regime differences enable jurisdictional arbitrage').

omega_variable(
    open_source_defensibility,
    'Are open-source projects actually defensible against patent assertion, or is the ''collective defense'' narrative overstated?',
    'Analysis of open-source patent defense fund outcomes; litigation success rates; correlation between fund participation and enforcement action avoidance.',
    'If defensible: victims have more exit options than measured (trapped → constrained). Classification shifts toward rope or scaffold. If not defensible: classification holds or worsens toward snare, and theater increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_defensibility, empirical, 'Whether open-source projects can defend against patent assertions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_patent_enforcement, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(swpat_tr_t0, software_patent_enforcement, theater_ratio, 0, 0.52).
narrative_ontology:measurement(swpat_tr_t10, software_patent_enforcement, theater_ratio, 10, 0.62).
narrative_ontology:measurement(swpat_tr_t20, software_patent_enforcement, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(swpat_be_t0, software_patent_enforcement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(swpat_be_t10, software_patent_enforcement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(swpat_be_t20, software_patent_enforcement, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_patent_enforcement, enforcement_mechanism).
narrative_ontology:affects_constraint(software_patent_enforcement, software_innovation_velocity).
narrative_ontology:affects_constraint(software_patent_enforcement, open_source_sustainability).
narrative_ontology:affects_constraint(software_patent_enforcement, technology_access_inequality).

% DUAL FORMULATION NOTE:
% Software patent enforcement is upstream of constraints on innovation velocity (the regime slows development through litigation threats and licensing requirements), open-source sustainability (the regime imposes asymmetric risks on unpaid volunteer projects), and technology access inequality (the regime concentrates patent-related costs on less-resourced developers and less-resourced geographies). Decomposition: the enforcement regime itself (this story) is distinct from the downstream effects on innovation, sustainability, and access.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(software_patent_enforcement, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
