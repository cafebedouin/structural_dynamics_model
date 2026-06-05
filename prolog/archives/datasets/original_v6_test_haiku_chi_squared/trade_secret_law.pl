% ============================================================================
% CONSTRAINT STORY: trade_secret_law
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_trade_secret_law, []).

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
 *   constraint_id: trade_secret_law
 *   human_readable: Trade Secret Law (Information Ownership)
 *   domain: legal/economic
 *
 * SUMMARY:
 *   Trade Secret Law creates a structural tension between two legitimate
 *   coordination functions and an extractive mechanism. The legitimate
 *   coordination function: firms need to protect R&D investment to justify
 *   the risk and capital required for innovation. Sharing technical details
 *   with partners requires legal assurance that information will not leak.
 *   This is genuine coordination — solving the problem of enabling beneficial
 *   information sharing while preventing free-riding. The extractive
 *   mechanism: once a firm has invested and built a dominant position, trade
 *   secret law becomes a tool to suppress competition, restrict employee
 *   mobility, and maintain artificial scarcity around knowledge. The
 *   constraint exhibits a clear tangled rope structure: it possesses BOTH a
 *   real coordination function AND asymmetric extraction, with the tension
 *   between them evolving over time. Over the 60-year interval examined (from
 *   early patent/trade-secret equilibrium to current open-source challenge),
 *   base extractiveness has increased from 0.28 to 0.52, while theater_ratio
 *   has increased from 0.38 to 0.55. The rising theater ratio (Goodhart
 *   drift) reflects that enforcement rituals increasingly consume resources
 *   without producing innovation, as trade secrets outlive their coordination
 *   purpose and harden into incumbent protection.
 *
 * KEY AGENTS:
 *   - Incumbent Technology Firms: Primary beneficiary (institutional/arbitrage) — use trade secret law to protect R&D investment and maintain market dominance
 *   - Knowledge Workers (Employees/Contractors): Primary victim (powerless/trapped) — cannot freely apply skills; constrained by NDAs and non-compete agreements; fear of litigation limits mobility
 *   - Competing Innovators/Startups: Secondary victim (moderate/constrained) — cannot access accumulated knowledge base; face litigation threats; hiring constrained by fear of misappropriation claims
 *   - Academic Research Community: Secondary actor (organized/constrained) — benefits from research carve-outs but suppressed by publication delays and disclosure restrictions
 *   - Large Patent Holders: Tertiary beneficiary (powerful/mobile) — use trade secrets strategically alongside patents; have negotiation power and selective disclosure options
 *   - Trade Secret Enforcement Apparatus: Institutional actor (institutional/constrained) — courts, legislatures, IP enforcement bodies; maintains regime through institutional inertia despite functional degradation
 *   - Open Source / Open Science Movement: Organized agent (organized/mobile) — building alternative coordination mechanisms; sees trade secret law as temporary protection layer with sunset trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(trade_secret_law, 0.52).
domain_priors:suppression_score(trade_secret_law, 0.68).
domain_priors:theater_ratio(trade_secret_law, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(trade_secret_law, extractiveness, 0.52).
narrative_ontology:constraint_metric(trade_secret_law, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(trade_secret_law, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(trade_secret_law, tangled_rope).
narrative_ontology:human_readable(trade_secret_law, "Trade Secret Law (Information Ownership)").
narrative_ontology:topic_domain(trade_secret_law, "legal/economic").

domain_priors:requires_active_enforcement(trade_secret_law).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(trade_secret_law, incumbent_firms).
narrative_ontology:constraint_beneficiary(trade_secret_law, intellectual_property_holders).
narrative_ontology:constraint_victim(trade_secret_law, competing_innovators).
narrative_ontology:constraint_victim(trade_secret_law, employee_mobility).
narrative_ontology:constraint_victim(trade_secret_law, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: KNOWLEDGE WORKER (SNARE) — Employee or contractor cannot freely apply learned skills without legal risk of trade secret misappropriation claims. Exit is blocked by non-compete agreements, non-disclosure agreements, and fear of litigation. Trapped within a single employer's ecosystem. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(trade_secret_law, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING STARTUP (SNARE) — New entrant cannot access the accumulated knowledge base that incumbents have legally protected. Constrained by discovery restrictions, litigation threats, and inability to hire talent with relevant experience. d≈0.88, f(d)≈1.28, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(trade_secret_law, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ACADEMIC RESEARCH COMMUNITY (TANGLED ROPE) — Universities and researchers benefit from trade secret carve-outs for research purposes (reverse engineering, independent discovery for published research). This creates a genuine coordination function: protecting firm knowledge while preserving academic freedom. But suppression is asymmetric — industry can sue academics for publication delays or information access; academics have limited reciprocal claims. d≈0.58, f(d)≈0.78, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT TECHNOLOGY FIRM (ROPE) — Primary beneficiary. Trade secret law enables coordination of R&D investment: firms can invest in innovation because they can control information dissemination and capture returns. Also enables partners to share sensitive technical details via NDAs. The constraint solves a genuine coordination problem: preventing free-riding on innovation while enabling beneficial partnership. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary with minimal effective extraction cost.
constraint_indexing:constraint_classification(trade_secret_law, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LARGE PATENT HOLDER (TANGLED ROPE) — Multinational firms can use trade secrets strategically alongside patents, creating layered IP defense. Benefits from the coordination function (protecting investment) while extracting from competitors who cannot access foundational knowledge. Has some mobility: can license, partner, or share selectively. d≈0.35, f(d)≈0.38, σ=1.2 → χ≈0.19. Low-to-moderate extraction because this agent has structural power to negotiate or exit.
constraint_indexing:constraint_classification(trade_secret_law, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADE SECRET ENFORCEMENT APPARATUS (PITON) — Courts, legislatures, and IP enforcement bodies maintain trade secret law through institutional inertia. The primary function (protecting legitimate R&D investment) has been displaced by secondary uses (maintaining market dominance, restricting mobility, suppressing competing innovation). Theater_ratio=0.55 reflects moderate performativity: enforcement rituals (discovery disputes, protective orders, expert testimony) consume resources without producing innovation. The apparatus persists because it is institutionally entrenched and because some coordination function remains real, but the functional degradation is evident in the gap between stated purpose (protect innovation investment) and actual effect (entrench incumbents).
constraint_indexing:constraint_classification(trade_secret_law, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN SOURCE / OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized coalition building alternative mechanisms for knowledge coordination and innovation incentives. Open-source software, open science norms, and data commons provide legitimate alternatives to trade secret protection. This perspective sees trade secret law as a temporary protection layer for a specific type of innovation (capital-intensive, centralized R&D) but not for all knowledge work. Sunset clause present: as distributed innovation platforms and open-source models mature, the extractive function of trade secrets weakens. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.22. Effective extraction is low because this actor sees and is building an exit path.
constraint_indexing:constraint_classification(trade_secret_law, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, information asymmetry is an inherent feature of knowledge: once information is transmitted, the sender cannot erase it from the recipient's mind. Trade secret law attempts to create an artificial scarcity around inherently abundant information. This perspective risks naturalizing what is a contingent legal choice. However, the structural data (ε=0.52, suppression=0.68, theater=0.55) contradicts the mountain classification — the engine's false summit detector reveals this as a naturalization of contingent institutional design.
constraint_indexing:constraint_classification(trade_secret_law, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(trade_secret_law_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(trade_secret_law, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(trade_secret_law, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(trade_secret_law, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(trade_secret_law, TR),
    TR >= 0.70.

:- end_tests(trade_secret_law_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Trade secret law creates genuine extraction from workers and competitors, but the extraction is not absolute like a slavery/debt trap snare would be. Workers can change careers or employers; startups can license, partner, or independently discover. The 0.52 value reflects that extraction is significant and costly but not total. The rising trajectory from 0.28 to 0.52 over 60 years indicates that incumbent firms have progressively layered trade secret claims onto their dominance, expanding the extraction mechanism beyond its original coordination purpose. Suppression (0.68): High. Significant barriers to exit include: (1) risk of litigation; (2) non-compete and non-disclosure agreements that are enforceable in most US jurisdictions; (3) information asymmetry (workers don't know what they're legally forbidden from using); (4) knowledge irreversibility (once learned, tacit knowledge is hard to erase); (5) discovery rules that can expose confidential business information during litigation. Theater ratio (0.55): Moderate. Trade secret enforcement involves procedural theater: protective orders, expert testimony, confidential depositions, discovery disputes. But the ratio is not as high as patent enforcement because trade secrets are harder to litigate (no public record makes proof difficult) and settlements are common. The rising trajectory reflects that as enforcement has become more formalized and proceduralized, theater has increased without corresponding innovation benefit.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates an extreme perspectival gap because the base properties (moderate-high extraction + high suppression) produce qualitatively different classifications across observation sites. The knowledge worker sees pure extraction (Snare) — they are trapped and cannot exit. The incumbent firm sees pure coordination (Rope) — they solve a genuine problem of enabling beneficial information sharing while preventing free-riding. The competing startup sees extraction (Snare) — they are blocked from accessing knowledge. The academic researcher sees mixed coordination and extraction (Tangled Rope) — they benefit from research carve-outs but are suppressed by publication restrictions. The enforcement apparatus sees its own degraded function (Piton) — the original purpose (coordinate innovation investment) has been displaced by secondary use (maintain dominance), and theater has risen. The open-source movement sees a temporary problem being solved by alternatives (Scaffold) — distributed innovation, open-source licensing, and open science norms are building exits. The analytical observer risks seeing an immutable scarcity of information (Mountain) — but the structural data reveals this as a naturalization of a contingent legal choice. The perspectival gap is diagnostic: it shows that trade secret law's extractive function is not equally distributed across observation sites. Incumbents and workers experience opposite realities from the same constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Knowledge workers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Competing startups: Victim + constrained → d≈0.88, f(d)≈1.28. High extraction. Academic researchers: Mixed beneficiary/victim + constrained → d≈0.58, f(d)≈0.78. Moderate extraction (they have some legal protections via research carve-outs). Incumbent firms: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with minimal extraction cost. Large patent holders: Beneficiary + mobile → d≈0.35, f(d)≈0.38. Low extraction (they have power to negotiate or license selectively). Enforcement apparatus: Institutional + constrained → d≈0.45, f(d)≈0.53. Moderate. The apparatus is institutionally constrained (must enforce existing law) but is also somewhat complicit in the beneficiary's interest (institutional bias toward IP protection). Open-source movement: Organized + mobile → d≈0.42, f(d)≈0.42. Low extraction because this actor sees and is actively building an exit path.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that trade secret law is NOT a pure extraction mechanism (Snare) despite high suppression, and is NOT a pure coordination mechanism (Rope) despite real coordination benefits. It is a genuine Tangled Rope: it solves a real coordination problem (enabling firms to protect R&D investment and share sensitive information with partners) while simultaneously creating asymmetric extraction (workers cannot exit, startups cannot compete, knowledge commons is suppressed). The mandatrophy resolution requires explicitly acknowledging BOTH functions. The knowledge worker and startup perspectives see only the extraction because they are victims. The incumbent firm perspective sees only the coordination because they are beneficiaries. The analytical observer at a civilizational scale risks naturalizing the coordination function as inherent to innovation (false Mountain) while ignoring the contingent institutional design that creates the extraction. The tangled rope classification is correct because: (1) Beneficiaries are present (incumbent firms) and derive real coordination benefit. (2) Victims are present (workers, startups, knowledge commons) and suffer real extraction. (3) Active enforcement is required (courts must interpret misappropriation, apply protective orders, award damages). (4) Both functions are structural, not accidental. The rising extractiveness (0.28→0.52) and theater (0.38→0.55) over 60 years indicate that the coordination function has become subordinate to the extraction function — the constraint is degrading toward Snare. The scaffold perspective (open-source alternatives) offers a genuine exit path that could allow the constraint to sunset without eliminating innovation incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reverse_engineering_boundary,
    'What constitutes legitimate reverse engineering versus misappropriation? Where is the line between independent discovery and derived knowledge?',
    'Comparative analysis of reverse engineering cases; documentation of discovery pathways for independently-derived secrets; expert technical assessment of knowledge transfer feasibility',
    'If boundary is permeable: knowledge workers and competitors have meaningful exit options (mobile/analytical). If boundary is tight: exit options collapse to trapped/constrained. Classification shifts snare ↔ tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reverse_engineering_boundary, conceptual, 'Boundary between reverse engineering and misappropriation').

omega_variable(
    innovation_incentive_threshold,
    'What level of trade secret protection is actually required to incentivize innovation? Is the current enforcement level below, at, or above optimal?',
    'Empirical comparison of R&D investment and innovation rates across jurisdictions with different trade secret regimes; analysis of patent vs trade secret strategy adoption; startup creation and exit rates correlated with trade secret enforcement stringency',
    'If current > optimal: extraction is unnecessary and constitutes rent-seeking (Snare classification confirmed). If current < optimal: weak enforcement harms innovation (Rope classification correct). If current ≈ optimal: tangled rope classification confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(innovation_incentive_threshold, empirical, 'Whether current trade secret protection exceeds innovation incentive requirements').

omega_variable(
    knowledge_spillover_velocity,
    'How quickly do trade secrets become obsolete or are independently discovered by competitors? What is the effective protection window?',
    'Time-series analysis of firm competitive advantage duration; correlation of trade secret age with patent filing or public disclosure; survey of firms on secret-to-public knowledge timeline',
    'If window < 3 years: suppression has low real effect, constraint degrades to piton. If window > 10 years: suppression is severe, extraction is real (snare confirmed). If window 5-7 years: tangled rope confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(knowledge_spillover_velocity, empirical, 'Duration of competitive advantage from trade secrets before spillover').

omega_variable(
    enforcement_asymmetry_scope,
    'Do large firms use trade secret law as a competitive weapon against smaller competitors at rates exceeding their use for legitimate IP protection?',
    'Analysis of trade secret litigation patterns: initiator size, settlement vs victory rates, damages awarded; comparison of litigation frequency per firm size; survey of general counsel on enforcement strategy',
    'If asymmetry is high: constraint functions primarily as extraction mechanism for large firms (tangled rope with strong beneficiary/victim differentiation confirmed). If low: constraint functions symmetrically (rope classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_asymmetry_scope, empirical, 'Whether trade secret enforcement is asymmetrically biased toward large firms').

omega_variable(
    open_innovation_substitution,
    'Can distributed open-source models and open science norms actually replace trade secret incentives for complex capital-intensive R&D (pharma, semiconductors, aerospace)?',
    'Case study analysis of open-source vs proprietary outcomes in capital-intensive domains; funding model comparison (venture capital vs community funding); innovation rate and quality metrics across open/closed models',
    'If substitution is viable: scaffold perspective is confirmed, sunset clause is real, and alternative institutions can replace trade secret law. If substitution fails in capital-intensive domains: tangled rope persists as necessary evil.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_innovation_substitution, empirical, 'Whether open innovation models can substitute for trade secret protection in capital-intensive R&D').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(trade_secret_law, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(trade_secret_tr_t0, trade_secret_law, theater_ratio, 0, 0.38).
narrative_ontology:measurement(trade_secret_tr_t30, trade_secret_law, theater_ratio, 30, 0.47).
narrative_ontology:measurement(trade_secret_tr_t60, trade_secret_law, theater_ratio, 60, 0.55).

% Extraction over time
narrative_ontology:measurement(trade_secret_be_t0, trade_secret_law, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(trade_secret_be_t30, trade_secret_law, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(trade_secret_be_t60, trade_secret_law, base_extractiveness, 60, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(trade_secret_law, resource_allocation).
narrative_ontology:affects_constraint(trade_secret_law, patent_law).
narrative_ontology:affects_constraint(trade_secret_law, employee_non_compete_agreements).
narrative_ontology:affects_constraint(trade_secret_law, intellectual_property_enforcement).
narrative_ontology:affects_constraint(trade_secret_law, knowledge_commons_access).

% DUAL FORMULATION NOTE:
% Trade secret law constitutes a constraint family with patent law, non-compete agreements, and IP enforcement mechanisms. Trade secrets (ε=0.52, Tangled Rope) differ structurally from patents (which have explicit temporal limits and disclosure requirements) and from non-competes (which explicitly restrict worker mobility). These are distinct constraints linked by network relationships: trade secrets are often used in combination with patents for layered IP defense, and non-competes are frequently enforced alongside trade secret claims. The ε-invariance principle requires separate stories for each because measuring via patent disclosure creates ε≈0.08 (Mountain), while measuring via employee mobility restrictions creates ε≈0.72 (Snare). Trade secret law is the hybrid that bridges both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(trade_secret_law, organized, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
