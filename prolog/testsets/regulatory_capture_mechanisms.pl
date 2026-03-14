% ============================================================================
% CONSTRAINT STORY: regulatory_capture_mechanisms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_regulatory_capture_mechanisms, []).

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
 *   constraint_id: regulatory_capture_mechanisms
 *   human_readable: Regulatory Capture: Institutional Extraction Through Industry Influence
 *   domain: political_economy/institutional_dynamics
 *
 * SUMMARY:
 *   Regulatory capture occurs when regulated industries exercise sufficient
 *   influence over the agencies designed to regulate them that the agencies
 *   systematically advance industry interests instead of public welfare. The
 *   constraint embodies a fundamental tension: regulators genuinely need
 *   technical expertise, which concentrates in industry; industry supply of
 *   that expertise creates structural leverage; leverage converts to policy
 *   preference satisfaction; and the regulatory framework's legitimacy
 *   persists despite its hollowed-out function. This constraint exhibits all
 *   six DR types across different institutional positions. The mechanism
 *   operates through multiple channels: revolving-door employment creating
 *   career incentives for regulator-to-industry transitions, lobbying giving
 *   industry structural access to rule-making, technical complexity
 *   concentrating expertise and information asymmetry, and regulatory
 *   discretion enabling preference satisfaction without explicit quid pro
 *   quo. The constraint's extractiveness has risen from 0.35 (moderate
 *   asymmetry in the 1980s) to 0.58 (high asymmetry in recent decades) as
 *   regulatory scope expanded and technical complexity deepened. Theater has
 *   simultaneously risen from 0.48 to 0.68, indicating that performative
 *   compliance (public consultation, environmental review, safety
 *   certification) has increased while substantive independence has declined.
 *
 * KEY AGENTS:
 *   - Regulated Industry: Primary beneficiary (institutional/arbitrage) — captures regulatory preferences through expertise provision, lobbying, and revolving-door recruitment
 *   - Public Welfare (Consumers, Environment, Safety): Primary victim (powerless/trapped) — abstract collective good without concentrated benefits to enable organization; bears extraction as regulatory degradation without exit
 *   - Regulatory Agency: Institutional actor (institutional/constrained) — structurally dependent on industry expertise; experiences capture as institutional inertia (piton perspective); original mandate (public protection) has atrophied
 *   - Advocacy Coalition (Environmental, Consumer, Safety NGOs): Secondary victim (organized/constrained) — organized enough to perceive capture but resource-constrained and politically marginalized; benefit from regulatory framework's existence while bearing extraction
 *   - Transparency and Accountability Movement: Reform agents (organized/mobile) — investigative journalists, reform advocates, international bodies building exit routes through disclosure mandates and cross-jurisdictional competition
 *   - Analytical Observer: Civilizational economic view (analytical/analytical) — risks naturalizing capture as immutable institutional economics outcome rather than contingent arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(regulatory_capture_mechanisms, 0.58).
domain_priors:suppression_score(regulatory_capture_mechanisms, 0.62).
domain_priors:theater_ratio(regulatory_capture_mechanisms, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(regulatory_capture_mechanisms, extractiveness, 0.58).
narrative_ontology:constraint_metric(regulatory_capture_mechanisms, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(regulatory_capture_mechanisms, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(regulatory_capture_mechanisms, tangled_rope).
narrative_ontology:human_readable(regulatory_capture_mechanisms, "Regulatory Capture: Institutional Extraction Through Industry Influence").
narrative_ontology:topic_domain(regulatory_capture_mechanisms, "political_economy/institutional_dynamics").

domain_priors:requires_active_enforcement(regulatory_capture_mechanisms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(regulatory_capture_mechanisms, regulated_industry).
narrative_ontology:constraint_beneficiary(regulatory_capture_mechanisms, capture_facilitators).
narrative_ontology:constraint_victim(regulatory_capture_mechanisms, public_welfare).
narrative_ontology:constraint_victim(regulatory_capture_mechanisms, market_competition).
narrative_ontology:constraint_victim(regulatory_capture_mechanisms, consumer_safety).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED CONSUMER (SNARE) — Trapped in a market where regulatory protections have been hollowed out through industry influence. Cannot organize, has no information asymmetry reduction, no exit option. Bears full cost of capture: price inflation, safety degradation, market foreclosure. No coordination benefit visible to this agent.
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGULATED INDUSTRY (ROPE) — Experiences regulatory capture as pure coordination benefit. The industry captures regulators through revolving door, lobbying, and technical expertise provision. From the industry's viewpoint, this is solving a coordination problem: translating technical complexity into policy coherence. Net beneficiary experiencing effective extraction in reverse (subsidy flow).
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: ADVOCACY COALITION (TANGLED ROPE) — Environmental groups, consumer advocates, safety-focused NGOs are organized enough to perceive and resist capture, but constrained by funding and political access asymmetries. They benefit from the regulatory framework's existence (it gives them institutional leverage points) while bearing extraction costs (their goals are systematically deprioritized). Mixed experience of both coordination (framework exists) and asymmetric extraction (framework is corrupted).
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSPARENCY AND ACCOUNTABILITY MOVEMENT (SCAFFOLD) — Organized agents (investigative journalists, reform advocates, international standards bodies) are building escape routes through transparency mandates, conflict-of-interest disclosure, and cross-jurisdictional regulatory competition. These have sunset characteristics: if transparency norms mature, capture becomes more costly to maintain. Agents with global scope and generational horizon see temporary scaffolding, not permanent extraction.
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AGENCY (PITON) — The agency has become the regulated industry's institutional proxy. Its original mandate (protect public welfare) has atrophied; it now performs the ritual of regulation while executing industry preferences. Theater ratio is high: public hearings, environmental reviews, safety certifications all proceed with performative rigor while substantive conclusions are predetermined. The agency persists through institutional inertia and legal authority, but its functional independence is degraded.
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL ECONOMICS VIEW (MOUNTAIN) — From a civilizational economic perspective, regulatory capture may appear as an immutable consequence of information asymmetry and concentrated interests: complex regulations favor technical specialists, technical specialists cluster in industry, industry supplies the expertise regulators depend on, and capture follows mechanically. This natural law view naturalizes what is actually a contingent institutional arrangement. Engine will detect this as a false summit.
constraint_indexing:constraint_classification(regulatory_capture_mechanisms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(regulatory_capture_mechanisms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(regulatory_capture_mechanisms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(regulatory_capture_mechanisms, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(regulatory_capture_mechanisms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(regulatory_capture_mechanisms, TR),
    TR >= 0.70.

:- end_tests(regulatory_capture_mechanisms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, rising over time. The constraint extracts from consumers and public welfare (benefit concentrated in regulated industry, costs dispersed across society) but not at maximum intensity — some regulatory functions persist (baseline safety standards), and industry capture is incomplete (advocacy coalition retains some influence). The 0.35→0.58 trajectory reflects deepening capture as technical complexity increased and industry expertise concentration solidified. Suppression (0.62): High. Barriers to escape the capture constraint include: information asymmetry (industry has technical knowledge, public does not), structural dependence (regulators need industry expertise to write coherent rules), career incentives (regulator to industry employment is lucrative), and political concentration of power (industry has lobbying budget and sustained access; consumers are diffuse and episodically attentive). Consumer exit from regulated markets is often impossible (utilities, essential services), so suppression is structural, not merely economic. Theater ratio (0.68): High and rising. Public participation in rule-making, environmental and safety reviews, and transparency reporting are substantially performative — conclusions are often predetermined through industry channels before public processes formally begin. Theater has increased (0.48→0.68) as regulatory complexity grew and formal process requirements expanded; the appearance of deliberation has intensified while substantive independence has declined.
 *
 * PERSPECTIVAL GAP:
 *   The gap between Rope and Snare perspectives is the diagnostic core: the same constraint appears as coordination to the beneficiary but pure extraction to the victim. The gap reveals that capture's mechanism is not symmetric (win-win coordination) but asymmetric (subsidy flow to industry, cost flow to public). The scaffold perspective (transparency reform) provides a resolution path: if disclosure mandates and jurisdictional competition can reduce the information asymmetry and increase the costs of capture, both the extraction magnitude and the perspectival gap should shrink. The piton perspective reveals institutional inertia: the regulatory agency's original function (public protection) has been performatively maintained even as substantive independence degraded. The mountain perspective is a false summit: the framing of capture as an unavoidable consequence of technical complexity naturalizes what is actually a contingent institutional arrangement — jurisdictions with strong conflict-of-interest law show demonstrably lower capture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position: whether they benefit or bear costs from the extraction, and whether they have exit options. The regulated industry benefits (beneficiary + arbitrage exit = low d = negative experienced extraction = subsidy flow). The regulatory agency is structurally dependent (constrained exit due to legal mandate + victim status of being captured = moderate-high d). Advocacy coalitions are organized but resource-constrained (organized power + constrained exit = moderate d). Consumers are powerless and trapped (no exit + full victim status = maximum d = maximum experienced extraction). The analytical observer has no structural position in the extraction (analytical power + analytical exit = neutral d). The directionality chain produces a perspectival gap: the beneficiary perceives low extraction (subsidized coordination); the victim perceives high extraction (trapped). This gap is precisely what captures in the classification divergence between Rope (industry perspective) and Snare (consumer perspective).
 *
 * MANDATROPHY ANALYSIS:
 *   Regulatory capture resolves the mandatrophy through the perspectival presheaf structure. The constraint is NOT 'regulatory capture is a rope' or 'regulatory capture is a snare' — it is 'regulatory capture is a rope from the beneficiary's perspective, a snare from the victim's perspective, a piton from the agency's perspective, and a scaffold from the reform movement's perspective.' The analytically correct answer is the presheaf over all positions, not any single type. The false summit at the mountain perspective (institutional economics view) is diagnostically important: it reveals the naturalizing move that captures make. The constraint's resolution depends not on settling a classification dispute but on structural reform (transparency, conflict-of-interest restrictions, rotation requirements) that changes the extractiveness and suppression metrics themselves, collapsing some of the perspectival gap. The mandatrophy is resolved by recognizing that classification divergence, properly interpreted, reveals institutional capture rather than ambiguity in the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_vs_legitimate_expertise,
    'Does industry influence on regulation represent illicit capture or legitimate provision of technical expertise that regulators need?',
    'Comparative analysis: jurisdictions with strong conflict-of-interest barriers vs. permissive revolving doors; outcomes (safety, innovation, market competitiveness) by jurisdiction; cost-benefit studies controlling for technical complexity',
    'If capture is illicit: high extractiveness, snare classification dominates. If industry expertise is necessary: lower extractiveness, rope classification justified. Distinction determines whether suppression is structural vs. institutional inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_vs_legitimate_expertise, empirical, 'Whether industry influence is capture or legitimate expertise provision').

omega_variable(
    opacity_as_mechanism,
    'Is regulatory opacity (closed meetings, delayed disclosure, technical jargon) a necessary feature of technical governance or a capture mechanism that obscures extraction?',
    'Natural experiments with transparency interventions (sunshine laws, real-time disclosure, plain-language mandates); measurement of capture persistence after transparency increases; analysis of what information asymmetries persist despite disclosure',
    'If opacity is necessary: theater_ratio is legitimate coordination cost. If opacity enables capture: theater_ratio reflects extraction hiding, and transparency can meaningfully reduce suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(opacity_as_mechanism, empirical, 'Whether opacity is necessary technical governance or capture mechanism').

omega_variable(
    revolving_door_causality,
    'Does the revolving door between industry and regulators cause capture, or do captured policies attract industry-aligned regulators (selection effect)?',
    'Longitudinal tracking of individual regulator trajectories; comparison of regulatory positions before vs. after industry employment; instrumental variable analysis using exogenous policy shocks; cross-country variation in revolving-door restrictions and capture outcomes',
    'If revolving door causes capture: restricting it is a high-impact reform. If selection effect dominates: institutional recruitment reform and ideological purge become necessary. Causality direction determines efficacy of scaffold reforms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revolving_door_causality, empirical, 'Whether revolving door causes capture or reflects selection').

omega_variable(
    consumer_collective_action_threshold,
    'At what level of extractiveness does consumer harm exceed coordination costs and enable collective action against capture?',
    'Historical case analysis of successful vs. failed capture reversal (tobacco, environmental, financial regulations); measurement of consumer mobilization thresholds; correlation between visible harm concentration and political viability of anti-capture reform',
    'If threshold is low: powerless agents can organize into powerful coalitions, downgrading snare to tangled_rope. If threshold is high: consumers remain trapped absent elite defection. Threshold determines likelihood of scaffold dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_collective_action_threshold, empirical, 'Consumer mobilization threshold against regulatory capture').

omega_variable(
    jurisdictional_regulatory_arbitrage,
    'Do inter-jurisdictional differences in capture intensity drive competitive pressure toward reform (good regulatory governance as equilibrium selection) or jurisdictional shopping that amplifies capture (race to the bottom)?',
    'Analysis of firm location decisions and regulatory regime differences; measurement of capital flight to low-regulation jurisdictions; political economy of international harmonization; case studies of regulatory races to the top vs. bottom',
    'If competition drives reform: scaffold perspective is empirically grounded, and international coordination strengthens it. If arbitrage amplifies capture: global perspective shows capture spreading rather than scaffolding — classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_regulatory_arbitrage, empirical, 'Whether jurisdictional competition reforms or amplifies capture').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(regulatory_capture_mechanisms, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regcap_tr_t0, regulatory_capture_mechanisms, theater_ratio, 0, 0.48).
narrative_ontology:measurement(regcap_tr_t15, regulatory_capture_mechanisms, theater_ratio, 15, 0.6).
narrative_ontology:measurement(regcap_tr_t30, regulatory_capture_mechanisms, theater_ratio, 30, 0.68).
narrative_ontology:measurement(regcap_tr_t45, regulatory_capture_mechanisms, theater_ratio, 45, 0.75).

% Extraction over time
narrative_ontology:measurement(regcap_be_t0, regulatory_capture_mechanisms, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regcap_be_t15, regulatory_capture_mechanisms, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(regcap_be_t30, regulatory_capture_mechanisms, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(regcap_be_t45, regulatory_capture_mechanisms, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(regulatory_capture_mechanisms, enforcement_mechanism).
narrative_ontology:affects_constraint(regulatory_capture_mechanisms, information_asymmetry_in_markets).
narrative_ontology:affects_constraint(regulatory_capture_mechanisms, revolving_door_career_incentives).
narrative_ontology:affects_constraint(regulatory_capture_mechanisms, lobbying_access_asymmetry).

% DUAL FORMULATION NOTE:
% Regulatory capture is the upstream constraint affecting specific sectoral capture mechanisms (financial regulation capture, environmental regulation capture, labor regulation capture, etc.). Each sectoral story has its own extractiveness value reflecting domain-specific capture intensity; the general regulatory capture mechanisms story is the abstract structural pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(regulatory_capture_mechanisms, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
