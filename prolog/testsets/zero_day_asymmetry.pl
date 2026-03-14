% ============================================================================
% CONSTRAINT STORY: zero_day_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_zero_day_asymmetry, []).

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
 *   constraint_id: zero_day_asymmetry
 *   human_readable: Zero Day Asymmetry in Cybersecurity Markets
 *   domain: cybersecurity/information_security/economics
 *
 * SUMMARY:
 *   Zero day asymmetry is a structural constraint in cybersecurity where
 *   vulnerabilities unknown to vendors create exploitable windows during
 *   which attackers have unfettered access while defenders operate without
 *   knowledge or patches. This asymmetry is the core extraction mechanism:
 *   defenders must protect all surfaces indefinitely; attackers need only one
 *   unpatched vulnerability. The constraint exhibits high suppression (actors
 *   have strong incentives to conceal vulnerability knowledge and exploit
 *   windows) and moderate theater ratio (while patch management appears
 *   functional, much of the security theater consists of compliance reporting
 *   and vulnerability scanning that cannot detect zero days by definition).
 *   The extractiveness has increased over the interval as nation state actors
 *   have increasingly weaponized zero days for intelligence and military
 *   operations, and as commercial vulnerability markets have professionalized
 *   the buy-and-hold strategy. The constraint generates different
 *   classifications from different structural positions: users and
 *   administrators experience pure extraction (Snare); organized critical
 *   infrastructure see mixed coordination and extraction (Tangled Rope);
 *   ethical discoverers experience coordination (Rope); state actors are the
 *   primary extractors; and compliance frameworks maintain performative
 *   theater (Piton) without addressing the asymmetry.
 *
 * KEY AGENTS:
 *   - Software Users: Primary victims (powerless/trapped) — experience extraction through compromised systems, data theft, operational disruption with no knowledge of or defense against zero days
 *   - System Administrators: Secondary victims (moderate/constrained) — responsible for protection without information; bear labor and incident costs from exploitation
 *   - Operating System Vendors: Institutional beneficiaries (institutional/arbitrage) — control patch timing, benefit from first-mover knowledge, extract support revenue; also coordinate ecosystem patches
 *   - Vulnerability Discoverers (Ethical): Institutional beneficiaries (institutional/arbitrage) — coordinate responsible disclosure; benefit from reputation and bug bounties; solve coordination problem
 *   - Critical Infrastructure Operators: Organized targets (organized/constrained) — need coordination for collective defense but face deliberately targeted extraction by state actors
 *   - Nation State / Offensive Actors: Primary extractors (powerful/arbitrage) — deliberately maintain zero days for asymmetric advantage; profit from exploitation windows and can choose disclosure timing
 *   - Security Researchers: Organized intermediaries (organized/constrained) — coordinate knowledge production but face extraction as their findings are weaponized
 *   - Compliance Frameworks: Institutional theater (institutional/arbitrage) — patch management requirements persist as performative without addressing zero day problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(zero_day_asymmetry, 0.68).
domain_priors:suppression_score(zero_day_asymmetry, 0.72).
domain_priors:theater_ratio(zero_day_asymmetry, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(zero_day_asymmetry, extractiveness, 0.68).
narrative_ontology:constraint_metric(zero_day_asymmetry, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(zero_day_asymmetry, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(zero_day_asymmetry, snare).
narrative_ontology:human_readable(zero_day_asymmetry, "Zero Day Asymmetry in Cybersecurity Markets").
narrative_ontology:topic_domain(zero_day_asymmetry, "cybersecurity/information_security/economics").

domain_priors:requires_active_enforcement(zero_day_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(zero_day_asymmetry, vulnerability_discoverers).
narrative_ontology:constraint_beneficiary(zero_day_asymmetry, nation_state_actors).
narrative_ontology:constraint_beneficiary(zero_day_asymmetry, offensive_security_contractors).
narrative_ontology:constraint_victim(zero_day_asymmetry, software_users).
narrative_ontology:constraint_victim(zero_day_asymmetry, system_administrators).
narrative_ontology:constraint_victim(zero_day_asymmetry, operating_system_vendors).
narrative_ontology:constraint_victim(zero_day_asymmetry, critical_infrastructure_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNPATCHED USER (SNARE) — Cannot exit the vulnerability window; bears extraction through compromised systems, data theft, operational disruption. Users have no meaningful choice: they cannot know about zero days, cannot patch before vendors know, cannot defend against attacks using unknown exploits. Maximum experienced extraction. The constraint's existence relies on suppressing user knowledge of vulnerabilities and suppressing alternative defensive mechanisms.
constraint_indexing:constraint_classification(zero_day_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM ADMINISTRATOR (SNARE) — Constrained by vendor patch cycles, responsible for critical systems they cannot fully protect. Faces extraction through mandatory vulnerability management labor, incident response costs, and career risk. Can partially mitigate through segmentation and monitoring, but cannot eliminate the structural asymmetry. High experienced extraction due to the constrained exit (bearing responsibility without sufficient information).
constraint_indexing:constraint_classification(zero_day_asymmetry, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CRITICAL INFRASTRUCTURE (TANGLED ROPE) — Organized actors with genuine coordination needs (shared threat intelligence, patch coordination) alongside extraction (targeted by state actors, required to maintain expensive 24/7 response capability). The constraint both solves a coordination problem (how do critical infrastructure operators collaborate on security?) and extracts from them (asymmetric vulnerability window makes them perpetual targets). Cannot exit the internet; can organize collective defense.
constraint_indexing:constraint_classification(zero_day_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: OS VENDORS (TANGLED ROPE) — Coordinate ecosystem through security updates and patch delivery (genuine coordination function). Simultaneously benefit from zero day uncertainty: vendors capture first-mover knowledge, control patch timing, extract premium support revenues. Can arbitrage between responsible disclosure (working with discoverers), vendor-to-vendor information sharing, and sales/support leverage. Mixed extraction and coordination.
constraint_indexing:constraint_classification(zero_day_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: VULNERABILITY DISCOVERERS / ETHICAL (ROPE) — Responsible disclosure practitioners who coordinate the patch ecosystem: they discover vulnerabilities and report to vendors through established channels. Benefit from responsible disclosure norms (reputation, potential bug bounties, career advancement). Experience the constraint as a coordination mechanism: their role is essential to the ecosystem functioning. Net beneficiary through career incentives and professional standing.
constraint_indexing:constraint_classification(zero_day_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATION STATE / OFFENSIVE ACTORS (SNARE from their perspective modified by arbitrage) — Primary extractors. Deliberately maintain zero days for intelligence, espionage, and military advantage. Extract asymmetric capabilities: access to unpatched systems, ability to conduct attribution-resistant attacks. The snare for others becomes pure extraction for state actors — they profit from the vulnerability window and have full arbitrage (can choose when/whether to disclose, sell, or exploit). Classification as snare reflects their role in the extraction mechanism, though their own exit options are arbitrage.
constraint_indexing:constraint_classification(zero_day_asymmetry, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: SECURITY RESEARCHERS (TANGLED ROPE) — Coordinate knowledge production through responsible disclosure frameworks and academic publication. Simultaneously face extraction: career incentives push toward publishable discoveries over quiet vulnerability reports; prestige accrues to those who find exploits; researcher findings are weaponized by state actors. Constrained exit: cannot fully opt out of the vulnerability discovery process without abandoning the field.
constraint_indexing:constraint_classification(zero_day_asymmetry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: COMPLIANCE FRAMEWORKS (PITON) — Patch management requirements, vulnerability disclosure laws, and incident notification regulations persist as institutional theater: organizations conduct vulnerability scanning and patch cycles largely as performative compliance rather than effective defense. The theater persists because alternatives (e.g., formal verification, secure-by-design mandates) are harder to implement and measure. Theater ratio is high for compliance reporting (vulnerability counts, patch compliance percentages) but low for actual risk reduction from zero days.
constraint_indexing:constraint_classification(zero_day_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the zero day asymmetry reflects an immutable feature of adversarial information dynamics: defenders must protect all surfaces indefinitely; attackers need to find only one unpatched vulnerability. This asymmetry is a structural consequence of the attacker-defender imbalance, appearing as a natural law of cybersecurity. However, this perspective risks naturalizing what is contingent: the vulnerability discovery-to-patch window is shaped by institutional choices (disclosure norms, patch cycle timing, vendor incentives) not inherent logical constraints.
constraint_indexing:constraint_classification(zero_day_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zero_day_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zero_day_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zero_day_asymmetry, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(zero_day_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(zero_day_asymmetry, TR),
    TR >= 0.70.

:- end_tests(zero_day_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The vulnerability window is inherently exploitable, and state actors have strong incentives to maintain it (intelligence value, military capability). Commercial zero day markets have professionalized the buy-and-hold strategy, turning individual discoveries into commodities. The measurement trajectory shows extractiveness climbing from 0.52 to 0.68 as offensive capabilities have been industrialized and hoarded. Suppression (0.72): Very high. Multiple suppression mechanisms operate: vulnerability discoverers are incentivized to withhold knowledge (market value, military use); state actors actively conceal zero day stockpiles; vendors suppress information about unpatched vulnerabilities; users and administrators operate without knowledge of threats. The suppression is structural: defenders cannot know what they don't know. Theater ratio (0.35): Moderate-low. Unlike compliance-driven constraints, zero day asymmetry does NOT generate high theater: vulnerability scanning and patch management are genuinely functional activities, they simply cannot detect zero days by definition. The theater that exists (compliance reporting, vulnerability counting) is low because it's addressing a different risk surface than the zero day asymmetry itself.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The user/administrator perspectives see pure extraction (Snare) — they are trapped and cannot exit without full system replacement. The ethical discoverer perspective sees coordination (Rope) — responsible disclosure solves a genuine coordination problem. The vendor perspective sees mixed (Tangled Rope) — they coordinate the ecosystem and benefit simultaneously. The state actor perspective is itself extractive but from the analytical view, these actors are the primary mechanism extractors. The critical infrastructure perspective is Tangled Rope — coordination needs are real (sharing threat intelligence, synchronized patching) but extraction is also real (targeted by sophisticated threats). The compliance framework perspective is Piton — patch management requirements persist as institutional theater despite being unable to address zero days. The analytical/civilizational perspective risks a false summit (Mountain) — naturalizing the attacker-defender asymmetry as inherent to computation rather than recognizing it as a contingent product of industrial decisions around disclosure, hoarding, and patent/secrecy incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect each actor's relationship to the extraction flow. Powerless users have d ≈ 1.0 (maximum target). System administrators have d ≈ 0.75 (target, constrained). Organized critical infrastructure have d ≈ 0.65 (targets, but organized and partially mobile through collective defense). Ethical discoverers and vendors have d ≈ 0.20 (beneficiaries, arbitrage exit). State actors have d ≈ 0.05 (primary beneficiaries, intentional extractors). The chi formula χ = ε × f(d) × σ(S) scales extractiveness by position and scope: a global-scope snare targeting powerless agents has maximum chi; the same constraint at local scope has reduced chi. For the Snare classification to hold, χ ≥ 0.66 must be satisfied from the victim's perspective, which it is: ε=0.68 × f(d=0.95) × σ(global=1.2) ≈ 0.68 × 1.42 × 1.2 ≈ 1.16, compressed to canonical range ≈ 0.85.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that zero day asymmetry operates across multiple distinct mechanisms: (1) Information asymmetry (attackers know vulnerabilities, defenders don't) — this is near-immutable at any fixed moment but subject to disclosure norms and patch speed; (2) Incentive asymmetry (attackers profit from secrecy, defenders don't) — this is contingent on how vulnerability markets are structured and whether states participate; (3) Attacker-defender imbalance (attackers need one success, defenders need perfection) — this is structural to adversarial dynamics but can be mitigated through defense-in-depth; (4) Institutional hoarding (state actors deliberately maintain zero day stockpiles) — this is purely volitional extraction. The Snare classification is valid for the user/administrator perspectives because mechanisms (1) and (3) are nearly immutable, and mechanism (4) is active extraction. The Rope/Tangled Rope classifications for ethical discoverers and vendors are valid because they operate in the coordination space (disclosure timing, patch delivery, responsible communication). The classification is not ambiguous — each perspective is capturing a structurally different relationship to the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vulnerability_window_duration,
    'What vulnerability window duration (time from discovery to patch availability) constitutes legitimate coordination lag versus extractive delay?',
    'Historical analysis of zero day lifespans; comparison between coordinated disclosure timelines and actual industry patch release cycles; correlation between window duration and documented exploitation rates',
    'If legitimate lag ~30 days: most vulnerabilities are coordination delay (more Rope perspectives). If extractive delay ~180 days: asymmetry is deliberately maintained (more Snare perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vulnerability_window_duration, empirical, 'Threshold for distinguishing coordination lag from extractive delay in vulnerability windows').

omega_variable(
    nation_state_disclosure_strategy,
    'What fraction of known zero days are deliberately withheld by state actors versus naturally undiscovered? What is the ratio of exploited-but-unknown to actively-hoarded vulnerabilities?',
    'Intelligence analysis, vendor patch data forensics, CVE repository statistical analysis; comparison of disclosed zero days with government stockpile estimates (where available); longitudinal tracking of zero day exploitation patterns',
    'If ratio of withheld >> undiscovered: zero day asymmetry is primarily structural extraction (Snare confirmed). If ratio comparable: asymmetry is mixed coordination problem and extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nation_state_disclosure_strategy, empirical, 'Proportion of zero days actively withheld by states versus naturally undiscovered').

omega_variable(
    alternative_defense_viability,
    'Can defense-in-depth and anomaly detection substantially mitigate zero day risk without patch knowledge, or is the vulnerability window irreducibly exploitable?',
    'Empirical testing of zero day exploitation success rates against systems with strong EDR, segmentation, and monitoring (no patch knowledge); attack simulation studies; comparison of breach rates between high-monitoring and low-monitoring organizations',
    'If substantial mitigation: zero day window is constraining but not fully extractive (more Tangled Rope). If irreducibly exploitable: asymmetry is fundamental extraction (confirms Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_defense_viability, empirical, 'Whether defense-in-depth can substantially mitigate zero day exploitation').

omega_variable(
    vendor_patch_incentive_structure,
    'Are OS vendors delaying patch releases to maintain customer support revenue, or are delays genuinely required by verification and compatibility testing?',
    'Analysis of patch release timelines for identical vulnerabilities across vendors; correlation between vendor business model and patch speed; comparison of timeline for critical vs low-severity vulnerabilities',
    'If revenue-driven: vendors are intentional extractors (Snare perspective for users shifts to higher chi). If testing-required: delays are legitimate coordination overhead (more Rope/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vendor_patch_incentive_structure, empirical, 'Whether vendor patch delays are financially motivated or technically necessary').

omega_variable(
    responsible_disclosure_effectiveness,
    'Does responsible disclosure (coordinated, time-limited vulnerability reporting) actually reduce zero day exploitation compared to full secrecy or full disclosure?',
    'Comparative analysis of exploitation rates: vulnerabilities reported through responsible disclosure vs. those leaked or independently discovered; measurement of average time-to-exploitation under each disclosure regime',
    'If RD reduces exploitation: it''s functional coordination (Rope/Tangled Rope valid). If exploitation rates are similar: RD is performative theater (Piton perspective valid).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(responsible_disclosure_effectiveness, empirical, 'Whether responsible disclosure reduces zero day exploitation compared to alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zero_day_asymmetry, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zda_tr_t0, zero_day_asymmetry, theater_ratio, 0, 0.28).
narrative_ontology:measurement(zda_tr_t8, zero_day_asymmetry, theater_ratio, 8, 0.31).
narrative_ontology:measurement(zda_tr_t16, zero_day_asymmetry, theater_ratio, 16, 0.35).
narrative_ontology:measurement(zda_tr_t24, zero_day_asymmetry, theater_ratio, 24, 0.35).

% Extraction over time
narrative_ontology:measurement(zda_be_t0, zero_day_asymmetry, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(zda_be_t8, zero_day_asymmetry, base_extractiveness, 8, 0.61).
narrative_ontology:measurement(zda_be_t16, zero_day_asymmetry, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(zda_be_t24, zero_day_asymmetry, base_extractiveness, 24, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(zero_day_asymmetry, enforcement_mechanism).
narrative_ontology:affects_constraint(zero_day_asymmetry, vulnerability_disclosure_norms).
narrative_ontology:affects_constraint(zero_day_asymmetry, software_supply_chain_security).
narrative_ontology:affects_constraint(zero_day_asymmetry, national_cyberwarfare_capability).

% DUAL FORMULATION NOTE:
% Zero day asymmetry can be decomposed into three structurally distinct constraints: (1) information_asymmetry_inherent (ε=0.15, natural limit on knowledge coordination, Mountain), (2) disclosure_incentive_misalignment (ε=0.55, the economic structures that reward withholding, Tangled Rope), and (3) nation_state_zero_day_stockpiling (ε=0.72, the deliberate hoarding of military capability, Snare). This story focuses on the combined observed effect (ε=0.68) but the upstream constraints show where the asymmetry becomes pathological.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(zero_day_asymmetry, powerful, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
