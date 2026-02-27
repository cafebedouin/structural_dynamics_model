% ============================================================================
% CONSTRAINT STORY: lula_hemisphere_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lula_hemisphere_2026, []).

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
 *   constraint_id: lula_hemisphere_2026
 *   human_readable: The Monroe Doctrine Revival (Unilateral US Hegemony)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   The Monroe Doctrine revival represents the reassertion of explicit
 *   unilateral hegemonic control over the Western Hemisphere by the United
 *   States, embodied in the Trump administration's 2021 National Security
 *   Strategy and signaled through military posturing around Venezuela. Lula's
 *   2026 articulation of this constraint in response to threats of US
 *   military intervention reveals the structural extraction mechanism: Latin
 *   American states face pressure to accept US dominance of their region,
 *   constrain their foreign policy alignment (blocking Chinese or Russian
 *   investment/security partnerships), and subordinate their preferences to
 *   US strategic interests. The constraint exhibits high suppression
 *   (military capability asymmetry, CIA operational history, economic
 *   coercion capacity) and moderate-to-high theater (maintained through
 *   diplomatic language about 'partnership' while unilateral military
 *   intervention remains the enforcement mechanism). The theater ratio has
 *   increased from 0.42 to 0.58 over six years as US rhetoric emphasizes
 *   'defending democracy' while enforcement mechanisms remain unilateral
 *   military power. This constraint demonstrates the taxonomy's capacity to
 *   distinguish pure extraction (snare) from coordination (rope) from
 *   degraded coordination (piton) from inevitable natural limits (mountain),
 *   even when examining the same hegemonic system from different structural
 *   positions.
 *
 * KEY AGENTS:
 *   - US Security Establishment: Primary beneficiary (institutional/arbitrage) — maintains unilateral sphere of influence, excludes rival powers, extracts alignment costs from regional states
 *   - Latin American Sovereignty: Primary victim (powerless/trapped) — faces threat of military intervention with no credible exit; forced to constrain foreign policy choices to avoid confrontation
 *   - Brazil (Regional Power): Secondary victim (powerful/constrained) — Lula articulates constraint; has greater agency than smaller states but faces escalation costs that constrain genuine resistance
 *   - Rising Powers (China, Russia, India): Secondary beneficiaries of constraint violation but trapped by countervailing deterrence (tangled rope perspective)
 *   - International Multilateral Order: Institutional actor (institutional/arbitrage) — formal structure (UN, OAS) becomes degraded piton; multilateralism becomes performative while hegemony operates underneath
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing hegemonic extraction as inevitable structural necessity rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lula_hemisphere_2026, 0.68).
domain_priors:suppression_score(lula_hemisphere_2026, 0.72).
domain_priors:theater_ratio(lula_hemisphere_2026, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lula_hemisphere_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(lula_hemisphere_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lula_hemisphere_2026, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lula_hemisphere_2026, snare).
narrative_ontology:human_readable(lula_hemisphere_2026, "The Monroe Doctrine Revival (Unilateral US Hegemony)").
narrative_ontology:topic_domain(lula_hemisphere_2026, "political/international_relations").

domain_priors:requires_active_enforcement(lula_hemisphere_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lula_hemisphere_2026, united_states_security_establishment).
narrative_ontology:constraint_victim(lula_hemisphere_2026, latin_american_sovereignty).
narrative_ontology:constraint_victim(lula_hemisphere_2026, global_multilateral_order).
narrative_ontology:constraint_victim(lula_hemisphere_2026, non_aligned_states).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LATIN AMERICAN SOVEREIGNTY (SNARE) — Regional states face unilateral military intervention threat without credible exit or resistance. Suppression is total: no coalition of regional states can credibly deter US intervention; OAS mechanisms are captured; non-aligned alternatives (ALBA, CELAC) lack enforcement power. d≈0.96, f(d)≈1.43, σ=0.9 → χ≈0.88. This is pure extraction with maximal coercion.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: BRAZIL (MODERATE/CONSTRAINED) — Brazil has greater power than smaller states but faces constrained exit: military intervention costs deter direct resistance, but economic retaliation and diplomatic isolation are credible threats. Lula's articulation of the Monroe Doctrine constraint signals this perception: Brazil sees the constraint but recognizes that confrontation carries unacceptable costs. d≈0.78, f(d)≈1.12, σ=0.9 → χ≈0.61. High effective extraction via threat of escalation.
constraint_indexing:constraint_classification(lula_hemisphere_2026, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US SECURITY ESTABLISHMENT (ROPE) — From the US perspective, the Monroe Doctrine serves coordination: it establishes the hegemon's sphere of influence, clarifies intervention thresholds, and prevents other powers from establishing competing spheres. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.08. Negative effective extraction: the constraint subsidizes US power by establishing unilateral dominance as normal and unchallengeable.
constraint_indexing:constraint_classification(lula_hemisphere_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RISING POWERS (TANGLED ROPE) — India, Indonesia, Nigeria and other powers see the Monroe Doctrine as a template for US-style hegemonic exclusion. The constraint coordinates their response (strengthening multipolarity, building alternative institutions) while extracting costs: the doctrine signals that established hegemons will not permit challengers in their regions. d≈0.52, f(d)≈0.68, σ=1.2 → χ≈0.55. Mixed coordination (unifying opposition) and extraction (establishing precedent for unilateral spheres).
constraint_indexing:constraint_classification(lula_hemisphere_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL LAW & MULTILATERALISM (PITON) — The revival of the Monroe Doctrine reveals multilateralism as a degraded Rope. UN Charter principles (sovereign equality, non-intervention) remain the formal framework, but enforcement is purely theatrical: no mechanism exists to constrain a permanent Security Council member's military actions. The constraint persists through institutional inertia (remaining legalistic discourse, continued invocation of multilateral forums) despite manifest non-enforcement. theater_ratio=0.58 reflects this: formal legal language masks unilateral power. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.03. Low effective extraction because institutional actors can arbitrage between the formal multilateral order and the underlying hegemonic reality.
constraint_indexing:constraint_classification(lula_hemisphere_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN?) — From a civilizational/universal perspective, one might argue that the Monroe Doctrine is an immutable consequence of power asymmetry: any hegemon naturally seeks to exclude competitors from its region; this is inherent to hegemonic stability theory, not a contingent choice. However, the structural data (ε=0.68, suppression=0.72, theater=0.58) contradicts mountain classification. The engine will compute this as a false summit, revealing that the 'hegemony is natural' framing naturalizes what are actually contingent institutional arrangements (US military presence, CIA operations, economic coercion) and political choices (Trump administration's explicit Monroe Doctrine revival). A true natural law would require ε ≤ 0.25 and resistance ≤ 0.15.
constraint_indexing:constraint_classification(lula_hemisphere_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lula_hemisphere_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lula_hemisphere_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lula_hemisphere_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(lula_hemisphere_2026, TR),
    TR >= 0.70.

:- end_tests(lula_hemisphere_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts significant costs from Latin American states: they must limit foreign policy autonomy (blocking security partnerships with China/Russia), accept US-led regional governance (OAS mechanisms), and bear the risk and disruption of military intervention threats. The extraction is not total (some regional policy space remains) but is substantial and rising as US hegemonic rhetoric hardens. Theater ratio (0.58): Moderate-to-high and rising. The constraint is partially maintained through performative mechanisms: diplomatic language about 'partnership' and 'defending democracy,' invocation of OAS consensus (which is actually US-orchestrated), framing military intervention as 'humanitarian' or 'counter-narcotics.' But the underlying enforcement is raw military capability, making the theater ratio moderate rather than high. Suppression (0.72): High. Latin American states face severe barriers to exit: military capability asymmetry (the US spends more on defense than all Latin American states combined), CIA operational capacity and history of coups, economic retaliation tools (sanctions, trade mechanisms, debt manipulation), and lack of credible countervailing power (Chinese/Russian presence is growing but not yet sufficient for credible deterrence). The suppression has increased as US rhetoric explicitly reasserts the Monroe Doctrine (vs. earlier 'equal partnership' language under Obama administration).
 *
 * PERSPECTIVAL GAP:
 *   The Monroe Doctrine constraint exhibits a stark perspectival gap between US beneficiaries and Latin American victims. The US security establishment sees this as a coordination mechanism (Rope) — establishing clear rules about spheres of influence and preventing other powers from gaining regional access. Latin American states see it as pure extraction (Snare) — coercive pressure with no genuine negotiating power. Brazil, as a regional power with some agency, occupies an intermediate position (constrained Tangled Rope) — it can articulate the constraint and seek countervailing partnerships but faces escalation costs. Rising powers see the constraint as a template for the future threat to their own regions (Tangled Rope) — it coordinates their opposition to US hegemony while extracting from them the cost of accepting current US dominance. The international multilateral order (UN, OAS) functions as a degraded piton: the formal structure still invokes multilateral principles, but enforcement is purely theatrical; the real constraint operates through unilateral power. The analytical observer risks naturalizing this as an inevitable consequence of power distribution (Mountain), but the structural data reveals it as a contingent institutional arrangement maintained through active enforcement and theatrical legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   US Security Establishment: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. The constraint subsidizes US power by establishing unilateral dominance as normal and unchallengeable. Latin American states: Victims + trapped → d≈0.96, f(d)≈1.43. Maximum extraction. No credible exit; forced to accept subordinate position. Brazil: Victim + constrained → d≈0.78, f(d)≈1.12. High extraction with partial agency. Can articulate the constraint and seek alternatives, but escalation costs are prohibitive. Rising powers: Mixed position (beneficiary of constraint violation, victim of enforcement threat) → d≈0.52, f(d)≈0.68. Moderate extraction with some agency. Multilateral Order: Institutional + arbitrage (can leverage formal rules while being bypassed by unilateral power) → d≈0.12, f(d)≈0.05. Low effective extraction in piton classification. Analytical observer: d≈0.72, f(d)≈1.15. Mountain classification would be incorrect; the engine's false summit detector prevents naturalizing contingent institutional arrangements.
 *
 * MANDATROPHY ANALYSIS:
 *   The Monroe Doctrine constraint resolves the mandatrophy by distinguishing extraction from coordination through structural position. From the US perspective, it functions as coordination: establishing clear rules (no non-allied powers in Western Hemisphere), enabling predictability, and preventing conflicts with other great powers. From the Latin American perspective, it functions as pure extraction: coercive pressure without genuine negotiating power or benefits. The constraint is NOT simultaneously both rope and snare in an undecidable way — it is a snare that appears as rope from the beneficiary's position. The mandatrophy is resolved by recognizing that the 'coordination' benefit (clear rules, predictability) accrues exclusively to the hegemon, while costs (reduced autonomy, intervention risk) fall on subordinate states. This is the defining characteristic of a snare: a constraint that combines coercion (suppression=0.72) with extraction (ε=0.68) and provides asymmetric benefits (only to enforcer). A genuine rope would produce benefits for both parties; the Monroe Doctrine produces benefits only for the US and costs only for Latin American states. The rising powers' perspective (Tangled Rope) reflects the constraint's dual function as both a precedent for hegemonic spheres (extraction) and a coordination mechanism for opposing US dominance (coordination) — but this is a secondary effect, not the primary structural function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coalition_formation_threshold,
    'At what regional coordination level can Latin American states credibly deter or resist unilateral US intervention?',
    'Historical analysis of regional coalition-building (CELAC, ALBA, PROSUR); assessment of Venezuelan crisis response; credibility of joint defense commitments; military capability aggregation',
    'If threshold < 3 states: regional opposition remains too fragmented to deter intervention (snare persists). If threshold > 5 states: coalition power becomes credible, shifting Latin American exit from trapped to constrained, potentially reclassifying as tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_formation_threshold, empirical, 'Regional coalition threshold for credible deterrence').

omega_variable(
    us_strategic_pivot_commitment,
    'Is the US Monroe Doctrine revival a durable strategic commitment or a rhetorical artifact of particular administrations?',
    'Longitudinal analysis of US military presence in Latin America across administrations; frequency and escalation of interventionist rhetoric; allocation of military resources to Southern Command; congressional authorization patterns for regional operations',
    'If durable commitment: snare classification confirmed, suppression remains high. If rhetorical: suppression is overstated; constraint functions more as intimidation (lower actual enforcement), potentially reclassifying to tangled rope or scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_strategic_pivot_commitment, empirical, 'Durability of US strategic commitment to hegemonic enforcement').

omega_variable(
    china_russia_containment_credibility,
    'Can China and Russia provide sufficient countervailing security guarantees to Latin American states to make the Monroe Doctrine extractively unsustainable?',
    'Assessment of Chinese military presence, Russian operational capacity in hemisphere, credibility of defense treaties, economic incentive structures; game-theoretic analysis of escalation costs vs hegemonic benefits',
    'If countervailing power becomes credible: suppression drops significantly, exit options shift from trapped to constrained or mobile, extraction becomes unsustainable, constraint reclassifies to tangled rope or rope. If countervailing power remains limited: snare classification persists, suppression remains high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(china_russia_containment_credibility, empirical, 'Credibility of countervailing security guarantees').

omega_variable(
    global_multipolarity_threshold,
    'What level of multipolarity or regional autonomy makes unilateral hegemonic spheres structurally untenable?',
    'Measurement of power distribution indices (Herfindahl, Polarity indices); assessment of regional organization effectiveness; analysis of precedent-setting effects (if Brazil successfully resists, do other regions follow?)',
    'If threshold approaching: constraint begins degrading toward piton or scaffold (enforcement costs rising, theater rising as gap between stated rules and enforcement widens). If threshold distant: snare persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_multipolarity_threshold, conceptual, 'Structural threshold at which hegemonic spheres become untenable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lula_hemisphere_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lula_tr_t0, lula_hemisphere_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(lula_tr_t3, lula_hemisphere_2026, theater_ratio, 3, 0.5).
narrative_ontology:measurement(lula_tr_t6, lula_hemisphere_2026, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(lula_be_t0, lula_hemisphere_2026, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(lula_be_t3, lula_hemisphere_2026, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(lula_be_t6, lula_hemisphere_2026, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lula_hemisphere_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(lula_hemisphere_2026, us_china_strategic_competition).
narrative_ontology:affects_constraint(lula_hemisphere_2026, regional_autonomy_latin_america).
narrative_ontology:affects_constraint(lula_hemisphere_2026, multilateral_order_degradation).

% DUAL FORMULATION NOTE:
% The Monroe Doctrine revival is downstream of broader US-China strategic competition and hegemonic rivalry. It represents the enforcement mechanism by which US hegemony is maintained in one region; parallel constraints exist for other hegemons (Russian sphere of influence in post-Soviet space, Chinese sphere in Indo-Pacific). These constraints are structurally similar (hegemonic spheres enforced through military and economic coercion) but have distinct ε values reflecting different regional power distributions and enforcement costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lula_hemisphere_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
