% ============================================================================
% CONSTRAINT STORY: indo_russian_submarine_lease_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-03-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_russian_submarine_lease_2025, []).

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
 *   constraint_id: indo_russian_submarine_lease_2025
 *   human_readable: Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)
 *   domain: geopolitical/military_cooperation
 *
 * SUMMARY:
 *   The Indo-Russian nuclear submarine lease (Chakra III, Akula-class)
 *   represents a ~US$2 billion agreement that binds India to long-term
 *   Russian technological dependency while simultaneously advancing Russian
 *   strategic interests in the Indo-Pacific and constraining US dominance.
 *   The constraint exhibits simultaneous coordination and extraction
 *   functions: India gains submarine capability that deterred Chinese
 *   aggression (coordination benefit), but at the cost of
 *   operational/maintenance dependency on Russia (extraction mechanism). The
 *   lease illustrates how geopolitical partnerships can function as hybrid
 *   constraints: they solve immediate security problems while locking states
 *   into asymmetric long-term relationships. The analytical challenge is
 *   distinguishing whether the lease is a temporary scaffold toward Indian
 *   autonomy (if technology transfer is genuine and indigenous programs
 *   mature) or a permanent snare (if Russia withholds key technologies and
 *   maintains operational control). The theater ratio (0.58) reflects that
 *   both Russia and India present the lease as pure strategic partnership
 *   cooperation, while the actual structure includes significant dependency
 *   leverage. The extractiveness has increased from 0.38 (early agreement
 *   optimism) to 0.52 (as operational constraints and maintenance
 *   dependencies become apparent), indicating that the real extraction costs
 *   emerge only after deployment when India's exit options collapse.
 *
 * KEY AGENTS:
 *   - India (State Strategic Actor): Primary beneficiary/victim (powerful/mobile) — gains immediate submarine deterrent but sacrifices long-term autonomy; bears cost of Russian dependency and maintenance leverage
 *   - Russia (Institutional Beneficiary): Primary beneficiary (institutional/arbitrage) — captures maintenance revenue, extends strategic partnership, maintains influence in Indo-Pacific; minimal extraction cost
 *   - United States (Powerful Hegemon): Victim (powerful/trapped) — constrained by Russian-Indian military alignment; cannot prevent lease without escalation; loses exclusive Indo-Pacific dominance
 *   - China (Regional Power): Mixed victim/beneficiary (powerful/constrained) — loses short-term regional dominance to Indian submarine capability but maintains long-term advantage through Indian dependency on external partnerships
 *   - Indigenous Indian Submarine Programs: Organizational victim (organized/constrained) — Project 75I and Arihant successor face reduced funding/political priority because Russian lease meets immediate naval requirement; dependent on technology transfer that may not occur
 *   - Multilateral Defense Architecture (QUAD/AUKUS): Alternative provider (organized/mobile) — represents exit pathway; if matured, reduces snare classification and strengthens scaffold perspective
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, 0.52).
domain_priors:suppression_score(indo_russian_submarine_lease_2025, 0.68).
domain_priors:theater_ratio(indo_russian_submarine_lease_2025, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, extractiveness, 0.52).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_russian_submarine_lease_2025, tangled_rope).
narrative_ontology:human_readable(indo_russian_submarine_lease_2025, "Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)").
narrative_ontology:topic_domain(indo_russian_submarine_lease_2025, "geopolitical/military_cooperation").

domain_priors:requires_active_enforcement(indo_russian_submarine_lease_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, russian_defense_industrial_complex).
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, indian_strategic_autonomy).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, indo_pacific_balance_of_power).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, us_strategic_dominance).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, china_regional_hegemony).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIAN NAVAL DEPENDENCY (SNARE) — India's submarine fleet is structurally dependent on Russian maintenance, spare parts, and operational integration with Russian-designed systems. Exit from the lease creates acute vulnerability to Chinese submarine dominance in the Indo-Pacific. d≈0.90, f(d)≈1.38, σ=0.9 → χ≈0.64. India is operationally trapped.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RUSSIAN DEFENSE INDUSTRIAL COMPLEX (ROPE) — Leasing secures long-term maintenance contracts, operational integration revenue, and strategic partnership. Russia experiences the lease as pure coordination: maintaining the operational platform generates revenue and strategic alliance. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIAN STATE STRATEGIC AUTONOMY (TANGLED ROPE) — India gains immediate submarine capability (coordination benefit: deters regional aggression, establishes power projection) but sacrifices long-term autonomy. Dependent on Russian maintenance, technology transfer limitations, and political alignment. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.35. Mixed extraction and coordination.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: US STRATEGIC DOMINANCE (SNARE) — The lease advances Russian-Indian military cooperation, directly constraining US power projection in the Indo-Pacific. The constraint operates on US freedom of action: US cannot prevent the lease without military intervention (cost too high), and cannot exclude Russian influence from the region without escalation. d≈0.88, f(d)≈1.35, σ=1.1 → χ≈0.74. US is strategically trapped.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: CHINESE REGIONAL HEGEMONY (TANGLED ROPE) — The lease constrains China's near-term regional dominance (India gains submarine capability), but also benefits China by keeping India dependent on external powers rather than developing indigenous submarine capability. d≈0.52, f(d)≈0.70, σ=1.1 → χ≈0.41. Mixed constraint and opportunity.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INDO-PACIFIC MULTILATERAL DEFENSE ARCHITECTURE (SCAFFOLD) — AUKUS, Quad, and Japan's defense partnerships represent alternative pathways to Indian submarine capability without Russian dependency. The lease is a temporary coordination solution; multilateral architecture offers exit path. d≈0.40, f(d)≈0.40, σ=1.1 → χ≈0.18. Low effective extraction because alternatives exist.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: POST-COLD WAR STRATEGIC PARTNERSHIP RHETORIC (PITON) — The Indo-Russian relationship is maintained through performative strategic partnership despite structural tension: Russia's tilt toward China, India's alignment with QUAD. The submarine lease persists through institutional inertia and rhetorical commitment rather than deep shared interest. theater_ratio=0.58 (moderately performative). Maintenance of partnership ritual despite eroding functional alignment.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — From a civilizational perspective, submarine technology creates inherent strategic dependencies: no state can be fully independent in nuclear deterrence without foreign partnership. This perspective risks naturalizing the lease as structurally inevitable. However, India's domestic submarine programs (INS Arihant series, Project 75I), Japanese submarine partnerships, and AUKUS alternatives reveal the dependency as contingent, not inherent. The 'natural law' framing obscures geopolitical choice.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_russian_submarine_lease_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_russian_submarine_lease_2025, TR),
    TR >= 0.70.

:- end_tests(indo_russian_submarine_lease_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Russia captures long-term maintenance revenue and operational leverage; India bears costs of technology withholding and strategic inflexibility. The constraint is not as extractive as pure Russian dominance (ε would be 0.70+) because India retains strategic agency (can accelerate indigenous programs, can pivot to QUAD partners). But it is more extractive than pure coordination because Russia's maintenance control gives Russia veto power over Indian strategic operations during crisis. Suppression (0.68): High. India's exit options are severely constrained: exiting the lease creates near-term submarine gap during which China dominates; transitioning to Western partnerships faces technology denial barriers (US/Japan reluctant to share nuclear propulsion); accelerating indigenous programs requires time India doesn't have. Russia maintains operational control through maintenance and parts supply. Theater ratio (0.58): Moderate. The partnership is presented as pure strategic cooperation, but both parties know the dependency structure. India must perform enthusiastic Russian alignment despite hedging toward QUAD. Russia must maintain partnership despite China-first pivot. The performative content has increased as the geopolitical environment has shifted (Ukraine sanctions, China alignment) and the actual partnership has become more transactional. Claimed type (Tangled Rope): The constraint requires active enforcement (Russia must maintain the submarine, enforce parts dependency; India must sustain political commitment despite domestic pressure), exhibits both coordination (submarine capability deters aggression) and extraction (Russian leverage over operational decisions), and involves asymmetric benefits (Russia's revenue vs India's strategic cost). The mandatrophy is resolved: this is not a purely extractive snare (India does gain real deterrent capability) and not a pure rope (Russia does extract operational and strategic control).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a five-way perspectival split that reveals the complexity of geopolitical hybrid constraints. India (powerful beneficiary/victim) sees tangled rope: the submarine solves an urgent strategic problem but creates long-term dependency. Russia (institutional beneficiary) sees rope: pure coordination revenue without extraction cost from Russia's perspective. The US (powerful victim) sees snare: constrained by Russian influence expansion with no exit option. China (powerful mixed actor) sees different snares and ropes depending on timeline: short-term snare (loses dominance), long-term rope (India remains dependent on external partnerships rather than developing autonomy). The open science coalition analog (QUAD/AUKUS) sees scaffold: the lease is temporary; multilateral alternatives are emerging. The institutional rhetoric (post-Cold War partnership) sees piton: the partnership persists through inertia despite eroding functional alignment. The analytical observer risks seeing a mountain (strategic dependencies are inherent to submarine deterrence) but structural data reveals this as a false summit — India's choices (indigenous programs, QUAD pivot, technology partnerships) prove the dependency is contingent. The perspectival gaps arise because different actors have different exit costs: India can eventually exit (high cost but possible); Russia wants to lock India in (no exit); US wants India to exit (reverse the constraint); China wants India dependent on anyone but China (constraint useful to prevent Indian autonomy).
 *
 * DIRECTIONALITY LOGIC:
 *   Indian state (powerful + mobile): Beneficiary (gains deterrent) + victim (loses autonomy) = mixed directionality. d≈0.55, f(d)≈0.75. Mixed extraction and coordination. Russian institutional (institutional + arbitrage): Pure beneficiary + arbitrage exit = low d. d≈0.08, f(d)≈-0.10. Net beneficiary (negative χ means Russia benefits, not extracts). US (powerful + trapped in Indo-Pacific): Victim (constrained by Russian influence) + no exit in region = high d. d≈0.88, f(d)≈1.35. Severe extraction from US perspective. China (powerful + constrained): Victim short-term (loses dominance) + constrained long-term (wants to prevent Indian autonomy but can't directly intervene) = medium d. d≈0.52, f(d)≈0.70. Mixed. QUAD/AUKUS (organized + mobile): Neither beneficiary nor victim of the lease itself, but provider of alternative exit pathways = low d for coordination frame. d≈0.40, f(d)≈0.40. Low effective extraction because exit options exist. Indigenous submarine programs (organized + constrained): Victim of political priority shift toward Russian lease = high d. d≈0.78, f(d)≈1.12. High extraction from their perspective (deprioritized).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The lease is a tangled rope, not a misclassified snare pretending to be rope or vice versa. Evidence: (1) COORDINATION FUNCTION EXISTS: The lease provides genuine submarine capability that shifts regional deterrence balance. India would be operationally vulnerable without it. Russia credibly supplies operational submarines. This is not theatrical security theater — the platform is real and functions. (2) ASYMMETRIC EXTRACTION EXISTS: Russia extracts maintenance revenue, operational leverage, and strategic presence indefinitely. India bears costs of technological dependency and strategic inflexibility. The asymmetry is structural and persistent. (3) ACTIVE ENFORCEMENT REQUIRED: Russia must continuously maintain the platform; India must sustain political commitment despite domestic QUAD pressure. Both parties enforce the arrangement actively. (4) PERSPECTIVAL CONSISTENCY: The tangled_rope classification appears consistently across beneficiary (India, partial), institutional (Russia), and powerful victim (US) perspectives, with divergence only at the analytical level (where false mountain appears) and scaffold level (where exit paths are theorized). The omegas (technology transfer depth, US alternative pathway viability, Russian reliability) all track the core tension: whether the scaffold exit emerges (reducing snare risk) or whether extraction deepens as India becomes locked in. Mandatrophy is resolved because the classification is stable under perturbation: whether extractiveness rises to 0.65 (deeper lock-in) or falls to 0.38 (exit paths mature), the constraint remains tangled rope, not a misidentified pure type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    russian_technology_transfer_depth,
    'Does the lease include meaningful technology transfer for indigenous Indian submarine development, or is it purely operational lease with dependency lock-in?',
    'Analysis of lease terms (classified), Indian acquisition of propulsion technology blueprints, timeline for indigenous Arihant-class successor development',
    'If transfer: lease is temporary scaffold toward autonomy (χ lower). If no transfer: lease is permanent dependency mechanism (χ higher, snare persists).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(russian_technology_transfer_depth, empirical, 'Degree of technology transfer in lease agreement').

omega_variable(
    us_response_constraint_intensity,
    'Will US strategic response to the lease (expanded Japan/Australia/India partnerships) materially increase India''s exit costs or provide genuine alternative to Russian dependency?',
    'Timeline of AUKUS submarine deliveries to Australia, Japan defense technology partnership depth, US willingness to share nuclear submarine technology with India',
    'If QUAD alternatives mature: scaffold exit path solidifies, lessens snare classification. If AUKUS stalls: Russian dependency persists, snare deepens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_response_constraint_intensity, empirical, 'Effectiveness of Western multilateral alternatives to Russian submarine partnership').

omega_variable(
    russian_strategic_pivot_reliability,
    'What is Russia''s actual strategic commitment to India given Russia''s deepening China alignment? Will Russia sustain submarine partnership if China views it as threat to Chinese-Russian coordination?',
    'Russia-China joint statements on Indian partnership, Russia''s behavior in India-China border crises, sanctioning regime impact on Russian defense exports',
    'If Russia pivots toward China primacy: Indian lease becomes politically vulnerable (Russia may reduce support), increasing extraction risk. If Russia maintains India partnership as China hedge: constraint remains stable tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_strategic_pivot_reliability, conceptual, 'Russia''s commitment reliability in face of China-Russia strategic alignment').

omega_variable(
    indigenous_submarine_program_viability,
    'Can India''s Project 75I (Scorpène follow-on) and Project 75(I) conventional submarine program mature faster than Russian dependency lock-in occurs?',
    'Timelines for Indian diesel-electric submarine delivery, propulsion technology sourcing (Siemens partnership, indigenous diesel engines), comparison to nuclear submarine development timeline',
    'If indigenous programs accelerate: reduces Russian dependency, unlocks exit option, tangled_rope degrades toward rope. If programs stall: Russian lease becomes permanent, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_submarine_program_viability, empirical, 'Viability of Indian autonomous submarine development timeline').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_russian_submarine_lease_2025, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(irsl_tr_t0, indo_russian_submarine_lease_2025, theater_ratio, 0, 0.42).
narrative_ontology:measurement(irsl_tr_t5, indo_russian_submarine_lease_2025, theater_ratio, 5, 0.5).
narrative_ontology:measurement(irsl_tr_t10, indo_russian_submarine_lease_2025, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(irsl_be_t0, indo_russian_submarine_lease_2025, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(irsl_be_t5, indo_russian_submarine_lease_2025, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(irsl_be_t10, indo_russian_submarine_lease_2025, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_russian_submarine_lease_2025, enforcement_mechanism).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, quad_defense_architecture).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, aukus_submarine_partnership).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, us_indo_pacific_hegemony).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, chinese_regional_dominance).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, indian_defense_autonomy).

% DUAL FORMULATION NOTE:
% The submarine lease decomposes into two distinct constraint stories: (1) operational submarine platform as coordination mechanism (ε≈0.25, mostly rope from India's operational perspective), and (2) long-term technological dependency as extraction mechanism (ε≈0.65, snare from India's strategic autonomy perspective). The unified story (ε=0.52, tangled_rope) integrates both. Upstream constraint: Russian defense industrial capability enables the lease. Downstream constraints: India's domestic submarine programs compete with the lease for resources; QUAD alternatives represent exit pathways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(indo_russian_submarine_lease_2025, powerful, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
