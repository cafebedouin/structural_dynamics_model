% ============================================================================
% CONSTRAINT STORY: geopolitical_insularity_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geopolitical_insularity_2026, []).

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
 *   constraint_id: geopolitical_insularity_2026
 *   human_readable: Geopolitical Nationalist Insularity
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The 2026 geopolitical order represents a structural shift from the
 *   post-Cold War liberal international system (1990-2020) toward a 'Great
 *   Realignment' where nationalist elites treat international cooperation as
 *   zero-sum. Trust has become a zero-sum nationalist asset: gains by rival
 *   powers are framed as losses by the home nation, even when absolute mutual
 *   gains exist. This constraint operates as a hybrid coordination-extraction
 *   mechanism. For nationalist elites and their domestic extraction
 *   coalitions, insularity is pure coordination: it consolidates power,
 *   rallies domestic support, and captures rents through trade protection and
 *   capital controls. For supply chains, it is mixed extraction and
 *   coordination: firms aligned with nationalist policy receive protected
 *   markets but face rising fragmentation costs. For the global poor, climate
 *   commons, and future generations, it is pure extraction: they bear all
 *   suppression costs with no exit. For rival nationalist blocs, it is
 *   recursive: each bloc's insularity validates the other's, creating a
 *   coordination-competition hybrid at the bloc level. For multilateral
 *   institutions, insularity is inertial: the institutions persist through
 *   procedural theater (summits, agreements, mandates) but their actual
 *   coordination function is severely degraded. The analytical observer risks
 *   naturalizing this arrangement as an inevitable feature of political
 *   organization (Mountain) when in fact it requires continuous enforcement,
 *   propaganda, and suppression of exit pathways.
 *
 * KEY AGENTS:
 *   - Nationalist Political Elites: Primary beneficiaries (institutional/arbitrage) — consolidate power, capture extraction rents, zero-sum control of narrative
 *   - Domestic Extraction Coalitions: Secondary beneficiaries (organized/constrained) — protected domestic industries, reduced competition, rents from tariffs and capital controls
 *   - Global Poor: Primary victims (powerless/trapped) — reduced market access, blocked migration, employment disruption, no exit
 *   - Climate and Biodiversity Commons: Structural victim (powerless/trapped) — cannot exit nationalist fragmentation, bears deferred externalities and prevented coordination
 *   - International Supply Chains: Mixed actors (organized/constrained) — constrained by fragmentation but benefit from nationalist protection if aligned
 *   - Rival Nationalist Blocs: Competing beneficiaries (organized/constrained) — each validates the other's insularity logic, recursive extraction-coordination
 *   - Multilateral Institutions: Institutional observers (institutional/arbitrage) — persist but degraded, theater ratio increases over interval
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent arrangement as immutable political law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geopolitical_insularity_2026, 0.58).
domain_priors:suppression_score(geopolitical_insularity_2026, 0.72).
domain_priors:theater_ratio(geopolitical_insularity_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geopolitical_insularity_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(geopolitical_insularity_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geopolitical_insularity_2026, tangled_rope).
narrative_ontology:human_readable(geopolitical_insularity_2026, "Geopolitical Nationalist Insularity").
narrative_ontology:topic_domain(geopolitical_insularity_2026, "geopolitical/economic").

domain_priors:requires_active_enforcement(geopolitical_insularity_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, nationalist_political_elites).
narrative_ontology:constraint_beneficiary(geopolitical_insularity_2026, domestic_extraction_coalitions).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, international_supply_chains).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, global_poor).
narrative_ontology:constraint_victim(geopolitical_insularity_2026, future_generations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE GLOBAL POOR (SNARE) — Trapped within trade barriers, remittance fragmentation, and capital flight restrictions imposed by insularity. Cannot exit nationalist compartmentalization. Bears full extraction cost through reduced market access, employment disruption, and blocked migration pathways. Maximum suppression: no alternatives available within nationalist frameworks.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE AND BIODIVERSITY COMMONS (SNARE) — Trapped by nationalist fragmentation that prevents coordinated emissions limits, methane reduction, and species protection. Cannot exit the constraint. Extraction through deferred externalities: nationalist insularity allows domestic polluters to externalize costs onto the global commons. Suppression absolute — no self-correcting mechanism at the nationalist-fragmented scale.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTINATIONAL SUPPLY CHAINS (TANGLED ROPE) — Constrained by re-shoring mandates, foreign investment restrictions, and tariff walls, but also benefit from nationalist protection of 'domestic' manufacturing. Faces simultaneous extraction (supply chain fragmentation raises costs) and coordination benefits (preferred access to protected domestic markets for aligned firms). Effective extraction moderate but persistent — firms can partially arbitrage across nationalist blocks but face rising transaction costs.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: NATIONALIST POLITICAL ELITES (ROPE) — Primary beneficiaries. Experience insularity as pure coordination: rallying domestic support behind 'national interest,' consolidating power, capturing extraction rents through trade restrictions and capital controls. Zero-sum framing of trust (other nations' gains = our losses) justifies concentration of authority. High arbitrage: can exit to international forums while maintaining domestic nationalist narrative, or pivot between blocs. Effective extraction negative — elites accrue benefits, not costs.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: RIVAL NATIONALIST BLOC (TANGLED ROPE) — Competing power structure with its own extractive insularity logic. Experiences the constraint as both coordination (unifying its members against external rival) and extraction (forced capital reallocation, military spending increases, technology decoupling). Constrained exit: cannot fully exit the nationalist arms race without strategic vulnerability. Suppression moderate — bloc members can exit to the rival bloc, providing some pressure release, but switching costs are high.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: MULTILATERAL INSTITUTIONS (PITON) — The UN, WTO, IMF, World Bank, and regional development banks persist in nationalist-fragmented world but with degraded function. Their coordination role (setting norms, arbitrating disputes, allocating resources) is systematically undermined by great-power veto, nationalist non-compliance, and brain-drain to nationalist intelligence/economic agencies. Theater ratio high: summits and agreements proceed but lack binding enforcement. Piton derives from 0.68 theater ratio — institutional performance is maintained through procedure, not function.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN?) — From a deep civilizational view, some degree of local/regional preference and in-group cooperation are intrinsic to human political organization. Trust asymmetries between in-groups and out-groups reflect fundamental aspects of kinship logic and bounded rationality. However, the constraint story shows that the 2026 form of nationalist insularity is not immutable — it requires continuous active enforcement (tariffs, capital controls, visa restrictions, technology decoupling), sustained propaganda (zero-sum framing), and suppression of exit pathways. The mountain classification is likely false summit: what appears as inherent to politics is a contingent institutional arrangement built on enforcement, theater, and narrative engineering.
constraint_indexing:constraint_classification(geopolitical_insularity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geopolitical_insularity_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geopolitical_insularity_2026, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geopolitical_insularity_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geopolitical_insularity_2026, TR),
    TR >= 0.70.

:- end_tests(geopolitical_insularity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Nationalist insularity extracts through multiple channels: trade protection rents (captured by domestic elites), capital control restrictions (prevented exit reduces bargaining power of labor and small capital), technology decoupling (raises innovation costs across all sectors), supply chain fragmentation (disrupts production and increases costs), and suppression of migration (restricts labor mobility). The extraction is significant but not maximal (0.70+) because moderate actors (multinational firms, some international traders) can partially arbitrage across blocs and within constrained pathways. Suppression (0.72): High. Multiple barriers prevent exit: tariffs and quotas prevent trade exit; capital controls prevent financial exit; visa restrictions prevent migration exit; technology decoupling prevents knowledge exit; nationalist propaganda suppresses exit desirability by framing international engagement as betrayal. However, suppression is not absolute (1.0) because black markets, smuggling routes, visa overstays, and technology theft provide partial circumvention pathways. Theater ratio (0.68): Moderate-high. Procedural performance is high: summits occur, trade agreements are negotiated, international law is cited, institutional mandates are restated. But functional coordination has degraded: agreements are circumvented, institutional enforcement is blocked by great-power veto, compliance is selective. The 2026 interval shows theater increasing from 0.42 to 0.68 as elites invest more in propaganda (nationalist media, conspiracy theories, strategic misinformation) to sustain the zero-sum narrative despite evidence of mutual losses.
 *
 * PERSPECTIVAL GAP:
 *   Nationalist elites experience Rope (pure coordination for domestic power consolidation). Supply chains experience Tangled Rope (mixed coordination and extraction). The global poor experience Snare (pure extraction). Climate commons experience Snare (pure extraction with no corrective mechanism). Rival blocs experience recursive Tangled Rope (each bloc is both beneficiary within itself and target from without). Multilateral institutions experience Piton (procedural theater masking functional degradation). The analytical observer risks Mountain (naturalizing zero-sum nationalism as inevitable feature of politics). The perspectival range from Rope to Snare to Mountain across identical structural facts indicates that the constraint's classification depends entirely on observer position and exit options — there is no single 'truth' about whether nationalist insularity is coordination or extraction. It is both, from different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base extractiveness (ε=0.58), directionality d (derived from beneficiary/victim status and exit options), the sigmoid f(d), and scope modifier σ(S). Nationalist elites: beneficiary status, arbitrage options → d≈0.08 → f(d)≈-0.01 → χ≈-0.006 (negative, subsidized). Supply chains: mixed beneficiary-victim, constrained → d≈0.50 → f(d)≈0.65 → χ≈0.38 (moderate extraction). Global poor: victim status, trapped → d≈0.95 → f(d)≈1.42 → χ≈0.82 (maximal extraction). Climate commons: victim status, trapped, universal scope → d≈0.95, σ(global)=1.2 → χ≈0.98 (extraction amplified by scope). No overrides are necessary because the structural derivation accurately captures the asymmetry: the constraint extracts differently depending on power position.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false classification by refusing to collapse the perspectival range into a single type. The analytical observer's Mountain classification is identified as a false summit through the accessibility_collapse and resistance metrics: nationalist insularity requires continuous active enforcement (tariffs, capital controls, visa systems, technology decoupling), meaning it is not an 'immutable natural law' but a contingent institutional arrangement. Accessibility to alternatives is not collapsed (arbitrage exists for elites and intermediate actors), and resistance to the constraint is not negligible (black markets, visa overstays, smuggling, technology diffusion all undermine the barrier). The constraint's true character is Tangled Rope at the systemic level: it provides genuine coordination benefits for nationalist elites and their domestic coalitions while imposing extraction on supply chains and the global poor. The mandatrophy is resolved by explicitly recognizing that 'coordination' and 'extraction' are observer-relative: the same institutional arrangement is coordination for beneficiaries (low d, negative χ) and extraction for victims (high d, high χ). The constraint is neither purely coordination nor purely extraction — it is hybrid, and its classification depends on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_sum_inevitability,
    'Is the zero-sum framing of nationalist trust a necessary feature of political organization or a contingent narrative choice by elites?',
    'Historical comparison of periods with lower-extractiveness international coordination (1950-1990 Bretton Woods, GATT); analysis of elite messaging: how often does ''mutual gain'' framing appear relative to ''national interest'' framing in political speech',
    'If necessary: insularity approaches Mountain classification (immutable). If contingent: insularity is Tangled Rope or Snare sustained by enforcement, not by natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_sum_inevitability, conceptual, 'Whether zero-sum framing is inevitable or contingent').

omega_variable(
    supply_chain_resilience_threshold,
    'What level of supply chain fragmentation creates inefficiency costs that exceed nationalist security benefits?',
    'Empirical measurement of inflation, productivity loss, and innovation lag in re-shored vs globally integrated sectors; comparison across blocs (EU integrated vs US-allied fragmented)',
    'If threshold < 2% GDP loss: insularity is purely extractive (Snare intensifies). If threshold > 5% GDP loss: beneficiaries face rebellion from moderate actors experiencing high costs (Tangled Rope destabilizes).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_chain_resilience_threshold, empirical, 'Threshold where supply chain costs exceed security benefits').

omega_variable(
    bloc_permeability,
    'Can firms, capital, and individuals migrate between nationalist blocs, or are blocs functionally sealed?',
    'Tracking of corporate re-domiciliation, capital flight restrictions enforced, visa/emigration barriers to bloc rivals; measurement of defection rates among aligned firms and talented workers',
    'If permeable: supply chain actors and individuals have exit options (Tangled Rope classification holds, suppression < 1.0). If sealed: exit is blocked and suppression approaches 1.0 (transitions toward pure Snare for trapped agents).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bloc_permeability, empirical, 'Degree of permeability between nationalist blocs').

omega_variable(
    multilateral_institution_survival,
    'Will multilateral institutions persist as empty procedural theaters (Piton) or collapse entirely?',
    'Measurement of institutional output (agreements reached, enforcement actions, resource allocation); comparison of actual compliance rates with stated mandate; tracking of brain-drain from institutions to nationalist agencies',
    'If Piton persists: theater_ratio remains high, constraint remains Tangled Rope. If institutions collapse: suppression increases, Snare classification spreads, no alternative coordination mechanisms (transition risk).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(multilateral_institution_survival, empirical, 'Whether multilateral institutions degrade to piton or collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geopolitical_insularity_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geopol_tr_t0, geopolitical_insularity_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(geopol_tr_t8, geopolitical_insularity_2026, theater_ratio, 8, 0.55).
narrative_ontology:measurement(geopol_tr_t16, geopolitical_insularity_2026, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(geopol_be_t0, geopolitical_insularity_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(geopol_be_t8, geopolitical_insularity_2026, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(geopol_be_t16, geopolitical_insularity_2026, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geopolitical_insularity_2026, enforcement_mechanism).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, semiconductor_supply_chain_decoupling).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, energy_transition_blockade).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, pandemic_vaccine_nationalism).
narrative_ontology:affects_constraint(geopolitical_insularity_2026, ai_capability_race_fragmentation).

% DUAL FORMULATION NOTE:
% Nationalist insularity operates at the systemic level but manifests through specific sectoral constraints (semiconductors, energy, vaccines, AI). Each sectoral constraint has its own ε and perspectives but is structurally linked to the parent insularity constraint. Semiconductors exhibit ε≈0.62 (higher extraction due to dual-use sensitivity); energy shows ε≈0.52 (lower extraction due to geographic necessity of trade); vaccines showed ε≈0.68 during 2020-2021 (highest extraction during scarcity); AI shows ε≈0.55 (rising as dual-use military value increases). All are downstream manifestations of the parent constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(geopolitical_insularity_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
