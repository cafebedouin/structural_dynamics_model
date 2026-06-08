% ============================================================================
% CONSTRAINT STORY: state_capacity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_capacity_threshold, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_capacity_threshold
 *   human_readable: State Capacity Threshold: Meiji Calendar and Dress Code Imposition
 *   domain: historical_sociology/state_formation/commitment_systems
 *
 * SUMMARY:
 *   The Meiji Restoration (1868-1912) represents a paradigmatic case of
 *   top-down, state-imposed commitment system transformation: the wholesale
 *   replacement of traditional lunar calendars and Edo-period dress codes
 *   with Gregorian calendar and Western dress standards. This constraint
 *   embodies the analytical puzzle: does the commitment system framework
 *   require a 'top-down imposition' cell, or can state-enforced reading
 *   replacement be adequately modeled through existing cells
 *   (legitimacy-shift, authority-override, practice-collapse)? The Meiji case
 *   forces a choice: either the framework correctly excludes exogenous
 *   override as a distinct commitment mode (because all reading replacement
 *   is ultimately driven by legitimacy pressure, not pure coercion), or the
 *   framework has a blind spot in modeling state capacity as an independent
 *   variable. The constraint shows measurable suppression escalation
 *   following the 1873 decree (suppression_requirement jumps from 0.15 to
 *   0.65 at decree issuance), suggesting this is NOT a gradual
 *   fringe-to-center adoption but a structural replacement imposed against
 *   endogenous resistance. Yet within 20 years, extractiveness has normalized
 *   (0.50) and suppression has declined (0.35), indicating that
 *   enforcement-driven compliance gradually became culturally embedded. The
 *   theater ratio (0.55 at t=10, peak displacement of authentic practice by
 *   performative compliance) and subsequent decline to 0.50 reflect the
 *   persistence of traditional calendars as vestigial ritual after
 *   administrative displacement.
 *
 * KEY AGENTS:
 *   - Meiji State Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures international legitimacy, unified bureaucratic time, foreign trade synchronization
 *   - Rural Agrarian Communities: Primary victims (powerless/trapped) — bearing costs of epistemic disruption (lunar-based seasonal knowledge displaced); cannot exit territorial governance
 *   - Traditional Artisans and Calendar Practitioners: Secondary victims (moderate/constrained) — expertise markets destroyed; constrained by economic dependency on state-regulated commerce
 *   - Foreign Commerce Integration Framework: Secondary beneficiary (institutional/constrained) — Western trading partners require standardization; Japan's trade access depends on compliance
 *   - Modernization Coalition (Intellectuals, Foreign-Educated Bureaucrats): Organized actors (organized/mobile) — see calendar standardization as transitional (scaffold logic); expect enforcement to decrease as standards internalize
 *   - Vestigial Traditional Calendar Authority (Shrine Keepers, Folk Practitioners): Institutional actor (institutional/constrained) — maintain traditional calendars theatrically after administrative displacement; constrained by state tolerance rather than genuine functional role
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing Western calendar synchronization as inevitable law of global administration
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_capacity_threshold, 0.65).
domain_priors:suppression_score(state_capacity_threshold, 0.7).
domain_priors:theater_ratio(state_capacity_threshold, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_capacity_threshold, extractiveness, 0.65).
narrative_ontology:constraint_metric(state_capacity_threshold, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(state_capacity_threshold, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_capacity_threshold, tangled_rope).
narrative_ontology:human_readable(state_capacity_threshold, "State Capacity Threshold: Meiji Calendar and Dress Code Imposition").
narrative_ontology:topic_domain(state_capacity_threshold, "historical_sociology/state_formation/commitment_systems").

domain_priors:requires_active_enforcement(state_capacity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_capacity_threshold, 'c4805c5b-2b42-4a1a-b479-0bce5bc9ad00').
narrative_ontology:cs_kernel_codification('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', formalized).
narrative_ontology:cs_authority_grounding('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', extraction).
narrative_ontology:cs_reading_relation('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', state_capacity_threshold__gradualist_selective_modernization_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', state_capacity_threshold__synthetic_modernization_reading, influences).
narrative_ontology:cs_reading_relation('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', state_capacity_threshold__cultural_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', foundational, western_calendar_is_objective_standard).
narrative_ontology:cs_axiom_status(western_calendar_is_objective_standard, holdable).
narrative_ontology:cs_axiom_grounding('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', western_calendar_is_objective_standard, empirically_contingent).
narrative_ontology:cs_axiom('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', secondary, traditional_calendars_are_feudal_superstition).
narrative_ontology:cs_axiom_status(traditional_calendars_are_feudal_superstition, holdable).
narrative_ontology:cs_axiom_grounding('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', traditional_calendars_are_feudal_superstition, conventional).
narrative_ontology:cs_axiom('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', foundational, modernization_requires_western_cultural_forms).
narrative_ontology:cs_axiom_status(modernization_requires_western_cultural_forms, overridden).
narrative_ontology:cs_axiom_grounding('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', modernization_requires_western_cultural_forms, instrumental).
narrative_ontology:cs_reference_frame('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', japanese_commitment_to_western_synchronization).
narrative_ontology:cs_drift_state('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', contemporary_post_taisho_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c4805c5b-2b42-4a1a-b479-0bce5bc9ad00', '2026-02-26T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_capacity_threshold, meiji_state_authority).
narrative_ontology:constraint_beneficiary(state_capacity_threshold, imperial_administration).
narrative_ontology:constraint_beneficiary(state_capacity_threshold, foreign_commerce_integration).
narrative_ontology:constraint_victim(state_capacity_threshold, local_calendar_practitioners).
narrative_ontology:constraint_victim(state_capacity_threshold, traditional_dress_artisans).
narrative_ontology:constraint_victim(state_capacity_threshold, rural_agrarian_systems).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(state_capacity_threshold, foreign_commerce_framework).
narrative_ontology:constraint_beneficiary(state_capacity_threshold, modernization_coalition).
narrative_ontology:constraint_victim(state_capacity_threshold, rural_agrarian_communities).
narrative_ontology:constraint_victim(state_capacity_threshold, traditional_calendar_artisans).
narrative_ontology:constraint_vindicates(state_capacity_threshold, state_modernization_doctrine).
narrative_ontology:constraint_vindicates(state_capacity_threshold, synchronization_with_western_standards).
narrative_ontology:constraint_vindicates(state_capacity_threshold, rational_administrative_universality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Meiji regime controls state printing, education curricula, administrative employment, and law enforcement. Issues calendar decree (1873), enforces compliance through licensing and dress codes, controls official time-setting. Benefits from unified administrative time and international trade synchronization. Can exit traditional standards by leveraging new calendar for prestige and commerce access.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, meiji_state_apparatus, agenda_setter,
    institutional, immediate, arbitrage, national).

% Farmers rely on lunar calendar for crop timing, festival scheduling, and kinship obligations. Gregorian calendar is pedagogically foreign — children learn it in state schools, but seasonal knowledge remains embedded in lunar terms. Cannot exit Meiji jurisdiction. Bear full cost of knowledge disruption without functionally improved agricultural outcomes (Gregorian calendar requires new learned practices without agronomic advantage at farm scale).
narrative_ontology:constraint_stakeholder(state_capacity_threshold, rural_agrarian_communities, payer,
    powerless, biographical, trapped, local).

% Calendar makers, printers, and textile workers producing traditional-calendar almanacs. State controls printing licenses and paper supply, making independent publication impossible. Some can adapt by producing Gregorian calendars or Western dress textiles, but expertise in traditional systems becomes commercially worthless. Constrained by economic dependency on state-licensed commerce.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, traditional_calendar_artisans, payer,
    moderate, biographical, constrained, regional).

% Western trading partners (British, American, Dutch merchants in treaty ports) require calendar and dress code standardization for commercial predictability and cultural recognition. Japan's adoption of Western standards enables treaty negotiations and market integration. Foreign commerce framework benefits through reduced transaction costs; can move capital and trading focus if Japan fails to standardize, hence mobile exit.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, foreign_commerce_framework, beneficiary,
    institutional, immediate, mobile, global).

% Foreign-educated bureaucrats, Meiji intellectuals, and commercial reformers (Fukuzawa Yukichi cohort) see calendar standardization as temporary transitional enforcement toward full international integration. Believe compliance will become habitual within 20-30 years, reducing enforcement need. Set the agenda for decree through influence on Meiji oligarchs; benefit from prestige of modernization success. Mobile exit through career advancement in modernized institutions.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, modernization_coalition, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(state_capacity_threshold, modernization_coalition, beneficiary).

% Religious practitioners and community elders who maintained traditional calendars through shrine almanacs and oral transmission. Excluded from post-decree decision-making; their authority to set time is transferred to state administration. Permitted to maintain traditional calendars for ceremonial purposes (shrine festivals, family observances) provided they do not challenge state calendar authority. Constrained by state permission rather than direct enforcement — a theatrical arrangement.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, shrine_keepers_folk_practitioners, excluded,
    organized, generational, constrained, local).

% The Gregorian calendar as a technical standard, not an actor, but included to note that calendar standardization at global scale is often naturalized as inevitable necessity rather than contingent arrangement sustained by Western economic dominance. Analysis treats this pseudo-agent as an 'observer' role to flag the risk of false-summit classification.
narrative_ontology:constraint_stakeholder(state_capacity_threshold, western_calendar_standard, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(state_capacity_threshold, western_calendar_standard).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unify Japanese administrative timekeeping with Western standards to enable state-scale bureaucratic coordination and international treaty synchronization. Solve the coordination problem of heterogeneous local calendar systems making national administration impossible and international commerce unpredictable.
% TRANSFER_FUNCTION: The constraint moves epistemic authority (from distributed lunar-calendar practitioners to centralized state time-setting), knowledge value (from lunar calendar expertise to Gregorian calendar literacy), and trade access (from isolated-region production to integrated foreign-commerce networks). Transfers from rural communities and traditional artisans to state apparatus and foreign trade merchants.
% ABSENT_VOICES: Astronomical observers and agricultural specialists who might have advocated for hybrid systems (Gregorian calendar for administration, lunar calendar for farming) are excluded — their input is not solicited in the decree. Rural voices are structurally unheard (powerless/trapped status). These absent voices represent the road not taken: a gradual, voluntary adoption pathway in which hybrid calendars persisted in agrarian regions while state administration used Gregorian time.
% DISAPPEARANCE_RATIONALE: If the calendar standardization decree had never been issued and Meiji Japan had permitted voluntary adoption, the outcome would differ substantially: (1) lunar calendars would persist in rural areas with Gregorian time used only in state/foreign contexts, creating long-term friction; (2) state administrative efficiency would be lower, requiring complex calendar conversion; (3) Japan's international prestige and treaty negotiation capacity would depend on proving 'modernity' through other means. The constraint's disappearance would rearrange Japan's institutional trajectory — either slower modernization or alternative paths to proving international legitimacy.
% FOUNDING_PROBLEM: Meiji leadership perceived a coordination failure: Japan's traditional calendar systems were incompatible with Western time standards (which dominated international commerce and diplomacy). The founding problem was not agronomic (traditional calendars worked fine for farming) but political: Japan's inability to synchronize with Western powers threatened its treaty negotiation standing and international legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Founding problem attested by: Foreign Ministry memoranda citing calendar incompatibility in treaty negotiations (1870-1872); Meiji oligarchs' writings on synchronization with 'civilized nations'; Japanese merchants in treaty ports citing Western trade friction from calendar differences. No contemporary rural voice attests that the founding problem was urgent at agrarian scale — corroboration comes exclusively from state/commercial seats.
narrative_ontology:disappearance_verdict(state_capacity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(state_capacity_threshold, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL AGRARIAN COMMUNITY (SNARE) — Agricultural timing and festival calendars deeply embedded in harvest cycles and kinship networks. Farmers cannot exit Gregorian calendar imposition without severing epistemic ties to seasonal knowledge. Trapped by territorial governance and inability to coordinate alternative calendar maintenance. No exit path; maximum experienced extraction as coordination function (lunar-based harvest timing) is dismantled without functionally replacing its agronomic role.
constraint_indexing:constraint_classification(state_capacity_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: TRADITIONAL ARTISANS AND MERCHANTS (TANGLED ROPE) — Textile workers, calendar makers, and local merchants experience forced standardization as both constraint and partial opportunity. Gregorian calendar adoption opens foreign trade networks (benefit) while destroying local calendar expertise markets (cost). High enforcement requirement for compliance; constrained exit due to economic dependency on state-regulated commerce. Mixed extraction and coordination.
constraint_indexing:constraint_classification(state_capacity_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MEIJI STATE ADMINISTRATIVE APPARATUS (ROPE) — Gregorian calendar and Western dress standardization solve genuine coordination problems: synchronizing Japan with Western trade networks, enabling standardized administrative record-keeping, and creating uniform legal/bureaucratic time. State experiences constraint as pure coordination mechanism with net benefit. Arbitrage exit (can leverage standardization to extract international prestige) and institutional power enable this reading.
constraint_indexing:constraint_classification(state_capacity_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOREIGN COMMERCE INTEGRATION FRAMEWORK (TANGLED ROPE) — Western trading partners require calendar and dress code standardization for commercial predictability and cultural legibility. Japan's merchant class and administrative apparatus benefit from integration (trade access, capital flows) while bearing costs of traditional expertise obsolescence. Foreign integration constrains Japan's exit from Western standards — reversing the calendar would damage trade relationships. Active enforcement of synchronization through treaty obligations and market incentives.
constraint_indexing:constraint_classification(state_capacity_threshold, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: VESTIGIAL TRADITIONAL CALENDAR AUTHORITY (PITON) — Prior to Meiji, lunar/traditional calendars were maintained through official almanac systems and priestly authority. Post-imposition, traditional calendar knowledge persists as hobby, family practice, and religious ritual but is stripped of administrative function. Theaters of maintenance (shrine festivals preserving lunar timing, folk calendar books) continue with no actual coordinating role. Theater ratio (0.55) reflects the performative persistence of traditional calendars after functional displacement.
constraint_indexing:constraint_classification(state_capacity_threshold, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: MODERNIZATION COALITION (SCAFFOLD) — Meiji intellectuals, foreign-educated bureaucrats, and commercial reformers see calendar standardization as temporary transitional enforcement (sunset: full international integration, ~30 years) toward a stable state where Western standards are internalized as legitimate. Once embedded in education systems and commerce, external enforcement becomes unnecessary. Sunset logic: enforcement intensity should decrease as compliance becomes habitual and culturally naturalized.
constraint_indexing:constraint_classification(state_capacity_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — NATURAL LAW VIEW (MOUNTAIN) — From a civilizational vantage, global time synchronization is an irreducible feature of planetary administration and commerce: shared calendars are logically necessary for coordination at global scale. This perspective naturalizes the Gregorian calendar as an inevitable convergence toward functional optimality. However, the structural data reveals this as a false summit: the 'naturalness' of Western calendar standards depends on Western economic dominance and state capacity to enforce adoption — non-Western alternatives (Islamic calendar, traditional seasonal timing systems) remain functionally adequate outside the Western commerce framework. The mountain classification masks contingent power asymmetry.
constraint_indexing:constraint_classification(state_capacity_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_capacity_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_capacity_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_capacity_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_capacity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(state_capacity_threshold, TR),
    TR >= 0.70.

:- end_tests(state_capacity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65 baseline): High. The state captures substantial benefits from calendar standardization (administrative unity, trade integration, international prestige) while rural and artisan communities bear knowledge displacement costs without compensation. However, extractiveness is not as severe as pure extraction (snare-level ~0.85) because the constraint does provide genuine coordination function — unified time is functionally superior for state-scale administration and international commerce. Suppression (0.70): High. Enforcement mechanisms include legal penalties for non-compliance, exclusion from administrative employment for dress code violations, reframing of traditional calendars as 'feudal superstition,' and control of printing to prevent alternative calendar publication. Barriers to exit include territorial governance (cannot flee jurisdiction), economic dependency (state controls commerce licensing), and epistemic capture (education system teaches Gregorian calendar as objective standard). Theater ratio (0.55 at t=10, declining to 0.50 at t=20): Moderate-high. Compliance rituals develop (formal dress codes for official contexts, Gregorian calendar for administration, traditional calendars for ceremony) creating performative separation between public standardization and private practice. The theater represents the state's tolerance of vestigial traditional calendars provided they remain ceremonial and do not challenge administrative authority. The decline from peak theater (0.55) to normalized theater (0.50) reflects that within 20 years, the theater has become so embedded that distinction between 'authentic' and 'performative' blurs — Gregorian calendar is simply how time works, traditional calendars are how heritage is expressed, no longer contradiction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows the sharpest perspectival divergence when the observer position changes EXIT OPTIONS rather than power level. At the same temporal horizon (biographical), a powerless/trapped agent classifies as Snare (maximum experienced extraction) while a powerful/arbitrage agent classifies as Rope (pure coordination). The gap is NOT explained by power asymmetry alone — a moderate/constrained artisan sees Tangled Rope precisely because they have some agency and some benefit, placing them structurally between the trapped farmer and the arbitrage-enabled state. The analytical observer's Mountain classification is a false summit: it naturalizes what is actually a contingent arrangement dependent on state enforcement capacity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation follows from beneficiary/victim declarations + power + exit. Meiji state (institutional, arbitrage): declared as beneficiary, has high exit options (can lever standardization internationally), derives d ≈ 0.15. Rural farmers (powerless, trapped): declared as victims, have zero exit options (territorial governance), derives d ≈ 0.95. Artisans (moderate, constrained): declared as victims (expertise markets destroyed) but also partial beneficiaries (new markets in Western goods), constrained exit (dependent on state commerce licensing) derives d ≈ 0.65. The framework's directionality chain (beneficiary + exit → low d; victim + trapped → high d) produces these values without override.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy resolution: The constraint's mandate ('synchronize Japan with Western global standards') has NOT outlived its function — international calendar synchronization remains essential for commerce, diplomacy, and planetary administration through the analysis period (t=0-20) and beyond. However, the ENFORCEMENT mandate has become partially obsolete: after t=10, compliance is so internalized that suppression_requirement can decline (0.70→0.35) while extractiveness remains moderate (0.65→0.50). This is the Scaffold diagnosis: the constraint was correctly classified as temporary enforcement with a sunset, but the sunset is measured in decades of gradually declining suppression, not sudden termination. The theater ratio stabilizes because the constraint has successfully transformed from 'coercive replacement of one reading with another' (theater ~0 at t=0, pure authentic resistance) to 'internalized reading with vestigial performance of the displaced one' (theater ~0.50 at t=20, both treated as normal). The mandatrophy is NOT resolved — the founding mandate persists — but the enforcement intensity has declined substantially, which is the expected trajectory for a successfully internalized reading replacement. The persistence of theater (not declining below 0.45) reflects that even successful reading replacement leaves traces of the displaced commitment (shrine festivals, folk calendar knowledge), maintained theatrically rather than functionally.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    endogenous_vs_exogenous_adoption,
    'Did Gregorian calendar adoption occur through endogenous climb (fringe adoption spreading to center) or exogenous override (state imposition from above)?',
    'Granular historical record analysis: dating of voluntary adoption vs state decree; geographic spread pattern (did adoption cluster around foreign trade zones or spread uniformly?); measurement of adoption compliance before vs after enforcement; analysis of adoption rate across social strata before penalties were imposed.',
    'If endogenous: constraint is legitimacy-based (Rope or Scaffold with low suppression). If exogenous: constraint is coercive (Snare or Tangled Rope with high suppression and theater). This test directly challenges whether M-set framework requires a ''top-down imposition'' cell.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(endogenous_vs_exogenous_adoption, empirical, 'Whether calendar adoption was endogenous fringe-to-center or exogenous state-imposed').

omega_variable(
    functional_replacement_sufficiency,
    'Did Gregorian calendar standardization actually improve agronomic coordination, or did it merely impose administrative synchronization at the cost of ecological knowledge?',
    'Comparative analysis of harvest timing under lunar vs Gregorian systems; crop yield data pre- and post-standardization; regional variation in agricultural outcomes based on compliance intensity; reconstruction of information loss from calendar transition (seasonal knowledge embedded in lunar terms).',
    'If functionally sufficient: coordination function is real (Rope or Tangled Rope legitimate). If administratively imposed at knowledge cost: extraction mechanism is primary and coordination is secondary cover (Snare or Piton reclassification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_replacement_sufficiency, empirical, 'Whether Gregorian calendar replaced lunar system functionally or administratively').

omega_variable(
    natural_law_vs_constructed_synchronization,
    'Is global time synchronization a natural law (inherent to coordinating large systems) or a constructed constraint (contingent on Western economic dominance)?',
    'Historical comparison: did non-Western economies adopt Western calendars before or after Western trade dominance? Analysis of functional alternatives: can Islamic calendar, traditional seasonal timing, or astronomical/agrarian systems provide equivalent coordination? Counterfactual: if Western dominance had shifted 50 years later, would calendar adoption follow different pattern?',
    'If natural law: Mountain classification confirmed; state capacity merely implements inevitable necessity. If constructed: false summit detected; ''naturalness'' masks beneficiary-driven normalization of contingent arrangement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_synchronization, conceptual, 'Whether time synchronization is natural law or beneficiary-driven normalization').

omega_variable(
    theater_persistence_mechanism,
    'Why do traditional calendars persist theatrically (shrine festivals, folk practices) after administrative displacement? Is persistence cultural resilience or enforced nostalgia?',
    'Ethnographic and historical documentation: Are traditional calendar practices actively defended by communities or tolerated as harmless ritual? Do state policies incentivize, suppress, or ignore traditional calendar maintenance? Do practitioners experience traditional calendars as identity-affirming or as vestigial performance?',
    'If cultural resilience: Piton perspective is incomplete; traditional system maintains partial functional role. If enforced nostalgia: Piton perspective confirmed; state permits ceremonial persistence while enforcing administrative supremacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_persistence_mechanism, empirical, 'Whether traditional calendar persistence is resilience or state-permitted performance').

omega_variable(
    commitment_system_gap_m_set,
    'Does the absence of a ''top-down imposition'' cell in the M-set framework represent a structural requirement (exogenous override is not a distinct commitment mode) or an analytical blind spot?',
    'Comparative analysis of commitment system kernels: Can top-down reading replacement (state imposing new calendar interpretation) be adequately modeled through existing M-set cells (legitimacy-shift, practice-drift, authority-collapse)? Or does the Meiji case require a new cell describing state capacity to impose readings against endogenous resistance?',
    'If structural requirement: existing framework is complete; Meiji imposition is a hybrid of existing cells. If analytical gap: framework requires new cell for exogenous override; state capacity is a distinct commitment-system dynamic.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commitment_system_gap_m_set, conceptual, 'Whether M-set framework gap is structural or analytical blind spot').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_capacity_threshold, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sct_theater_t0_authentic_practice, state_capacity_threshold, theater_ratio, 0, 0.2).
narrative_ontology:measurement(sct_theater_t5_compliance_ritual, state_capacity_threshold, theater_ratio, 5, 0.4).
narrative_ontology:measurement(sct_theater_t10_peak_theater, state_capacity_threshold, theater_ratio, 10, 0.55).
narrative_ontology:measurement(sct_theater_t20_vestigial_tradition, state_capacity_threshold, theater_ratio, 20, 0.5).

% Extraction over time
narrative_ontology:measurement(sct_extr_t0_pre_decree, state_capacity_threshold, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sct_extr_t1_decree_issued, state_capacity_threshold, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(sct_extr_t5_enforcement_peak, state_capacity_threshold, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(sct_extr_t10_internalization_begins, state_capacity_threshold, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(sct_extr_t20_normalized, state_capacity_threshold, base_extractiveness, 20, 0.5).

% Suppression requirement over time
narrative_ontology:measurement(sct_supp_t0_pre_decree, state_capacity_threshold, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(sct_supp_t1_decree_issued, state_capacity_threshold, suppression_requirement, 1, 0.65).
narrative_ontology:measurement(sct_supp_t5_enforcement_peak, state_capacity_threshold, suppression_requirement, 5, 0.7).
narrative_ontology:measurement(sct_supp_t10_internalization, state_capacity_threshold, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(sct_supp_t20_habitual_compliance, state_capacity_threshold, suppression_requirement, 20, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_capacity_threshold, information_standard).
narrative_ontology:affects_constraint(state_capacity_threshold, meiji_dress_code_standardization).
narrative_ontology:affects_constraint(state_capacity_threshold, state_epistemic_authority).
narrative_ontology:affects_constraint(state_capacity_threshold, traditional_knowledge_displacement).

% DUAL FORMULATION NOTE:
% The calendar standardization constraint and the dress code constraint are structurally distinct (different ε values: calendar is coordination + enforcement, dress is primarily enforcement/social standardization), but both instantiate the same commitment system reading replacement (Meiji modernization doctrine). Decomposed into separate stories per ε-invariance principle; linked via affects_constraints to show network dependency — dress code enforcement reinforces calendar standardization legitimacy through unified 'modernity' framing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
