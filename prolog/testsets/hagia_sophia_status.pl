% ============================================================================
% CONSTRAINT STORY: hagia_sophia_status
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hagia_sophia_status, []).

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
 *   constraint_id: hagia_sophia_status
 *   human_readable: The Enforced Religious and Political Status of the Hagia Sophia
 *   domain: religious/political/cultural
 *
 * SUMMARY:
 *   The Hagia Sophia's contested status exemplifies how a single
 *   architectural monument can embody multiple competing claims about
 *   religious identity, state authority, cultural heritage, and historical
 *   justice. Built as an Orthodox cathedral (537 CE), converted to a mosque
 *   after Ottoman conquest (1453), secularized as a museum under Turkish
 *   republicanism (1935), and reconverted to a mosque (2020), the structure
 *   has been weaponized as a symbol in ongoing contests over Turkish
 *   identity, the relationship between Islam and secularism, religious
 *   minority rights, and the meaning of cultural heritage. The 2020 decision
 *   by Turkish President Erdoğan to return the Hagia Sophia to mosque status
 *   was presented as religious restitution to the Muslim majority but
 *   experienced by Orthodox Christians and heritage advocates as forced
 *   symbolic dispossession backed by state authority. The constraint operates
 *   across multiple dimensions: religious affiliation, state control,
 *   international cultural norms, minority protection, and democratic
 *   legitimacy. Each actor perceives a different structure: Turkish state
 *   sees coordination (solving the identity-management problem), Muslim
 *   majority sees justice (restitution), Orthodox Christians see extraction
 *   (loss of pilgrimage and belonging), heritage advocates see temporary
 *   friction (scaffold with dialogue potential), and the museum apparatus
 *   appears retrospectively as performative theater that masked rather than
 *   resolved underlying claims.
 *
 * KEY AGENTS:
 *   - Turkish State: Primary beneficiary (institutional/arbitrage) — wields legal authority to enforce religious designation; uses constraint to consolidate Islamist political legitimacy
 *   - Sunni Muslim Majority in Turkey: Primary beneficiary (organized/arbitrage) — experiences constraint as restitution and religious legitimacy; benefits from demographic majority expression through state authority
 *   - Orthodox Christian Diaspora: Primary victim (powerless/trapped) — structurally unable to contest state decree; experiences total loss of primary sacred site; no alternative recourse available
 *   - UNESCO and International Heritage Advocates: Secondary victim (moderate/constrained) — capacity to protest limited by state sovereignty; benefit from visibility but cannot enforce heritage protections
 *   - Secular Heritage Constituency: Secondary victim (moderate/constrained) — Turkish secularists who value museum status face suppression of alternative vision; constrained by electoral minority status
 *   - Museum Institution (1935-2020): Institutional arrangement (institutional/constrained) — performative apparatus that delayed rather than resolved constraint; degraded by inability to sustain itself against underlying political claims
 *   - International Religious Dialogue Movement: Organized agent (organized/constrained) — perceives scaffold structure with sunset through generational norm change; has agency through cultural advocacy but limited enforcement capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hagia_sophia_status, 0.58).
domain_priors:suppression_score(hagia_sophia_status, 0.72).
domain_priors:theater_ratio(hagia_sophia_status, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hagia_sophia_status, extractiveness, 0.58).
narrative_ontology:constraint_metric(hagia_sophia_status, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hagia_sophia_status, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hagia_sophia_status, tangled_rope).
narrative_ontology:human_readable(hagia_sophia_status, "The Enforced Religious and Political Status of the Hagia Sophia").
narrative_ontology:topic_domain(hagia_sophia_status, "religious/political/cultural").

domain_priors:requires_active_enforcement(hagia_sophia_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hagia_sophia_status, turkish_state).
narrative_ontology:constraint_beneficiary(hagia_sophia_status, sunni_muslim_majority).
narrative_ontology:constraint_victim(hagia_sophia_status, orthodox_christian_minority).
narrative_ontology:constraint_victim(hagia_sophia_status, secular_heritage_advocates).
narrative_ontology:constraint_victim(hagia_sophia_status, religious_pluralism_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Orthodox Christians witness the primary sanctuary of their faith removed from their control and repurposed through state decree. Exit is structural impossibility — the site cannot be physically relocated, and religious meaning is non-transferable. Extraction is total: symbolic loss, pilgrimage denial, loss of institutional voice.
constraint_indexing:constraint_classification(hagia_sophia_status, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% Heritage advocates see coordination value (preserving universal human patrimony) but face coercive constraints: state sovereignty over territory prevents enforcement of heritage protections. Extract asymmetry exists — the constraint benefits Turkish state authority at cost of international norm compliance. But advocates also benefit from the visibility and platform the constraint provides to cultural preservation movements.
constraint_indexing:constraint_classification(hagia_sophia_status, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% State experiences the Hagia Sophia as pure coordination — solving the problem of managing a contested historical site through clear religious authority. The constraint enables the state to reconcile religious majority preference with institutional control. Exit is unnecessary (state is the enforcer). Extraction runs toward institutional capacity and legitimacy.
constraint_indexing:constraint_classification(hagia_sophia_status, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Muslim majority experiences the constraint as pure coordination — the site is returned to religious use matching demographic reality and historical restitution narratives. Extraction is minimal because the constraint solves a genuine collective action problem (managing shared sacred space). Benefits accrue through religious legitimacy and institutional representation.
constraint_indexing:constraint_classification(hagia_sophia_status, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% Multi-faith dialogue advocates see the constraint as temporary institutional friction in a longer arc toward pluralistic legitimacy. The closure to non-Muslim worship (2020-present) creates enforcement pressure, but dialogue movements perceive a sunset: generational changes in Turkish secularism and EU integration pressures may restore shared sacred space models. Low effective extraction because advocates have agency and perceive exit pathways through cultural norm evolution.
constraint_indexing:constraint_classification(hagia_sophia_status, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% The mid-20th-century 'solution' of converting Hagia Sophia to a secular museum was largely performative — theater masking the constraint rather than resolving it. Museum status satisfied neither Orthodox Christians nor Muslim majority, maintained only through state monopoly on cultural authority and international acquiescence. Theater ratio (0.68) reflects this: the museum framing was institutional theater that ultimately could not sustain itself against underlying religious and political claims.
constraint_indexing:constraint_classification(hagia_sophia_status, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% From a civilizational perspective, contested sacred sites are inherent to religious and political history — immutable features of how communities mark meaning and authority over space. The Hagia Sophia's transformation reflects inescapable structures: sovereignty cannot be shared without contradiction, religious majority claims cannot be indefinitely suppressed in democratic contexts, and symbolic significance cannot be neutralized through institutional naming. This perspective risks naturalizing what is actually a contingent political choice.
constraint_indexing:constraint_classification(hagia_sophia_status, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hagia_sophia_status_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hagia_sophia_status, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hagia_sophia_status, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hagia_sophia_status, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hagia_sophia_status, TR),
    TR >= 0.70.

:- end_tests(hagia_sophia_status_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The Turkish state extracts institutional legitimacy and political capital by aligning state authority with religious majority preference. Orthodox Christians and secular heritage advocates bear costs through loss of access and representation. However, extraction is not maximal because the constraint also solves a genuine coordination problem (the site must have some operational status), and the state does maintain the building's physical integrity. The extraction reflects state capacity to impose its vision but constrained by international pressure and minority resistance. Suppression (0.72): High. Multiple barriers prevent contestation: state monopoly on territorial control, religious majority dominance, international law's deference to state sovereignty, asymmetric information access (Turkish state controls public narrative), and structural powerlessness of diaspora communities. Barriers are reinforced through selective interpretation of Ottoman history and Islamic restitution narratives that frame the constraint as natural justice. Theater ratio (0.68): Moderately high. The 2020 conversion involved significant ritual and performative elements — religious ceremonies, state statements about historical restitution, public spectacle. But the theater has declined from the museum era (1935-2020), when the secular framing itself was largely performative theater. The current constraint is more directly extractive (less theater, more actual power asymmetry), though ritual performance remains significant. The trend shows extractiveness rising while theater declines, indicating that institutional performance has given way to direct coercive authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp divergence across three key perspectives. The Turkish state and Muslim majority experience Rope — a coordination mechanism that solves the problem of religious site management through clear institutional authority. Orthodox Christians experience Snare — they have no exit option and bear total symbolic loss. Heritage advocates experience Scaffold with dialogue potential — they see the constraint as temporary friction in a longer civilizational arc toward pluralistic institutions. The piton perspective reveals that the museum era (1935-2020) was largely theater that masked rather than resolved the underlying extraction — the secular solution could not sustain itself against claims rooted in religious identity and state power. The analytical observer risks naturalizing this as an immutable law (Mountain) — that contested sacred sites reflect inescapable historical forces — when the structural data reveals contingent political choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each agent's structural position relative to the constraint. The Turkish state as beneficiary with institutional power and full arbitrage options derives d ≈ 0.05 (near-pure beneficiary), producing negative effective extractiveness — the constraint subsidizes state legitimacy. The Sunni majority as beneficiary with organized power and arbitrage derives similar low d. Orthodox Christians as victims with powerless status and trapped exit derive d ≈ 0.95 (near-pure target), producing high effective extractiveness χ via the sigmoid. UNESCO advocates as moderate-power agents with constrained exit derive intermediate d ≈ 0.55, experiencing moderate χ. The scaffold perspective (dialogue advocates) derives d ≈ 0.40 due to organized power and perceived exit pathways through norm change, producing lower experienced extraction. The piton perspective captures institutional inertia — the museum apparatus had d ≈ 0.50 (symmetric cost-benefit) but theater_ratio = 0.75, indicating the symmetry was illusory, maintained through performative institutional work rather than genuine balance.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION STRATEGY: The constraint avoids mandatrophy (false labeling of coordination as extraction or vice versa) by disambiguating the perspectives. The Turkish state genuinely experiences coordination — the constraint solves the institutional problem of site management. The Muslim majority genuinely experiences rope-like benefits — religious legitimacy and expression. These are not false framings; they are accurate perspectives from beneficiary positions. Simultaneously, Orthodox Christians genuinely experience snare — pure extraction with no coordination benefit and no exit. The mandatrophy is resolved by accepting that the SAME constraint is Rope from beneficiary perspectives and Snare from victim perspectives. The constraint is a tangled rope (per the claimed_type) because it combines genuine coordination function (site management, religious expression) with genuine asymmetric extraction (minority dispossession, heritage norm violation). The scaffold perspective is prophylactic against future mandatrophy — it prevents naturalizing the current extraction as eternal by identifying the sunset mechanism (generational political change, dialogue norm evolution). The piton analysis of the museum era guards against false naturalization of performative theater as genuine neutrality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_possibility,
    'Is the 2020 conversion to mosque final, or does the constraint contain built-in reversibility for future political settlements?',
    'Documentary analysis of the 2020 decree; examination of legal mechanisms for reversal; tracking of political discourse and electoral pressure from secularist and heritage constituencies',
    'If reversible: constraint is genuinely temporary (scaffold) with sunset mechanism. If permanent: constraint is locked tangled rope with asymmetric extraction institutionalized indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_possibility, empirical, 'Whether the mosque conversion is reversible or permanent').

omega_variable(
    pluralistic_coexistence_feasibility,
    'Can shared sacred space arrangements (overlapping use schedules, architectural partitioning, interfaith governance) actually function at Hagia Sophia''s scale and symbolic significance?',
    'Case study analysis of other shared sacred sites (Church of the Holy Sepulchre, Temple Mount management models); technical assessment of architectural compatibility with simultaneous religious uses',
    'If feasible: exit pathway exists beyond binary control, reducing effective extraction. If infeasible: constraint is inherently zero-sum, confirming snare classification for losers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pluralistic_coexistence_feasibility, empirical, 'Whether pluralistic coexistence models can function at Hagia Sophia').

omega_variable(
    turkish_secularism_trajectory,
    'Is Turkey''s religious policy direction durable under current state leadership, or does political cycling create vulnerability to institutional reversal?',
    'Multi-decade trend analysis of Turkish electoral cycles, constitutional amendments, and religious policy evolution; comparison to neighboring regional states'' religious policy cycles',
    'If durable: constraint is locked in (high suppression persists). If cyclical: constraint exhibits conditional stability, reducing long-term suppression estimates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(turkish_secularism_trajectory, preference, 'Durability of Turkish religious policy trajectory').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hagia_sophia_status, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hagia_tr_t0, hagia_sophia_status, theater_ratio, 0, 0.75).
narrative_ontology:measurement(hagia_tr_t50, hagia_sophia_status, theater_ratio, 50, 0.7).
narrative_ontology:measurement(hagia_tr_t100, hagia_sophia_status, theater_ratio, 100, 0.68).

% Extraction over time
narrative_ontology:measurement(hagia_be_t0, hagia_sophia_status, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hagia_be_t50, hagia_sophia_status, base_extractiveness, 50, 0.5).
narrative_ontology:measurement(hagia_be_t100, hagia_sophia_status, base_extractiveness, 100, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hagia_sophia_status, enforcement_mechanism).
narrative_ontology:affects_constraint(hagia_sophia_status, temple_mount_shared_access).
narrative_ontology:affects_constraint(hagia_sophia_status, greek_orthodox_minority_rights_turkey).
narrative_ontology:affects_constraint(hagia_sophia_status, ottoman_cultural_heritage_restitution).

% DUAL FORMULATION NOTE:
% The Hagia Sophia constraint operates at the intersection of religious identity, state sovereignty, and heritage norms. Upstream constraints include Ottoman historical restitution claims and Turkish religious demographics (higher-extractiveness, these shape the environment). Downstream constraints include minority rights protections and heritage norm compliance (these are affected by the current enforcement). The constraint family maps onto the broader question of how contested sacred sites are governed under competing claims of religious authority, historical justice, and pluralistic coexistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hagia_sophia_status, moderate, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
