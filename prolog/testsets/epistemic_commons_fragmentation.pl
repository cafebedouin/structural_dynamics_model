% ============================================================================
% CONSTRAINT STORY: epistemic_commons_fragmentation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_commons_fragmentation, []).

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
 *   constraint_id: epistemic_commons_fragmentation
 *   human_readable: Epistemic Commons Fragmentation
 *   domain: epistemology/information_systems/social_coordination
 *
 * SUMMARY:
 *   The epistemic commons — the shared, accessible knowledge base enabling
 *   communities to build understanding collectively — is fragmenting across
 *   multiple structural axes simultaneously. Proprietary platforms gatekeep
 *   access (paywalls, closed APIs, algorithmic opacity), institutional
 *   credentialism concentrates authority in traditional journals and
 *   universities, network effects reward platform lock-in over
 *   interoperability, and attention scarcity creates competition for
 *   visibility rather than collaboration. This constraint exhibits the full
 *   DR spectrum: from the standpoint of the abstract epistemic commons
 *   (powerless), fragmentation is pure extraction; from the standpoint of
 *   platform operators (institutional), it is coordination enabling scale;
 *   from organized movements building open alternatives (organized), it is a
 *   temporary problem with a sunset; from legacy gatekeepers
 *   (institutional/constrained), it is degraded ritual persisting through
 *   inertia; from knowledge seekers (moderate), it is mixed extraction and
 *   coordination. The constraint's extractiveness has risen from 0.32 to 0.58
 *   over the interval as proprietary platforms have consolidated market
 *   power, algorithmic opacity has increased, and open-access alternatives
 *   have fragmented rather than unified. Theater ratio has risen from 0.48 to
 *   0.68 as credentialing mechanisms have become increasingly performative —
 *   the social authority of traditional publishing persists despite declining
 *   functional necessity.
 *
 * KEY AGENTS:
 *   - Platform Operators (institutional/arbitrage): Primary beneficiaries — capture network effects, data value, and attention rent through proprietary curation and algorithmic gatekeeping
 *   - Epistemic Commons (powerless/trapped): Primary victim — the collective knowledge base as abstract entity cannot organize, negotiate, or exit; bears full cost of fragmentation
 *   - Knowledge Seekers / Independent Researchers (moderate/constrained): Secondary victims and secondary beneficiaries — face gatekeeping barriers but access specialized knowledge through fragmented platforms; benefit from network effects but pay extraction rent
 *   - Open Knowledge Movement (organized/mobile): Organized agents building alternatives — Wikipedia, ArXiv, open-access publishing, blockchain-based systems, decentralized protocols; have agency and see a sunset
 *   - Traditional Academic Publishers (institutional/constrained): Institutional actor experiencing constraint as degraded ritual — gatekeeping authority persists through credentialism despite loss of functional necessity
 *   - Algorithmic Curation Systems (analytical observer role): Abstract systems that mediate knowledge access — risk naturalizing information scarcity as inherent cognitive limit rather than engineered selection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_commons_fragmentation, 0.58).
domain_priors:suppression_score(epistemic_commons_fragmentation, 0.62).
domain_priors:theater_ratio(epistemic_commons_fragmentation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_commons_fragmentation, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_commons_fragmentation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_commons_fragmentation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_commons_fragmentation, tangled_rope).
narrative_ontology:human_readable(epistemic_commons_fragmentation, "Epistemic Commons Fragmentation").
narrative_ontology:topic_domain(epistemic_commons_fragmentation, "epistemology/information_systems/social_coordination").

domain_priors:requires_active_enforcement(epistemic_commons_fragmentation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_commons_fragmentation, platform_operators).
narrative_ontology:constraint_beneficiary(epistemic_commons_fragmentation, algorithmic_gatekeepers).
narrative_ontology:constraint_beneficiary(epistemic_commons_fragmentation, proprietary_knowledge_holders).
narrative_ontology:constraint_victim(epistemic_commons_fragmentation, epistemic_commons_reliability).
narrative_ontology:constraint_victim(epistemic_commons_fragmentation, knowledge_seekers).
narrative_ontology:constraint_victim(epistemic_commons_fragmentation, independent_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS AS POWERLESS VICTIM (SNARE) — The shared knowledge base has no agent, no exit option, and no defense against fragmentation. Bears the full cost of siloed information systems, algorithmic curation opacity, and proprietary knowledge enclosure. Cannot organize, cannot negotiate, cannot leave. Maximum experienced extraction from a structurally powerless position.
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: KNOWLEDGE SEEKER / INDEPENDENT RESEARCHER (TANGLED ROPE) — Constrained by gatekeeping (paywalls, platform algorithms, proprietary databases), but also benefits from the fragmented ecosystem: specialized communities develop deep expertise, open-source projects provide alternatives, and network effects create coordination benefits alongside extraction. Exit costs are real (relocation to alternative platforms, loss of reach) but not insurmountable. Mixed extraction and coordination.
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR / ALGORITHMIC GATEKEEPER (ROPE) — Experiences the constraint as coordination: algorithms mediate access to knowledge at scale, platform infrastructure enables information exchange that would not exist without curation. High arbitrage options — can switch between platforms, licensing models, and distribution strategies. Net beneficiary, but benefits flow through coordination function (curated access enables scale).
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN KNOWLEDGE MOVEMENT / DECENTRALIZATION COALITION (SCAFFOLD) — Organized agents (open-access publishing, Wikipedia, ArXiv, blockchain-based knowledge systems, decentralized protocols) see fragmentation as a temporary coordination failure with architectural solutions. Low effective extraction because organized agents have agency and see a genuine sunset: federated systems, semantic web standards, and open knowledge infrastructure are building alternatives to proprietary curation. Sunset timeline: 15-30 years as decentralized architectures mature.
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ACADEMIC PUBLISHING SYSTEM (PITON) — Traditional peer-reviewed journals maintain gate-keeping authority largely through institutional inertia: universities still count journal publications in tenure decisions, funding agencies still weight journal impact factors, and credentialism persists despite open alternatives. The ritual persists as performative authority despite declining functional necessity. Theater ratio is high because the gatekeeping authority (journal impact factor, editorial board prestige) is substantially theatrical — it validates knowledge through social ritual rather than through distributed verification. The publishing system itself sees this as degraded (journals acknowledge crisis, announce open access initiatives) but persists because alternatives haven't fully replaced institutional credentialism.
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — A natural law reading claims that fragmentation is inherent to knowledge systems: cognitive specialization requires bounded domains, information overload forces filtering, and trust networks naturally partition. Some aspects are genuinely immutable (humans have finite attention), but the engine will detect this as a false summit — the structural data reveals that institutional arrangements (proprietary APIs, paywalls, algorithmic opacity, credentialism) are contingent, not natural laws. The constraint naturalize human cognitive limits to justify engineered scarcity.
constraint_indexing:constraint_classification(epistemic_commons_fragmentation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_commons_fragmentation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_commons_fragmentation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_commons_fragmentation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_commons_fragmentation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_commons_fragmentation, TR),
    TR >= 0.70.

:- end_tests(epistemic_commons_fragmentation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. Proprietary platforms capture value from knowledge exchange through attention monetization, data extraction, and licensing fees. The extractiveness trend (0.32 → 0.58 over 9 periods) reflects market consolidation: platform concentration has increased, alternative knowledge systems have fragmented rather than coalesced, and the cost of independent knowledge discovery has risen. Suppression (0.62): High. Multiple barriers prevent exit or alternative knowledge infrastructure: API lock-in (switching platforms requires rebuilding networks), paywalls and subscription barriers, algorithmic opacity preventing knowledge discovery outside platform curation, and credentialist gatekeeping (publication in non-prestigious outlets counts less). Theater ratio (0.68): High and rising. Academic credentialism (journal impact factor, editorial board prestige, citation counts) is substantially performative — it validates knowledge through social ritual rather than through independent verification or practical utility. Algorithmic recommendation systems are theatrical in a different sense: the algorithms present themselves as objective (data-driven, neutral) while actually embedding commercial incentives (engagement maximization, advertiser goals). Claimed type (tangled_rope): The constraint coordinates knowledge access at scale (platforms enable information exchange that wouldn't exist without curation) while simultaneously extracting value (rent capture, attention monetization, credentialist gatekeeping).
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (rope) and victim (snare) perspectives is maximized: the platform operator sees coordination enabling scale; the epistemic commons sees pure extraction with no self-correction mechanism. This gap reveals the structural asymmetry: platform operators can arbitrage between different gatekeeping strategies, while the commons cannot. The scaffold perspective (organized agents) introduces a crucial rupture: organized agents see what trapped agents do not — that the constraint has a sunset and that alternatives are being built. This is the diagnostic signature of scaffold: it is the same constraint as snare, but from an agent position with agency and exit visibility. The piton perspective reveals institutional degradation: legacy publishers maintain gatekeeping authority despite reduced functionality, using credentialism as theatrical maintenance. The analytical observer risks a false summit by naturalizing what is actually engineered.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Beneficiaries (platform operators, proprietary knowledge holders) with arbitrage options derive low d (≈0.05-0.20) — they can switch platforms, licensing models, curation strategies; they experience the constraint as advantageous. Victims (epistemic commons, knowledge seekers) with limited exit options derive high d (≈0.75-0.95 for trapped commons; ≈0.55-0.75 for constrained seekers) — they face real costs of fragmentation with limited alternatives. Organized agents (open knowledge movement) with genuine exit paths and agency derive moderate d (≈0.40-0.55) — they experience extraction but have structural agency and a sunset. The sigmoid f(d) scales experienced extractiveness: low d produces negative χ (the beneficiary experiences the constraint as net beneficial); high d produces χ > ε (the trapped agent experiences extraction more severely than the baseline metric indicates). Scope modifier σ(S) = 1.2 for global scope, increasing χ slightly — global fragmentation is harder to escape than local fragmentation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint resolves the mandatrophy by showing that tangled_rope at the baseline is a partial reading of a multi-perspective constraint family. The extraction (snare from powerless perspective) and coordination (rope from institutional perspective) are both genuine structural facts — they describe different agent experiences of the same institutional arrangement. The snare is not 'wrong' and the rope is not 'wrong'; they are mutually accurate readings from different structural positions. The constraint is a snare from the epistemic commons perspective (no exit, no voice, maximum extraction), a rope from platform operators (arbitrage, coordination function), a tangled rope from knowledge seekers (mixed extraction and coordination), and a scaffold from organized movements (temporary, with a sunset). The analytical observer's false summit (naturalizing fragmentation as cognitive law) is detected through the structural data: if fragmentation were truly immutable, it would have the accessibility_collapse ≥ 0.85 signature of mountains, but it doesn't — accessibility_collapse is ≈0.45 (engineered scarcity, not natural limit), and resistance to change is ≈0.55 (institutional inertia, not immutable law). Remediation requires either regulatory intervention (breaking up platform concentration, mandating interoperability) or architectural alternatives (decentralized systems achieving critical mass). The scaffold perspective suggests that architectural alternatives are being built, which may reduce the constraint's effective extraction over the next 15-30 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fragmentation_inevitability,
    'Is epistemic fragmentation inevitable due to cognitive limits and complexity, or engineered by institutional and commercial incentives?',
    'Comparative analysis of information access patterns in pre-digital vs digital ecosystems; measurement of knowledge discovery rates in open vs proprietary systems; analysis of attention distribution in curated vs uncurated information flows',
    'If inevitable: constraint is closer to mountain (unavoidable human cognitive limit). If engineered: constraint is snare (institutional extraction masquerading as natural law). This determines whether remediation is architectural or regulatory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fragmentation_inevitability, empirical, 'Whether fragmentation is inevitable or engineered').

omega_variable(
    commons_restoration_feasibility,
    'Can decentralized and open-access architectures actually restore epistemic commons functionality, or do they create parallel fragmentation (different siloes, different gatekeepers)?',
    'Empirical measurement of information accessibility, discovery, and trust in open vs proprietary systems; analysis of fragmentation metrics (number of competing platforms, cross-platform integration, knowledge portability) across 10+ years of implementation in different domains',
    'If restoration is feasible: scaffold perspective is correct, sunset is real. If parallel fragmentation emerges: constraint persists in new form, making it closer to piton (persistent inertia) or tangled_rope (coordination always includes extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_restoration_feasibility, empirical, 'Whether open architectures can restore epistemic commons').

omega_variable(
    algorithmic_curation_necessity,
    'Is algorithmic curation a necessary coordination mechanism for knowledge access at scale, or primarily a rent-extraction mechanism dressed as filtering?',
    'Comparative analysis of information quality, discovery efficiency, and user outcomes in heavily curated vs lightly curated systems; measurement of how much algorithmic filtering contributes to knowledge discovery vs attention capture',
    'If necessary: rope perspective dominates (coordination justified). If extraction: snare perspective dominates (curation as control mechanism). This reshapes the baseline extractiveness value.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_curation_necessity, empirical, 'Whether algorithmic curation is necessary coordination or rent extraction').

omega_variable(
    identity_lock_in_knowledge_workers,
    'Are knowledge workers (researchers, scholars, journalists) trapped by material barriers to exit (career dependence on proprietary platforms, paywall access) or identity-locked (professional identity constituted through traditional gatekeepers)?',
    'Ethnographic and survey analysis of motivations for platform loyalty; measurement of post-exit trajectory when knowledge workers switch to open platforms (do they recover career status, or have they become ''not real'' researchers?); analysis of credentialism as internalized identity constraint vs material barrier',
    'If trapped: exit_options at moderate power are ''constrained'' (material barriers). If identity-locked: exit_options should be ''identity_locked'' (internalized frames prevent exit), potentially changing classifications. This reshapes the biographical horizon classification for moderate agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_knowledge_workers, empirical, 'Whether knowledge workers face material trap or identity lock').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_commons_fragmentation, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecf_tr_t0, epistemic_commons_fragmentation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ecf_tr_t3, epistemic_commons_fragmentation, theater_ratio, 3, 0.56).
narrative_ontology:measurement(ecf_tr_t6, epistemic_commons_fragmentation, theater_ratio, 6, 0.64).
narrative_ontology:measurement(ecf_tr_t9, epistemic_commons_fragmentation, theater_ratio, 9, 0.68).

% Extraction over time
narrative_ontology:measurement(ecf_be_t0, epistemic_commons_fragmentation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ecf_be_t3, epistemic_commons_fragmentation, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(ecf_be_t6, epistemic_commons_fragmentation, base_extractiveness, 6, 0.52).
narrative_ontology:measurement(ecf_be_t9, epistemic_commons_fragmentation, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_commons_fragmentation, information_standard).
narrative_ontology:affects_constraint(epistemic_commons_fragmentation, algorithmic_opacity_power_asymmetry).
narrative_ontology:affects_constraint(epistemic_commons_fragmentation, credentialism_lock_in).
narrative_ontology:affects_constraint(epistemic_commons_fragmentation, knowledge_worker_precarity).
narrative_ontology:affects_constraint(epistemic_commons_fragmentation, open_science_infrastructure).

% DUAL FORMULATION NOTE:
% Epistemic commons fragmentation is downstream of platform consolidation and architectural design choices (proprietary APIs, algorithmic curation opacity, credentialist validation). Each upstream constraint (algorithmic_opacity, credentialism, platform_lock_in) contributes to the fragmentation effect. The commons fragmentation story is the integrated view; upstream stories should link to this as a structural dependent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(epistemic_commons_fragmentation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
