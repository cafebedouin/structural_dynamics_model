% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_protection, []).

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
 *   constraint_id: copyright_protection
 *   human_readable: Copyright Protection Framework
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   Copyright protection represents a foundational constraint on knowledge
 *   and cultural production. Framed as a legal mechanism to incentivize
 *   creation, it grants creators and rights holders exclusive control over
 *   reproduction and distribution of original works. This framework exhibits
 *   the full spectrum of Deferential Realism classifications across different
 *   stakeholder perspectives. Original creators experience copyright as
 *   genuine coordination—establishing attribution rights, enabling licensing
 *   negotiation, and providing income streams. Publishing corporations
 *   experience it as a pure extraction mechanism, using copyright to suppress
 *   competition and control markets. Derivative creators, educators, and
 *   cultural commons advocates experience it as suppression that constrains
 *   their ability to build on existing works. The constraint's extractiveness
 *   has increased over the 50-year interval as copyright terms have extended
 *   (Sonny Bono Act, international harmonization), anticircumvention
 *   provisions have proliferated (DMCA), and enforcement technology has
 *   improved. Theater ratio has risen as registration and bureaucratic
 *   procedures persist despite digital metadata reducing their necessity, and
 *   as copyright enforcement becomes increasingly performative—takedown
 *   notices, litigation theater—rather than focused on actual infringement.
 *
 * KEY AGENTS:
 *   - Original Creators: Primary beneficiary (institutional/arbitrage) — gain coordination mechanism for licensing and attribution; possess exit options (licensing, direct sales, alternative platforms)
 *   - Publishing Corporations: Institutional beneficiary-with-control (institutional/arbitrage) — use copyright as extraction lever; possess maximal exit options and institutional power
 *   - Derivative Creators: Primary victim (powerless/trapped) — cannot legally build on existing works without permission; face licensing costs and legal barriers; trapped within framework
 *   - Educational Institutions: Secondary victim (powerless/trapped) — restricted in reproducing materials for teaching; face licensing fees and compliance burdens; trapped in budget constraints
 *   - Cultural Commons: Tertiary victim (powerless/trapped) — preservation and access constrained by copyright restrictions; cannot legally maintain cultural heritage archives
 *   - Open Culture Coalition: Organized agent (organized/constrained) — librarians, digital archivists, open-access advocates; some agency through alternative licensing but subordinate to copyright default
 *   - Copyright Bureaucracy: Institutional maintainer (institutional/arbitrage) — registration offices, Copyright Offices; maintains theatrical procedures; possessed of full agency but committed to status quo
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_protection, 0.52).
domain_priors:suppression_score(copyright_protection, 0.68).
domain_priors:theater_ratio(copyright_protection, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.52).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(copyright_protection, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_protection, tangled_rope).
narrative_ontology:human_readable(copyright_protection, "Copyright Protection Framework").
narrative_ontology:topic_domain(copyright_protection, "economic/legal/technological").

domain_priors:requires_active_enforcement(copyright_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_protection, original_creators).
narrative_ontology:constraint_beneficiary(copyright_protection, publishing_corporations).
narrative_ontology:constraint_beneficiary(copyright_protection, rights_administrators).
narrative_ontology:constraint_victim(copyright_protection, derivative_creators).
narrative_ontology:constraint_victim(copyright_protection, educational_institutions).
narrative_ontology:constraint_victim(copyright_protection, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DERIVATIVE CREATOR (SNARE) — Artists, scholars, and educators who want to build on existing work face legal barriers that suppress alternatives. Cannot exit without abandoning their creative practice. Licensing costs, permission requirements, and litigation risk create severe suppression. Maximum extraction experienced by those without resources to navigate legal complexity.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATIONAL INSTITUTION (SNARE) — Schools and universities face copyright restrictions on materials needed for teaching. Cannot legally reproduce copyrighted works for classroom use without licensing fees. Trapped within budget constraints and institutional compliance requirements. Extraction flows toward rights holders; educational mission is subordinated.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: OPEN CULTURE COALITION (TANGLED ROPE) — Librarians, digital archivists, and open-access advocates possess some agency through licensing alternatives (Creative Commons, copyleft), but face institutional headwinds and remain subordinate to copyright law's default restrictiveness. Extraction persists because copyright extends legal reach beyond the coalition's institutional reach. Both coordination function (enabling attribution, enabling remixing) and asymmetric extraction (licensing requirements still apply) present.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ORIGINAL CREATOR (ROPE) — Individual artists and authors benefit from copyright's coordination function: it establishes attribution rights, enables contract negotiation, and provides a mechanism for royalties. For creators of mass-appeal work, this is genuine coordination enabling viable creative careers. Exit options are high (licensing, direct sales, alternative platforms). Net beneficiary.
constraint_indexing:constraint_classification(copyright_protection, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PUBLISHING CORPORATION (SNARE) — Large publishing, entertainment, and software corporations use copyright as a pure extraction mechanism. They acquire rights through contracts, extend monopoly control through lobbyists, and suppress competitive formats (DRM, circumvention bans). Exit options are maximal — they can license, sue, lobby, or shift business models. Effective extraction is high because they extract from trapped victims while possessing full agency.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT BUREAUCRACY (PITON) — Registration offices, licensing administrators, and Copyright Offices maintain elaborate theatrical procedures (notice, registration, deposit) that persist despite digital metadata making most steps unnecessary. The bureaucracy sees its own function as partly degraded — it continues because institutional inertia and international treaties lock in procedures, not because they efficiently serve creators. Theater ratio is moderate-high.
constraint_indexing:constraint_classification(copyright_protection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Copyright's stated purpose is coordinating creator incentives (genuine function). But in practice it also enables corporate rent extraction and suppresses cultural commons. The analytical observer sees BOTH the coordination mechanism (attribution, contract framework, incentive alignment) AND the asymmetric extraction (duration extensions, anticircumvention bans, restrictions on fair use). This mixed character is irreducible.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_protection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_protection, TR),
    TR >= 0.70.

:- end_tests(copyright_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Copyright's extractiveness is moderate-to-high because it combines genuine coordination function (enabling creator compensation) with significant asymmetric extraction (duration extensions, corporate consolidation, suppression of derivative works, anticircumvention restrictions). The 50-year trajectory shows extractiveness rising from 0.28 to 0.52 as copyright terms have extended repeatedly, anticircumvention provisions proliferated, and corporate consolidation increased. Suppression (0.68): High. Copyright law eliminates alternatives to permission-based use through legal prohibition. Derivative creators cannot build on works; educators cannot reproduce materials; archivists cannot preserve; technologists cannot interoperate. These are not natural limits but legal suppression. DRM circumvention bans extend suppression into technical domains. Theater ratio (0.55): Moderate. Copyright bureaucracy (registration, deposit, notice) maintains theatrical procedures despite digital metadata reducing necessity. Enforcement is increasingly performative—takedown notices, litigation theater—relative to actual infringement prevention. But copyright still has real coordination function, preventing theater ratio from being very high.
 *
 * PERSPECTIVAL GAP:
 *   Original creators and corporations diverge sharply from derivative creators and educators. The beneficiaries perceive coordination and legitimate reward; the victims perceive suppression and unjust barrier. The open culture coalition perceives contingent constraint with emergent alternatives. The copyright bureaucracy perceives its own institutional degradation. These are not measurement artifacts but structural realities of being in different positions within the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across stakeholders based on their structural position. Original creators are genuine beneficiaries with high exit options (arbitrage), yielding low d and negative effective extraction (they experience coordination). Publishing corporations are institutional beneficiaries with arbitrage options, similarly yielding low structural d. Derivative creators are powerless victims with no exit (trapped), yielding very high d and maximum experienced extraction. Educators are powerless victims with constrained but not trapped exit (some workarounds exist), yielding high d. The open culture coalition is organized with constrained exit, yielding moderate d. The copyright bureaucracy is institutional with arbitrage options, yielding low d despite maintaining the structure. The directionality derivation from beneficiary/victim declarations plus exit options produces the perspectival gap: beneficiaries see rope-like coordination; victims see snare-like suppression; the analytical observer sees both.
 *
 * MANDATROPHY ANALYSIS:
 *   Copyright resolves the mandatrophy by displaying the tangled rope signature clearly: BOTH coordination function AND asymmetric extraction are present and irreducible. Copyright genuinely creates incentives for creation (coordination) while also enabling corporate monopoly and cultural suppression (extraction). The false summit would be claiming copyright is 'purely incentive-aligned' (mountain, coordination-only) or 'purely exploitative' (snare, extraction-only). The tangled rope classification captures that both functions coexist. The rising extractiveness and theater over 50 years shows the framework degrading toward snare—as terms extend, as DRM restrictions tighten, as corporate consolidation increases, the coordination function becomes subordinate to the extraction function. This trajectory suggests potential future decomposition: copyright may be evolving into two distinct constraints—creator incentives (rope-like) and corporate monopoly (snare-like)—that should be split into separate stories if extractiveness continues rising toward 0.65+.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_copyright_duration,
    'What copyright duration optimally balances creator incentives against cultural commons access?',
    'Empirical analysis of publication and innovation rates under different copyright term lengths; economic modeling of creator behavior; historical comparison of creative output before/after term extensions',
    'If optimal duration is short (10-20 years): current framework is severe extraction mechanism (ε closer to 0.65). If optimal duration matches current law: framework is justified coordination mechanism (ε closer to 0.25). If no clear optimum: framework is politically contingent (ε remains at structural baseline ~0.52).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(optimal_copyright_duration, empirical, 'Optimal copyright duration for creator incentives vs. cultural access').

omega_variable(
    drm_circumvention_necessity,
    'Does legal prohibition of DRM circumvention serve copyright enforcement or function as pure suppression mechanism?',
    'Comparison of piracy rates and creator revenue before/after DMCA anti-circumvention provisions; analysis of legitimate uses blocked by DRM; assessment of whether traditional copyright law adequately protects works',
    'If circumvention bans prevent substantial infringement: they extend copyright''s coordination function (tangled_rope remains). If circumvention bans prevent primarily legitimate interoperability/repair: they function as pure suppression (ε toward 0.65, snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drm_circumvention_necessity, empirical, 'Whether DRM prohibitions serve enforcement or pure suppression').

omega_variable(
    corporate_rights_acquisition_asymmetry,
    'Does copyright''s transfer mechanism favor individual creators or corporate rights consolidation?',
    'Analysis of work-for-hire practices, contract analysis of standard terms offered to creators, tracking of rights concentration over time, comparison of creator bargaining power by market size',
    'If framework enables fair licensing: coordination mechanism dominates (tangled_rope from creator perspective). If framework systematizes creator subordination: extraction mechanism dominates (snare from creator perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(corporate_rights_acquisition_asymmetry, empirical, 'Whether copyright enables fair creator licensing or corporate consolidation').

omega_variable(
    fair_use_judicial_determination,
    'Is fair use (transformative use exception) a genuine coordinate limiting copyright power or a vestigial doctrine with inconsistent application?',
    'Meta-analysis of fair use case outcomes; assessment of litigation costs vs. awards; comparison of fair use success rates across jurisdictions; analysis of whether fair use doctrine has narrowed over time',
    'If fair use is reliably protective: it functions as a real constraint on copyright extraction (ε closer to 0.38, more Rope-like). If fair use is unreliable or expensive: it functions as theatrical limitation (ε remains high, theater_ratio increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_judicial_determination, empirical, 'Whether fair use doctrine provides meaningful limitation on copyright').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_protection, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_protection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(copy_tr_t25, copyright_protection, theater_ratio, 25, 0.45).
narrative_ontology:measurement(copy_tr_t50, copyright_protection, theater_ratio, 50, 0.55).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_protection, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(copy_be_t25, copyright_protection, base_extractiveness, 25, 0.4).
narrative_ontology:measurement(copy_be_t50, copyright_protection, base_extractiveness, 50, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_protection, information_standard).
narrative_ontology:affects_constraint(copyright_protection, patent_protection_framework).
narrative_ontology:affects_constraint(copyright_protection, trademark_protection_mechanism).
narrative_ontology:affects_constraint(copyright_protection, digital_rights_management).
narrative_ontology:affects_constraint(copyright_protection, fair_use_doctrine).
narrative_ontology:affects_constraint(copyright_protection, open_access_publishing).

% DUAL FORMULATION NOTE:
% Copyright protection may decompose into two structurally distinct constraints as extractiveness rises and corporate consolidation increases. Creator incentives (rope-like, ε≈0.25) and corporate monopoly (snare-like, ε≈0.70) are currently bundled in law but represent distinct coordination vs. extraction mechanisms. If extractiveness exceeds 0.65 in future measurement, separate constraint stories for copyright_creator_incentives and copyright_corporate_monopoly should be generated with network linkage showing how creator incentive mechanisms (genuine coordination) are instrumentalized by corporate consolidation (pure extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_protection, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
