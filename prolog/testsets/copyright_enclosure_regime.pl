% ============================================================================
% CONSTRAINT STORY: copyright_enclosure_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_enclosure_regime, []).

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
 *   constraint_id: copyright_enclosure_regime
 *   human_readable: Copyright Enclosure Regime
 *   domain: intellectual_property/cultural_commons
 *
 * SUMMARY:
 *   The copyright enclosure regime creates a structural tension between the
 *   legitimate coordination function of attributed authorship and property
 *   rights assignment, and the extractive maintenance of artificial scarcity
 *   through term extensions, enforcement overhead, and suppression of
 *   derivative creation. The regime exhibits genuine coordination
 *   (attribution prevents plagiarism, property assignment incentivizes
 *   creation) layered with extraction (licensing fees, derivative work
 *   restrictions, public domain delay). The constraint's extractiveness
 *   (0.58) reflects that the regime increasingly functions as rent extraction
 *   rather than incentive mechanism — copyright terms have extended far
 *   beyond author lifespans (70+ years) while enforcement machinery has
 *   intensified despite digital technologies making copying essentially
 *   costless. The theater ratio (0.62) reflects that significant portions of
 *   enforcement are performative: copyright office registration is largely
 *   redundant given digital timestamps; DMCA takedown notices function as
 *   gatekeeping rather than genuine ownership verification; licensing
 *   negotiation is theater for price discrimination rather than coordination.
 *
 * KEY AGENTS:
 *   - Copyright Holders: Primary beneficiary (institutional/arbitrage) — capture licensing revenue, prevent competitors from derivative use, benefit from term extensions that delay public domain
 *   - Public Domain / Access-Constrained Populations: Primary victim (powerless/trapped) — unable to access or remix culturally significant materials within legal bounds; disproportionately affects developing economies with lower purchasing power
 *   - Derivative Creators: Secondary victim (powerless to moderate/trapped to constrained) — fan creators, remix artists, educators, researchers face cease-and-desist letters and licensing costs; some institutional protection available for educators
 *   - Licensing Intermediaries: Secondary beneficiary (institutional/arbitrage) — platforms like Spotify, streaming services, licensing collectives capture transaction fees and enforce terms
 *   - Open Access / Commons Movement: Organized agent (organized/constrained) — Creative Commons, open-access publishers, public domain projects building alternative pathways; see sunset trajectory through expanding permissive licensing
 *   - Copyright Office: Institutional actor (institutional/arbitrage) — maintains registration and enforcement apparatus; sees own function as partially degraded but maintains it through institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — assesses whether term extensions and enforcement intensity are justified by creation incentive elasticity or represent pure rent extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_enclosure_regime, 0.58).
domain_priors:suppression_score(copyright_enclosure_regime, 0.68).
domain_priors:theater_ratio(copyright_enclosure_regime, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_enclosure_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(copyright_enclosure_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(copyright_enclosure_regime, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_enclosure_regime, tangled_rope).
narrative_ontology:human_readable(copyright_enclosure_regime, "Copyright Enclosure Regime").
narrative_ontology:topic_domain(copyright_enclosure_regime, "intellectual_property/cultural_commons").

domain_priors:requires_active_enforcement(copyright_enclosure_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_enclosure_regime, copyright_holders).
narrative_ontology:constraint_beneficiary(copyright_enclosure_regime, licensing_intermediaries).
narrative_ontology:constraint_victim(copyright_enclosure_regime, derivative_creators).
narrative_ontology:constraint_victim(copyright_enclosure_regime, public_domain_commons).
narrative_ontology:constraint_victim(copyright_enclosure_regime, access_constrained_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PUBLIC DOMAIN / ACCESS-CONSTRAINED POPULATIONS (SNARE) — Unable to exit the extraction regime without legal violation or significant cost. Works remain locked behind copyright terms that exceed human lifespans. No alternatives for accessing foundational cultural materials except purchasing licenses or violation. Maximum suppression: enforcement mechanisms (DRM, legal penalty, institutional gatekeeping) prevent workarounds. The public domain has no advocate with structural power and no exit option.
constraint_indexing:constraint_classification(copyright_enclosure_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DERIVATIVE CREATOR / POWERLESS POSITION (SNARE) — Fan creators, remix artists, educators, researchers without institutional affiliation face full legal and financial barriers to derivative work. Copyright terms extending 70+ years post-author-death mean culturally vibrant material remains locked. Suppression is structural: cease-and-desist letters, takedown notices, licensing costs exceed income from non-commercial derivatives. No lawful path except permission-seeking (high cost) or working only with pre-1928 material (narrow choice set).
constraint_indexing:constraint_classification(copyright_enclosure_regime, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INSTITUTIONAL EDUCATOR (TANGLED ROPE) — Universities, libraries, museums have both coordination benefits and extraction costs from copyright. Copyright enables attribution and prevents plagiarism (coordination function). But licensing fees, licensing negotiations, and legal uncertainty create high transaction costs and suppress educational material remixing. Exit options are constrained: institutional affiliation provides legal cover for fair use claims, but only at cost of maintaining institutional legitimacy. Mixed extraction and genuine coordination function.
constraint_indexing:constraint_classification(copyright_enclosure_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR COPYRIGHT HOLDER (ROPE) — Large publishers, studios, music labels with extensive back catalogs experience copyright as pure coordination: assignment of authorship, prevention of plagiarism, standardized licensing terms enable efficient value extraction and global distribution. Exit options are maximal (arbitrage) — can shift licensing models, bundle content, create derivative works under their own copyright, negotiate collective licensing. Net beneficiary experiencing the regime as coordination mechanism for legitimate property claims.
constraint_indexing:constraint_classification(copyright_enclosure_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN ACCESS / COMMONS MOVEMENT (SCAFFOLD) — Creative Commons licensing, open-access publishing, public domain reclamation projects represent organized agents building sunset pathways. CC licenses create legal alternatives that allow derivative use while preserving attribution. The movement has real agency and sees a structural exit path: expanding works released under permissive terms, automated expiration of copyright terms, institutional norm shifts toward open licensing. Theater is moderate (0.62) because the regime's enforcement requires active institutional maintenance (copyright offices, takedown notice systems), not pure theater.
constraint_indexing:constraint_classification(copyright_enclosure_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT OFFICE APPARATUS (PITON) — Government copyright registration and enforcement machinery persists largely through institutional inertia. The primary verification function (registration enabling attribution claims) has been superseded by digital timestamps and author assertion. Enforcement machinery (takedown notices, DMCA prosecution) is theater-heavy: most creators never register, most enforcement is outsourced to intermediaries through contractual terms rather than copyright law itself. The apparatus maintains itself through legacy institutional structure, not through genuine functional verification of ownership claims.
constraint_indexing:constraint_classification(copyright_enclosure_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, copyright serves a genuine coordination function (incentivizing creation through property assignment) layered with extractive maintenance (term extensions, enforcement overhead). The regime has real coordination benefits (attribution norms, prevention of plagiarism through legal assignment) but also real extraction (restricting access, preventing derivative creation, concentration of cultural production). The effective extraction (chi ≈ 0.58 × f(d) × σ(global)) depends on the observer's directionality: beneficiaries with arbitrage options experience low chi; trapped derivative creators experience high chi.
constraint_indexing:constraint_classification(copyright_enclosure_regime, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_enclosure_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_enclosure_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_enclosure_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_enclosure_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_enclosure_regime, TR),
    TR >= 0.70.

:- end_tests(copyright_enclosure_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The regime extracts through licensing fees, derivative work restrictions, and enforced scarcity of culturally valuable materials. But this is not maximal extraction (0.70+) because legitimate creator incentives and property assignment do coordinate genuine creative output. The increase over time (0.35 → 0.58 over 50 years) reflects successive term extensions (Sonny Bono Act 1998, European Term Directives) that decoupled copyright length from author-death incentive logic, shifting from coordination incentive toward rent extraction. Suppression (0.68): High. Multiple enforcement layers prevent alternative models: DRM restrictions (DMCA anti-circumvention), takedown notices (DMCA 512), licensing gatekeeping, legal penalties (statutory damages $750-$30,000 per work). Suppression is structural — these mechanisms require active institutional maintenance (takedown notice processing, legal prosecution, technology enforcement) rather than emerging naturally from copyright law. Theater ratio (0.62): Moderate-high. Much enforcement is performative: copyright office registration is largely symbolic (ownership can be asserted without registration); DMCA takedown notices function as institutional gatekeeping rather than legal verification of ownership; licensing negotiations are theater for price discrimination rather than verification of legitimate rights.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Major copyright holders see pure coordination (Rope) — the regime enables global licensing, prevents plagiarism, assigns property rights. Open access movements see a degrading mechanism with a sunset (Scaffold) — open licensing, public domain reclamation, and institutional norm shifts are building alternative pathways. Copyright offices see their own apparatus as partially inert (Piton) — registration is largely redundant, enforcement relies on contractor intermediaries, primary function has been superseded by digital technologies. Institutional educators see mixed coordination and extraction (Tangled Rope) — copyright enables structured licensing but suppresses remixing and increases transaction costs. Powerless derivative creators see pure extraction (Snare) — they cannot legally remix culturally significant materials, face full suppression, have no exit option. Public domain access (characterized as powerless agent at generational horizon) sees pure extraction at maximum severity (Snare) — works remain locked for 70+ years, no lawful alternative pathway.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries with arbitrage options (major copyright holders, licensing intermediaries) have low d (0.10-0.20) — they experience the regime as coordination because they can shift between licensing models, bundle content, create derivatives under their own copyrights. They receive negative χ (extraction flows toward them). Victims trapped by the regime (public domain aspiration, powerless derivative creators) have high d (0.90-1.0) — they cannot exit without legal violation, experience maximum suppression, receive high χ (extraction flows away from them). Institutional educators have moderate d (0.50-0.60) — they benefit from copyright's coordination function (attribution prevents plagiarism, enables structured licensing for educational distribution) but are constrained by licensing costs and legal uncertainty around fair use. The organization of the open-access movement shifts d for organized agents from the baseline — they have agency and see exit paths (expanding permissive licensing, public domain reclamation) but face constraints from institutional resistance and collective action barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   COPYRIGHT ENCLOSURE AS TANGLED ROPE FAMILY: The regime genuinely coordinates attribution and property assignment (coordination function) while simultaneously extracting through licensing fees, term extensions, and derivative work restrictions (extraction function). The mandatrophy is resolved by recognizing that the coordination component (attribution, incentive structure) is real and necessary, but the extraction component (term lengths, enforcement intensity, suppression of derivatives) has grown beyond coordination cost. The regime has NOT degraded into pure Snare because the coordination function remains: creators do receive attribution and property rights that incentivize creation. It has NOT become pure Rope because real extraction occurs: derivative creators are suppressed, licensing fees create access barriers, term extensions serve no incentive function. The classification as Tangled Rope holds across perspectives because the trade-off between coordination and extraction is genuine from all positions — they disagree on whether the extraction justifies the coordination, not on whether both functions exist. The analytical observer can verify this by testing whether shortening copyright terms or reducing enforcement intensity would significantly reduce creation output: if creation is resilient to shorter terms (empirically suggested by open-source software, Creative Commons adoption), the extraction component is non-trivial and the regime is not approaching Rope purity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_copyright_term_length,
    'What copyright term length optimizes for both creator incentive and public domain rotation?',
    'Empirical correlation analysis: creative output vs copyright term length across jurisdictions and time periods; econometric modeling of incentive elasticity; comparison of creation rates before and after term extensions',
    'If optimal term < 20 years: current 70-year regime is severe extraction with minimal coordination benefit. If optimal term > 50 years: regime is closer to pure coordination. Classification shifts from Snare toward Rope as term shortens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_copyright_term_length, empirical, 'Optimal copyright term for incentivizing creation while enabling public domain rotation').

omega_variable(
    enforcement_mechanism_necessity,
    'Are DMCA anti-circumvention provisions and aggressive takedown enforcement necessary for copyright coordination, or are they parasitic extraction mechanisms riding on legitimate attribution norms?',
    'Controlled comparison: jurisdictions with different enforcement intensity holding copyright terms constant; measurement of creator incentive responsiveness to enforcement intensity vs term length; analysis of creator behavior under statutory licensing (lower enforcement) vs permission-based licensing (high enforcement)',
    'If enforcement is necessary: suppression (0.68) is legitimate coordination cost. If parasitic: suppression is pure overhead, classification shifts toward Snare from institutional perspectives, effective extraction increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_necessity, empirical, 'Whether aggressive enforcement mechanisms are necessary or parasitic extraction').

omega_variable(
    derivative_creation_market_size,
    'How much economically valuable derivative creation is suppressed by copyright restrictions, and could licensing frameworks capture this value more efficiently?',
    'Estimation from Creative Commons adoption rates, remix culture output, fan fiction markets, open-source software patterns; comparison of transaction costs (licensing negotiation, lawyer fees) vs potential licensing revenue; counterfactual analysis of derivative creation under permissive licensing',
    'If suppression > 40% of potential market: significant dead-weight loss, extraction is non-trivial. Classification shifts toward Snare from derivative creator perspectives. If suppression < 10%: extraction is minimal, regime approaches pure coordination (Rope).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(derivative_creation_market_size, empirical, 'Suppressed derivative creation market size and potential licensing efficiency gains').

omega_variable(
    collective_licensing_viability,
    'Could statutory or blanket licensing mechanisms (similar to music publishing) replace permission-based licensing while preserving creator compensation and attribution?',
    'Analysis of existing statutory licensing regimes (music performance, educational photocopying); simulation of extended collective licensing for literary and visual works; measurement of transaction cost reduction and derivative creation increase under blanket models',
    'If viable: regime structure could shift from Snare toward Tangled Rope or even Scaffold (sunset toward open licensing). If not viable: current permission-based model is necessary, extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_licensing_viability, empirical, 'Whether collective licensing could replace permission-based copyright while preserving incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_enclosure_regime, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t0, copyright_enclosure_regime, theater_ratio, 0, 0.4).
narrative_ontology:measurement(copy_tr_t25, copyright_enclosure_regime, theater_ratio, 25, 0.52).
narrative_ontology:measurement(copy_tr_t50, copyright_enclosure_regime, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(copy_be_t0, copyright_enclosure_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(copy_be_t25, copyright_enclosure_regime, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(copy_be_t50, copyright_enclosure_regime, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_enclosure_regime, attachment_coordination).
narrative_ontology:affects_constraint(copyright_enclosure_regime, digital_rights_management).
narrative_ontology:affects_constraint(copyright_enclosure_regime, term_extension_dynamics).
narrative_ontology:affects_constraint(copyright_enclosure_regime, fair_use_accessibility).

% DUAL FORMULATION NOTE:
% Copyright enclosure regime decomposes into three structurally distinct constraints: (1) attribution coordination (low extractiveness, pure rope), (2) term extension maintenance (high extractiveness, snare dynamics), (3) derivative work suppression (moderate extractiveness, tangled rope with asymmetric enforcement). This story models the aggregate regime; upstream constraints model the individual mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_enclosure_regime, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
