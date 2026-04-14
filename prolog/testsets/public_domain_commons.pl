% ============================================================================
% CONSTRAINT STORY: public_domain_commons
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_domain_commons, []).

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
 *   constraint_id: public_domain_commons
 *   human_readable: The Public Domain as a Cultural Commons
 *   domain: legal/economic/social
 *
 * SUMMARY:
 *   The public domain as a cultural commons exhibits a structural tension
 *   between its intended function (providing permanent access to expired
 *   creative works) and the mechanisms that suppress that access (legal
 *   uncertainty, digital rights management, organizational gatekeeping). The
 *   constraint encompasses copyright term extensions that push works further
 *   from entry to public domain, technical barriers that prevent access even
 *   to legal public domain material, and institutional arrangements that
 *   maintain extraction streams from works that should generate no exclusive
 *   revenue. The same phenomenon appears as an essential coordination
 *   mechanism enabling remix culture (rope), a temporary problem being solved
 *   by open digitization (scaffold), a degraded enforcement system
 *   maintaining theater (piton), an unavoidable side effect of property
 *   rights (mountain), or pure extraction targeting orphaned works and legacy
 *   access restrictions (snare), depending on the observer's structural
 *   position. Over the 30-year interval, extractiveness has increased as
 *   copyright term extensions (Sonny Bono Act 1998, EU harmonization
 *   1993-2006) have pushed more works into legal ambiguity, while theater has
 *   grown as rights-management systems (DRM, TPM, licensing platforms) have
 *   proliferated despite covering material with no active creator to benefit.
 *   Simultaneously, open-access infrastructure (Internet Archive, Creative
 *   Commons, public digitization initiatives) has created genuine
 *   alternatives, making the constraint increasingly visible as institutional
 *   choice rather than natural necessity.
 *
 * KEY AGENTS:
 *   - Orphaned Works: Primary victim (powerless/trapped) — legally in public domain but functionally inaccessible due to rights uncertainty
 *   - Legacy Rights Holders: Primary beneficiary and extractor (organized/constrained) — publishers, studios, estates that maintain control over copyright-expired material through licensing and legal barriers
 *   - Educators and Librarians: Secondary victim (moderate/constrained) — face licensing costs and legal uncertainty; benefit from portions of public domain but constrained by access restrictions
 *   - Remix Creators and Artists: Secondary beneficiary (institutional/arbitrage) — depend on public domain material for derivative works; experience constraint as coordination mechanism
 *   - Open Culture Coalition: Organized agents (organized/mobile) — Internet Archive, Creative Commons, public libraries building alternative access infrastructures with sunset logic
 *   - Copyright System: Institutional actor (institutional/arbitrage) — maintains rights-management apparatus on expired works where original creators receive zero benefit; piton perspective reveals degraded function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent copyright regime as inherent to property systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_domain_commons, 0.52).
domain_priors:suppression_score(public_domain_commons, 0.65).
domain_priors:theater_ratio(public_domain_commons, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_domain_commons, extractiveness, 0.52).
narrative_ontology:constraint_metric(public_domain_commons, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(public_domain_commons, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_domain_commons, tangled_rope).
narrative_ontology:human_readable(public_domain_commons, "The Public Domain as a Cultural Commons").
narrative_ontology:topic_domain(public_domain_commons, "legal/economic/social").

domain_priors:requires_active_enforcement(public_domain_commons).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_domain_commons, cultural_creators).
narrative_ontology:constraint_beneficiary(public_domain_commons, educational_institutions).
narrative_ontology:constraint_beneficiary(public_domain_commons, remix_derivative_works_producers).
narrative_ontology:constraint_victim(public_domain_commons, legacy_rights_holders).
narrative_ontology:constraint_victim(public_domain_commons, public_domain_access_constraint).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORPHANED WORK / POWERLESS VICTIM (SNARE) — A creative work whose copyright holder cannot be identified or located. Despite technically entering public domain at some date, the work remains inaccessible due to legal uncertainty: digitization platforms fear liability, archives cannot clear rights, educators cannot safely use the material. The orphaned work is trapped in a status quo of non-use despite legal expiration of copyright. Maximum experienced extraction — the work generates no value, benefits no one, and cannot escape the legal limbo.
constraint_indexing:constraint_classification(public_domain_commons, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EDUCATOR / LIBRARIAN (TANGLED ROPE) — Constrained by licensing costs, legal uncertainty, and resource limitations. Also genuinely benefits from public domain material that reduces licensing costs and enables curriculum design. The constraint is mixed: active enforcement of copyright term extensions and digital rights management extract value, but the existence of the public domain itself enables lower-cost educational access. Moderate experienced extraction with real agency and some benefit.
constraint_indexing:constraint_classification(public_domain_commons, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: REMIX CREATOR / INSTITUTIONAL BENEFICIARY (ROPE) — Artists, musicians, and filmmakers who build derivative works from public domain material. For them, the constraint is primarily coordination: public domain access enables remixing, sampling, and adaptation. They benefit from clear legal status and freely available material. Net beneficiary perspective — the constraint enables their primary workflow.
constraint_indexing:constraint_classification(public_domain_commons, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LEGACY COPYRIGHT HOLDER / ORGANIZED EXTRACTION (SNARE) — Publishers, film studios, and estate executors who control copyright-expired material and leverage legal uncertainty to maintain access restrictions. These actors have been organized to lobby for copyright term extensions (Sonny Bono Act, EUCD, etc.). They experience the constraint as an opportunity to extract licensing fees from material that should be free. Organized actors benefit from suppression (legal uncertainty, technical barriers, DRM); they are trapped in the enforcement regime they have created.
constraint_indexing:constraint_classification(public_domain_commons, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN CULTURE COALITION (SCAFFOLD) — Creative Commons, Internet Archive, Wikipedia, and public libraries working to build alternative pathways for public domain access. These actors see the constraint as temporary coordination failure with a sunset: metadata standards, full-text indexing, open-access digitization, and legal clarity initiatives (European Public Domain Navigator, Public Domain Review) are creating parallel infrastructures that bypass copyright gatekeeping. High agency, exit path visible.
constraint_indexing:constraint_classification(public_domain_commons, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT SYSTEM AS PITON (DEGRADED ENFORCEMENT) — The copyright system's stated purpose is to incentivize creation and reward creators. For works entering the public domain (copyright expired), this function has completely atrophied: the creator receives no benefit, yet enforcement overhead persists. The system maintains rights-management theater (TPM, licensing systems, rights clearance fees) on material with no active creator to reward. Theater ratio high because the legal apparatus continues with minimal functional verification — many orphaned works have no identifiable rights holder yet enforcement persists. Institutional inertia maintains the constraint despite loss of primary function.
constraint_indexing:constraint_classification(public_domain_commons, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — From a civilizational perspective, some might argue that rights scarcity is inherent to property systems: someone always owns something, and access always requires permission or payment. This naturalizes the contingent institutional choice to extend copyright terms and enforce DRM. However, the public domain itself disproves the necessity claim — works can exist in commons status where use requires no permission. The mountain classification here reveals a false summit: confusing the institutional design choice (copyright enforcement) with a law of nature (scarcity of cultural goods).
constraint_indexing:constraint_classification(public_domain_commons, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_domain_commons_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_domain_commons, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_domain_commons, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_domain_commons, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(public_domain_commons, TR),
    TR >= 0.70.

:- end_tests(public_domain_commons_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts value through multiple mechanisms: copyright term extensions delay public domain entry (currently life+70 in many jurisdictions), licensing fees capture value from material that should be free, DRM and access controls create artificial scarcity on digital public domain works, and orphan works remain inaccessible despite legal expiration. The extraction is not total (0.70+) because the public domain itself exists, access to some material is unrestricted, and remix culture does occur at scale. The value has increased from 0.25 to 0.52 over the interval as term extensions have layered additional extraction mechanisms. Suppression (0.65): Moderate-high. Barriers include legal uncertainty about orphan works and rights clearance, technical barriers (DRM, paywalls on digitized public domain), institutional gatekeeping (Google Books settlement limiting full-text access), and coordination costs for educators to verify rights status. However, suppression is not total — some public domain material is fully accessible, open-access digitization is expanding, and legal clarity initiatives are reducing uncertainty. Theater ratio (0.58): Moderate. Rights-management systems (licensing, TPM, search barriers) perform significant theater: they suggest verification of rights status, but much of this theater applies to material with no identifiable rights holder. Traditional copyright functions (incentivizing creators, providing revenue to creators) have completely atrophied for public domain works, yet enforcement overhead persists. The theater has increased as digital systems have proliferated.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. Orphaned works see pure extraction (snare) — legal expiration provides no benefit, only perpetual inaccessibility. Educators see mixed extraction and access (tangled rope) — licensing costs extract value, but public domain material is cheaper than alternatives. Remix creators see coordination (rope) — public domain enables their primary workflow. Legacy rights holders organized through lobbying see opportunity (snare from their perspective — they are trapped in maintaining enforcement systems). The open culture coalition sees a solvable problem with sunset (scaffold) — digitization and legal clarity are building exits. The copyright system sees its own degradation (piton) — rights management persists on works with zero active creators. The civilizational view risks naturalizing this entire arrangement (false mountain) — confusing contingent institutional choice with inherent scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the extraction flow. Orphaned works and educators have high d (victims/trapped or constrained) — they bear the cost of access restrictions with minimal exit. Legacy rights holders have low d (beneficiaries/arbitrage) — they extract licensing revenue and can shift to other business models if copyright were reformed. Remix creators have very low d (beneficiaries/institutional) — public domain access directly enables their creative work. The open culture coalition has moderate d (organized agents with mobile exit) — they can build alternatives even if legacy systems persist. The copyright system itself (piton perspective) has low d as beneficiary but high theater: it maintains rights-management machinery on expired works where the beneficiary (creator) no longer exists. The analytical observer risks d ≈ 0.5 by naturalizing institutional choice as inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that the public domain is fundamentally a coordination mechanism (rope for creators, scaffold for institutions building alternatives) being weaponized into an extraction mechanism (snare for orphaned works, tangled rope for educators). The constraint is tangled rope because it possesses genuine coordination function (enabling remix, providing archival access, reducing transaction costs for derivative works) PLUS asymmetric extraction (copyright term extensions, licensing gatekeeping, DRM barriers targeting material that should be free). The active enforcement requirement is satisfied: copyright term extensions, DMCA takedown enforcement, and DRM systems actively suppress access to material that has legally entered the public domain. The beneficiaries are clear: remix creators, educators, and cultural institutions benefit from public domain access. The victims are equally clear: orphaned works are trapped in inaccessibility, educators face licensing costs, and legacy archivists cannot safely digitize. The theater ratio (0.58) reflects that licensing and rights-clearance systems perform significant institutional theater on material with no active rights holder. The constraint resolves to tangled rope, not pure extraction (snare) at the system level, because the public domain infrastructure itself provides real coordination value — the problem is suppression and term extension layered onto that infrastructure, not the coordination function itself. The open culture coalition's scaffold perspective is empirically structural (not aspirational) because digitization infrastructure, metadata standards, and legal clarity initiatives are demonstrably creating parallel pathways that will sunset legacy gatekeeping within 10-20 years.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    orphan_works_threshold,
    'At what point does legal uncertainty about copyright status become equivalent to practical non-access? Is a 50-year-old work with unidentifiable owner functionally ''public domain'' if no one dares use it?',
    'Empirical tracking of digitization rates for orphaned works before/after legal clarity provisions; comparison with works of known rights status and similar age',
    'If threshold is low (< 5 years uncertainty): orphaned works are effective snare traps. If threshold is high (> 20 years): legal status matters less than technical access, reframing the constraint as infrastructure problem rather than IP problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(orphan_works_threshold, empirical, 'Threshold for when copyright uncertainty equals functional non-access').

omega_variable(
    copyright_term_extension_incentive,
    'Do extended copyright terms (life+70) actually incentivize creation, or do they primarily benefit legacy rights holders and extractors of orphaned works?',
    'Cross-national comparison of creation rates before/after term extensions (UK 1998, US 1998, EU 1993); correlation with measurable creative output metrics',
    'If extensions incentivize creation: term lengths are legitimate coordination mechanism (rope). If extensions primarily benefit extractors: term extensions are pure rent-seeking (snare from creator perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(copyright_term_extension_incentive, empirical, 'Whether copyright term extensions incentivize creation or just extraction').

omega_variable(
    digital_commons_viability,
    'Can large-scale public domain digitization (Internet Archive, Google Books, Europeana) achieve discovery and usability parity with commercial platforms without institutional resources?',
    'User studies comparing search success rates, metadata quality, interface usability between public archives and commercial platforms; longitudinal tracking of institutional funding sustainability for public archives',
    'If viability achieved: scaffold perspective confirmed — open-access infrastructure can sunset copyright gatekeeping. If unachievable: legacy platforms remain necessary, constraining alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_commons_viability, empirical, 'Viability of large-scale open-access digitization infrastructure').

omega_variable(
    remix_culture_dependency,
    'Does contemporary creative culture actually depend on large-scale public domain access, or can creators operate within a regime of constant licensing negotiations?',
    'Analysis of derivative works by public domain dependence: songs sampled from pre-1928 recordings; films using pre-1923 footage; literature explicitly built from older works',
    'If high dependency (>30%): public domain is essential infrastructure (rope for creators). If low dependency (<10%): remix is niche practice, and constraint primarily affects archivists/educators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remix_culture_dependency, empirical, 'Extent to which contemporary culture depends on public domain access').

omega_variable(
    institutional_sunset_timeline,
    'Given current trends in open-access digitization and legal harmonization (EU copyright reform), what is the realistic sunset date for copyright-based gatekeeping on expired works?',
    'Policy analysis of DSM Directive implementation, Sonny Bono Act reform proposals, and institutional funding commitments for public digitization projects',
    'If sunset < 10 years: scaffold is structural (real exit path). If sunset > 30 years: scaffold is aspirational (institutional momentum maintains current regime).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_sunset_timeline, conceptual, 'Realistic timeline for sunset of copyright gatekeeping on public domain works').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_domain_commons, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubdom_tr_t0, public_domain_commons, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pubdom_tr_t15, public_domain_commons, theater_ratio, 15, 0.48).
narrative_ontology:measurement(pubdom_tr_t30, public_domain_commons, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(pubdom_be_t0, public_domain_commons, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(pubdom_be_t15, public_domain_commons, base_extractiveness, 15, 0.38).
narrative_ontology:measurement(pubdom_be_t30, public_domain_commons, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_domain_commons, global_infrastructure).
narrative_ontology:affects_constraint(public_domain_commons, copyright_term_extension).
narrative_ontology:affects_constraint(public_domain_commons, digital_rights_management).
narrative_ontology:affects_constraint(public_domain_commons, orphan_works_access).

% DUAL FORMULATION NOTE:
% The public domain as commons is upstream of three specific extraction mechanisms: copyright term extensions (regulatory), DRM barriers (technical), and orphan works uncertainty (legal). Each downstream constraint has higher ε values (0.65-0.75) reflecting more specialized extraction mechanisms. The public domain story models the commons infrastructure itself; the downstream stories model specific institutional mechanisms that suppress it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(public_domain_commons, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
