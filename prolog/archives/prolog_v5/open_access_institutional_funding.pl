% ============================================================================
% CONSTRAINT STORY: open_access_institutional_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_access_institutional_funding, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: open_access_institutional_funding
 *   human_readable: Open Access Institutional Funding Mandate
 *   domain: academic_publishing/institutional_policy
 *
 * SUMMARY:
 *   Open access institutional funding mandates represent a structural tension
 *   between the goal of democratizing knowledge access and the mechanisms
 *   that institutions use to enforce compliance. The constraint exhibits
 *   tangled rope dynamics: genuine coordination function (making research
 *   publicly available) coexists with asymmetric extraction (unfunded
 *   compliance burden, consolidation of publishing power). Research
 *   institutions and funders benefit by demonstrating commitment to open
 *   science and capturing increased citation advantage; small publishers and
 *   independent scholars bear disproportionate costs; journal editors
 *   coordinate peer review while experiencing labor intensification; and the
 *   legacy publishing system persists through performative compliance while
 *   retaining prestige-filtering power. The constraint's theater ratio (0.68)
 *   reflects that institutions have rebranded traditional publishing
 *   workflows as 'open access' without fundamentally altering the peer review
 *   ritual, prestige hierarchies, or incentive structures that concentrate
 *   scientific authority. The extractiveness (0.58) indicates moderate but
 *   growing burden: institutions initially absorbed OA costs through library
 *   budgets and negotiated APC discounts, but as mandates expand and preprint
 *   adoption increases, the extraction mechanism has shifted toward unfunded
 *   mandate enforcement and APC fee burden concentration on poorly funded
 *   institutions.
 *
 * KEY AGENTS:
 *   - Research Institution: Primary beneficiary (institutional/arbitrage) — mandates OA for authors, captures increased citation impact, complies to funder requirements without bearing full cost burden
 *   - Funding Agency: Co-beneficiary (institutional/arbitrage) — mandates OA as compliance requirement, demonstrates public investment return, can arbitrage between publication venues
 *   - Small Independent Publisher: Primary victim (powerless/trapped) — faces unfunded mandate to provide OA infrastructure, cannot exit without losing institutional partnerships, absorbs infrastructure costs
 *   - Journal Editor: Secondary victim (moderate/constrained) — coordinates peer review function but bears increased administrative burden managing OA compliance; can exit at high career cost
 *   - Independent Scholar: Victim (moderate/constrained) — lacks institutional funding to pay APCs, faces barriers to publishing in newly-converted OA journals, can exit through preprint-only publishing but at reputation cost
 *   - Open Science Coalition: Organized actor (organized/constrained) — funders, librarians, and technologists building alternative dissemination pathways (preprint servers, institutional repositories, community platforms); sees mandate as temporary bridge to decentralized infrastructure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy choice as inevitable law, missing the contingency of current implementation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_access_institutional_funding, 0.58).
domain_priors:suppression_score(open_access_institutional_funding, 0.65).
domain_priors:theater_ratio(open_access_institutional_funding, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_access_institutional_funding, extractiveness, 0.58).
narrative_ontology:constraint_metric(open_access_institutional_funding, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(open_access_institutional_funding, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_access_institutional_funding, tangled_rope).
narrative_ontology:human_readable(open_access_institutional_funding, "Open Access Institutional Funding Mandate").
narrative_ontology:topic_domain(open_access_institutional_funding, "academic_publishing/institutional_policy").

domain_priors:requires_active_enforcement(open_access_institutional_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_access_institutional_funding, research_institutions).
narrative_ontology:constraint_beneficiary(open_access_institutional_funding, funding_agencies).
narrative_ontology:constraint_beneficiary(open_access_institutional_funding, public_knowledge_ecosystem).
narrative_ontology:constraint_victim(open_access_institutional_funding, small_publishers).
narrative_ontology:constraint_victim(open_access_institutional_funding, journal_editors).
narrative_ontology:constraint_victim(open_access_institutional_funding, independent_scholars).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL INDEPENDENT PUBLISHER (SNARE) — Faces mandate to provide open access infrastructure without funding support. Cannot exit: either comply with unfunded mandate (absorbing costs) or lose institutional subscriptions and prestige. Zero degrees of freedom. Maximum extraction from powerless agent with no alternative revenue model.
constraint_indexing:constraint_classification(open_access_institutional_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JOURNAL EDITOR (TANGLED ROPE) — Coordinates peer review and manuscript management (genuine coordination function) while extraction occurs through labor burden and reduced journal autonomy. Editors benefit from increased submission volume and prestige but bear disproportionate work managing compliance. Can exit but at high career cost (losing editorial position, reputation). Mixed coordination and extraction.
constraint_indexing:constraint_classification(open_access_institutional_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESEARCH INSTITUTION (ROPE) — Mandates open access for authors; benefits through increased citation rates, research impact, and compliance with funder requirements. Can arbitrage across publication venues and fund APCs from research budgets. Net beneficiary experiencing the constraint as coordination mechanism that amplifies research visibility.
constraint_indexing:constraint_classification(open_access_institutional_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SCIENCE COALITION (SCAFFOLD) — Organized funders, universities, and librarians implementing the mandate with sunset logic: preprint servers (arXiv, bioRxiv), open-access publishing platforms, and institutional repositories are building alternative dissemination pathways. The mandate accelerates transition toward distributed publishing infrastructure. Sunset horizon: 15-20 years as alternative models mature and journal dominance declines.
constraint_indexing:constraint_classification(open_access_institutional_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY PUBLISHING SYSTEM (PITON) — Traditional journal gatekeeping persists through institutional inertia: tenure committees still weight journal prestige, impact factors still drive hiring decisions, and prestige filters still concentrate attention despite open access mandate. The publishing ritual (peer review for prestige filtering) remains performative and largely unchanged by open access policies. Theater ratio reflects that compliance with OA mandates has not altered fundamental incentive structures.
constraint_indexing:constraint_classification(open_access_institutional_funding, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing contingent policy as immutable law: 'Open access is inevitable' or 'Paywalls are obsolete by definition.' This perspective frames the constraint as an unchangeable feature of modern academia. However, structural data reveals this as false naturalization — the mandate is a policy choice, not a law of nature. Different policy choices produce different constraints.
constraint_indexing:constraint_classification(open_access_institutional_funding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_access_institutional_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_access_institutional_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_access_institutional_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(open_access_institutional_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_access_institutional_funding, TR),
    TR >= 0.70.

:- end_tests(open_access_institutional_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mandate creates genuine coordination benefit (knowledge access expansion) but asymmetric extraction persists through multiple mechanisms: (1) APC fees concentrate publishing access toward well-funded institutions, creating new extraction pathway; (2) article processing costs shift from readers to authors/institutions, creating unfunded mandate burden on small publishers; (3) preprint adoption transfers quality-filtering burden from journal peer review to distributed readers and authors. The extractiveness increased from 0.42 to 0.58 over the measurement interval as APC inflation accelerated and mandate scope expanded across disciplines. Suppression (0.65): High. Barriers to exit include: institutional policy lock-in (tenure/hiring committees still weight journal prestige despite OA mandate), legacy publishing infrastructure dominance (journal impact factors and prestige remain effective filtering mechanism), funding imbalance (well-funded institutions can absorb APC and infrastructure costs; poorly-funded institutions cannot), and regulatory asymmetry (mandates apply universally but capacity to comply is institution-dependent). Theater ratio (0.68): High and increasing. OA compliance has become largely performative: institutions publish in OA venues while maintaining parallel subscriptions and prestige hierarchies; journals repackage traditional peer review as 'open access' with identical review workflows; citations still concentrate around high-prestige journals regardless of access status; academic hiring and tenure still reward journal prestige filtering rather than measuring research impact directly. The theater ratio increased from 0.52 to 0.68 as 'open access' became a compliance checkbox rather than a fundamental shift in dissemination and evaluation mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   The original research institution sees Rope — the mandate enhances their research visibility and funder compliance at minimal cost (their library negotiates APC discounts or shifts costs to author grant budgets). The small publisher sees Snare — the mandate is an existential threat with no exit path and no compensation mechanism. The journal editor sees Tangled Rope — peer review coordination remains genuine, but the labor burden and institutional pressure create extraction alongside coordination benefit. The open science coalition sees Scaffold — the mandate accelerates decentralization of publishing infrastructure toward preprint servers and institutional repositories, with a generational sunset as these mature. The legacy publishing system sees Piton — the ritual persists unchanged (journal prestige filtering, peer review gatekeeping, impact factors) despite OA compliance, maintained by institutional inertia in hiring and tenure. The analytical observer risks a false Mountain — assuming open access is an immutable feature of future academia rather than recognizing it as a contingent policy choice with reversible implementation. The perspectival gaps reveal that the constraint's true mechanism is not knowledge access (which the mandate does improve) but redistribution of publishing infrastructure power and cost burden across institutional actors with asymmetric capacity to absorb changes.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the extraction flow. Research institutions and funders occupy beneficiary positions with arbitrage options: they can shift compliance burden to author budgets, negotiate APC discounts, or maintain parallel subscription access. Their d values are low (0.15-0.25), producing negative or minimal effective extraction. Small publishers and independent scholars occupy victim positions with trapped or constrained exit: they cannot exit the mandate without losing institutional partnerships (small publishers) or publication prestige (independent scholars), and they lack the funding capacity to absorb infrastructure costs. Their d values are high (0.85-0.95), producing maximum or high effective extraction. Journal editors occupy a moderate-constrained position: they can exit through career costs, but they also benefit from increased submission volume and prestige. Their d value is moderate (0.55-0.65), producing moderate extraction. Organized coalitions (funders, librarians, technologists) occupy constrained positions with real exit paths (they can build alternative infrastructure and see sunset), producing lower d values (0.40-0.50) and moderate extraction that declines over generational time. The analytical observer at the universal/civilizational level risks the highest d (0.72), experiencing extraction through the naturalizing lens that treats contingent policy as immutable law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by differentiating genuine coordination function (knowledge access expansion, which is real and beneficial) from extraction mechanism (cost and compliance burden concentration, which is real and asymmetric). The tangled rope classification holds: the constraint genuinely coordinates dissemination of research findings across institutions while simultaneously extracting from powerless actors (small publishers, unfunded scholars) who cannot absorb infrastructure costs. The piton classification for the legacy publishing system is crucial — it reveals that institutional compliance with OA mandates has not eliminated prestige-filtering extraction. Journals remain gatekeepers; impact factors remain hiring criteria; prestige hierarchies remain concentrated. The mandate's theater ratio (0.68) indicates that much of the OA compliance is performed conformity to new policy language while underlying extraction mechanisms persist. The scaffold classification for the open science coalition is key to understanding the constraint's future: preprint servers, institutional repositories, and community publishing platforms are building alternative pathways that will eventually render the journal-based extraction mechanism obsolete. The mandate accelerates this transition by making traditional journal costs visible and expensive, creating exit incentive for well-funded institutions. However, the transition period (scaffold generational horizon) creates a window where poorly-funded actors bear maximum extraction burden. Without active redistribution mechanisms (open access funding for low-resource institutions, APC capping, library-funded preprint infrastructure), the mandate risks becoming a transitory snare for the powerless while the powerful arbitrage toward decentralized systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apc_fee_regressive_extraction,
    'Do open access Article Processing Charges (APCs) create a new, more extractive payment mechanism that concentrates publishing access toward well-funded institutions?',
    'Longitudinal analysis of publication rates by institution funding status; comparison of global South vs North publication volume post-mandate; measurement of average APC costs and institutional capacity to absorb them',
    'If APCs are more extractive than subscription model: the mandate shifts rather than eliminates extraction. Tangled rope classification becomes snare for unfunded institutions. If APCs enable equal access: mandate achieves coordination goal. Classification may become genuine rope or scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(apc_fee_regressive_extraction, empirical, 'Whether OA mandates increase extraction through APC mechanisms').

omega_variable(
    preprint_quality_filter_displacement,
    'Does removal of journal gatekeeping and peer review screening (through preprint-first models) transfer quality-filtering burden to readers and authors themselves, creating invisible new labor extraction?',
    'Measurement of reader time investment in filtering preprints; analysis of citation-based quality signals in preprint vs journal-published content; survey of author burden for quality management in distributed systems',
    'If filtering burden shifts invisibly to readers/authors: suppression increases (readers trapped in filter overload, authors bear quality maintenance burden). Extraction persists in new form. If filtering improves or equalize: mandate achieves intended effect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_quality_filter_displacement, empirical, 'Whether OA mandate displaces quality filtering burden to invisible labor').

omega_variable(
    power_asymmetry_persists_across_disciplines,
    'Does the open access mandate''s impact vary systematically by discipline, creating winners and losers based on field-level prestige and funding infrastructure?',
    'Cross-discipline analysis of publication cost distribution, author funding capacity, journal conversion rates, and prestige perception changes; identify disciplines where OA has created genuine rope vs those where snare dynamics remain',
    'If asymmetry persists: mandate is a tangled rope at aggregate level but snare in low-power disciplines. Policy may require targeted redistribution. If asymmetry declines: mandate is working as scaffold for systemic change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(power_asymmetry_persists_across_disciplines, empirical, 'Whether OA mandate impact varies by discipline and power asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_access_institutional_funding, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(oaif_tr_t0, open_access_institutional_funding, theater_ratio, 0, 0.52).
narrative_ontology:measurement(oaif_tr_t3, open_access_institutional_funding, theater_ratio, 3, 0.6).
narrative_ontology:measurement(oaif_tr_t6, open_access_institutional_funding, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(oaif_be_t0, open_access_institutional_funding, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(oaif_be_t3, open_access_institutional_funding, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(oaif_be_t6, open_access_institutional_funding, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_access_institutional_funding, information_standard).
narrative_ontology:boltzmann_floor_override(open_access_institutional_funding, 0.12).
narrative_ontology:affects_constraint(open_access_institutional_funding, academic_prestige_hierarchy).
narrative_ontology:affects_constraint(open_access_institutional_funding, journal_impact_factor_gatekeeping).
narrative_ontology:affects_constraint(open_access_institutional_funding, institutional_publishing_infrastructure).

% DUAL FORMULATION NOTE:
% Open access institutional funding is upstream of specific journal publishing constraints but represents a distinct structural policy-level constraint. The mandate affects how journals operate (APC models, compliance burden) and how institutions distribute publishing infrastructure (library budgets, author fund management), creating cascading effects on prestige hierarchies and gatekeeper power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_access_institutional_funding, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
