% ============================================================================
% CONSTRAINT STORY: publishing_embargo
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publishing_embargo, []).

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
 *   constraint_id: publishing_embargo
 *   human_readable: Academic Publishing Embargo
 *   domain: social/publishing/knowledge_dissemination
 *
 * SUMMARY:
 *   Academic publishing embargoes restrict researchers from publicly sharing
 *   findings before official journal publication, creating a temporal
 *   monopoly on knowledge dissemination. The constraint exhibits structural
 *   ambiguity: embargoes appear as legitimate peer review coordination
 *   (journals manage editorial workflow and ensure publication exclusivity)
 *   but operate simultaneously as extraction mechanism (publishers control
 *   access, delay knowledge diffusion, and reinforce subscription-based
 *   revenue models). The rise of preprint servers (arXiv, bioRxiv, medRxiv)
 *   and open access mandates (NIH Public Access, Plan S, European Open
 *   Science Cloud) has revealed that embargo enforcement is increasingly
 *   selective and contingent. Early career researchers in
 *   resource-constrained institutions bear maximum costs while established
 *   researchers at well-funded institutions navigate multiple dissemination
 *   channels with less penalty. The constraint is transitioning from Mountain
 *   (immutable feature of peer review) through Tangled Rope (mixed
 *   coordination-extraction) toward Piton (degraded institutional ritual
 *   maintained through inertia) as open access norms mature.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary victims (powerless/trapped) — dependent on journal prestige, tenure requirements, funding funder mandates; cannot exit embargo without career penalty
 *   - Global South Institutions: Secondary victims (powerless/trapped) — additional barriers through subscription costs, language norms, limited preprint access; face compounded embargo effects
 *   - Journal Publishers: Primary beneficiary (institutional/arbitrage) — control publication sequence, subscription revenue, and market access; capture prestige and citation advantage
 *   - Preprint Infrastructure Operators: Organized secondary actors (organized/constrained) — arXiv, bioRxiv, medRxiv building alternative dissemination pathways; constrained by institutional journal policy enforcement
 *   - Funding Bodies: Organized actors with growing power (organized/constrained) — NIH, EU, national funding councils implementing open access mandates; creating sunset pressure on embargoes
 *   - Academic Institutions: Institutional enforcers (institutional/arbitrage) — maintain embargo through tenure criteria, impact factor reputation dependence, and publishing agreements; benefit from prestige association
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publishing_embargo, 0.52).
domain_priors:suppression_score(publishing_embargo, 0.65).
domain_priors:theater_ratio(publishing_embargo, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publishing_embargo, extractiveness, 0.52).
narrative_ontology:constraint_metric(publishing_embargo, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(publishing_embargo, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publishing_embargo, tangled_rope).
narrative_ontology:human_readable(publishing_embargo, "Academic Publishing Embargo").
narrative_ontology:topic_domain(publishing_embargo, "social/publishing/knowledge_dissemination").

domain_priors:requires_active_enforcement(publishing_embargo).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publishing_embargo, journal_publishers).
narrative_ontology:constraint_beneficiary(publishing_embargo, institutional_gatekeepers).
narrative_ontology:constraint_victim(publishing_embargo, early_career_researchers).
narrative_ontology:constraint_victim(publishing_embargo, global_south_institutions).
narrative_ontology:constraint_victim(publishing_embargo, knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY CAREER RESEARCHER (SNARE) — Cannot exit embargo without career penalty. Trapped by tenure track incentives, funding source restrictions, and institutional policy enforcement. Bears full cost of delayed dissemination while competitors share findings through alternative channels (conferences, preprints). No agency in embargo timeline — journals control release dates.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GLOBAL SOUTH INSTITUTION (SNARE) — Additional trapped exit due to dependence on institutional journal subscriptions and English-language publishing norms. Embargo delays knowledge transfer to institutions with resource constraints and higher barrier to preprint archives. Bears extraction on timeline and access dimensions simultaneously.
constraint_indexing:constraint_classification(publishing_embargo, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: JOURNAL PUBLISHER (ROPE) — Experiences embargo as coordination mechanism: controls publication sequencing to manage editorial workflow, copyright claims, and revenue capture from subscription access. Net beneficiary. Embargo serves legitimate coordination function (managing peer review queue, ensuring exclusive publication rights) but also extracts through subscription monopoly and access restriction.
constraint_indexing:constraint_classification(publishing_embargo, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PREPRINT INFRASTRUCTURE COALITION (TANGLED ROPE) — arXiv, bioRxiv, medRxiv, OSF provide parallel dissemination with minimal embargo enforcement. Organized actors constrained by institutional journal policies but building alternative coordination pathways. Experiences both coordination benefit (immediate dissemination) and extraction cost (journals penalize preprint submissions, reduce impact factors). Mixed experience reflects active tension between systems.
constraint_indexing:constraint_classification(publishing_embargo, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL ACADEMIC INSTITUTION (PITON) — Maintains embargo compliance through institutional policy and faculty contracts despite degraded functional justification. Embargo ritual persists through accreditation requirements and prestige metrics (journal impact factor, journal rank) despite weak verification of peer review quality. Theater ratio high because institutional reputation is largely performative — tied to brand recognition rather than research quality verification.
constraint_indexing:constraint_classification(publishing_embargo, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN ACCESS MANDATE COALITION (SCAFFOLD) — Funder and institutional open access mandates (NIH Public Access, Plan S) are creating temporary coordination with sunset logic. Embargo restrictions declining as funding bodies require immediate access. Theater declining as preprint evaluation and post-publication peer review replace journal gatekeeping. Estimated sunset: 10-15 years for norms to mature as journal prestige decouples from access restriction.
constraint_indexing:constraint_classification(publishing_embargo, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE NATURAL LAW (MOUNTAIN) — Risks naturalizing embargo as inherent feature of peer review, suggesting that verification delay is necessary. However, structural data contradicts mountain classification: embargo is enforced institutional practice, not immutable law. Open access and preprint systems demonstrate that immediate dissemination with distributed scrutiny can replace traditional embargo cycle. Engine will flag as false summit.
constraint_indexing:constraint_classification(publishing_embargo, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publishing_embargo_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publishing_embargo, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publishing_embargo, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publishing_embargo, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publishing_embargo, TR),
    TR >= 0.70.

:- end_tests(publishing_embargo_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Publishers capture subscription revenue and prestige leverage during embargo window, creating temporal asymmetry favoring established researchers with institutional resources. Embargo delays knowledge diffusion for 6-18 months depending on journal and field, creating information lag that favors those with alternative access channels. However, extractiveness is not maximal because legitimate peer review coordination exists — journals do perform editorial curation, and some delay is functionally necessary for quality verification. The value reflects genuine mixed coordination-extraction tension. Suppression (0.65): High. Researchers face significant barriers to exit: tenure track incentives tied to journal prestige, funder restrictions on preprint sharing, institutional policies enforcing embargo compliance, and career risk of journal rejection for embargo violation. Alternative channels (preprint servers) exist but carry reputational cost and incomplete acceptance in some fields. Enforcement is unevenly applied — established researchers navigate embargo norms more flexibly than early career researchers. Theater ratio (0.68): Moderately high. Journal peer review ritual contributes to prestige reputation, but the performative component has increased as embargo enforcement has weakened. Preprints demonstrate that distributed scrutiny can replace journal gatekeeping, revealing the theater in traditional embargo cycle. Theater has increased over the interval as open access alternatives have exposed that journal-exclusive publication is not necessary for quality verification.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between journal publishers (Rope) and early career researchers (Snare). Publishers see embargo as legitimate coordination that happens to benefit them; researchers see pure extraction with coordination framing. This gap reflects the fundamental asymmetry: publishers control the embargo timeline and have alternatives (subscription revenue, prestige from brand recognition), while researchers have no exit within the journal prestige system. The preprint coalition (Tangled Rope) occupies an intermediate position — they experience genuine coordination benefit (immediate dissemination) paired with extraction cost (journal prestige penalties). The scaffold perspective (open access mandates) reveals the constraint's contingency — embargo is not immutable but enforced institutional practice that is gradually losing power as funding bodies make open access mandatory.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position within the extraction flow and available exit options. Journal publishers (institutional/arbitrage) occupy beneficiary position with low d ≈ 0.15 — they control embargo timing and capture prestige benefits, with abundant alternative revenue models (hybrid open access, preprints, author fees) available but not chosen. Early career researchers (powerless/trapped) occupy maximum victim position with high d ≈ 0.95 — they depend entirely on journal prestige for career advancement and cannot exit embargo without penalty. Preprint operators (organized/constrained) occupy mixed position with moderate d ≈ 0.55 — they enable alternative pathways but remain constrained by institutional journal preferences. Funding bodies (organized/constrained) have declining d ≈ 0.40 — they are moving from constraint toward beneficiary position as open access mandates gain power. The directionality chain shows extraction flowing from researchers toward publishers, with funders and institutions playing intermediate roles in enforcement and compliance.
 *
 * MANDATROPHY ANALYSIS:
 *   The embargo constraint faces unresolved mandatrophy between coordination and extraction narratives. COORDINATION CASE: Embargo represents legitimate peer review workflow — journals must manage editorial sequencing, coordinate reviewer assignment, and ensure publication exclusivity. Embargo delay enables peer review quality assurance and prevents simultaneous publication in multiple venues. EXTRACTION CASE: Embargo primarily serves subscription revenue protection and journal market power consolidation. Publishers capture prestige leverage from exclusive publication rights while researchers bear cost of delayed dissemination. Preprint servers prove that distributed post-publication scrutiny can replace pre-publication gatekeeping, suggesting embargo coordination function is weaker than framing suggests. The rising theater ratio (0.68) indicates increasing performative component — embargo persists through accreditation inertia despite open access alternatives demonstrating functional equivalence. RESOLUTION PATH: The constraint will resolve toward Snare classification (extraction) as open access mandates mature and preprint-first dissemination becomes field norm. Publisher coordination role will diminish from primary to secondary function. This suggests embargo was always primarily extractive but justified through coordination framing — classic mandatrophy pattern where institutional practice misrepresents its own nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preprint_quality_parity,
    'Do preprint-first dissemination models with post-publication peer review achieve equivalent error detection rates compared to traditional pre-publication embargo-gated review?',
    'Longitudinal comparison of retraction rates, correction rates, and citation quality between preprint-first papers and traditional embargo papers in same fields; meta-analysis of peer review effectiveness across dissemination models',
    'If parity achieved: embargo functional justification collapses to pure extraction (Snare classification strengthens across more perspectives). If preprint quality significantly lower: embargo represents necessary coordination cost (Tangled Rope justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_quality_parity, empirical, 'Whether preprint-first dissemination achieves equivalent verification quality').

omega_variable(
    embargo_enforcement_coalition_breakdown,
    'What percentage of researchers violate embargo through preprint posting, conference presentation, or social media disclosure?',
    'Survey of researcher behavior; tracking of preprint posting relative to journal embargo dates; identification of sanctions and enforcement disparities',
    'If violation rate > 30%: embargo enforcement is breaking down; constraint transitions toward Piton (inertial). If enforcement remains strict: coordination function may be genuine but selective (benefits established researchers with institutional protection).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(embargo_enforcement_coalition_breakdown, empirical, 'Actual enforcement rate and sanction patterns for embargo violation').

omega_variable(
    subscription_dependence_artifact,
    'Is embargo justified by genuine peer review coordination, or does it primarily serve subscription revenue protection and journal market power?',
    'Analysis of embargo duration trends across open-access vs subscription journals; comparison of peer review quality metrics by journal business model; identification of embargo relaxation following journal transition to open access',
    'If primarily revenue protection: embargo is pure Snare with coordination framing (mandatrophy resolution toward extraction). If coordination justified: embargo is genuine Tangled Rope with legitimate benefit flow to publishers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscription_dependence_artifact, conceptual, 'Whether embargo serves peer review coordination or revenue protection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publishing_embargo, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubemb_tr_t0, publishing_embargo, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pubemb_tr_t5, publishing_embargo, theater_ratio, 5, 0.6).
narrative_ontology:measurement(pubemb_tr_t10, publishing_embargo, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(pubemb_be_t0, publishing_embargo, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pubemb_be_t5, publishing_embargo, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(pubemb_be_t10, publishing_embargo, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publishing_embargo, information_standard).
narrative_ontology:affects_constraint(publishing_embargo, open_access_fragmentation).
narrative_ontology:affects_constraint(publishing_embargo, journal_impact_factor_capture).
narrative_ontology:affects_constraint(publishing_embargo, knowledge_commons_access).

% DUAL FORMULATION NOTE:
% Publishing embargo represents a specific enforcement mechanism within broader academic prestige system. Related constraints include journal impact factor (measures prestige), publication bias (publication decision given to gatekeepers), and open access fragmentation (multiple incompatible open access models). The embargo constraint is downstream of journal prestige system and upstream of knowledge access inequality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publishing_embargo, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
