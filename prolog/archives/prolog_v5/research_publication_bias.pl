% ============================================================================
% CONSTRAINT STORY: research_publication_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_research_publication_bias, []).

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
 *   constraint_id: research_publication_bias
 *   human_readable: Research Publication Bias
 *   domain: academic/scientific_publishing
 *
 * SUMMARY:
 *   Publication bias is the systematic tendency to publish research with
 *   positive or novel results while suppressing null results, negative
 *   findings, and replications. This constraint operates through career
 *   incentives (publications count toward tenure, grants, promotion), journal
 *   incentives (positive results attract citations and readership), and
 *   cognitive biases (novel results seem more valuable). The constraint
 *   exhibits tangled rope structure: it has a genuine coordination function
 *   (journals filter signal from noise, rewarding novel findings) alongside
 *   systematic asymmetric extraction (null-result researchers and the
 *   knowledge commons bear costs while positive-result researchers and
 *   publishers benefit). The extractiveness has increased over the interval
 *   (0.35 → 0.58) as metrics-based evaluation has intensified and publication
 *   counts have become more directly tied to career outcomes. The theater
 *   ratio has also increased (0.52 → 0.68) as peer review has become
 *   increasingly performative relative to its verification capacity.
 *   Publication bias affects different agents asymmetrically: junior
 *   researchers without reputational capital are trapped; established
 *   researchers with tenure can exit by publishing null results; the
 *   knowledge commons cannot exit.
 *
 * KEY AGENTS:
 *   - Journal Publishers: Primary beneficiary (institutional/arbitrage) — profit from positive results through citations, impact factor, subscription revenue
 *   - Tenure-Track Academics: Secondary beneficiary (powerful/mobile with high-capital subset) — benefit from publication counts and novel findings; established researchers can exit by publishing null results
 *   - Positive-Result Researchers: Beneficiary (moderate/constrained) — career trajectory reinforced by bias; publishable findings create network effects
 *   - Null-Result Researchers: Primary victim (powerless/trapped) — unpublished work creates career damage; no exit option without abandoning research
 *   - Replication Studies Scholars: Secondary victim (moderate/constrained) — replication work undervalued; face barriers to publishing but can benefit from open-science alternatives
 *   - Scientific Knowledge Commons: Tertiary victim (powerless/trapped) — systematically contaminated with false positives; accumulates bias in documented knowledge; abstract collective good with no agent advocate
 *   - Open Science Coalition: Organized agents (organized/constrained) — building alternative pathways (preprints, registered reports, open access) that reduce bias but also impose new costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(research_publication_bias, 0.58).
domain_priors:suppression_score(research_publication_bias, 0.65).
domain_priors:theater_ratio(research_publication_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(research_publication_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(research_publication_bias, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(research_publication_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(research_publication_bias, tangled_rope).
narrative_ontology:human_readable(research_publication_bias, "Research Publication Bias").
narrative_ontology:topic_domain(research_publication_bias, "academic/scientific_publishing").

domain_priors:requires_active_enforcement(research_publication_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(research_publication_bias, journal_publishers).
narrative_ontology:constraint_beneficiary(research_publication_bias, positive_result_researchers).
narrative_ontology:constraint_beneficiary(research_publication_bias, tenure_track_academics).
narrative_ontology:constraint_victim(research_publication_bias, null_result_researchers).
narrative_ontology:constraint_victim(research_publication_bias, replication_studies_scholars).
narrative_ontology:constraint_victim(research_publication_bias, scientific_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NULL-RESULT RESEARCHER (SNARE) — Structurally trapped by career dependence on publications. Publishing null results faces rejection, desk rejects, and career damage. No exit option: must publish positive results or face professional extinction. Experiences maximum extraction through publication gatekeeping and suppression of their actual findings.
constraint_indexing:constraint_classification(research_publication_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SCIENTIFIC KNOWLEDGE COMMONS (SNARE) — Cannot exit the publication system; is systematically poisoned by false positives and omitted null results. The commons has no agent to advocate for it. Suffers maximum extraction through resource waste chasing false positives and accumulated bias in documented knowledge.
constraint_indexing:constraint_classification(research_publication_bias, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REPLICATION SCHOLAR (TANGLED ROPE) — Constrained by low prestige of replication work and resource barriers, but also benefits from the publication ecosystem for disseminating replication results. Receives genuine coordination benefit (venue exists for publishing replications) alongside asymmetric extraction (replication work is undervalued). Medium experienced extraction with agency.
constraint_indexing:constraint_classification(research_publication_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JOURNAL PUBLISHERS (ROPE) — Experience the publication bias mechanism as pure coordination: selecting for novel, positive results attracts readers, citations, and impact factor. Net beneficiary. Can arbitrage by founding new journals or shifting publication models. Coordination function is genuine (filtering signal from noise) even though the filter is biased.
constraint_indexing:constraint_classification(research_publication_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ESTABLISHED RESEARCHER (ROPE) — High reputational capital provides mobile exit option: can publish negative results and still maintain career trajectory. Also benefits from coordination (prestigious journals provide visibility). Constrained chi from high f(d) but genuine coordination function. Can weather career risk that traps junior researchers.
constraint_indexing:constraint_classification(research_publication_bias, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: TRADITIONAL PEER REVIEW (PITON) — Performative review ritual justified by quality control but increasingly theatrical. Reviewers cannot verify most empirical claims; acceptance decisions often rest on novelty assessment and plausibility heuristics rather than replication. Peer review persists through institutional inertia despite degraded function. Theater ratio is high because the review's gatekeeping function is real (sorts submissions) but its verification function is largely performed.
constraint_indexing:constraint_classification(research_publication_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: OPEN SCIENCE MOVEMENT (TANGLED ROPE) — Organized agents (preprint servers, registered reports, open-access mandates) are building alternative publication pathways. Genuine coordination function (reducing barrier to null-result publication) combined with extraction (mandatory open data requirements can impose high costs on resource-limited groups). Constrained exit because institutional change is slow; genuine agency because norms are measurably shifting.
constraint_indexing:constraint_classification(research_publication_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks viewing publication bias as an immutable feature of how humans process information: positive results are more salient, memorable, and publishable by nature. This perspective naturalizes what is actually a contingent institutional arrangement (journal-based reward structure, impact factor metrics, career dependence on publication counts). The engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(research_publication_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(research_publication_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(research_publication_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(research_publication_bias, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(research_publication_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(research_publication_bias, TR),
    TR >= 0.70.

:- end_tests(research_publication_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Publication bias imposes substantial costs on null-result researchers through career damage, resource waste on failed replications, and lost research productivity. The extraction is not maximal because: (1) alternative publication venues exist (preprints, some specialized journals), (2) meta-scientific work on publication bias is increasing visibility, (3) some fields (physics, chemistry) show lower bias than others (psychology, biomedicine). The trajectory from 0.35 → 0.58 reflects intensification of metrics-based evaluation over the past decade. Suppression (0.65): Moderate-high. Barriers to null-result publication include desk rejects, reviewer bias toward novelty, career risk, reduced citations, and funding consequences. But suppression is not total — some researchers do publish null results and maintain careers, and preprint servers reduce formal publication barriers. Theater ratio (0.68): High. Peer review for novel results is substantially performative: reviewers assess novelty and plausibility but cannot verify empirical claims at scale. The editorial process for positive results focuses on significance and saliency rather than replicability. The performance is rising as journals emphasize impact-factor optimization and acceptance decisions drift toward marketing logic.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between null-result researchers (snare) and journal publishers (rope) is maximal. Publishers see the bias as a coordination mechanism — they filter for significance and novelty, solving the publication problem of separating signal from noise. Null-result researchers see the same mechanism as pure extraction — their work is suppressed regardless of quality because it lacks positive findings. The open-science coalition sees a constraint that can be sunset through alternative publication models. Established researchers see a manageable coordination challenge they can navigate with reputational capital. The knowledge commons sees systematic contamination it cannot escape. The analytical observer risks naturalizing the bias as an immutable feature of scientific attention rather than a contingent institutional arrangement. The gap reveals that 'what is this constraint?' depends entirely on power position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position. Null-result researchers have high d (0.95+) — they are victims experiencing maximum extraction through suppressed publication. Journal publishers have low d (0.05-0.15) — they are beneficiaries whose interests align with the bias. Established researchers have moderate d (0.40-0.50) — they benefit from the system but have exit options. Replication scholars have high-moderate d (0.65-0.75) — they bear extraction costs but can also publish through alternative venues. The knowledge commons has maximum d (0.99) — abstract victim with no agency. Directionality flows from beneficiary/victim declarations and exit option capacity: trapped agents get high d and experience maximum f(d); arbitrage-capable agents get low d and experience negative effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Publication bias is NOT misclassified extraction presented as coordination. It genuinely serves coordination functions (filtering, priority-setting, resource allocation to novel claims) while simultaneously extracting asymmetrically from null-result researchers. Both aspects are structurally real. The tangled rope classification holds. However, the constraint's classification changes under different temporal horizons and power positions: from the immediate perspective of a powerless null-result researcher, it is a snare (maximum extraction, no exit). From the generational perspective of the knowledge commons, it is also a snare (systematic contamination of knowledge). From the immediate perspective of publishers, it is rope (pure coordination). From the generational perspective of the open-science movement, it is a temporary constraint with a sunset — the scaffold classification emerges as alternative publication pathways mature. No single type is correct — the constraint IS a tangled rope that appears as snare to trapped agents and rope to beneficiaries. The mandatrophy resolves by acknowledging this perspectival reality rather than trying to force a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    publication_bias_magnitude_empirical,
    'What is the actual proportion of null results that remain unpublished across scientific disciplines?',
    'Meta-analyses of trial registries (e.g., ClinicalTrials.gov) comparing registered studies to published results; file drawer effect quantification across multiple fields; arXiv preprints vs journal publications ratio analysis',
    'If true null-suppression > 40%: extractiveness increases to 0.68+, tangled rope → snare. If < 20%: extractiveness decreases to 0.35, tangled rope → rope. Current estimates range 30-60% across biomedical research.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(publication_bias_magnitude_empirical, empirical, 'Quantification of unpublished null results across disciplines').

omega_variable(
    career_cost_null_results_specificity,
    'How much does publishing null results actually damage career trajectories in different fields and career stages?',
    'Longitudinal career tracking of researchers with high null-result publication rates vs peers; analysis of tenure decisions, grant success rates, and job placement for null-result publishers; field-specific variation (molecular biology vs psychology vs physics)',
    'If cost is severe and universal: suppression increases, transforms powertracks from constrained to trapped. If cost is low or field-dependent: suppression decreases, exit options broaden for some agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(career_cost_null_results_specificity, empirical, 'Career impact of publishing null results by field and stage').

omega_variable(
    preprint_server_efficacy_alternative,
    'Do preprint servers (arXiv, bioRxiv, medRxiv) actually reduce publication bias or merely create parallel publication systems with their own biases?',
    'Comparison of result distributions (positive vs null rates) on preprint servers vs journals; analysis of which preprints get picked up by journals (selection bias within preprints); measurement of null-result visibility on preprint servers',
    'If truly alternative: open science sunset is real, scaffold perspective confirmed. If parallel bias: constraint persists, just decentralized; piton classification for preprints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_server_efficacy_alternative, empirical, 'Whether preprint servers provide genuine alternative to publication bias').

omega_variable(
    registered_reports_structural_efficacy,
    'Does the registered reports model (pre-registration + in-principle acceptance) actually prevent publication bias or does it concentrate bias at the registration stage?',
    'Analysis of registered reports outcomes: what fraction get published; what fraction report originally hypothesized results; whether registration itself creates gatekeeping (selective registration of ''publishable'' protocols)',
    'If effective: extractiveness decreases, rescue mechanism confirmed. If shifted: extraction redistributes upstream (to registration gatekeepers), maintains tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(registered_reports_structural_efficacy, empirical, 'Efficacy of registered reports model in preventing publication bias').

omega_variable(
    journal_business_model_causality,
    'Is publication bias a necessary consequence of journal business models (impact factor, subscription revenue) or a contingent choice that could be eliminated within existing institutional structures?',
    'Comparison of publication bias metrics across journals with different business models (open access vs subscription, pre-registration advocates vs traditional, impact-factor-dependent funding vs alternative metrics); analysis of editor policies on null results',
    'If necessary: constraint is harder to escape; institutional entrenchment is high. If contingent: suggests policy interventions could reduce extractiveness without system redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(journal_business_model_causality, conceptual, 'Whether publication bias is inherent to journal-based publishing or contingent on specific models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(research_publication_bias, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rpb_tr_t0, research_publication_bias, theater_ratio, 0, 0.52).
narrative_ontology:measurement(rpb_tr_t5, research_publication_bias, theater_ratio, 5, 0.62).
narrative_ontology:measurement(rpb_tr_t10, research_publication_bias, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(rpb_be_t0, research_publication_bias, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rpb_be_t5, research_publication_bias, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(rpb_be_t10, research_publication_bias, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(research_publication_bias, information_standard).
narrative_ontology:affects_constraint(research_publication_bias, research_replication_crisis).
narrative_ontology:affects_constraint(research_publication_bias, career_incentive_misalignment).
narrative_ontology:affects_constraint(research_publication_bias, scientific_knowledge_accumulation_bias).

% DUAL FORMULATION NOTE:
% Publication bias decomposes into multiple structurally distinct constraints: (1) journal editor selection bias (ε=0.42, tangled rope), (2) researcher career incentive misalignment (ε=0.65, snare), (3) null-result suppression dynamics (ε=0.58, snare). This story represents the system-level constraint encompassing all three. Downstream constraints inherit publication bias as a causal prerequisite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(research_publication_bias, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
