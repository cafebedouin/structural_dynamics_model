% ============================================================================
% CONSTRAINT STORY: publication_bias_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publication_bias_mechanism, []).

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
 *   constraint_id: publication_bias_mechanism
 *   human_readable: Publication Bias Mechanism in Scientific Literature
 *   domain: epistemology/scientific_publishing
 *
 * SUMMARY:
 *   Publication bias — the systematic preference for publishing studies with
 *   positive, novel, or statistically significant results over null,
 *   negative, or non-significant results — creates a structural constraint
 *   that contaminates the scientific literature with inflated effect sizes
 *   and false discoveries. The mechanism operates through multiple
 *   reinforcing channels: journal acceptance rates (high for positive
 *   results, low for null), researcher career incentives (publication counts
 *   reward positive results), funding agency visibility metrics (positive
 *   results demonstrate effectiveness), and reader attention (surprising
 *   findings are more interesting than confirmations or failures). This
 *   constraint exhibits a critical feature: it operates both as a
 *   coordination mechanism (journals selecting for publishable narrative
 *   clarity and practical utility) and as an extraction mechanism (systematic
 *   suppression of inconvenient null results that would constrain false
 *   claims and slow publication-driven careers). The theater_ratio has
 *   increased over the measured interval as competitive publishing pressures
 *   intensified and impact-factor weighting of career evaluations amplified
 *   the selection for surprising results. The base_extractiveness has risen
 *   from 0.28 (when null results were unpublished but not actively
 *   suppressed) to 0.52 (current state where suppression is active and
 *   extraction is normalized through citation advantage). The open science
 *   movement represents a structural response with real sunset mechanisms
 *   (pre-registration, registered reports, null result journals), though
 *   adoption remains constrained by traditional incentive structures.
 *
 * KEY AGENTS:
 *   - Null Result Researchers: Primary victims (powerless/trapped) — conduct valid research that finds no significant effect, face rejection, career stalling, funding loss with no exit mechanism
 *   - Research Integrity / Epistemic Commons: Secondary victim (powerless/trapped) — abstract collective good bearing cost of false positive accumulation across generations
 *   - Journal Publishers and Editors: Primary beneficiary (institutional/arbitrage) — benefit from selection of publishable claims, high impact factors, reader engagement; maintain editorial control with option to shift policies
 *   - Positive Result Authors: Mixed agent (moderate/constrained) — benefit from publication and career advancement but constrained by pressure to find positive results and publication expectations shaping research designs
 *   - Funding Agencies: Secondary beneficiary (institutional/arbitrage) — benefit from positive reporting that demonstrates funding effectiveness and justifies budgets
 *   - Open Science Coalition: Organized responders (organized/constrained) — building alternative verification pathways (pre-registration, registered reports, open data) that represent a sunset mechanism for traditional bias
 *   - Peer Review System: Institutional actor (institutional/arbitrage) — maintains performative verification ritual that cannot detect publication bias or missing data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publication_bias_mechanism, 0.52).
domain_priors:suppression_score(publication_bias_mechanism, 0.68).
domain_priors:theater_ratio(publication_bias_mechanism, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publication_bias_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(publication_bias_mechanism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(publication_bias_mechanism, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publication_bias_mechanism, tangled_rope).
narrative_ontology:human_readable(publication_bias_mechanism, "Publication Bias Mechanism in Scientific Literature").
narrative_ontology:topic_domain(publication_bias_mechanism, "epistemology/scientific_publishing").

domain_priors:requires_active_enforcement(publication_bias_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publication_bias_mechanism, journal_publishers).
narrative_ontology:constraint_beneficiary(publication_bias_mechanism, positive_result_authors).
narrative_ontology:constraint_beneficiary(publication_bias_mechanism, funding_agencies_with_positive_mandates).
narrative_ontology:constraint_victim(publication_bias_mechanism, research_integrity).
narrative_ontology:constraint_victim(publication_bias_mechanism, null_result_researchers).
narrative_ontology:constraint_victim(publication_bias_mechanism, field_replication_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NULL RESULT RESEARCHER (SNARE) — Trapped by the publication bottleneck. Null results are rejected, career progression stalls, grants dry up. No mechanism for exiting this constraint without abandoning the research program. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(publication_bias_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCH INTEGRITY / EPISTEMIC COMMONS (SNARE) — Cannot exit the contamination cycle. False positives accumulate in the literature without self-correction mechanism. Generational time horizon reveals that the bias compounds across research cohorts — each generation inherits a literature polluted by positive result selection. No power to organize or escape.
constraint_indexing:constraint_classification(publication_bias_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: POSITIVE RESULT AUTHOR (TANGLED ROPE) — Benefits from publication bias (elevated to 'discoverer' status, career advancement) while also constrained by it (pressure to find 'findings,' publication expectations shape research choices). Constrained rather than mobile due to career penalties for negative results and need for continuous publication record. Mixed experience of coordination (journal selects publishable claims) and extraction (selective filtering distorts field-wide inference).
constraint_indexing:constraint_classification(publication_bias_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JOURNAL PUBLISHERS (ROPE) — Experience the constraint as pure coordination: filtering claims for publishability, managing peer review, curating the literature. Net beneficiary with arbitrage exit (can shift editorial policies, launch preprint servers, change impact factor metrics). Experiences the constraint as enabling their function, not restricting it.
constraint_indexing:constraint_classification(publication_bias_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FUNDING AGENCIES (ROPE) — Benefit from positive results (demonstrate funding effectiveness, justify budget allocation). Experience publication bias as coordination mechanism: selective reporting aligns researcher incentives with funder visibility goals. Arbitrage exit through reporting policy changes (shift to pre-registration, require null results, mandate open data). Experiences constraint as enabling monitoring function.
constraint_indexing:constraint_classification(publication_bias_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE COALITION (SCAFFOLD) — Organized response to publication bias through pre-registration, null results publishing, open science frameworks, and meta-science. See the constraint as temporary — alternative verification pathways (preprints, registered reports, open data mandates) are building a sunset mechanism. Constrained by institutional adoption barriers (journals still weight traditional publication, career incentives favor traditional journals). Estimated sunset: field norms shift over 15-25 years as pre-registration becomes standard.
constraint_indexing:constraint_classification(publication_bias_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PEER REVIEW AS RITUAL (PITON) — Peer review for detecting publication bias is substantially performative. Reviewers cannot verify whether null results were hidden before submission, cannot access unpublished experiments, cannot audit researcher exploration of the design space. The review ritual persists because editorial systems have invested in it and alternatives haven't fully replaced it. High theater_ratio reflects this degraded verification function. Still benefits publishers (maintains authority illusion) through inertia.
constraint_indexing:constraint_classification(publication_bias_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing publication bias as an immutable feature of how journals coordinate peer review and editorial selection. From universal/civilizational scope, positive results may appear as 'inherently more publishable' due to narrative clarity, practical utility, or reader interest. However, structural data contradicts this: publication bias is enforced through incentive alignment (career advancement, journal impact factor, funding visibility) and suppression of alternatives (paywall access, rejection bottleneck, impact factor weighting). These are contingent institutional arrangements, not laws of nature. Engine will flag as false summit.
constraint_indexing:constraint_classification(publication_bias_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publication_bias_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publication_bias_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publication_bias_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publication_bias_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publication_bias_mechanism, TR),
    TR >= 0.70.

:- end_tests(publication_bias_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts through multiple mechanisms: career advancement concentrated on positive results, funding visibility amplifying positive-result authors, journal prestige concentrated in publications of surprising findings, and loss of research value when null results are suppressed. The 0.52 value reflects that extraction is real and systematic, but not total — some null results do publish, some funding agencies are shifting toward pre-registration, and open science alternatives are emerging. The increase from 0.28 to 0.52 over the measured interval indicates that competitive publishing pressures have intensified the extraction component over time. Suppression (0.68): High. Active suppression includes rejection of null result manuscripts, file-drawering by researchers anticipating rejection, lack of institutional pathways for null result researchers, and absence of career incentives for reporting negative findings. Theater ratio (0.64): Moderate-high. Peer review cannot detect publication bias, cannot access hidden unpublished studies, cannot verify the researcher's full exploration of design space, and cannot audit whether null results were discovered but hidden. The journal's claim to verify scientific claims is substantially theatrical when applied to detecting publication bias itself. The theater ratio's increase from 0.48 to 0.64 reflects that journals' performative authority has become more disconnected from actual verification capacity as research complexity increased and access to raw data remained restricted.
 *
 * PERSPECTIVAL GAP:
 *   Maximum divergence between powerless/trapped agents (Snare) and institutional/arbitrage beneficiaries (Rope). The powerless victim experiences χ ≈ 0.52 × 1.42 × 1.2 ≈ 0.89 (very high effective extraction). The journal publisher experiences χ ≈ 0.52 × (-0.12) × 1.2 ≈ -0.07 (negative effective extraction — they benefit). This gap is the diagnostic signal that the constraint is a tangled rope at the moderate perspective (where agents have partial agency and mixed outcomes) and a snare at the powerless perspective (where there is only cost).
 *
 * DIRECTIONALITY LOGIC:
 *   Publication bias systematically extracts from researchers with inconvenient results and transfers benefit to researchers with surprising results, journals seeking high impact factors, and funding agencies claiming success. The extraction flow is enforced through: (1) editorial gatekeeping (acceptance rates favor positive results), (2) incentive structure (careers advance through publication count, funding visibility metrics reward positive results), (3) suppression of alternatives (paywall barriers prevent access to null result databases, impact factor metrics downweight null result journals), and (4) reader attention economy (surprising results get cited more, generating citation advantage). No single actor 'owns' the constraint — it emerges from aligned incentives across publishers, researchers seeking career advancement, funding agencies, and reward structures. This distributed enforcement makes the constraint a tangled rope rather than a snare where a single actor controls the mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the perspectival structure: publication bias genuinely coordinates the literature by curating for publishable claims with sufficient novelty and clarity; AND it genuinely extracts from researchers unlucky enough to conduct inconvenient research. Both are true simultaneously. The constraint is NOT purely extractive (there are real coordination benefits to journal curation) and NOT purely coordination (the selection mechanism is systematically asymmetric). The tangled_rope classification prevents confusion between 'journal editors select interesting papers' (coordination) and 'journal editors suppress null results to boost impact factors' (extraction). The presence of beneficiaries (publishers, positive-result authors) and victims (null-result researchers, epistemic commons) confirms the tangled_rope gate. The measured increase in theater_ratio (0.48 → 0.64) indicates that peer review's performative component has grown relative to actual verification — reviewers cannot detect whether studies were pre-registered, whether null results were hidden, or whether the effect size is inflated due to publication selection. This is not degradation into a piton (the coordination function remains real), but it is drift toward more theater and less function. The open science coalition perspective shows a real sunset mechanism (pre-registration eliminates file-drawering, registered reports commit researchers to hypotheses before seeing results, null result journals remove selection pressure) — this is not aspirational but structural. However, the adoption rate is constrained by traditional career incentives, moving the coalition from 'mobile' to 'constrained' exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    file_drawer_quantification,
    'What proportion of conducted research goes unreported versus what proportion represents genuine unsuccessful null findings?',
    'Meta-analysis of registered reports vs published results; surveys of researchers on conducted-but-unreported studies; archival analysis of grant databases vs publication counts by institution',
    'If file-drawer proportion > 50%: publication bias is a major extraction mechanism (snare from epistemic perspective). If < 20%: selective reporting explains most bias and constraint is more coordination-like (tangled rope). Affects χ calculation and victim-group impact severity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(file_drawer_quantification, empirical, 'Quantification of unreported vs conducted research').

omega_variable(
    field_specific_bias_heterogeneity,
    'Does publication bias vary systematically by field, funding source, and disciplinary norm?',
    'Comparative analysis: effect size distributions across fields with different review cultures (physics vs psychology vs medicine); correlation analysis of publication bias magnitude with replication rate metrics (Replicability Index, open science adoption metrics)',
    'If heterogeneous: publication bias is partially contingent institutional arrangement (constraint exists but not universally). If uniform across fields: bias reflects deeper structural feature (closer to mountain). Affects whether scaffold (open science sunset) is realistic or aspirational across different domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(field_specific_bias_heterogeneity, empirical, 'Field-specific variation in publication bias magnitude and mechanism').

omega_variable(
    pre_registration_effectiveness,
    'Do pre-registered studies eliminate publication bias or merely relocate selection to the pre-registration phase?',
    'Longitudinal comparison of published vs unfinished pre-registered studies; analysis of selective de-registration patterns; effect size distributions pre vs post pre-registration mandate by journal',
    'If effective elimination: scaffold perspective is realistic — open science provides genuine sunset mechanism. If bias relocates: constraint mutates rather than resolves — pre-registration becomes new bottleneck (specification hiding, p-hacking via pre-registration language). Affects confidence in open science coalition''s claimed sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pre_registration_effectiveness, empirical, 'Whether pre-registration eliminates or relocates publication bias').

omega_variable(
    incentive_vs_structural_causation,
    'Is publication bias primarily driven by individual researcher incentives (ambitious-researcher hypothesis) or by structural features of journal editing and impact metrics (institutional hypothesis)?',
    'Experimental intervention: randomized career reward structures (remove impact factor weighting, create equal prestige for null results); compare researcher behavior before/after. Observational: analyze publication patterns across institutions with different incentive structures (universities vs national labs vs industry labs)',
    'If incentive-driven: constraint can be modified by policy (change career evaluation metrics, pre-register, mandate reporting). If structural (journal capacity, reader attention, printing costs): bias reflects genuine coordination constraints harder to dissolve. Affects whether tangled_rope classification (mixed coordination and extraction) is accurate or understates extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_vs_structural_causation, empirical, 'Individual incentives vs structural institutional drivers of publication bias').

omega_variable(
    suppression_enforcement_mechanism,
    'What mechanisms enforce suppression of null results — active rejection, inactive file-drawering, or lack of institutional support (grants, mentorship, career path)?',
    'Content analysis of rejection letters for null result papers; interviews with rejected researchers; comparative analysis of effort required to publish null results in established journals vs new venues (preprints, open access journals, registered report platforms); tracking of researcher career outcomes post-null-result publication',
    'If active (explicit rejection): suppression = 0.80+, snare classification confirmed. If inactive (researcher self-selects non-submission): suppression = 0.50-0.70, tangled_rope appropriate. If structural lack of support (no mentorship for null-result researchers): suppression = 0.65-0.75, intermediate mechanism. Affects measured suppression value and victim identification precision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_enforcement_mechanism, empirical, 'Mechanisms enforcing suppression of null results').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publication_bias_mechanism, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pubias_tr_t0, publication_bias_mechanism, theater_ratio, 0, 0.48).
narrative_ontology:measurement(pubias_tr_t10, publication_bias_mechanism, theater_ratio, 10, 0.56).
narrative_ontology:measurement(pubias_tr_t20, publication_bias_mechanism, theater_ratio, 20, 0.64).
narrative_ontology:measurement(pubias_tr_t30, publication_bias_mechanism, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(pubias_be_t0, publication_bias_mechanism, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pubias_be_t10, publication_bias_mechanism, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(pubias_be_t20, publication_bias_mechanism, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(pubias_be_t30, publication_bias_mechanism, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publication_bias_mechanism, information_standard).
narrative_ontology:affects_constraint(publication_bias_mechanism, replication_crisis_mechanism).
narrative_ontology:affects_constraint(publication_bias_mechanism, verification_bottleneck).
narrative_ontology:affects_constraint(publication_bias_mechanism, effect_size_inflation).

% DUAL FORMULATION NOTE:
% Publication bias decomposes into distinct structural constraints: (1) journal_curation_coordination (ε=0.15, Rope) — the genuine function of selecting publishable claims; (2) positive_result_selection (ε=0.42, Tangled Rope) — asymmetric filtering that concentrates extraction; (3) null_result_suppression (ε=0.65, Snare) — active suppression of inconvenient findings. This file treats publication bias as a unified constraint at the moderate ε=0.52 level. Upstream constraints: journal business model (impact factor incentives). Downstream constraints: replication crisis, effect size inflation, verification bottleneck in specialized fields.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publication_bias_mechanism, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
