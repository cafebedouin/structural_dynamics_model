% ============================================================================
% CONSTRAINT STORY: publication_bias_replication
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_publication_bias_replication, []).

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
 *   constraint_id: publication_bias_replication
 *   human_readable: Publication Bias Mechanism in Replication Science
 *   domain: epistemology/research_methodology/social_structure
 *
 * SUMMARY:
 *   Publication bias in replication science represents a structural
 *   constraint where the mechanisms for validating scientific claims
 *   systematically exclude the results of replication attempts. Early-career
 *   scientists who conduct rigorous replications find their null or
 *   contradictory results unpublishable through traditional journal channels,
 *   creating a career trap where the most scientifically valuable work
 *   (independent verification) is systematically devalued. This constraint
 *   combines genuine coordination functions (journals allocating scarce
 *   publication space) with asymmetric extraction (negative results excluded
 *   from the literature, creating a false impression of confirmed findings).
 *   The constraint exhibits all six classification types from different
 *   positions: pure extraction from the perspective of replication scientists
 *   trapped in the career penalty, mixed coordination-extraction from the
 *   perspective of established researchers who benefit from bias protection,
 *   pure coordination from the journal perspective, a degraded ritual from
 *   the peer review perspective, a temporary problem with a sunset from the
 *   open science perspective, and an apparent natural law from the
 *   civilizational analytical view. The extractiveness has increased over the
 *   20-year interval from 0.35 to 0.58 as experiments have become more
 *   complex, replication has become harder, and the career penalty for
 *   negative results has intensified. The theater ratio has increased from
 *   0.52 to 0.68 as peer review has become more focused on defending prestige
 *   through narrative justifications ('this replication doesn't replicate the
 *   same conditions,' 'these findings are marginal') rather than evaluating
 *   actual methodological validity.
 *
 * KEY AGENTS:
 *   - Early-Career Replication Scientist: Primary victim (powerless/trapped) — bears full extraction through career annihilation and unpublishability of negative results
 *   - Research Methodologist: Secondary victim (moderate/constrained) — benefits from methodological networks but carries disproportionate burden of proving replication validity
 *   - High-Impact Journal: Primary beneficiary (institutional/arbitrage) — maintains journal prestige through selective publication that amplifies positive findings
 *   - Established Researcher: Secondary beneficiary (powerful/mobile) — benefits from publication bias that protects prior findings and inflates their citation records
 *   - Peer Review System: Institutional mechanism (institutional/arbitrage) — maintains performative gatekeeping that enforces publication bias while claiming quality control
 *   - Open Science Coalition: Organized agent (organized/constrained) — builds alternative infrastructure (preprints, registered reports, open data) to bypass traditional gatekeeping
 *   - Replication Science Ecosystem: Abstract victim (powerless/trapped) — collective literature contaminated by false positives; no mechanism for self-correction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(publication_bias_replication, 0.58).
domain_priors:suppression_score(publication_bias_replication, 0.62).
domain_priors:theater_ratio(publication_bias_replication, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(publication_bias_replication, extractiveness, 0.58).
narrative_ontology:constraint_metric(publication_bias_replication, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(publication_bias_replication, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(publication_bias_replication, tangled_rope).
narrative_ontology:human_readable(publication_bias_replication, "Publication Bias Mechanism in Replication Science").
narrative_ontology:topic_domain(publication_bias_replication, "epistemology/research_methodology/social_structure").

domain_priors:requires_active_enforcement(publication_bias_replication).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(publication_bias_replication, high_impact_journals).
narrative_ontology:constraint_beneficiary(publication_bias_replication, established_researchers).
narrative_ontology:constraint_beneficiary(publication_bias_replication, positive_result_generators).
narrative_ontology:constraint_victim(publication_bias_replication, replication_science_ecosystem).
narrative_ontology:constraint_victim(publication_bias_replication, early_career_researchers).
narrative_ontology:constraint_victim(publication_bias_replication, negative_result_publishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER REPLICATION SCIENTIST (SNARE) — Trapped in a system where negative results (the core output of replication) are systematically devalued. Cannot exit the career path without abandoning research entirely. Bears full extraction: years of work produce unpublishable results, no career advancement, no funding renewal. The constraint enforces publication bias through career annihilation of those who replicate.
constraint_indexing:constraint_classification(publication_bias_replication, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCH METHODOLOGIST (TANGLED ROPE) — Constrained by limited career opportunities in methodology/replication space and journal reputation barriers, but also benefits from methodology development, collaborative networks, and increasing recognition of replication importance. Mixed experience: genuine coordination function (making replication science work) alongside asymmetric extraction (methodologists carry disproportionate burden of proving replication validity).
constraint_indexing:constraint_classification(publication_bias_replication, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-IMPACT JOURNAL (ROPE) — Experiences publication bias as pure coordination mechanism. Journals solve the legitimate problem of allocating scarce publication space through impact-weighted selection. No enforcement burden; journals simply apply editorial criteria that (coincidentally) amplify positive results and inhibit replications. Beneficiary position: positive results increase journal citations and perceived prestige.
constraint_indexing:constraint_classification(publication_bias_replication, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ESTABLISHED RESEARCHER (TANGLED ROPE) — Has genuine mobility (can publish anywhere, secure funding from prior prestige) but is also partially captured by the incentive structure that rewarded their rise. Benefits from publication bias that protects their prior findings from replication scrutiny. Asymmetric: the constraint both enables and constrains their work — they benefit from the bias that would destroy an early-career replicator.
constraint_indexing:constraint_classification(publication_bias_replication, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PEER REVIEW RITUAL (PITON) — The traditional peer review process has degraded into theater for enforcing publication bias. Original function (quality control) has atrophied; current function (protecting prestige of positive findings) is maintained through institutional inertia. Reviewers cannot access raw data, replication groups lack resources, and negative results are framed as 'failed experiments' rather than valid findings. Theater ratio (0.68) reflects that much of peer review effort goes into defending why a replication 'doesn't count' rather than evaluating actual methodological validity.
constraint_indexing:constraint_classification(publication_bias_replication, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SCIENCE MOVEMENT (SCAFFOLD) — Organized agents (OSF, preprint servers, open data initiatives, registered reports) see publication bias as a temporary coordination failure being solved through infrastructure change. Preprints allow negative results to be shared without journal gatekeeping; registered reports lock in hypotheses before results are known; open data enables scrutiny. Sunset mechanism: as these platforms mature and gain academic credit, traditional journal gatekeeping loses enforcement power. Estimated timeline: 15-25 years for norms to establish in most fields.
constraint_indexing:constraint_classification(publication_bias_replication, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, publication bias appears as an inevitable consequence of human psychology: people are more likely to report surprising/positive findings; journals are more likely to publish them; researchers are more likely to pursue novel rather than replication directions. This perspective naturalizes publication bias as immutable human nature. Engine will flag this as false summit — the structural data contradicts the mountain classification, revealing contingent institutional arrangements (incentives, journal gatekeeping, career advancement rules) falsely presented as natural law.
constraint_indexing:constraint_classification(publication_bias_replication, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(publication_bias_replication_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(publication_bias_replication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(publication_bias_replication, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(publication_bias_replication, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(publication_bias_replication, TR),
    TR >= 0.70.

:- end_tests(publication_bias_replication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. Publication bias extracts value from replication scientists (career penalty, unpublishability, time investment producing no professional return) and transfers it to high-impact journals (increased prestige through curated positive findings) and established researchers (inflated citation records protected from replication scrutiny). The constraint is not maximally extractive because some alternative pathways exist (niche journals, preprints, institutional repositories), but the career consequences of using them are severe. The increase from 0.35 to 0.58 reflects intensifying competition for journal space as research output has increased without proportional increase in publication venues, raising the stakes for positive findings. Suppression (0.62): Moderate-high and structural. Multiple barriers reinforce publication bias: (a) Journal editorial criteria explicitly favor novelty/impact over replication; (b) Career advancement rules weight journal prestige over methodological rigor; (c) Funding agencies allocate resources toward novel discoveries rather than verification; (d) Peer review culture frames negative results as 'failure to replicate' rather than valid findings; (e) Data access barriers prevent independent scrutiny of original findings. Theater ratio (0.68): Moderate-high and increasing. Peer review of replications has become substantially performative: reviewers spend effort justifying why replications 'don't count' (different conditions, marginal effects, methodological quibbles) rather than genuinely evaluating whether the replication was conducted properly. The theater serves to defend the reputation of positive findings and journals, not to advance scientific accuracy. The increase from 0.52 to 0.68 reflects greater investment in sophisticated post-hoc narratives explaining away replications as the replication crisis has made the bias more visible and defensive.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is fundamental: early-career replication scientists see pure extraction (Snare) because they experience unambiguous career costs with no offsetting benefits. Established researchers see mixed coordination-extraction (Tangled Rope) because the same constraint that harms replicators protects their prior findings and privileges their novel work. High-impact journals see pure coordination (Rope) because publication bias emerges 'naturally' from their editorial criteria without explicit enforcement—journals are simply solving the allocation problem given the scarce resource of high-prestige publication space. Methodologists see tangled rope because they coordinate the replication infrastructure (creating genuine value) while bearing extraction (methodological work is undervalued in career advancement). The peer review system sees itself as maintaining quality (Rope) but functions as degraded theater (Piton) because the original gatekeeping function (preventing poor science from publication) has atrophied and been replaced by narrative defense of prestige. Open science sees the constraint as temporary (Scaffold) with a sunset mechanism: preprints and registered reports will eventually provide alternative pathways that bypass traditional journal gatekeeping. The civilizational analytical observer risks naturalizing publication bias (Mountain) as inevitable human psychology, but this falsely transforms contingent institutional arrangements (journal incentives, career rules, funding structures) into laws of nature. The engine's false summit detector should flag the mountain classification as naturalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are determined by each agent's structural position in the extraction flow. Replication scientists trapped in the system: d ≈ 0.95 (full target, trapped exit) → maximum experienced extraction. Established researchers with mobility: d ≈ 0.35 (partial beneficiary, mobile exit) → moderate to low extraction. Journals with arbitrage options: d ≈ 0.05 (full beneficiary, arbitrage exit) → negative extraction (constraint subsidizes them). Methodologists with constrained exit: d ≈ 0.62 (mixed position—they coordinate methodology but are extracted from through undervaluation) → moderate extraction. The early-career scientist's high d derives from trapped exit options + victim status (negative results unpublishable) + high suppression (peer review actively defends against replication); the journal's low d derives from beneficiary status (bias amplifies prestige) + arbitrage options (can publish anywhere, can abandon publication if needed) + minimal suppression (journals control the gatekeeping, not vice versa).
 *
 * MANDATROPHY ANALYSIS:
 *   Publication bias resolves the mandatrophy by showing that coordinating scientific communication (Rope perspective) is genuinely entangled with asymmetric extraction of replication scientists (Snare perspective). The pure coordination reading (journals allocating scarce prestige-bearing publication space is a legitimate coordination problem) is accurate from the journal's viewpoint, but it is incomplete from the replication scientist's viewpoint because it ignores that the allocation mechanism systematically favors positive results and punishes attempts to verify them. The mandatrophy is resolved not by choosing one type (Tangled Rope), but by recognizing that the constraint produces different types from different positions because the coordination function and extraction function are structurally coupled: journals coordinate research communication through criteria that explicitly extract value from negative results. The constraint is Tangled Rope because it performs both functions simultaneously—it coordinates publication space allocation AND extracts from replication science. This is not a confused classification; it is a precise diagnosis of a mixed mechanism where genuine coordination is asymmetrically distributed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positive_result_definition_ambiguity,
    'Is a ''positive result'' defined by statistical significance, effect size, hypothesis confirmation, or novelty? Different definitions change what counts as biased.',
    'Longitudinal analysis of publication patterns across different statistical thresholds (p<0.05 vs p<0.01) and field-specific definitions of confirmatory vs exploratory results.',
    'If definition is statistical significance: publication bias is measurable and quantifiable. If definition is novelty: bias becomes entangled with genuine scientific interest. Classification shifts from Snare (objective extraction) to Tangled Rope (mixed novelty-seeking and extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(positive_result_definition_ambiguity, conceptual, 'Ambiguity in defining ''positive result'' changes bias measurement').

omega_variable(
    replication_resource_causality,
    'Does publication bias cause replication underfunding, or does replication underfunding cause publication bias? Are they the same constraint or two separate ones?',
    'Historical intervention analysis: study cases where funding was increased for replication work independently of journal policy, and cases where journal policy changed independent of funding changes.',
    'If causally coupled: single constraint with multiple mechanisms. If separate: should decompose into publication_bias_replication and replication_funding_barriers as linked stories. Cascading impact on network structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_resource_causality, empirical, 'Causal relationship between publication bias and replication underfunding').

omega_variable(
    field_variation_in_bias_magnitude,
    'Does publication bias vary systematically by field (physics vs psychology vs medicine)? If so, do we have one constraint or a family of field-specific constraints?',
    'Meta-analysis of publication bias effect sizes across fields; correlation with field-specific resource scarcity and replication difficulty.',
    'If uniform: single global constraint. If field-specific: decompose into domain-specific stories (publication_bias_psychology, publication_bias_medicine) with different epsilon values and different institutional actors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(field_variation_in_bias_magnitude, empirical, 'Whether publication bias is uniform across scientific fields').

omega_variable(
    preprint_paradox,
    'Do preprints reduce publication bias or simply create a parallel publication system where the same bias mechanisms operate twice (preprint impact + journal impact)?',
    'Comparison of bias magnitudes: preprint servers alone vs traditional journal system vs hybrid; tracking whether preprint authors continue to experience career penalties for negative results despite preprint availability.',
    'If preprints solve bias: scaffold sunset timeline is accurate (15-25 years). If preprints duplicate bias: scaffold is aspirational theater, not structural solution. May require separate story (preprint_gatekeeping_parallel_structure) documenting bias reproduction at multiple levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_paradox, empirical, 'Whether preprints solve publication bias or duplicate it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(publication_bias_replication, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pub_bias_tr_t0, publication_bias_replication, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pub_bias_tr_t10, publication_bias_replication, theater_ratio, 10, 0.61).
narrative_ontology:measurement(pub_bias_tr_t20, publication_bias_replication, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(pub_bias_be_t0, publication_bias_replication, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pub_bias_be_t10, publication_bias_replication, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pub_bias_be_t20, publication_bias_replication, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(publication_bias_replication, information_standard).
narrative_ontology:affects_constraint(publication_bias_replication, replication_crisis_visibility).
narrative_ontology:affects_constraint(publication_bias_replication, research_funding_novelty_bias).
narrative_ontology:affects_constraint(publication_bias_replication, peer_review_epistemic_structure).

% DUAL FORMULATION NOTE:
% Publication bias in replication is downstream of career advancement incentives (research_funding_novelty_bias) and peer review gatekeeping (peer_review_epistemic_structure) but represents a distinct structural constraint with its own extraction mechanism. The three constraints form a causal cluster: funding bias generates novel results, peer review selectively publishes them, and publication bias then excludes replications from the literature. Each constraint has different epsilon values reflecting different mechanisms (funding scarcity → 0.45, peer review theater → 0.52, publication bias extraction → 0.58).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(publication_bias_replication, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
