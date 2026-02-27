% ============================================================================
% CONSTRAINT STORY: wikipedia_notability_requirement_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wikipedia_notability_requirement_2026, []).

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
 *   constraint_id: wikipedia_notability_requirement_2026
 *   human_readable: Wikipedia Notability Requirement (2026)
 *   domain: social/technological
 *
 * SUMMARY:
 *   Wikipedia's notability requirement is a gatekeeping policy that
 *   determines which subjects merit dedicated articles on the encyclopedia.
 *   Established in the early 2000s to prevent spam and pseudoscience, the
 *   policy has evolved into a hybrid constraint combining genuine
 *   coordination (filtering unreliable content) with significant extraction
 *   (privileging established sources and Western institutions). The
 *   constraint exhibits a perspectival gap: established institutions and
 *   publishers see the requirement as legitimate quality control (Rope),
 *   while emerging creators and marginalized communities experience it as
 *   exclusionary gatekeeping (Snare). Volunteer reviewers experience it as a
 *   mixed burden (Tangled Rope), simultaneously maintaining encyclopedia
 *   quality and enforcing asymmetric access. The theater ratio (0.64)
 *   reflects that notability enforcement relies heavily on ritual discussion
 *   (Articles for Deletion debates) rather than coherent criteria — reviewers
 *   spend substantial effort in performative argumentation that rarely
 *   produces a principled standard. The extractiveness (0.52) indicates
 *   moderate but meaningful asymmetry: the policy benefits established
 *   publishers whose work is used as notability evidence, while constraining
 *   creators without institutional backing. The suppression (0.68) is high:
 *   creators face substantial barriers (obtaining coverage in 'reliable
 *   sources,' demonstrating 'independent notability,' navigating policy
 *   ambiguity), and alternatives (Wikidata, subject-specific wikis, personal
 *   websites) are materially weaker than encyclopedic Wikipedia coverage.
 *
 * KEY AGENTS:
 *   - Emerging Creators: Primary victim (powerless/trapped) — subjects without institutional backing must prove notability through external validation but lack pathways to do so
 *   - Non-English Communities: Primary victim (powerless/trapped) — notability criteria privilege English-language and Western sources, structurally disadvantaging non-English subject documentation
 *   - Established Publishers: Primary beneficiary (institutional/arbitrage) — their publications are used as evidence of notability, creating self-reinforcing access and citation advantages
 *   - Wikipedia Volunteer Reviewers: Secondary actor (moderate/constrained) — enforce standards with ambiguous criteria; experience both stewardship reward and interpersonal friction cost
 *   - Wikipedia Editorial Authority: Institutional enforcer (organized/constrained) — manages gatekeeping while also curating encyclopedic quality; benefits from authority while constrained by consensus governance
 *   - Marginalized Subjects: Structural victim — entire categories (local historical figures, indigenous knowledge, non-Western achievements) face systematic underrepresentation
 *   - Wikipedia Encyclopedia Completeness: Victim (abstract collective) — gating mechanism may prevent valuable subjects from being documented
 *   - Analytical Observer: Civilizational perspective — sees both the genuine need for content quality and the way gatekeeping reproduces status hierarchies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, 0.52).
domain_priors:suppression_score(wikipedia_notability_requirement_2026, 0.68).
domain_priors:theater_ratio(wikipedia_notability_requirement_2026, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, extractiveness, 0.52).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(wikipedia_notability_requirement_2026, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wikipedia_notability_requirement_2026, tangled_rope).
narrative_ontology:human_readable(wikipedia_notability_requirement_2026, "Wikipedia Notability Requirement (2026)").
narrative_ontology:topic_domain(wikipedia_notability_requirement_2026, "social/technological").

domain_priors:requires_active_enforcement(wikipedia_notability_requirement_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, established_publishers).
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, wikipedia_editorial_gatekeepers).
narrative_ontology:constraint_beneficiary(wikipedia_notability_requirement_2026, institutional_subjects).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, emerging_creators).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, non_english_communities).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, marginalized_subjects).
narrative_ontology:constraint_victim(wikipedia_notability_requirement_2026, wikipedia_encyclopedia_completeness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING CREATOR (SNARE) — Subject to gatekeeping with no meaningful appeal. Cannot exit: the subject either achieves notability through external validation or remains unencyclopedic. No recourse for disputing the evaluation. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.78.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ENGLISH COMMUNITIES (SNARE) — Notability criteria privilege English-language sources and Western institutions. Cannot exit: subjects documented only in local languages face structural disadvantage. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.80.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: VOLUNTEER REVIEWER (TANGLED ROPE) — Benefits from encyclopedia quality maintenance (coordination function) but bears burden of enforcing standards with limited guidance and high interpersonal friction. Trapped in role by sense of stewardship; exit costs include loss of community standing. d≈0.62, f(d)≈0.90, σ=1.0 → χ≈0.47.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED PUBLISHERS (ROPE) — Benefit from notability requirement's citation loop: their publications are used as evidence of notability, creating self-reinforcing access. Can arbitrage: publishing houses have institutional credibility that grants subjects notability via inclusion in their catalogs. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: WIKIPEDIA EDITORIAL AUTHORITY (TANGLED ROPE) — Enforces notability policy (extraction from subjects) while also managing the encyclopedia as a coordination mechanism (Rope function: curating reliable content). Constrained by community governance and lack of resources to verify subjective 'significance.' Benefits from gatekeeping power; constrained by consensus requirement. d≈0.35, f(d)≈0.32, σ=1.2 → χ≈0.20.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NOTABILITY POLICY FRAMEWORK (PITON) — Theater ratio 0.64 reflects heavy performative element: the policy is enforced through ritual (AfD deletion discussions, citation hunts, debate over source quality) rather than clear functional criteria. The underlying question 'is this subject important enough?' remains unanswerable by policy; enforcement persists through institutional inertia despite chronic ambiguity.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination (preventing unreliable content) and genuine extraction (gatekeeping creates access asymmetry favoring established sources). Neither function dominates from a civilizational perspective; Wikipedia simultaneously democratizes knowledge and reproduces status hierarchies. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.71.
constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wikipedia_notability_requirement_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wikipedia_notability_requirement_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wikipedia_notability_requirement_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wikipedia_notability_requirement_2026, TR),
    TR >= 0.70.

:- end_tests(wikipedia_notability_requirement_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The policy systematically advantages established publishers (institutional credibility grants subjects notability via inclusion in their catalogs) while disadvantaging creators dependent on primary documentation or non-institutional sources. The extraction is not total because alternative pathways exist (Wikidata, wikis, personal websites) and Wikipedia's notability policy has become more permissive for niche subjects over time. However, the economic value captured by Wikipedia (free, high-rank encyclopedia articles) creates meaningful asymmetry favoring those with established publication infrastructure. Suppression (0.68): High. Barriers include requirement for 'reliable sources' (privilege institutional publishers), difficulty proving 'independent notability' (circular definition), and AfD process itself (time-consuming, adversarial). Appeals exist but are slow and uncertain. Theater ratio (0.64): Moderate-high. Notability policy is enforced through ritual discussion (AfD debates, citation hunts, source quality disputes) that performs decision-making without producing coherent criteria. Editors spend significant effort in performative argumentation; the underlying question 'is this subject important enough?' remains unanswerable by policy. Theater increased from 0.48 to 0.64 over the interval as the volume of AfD discussions expanded while the underlying ambiguity remained constant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximal perspectival disagreement. Established publishers see pure Rope (coordination that maintains quality). Emerging creators see pure Snare (extraction with no escape). Volunteer reviewers experience Tangled Rope (simultaneous stewardship and burden). The policy framework itself is Piton (performative enforcement of degraded standards). The editorial authority sees Tangled Rope (coordination + extraction trade-off). The analytical observer sees Tangled Rope (genuine coordination need co-existing with genuine gatekeeping asymmetry). This is not a failure of the framework — it reveals that the notability requirement genuinely performs both functions simultaneously. The perspectival gap reflects real structural differences in how agents experience the constraint, not disagreement about the facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Emerging creators: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. Non-English communities: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction due to structural language barrier. Established publishers: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; low d. Volunteer reviewers: Victim + constrained → d≈0.62, f(d)≈0.90. Significant extraction because exit has cost (community standing, stewardship role). Editorial authority: Organized + constrained → d≈0.35, f(d)≈0.32. Lower extraction despite power; constrained by consensus governance and ambiguous standards. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Sees full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that notability requirement is genuinely a hybrid mechanism. It is not 'coordination mislabeled as extraction' — it performs real content quality control (Rope function). It is not 'pure extraction mislabeled as coordination' — that would require no genuine quality problem to solve. The Tangled Rope classification holds because: (1) Beneficiaries exist (quality-focused volunteers, established institutions) and they derive genuine coordination benefit (avoiding spam, organizing reliable knowledge). (2) Victims exist (emerging creators, non-English communities) and extraction is material (asymmetric access to encyclopedic prestige). (3) Active enforcement is required — the policy must be continually re-justified in AfD discussions because no coherent standard generates automatic compliance. The theater ratio (0.64) indicates degraded Rope function: enforcement relies on ritual rather than clear criteria. This is not a Piton (would require theater ≥ 0.70 and ε ≤ 0.25; we have ε=0.52). This is Tangled Rope with increasing performative overhead. The resolution is not to eliminate the constraint (quality problems are real) but to reduce theater by developing more coherent notability criteria or explicit source-independence pathways.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_circularity,
    'Does the notability requirement enforce an empirically resolvable standard or an institutionally circular one?',
    'Analysis of overturned AfD decisions: if overturned articles demonstrate alternative source bases that satisfy the same criteria, the standard is empirically coherent. If overturns are reversed again, the standard is circular.',
    'If empirically resolvable: Tangled Rope holds with coordination function dominant. If circular: classification shifts toward Snare, as gatekeepers enforce power rather than an objective standard.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notability_circularity, empirical, 'Whether notability criteria are empirically resolvable or institutionally circular').

omega_variable(
    source_hierarchy_bias,
    'What fraction of notability decisions depend on source prestige (journal, publisher, news outlet) versus subject characteristics?',
    'Controlled retrospective study: identical notability claims (same subject, same evidence quantity) presented with varying source tiers (arXiv vs Nature, local blog vs Reuters, self-published vs academic press). Measurement of decision variance attributable to source tier.',
    'High variance by source tier (>60%): extraction mechanism dominant, victims are creators without institutional backing. Low variance (<20%): coordinate decision-making, Rope or pure Tangled Rope. Medium variance: current state confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(source_hierarchy_bias, empirical, 'Bias toward established sources in notability decisions').

omega_variable(
    encyclopedia_coverage_completeness,
    'Does the notability requirement maximize or minimize Wikipedia''s utility as a comprehensive reference?',
    'Comparison of deletion rate by subject category (scientists, artists, athletes, technical topics, regional subjects, marginalized communities). Correlation with external importance metrics (academic citation counts, impact measures, community size).',
    'If notability deletions correlate with low external importance: Rope (coordination working correctly). If deletions correlate with marginalized status despite external importance: Snare (gatekeeping independent of actual importance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(encyclopedia_coverage_completeness, empirical, 'Whether notability requirement serves encyclopedic completeness').

omega_variable(
    appeal_mechanism_effectiveness,
    'Can creators realistically challenge notability decisions, or does the policy permit only downstream accommodation?',
    'Tracking of AfD overturned rates by appeal type: resubmission after improvement vs. reversal of previous decision vs. policy reinterpretation. Measurement of perceived fairness in appeals process by creators.',
    'High overturn rates and creator confidence: exits are genuinely mobile, shift toward Rope or Scaffold. Low overturn rates and creator frustration: exits are constrained or trapped, supporting Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(appeal_mechanism_effectiveness, empirical, 'Effectiveness of appeal mechanisms for notability decisions').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wikipedia_notability_requirement_2026, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wpnot_tr_t0, wikipedia_notability_requirement_2026, theater_ratio, 0, 0.48).
narrative_ontology:measurement(wpnot_tr_t8, wikipedia_notability_requirement_2026, theater_ratio, 8, 0.58).
narrative_ontology:measurement(wpnot_tr_t16, wikipedia_notability_requirement_2026, theater_ratio, 16, 0.64).

% Extraction over time
narrative_ontology:measurement(wpnot_be_t0, wikipedia_notability_requirement_2026, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wpnot_be_t8, wikipedia_notability_requirement_2026, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(wpnot_be_t16, wikipedia_notability_requirement_2026, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wikipedia_notability_requirement_2026, information_standard).
narrative_ontology:affects_constraint(wikipedia_notability_requirement_2026, wikipedia_deletion_bias_geographic).
narrative_ontology:affects_constraint(wikipedia_notability_requirement_2026, encyclopedia_coverage_completeness).

% DUAL FORMULATION NOTE:
% The notability requirement decomposes into several structurally distinct constraints: (1) content quality assurance (coordination, low ε), (2) source prestige bias (extraction, high ε), (3) language/cultural bias (extraction, high ε), (4) appeal mechanism effectiveness (varies). This story treats notability as the unified gatekeeping policy. Upstream constraints (what defines 'reliable source,' what constitutes 'independent notability') affect this one. Downstream constraints (geographic deletion bias, marginalized subject underrepresentation) are affected by this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wikipedia_notability_requirement_2026, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
