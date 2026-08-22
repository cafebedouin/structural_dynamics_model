% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Quality-Preserving Epistemic Filter (Deletionist Reading)
 *   domain: epistemic/digital_commons/governance
 *
 * SUMMARY:
 *   Wikipedia's Notability Guidelines (WP:N) are the primary mechanism by
 *   which volunteer editors enforce a minimum threshold for article
 *   inclusion: subjects must have received significant independent coverage
 *   in reliable sources. The deletionist reading frames this as necessary
 *   epistemic quality control: notability gating prevents the encyclopedia
 *   from degrading into vanity, self-promotion, and fringe theories. Readers
 *   benefit from a curated, verifiable reference; volunteer editors benefit
 *   from clear standards; the digital commons is protected from
 *   commons-tragedy degradation. This reading faces significant contestation
 *   from inclusionist and deliberative alternatives (sibling constraints,
 *   other kernel readings) that argue WP:N systematically excludes legitimate
 *   knowledge, marginalizes non-institutional voices, and serves gatekeeping
 *   rather than quality. The claim/metric gap is intentional: extractiveness
 *   is authored at 0.28 (moderate, reflecting the deletionist reading's own
 *   assessment that some excluded voices bear costs) while the claimed type
 *   is Rope (coordination-dominant). The engine will compute per-seat
 *   classifications; divergence is expected and reveals seat-specific
 *   experience of the constraint.
 *
 * KEY AGENTS:
 *   - encyclopedia_readership: organized, mobile globally — benefits from quality filtering; sees genuine coordination value
 *   - volunteer_editors: moderate power, biographical horizon — enforce standards; experience legitimate curation work mixed with boundary-policing burden
 *   - excluded_potential_subjects: powerless, trapped locally — bear costs of non-notability; have no internal voice within the constraint's own epistemic rules
 *   - reliable_source_ecosystem: powerful, arbitrage-capable — institutional publishers and news organizations benefit from notability-gating's reinforcement of their authority
 *   - platform_governance_body: institutional, analytical — maintains policy; structurally positioned as custodian rather than extractor
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.28).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.42).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines as Quality-Preserving Epistemic Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "epistemic/digital_commons/governance").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4').
narrative_ontology:cs_kernel_codification('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', formalized).
narrative_ontology:cs_authority_grounding('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', expertise).
narrative_ontology:cs_interpretation_layer_present('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4').
narrative_ontology:cs_reading_relation('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', foundational, notability_criteria_epistemically_neutral).
narrative_ontology:cs_axiom_status(notability_criteria_epistemically_neutral, holdable).
narrative_ontology:cs_axiom_grounding('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', notability_criteria_epistemically_neutral, empirically_contingent).
narrative_ontology:cs_axiom('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', foundational, commons_degradation_without_gatekeeping).
narrative_ontology:cs_axiom_status(commons_degradation_without_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', commons_degradation_without_gatekeeping, empirically_contingent).
narrative_ontology:cs_reference_frame('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', notability_as_neutral_quality_criterion).
narrative_ontology:cs_drift_state('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', contemporary_debate_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('e9b94d7b-9174-4c24-920e-7a0cb2e6f3c4', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, knowledge_commons_integrity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, reliable_source_ecosystem).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receives a curated, verifiable encyclopedia filtered for notability. This reading holds that deletionist enforcement preserves signal-to-noise ratio: readers can trust that subjects with Wikipedia entries meet a minimum bar of public significance, reliable sourcing, and encyclopedic relevance. Exit is available but costly: no equally comprehensive alternative exists at comparable quality and accessibility.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, encyclopedia_readership, beneficiary,
    organized, generational, mobile, global).

% Enforce notability guidelines through article deletion (AfD), redirect, or demotion. They interpret and apply WP:N, make deletion judgments, and maintain the epistemic boundary. In the deletionist reading, their work is essential curation: rejecting vanity, spam, and non-notable fringe claims protects the encyclopedia's epistemic standing. Exit is available; editors can leave or shift to inclusionist projects.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editors, agenda_setter,
    moderate, biographical, mobile, global).

% Individuals, organizations, or ideas rejected as non-notable: local businesses, niche subcultures, emerging figures, fringe academic theories, marginalized historical records. They would argue for inclusion but have no seat at AfD deliberation. In the deletionist frame, their exclusion is justified—they lack the reliable sourcing or public prominence WP:N requires—but they cannot appeal this judgment within the constraint's own epistemic rules.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, excluded_potential_subjects, excluded,
    powerless, immediate, trapped, global).

% Academic publishers, news organizations, and established institutions benefit from notability-gating: their publications are the gatekeepers for what counts as 'reliable source,' and WP:N enforcement ensures Wikipedia's editorial standards reinforce that gatekeeping. They have multiple channels (publish elsewhere, build corporate wikis, seed media coverage) and are not trapped.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, reliable_source_ecosystem, beneficiary,
    powerful, generational, arbitrage, global).

% Wikimedia Foundation and the broader Wikipedia governance structure maintain WP:N as policy, fund the enforcement infrastructure, and adjudicate disputes over guideline interpretation. They could revise or abandon the guidelines but treat deletionism as necessary to the project's stated mission: a reliable, verifiable encyclopedia. Their position is structural rather than extractive.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, platform_governance_body, agenda_setter,
    institutional, generational, analytical, global).

% A non-agent entry for the epistemic commons itself: the abstract good of maintaining a shared knowledge resource free from vandalism, promotion, and unsourced claims. In the deletionist reading, this is what WP:N protects—not a party that collects from the constraint, but a condition the constraint's operation vindicates.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, commons_integrity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(notability_guidelines__deletionist_reading, commons_integrity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of maintaining a shared knowledge resource: without notability gatekeeping, the encyclopedia would accumulate self-promotion, fringe theories, and unsourced claims, degrading its utility as a trusted reference. Notability standards align contributor effort toward articles meeting encyclopedic criteria and reader expectations.
% TRANSFER_FUNCTION: Moves editorial authority and content-inclusion decisions away from individual article-creators toward a community-enforced notability standard. Subjects 'transfer' from 'proposed article' to either 'accepted entry with encyclopedic status' or 'rejected as non-notable.' In the deletionist frame, this is not extraction but calibration: the transfer is the standard itself, not a hidden rent.
% ABSENT_VOICES: Excluded subjects themselves (local figures, niche communities, fringe scholars) have no seat at the AfD table. They experience the constraint but cannot challenge its epistemic premises within the Wikipedia process. External critics arguing for inclusionism (other kernel readings) are present in the larger discourse but not in individual deletion decisions.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight and notability enforcement ceased, Wikipedia would face rapid degradation: vanity articles, spam, promotional content, and fringe theories would accumulate; the encyclopedia's reliable-reference status would erode; readers would seek alternatives or invest less trust in entries; the volunteer editor base would fracture over quality standards. The digital commons would reorganize around either a new epistemic-quality standard or multiple competing wikis with different philosophies.
% FOUNDING_PROBLEM: Early Wikipedia faced vandalism, promotional content, and article-writing as marketing: without editorial standards, the encyclopedia risked becoming a bulletin board rather than a curated reference. Notability guidelines emerged to solve the problem of maintaining epistemically coherent, verifiable content at scale.
% FOUNDING_PROBLEM_CORROBORATION: Wikimedia Foundation and core editor communities attest the founding problem is still live: vandalism, spam, and promotional biographies continue daily and require active deletion. Inclusionist critics and marginalized knowledge scholars attest the problem has been largely solved (vandalism is manageable through other means) and notability now serves gatekeeping rather than quality preservation. Neither reading is corroborated solely within the constraint's own beneficiary set; both appeal to observable facts (deletion rates, spam trends, editor workload) and to divergent epistemic principles.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).
:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is measured at 0.28 because the deletionist reading holds that excluded subjects are justly excluded—their non-notability is not suppression but accurate epistemic filtering. However, 0.28 is not zero because even in the deletionist frame some excluded voices bear real costs: local historians, emerging figures, marginalized communities whose knowledge is not yet documented in 'reliable sources' (a definition itself tied to institutional gatekeeping). The measured extractiveness reflects the reading's own acknowledgment that notability standards have winners and losers; the reading justifies this as necessary, not as hiding it. Suppression is 0.42 because exclusion enforces a boundary—subjects are kept out by active deletion and policy, not by lack of interest—but suppression is not higher because the mechanism is transparent (AfD is public, criteria are written, editors explain deletions) and because inclusion remains formally available to any subject that acquires sufficient sourcing. Theater is low (0.18) because the core editorial work—verifying sources, assessing notability, writing encyclopedia entries—is genuine; theater reflects the boundary-policing burden (explaining why something is not notable takes institutional effort that could go to writing) but does not suggest the function is theatrical. Accessibility_collapse is high (0.71) because once a subject is marked non-notable, alternatives collapse: Wikipedia's scale and search-engine ranking mean non-inclusion is nearly equivalent to digital erasure for many readers. Resistance is substantial (0.58) because inclusionist editors, marginalized-knowledge advocates, and external critics actively resist the constraint; AfD discussions are contested; deletionism is not accepted as natural law. The measurement series shows slow drift: extractiveness and suppression inch upward over the interval as notability enforcement becomes more consistent and formalized, and as institutional source-dependence deepens. Theater rises slightly as boundary-policing documentation increases. This modest drift is consistent with a Rope constrained by increasing formalization rather than a Snare capturing value or a Piton in theatrical maintenance.
 *
 * PERSPECTIVAL GAP:
 *   The volunteer editor seat and the excluded subject seat will compute starkly different types from identical structural data. Editors experience WP:N as a coordination mechanism they maintain collectively—shared standards, defensible exclusion, protection of the commons. Excluded subjects experience it as structural gatekeeping they cannot contest—their knowledge is deemed 'non-notable' by criteria they did not set and cannot satisfy within the constraint's own rules (if reliable sources don't cover your subject yet, being reliable doesn't help; you are trapped in a dependency loop). Readers experience modest benefit-at-a-cost: quality is genuinely preserved, but some legitimate knowledge is hidden. The engine's per-seat computation will expose this divergence. From the deletionist reading's own frame, this divergence is not a defect—it is the constraint working as designed: protecting shared standards requires some exclusion to be structural and unavoidable. From inclusionist and deliberative readings (other sibling constraints), the same divergence is the constraint's defining extractive feature. The claim stays independent of the metrics to preserve this interpretive diversity.
 *
 * DIRECTIONALITY LOGIC:
 *   Encyclopedia readership benefits globally (low d, asymptoting to beneficiary); reliable-source institutions benefit in a coupled way (low d asymptoting to moderate beneficiary—they benefit from notability-gating reinforcing their authority). Volunteer editors sit near symmetric: they benefit from shared standards and community governance, but they bear the burden of boundary-maintenance labor (d ≈ 0.45). Excluded potential subjects bear costs without benefit (high d, asymptoting to 0.85–0.95), but in the deletionist frame this is justified exclusion, not trapped extraction—the distinction is contested in omega variables. Platform governance sits analytical (d ≈ 0.5 by institutional convention, neither collecting nor paying in the deletionist frame). No directionality overrides are needed; the derivation from beneficiary/victim declarations and exit options produces the right structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested: is notability-gating still solving the problem it was built to solve (preventing commons degradation through vanity and spam), or has the problem shifted and the constraint persists by inertia, becoming instead a mechanism for institutional gatekeeping? The deletionist reading holds the founding problem is still live. The engine's mandatrophy logic will flag the divergence between founding_problem_status=contested and disappearance_verdict=world_rearranges: if the world would genuinely rearrange, the constraint is still doing structural work, not just performing. The measurement series shows slow metric drift rather than sharp theater_ratio rise or suppression_requirement collapse; this is consistent with a Rope whose founding problem is being challenged but not yet overridden. If sister-wiki data in omega resolution shows quality-acceptable alternatives without strict notability gating, mandatrophy logic would fire and suggest the constraint has become piton-adjacent (justified by institutional inertia more than coordination necessity). The current authored state holds that mandatrophy is unresolved: the constraint's mandate (preserve epistemic quality) is live but its necessity is contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_criteria_stability,
    'Are the notability criteria (WP:N) epistemically stable, or do they systematically exclude categories of legitimate knowledge?',
    'Comparative analysis of deletion patterns by subject category (geographic origin, academic discipline, socioeconomic profile of subjects): if deletions cluster on marginalized-knowledge categories at rates unexplained by sourcing patterns, the criteria are biased and the exclusion is not justified epistemic filtering.',
    'If criteria are biased, the constraint reclassifies from rope (justified gatekeeping) to tangled_rope or snare (gatekeeping that extracts authority from dominant-knowledge suppliers). The suppression metric would be reinterpreted as cultural rather than vandalism-protective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_criteria_stability, empirical, 'Whether WP:N criteria instantiate neutral epistemic quality or structural bias against marginalized knowledge.').

omega_variable(
    reading_contested_kernel,
    'This constraint is one reading (deletionist) of the notability_guidelines kernel. Are the sibling readings (inclusionist, deliberative) logically foreclosed by the deletionist axioms, or do they coexist as live positions?',
    'Structural analysis of axiom contradiction: if the deletionist axiom ''notability criteria are epistemically neutral quality filters'' and the inclusionist axiom ''notability criteria reflect dominant-knowledge bias'' can both be held by different parties without internal contradiction, they coexist; if one directly contradicts the other such that no framework could hold both, foreclosure applies.',
    'If coexistence holds, the three readings form a genuine constraint family with contested epistemic grounds; the engine''s per-seat classification will diverge (deletionist editors see rope; inclusionist critics see snare). If foreclosure holds, the readings are genuinely incommensurable and the constraint family represents competing metaphysical claims, not just empirical disagreement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contested_kernel, conceptual, 'Whether the three notability readings coexist or whether one logically forecloses the others.').

omega_variable(
    commons_degradation_empirical,
    'What is the empirical baseline for ''commons degradation without notability standards''? Is it a genuine risk or a cover story?',
    'Natural experiment from sister wikis with weaker notability enforcement (Fandom, Wikivoyage, Everipedia) and comparison of reader-trust metrics, edit-war frequency, and vandalism rates relative to Wikipedia''s.',
    'If sister wikis show rapid quality collapse without notability enforcement, the deletionist reading''s founding problem is live and the constraint''s coordination function is real. If sister wikis maintain acceptable quality without strict notability gating, the constraint becomes harder to justify as necessary (may reclassify to piton—persistence via inertia rather than coordination need).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_degradation_empirical, empirical, 'Whether strict notability enforcement is empirically necessary to prevent commons degradation.').

omega_variable(
    reliable_source_circularity,
    'Does the constraint-enforcement loop reinforce existing gatekeepers (academic publishing, mainstream media) and exclude non-institutional knowledge production?',
    'Analysis of source types cited in deleted vs. retained articles: if deletion correlates with reliance on non-peer-reviewed, non-institutional, or community-authored sources, the constraint is coupled to institutional authority in a way that looks like coordination but extracts authority in favor of existing epistemic hierarchies.',
    'If circularity is high, the constraint reclassifies as tangled_rope: coordination (quality preservation for readers) AND extraction (institutional authority reinforcement); the beneficiary shifts from ''readership'' to ''institutional knowledge suppliers.'' Suppression metrics would be reinterpreted as institutional gatekeeping rather than vandalism protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reliable_source_circularity, empirical, 'Whether WP:N enforcement is coupled to existing institutional knowledge hierarchies in a way that produces systematic exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nota_tr_t5, notability_guidelines__deletionist_reading, theater_ratio, 5, 0.14).
narrative_ontology:measurement(nota_tr_t10, notability_guidelines__deletionist_reading, theater_ratio, 10, 0.16).
narrative_ontology:measurement(nota_tr_t15, notability_guidelines__deletionist_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(nota_be_t5, notability_guidelines__deletionist_reading, base_extractiveness, 5, 0.22).
narrative_ontology:measurement(nota_be_t10, notability_guidelines__deletionist_reading, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(nota_be_t15, notability_guidelines__deletionist_reading, base_extractiveness, 15, 0.27).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(nota_su_t5, notability_guidelines__deletionist_reading, suppression_requirement, 5, 0.37).
narrative_ontology:measurement(nota_su_t10, notability_guidelines__deletionist_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(nota_su_t15, notability_guidelines__deletionist_reading, suppression_requirement, 15, 0.41).
narrative_ontology:measurement(nota_su_t20, notability_guidelines__deletionist_reading, suppression_requirement, 20, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.06).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel has three constraint stories corresponding to three live readings: deletionist (this story) — WP:N as quality-preserving coordination; inclusionist — WP:N as structural gatekeeping; deliberative — WP:N as evolving norm-negotiation. Each reading instantiates a different constraint with different ε, beneficiary/victim structures, and claimed types. The stories are linked via network.affects_constraints; they represent a contested kernel, not competing observables of one constraint (per ε-invariance principle). Deletionist and inclusionist readings have an epistemic-grounding dispute (whether notability criteria are neutral quality filters or biased gatekeeping); this dispute lives in the axioms and reading_relations fields, not in the claimed_type or metrics (both readings author their structural truth independently).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
