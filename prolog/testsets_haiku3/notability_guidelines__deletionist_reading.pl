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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Wikipedia Notability Guidelines (Deletionist Reading)
 *   domain: digital_commons/knowledge_infrastructure
 *
 * SUMMARY:
 *   The deletionist reading of Wikipedia notability guidelines frames WP:N as
 *   a necessary epistemic quality filter that preserves the encyclopedia's
 *   coherence and utility. Under this reading, the guidelines solve a genuine
 *   commons coordination problem: preventing unbounded growth, managing
 *   vandalism and spam, and maintaining a bounded reference work. The reading
 *   treats article deletion as legitimate boundary maintenance protecting the
 *   epistemic product, not as unjust exclusion. This constraint is ONE
 *   reading of the contested kernel 'notability_guidelines'; sibling readings
 *   (inclusionist, deliberative) instantiate different constraints with
 *   different beneficiary/victim structures and different ε values. The
 *   deletionist reading treats the excluded (article subjects below the bar,
 *   spam actors) as appropriately excluded rather than victimized.
 *
 * KEY AGENTS:
 *   - encyclopedia_readers: beneficiary (quality preservation)
 *   - knowledge_maintenance_community: beneficiary (bounded scope)
 *   - article_subjects: payer (potential exclusion)
 *   - marginalized_knowledge_producers: excluded (absent from deliberation)
 *   - wikipedia_admin_council: agenda_setter (interprets and enforces policy)
 *   - alternative_knowledge_platforms: observer (demonstrate viability of different models)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.18).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.18).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons/knowledge_infrastructure").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '58e87f72-a840-46af-b6d1-2a1a6ba89949').
narrative_ontology:cs_kernel_codification('58e87f72-a840-46af-b6d1-2a1a6ba89949', fixed_text).
narrative_ontology:cs_authority_grounding('58e87f72-a840-46af-b6d1-2a1a6ba89949', lineage).
narrative_ontology:cs_interpretation_layer_present('58e87f72-a840-46af-b6d1-2a1a6ba89949').
narrative_ontology:cs_reading_relation('58e87f72-a840-46af-b6d1-2a1a6ba89949', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('58e87f72-a840-46af-b6d1-2a1a6ba89949', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('58e87f72-a840-46af-b6d1-2a1a6ba89949', foundational, epistemic_quality_requires_bounded_scope).
narrative_ontology:cs_axiom_status(epistemic_quality_requires_bounded_scope, holdable).
narrative_ontology:cs_axiom_grounding('58e87f72-a840-46af-b6d1-2a1a6ba89949', epistemic_quality_requires_bounded_scope, empirically_contingent).
narrative_ontology:cs_axiom('58e87f72-a840-46af-b6d1-2a1a6ba89949', secondary, marginalized_exclusion_justified_by_necessity).
narrative_ontology:cs_axiom_status(marginalized_exclusion_justified_by_necessity, holdable).
narrative_ontology:cs_axiom_grounding('58e87f72-a840-46af-b6d1-2a1a6ba89949', marginalized_exclusion_justified_by_necessity, conventional).
narrative_ontology:cs_reference_frame('58e87f72-a840-46af-b6d1-2a1a6ba89949', bounded_encyclopedia_governance).
narrative_ontology:cs_drift_state('58e87f72-a840-46af-b6d1-2a1a6ba89949', contemporary_2026, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('58e87f72-a840-46af-b6d1-2a1a6ba89949', '2026-06-12T14:32:15Z').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readers).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, knowledge_maintenance_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, article_subjects).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access a curated encyclopedia where articles meet a consistent evidentiary bar. They benefit from knowing that a claim's presence signals it has passed editorial scrutiny, reducing the cognitive burden of source evaluation. Their exit is costless: they can use competing encyclopedias or return to primary sources if Wikipedia declines in utility.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, encyclopedia_readers, beneficiary,
    powerless, biographical, mobile, global).

% Wikipedia editors who maintain the collection's quality and coherence. They benefit from a bounded problem: deciding notability by a transparent standard prevents infinite growth, keeps maintenance tractable, and preserves the encyclopedia as a distinct epistemic product rather than a universal directory.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, knowledge_maintenance_community, beneficiary,
    organized, generational, constrained, global).

% People, organizations, works, and events evaluated against notability criteria. Those who fail to meet the bar are excluded from Wikipedia's ecosystem. Their exit is constrained: no alternative path exists to claim Wikipedia representation; deletion is permanent and the appeal process is narrow. However, the deletionist reading asserts this exclusion is justified by epistemic necessity, not arbitrary gatekeeping.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, article_subjects, payer,
    powerless, biographical, trapped, global).

% Communities whose scholarship, cultural products, and historical records are systematically underrepresented in citation databases and commercial publishing. They would argue that notability criteria designed around legacy media and institutional affiliation embed colonial/Western/commercial biases. Their exclusion from Wikipedia deliberation is what an inclusionist reading contests.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, marginalized_knowledge_producers, excluded,
    powerless, biographical, trapped, global).

% Actors motivated to add promotional content, false claims, or noise to Wikipedia for marketing or disruption. Notability standards exclude them by design. The deletionist reading treats this exclusion as legitimate boundary maintenance, not an unjust victim set.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, vandalism_and_spam_actors, excluded,
    powerless, immediate, mobile, global).

% Administers notability guidelines and adjudicates borderline cases through AfD (Articles for Deletion) process. They interpret policy, set precedents, and enforce deletions. They do not collect extraction; they serve the community's epistemic mission. The deletionist reading trusts their stewardship; inclusionist and deliberative readings contest whether they accurately represent the community's values.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, wikipedia_admin_council, agenda_setter,
    organized, generational, constrained, global).

% Wikia, Fandom, subject-specific wikis, and other platforms that adopt looser inclusion criteria. Their existence demonstrates that the notability bar is not inevitable—alternative models are viable. From the deletionist framing, their different choices are experiments in different epistemic strategies, not evidence that Wikipedia's bar is wrong.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, alternative_knowledge_platforms, observer,
    organized, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bounded, internally coherent reference work by establishing a shared standard for what qualifies as 'notable enough' for inclusion. This prevents unlimited growth, keeps editorial burden tractable, and preserves Wikipedia's identity as an encyclopedia (curated synthesis) rather than a universal directory or archive.
% TRANSFER_FUNCTION: Moves authority over what is representable in public knowledge infrastructure from individual editors (who would have infinitely varying thresholds) to a collectively negotiated and documented standard. This transfer concentrates gatekeeping power in the admin body and the deliberative process, but the deletionist reading frames this as appropriate delegation to stewards of a shared epistemic resource.
% ABSENT_VOICES: Marginalized knowledge producers and communities whose scholarship is outside the legacy media and institutional citation networks that notability criteria anchor to. They would argue the criteria are biased; their exclusion from the standard-setting process itself is what an inclusionist reading highlights.
% DISAPPEARANCE_RATIONALE: If notability standards disappeared and Wikipedia became an open directory (no deletion, only curation by subject), the platform would be reorganized: maintenance costs would spike, user navigation would become harder (signal-to-noise ratio shifts), and Wikipedia's distinctive epistemic authority would degrade. The encyclopedia would cease to be a bounded reference work and become something more like an indexed archive.
% FOUNDING_PROBLEM: Wikimedia founders recognized that an encyclopedia without editorial standards would collapse into noise: anyone could add anything, vandalism would be unmanaged, and the collection would lose coherence and utility as a reference work. Notability standards emerged as the boundary rule that preserved the epistemic product.
% FOUNDING_PROBLEM_CORROBORATION: The deletionist community attests the problem is live: maintenance costs, spam prevention, and quality preservation remain necessary. Inclusionist scholars and marginalized-knowledge advocates attest the founding problem was real in 2004 but has been partly solved by technology (edit filtering, bots, tagging systems) and that notability standards now serve rent-seeking gatekeeping more than epistemic necessity. Independent scholarship on Wikipedia governance (e.g., Forte & Bruckman, Reagle) corroborates that notability enforcement has become performative in some contexts.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is low (0.12 at t=2026) because there is no concentrated beneficiary collecting rents—readers benefit diffusely, editors benefit from a tractable problem, and the constraint operates without surplus extraction. Suppression is similarly low (0.18): the constraint's persistence depends primarily on shared agreement about epistemic necessity, not coercion. Theater has risen moderately (0.08→0.22) over the interval: early Wikipedia relied on organic curation, but recent decades show increased emphasis on performative enforcement (heated AfD debates, policy elaboration, admin visibility) relative to functional gate-keeping. This suggests the founding problem (spam control) has been partly solved by technology (bots, filters) but enforcement culture persists, indicating some drift toward inertial theater. Accessibility collapse is high (0.68) because once one understands notability criteria, alternatives vanish—there is no way to 'opt out' and still have a Wikipedia article; the choice is binary. Resistance is moderate (0.41) because the guidelines meet real pushback from inclusionists and marginalized communities, but that resistance has not substantially shifted the policy. The measurement series is authored on one shared time grid (5-point interval from 2001 to 2026) so every metric carries a value at every examined moment.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist reading posits consensus: readers, editors, and admins all benefit from the bounded-encyclopedia model, and article subjects excluded by the bar are not victims because their exclusion serves epistemic necessity. An inclusionist reading, by contrast, would compute high directionality divergence: the same constraint extracts from marginalized communities (preventing their representation), benefits privileged knowledge (institutional, Western, commercial), and creates a victim set that the deletionist reading denies. The engine should compute different types for the same constraint evaluated from different seats; the deletionist reading predicts near-unanimous beneficiary classification, while the inclusionist reading would predict sharply divided classifications. The committer structure is recorded in the omegas and axioms below.
 *
 * DIRECTIONALITY LOGIC:
 *   In the deletionist framing: encyclopedia readers (powerless but mobile) are beneficiaries with low d (the constraint subsidizes their information access). The knowledge maintenance community (organized, constrained exit) are beneficiaries with moderate d (they gain a tractable problem, but they have invested institutional identity in the role, making exit costly). Article subjects below the bar (powerless, trapped) sit at moderate-to-high d under the deletionist reading: they bear a cost (exclusion) but not an unjust one—the reading reframes them from 'victims' to 'appropriately excluded by epistemic necessity.' This reframing is the core contestation. An inclusionist reading would compute these same actors as victims (d near 1.0) and declare a snare. The directionality divergence across readings is an omega variable, not a data error.
 *
 * MANDATROPHY ANALYSIS:
 *   The deletionist reading treats notability as alive and necessary. The mandatrophy mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) signals that the founding problem persists but its salience has declined—technology has reduced spam and vandalism, but enforcement culture persists through theater (rising theater_ratio). This is not mandatrophy proper (founding problem dead + constraint persists); it is mandatrophy-contested. The deletionist reading denies mandatrophy; the inclusionist reading asserts it. The deliberative reading treats mandatrophy as the point: notability boundaries should evolve as the founding problem changes. This contestation should be tracked as an omega and resolved through the reading_relations network.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    whether_excluded_are_victims,
    'Are article subjects below the notability bar appropriately excluded by epistemic necessity (deletionist framing) or systematically victimized by embedded criteria bias (inclusionist framing)?',
    'Empirical: audit notability-criterion application across categories (gender, geography, subject domain) to detect systematic disparities. Normative: evaluate whether the criteria correlate to epistemic reliability or to institutional/commercial indexing bias.',
    'If excluded subjects are victims (systematic bias), the constraint is a snare for marginalized communities; directionality recomputation would shift the agenda-setter and major beneficiaries. If exclusion is epistemically justified, the deletionist type holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(whether_excluded_are_victims, empirical, 'Whether notability exclusion is justified by epistemic necessity or constitutes systematic gatekeeping.').

omega_variable(
    foundational_problem_persistence,
    'Does the original founding problem (unbounded growth, vandalism, spam) remain acute, or has technology + community norms substantially solved it?',
    'Quantitative: compare spam-to-legitimate-edit ratios 2001 vs. 2026; measure deletion-rate trends; audit how much enforcement is reactive (fighting vandalism) vs. proactive (preventing low-notability submissions). Qualitative: survey editor perceptions of whether notability gates are solving the named problem or performing a different function.',
    'If founding problem is substantially solved, rising theater_ratio indicates mandatrophy-in-motion; the constraint becomes piton-like (inertial enforcement of settled boundaries). If the problem persists, the theater rise is a side effect of growth/complexity, and the constraint remains rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foundational_problem_persistence, empirical, 'Whether the epistemic necessity motivating the constraint is still acute or has been largely addressed by technology.').

omega_variable(
    knowledge_framework_committer_structure,
    'Does the deletionist reading''s appeal to ''epistemic necessity'' depend on particular epistemological commitments that the inclusionist and deliberative readings reject?',
    'Philosophical: contrast epistemic standards embedded in notability criteria (reliance on legacy media, institutional affiliation, citation counts) with alternative epistemologies (oral tradition, community documentation, marginalized-community scholarship). Structural: map how each reading''s axioms (see cs_structure.axioms) ground different legitimacy claims for the boundary.',
    'If the epistemological grounding is contestable (as an inclusionist reading would argue), the deletionist appeal to necessity is itself a reading, not a discovery. This does not change the type classification but locates the type divergence in foundational axiom differences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_framework_committer_structure, conceptual, 'Whether the deletionist reading''s epistemic necessity appeal depends on epistemological commitments that other readings reject.').

omega_variable(
    alternative_model_viability,
    'Do platforms with looser or absent notability criteria (Wikia, subject wikis, other Wikimedia projects) demonstrate that notability standards are inessential, or do they demonstrate that different epistemic strategies produce different products?',
    'Empirical: compare signal-to-noise ratio, user satisfaction, maintenance burden, and citation impact across platforms with different notability policies. Normative: evaluate whether the difference in products reflects different missions (encyclopedia vs. archive) or different epistemic competence.',
    'If loose criteria produce degraded encyclopedic products (higher noise, lower authority), the deletionist frame is strengthened. If they produce different-but-valid products, notability becomes a design choice, not an epistemic necessity—moving the constraint toward snare or piton framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_viability, empirical, 'Whether notability standards are necessary for epistemic quality or one viable design choice among many.').

omega_variable(
    sibling_reading_boundaries,
    'How sharply does the deletionist reading''s core axiom (epistemic_quality_requires_bounded_scope) differ from the inclusionist axiom (epistemic_plurality_requires_expanded_scope) and the deliberative axiom (boundaries_should_evolve_with_context)?',
    'Analytical: state each axiom precisely and identify whether they can coexist in a single framework or logically foreclose each other. Review historical cases where the three readings collided in real AfD votes to identify points of logical incompatibility vs. points of mere disagreement.',
    'If the axioms logically foreclose each other (e.g., bounded scope → no expansion of scope), the readings belong in ''forecloses'' relation in cs_structure. If they can coexist as different preferences held by different parties, ''coexists_with'' is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_boundaries, conceptual, 'Whether the deletionist, inclusionist, and deliberative readings logically foreclose each other or coexist as contending positions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 2001, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t2001, notability_guidelines__deletionist_reading, theater_ratio, 2001, 0.08).
narrative_ontology:measurement(nota_tr_t2008, notability_guidelines__deletionist_reading, theater_ratio, 2008, 0.14).
narrative_ontology:measurement(nota_tr_t2014, notability_guidelines__deletionist_reading, theater_ratio, 2014, 0.19).
narrative_ontology:measurement(nota_tr_t2020, notability_guidelines__deletionist_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(nota_tr_t2026, notability_guidelines__deletionist_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(nota_be_t2001, notability_guidelines__deletionist_reading, base_extractiveness, 2001, 0.06).
narrative_ontology:measurement(nota_be_t2008, notability_guidelines__deletionist_reading, base_extractiveness, 2008, 0.09).
narrative_ontology:measurement(nota_be_t2014, notability_guidelines__deletionist_reading, base_extractiveness, 2014, 0.11).
narrative_ontology:measurement(nota_be_t2020, notability_guidelines__deletionist_reading, base_extractiveness, 2020, 0.12).
narrative_ontology:measurement(nota_be_t2026, notability_guidelines__deletionist_reading, base_extractiveness, 2026, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(nota_su_t2001, notability_guidelines__deletionist_reading, suppression_requirement, 2001, 0.05).
narrative_ontology:measurement(nota_su_t2008, notability_guidelines__deletionist_reading, suppression_requirement, 2008, 0.12).
narrative_ontology:measurement(nota_su_t2014, notability_guidelines__deletionist_reading, suppression_requirement, 2014, 0.16).
narrative_ontology:measurement(nota_su_t2020, notability_guidelines__deletionist_reading, suppression_requirement, 2020, 0.18).
narrative_ontology:measurement(nota_su_t2026, notability_guidelines__deletionist_reading, suppression_requirement, 2026, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, wikipedia_deletion_practice).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, citation_network_bias).

% DUAL FORMULATION NOTE:
% The kernel 'notability_guidelines' decomposes into three constraint stories, each instantiating the same policy under a different reading. The deletionist reading (this file) frames WP:N as quality preservation (Rope). The inclusionist reading instantiates it as gatekeeping that excludes marginalized knowledge (Snare). The deliberative reading instantiates it as a boundary undergoing contestation and evolution (Scaffold or Tangled Rope). The three ε values differ substantially because each reading has different beneficiary/victim structures and different interpretations of what the policy accomplishes. They are linked via network.affects_constraints to establish the constraint family and enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
