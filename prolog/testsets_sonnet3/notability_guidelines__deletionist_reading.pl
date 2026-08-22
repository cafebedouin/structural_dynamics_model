% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   This story instantiates the deletionist reading of the
 *   notability_guidelines kernel: WP:N (Wikipedia's General Notability
 *   Guideline and its subject-specific variants) functions as a coordination
 *   mechanism that lets a large, uncoordinated volunteer editor corps agree
 *   on inclusion thresholds without central editorial fiat, protecting the
 *   encyclopedia's verifiability and readability for its readership. On this
 *   reading, exclusion of marginally-sourced subjects is not extraction from
 *   a victim class — it is the correct operation of a quality filter applied
 *   to subjects whose secondary-source record does not yet support a neutral,
 *   verifiable article. This is one of three sibling constraints sharing the
 *   notability_guidelines kernel: the inclusionist_reading treats the same
 *   guideline as systematically exclusionary gatekeeping (high extraction,
 *   real victim set of marginalized-topic contributors), and the
 *   deliberative_reading treats it as an evolving negotiation process rather
 *   than a fixed filter. All three share the same kernel text (WP:N) but
 *   diverge sharply on beneficiary/victim structure and ε — per the
 *   ε-invariance principle, each is authored as its own constraint rather
 *   than as one story with a hidden observable parameter.
 *
 * KEY AGENTS:
 *   - encyclopedia_readership: primary beneficiary (moderate/mobile) — receives higher-quality, verifiable corpus
 *   - volunteer_editor_corps: agenda-setter and secondary beneficiary (organized/mobile) — administers the filter, reduces own maintenance burden
 *   - article_subjects_marginal_notability: bears exclusion (powerless/constrained) — read here as correctly filtered, not victimized
 *   - administrators_and_afd_closers: agenda-setter (institutional/mobile) — interprets and closes deletion discussions
 *   - platform_operating_foundation: analytical observer (institutional/analytical) — hosts infrastructure, rarely intervenes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.14).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.28).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.14).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'cc41601d-77f3-4982-abf9-015b88302ab8').
narrative_ontology:cs_kernel_codification('cc41601d-77f3-4982-abf9-015b88302ab8', formalized).
narrative_ontology:cs_authority_grounding('cc41601d-77f3-4982-abf9-015b88302ab8', practice).
narrative_ontology:cs_interpretation_layer_present('cc41601d-77f3-4982-abf9-015b88302ab8').
narrative_ontology:cs_reading_relation('cc41601d-77f3-4982-abf9-015b88302ab8', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cc41601d-77f3-4982-abf9-015b88302ab8', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('cc41601d-77f3-4982-abf9-015b88302ab8', foundational, secondary_sourcing_as_neutral_inclusion_threshold).
narrative_ontology:cs_axiom_status(secondary_sourcing_as_neutral_inclusion_threshold, holdable).
narrative_ontology:cs_axiom_grounding('cc41601d-77f3-4982-abf9-015b88302ab8', secondary_sourcing_as_neutral_inclusion_threshold, conventional).
narrative_ontology:cs_axiom('cc41601d-77f3-4982-abf9-015b88302ab8', foundational, exclusion_of_unsourced_subjects_is_not_extraction).
narrative_ontology:cs_axiom_status(exclusion_of_unsourced_subjects_is_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('cc41601d-77f3-4982-abf9-015b88302ab8', exclusion_of_unsourced_subjects_is_not_extraction, instrumental).
narrative_ontology:cs_created_at('cc41601d-77f3-4982-abf9-015b88302ab8', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, volunteer_editor_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(notability_guidelines__deletionist_reading, article_subjects_marginal_notability).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, verifiability_as_encyclopedic_baseline).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, secondary_source_requirement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consumes the encyclopedia expecting verifiable, well-sourced entries. Benefits when notability screening keeps the corpus navigable and reliably sourced rather than diluted with unverifiable vanity or promotional entries. Can leave for other reference sources at any time; has no direct role in enforcement but is the reason the filter is claimed to exist.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, encyclopedia_readership, beneficiary,
    moderate, generational, mobile, global).

% Writes, patrols, and nominates articles for deletion using the notability guideline as the operative standard. Benefits from a workable filter because it reduces the maintenance burden of policing an otherwise unbounded influx of subjects. Can step back from editing without personal cost, though sustained participation is identity-forming for many.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editor_corps, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, volunteer_editor_corps, beneficiary).

% Individuals, organizations, or topics whose coverage in independent secondary sources is thin get their proposed or existing articles removed. From the deletionist reading, this is not a victim class but a correctly-filtered case: the subject genuinely lacks the secondary-source record the encyclopedia needs to write a neutral, verifiable article, and can seek coverage in more permissive venues (wikis, blogs, niche databases).
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, article_subjects_marginal_notability, payer,
    powerless, immediate, constrained, global).

% Attempt to use the encyclopedia's visibility for self-promotion or undisclosed advertising. The guideline's enforcement removes their content by design; they have no standing voice in AfD discussions because their interest (promotion) is not one the project recognizes as legitimate input.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, spam_and_vanity_page_creators, excluded,
    powerless, immediate, constrained, global).

% Interpret and apply the notability guideline at Articles for Deletion, closing discussions by assessing consensus against the sourcing standard. They administer the filter and could in principle loosen or tighten it, but operate under community-wide policy rather than personal discretion.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, administrators_and_afd_closers, agenda_setter,
    institutional, generational, mobile, global).

% Hosts the infrastructure and funds legal defense of editorial decisions but does not itself write or enforce content policy. Watches the notability system operate as a self-governing mechanism and intervenes only in extreme cases (legal risk, foundation-level policy).
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, platform_operating_foundation, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, community-legible threshold — significant coverage in independent, reliable secondary sources — that lets thousands of uncoordinated volunteer editors decide, without central editorial authority, which subjects merit a standalone article, preventing the corpus from being overrun by unverifiable or self-interested content.
% TRANSFER_FUNCTION: Moves editorial attention and inclusion-space away from subjects lacking independent secondary coverage and toward subjects the existing source record can support, preserving the encyclopedia's average verifiability and readability for its readership.
% ABSENT_VOICES: Subjects excluded for thin sourcing have no formal voice in the AfD process beyond a rebuttal on the talk page; from this reading their absence is not suppression but the natural consequence of the sourcing record not yet existing — they would object, but their objection does not by itself supply the missing secondary sources.
% DISAPPEARANCE_RATIONALE: If the notability guideline vanished overnight, the deletionist reading holds that the encyclopedia would rapidly fill with unverifiable, promotional, and vanity content, editor attention would fragment across an unbounded set of low-quality articles, and the verifiability standard that gives the corpus its reference value would erode within a short period.
% FOUNDING_PROBLEM: Early Wikipedia had no consistent standard for what deserved a standalone article, leading to uncontrolled growth of vanity pages, promotional entries, and unverifiable trivia that diluted the reference value of the corpus and consumed disproportionate editor attention on disputes with no shared criterion for resolution.
% FOUNDING_PROBLEM_CORROBORATION: Independent researchers studying Wikipedia's content quality (e.g. studies of article reliability and deletion outcomes) and journalists covering online reference reliability corroborate that unverifiable and promotional content remains a live, ongoing pressure on the project, not a historical problem the guideline solved once and can now retire from.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.14, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low (0.14) because, on this reading, the guideline's operation transfers essentially nothing of value away from any party with a legitimate claim to inclusion — it screens out content the source record cannot yet support. Suppression is moderate-low (0.28): AfD enforcement is real and can feel coercive to an affected article's creator, but alternatives are not suppressed (content can migrate to other wikis, personal sites, or return once sources accumulate). Accessibility_collapse is moderate (0.35) — the standard is stable and well-documented, but its application involves editorial judgment calls that some subjects can contest and win. Resistance is moderate (0.3): most deletions are uncontested or mildly contested; a persistent minority of editors dispute individual applications without disputing the standard itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Readership and the editor corps sit near the beneficiary end: the guideline reduces their maintenance and search costs and is a coordination device they collectively wrote and continue to police. Marginal-notability subjects sit closer to a mild-target position, but on this reading their position is structurally distinct from a victim's — the guideline does not extract value they were entitled to; it withholds a platform benefit (a standalone article) pending an objective sourcing threshold they can meet later. This is why base_properties.victims is deliberately left empty on this reading — declaring a victim set here would contradict the deletionist premise that exclusion is justified filtering, not extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (uncontrolled vanity/promotional content diluting the corpus) is authored as still live, corroborated by independent research on content-quality pressure, which forecloses a mandatrophy reading on this axis — the mandate has not obviously outlived its function. Classifying this reading as rope rather than tangled_rope or snare depends on treating the exclusion of thinly-sourced subjects as a cost of legitimate filtering rather than an asymmetric extraction; the sibling inclusionist_reading disputes exactly this move by naming a victim class where this reading names none.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    filter_vs_gatekeeping_boundary,
    'Is the line the notability guideline draws between ''insufficiently sourced'' and ''systematically under-covered'' a neutral epistemic threshold, or does it encode structural bias in which topics accumulate secondary source coverage in the first place?',
    'Comparative studies of AfD outcomes by topic demographic (geographic origin, gender, language of primary sources) against source-availability baselines; if exclusion correlates strongly with known media-coverage biases independent of actual notability, the deletionist premise weakens.',
    'If the correlation is strong and independent of underlying notability, this reading''s claim that no victim class exists becomes harder to sustain, pushing the constraint toward the inclusionist reading''s tangled_rope/snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(filter_vs_gatekeeping_boundary, empirical, 'Whether notability screening is bias-neutral filtering or encodes source-availability bias.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Which of the three readings of WP:N (deletionist, inclusionist, deliberative) best captures the guideline''s actual operating logic, given that all three are defensible framings of the same kernel text?',
    'This is inherent to the committer-frame structure: no single empirical test resolves it because each reading is a different normative lens on the same enforcement data. Cross-reading comparison of authored ε and victim declarations (this file vs. its siblings) is the intended analytical output, not a resolution.',
    'Different readings assign this same underlying text different types (rope vs. tangled_rope vs. an evolving-process framing) and different ε values; policy conclusions about reforming or retaining WP:N depend heavily on which reading is adopted as authoritative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Structural under-determination among the three sibling readings of the notability_guidelines kernel.').

omega_variable(
    afd_enforcement_discretion_scope,
    'How much of AfD closure outcomes is determined by the written guideline text versus closer discretion and prevailing community sentiment at the time of the discussion?',
    'Analysis of AfD closure rationales over time for consistency given similar sourcing profiles; high variance would indicate discretion dominates the written standard.',
    'High discretion variance would support the deliberative_reading''s framing over this reading''s fixed-filter framing, and would raise the effective suppression/accessibility_collapse values authored here.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(afd_enforcement_discretion_scope, empirical, 'Whether the guideline functions as a fixed filter or a discretion-heavy deliberative process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.09).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.12).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.135).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.14).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(notability_guidelines__deletionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories share the notability_guidelines kernel (same WP:N text, same AfD enforcement machinery): this file (deletionist_reading, rope, ε=0.14, no victim set), notability_guidelines__inclusionist_reading (gatekeeping apparatus, higher ε, declared victim set of systematically under-covered topics), and notability_guidelines__deliberative_reading (evolving negotiation process, distinct beneficiary/victim structure keyed to process participation rather than outcome). Each authors its own ε and stakeholder structure per the ε-invariance principle; do not average or reconcile across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
