% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Wikipedia Notability Guidelines â Deletionist Reading
 *   domain: digital_commons_governance
 *
 * SUMMARY:
 *   This constraint instantiates the deletionist reading of the Wikipedia
 *   Notability Guidelines (WP:N) kernel. Under this reading, WP:N functions
 *   as a necessary epistemic quality filter that prevents the digital commons
 *   from degrading into an indiscriminate collection of vanity pages, spam,
 *   and non-encyclopedic trivia. The beneficiary is the global readership,
 *   which receives a curated, high-signal reference work. There is no victim
 *   set: non-notable topics (vanity, spam) are excluded justly, and their
 *   proponents have alternative publishing venues. The constraint is claimed
 *   as ropeâpure coordinationâbecause it solves a genuine
 *   collective-action problem (quality control in an open-edit encyclopedia)
 *   without coercive extraction from legitimate participants. The metrics are
 *   authored independently and remain low across the board, consistent with a
 *   coordination mechanism whose primary cost is volunteer editorial labor
 *   rather than extracted rent.
 *
 * KEY AGENTS:
 *   - readership: Primary beneficiary (organized/mobile) â gains epistemic quality and reduced search costs.
 *   - notability_enforcers: Agenda-setter (organized/mobile) â experienced editors who administer the guideline and enforce boundaries through AfD, investing reputational and ideological labor without personal rent extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.15).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines â Deletionist Reading").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'e18cdf3c-29e0-4495-ad54-42d37f6a8dd0').
narrative_ontology:cs_kernel_codification('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', formalized).
narrative_ontology:cs_authority_grounding('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', practice).
narrative_ontology:cs_interpretation_layer_present('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0').
narrative_ontology:cs_reading_relation('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', foundational, encyclopedic_scope_requires_notability).
narrative_ontology:cs_axiom_status(encyclopedic_scope_requires_notability, holdable).
narrative_ontology:cs_axiom_grounding('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', encyclopedic_scope_requires_notability, conventional).
narrative_ontology:cs_axiom('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', foundational, commons_degradation_without_exclusion).
narrative_ontology:cs_axiom_status(commons_degradation_without_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', commons_degradation_without_exclusion, instrumental).
narrative_ontology:cs_reference_frame('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', encyclopedic_integrity_standard).
narrative_ontology:cs_drift_state('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', contemporary_inclusionist_pressure, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('e18cdf3c-29e0-4495-ad54-42d37f6a8dd0', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, readership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Readers who rely on Wikipedia as a high-signal reference work. They benefit from the notability boundary because it suppresses vanity pages, promotional spam, and indiscriminate trivia, reducing search costs and preserving epistemic trust. Their exit option is to use other reference sources or the open web, though Wikipedia's scale gives it default status.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, readership, beneficiary,
    organized, biographical, mobile, global).

% Experienced Wikipedia editors and administrators who maintain the notability guideline text, participate in Articles for Deletion debates, and enforce the boundary against non-notable submissions. They do not collect personal rent from the constraint; their investment is reputational and ideological (protecting encyclopedic integrity). They can exit by ceasing editorial activity.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, notability_enforcers, agenda_setter,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a global volunteer editor corps and a massive readership around a shared, finite-scope encyclopedia by establishing a minimum threshold of third-party significant coverage, preventing the commons from degrading into an indiscriminate accumulation of vanity, spam, and non-encyclopedic trivia.
% TRANSFER_FUNCTION: Moves editorial labor and deliberation cost toward the community of active editors, while transferring the benefit of a filtered, high-signal reference to the global readership. Non-notable topic proponents lose access to Wikipedia's platform and attention, but are not structurally prevented from publishing elsewhere.
% ABSENT_VOICES: Promoters of non-notable products, vanity biographers, and serial spammers are structurally excluded; they would object to the boundary but their exclusion is the constraint's intended function. Marginalized knowledge holders whose subjects lack established secondary sources may also be effectively absent, though the deletionist reading attributes this to source availability rather than guideline injustice.
% DISAPPEARANCE_RATIONALE: If the notability filter vanished, the encyclopedia would rapidly accumulate promotional content, vanity pages, and indiscriminate trivia, degrading reader trust and search efficiency. The editorial community would face an unmanageable quality-control load and would likely need to reconstruct an alternative boundary mechanism to preserve the project's epistemic utility.
% FOUNDING_PROBLEM: Early Wikipedia faced exponential growth in articles of negligible encyclopedic valueâvanity pages, band spam, neologisms, and promotional contentâthreatening the project's credibility and utility as a reference work.
% FOUNDING_PROBLEM_CORROBORATION: Wikimedia Foundation research on reader trust and article quality, independent HCI studies on information retrieval costs in unfiltered corpora, and the project's own pre-notability mailing list archives document the spam and vanity crisis. These sources are outside the immediate beneficiary group (passive readership).
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is low (0.12) because the constraint does not extract material surplus from any legitimate agent; its cost is borne voluntarily by the editor community and returned as a public good. Suppression is low (0.15) because non-notable topic proponents are not coercedâthey are excluded from one platform but retain abundant alternatives (blogs, wikis, social media, academic repositories). Theater ratio is low (0.10) because the quality-filter function is substantive and not primarily performative. Accessibility collapse is moderate-low (0.25): once an author understands Wikipedia's scope, alternatives are visible. Resistance is low-moderate (0.20): inclusionist editors contest specific deletions, but this is framed as normal editorial friction rather than systemic opposition. The temporal series shows slight drift upward in extractiveness and theater as the guideline accumulates bureaucratic complexity, but remains well within rope territory.
 *
 * PERSPECTIVAL GAP:
 *   The readership seat experiences the constraint as pure benefit: a clean, trusted reference source. The notability enforcer seat experiences it as a mission-aligned coordination task. The only structural asymmetry is between these beneficiaries and the excluded non-notable topic proponentâbut the deletionist reading holds that this exclusion is not extraction because the proponent has no legitimate claim to Wikipedia's specific platform, and their exit options to other venues are robust. Thus, from every legitimate seat, the constraint reads as coordination. The perspectival gap appears only when an illegitimate seat (vanity author) is mistakenly treated as a stakeholder; the deletionist reading denies that seat standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Readership is a diffuse beneficiary: the constraint subsidizes their information environment (low d). Notability enforcers are agenda-setters whose investment is ideological and reputational, not financial; they are near-symmetric or mild beneficiaries because the constraint validates their editorial identity and project commitment (d near 0.3). No victim group is declared because no legitimate agent is structurally targeted for extraction. Directionality derivation produces uniformly low d values across all declared agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as rope is protected from mandatrophy because the founding problem (commons degradation by vanity/spam) is live: removing the constraint would recreate the pre-notability crisis. The constraint is not a scaffold because it lacks a sunset clause and is not transitional; it is not a piton because its function is not atrophiedâtheater remains low and the quality-filter role is still actively performed. It is not a snare because there is no identifiable legitimate victim and no coercion of participants who lack alternatives. The deletionist reading thus prevents the false-summit or snare misclassifications that the inclusionist reading would assert.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginalized_knowledge_source_gap,
    'Does WP:N exclude marginalized knowledge because the subjects are inherently non-encyclopedic, or because reliable secondary sources are systemically absent for marginalized topics?',
    'Comparative analysis of AfD outcomes for topics from marginalized versus mainstream sources, controlling for source availability; ethnographic study of editors from underrepresented regions.',
    'If source absence is the primary driver, the deletionist reading holds but may require complementary sourcing initiatives; if the guideline itself introduces bias, the reading''s claim of just exclusion weakens and the structure edges toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_knowledge_source_gap, empirical, 'Whether exclusion is driven by source ecology or guideline bias.').

omega_variable(
    deletionist_naturalization_risk,
    'Is the deletionist framing of WP:N as an objective epistemic filter a genuine description of the constraint, or a naturalization of a constructed editorial preference that benefits existing readers at the cost of excluding legitimate knowledge?',
    'Historical genealogy of the notability guideline showing the contingency of its thresholds and scope; comparison with alternative encyclopedic projects that operate with different or no notability boundaries.',
    'If the filter is shown to be a contingent construction, the deletionist reading''s rope classification becomes contested and the constraint may reclassify as scaffold or tangled_rope depending on beneficiary structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deletionist_naturalization_risk, conceptual, 'Whether the quality filter is naturalized construction.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the structural classification of WP:N determined by the guideline text itself, or by which reading (deletionist, inclusionist, deliberative) dominates editorial practice?',
    'Corpus analysis of AfD arguments to measure which reading''s premises are actually invoked; track whether the same structural facts produce different classifications under different editorial majorities.',
    'If classification is reading-dependent rather than text-dependent, the constraint is inherently contested and must be modeled as a kernel with multiple stable readings rather than a single rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether WP:N''s structural type is reading-relative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ng_del_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(ng_del_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.06).
narrative_ontology:measurement(ng_del_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(ng_del_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.08).
narrative_ontology:measurement(ng_del_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.09).
narrative_ontology:measurement(ng_del_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ng_del_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(ng_del_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(ng_del_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(ng_del_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.1).
narrative_ontology:measurement(ng_del_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(ng_del_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(notability_guidelines__deletionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, deliberative_reading).

% DUAL FORMULATION NOTE:
% The notability_guidelines kernel decomposes into three structurally distinct readings. The deletionist reading (this file) claims low extraction and genuine coordination. The inclusionist reading claims asymmetric extraction on marginalized knowledge holders. The deliberative reading claims the constraint is primarily procedural. They share the same kernel text but instantiate different constraints with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
