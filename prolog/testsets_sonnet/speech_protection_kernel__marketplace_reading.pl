% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace-of-Ideas Reading of Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates the marketplace-of-ideas reading of the speech
 *   protection kernel: the position, traceable to Holmes's Abrams dissent,
 *   that speech protection is justified by its collective epistemic function
 *   — a competition among ideas that tends toward truth — rather than by
 *   individual autonomy, dignity, or democratic participation values. On this
 *   reading, content-based restriction is rejected primarily because it
 *   distorts the truth-discovery process, not because it violates the
 *   speaker's rights as such. The claimed type (tangled_rope) reflects a
 *   genuine coordination function — avoiding centralized state adjudication
 *   of truth — married to asymmetric extraction: the doctrine's
 *   self-correction premise assumes counter-speech capacity that is not
 *   evenly distributed, so its costs land disproportionately on those with
 *   the least distributional power to answer speech directed against them.
 *
 * KEY AGENTS:
 *   - incumbent_media_platforms: primary beneficiary (institutional/arbitrage) — content-neutral doctrine minimizes their moderation burden
 *   - well_resourced_speakers: primary beneficiary (powerful/arbitrage) — dominate the 'competition' by volume and reach
 *   - targets_of_disinformation_campaigns: primary victim (powerless/trapped) — harm precedes and outpaces any correction
 *   - constitutional_scholars_outside_marketplace_tradition: analytical observer — evaluates the empirical premise against platform-era evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.28).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace-of-Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a').
narrative_ontology:cs_kernel_codification('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', distributed).
narrative_ontology:cs_authority_grounding('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', practice).
narrative_ontology:cs_interpretation_layer_present('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a').
narrative_ontology:cs_reading_relation('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', foundational, protection_justified_by_collective_epistemic_benefit).
narrative_ontology:cs_axiom_status(protection_justified_by_collective_epistemic_benefit, holdable).
narrative_ontology:cs_axiom_grounding('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', protection_justified_by_collective_epistemic_benefit, instrumental).
narrative_ontology:cs_axiom('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', foundational, counter_speech_is_sufficient_remedy_for_false_or_harmful_speech).
narrative_ontology:cs_axiom_status(counter_speech_is_sufficient_remedy_for_false_or_harmful_speech, holdable).
narrative_ontology:cs_axiom_grounding('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', counter_speech_is_sufficient_remedy_for_false_or_harmful_speech, empirically_contingent).
narrative_ontology:cs_reference_frame('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', abrams_dissent_competition_of_ideas).
narrative_ontology:cs_drift_state('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', algorithmic_platform_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('55fd0e10-07bd-44f0-a7d3-9fe30bde4d0a', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, incumbent_media_platforms).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, well_resourced_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, academic_and_legal_institutions_citing_marketplace_doctrine).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, under_resourced_counter_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, minority_viewpoint_holders_lacking_distribution_access).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, truth_discovery_through_open_competition_of_ideas).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, counter_speech_as_sufficient_remedy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the distribution channels through which the 'marketplace' actually functions. Benefit from a doctrine that treats content moderation restraint as constitutionally virtuous, since it minimizes their own liability and editorial burden while preserving maximal traffic and engagement, regardless of whether truth actually prevails downstream.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, incumbent_media_platforms, beneficiary,
    institutional, generational, arbitrage, national).

% Have the money, legal teams, and existing audience reach to dominate any 'competition of ideas.' The marketplace framing legitimizes their outsized volume as simply winning fair competition rather than as a structural distribution advantage.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, well_resourced_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Courts, law reviews, and constitutional scholarship that maintain and elaborate the marketplace metaphor (Holmes's Abrams dissent lineage) as the operative justification for near-total content neutrality doctrine. They administer the doctrine's boundaries case by case and derive intellectual authority from its continued citation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, academic_and_legal_institutions_citing_marketplace_doctrine, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, academic_and_legal_institutions_citing_marketplace_doctrine, beneficiary).

% Individuals or groups falsely accused, defamed, or targeted by coordinated disinformation absorb reputational, physical safety, or economic harm well before any 'more speech' correction could plausibly reach the same audience. Exit is not available — the harm lands before rebuttal circulates, if it circulates at all.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_disinformation_campaigns, payer,
    powerless, biographical, trapped, national).

% Are theoretically free to answer false or harmful speech with more speech, but lack the platform algorithms, advertising budgets, or existing audience of those they are answering. Bear the practical cost of a remedy the doctrine assumes is symmetric but is not.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, under_resourced_counter_speakers, payer,
    moderate, biographical, constrained, national).

% Groups whose views are unpopular or structurally deprioritized by ranking and curation systems experience the marketplace as closed rather than open, since being technically free to speak means nothing without algorithmic or platform reach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, minority_viewpoint_holders_lacking_distribution_access, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, minority_viewpoint_holders_lacking_distribution_access, excluded).

% Could impose content-based remedies (mandated corrections, liability regimes, disclosure rules) but the marketplace doctrine's content-neutrality premise forecloses most such interventions as unconstitutional distortions of the truth-discovery process before they are even proposed.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, regulatory_and_legislative_bodies, excluded,
    institutional, generational, constrained, national).

% Legal historians and empiricists who study whether the marketplace metaphor's predicted self-correction actually occurs, examining platform-era evidence on disinformation persistence, algorithmic amplification, and asymmetric reach between speakers.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, constitutional_scholars_outside_marketplace_tradition, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, incumbent_media_platforms).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, content-neutral rule for adjudicating speech disputes without requiring courts or regulators to adjudicate truth or falsity directly — a genuine coordination benefit in avoiding centralized truth-arbitration.
% TRANSFER_FUNCTION: Moves the burden of correcting false or harmful speech from the state (via restriction) onto whoever is targeted or harmed, who must generate and distribute counter-speech using whatever resources and reach they individually possess.
% ABSENT_VOICES: Targets of disinformation and viewpoint-minority speakers with no algorithmic reach are structurally excluded from the doctrine's design conversation, which was shaped primarily by courts, platforms, and well-resourced institutional speakers who benefit from the content-neutral default.
% DISAPPEARANCE_RATIONALE: Speakers, platforms, and courts dispute whether removing the marketplace justification would meaningfully change outcomes: doctrine defenders argue any move toward content-based remedy risks state overreach and would rearrange enforcement fundamentally; critics argue actual practice already diverges so far from the marketplace's self-correction premise that formally abandoning the justification would change little on the ground, since platform moderation and defamation law already operate on different logics in practice.
% FOUNDING_PROBLEM: Early twentieth-century courts needed a principled basis for tolerating dissenting, unpopular, or seditious speech during periods of state suppression (Abrams, Whitney era) without granting government broad power to police truth and falsehood directly.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians outside the marketplace tradition (e.g., scholars studying platform-era amplification asymmetries) attest that the empirical premise — that open competition among speakers tends toward truth — was never rigorously tested and is increasingly contradicted by evidence on algorithmic distribution; the doctrine's own defenders, largely drawn from the institutions the doctrine legitimizes, are not an independent corroborating source.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate and rising (0.18 to 0.42 over the interval) because the doctrine's cost structure has grown as speech distribution has become algorithmically mediated: the 1919 world of pamphlets and street-corner speech genuinely approximated symmetric counter-speech capacity, while the 2025 world of algorithmically ranked, engagement-optimized platforms does not. Suppression is comparatively low (0.28) because the reading is defined by its rejection of content-based suppression — its extraction operates through omission (failure to remedy) rather than active coercion. Theater ratio rises sharply (0.1 to 0.4) as the doctrine increasingly functions as post-hoc justification for platform inaction rather than as a live description of how truth actually emerges from unmoderated speech competition.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of institutional legal scholarship maintaining the doctrine, this looks like principled epistemic humility — courts should not be truth-arbiters. From the seat of a disinformation target with no algorithmic reach, the identical rule looks like an extraction mechanism: the promise of a remedy (more speech) that is structurally unavailable to them, dressed as a a neutral process.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent platforms and well-resourced speakers sit near the beneficiary end: the doctrine's content-neutrality directly reduces their liability and legitimizes their structural reach advantage as 'winning' rather than as asymmetric distribution. Targets of disinformation and viewpoint-minority speakers sit near the target end: they bear the harm the doctrine defers to counter-speech, without the platform access to generate an effective counter-speech response. Regulatory bodies are excluded rather than positioned as targets — the doctrine forecloses their intervention options before the harm question is even reached.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state suppression of dissenting political speech in an era of print and assembly — is genuinely resolved in the narrow sense that direct state censorship of the kind at issue in Abrams is now rare in most contexts this doctrine governs. But the doctrine's persistence is justified by an empirical premise (self-correcting truth competition) that was formulated for a low-bandwidth, low-asymmetry speech environment and has not been re-examined against a high-bandwidth, high-asymmetry one. Classifying this as tangled_rope rather than snare prevents mislabeling a genuine coordination achievement (avoiding state truth-adjudication) as pure extraction, while still registering that its current operation transfers real costs onto those without distributional power — a distinction a pure mountain or pure snare label would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counter_speech_capacity_symmetry,
    'Does the marketplace reading''s core empirical premise — that counter-speech can effectively correct false or harmful speech — hold under contemporary algorithmic distribution, or was it only ever plausible in a low-bandwidth speech environment?',
    'Empirical study comparing reach and persistence of original false/harmful speech versus subsequent corrective speech across platforms with known ranking algorithms, controlling for speaker resource level.',
    'If capacity is structurally asymmetric at scale, the coordination/extraction balance shifts further toward extraction and the tangled_rope classification''s victim-side weighting strengthens; if approximately symmetric, the doctrine functions closer to a genuine rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_capacity_symmetry, empirical, 'Whether the self-correction premise holds under modern distribution conditions.').

omega_variable(
    marketplace_reading_committer_structure,
    'Which sibling reading of the speech_protection_kernel would a given jurisdiction or court actually apply, and where is the disagreement located structurally?',
    'Comparative doctrinal analysis: track which reading''s premise (epistemic benefit, autonomy, harm threshold, dignity, democratic participation) courts cite as the operative rationale in content-restriction cases across jurisdictions and eras, and note where holdings would flip under a sibling reading''s premise.',
    'The disagreement is located specifically in the justificatory ground for protection, not in the scope of protected categories — a harm_threshold_reading court would reach different outcomes on defamation and targeted harassment cases than a marketplace_reading court applying the same First Amendment text, because the sibling reading does not treat counter-speech as a sufficient remedy for demonstrated harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_reading_committer_structure, conceptual, 'Where the kernel readings structurally diverge and which reading governs which case types.').

omega_variable(
    distribution_advantage_naturalness,
    'Is the reach advantage held by incumbent platforms and well-resourced speakers a natural feature of any speech environment (larger audiences simply exist), or is it a constructed artifact of specific platform design choices (ranking algorithms, advertising economics) that could be regulated without touching the marketplace doctrine''s content-neutrality core?',
    'Comparative analysis of speech environments with different distribution architectures (algorithmic feed vs. chronological feed vs. no feed) to isolate whether reach asymmetry tracks architecture or is invariant across architectures.',
    'If constructed, remedies aimed at distribution architecture (not content) could address the asymmetry without requiring abandonment of the marketplace reading''s content-neutrality premise — narrowing the practical gap between this reading and the harm_threshold_reading''s outcomes without adopting its justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(distribution_advantage_naturalness, conceptual, 'Whether reach asymmetry is a natural or constructed feature of the speech environment this doctrine governs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1919, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t1919, speech_protection_kernel__marketplace_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(spee_tr_t1960, speech_protection_kernel__marketplace_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(spee_tr_t1990, speech_protection_kernel__marketplace_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(spee_tr_t2005, speech_protection_kernel__marketplace_reading, theater_ratio, 2005, 0.28).
narrative_ontology:measurement(spee_tr_t2016, speech_protection_kernel__marketplace_reading, theater_ratio, 2016, 0.36).
narrative_ontology:measurement(spee_tr_t2025, speech_protection_kernel__marketplace_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(spee_be_t1919, speech_protection_kernel__marketplace_reading, base_extractiveness, 1919, 0.18).
narrative_ontology:measurement(spee_be_t1960, speech_protection_kernel__marketplace_reading, base_extractiveness, 1960, 0.22).
narrative_ontology:measurement(spee_be_t1990, speech_protection_kernel__marketplace_reading, base_extractiveness, 1990, 0.28).
narrative_ontology:measurement(spee_be_t2005, speech_protection_kernel__marketplace_reading, base_extractiveness, 2005, 0.33).
narrative_ontology:measurement(spee_be_t2016, speech_protection_kernel__marketplace_reading, base_extractiveness, 2016, 0.38).
narrative_ontology:measurement(spee_be_t2025, speech_protection_kernel__marketplace_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(speech_protection_kernel__marketplace_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This file is one of five sibling readings of the speech_protection_kernel, each a separately authored constraint with its own ε, beneficiary/victim structure, and classification per the ε-invariance principle. The marketplace_reading's distinguishing structural feature is grounding protection in collective epistemic benefit rather than individual right; this produces a different victim set (targets of disinformation and viewpoint-minorities lacking distribution access) than, e.g., the dignity_reading (subordinated target groups generally) or harm_threshold_reading (demonstrable-harm victims specifically). All five stories should be read as a family; contamination or drift in one reading's empirical premises (e.g., collapse of the self-correction assumption) creates downstream pressure on how courts weigh the others, without logically foreclosing them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
