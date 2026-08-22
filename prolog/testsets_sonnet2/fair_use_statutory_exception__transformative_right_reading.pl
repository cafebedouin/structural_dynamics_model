% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__transformative_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__transformative_right_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__transformative_right_reading
 *   human_readable: Fair Use as Transformative-Use Right Enabling Cultural Production
 *   domain: intellectual_property_law/legal_interpretation/information_economics
 *
 * SUMMARY:
 *   This story instantiates the transformative_right_reading of the fair use
 *   kernel: the reading under which courts treat the 'purpose and character'
 *   factor — specifically whether a reuse is transformative — as doctrinally
 *   central, and treat market substitution/licensing availability as
 *   informative but not dispositive. Under this reading, fair use functions
 *   as an affirmative enabling right for cultural and technological
 *   production, not merely a narrow carve-out from a property default. This
 *   is deliberately ONE of three constraints sharing the
 *   fair_use_statutory_exception kernel: the narrow_defense_reading treats
 *   fair use as a tightly bounded affirmative defense preserving market value
 *   as the touchstone, and the market_licensing_reading treats any licensable
 *   use as presumptively infringing. Each reading is authored as its own
 *   constraint with its own epsilon; this file does not average across them.
 *
 * KEY AGENTS:
 *   - documentary_filmmakers: transformative user (moderate/constrained) — bears clearance risk absent this reading
 *   - parody_artists: transformative user (powerless/constrained) — depends on non-market-dispositive analysis
 *   - digital_archive_and_search_platforms: institutional transformative user (institutional/mobile) — largest beneficiary of scaled transformative-use rulings
 *   - individual_rightsholders_and_authors: payer (moderate/constrained) — bears uncompensated reuse
 *   - content_licensing_intermediaries: excluded commercial interest (organized/constrained) — structurally disfavored by this reading
 *   - federal_courts: agenda_setter (institutional/analytical) — administers the transformativeness standard case by case
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__transformative_right_reading, 0.22).
domain_priors:suppression_score(fair_use_statutory_exception__transformative_right_reading, 0.35).
domain_priors:theater_ratio(fair_use_statutory_exception__transformative_right_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fair_use_statutory_exception__transformative_right_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__transformative_right_reading, rope).
narrative_ontology:human_readable(fair_use_statutory_exception__transformative_right_reading, "Fair Use as Transformative-Use Right Enabling Cultural Production").
narrative_ontology:topic_domain(fair_use_statutory_exception__transformative_right_reading, "intellectual_property_law/legal_interpretation/information_economics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__transformative_right_reading, '91a44139-a5da-479c-b93a-e9fe24ab920b').
narrative_ontology:cs_kernel_codification('91a44139-a5da-479c-b93a-e9fe24ab920b', fixed_text).
narrative_ontology:cs_authority_grounding('91a44139-a5da-479c-b93a-e9fe24ab920b', practice).
narrative_ontology:cs_interpretation_layer_present('91a44139-a5da-479c-b93a-e9fe24ab920b').
narrative_ontology:cs_reading_relation('91a44139-a5da-479c-b93a-e9fe24ab920b', fair_use_statutory_exception__narrow_defense_reading, coexists_with).
narrative_ontology:cs_reading_relation('91a44139-a5da-479c-b93a-e9fe24ab920b', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('91a44139-a5da-479c-b93a-e9fe24ab920b', foundational, transformativeness_as_central_inquiry).
narrative_ontology:cs_axiom_status(transformativeness_as_central_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('91a44139-a5da-479c-b93a-e9fe24ab920b', transformativeness_as_central_inquiry, conventional).
narrative_ontology:cs_axiom('91a44139-a5da-479c-b93a-e9fe24ab920b', foundational, licensing_market_existence_non_dispositive).
narrative_ontology:cs_axiom_status(licensing_market_existence_non_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('91a44139-a5da-479c-b93a-e9fe24ab920b', licensing_market_existence_non_dispositive, instrumental).
narrative_ontology:cs_reference_frame('91a44139-a5da-479c-b93a-e9fe24ab920b', innovation_facilitation_default).
narrative_ontology:cs_drift_state('91a44139-a5da-479c-b93a-e9fe24ab920b', post_campbell_acuff_rose_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('91a44139-a5da-479c-b93a-e9fe24ab920b', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, parody_artists).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, software_interoperability_developers).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, researchers_and_educators).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, digital_archive_and_search_platforms).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__transformative_right_reading, remix_and_sampling_musicians).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fair_use_statutory_exception__transformative_right_reading, individual_rightsholders_and_authors).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, innovation_facilitation_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__transformative_right_reading, transformative_use_centrality).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uses archival footage, news clips, and copyrighted stills to build historical or critical works. Under this reading, the transformative purpose of commentary or historical documentation is treated as the central question, not whether a license could theoretically have been purchased. Relies on courts reading fair use generously to avoid clearance costs that would otherwise make many documentaries unmakeable.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, documentary_filmmakers, beneficiary,
    moderate, biographical, constrained, national).

% Creates satirical works commenting on or criticizing existing copyrighted material. Depends on courts recognizing that a licensing market for parody rights would never exist because rights holders would never license criticism of themselves — this reading treats that absence of a plausible license market as irrelevant to the fair use analysis, protecting the parody regardless.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, parody_artists, beneficiary,
    powerless, immediate, constrained, national).

% Reverse-engineers interfaces to build compatible or competing software. Benefits from a reading that treats interoperability-enabling copying as transformative even when it competes with the original rightsholder's licensing revenue, because the courts' focus is on functional transformation rather than market substitution.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, software_interoperability_developers, beneficiary,
    moderate, biographical, constrained, global).

% Quotes, excerpts, and reproduces copyrighted material for scholarship, criticism, and classroom teaching. Under this reading, the pedagogical or critical purpose weighs heavily even where a licensing clearinghouse exists, because the doctrine's stated purpose is facilitating knowledge production, not maximizing rightsholder revenue capture.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, researchers_and_educators, beneficiary,
    moderate, generational, constrained, national).

% Indexes, thumbnails, and full-text-searches copyrighted works at massive scale (book scanning, image search, text-and-data mining). This reading is the doctrinal basis on which such platforms have won major rulings — the transformative purpose of enabling search and discovery outweighs the fact that the underlying works are reproduced wholesale and a licensing market for such indexing could in principle be built.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, digital_archive_and_search_platforms, beneficiary,
    institutional, generational, mobile, global).

% Incorporates samples, mashups, and remixes of prior recordings into new works. Benefits when courts treat genuinely transformative recontextualization as fair use even though a sample-clearance market exists and is actively used by well-resourced artists; the smaller or independent artist without clearance budgets depends on this reading to survive litigation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, remix_and_sampling_musicians, beneficiary,
    powerless, biographical, constrained, national).

% Original creators whose works are reused without payment or permission under this reading. They bear the uncompensated use of their material whenever a court finds the reuse sufficiently transformative, even where they would have licensed it. Their objection — that transformation is often just relabeled appropriation — is heard in individual cases but does not control the doctrine's overall shape.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, individual_rightsholders_and_authors, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__transformative_right_reading, individual_rightsholders_and_authors, excluded).

% Stock-footage houses, sample-clearance agencies, and rights-management collectives whose business model depends on licensing markets being treated as dispositive. This reading structurally disfavors their argument that fair use should yield wherever a licensing market for the specific use exists; their commercial interest is directly opposed to the doctrine's operation.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, content_licensing_intermediaries, excluded,
    organized, biographical, constrained, national).

% Apply the four-factor fair use test case by case, weighting the 'purpose and character of the use' factor (transformativeness) heavily and treating market harm as one factor among several rather than a threshold gate. Their discretion to find transformation is what gives this reading its operative force; the doctrine's shape shifts with each new appellate ruling.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__transformative_right_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__transformative_right_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__transformative_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the tension between static copyright protection and dynamic cultural/technological production by giving courts a standard (transformativeness) that lets genuinely new expression, criticism, and innovation proceed without requiring case-by-case legislative amendment or universal licensing infrastructure.
% TRANSFER_FUNCTION: Moves the economic value of certain unlicensed reuses from original rightsholders to downstream transformative users and the platforms/institutions that host their work, on the theory that the reuse creates new value distinct from the original market.
% ABSENT_VOICES: Content licensing intermediaries and many individual rightsholders would argue that 'transformative' has become an elastic label that swallows substitutive harm; they are heard in litigation but do not control the doctrine's default orientation, which this reading treats as correctly favoring reuse.
% DISAPPEARANCE_RATIONALE: If this reading of fair use disappeared and courts reverted to treating any licensable use as infringing regardless of transformative purpose, documentary production, parody, search platforms, and remix culture would face immediate clearance costs many could not bear — entire genres of commentary and archival work would become legally unviable or migrate to jurisdictions with broader exceptions.
% FOUNDING_PROBLEM: Rigid copyright exclusivity, if applied literally, would block criticism, scholarship, parody, and technological innovation that depends on referencing or incorporating existing works — the doctrine was built to prevent copyright from becoming a veto over commentary and cultural development.
% FOUNDING_PROBLEM_CORROBORATION: Courts (Campbell v. Acuff-Rose and its progeny) and law-and-economics scholarship outside the beneficiary set attest the innovation-facilitation problem remains live, citing ongoing disputes over AI training data, search indexing, and documentary fair use as evidence the tension between exclusivity and reuse has not resolved; this corroboration comes from judicial reasoning and academic commentary, not solely from the beneficiaries who rely on the doctrine.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__transformative_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__transformative_right_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__transformative_right_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__transformative_right_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__transformative_right_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__transformative_right_reading_tests).
:- end_tests(fair_use_statutory_exception__transformative_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored low (0.22) because under this reading's own lights, genuinely transformative reuse creates new expressive or functional value rather than substituting for the original — the reading's structural premise is that most protected reuse is non-substitutive. Suppression is moderate (0.35): rightsholders retain infringement suits and can contest transformativeness in litigation, so alternatives are not fully foreclosed, but the doctrinal default favors reuse once transformation is found. Resistance is authored moderately high (0.55) because content industries and licensing intermediaries actively litigate against expansive transformativeness findings — this is a genuinely contested doctrine, not a settled mountain. Accessibility collapse is low (0.3): rightsholders retain substitutive-use claims and market-harm arguments; the four-factor test remains multi-factor rather than a single collapsed rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Transformative users (filmmakers, parodists, platforms, researchers, remix artists) are the structural beneficiaries under this reading — their reuse is treated as presumptively non-extractive when the purpose is judged transformative, so their directionality sits near the beneficiary end. Individual rightsholders and licensing intermediaries bear the uncompensated-use cost and are the reading's payers/excluded voices, pushing their directionality toward the target end. Courts occupy the agenda-setting seat: they administer the standard but do not themselves collect or pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — that literal copyright exclusivity would block commentary, scholarship, and technological development referencing existing works — remains live: AI training, digital archiving, and parody disputes continue to test the doctrine's boundaries. This reading resists mandatrophy by keeping the standard fact-intensive and case-by-case rather than freezing into either an absolute reuse right or an absolute exclusivity default; that flexibility is also why it does not fully foreclose the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transformativeness_standard_indeterminacy,
    'Is ''transformative purpose'' a stable, judicially administrable standard, or does its elasticity allow courts to launder substitutive uses as transformative depending on outcome-driven reasoning?',
    'Longitudinal doctrinal analysis of appellate transformativeness findings against ex ante market-substitution evidence; divergence between predicted and actual market harm across cases would indicate the standard is being applied result-first.',
    'If the standard is unstable, this reading''s low extraction claim for transformative uses may understate actual extraction from rightsholders in cases where transformation is found opportunistically; the reading would then function closer to the market_licensing_reading''s critique in practice despite its different doctrinal premise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformativeness_standard_indeterminacy, conceptual, 'Whether transformativeness is a stable standard or a results-oriented label.').

omega_variable(
    kernel_reading_divergence_locus,
    'Where exactly does this reading''s premise diverge from the market_licensing_reading and narrow_defense_reading, and which structural element carries the disagreement?',
    'Comparative doctrinal mapping of how each reading treats the four statutory factors — specifically whether ''effect on the market'' is weighted as a gate (market_licensing_reading), a narrowing factor (narrow_defense_reading), or one factor among several subordinate to transformativeness (this reading).',
    'The disagreement is located specifically in how much weight market-harm/licensing-availability carries relative to purpose-and-character; a sibling reading adopting this reading''s premise would need to demote market harm from a threshold question to a subordinate factor, which would foreclose the market_licensing_reading''s core structural claim within the same framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence_locus, conceptual, 'Locating the structural site of disagreement among the three kernel readings.').

omega_variable(
    ai_training_data_extension_uncertainty,
    'Does this reading''s low-extraction, transformativeness-centric framework extend cleanly to large-scale AI training on copyrighted corpora, or does the scale and substitutive market effect of AI outputs push such uses toward the high-epsilon substitutive pole this reading already reserves?',
    'Track emerging case law (e.g., generative AI training litigation) for whether courts apply this reading''s transformativeness-centric analysis or shift toward market-effect-dominant reasoning when the derivative market is itself displaced.',
    'If courts extend this reading to AI training uses that substitute for licensing markets rather than creating clearly distinct expressive value, the reading''s own low-epsilon claim would be undermined by its own logic, since the reading explicitly reserves high epsilon for substitutive uses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ai_training_data_extension_uncertainty, empirical, 'Whether this reading''s transformativeness framework holds at AI-training scale.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__transformative_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fair_tr_t8, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 8, 0.11).
narrative_ontology:measurement(fair_tr_t16, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 16, 0.12).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(fair_tr_t32, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 32, 0.14).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__transformative_right_reading, theater_ratio, 40, 0.15).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fair_be_t8, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 8, 0.26).
narrative_ontology:measurement(fair_be_t16, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 24, 0.23).
narrative_ontology:measurement(fair_be_t32, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 32, 0.22).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__transformative_right_reading, base_extractiveness, 40, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(fair_use_statutory_exception__transformative_right_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__transformative_right_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__transformative_right_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__transformative_right_reading, fair_use_statutory_exception__market_licensing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language label 'fair use.' Each reading shares the kernel_id fair_use_statutory_exception but authors a distinct epsilon and beneficiary/victim structure per the ε-invariance principle: transformative_right_reading (this file, low epsilon for transformative uses, high for substitutive), narrow_defense_reading (higher epsilon, market-value-preserving default), market_licensing_reading (epsilon keyed directly to whether a licensing market exists). All three link to each other via affects_constraints rather than being merged into a single averaged story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
