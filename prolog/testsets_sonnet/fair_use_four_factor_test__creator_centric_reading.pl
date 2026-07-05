% ============================================================================
% CONSTRAINT STORY: fair_use_four_factor_test__creator_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_four_factor_test__creator_centric_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: fair_use_four_factor_test__creator_centric_reading
 *   human_readable: Fair Use Four-Factor Test (Creator-Centric Reading)
 *   domain: legal/intellectual_property/cultural_production
 *
 * SUMMARY:
 *   This story instantiates the creator-centric reading of the fair use
 *   four-factor test: fair use as a narrow, exceptional carve-out from an
 *   otherwise robust exclusive property right, with the four statutory
 *   factors (purpose, nature, amount, market effect) weighed to preserve
 *   creator incentives against unauthorized appropriation. Under this
 *   reading, ambiguity in application defaults toward finding infringement
 *   absent a clear, provable exception — the opposite default from the
 *   sibling user-centric reading, and structurally distinct from the
 *   transformative-use reading's weighting of the first factor above the
 *   fourth. This story does not describe the doctrine as a whole; it
 *   describes ONE structurally coherent reading of the kernel, held by a real
 *   and identifiable set of institutional actors (rights holders, licensing
 *   bodies, and courts operating in that interpretive tradition). The rise in
 *   extraction and suppression over the measured interval reflects the
 *   documented drift from a judicially administered safety valve into an
 *   increasingly automated, platform-enforced default in favor of rights
 *   holders, particularly post-DMCA and post-automated-content-ID.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, 0.68).
domain_priors:suppression_score(fair_use_four_factor_test__creator_centric_reading, 0.58).
domain_priors:theater_ratio(fair_use_four_factor_test__creator_centric_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(fair_use_four_factor_test__creator_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_four_factor_test__creator_centric_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_four_factor_test__creator_centric_reading, "Fair Use Four-Factor Test (Creator-Centric Reading)").
narrative_ontology:topic_domain(fair_use_four_factor_test__creator_centric_reading, "legal/intellectual_property/cultural_production").

domain_priors:requires_active_enforcement(fair_use_four_factor_test__creator_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_four_factor_test__creator_centric_reading, 'b0559c9b-6b51-424d-b32e-5390baf5ba66').
narrative_ontology:cs_kernel_codification('b0559c9b-6b51-424d-b32e-5390baf5ba66', fixed_text).
narrative_ontology:cs_authority_grounding('b0559c9b-6b51-424d-b32e-5390baf5ba66', lineage).
narrative_ontology:cs_interpretation_layer_present('b0559c9b-6b51-424d-b32e-5390baf5ba66').
narrative_ontology:cs_reading_relation('b0559c9b-6b51-424d-b32e-5390baf5ba66', fair_use_four_factor_test__user_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('b0559c9b-6b51-424d-b32e-5390baf5ba66', fair_use_four_factor_test__transformative_use_reading, influences).
narrative_ontology:cs_axiom('b0559c9b-6b51-424d-b32e-5390baf5ba66', foundational, fair_use_is_affirmative_defense_not_entitlement).
narrative_ontology:cs_axiom_status(fair_use_is_affirmative_defense_not_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('b0559c9b-6b51-424d-b32e-5390baf5ba66', fair_use_is_affirmative_defense_not_entitlement, conventional).
narrative_ontology:cs_axiom('b0559c9b-6b51-424d-b32e-5390baf5ba66', foundational, market_harm_factor_deserves_primary_weight).
narrative_ontology:cs_axiom_status(market_harm_factor_deserves_primary_weight, holdable).
narrative_ontology:cs_axiom_grounding('b0559c9b-6b51-424d-b32e-5390baf5ba66', market_harm_factor_deserves_primary_weight, instrumental).
narrative_ontology:cs_reference_frame('b0559c9b-6b51-424d-b32e-5390baf5ba66', narrow_exception_to_exclusive_right).
narrative_ontology:cs_drift_state('b0559c9b-6b51-424d-b32e-5390baf5ba66', post_automated_content_id_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b0559c9b-6b51-424d-b32e-5390baf5ba66', '').
narrative_ontology:cs_kernel_id(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, major_content_licensors).
narrative_ontology:constraint_beneficiary(fair_use_four_factor_test__creator_centric_reading, collecting_societies).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).
narrative_ontology:constraint_victim(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold exclusive statutory rights and treat fair use as a narrow affirmative defense that must be proven by the party invoking it, not a right held by users. Litigate aggressively around the fourth factor (market harm), including speculative derivative markets, to keep the exception narrow. Fund lobbying and amicus efforts that shape how courts weigh the four factors, and license aggressively-priced permissions as the default expected transaction.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, copyright_holders, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fair_use_four_factor_test__creator_centric_reading, copyright_holders, agenda_setter).

% Operate licensing regimes for footage, music, and text that profit directly from the presumption that unauthorized use requires payment unless a narrow exception clearly applies. Benefit when courts read the four factors conservatively because it expands the space in which a license fee is the path of least resistance.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, major_content_licensors, beneficiary,
    institutional, generational, arbitrage, global).

% Administer licensing and royalty collection on behalf of rights holders; their institutional relevance depends on fair use remaining exceptional rather than a broad user entitlement, since a broad reading would shrink the pool of transactions requiring their intermediation.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, collecting_societies, beneficiary,
    organized, generational, arbitrage, national).

% Create parody, commentary, remix, or appropriation art that recontextualizes existing works. Under a narrow-exception reading, they bear the burden of proving their use is defensible fact-by-fact under all four factors, cannot rely on transformativeness alone, and face takedown, litigation threat, or licensing costs that a broad reading would not impose. Their practical exit is self-censorship or paying for licenses they may not legally need to.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, transformative_use_creators, payer,
    moderate, biographical, constrained, national).

% Use archival footage, quoted text, and cultural artifacts to comment on or critique the underlying works or the events they depict. A narrow reading of the four factors (especially amount-used and market-effect) forces costly pre-clearance, insurance requirements from distributors, or the removal of material central to the critique, chilling investigative and critical work.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, documentarians_and_critics, payer,
    moderate, biographical, constrained, national).

% Operate on platforms with automated content-matching systems calibrated to the narrow-exception reading; lack resources to litigate a fair use defense even when it would likely succeed, so algorithmic and platform-policy enforcement defaults against them regardless of the doctrine's formal flexibility. Effectively bear the extraction whether or not a court would ultimately vindicate their use.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, independent_remix_artists, payer,
    powerless, biographical, trapped, global).

% The stock of culturally available material that would otherwise accumulate through transformative reuse and quotation. A narrow-exception regime slows the rate at which contested or borderline works are safely built upon, keeping more material functionally locked behind licensing even where the four factors would likely favor free use, because uncertainty itself suppresses reuse.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, public_domain_commons, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(fair_use_four_factor_test__creator_centric_reading, public_domain_commons).

% Apply the four-factor balancing test case by case, setting precedent for how narrowly or broadly fair use is construed. Under the creator-centric tradition, courts are instructed to treat the exception as narrow and to weigh market harm heavily, effectively administering the doctrine's restrictive posture even though the statutory text does not compel that weighting.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, courts_and_appellate_judges, agenda_setter,
    institutional, generational, analytical, national).

% Automated systems implement rights-holder-favoring defaults at scale, operationalizing the narrow reading without judicial process. They are not party to the doctrinal debate but execute its practical consequences; no forum exists for affected users to contest the automated application of the doctrine before harm (a takedown) occurs.
narrative_ontology:constraint_stakeholder(fair_use_four_factor_test__creator_centric_reading, platform_content_moderation_systems, excluded,
    institutional, immediate, analytical, global).
narrative_ontology:stakeholder_non_agent(fair_use_four_factor_test__creator_centric_reading, platform_content_moderation_systems).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared legal standard for distinguishing licensable uses of copyrighted material from uses exempt from licensing, allowing courts to adjudicate disputes without a categorical rule that would either bar all secondary use or void all exclusive rights.
% TRANSFER_FUNCTION: Under this reading, the doctrine channels the benefit of ambiguity toward rights holders: uncertain or borderline uses default to requiring a license or risking litigation, moving licensing revenue and settlement leverage from downstream creators and critics to copyright holders and their intermediaries.
% ABSENT_VOICES: Independent remix artists and ordinary platform users rarely appear before appellate courts that set fair use precedent; their exposure is mediated entirely through platform takedown systems calibrated on a handful of high-profile, well-resourced disputes between institutional parties. Public domain interests have no direct representative in adversarial litigation at all.
% DISAPPEARANCE_RATIONALE: If the narrow-exception reading were displaced by a broad user-right reading, licensing markets built on defensive over-licensing would shrink, collecting societies would lose intermediation volume, and a substantial amount of currently self-censored or pre-cleared transformative work would proceed without payment — a measurable reallocation of both revenue and cultural output.
% FOUNDING_PROBLEM: Copyright grants exclusive rights that, applied literally, would forbid even minimal, socially valuable secondary uses (quotation for criticism, parody, scholarship); fair use was built as a judicially administered safety valve preventing copyright from becoming an absolute veto over commentary and reuse.
% FOUNDING_PROBLEM_CORROBORATION: Copyright holders and licensing bodies attest the narrow reading remains necessary to protect incentive structures for creative investment. Independent legal scholars, library associations, and documentary filmmaker guilds — outside the beneficiary set — attest that the narrow reading has drifted from the safety-valve function into a default presumption of infringement that chills exactly the commentary and transformative work fair use was designed to protect.
narrative_ontology:disappearance_verdict(fair_use_four_factor_test__creator_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_four_factor_test__creator_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_four_factor_test__creator_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_four_factor_test__creator_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_four_factor_test__creator_centric_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_four_factor_test__creator_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_four_factor_test__creator_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_four_factor_test__creator_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderately-high (0.68) because the narrow-exception framing concentrates licensing leverage with rights holders and licensing intermediaries even in cases where the four factors, evenly weighed, would likely favor the secondary user — the doctrine's practical operation extracts settlement and licensing revenue from uncertainty itself. Suppression (0.58) reflects both the litigation-cost barrier facing under-resourced users and the automated enforcement layer that operationalizes the narrow reading without adjudication. Theater ratio (0.40) captures a real coordination function (courts genuinely balance four factors) increasingly overtaken by performative compliance — platform notice-and-takedown regimes that gesture at fair use analysis without conducting it.
 *
 * PERSPECTIVAL GAP:
 *   From the rights-holder seat, the narrow reading is principled protection of creative incentives against free-riding. From the transformative-use creator or documentarian seat, the identical four-factor test operates as a cost and chilling-effect mechanism regardless of eventual legal outcome, because the uncertainty itself is the extraction. The engine computes this divergence from the declared power/exit asymmetry; the creator-centric claimed_type (tangled_rope) is authored independently of this observation.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders, major licensors, and collecting societies sit near the full-beneficiary end: they set the interpretive default, collect licensing revenue from the resulting uncertainty, and have arbitrage-grade exit (they can litigate, license, or walk away from any single dispute). Transformative-use creators, documentarians, and remix artists sit near the full-target end: constrained or trapped exit, bearing costs (legal fees, self-censorship, foregone work) regardless of ultimate legal merit. Courts are agenda-setters administering the doctrine's weighting but are analytically positioned rather than extracting directly. The public domain commons is a non-agent payer — it bears the diffuse, civilizational-scale cost of foreclosed reuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing copyright from becoming an absolute veto over commentary and reuse) remains structurally live — copyright's exclusive-rights architecture has not changed. What has drifted is the operational reading of the safety valve: from case-by-case judicial balancing toward a default presumption of infringement enforced increasingly by automated systems that never perform the four-factor analysis at all. This is not classic mandatrophy (the founding problem is not dead), but a founding-problem/operational-drift mismatch — the tool built to solve a narrow problem has been widened, in practice, into a general licensing-revenue-preserving default. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (courts do sometimes vindicate fair use; the doctrine is not a pure extraction vehicle) while registering the asymmetric extraction that the creator-centric weighting imposes on resource-poor secondary users.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indeterminacy,
    'Does the statutory four-factor text itself compel the creator-centric narrow-exception default, or is that default a judicially and institutionally constructed gloss layered onto text that is genuinely open between the three readings?',
    'Doctrinal history analysis comparing pre- and post-1990s appellate treatment of the fourth factor''s weight, cross-referenced against legislative history of the 1976 Act''s fair use codification, would show whether narrow-reading dominance is textually compelled or a contingent interpretive drift.',
    'If textually compelled, the creator-centric reading has a stronger claim to being the ''default'' kernel state rather than one reading among three; if contingent, all three readings sit on genuinely equal doctrinal footing and the narrow reading''s dominance is itself an artifact of who litigates most successfully.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the narrow-exception default is textually required or an interpretive construction layered onto open statutory language.').

omega_variable(
    automated_enforcement_versus_judicial_reading,
    'Is the measured rise in extraction and suppression driven by courts actually adopting a more restrictive four-factor balance, or by platform content-moderation systems implementing a crude proxy for the narrow reading that no court has actually endorsed?',
    'Compare published appellate fair use outcomes over the measured interval against platform takedown/reinstatement rates for content later found (on appeal or dispute) to be fair use.',
    'If the drift is platform-driven rather than judicial, the true extraction is occurring at a layer excluded from this story''s stakeholder set as an agent (platform_content_moderation_systems is authored non-agent) and the doctrinal reading itself may be more stable than the operational data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automated_enforcement_versus_judicial_reading, empirical, 'Whether measured drift reflects judicial doctrine change or automated enforcement divergence from doctrine.').

omega_variable(
    beneficiary_capture_of_incentive_rationale,
    'Is ''preserving creator incentives'' a genuine empirical justification for the narrow reading, or a rhetorical framing that persists because it benefits the institutional parties (major licensors, collecting societies) who did not originate the works but administer the rights?',
    'Empirical study of whether narrow-fair-use jurisdictions show measurably higher original creative output than broader-reading jurisdictions, controlling for market size and enforcement capacity.',
    'If no measurable incentive effect exists, the creator-centric reading''s stated rationale is decoupled from its actual function, which would be almost purely redistributive toward licensing intermediaries rather than toward original creators.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_capture_of_incentive_rationale, empirical, 'Whether the incentive-preservation rationale is empirically supported or serves as cover for intermediary rent extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_four_factor_test__creator_centric_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(fair_tr_t8, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 8, 0.27).
narrative_ontology:measurement(fair_tr_t16, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 16, 0.31).
narrative_ontology:measurement(fair_tr_t24, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(fair_tr_t32, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement(fair_tr_t40, fair_use_four_factor_test__creator_centric_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fair_be_t8, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 8, 0.53).
narrative_ontology:measurement(fair_be_t16, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(fair_be_t24, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(fair_be_t32, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(fair_be_t40, fair_use_four_factor_test__creator_centric_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fair_su_t8, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(fair_su_t16, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(fair_su_t24, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 24, 0.53).
narrative_ontology:measurement(fair_su_t32, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 32, 0.56).
narrative_ontology:measurement(fair_su_t40, fair_use_four_factor_test__creator_centric_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_four_factor_test__creator_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__transformative_use_reading).
narrative_ontology:affects_constraint(fair_use_four_factor_test__creator_centric_reading, fair_use_four_factor_test__user_centric_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the fair_use_four_factor_test kernel, decomposed per the ε-invariance principle because the natural-language label 'fair use doctrine' conflates structurally distinct claims about how ambiguity in the four-factor balance should default. The creator_centric_reading (this story) authors high ε concentrated on unauthorized-use defaults favoring rights holders. transformative_use_reading authors a different beneficiary structure (favoring transformative secondary creators when transformativeness is strong, subordinating market-harm concerns). user_centric_reading authors fair use as an affirmative right with rights holders as the constrained party. All three are linked bidirectionally; a court opinion strengthening one reading's precedent structurally weakens the operating space of the other two without formally overruling them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
