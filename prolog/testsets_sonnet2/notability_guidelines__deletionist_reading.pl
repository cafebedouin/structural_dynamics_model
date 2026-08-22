% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   human_readable: Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)
 *   domain: digital commons governance / knowledge infrastructure
 *
 * SUMMARY:
 *   This story instantiates the deletionist reading of the contested WP:N
 *   (notability guidelines) kernel: the guideline as a necessary epistemic
 *   quality filter that keeps the commons usable as a reference work by
 *   excluding subjects lacking independent, verifiable coverage. Under this
 *   reading the exclusion is not extraction — declined submitters lose no
 *   asset they held, and the filter's purpose is coordination among volunteer
 *   editors around a shared evidentiary standard, not rent capture. This is a
 *   distinct constraint from the sibling inclusionist_reading (which treats
 *   the same guideline text as a gatekeeping apparatus with a genuine victim
 *   class of marginalized-knowledge subjects) and the sibling
 *   deliberative_reading (which treats the boundary itself as perpetually
 *   renegotiated at AfD rather than settled). Per the ε-invariance principle,
 *   these are three separate constraints sharing a kernel, not one constraint
 *   measured three ways.
 *
 * KEY AGENTS:
 *   - general_readership: primary beneficiary (organized/mobile) — receives a filtered, higher-trust corpus
 *   - volunteer_editor_corps: agenda-setter (organized/mobile) — enforces the standard at direct time cost to itself
 *   - downstream_knowledge_reusers: institutional beneficiary — inherits pre-filtered content
 *   - promotional_and_vanity_submitters: excluded party under this reading — declined, not victimized, per the reading's own premises
 *   - afd_closing_administrators: interpretive agenda-setters who translate the guideline into case outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.12).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.28).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital commons governance / knowledge infrastructure").

domain_priors:requires_active_enforcement(notability_guidelines__deletionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, '2b639f6c-dbd3-4b03-a457-fc8fc52b7a21').
narrative_ontology:cs_kernel_codification('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', formalized).
narrative_ontology:cs_authority_grounding('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', practice).
narrative_ontology:cs_interpretation_layer_present('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21').
narrative_ontology:cs_reading_relation('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', foundational, independent_coverage_as_legitimate_neutral_filter).
narrative_ontology:cs_axiom_status(independent_coverage_as_legitimate_neutral_filter, holdable).
narrative_ontology:cs_axiom_grounding('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', independent_coverage_as_legitimate_neutral_filter, instrumental).
narrative_ontology:cs_axiom('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', foundational, declination_is_not_harm_absent_prior_entitlement).
narrative_ontology:cs_axiom_status(declination_is_not_harm_absent_prior_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', declination_is_not_harm_absent_prior_entitlement, deontological).
narrative_ontology:cs_reference_frame('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', verifiability_grounded_encyclopedic_standard).
narrative_ontology:cs_drift_state('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', contemporary_afd_practice, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2b639f6c-dbd3-4b03-a457-fc8fc52b7a21', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, general_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, volunteer_editor_corps).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, downstream_knowledge_reusers).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, encyclopedic_verifiability_standard).
narrative_ontology:constraint_vindicates(notability_guidelines__deletionist_reading, independent_source_requirement_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Consumes articles as a free reference resource and relies on the encyclopedia's credibility for basic fact-checking. Benefits when the notability bar keeps promotional, unverifiable, or trivial entries out of search results and cross-references. Can leave for any other source at zero cost, but the value of the commons depends on it staying trustworthy in aggregate.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, general_readership, beneficiary,
    organized, generational, mobile, global).

% Writes, reviews, and nominates articles for deletion under the notability guideline, citing WP:N and its subject-specific extensions as the operative standard at AfD. Volunteers their own time to apply the filter and bears the maintenance burden of an encyclopedia with no paid quality-control staff; unpaid labor is the actual enforcement mechanism, and burnout is the visible cost of running it at scale.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, volunteer_editor_corps, agenda_setter,
    organized, biographical, mobile, global).

% Search engines, voice assistants, language-model training pipelines, and mirror sites ingest Wikipedia content wholesale. They benefit from a filtered, verifiable corpus rather than an unfiltered wiki flooded with vanity pages and promotional copy, because filtering is done once upstream instead of redundantly by every downstream consumer.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, downstream_knowledge_reusers, beneficiary,
    institutional, generational, mobile, global).

% Attempt to place self-promotional biographies, company pages, or fan-created entries for subjects lacking independent coverage. Under this reading they are not treated as a victim class: they retain the option of building notability through independent secondary coverage elsewhere, self-publishing off-platform, or waiting until genuine notability accrues. Their submissions are declined, not their persons harmed.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, promotional_and_vanity_submitters, excluded,
    powerless, immediate, mobile, global).

% Interpret consensus at Articles for Deletion discussions and apply WP:N's source-independence and significant-coverage tests to close individual cases. Function as the interpretive layer that translates the general guideline into case outcomes; can be appealed to deletion review, and can themselves resign the role at will.
narrative_ontology:constraint_stakeholder(notability_guidelines__deletionist_reading, afd_closing_administrators, agenda_setter,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(notability_guidelines__deletionist_reading, afd_closing_administrators, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(notability_guidelines__deletionist_reading, diffuse).
narrative_ontology:fixing_cost_class(notability_guidelines__deletionist_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sets a common, source-based threshold — significant coverage in independent, reliable sources — so that thousands of uncoordinated volunteer editors can decide, without central management, which subjects merit a standalone article. This solves the problem of an open-editing wiki collapsing under undifferentiated promotional and trivial content.
% TRANSFER_FUNCTION: No sustained transfer between parties: the guideline moves editorial attention away from unverifiable or promotional submissions and toward subjects with an independent evidentiary record. The 'cost' borne by declined submitters is opportunity cost of an editorial decision, not an extracted asset.
% ABSENT_VOICES: Subjects and communities whose notability is genuinely under-documented by mainstream independent sources (due to historical neglect, non-English-language coverage gaps, or niche/local significance) are not separately represented in this reading's account — their situation is addressed by the inclusionist reading as a sibling constraint, not folded into this one.
% DISAPPEARANCE_RATIONALE: If WP:N vanished overnight, the encyclopedia would rapidly accumulate promotional pages, permastub vanity entries, and unverifiable claims at a rate volunteer review capacity could not absorb; search and citation trust in the corpus would degrade, and downstream reusers would need to build their own filtering layer redundantly. The commons' credibility is materially dependent on the standard existing and being enforced.
% FOUNDING_PROBLEM: Early Wikipedia (pre-2005) faced an unbounded influx of self-published biographies, garage-band pages, and business listings that threatened to make the project indistinguishable from an unmoderated web directory, undermining its claim to be a usable reference work.
% FOUNDING_PROBLEM_CORROBORATION: Independent academic studies of Wikipedia content quality (e.g. comparative reliability studies against traditional encyclopedias) and reporting from technology journalists covering AfD backlogs corroborate that promotional and non-notable submission volume remains a continuous, unresolved pressure on volunteer review capacity — this is attested by researchers and journalists outside the editor corps that benefits from the guideline's existence, not solely by editors themselves.
narrative_ontology:disappearance_verdict(notability_guidelines__deletionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(notability_guidelines__deletionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(notability_guidelines__deletionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(notability_guidelines__deletionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(notability_guidelines__deletionist_reading, 0.12, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction is authored low (0.12) because, under this reading's own lights, nothing of value is taken from anyone: submitters who fail WP:N retain their content, their reputations, and every off-platform avenue for establishing notability later. Suppression is moderate (0.28) reflecting the real friction of deletion review and appeal processes, but this is coordination overhead, not coercive extraction. Theater ratio is low and roughly flat (0.10→0.15) — AfD discussions are largely substantive rather than performative, though some drift toward procedural ritual over time as precedent accretes. Accessibility collapse is moderate (0.35): alternatives to inclusion (self-publishing, niche wikis, waiting for coverage) remain genuinely available, which is exactly what distinguishes a rope from a snare here.
 *
 * DIRECTIONALITY LOGIC:
 *   Readership and downstream reusers sit near the full-beneficiary end: they receive a filtered corpus at no cost and can leave for any other reference source at will, which is itself evidence the arrangement doesn't trap them. The volunteer editor corps is symmetric-to-beneficiary: it bears real labor costs enforcing the standard but does so voluntarily and captures the coordination benefit of a functioning, credible project. Promotional/vanity submitters are declared excluded rather than victims under this reading precisely because the reading's premise is that declining unverifiable content is not cost-imposition on a legitimate claimant — this is the structural delta from the inclusionist reading, which would declare the same submitter-analog group (there, reframed as marginalized-subject communities) as victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unmoderated promotional/vanity influx threatening reference credibility) remains live by external corroboration (academic quality studies, journalist coverage of AfD backlogs), so this reading finds no mandatrophy: the guideline's function has not outlived its necessity. This is the load-bearing difference from a piton reading of the same guideline, which would require the founding problem to be dead while the apparatus persists on inertia — that is not the case here.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_exclusion_as_harm_or_declination,
    'Is declining an article for failing WP:N a harm-free editorial declination (this reading''s premise) or does it impose real costs — reputational, informational-access, representational — on excluded subjects and their communities (the inclusionist reading''s premise)?',
    'Comparative study of deletion outcomes by subject demographic (gender, geography, language-coverage availability) cross-referenced against subsequent notability establishment rates — if declined subjects from under-covered demographics achieve notability and re-inclusion at markedly lower rates than others, that is evidence for cost rather than neutral declination.',
    'If the evidence shows systematic, non-random cost concentrated on identifiable groups, this reading''s ''no victim set'' premise weakens and the constraint''s structural profile converges toward the inclusionist reading''s tangled_rope/snare classification rather than remaining rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(notability_exclusion_as_harm_or_declination, empirical, 'Whether declination under WP:N is genuinely harm-free or is asymmetric cost misdescribed as neutral filtering.').

omega_variable(
    kernel_stability_vs_perpetual_renegotiation,
    'Is WP:N a stabilized, settled standard being applied consistently (this reading''s implicit premise) or is the boundary itself in continuous motion through AfD precedent, as the deliberative reading holds?',
    'Longitudinal analysis of AfD outcome consistency for comparable subjects across a multi-year window — high consistency supports a stabilized-kernel reading; drift in outcomes for materially similar cases supports the deliberative reading.',
    'If the boundary is shown to be substantially unstable rather than settled, this reading''s claim to represent ''the'' notability standard (rather than one snapshot of an ongoing negotiation) weakens, and the appropriate unit of analysis shifts toward the deliberative reading''s process-based framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_stability_vs_perpetual_renegotiation, conceptual, 'Whether the deletionist reading''s premise of a settled standard is itself contestable against the deliberative reading''s process framing.').

omega_variable(
    unpaid_enforcement_labor_extraction,
    'Does the reliance on unpaid volunteer labor to enforce WP:N constitute extraction from the editor corps itself, even if extraction from submitters is absent?',
    'Survey data on editor burnout, attrition rates correlated with AfD/deletion workload, and comparison to paid content-moderation labor models elsewhere.',
    'If editor burnout is substantial and structurally necessary to the guideline''s operation, a distinct beneficiary/payer axis (readership benefits, editor corps pays in uncompensated labor) could support a tangled_rope reading even while the submitter-facing side remains rope-like — this would be a separate decomposition question, not a modification of this story''s ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(unpaid_enforcement_labor_extraction, empirical, 'Whether volunteer enforcement labor constitutes a hidden extraction axis distinct from the submitter-facing coordination story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nota_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nota_tr_t4, notability_guidelines__deletionist_reading, theater_ratio, 4, 0.11).
narrative_ontology:measurement(nota_tr_t8, notability_guidelines__deletionist_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(nota_tr_t12, notability_guidelines__deletionist_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(nota_tr_t16, notability_guidelines__deletionist_reading, theater_ratio, 16, 0.14).
narrative_ontology:measurement(nota_tr_t20, notability_guidelines__deletionist_reading, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(nota_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(nota_be_t4, notability_guidelines__deletionist_reading, base_extractiveness, 4, 0.09).
narrative_ontology:measurement(nota_be_t8, notability_guidelines__deletionist_reading, base_extractiveness, 8, 0.1).
narrative_ontology:measurement(nota_be_t12, notability_guidelines__deletionist_reading, base_extractiveness, 12, 0.11).
narrative_ontology:measurement(nota_be_t16, notability_guidelines__deletionist_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(nota_be_t20, notability_guidelines__deletionist_reading, base_extractiveness, 20, 0.12).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(notability_guidelines__deletionist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(notability_guidelines__deletionist_reading, 0.08).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, deliberative_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings sharing the notability_guidelines kernel. deletionist_reading (this file) treats WP:N as coordination Rope with readership as beneficiary and no victim set. inclusionist_reading treats the same guideline text as structural gatekeeping with a victim class of marginalized-knowledge subjects, likely computing as tangled_rope or snare. deliberative_reading treats the guideline as a perpetually renegotiated process rather than a fixed filter, which may compute closer to scaffold or an unsettled rope. All three share the guideline text and enforcement apparatus as their common kernel but author different ε, different beneficiary/victim structures, and different claimed types, per the ε-invariance decomposition principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
