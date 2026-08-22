% ============================================================================
% CONSTRAINT STORY: permissive_license_text__commons_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_permissive_license_text__commons_coordination_reading, []).

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
 *   constraint_id: permissive_license_text__commons_coordination_reading
 *   human_readable: Permissive License Text (MIT/BSD/Apache-style) as Commons Coordination
 *   domain: software_licensing/intellectual_property/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the commons-coordination reading of the
 *   permissive-license-text kernel: the claim that relaxing copyright
 *   restrictions to a bare attribution requirement (MIT/BSD/Apache-style
 *   terms) maximizes universal implementation freedom by minimizing legal
 *   friction. Under this reading, the arrangement is read as a low-overhead
 *   coordination mechanism enabling frictionless global software reuse,
 *   voluntarily entered into by authors who retain full exit, with a
 *   universal beneficiary pool and no identified victim set. This is
 *   deliberately narrow: it does not describe the corporate-moat reading
 *   (uncompensated extraction into proprietary products) or the
 *   copyleft-counterfactual reading (the claim that non-reciprocal terms
 *   structurally enable exploitation) — those are separate constraints,
 *   sharing the same underlying license-text kernel but authoring different
 *   ε, different beneficiary/victim structures, and different
 *   classifications. See kernel_context and the reading_relations block.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(permissive_license_text__commons_coordination_reading, 0.08).
domain_priors:suppression_score(permissive_license_text__commons_coordination_reading, 0.03).
domain_priors:theater_ratio(permissive_license_text__commons_coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, suppression_requirement, 0.03).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(permissive_license_text__commons_coordination_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(permissive_license_text__commons_coordination_reading, rope).
narrative_ontology:human_readable(permissive_license_text__commons_coordination_reading, "Permissive License Text (MIT/BSD/Apache-style) as Commons Coordination").
narrative_ontology:topic_domain(permissive_license_text__commons_coordination_reading, "software_licensing/intellectual_property/technology_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(permissive_license_text__commons_coordination_reading, '082590b3-a518-46b9-aa04-5638f4b5cb38').
narrative_ontology:cs_kernel_codification('082590b3-a518-46b9-aa04-5638f4b5cb38', formalized).
narrative_ontology:cs_authority_grounding('082590b3-a518-46b9-aa04-5638f4b5cb38', practice).
narrative_ontology:cs_interpretation_layer_present('082590b3-a518-46b9-aa04-5638f4b5cb38').
narrative_ontology:cs_reading_relation('082590b3-a518-46b9-aa04-5638f4b5cb38', permissive_license_text__corporate_moat_reading, coexists_with).
narrative_ontology:cs_reading_relation('082590b3-a518-46b9-aa04-5638f4b5cb38', permissive_license_text__copyleft_counterfactual_reading, coexists_with).
narrative_ontology:cs_axiom('082590b3-a518-46b9-aa04-5638f4b5cb38', foundational, minimal_restriction_maximizes_aggregate_adoption).
narrative_ontology:cs_axiom_status(minimal_restriction_maximizes_aggregate_adoption, holdable).
narrative_ontology:cs_axiom_grounding('082590b3-a518-46b9-aa04-5638f4b5cb38', minimal_restriction_maximizes_aggregate_adoption, empirically_contingent).
narrative_ontology:cs_axiom('082590b3-a518-46b9-aa04-5638f4b5cb38', foundational, voluntary_relicensing_exit_negates_extraction_claim).
narrative_ontology:cs_axiom_status(voluntary_relicensing_exit_negates_extraction_claim, holdable).
narrative_ontology:cs_axiom_grounding('082590b3-a518-46b9-aa04-5638f4b5cb38', voluntary_relicensing_exit_negates_extraction_claim, deontological).
narrative_ontology:cs_reference_frame('082590b3-a518-46b9-aa04-5638f4b5cb38', voluntary_minimal_restriction_norm).
narrative_ontology:cs_drift_state('082590b3-a518-46b9-aa04-5638f4b5cb38', contemporary_large_scale_corporate_adoption_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('082590b3-a518-46b9-aa04-5638f4b5cb38', '').
narrative_ontology:cs_kernel_id(permissive_license_text__commons_coordination_reading, permissive_license_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, universal_implementer_pool).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, original_authors).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, downstream_commercial_adopters).
narrative_ontology:constraint_beneficiary(permissive_license_text__commons_coordination_reading, academic_and_hobbyist_developers).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, minimal_friction_maximizes_adoption).
narrative_ontology:constraint_vindicates(permissive_license_text__commons_coordination_reading, attribution_alone_suffices_for_coordination).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write the initial code and choose the permissive license text voluntarily, requiring only attribution and disclaiming warranty. They retain the option to dual-license, fork their own work commercially, or walk away at any time; no one compels them to release under these terms.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, original_authors, agenda_setter,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(permissive_license_text__commons_coordination_reading, original_authors, beneficiary).

% Anyone anywhere — students, startups, hobbyists, large firms, governments — can take the code, modify it, embed it in anything, and redistribute it with no royalty and no reciprocal disclosure obligation. Their only obligation is preserving the attribution notice. They can walk into or out of using the code at will.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, universal_implementer_pool, beneficiary,
    powerless, biographical, mobile, global).

% Companies incorporate the permissively licensed code into proprietary products without needing to open-source their own additions. They benefit from zero licensing negotiation cost and zero legal review overhead beyond confirming attribution compliance.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, downstream_commercial_adopters, beneficiary,
    organized, biographical, mobile, global).

% Use the code for research, teaching, and personal projects without needing to understand complex reciprocal-licensing obligations. The near-zero legal friction lets them build and share without legal counsel.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, academic_and_hobbyist_developers, beneficiary,
    powerless, biographical, mobile, global).

% The broader software commons observes cumulative effects: interoperability standards, shared infrastructure libraries, and rapid diffusion of technique, all attributable in part to low-friction reuse terms across the industry.
narrative_ontology:constraint_stakeholder(permissive_license_text__commons_coordination_reading, software_ecosystem_at_large, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(permissive_license_text__commons_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(permissive_license_text__commons_coordination_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Permissive licensing solves the coordination problem of enabling maximal reuse and recombination of code across organizational and jurisdictional boundaries without requiring case-by-case negotiation, legal review, or reciprocal obligations — the friction that would otherwise prevent widespread adoption is minimized to near zero.
% TRANSFER_FUNCTION: The license transfers essentially nothing extractive: the original author gives up exclusive commercial control voluntarily and receives attribution and (often) increased adoption, visibility, and ecosystem contribution in return. What moves is code and permission, not rent.
% ABSENT_VOICES: No party is structurally excluded from participating — this is the reading's central claim. Anyone who could object (e.g., an author who feels under-compensated, or a copyleft advocate who thinks reciprocity should be mandatory) has full exit: they can choose a different license for their own future work.
% DISAPPEARANCE_RATIONALE: If permissive license text vanished overnight, the enormous body of freely reusable code (web servers, cryptographic libraries, language runtimes, build tooling) would revert to either unlicensed status (legally unusable) or require case-by-case negotiated licensing — collapsing the low-friction reuse that underlies most modern software infrastructure. The arrangement is doing real coordination work; its disappearance is not neutral.
% FOUNDING_PROBLEM: Early software distribution required either restrictive proprietary licensing (blocking reuse) or ambiguous/unlicensed sharing (legally risky for both distributor and reuser). Permissive licenses were built to solve the specific problem of enabling legal, frictionless reuse without requiring reciprocal contribution back.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set by legal scholars studying open-source license adoption (e.g., empirical license-choice studies showing permissive licenses dominate in contexts prioritizing adoption breadth over reciprocity), and by copyleft advocates themselves, who do not dispute that permissive licenses solve a real coordination problem — they dispute only whether solving it without reciprocity is normatively sufficient (that dispute is the sibling reading, not this one).
narrative_ontology:disappearance_verdict(permissive_license_text__commons_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(permissive_license_text__commons_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(permissive_license_text__commons_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(permissive_license_text__commons_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(permissive_license_text__commons_coordination_reading, 0.08, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(permissive_license_text__commons_coordination_reading_tests).
:- end_tests(permissive_license_text__commons_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored near-zero (0.08) because, under this reading's own lights, no party is coerced into using or releasing under the license, no rent is extracted from any structurally trapped actor, and the author's foregone exclusivity is a voluntary trade for adoption and ecosystem benefit. Suppression is near-zero because there is no enforcement machinery beyond a copyright-notice preservation norm with no meaningful penalty regime. Accessibility collapse is low (0.1): alternative licensing models (proprietary, copyleft, dual-licensing) remain fully available to any future author — this reading does not claim permissive licensing forecloses other models, only that it coordinates well where chosen. Resistance is low because the coordination reading meets essentially no organized opposition on its own terms — the opposition exists at the level of the sibling readings, not this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Original authors are agenda-setters who voluntarily choose the license and remain mobile (they can dual-license or relicense future versions). The universal implementer pool, downstream commercial adopters, and academic/hobbyist developers are all beneficiaries with mobile exit — none are locked into using any particular piece of permissively licensed code. There is no victim class authored in this reading: the structural claim is precisely that the low-friction terms produce a positive-sum coordination outcome for all named parties. This is what distinguishes the reading from its siblings, which authors a victim class (uncompensated original authors, in the corporate-moat reading) that this reading does not recognize as structurally present.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (enabling frictionless, legally safe reuse) remains live by this reading's own account and by outside corroboration from license-adoption scholarship — this is not a case of an arrangement persisting past its function. The classification as rope (not tangled_rope or snare) prevents mislabeling a genuinely voluntary, exit-rich coordination mechanism as extraction merely because some adopters profit more visibly than others; profit asymmetry alone, absent coercion or lock-in, does not constitute the extraction this framework tracks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_selection_for_permissive_license_text,
    'Is the commons_coordination_reading the empirically dominant lived experience of permissive licensing, or is it the reading favored by the parties best positioned to benefit from minimal restriction (large adopters, well-resourced firms), with the corporate_moat_reading and copyleft_counterfactual_reading better describing the experience of volunteer maintainers and small original authors?',
    'Survey data from open-source maintainers on perceived fairness/burden distribution under permissive vs. copyleft licenses; comparison of corporate contribution-back rates under each license family; analysis of maintainer burnout and uncompensated-labor complaints specifically tied to permissively-licensed high-adoption projects (e.g., widely-used but under-resourced libraries).',
    'If maintainer-reported experience skews heavily toward the corporate_moat_reading''s extraction narrative, that does not change THIS story''s ε (which is scoped to this reading''s own lights) but would suggest the corpus should weight the moat reading as more descriptively representative of the kernel''s real-world operation, informing how the family of readings is presented in aggregate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_for_permissive_license_text, conceptual, 'Which reading of the permissive-license kernel best describes lived practice is contested and not resolvable from license text alone.').

omega_variable(
    attribution_only_sufficiency,
    'Does bare attribution constitute adequate reciprocity for the coordination benefit received, or is attribution-only a structurally thin obligation that permits uncompensated extraction to occur without technically violating the license?',
    'Compare economic value captured by downstream commercial adopters against value returned (in contributions, funding, or attribution-driven goodwill) to original maintainers across a sample of high-adoption permissively-licensed projects.',
    'If value capture is systematically asymmetric at scale, this supports the sibling corporate_moat_reading''s higher ε without altering this reading''s own low-ε account, which is scoped to the voluntary-exchange framing this reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_only_sufficiency, empirical, 'Whether attribution is a meaningful reciprocity mechanism or a legal formality that permits asymmetric value capture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(permissive_license_text__commons_coordination_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(perm_tr_t0, permissive_license_text__commons_coordination_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(perm_tr_t5, permissive_license_text__commons_coordination_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(perm_tr_t10, permissive_license_text__commons_coordination_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(perm_tr_t15, permissive_license_text__commons_coordination_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(perm_tr_t20, permissive_license_text__commons_coordination_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(perm_tr_t25, permissive_license_text__commons_coordination_reading, theater_ratio, 25, 0.05).

% Extraction over time
narrative_ontology:measurement(perm_be_t0, permissive_license_text__commons_coordination_reading, base_extractiveness, 0, 0.06).
narrative_ontology:measurement(perm_be_t5, permissive_license_text__commons_coordination_reading, base_extractiveness, 5, 0.07).
narrative_ontology:measurement(perm_be_t10, permissive_license_text__commons_coordination_reading, base_extractiveness, 10, 0.07).
narrative_ontology:measurement(perm_be_t15, permissive_license_text__commons_coordination_reading, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(perm_be_t20, permissive_license_text__commons_coordination_reading, base_extractiveness, 20, 0.08).
narrative_ontology:measurement(perm_be_t25, permissive_license_text__commons_coordination_reading, base_extractiveness, 25, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(permissive_license_text__commons_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(permissive_license_text__commons_coordination_reading, information_standard).
narrative_ontology:boltzmann_floor_override(permissive_license_text__commons_coordination_reading, 0.02).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__corporate_moat_reading).
narrative_ontology:affects_constraint(permissive_license_text__commons_coordination_reading, permissive_license_text__copyleft_counterfactual_reading).

% DUAL FORMULATION NOTE:
% Three sibling constraints share the permissive_license_text kernel: commons_coordination_reading (this story, low ε, rope, universal beneficiary pool, no victims), corporate_moat_reading (high ε, tangled_rope or snare, victim set of uncompensated maintainers/authors), and copyleft_counterfactual_reading (moderate ε, framing reciprocity-free terms as structurally exploitative relative to a GPL counterfactual). Each authors its own ε and classification per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
