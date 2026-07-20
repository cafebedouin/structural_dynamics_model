% ============================================================================
% CONSTRAINT STORY: divine_marriage_command__continuationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_marriage_command__continuationist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: divine_marriage_command__continuationist_reading
 *   human_readable: Continuationist Reading of Divine Marriage Command
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   The divine marriage command kernel is contested across three readings.
 *   This story instantiates the continuationist reading: the original command
 *   authorizing plural marriage remains doctrinally valid, and the Manifesto
 *   that suspended it was a prudential accommodation under federal duress
 *   rather than a doctrinal rescission or supersession. Fundamentalist
 *   splinters claim continuity with original revelation, practitioners bear
 *   the legal and social costs of the suspension, and the mainstream
 *   institutional authority enforces the Manifesto as binding policy. The
 *   constraint is authored as a tangled rope because it carries a genuine
 *   coordination function (preserving communal identity and continuity under
 *   threat) while asymmetrically extracting from practitioners who are
 *   trapped between theological command and legal prohibition.
 *
 * KEY AGENTS:
 *   - mainstream_church_authority: agenda_setter (institutional/constrained) â enforces the Manifesto and suppresses continuationist practice
 *   - fundamentalist_splinters: beneficiary (organized/identity_locked) â collects legitimacy and communal authority from continuity claims
 *   - plural_marriage_practitioners: payer (powerless/trapped) â bears legal jeopardy and social cost of doctrinal suspension
 *   - theological_scholars: observer (analytical/analytical) â external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, 0.68).
domain_priors:suppression_score(divine_marriage_command__continuationist_reading, 0.7).
domain_priors:theater_ratio(divine_marriage_command__continuationist_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(divine_marriage_command__continuationist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_marriage_command__continuationist_reading, tangled_rope).
narrative_ontology:human_readable(divine_marriage_command__continuationist_reading, "Continuationist Reading of Divine Marriage Command").
narrative_ontology:topic_domain(divine_marriage_command__continuationist_reading, "religious/political_theology").

domain_priors:requires_active_enforcement(divine_marriage_command__continuationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_marriage_command__continuationist_reading, 'f1028e04-5252-4dc1-a8ea-5facd2bb3019').
narrative_ontology:cs_kernel_codification('f1028e04-5252-4dc1-a8ea-5facd2bb3019', fixed_text).
narrative_ontology:cs_authority_grounding('f1028e04-5252-4dc1-a8ea-5facd2bb3019', lineage).
narrative_ontology:cs_interpretation_layer_present('f1028e04-5252-4dc1-a8ea-5facd2bb3019').
narrative_ontology:cs_reading_relation('f1028e04-5252-4dc1-a8ea-5facd2bb3019', divine_marriage_command__substitutionist_reading, forecloses).
narrative_ontology:cs_reading_relation('f1028e04-5252-4dc1-a8ea-5facd2bb3019', divine_marriage_command__coercion_visibility_reading, coexists_with).
narrative_ontology:cs_axiom('f1028e04-5252-4dc1-a8ea-5facd2bb3019', foundational, divine_polygamy_command_perpetually_valid).
narrative_ontology:cs_axiom_status(divine_polygamy_command_perpetually_valid, holdable).
narrative_ontology:cs_axiom_grounding('f1028e04-5252-4dc1-a8ea-5facd2bb3019', divine_polygamy_command_perpetually_valid, theological).
narrative_ontology:cs_axiom('f1028e04-5252-4dc1-a8ea-5facd2bb3019', foundational, manifesto_prudential_not_doctrinal).
narrative_ontology:cs_axiom_status(manifesto_prudential_not_doctrinal, holdable).
narrative_ontology:cs_axiom_grounding('f1028e04-5252-4dc1-a8ea-5facd2bb3019', manifesto_prudential_not_doctrinal, theological).
narrative_ontology:cs_reference_frame('f1028e04-5252-4dc1-a8ea-5facd2bb3019', original_revelation_authority).
narrative_ontology:cs_drift_state('f1028e04-5252-4dc1-a8ea-5facd2bb3019', contemporary_mainstream_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f1028e04-5252-4dc1-a8ea-5facd2bb3019', '').
narrative_ontology:cs_kernel_id(divine_marriage_command__continuationist_reading, divine_marriage_command).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_marriage_command__continuationist_reading, fundamentalist_splinters).
narrative_ontology:constraint_victim(divine_marriage_command__continuationist_reading, plural_marriage_practitioners).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, original_revelation_continuity).
narrative_ontology:constraint_vindicates(divine_marriage_command__continuationist_reading, manifesto_prudential_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the Manifesto as binding ecclesiastical policy, enforcing monogamy through disciplinary mechanisms. Maintains institutional legitimacy by presenting the Manifesto as either new revelation or necessary survival accommodation, thereby suppressing continuationist practice within the official body.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, mainstream_church_authority, agenda_setter,
    institutional, generational, constrained, national).

% Claim exclusive continuity with the original divine command; organize communities around the doctrinal validity of plural marriage. Derive religious authority, group cohesion, and distinct identity from the continuationist reading, capturing the loyalty and resources of adherents who accept their leadership as the true remnant.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, fundamentalist_splinters, beneficiary,
    organized, biographical, identity_locked, regional).

% Believe plural marriage remains doctrinally commanded but live under active threat of criminal prosecution and excommunication. Bear the direct legal, social, and familial costs of the gap between theological validity and practical prohibition; exit means abandoning religious conviction or community.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, plural_marriage_practitioners, payer,
    powerless, immediate, trapped, local).

% Document and analyze the competing readings of the divine marriage command, the historical context of federal coercion, and the institutional divergence between mainstream and splinter groups. Do not participate in the theological constraint but provide external accounts of its operation.
narrative_ontology:constraint_stakeholder(divine_marriage_command__continuationist_reading, theological_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(divine_marriage_command__continuationist_reading, fundamentalist_splinters).
narrative_ontology:fixing_cost_class(divine_marriage_command__continuationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves communal continuity with founding revelation under conditions of external political threat; coordinates fundamentalist splinter identity and theological resistance to mainstream institutional assimilation.
% TRANSFER_FUNCTION: Moves religious authority and communal legitimacy from the mainstream institutional structure to fundamentalist splinter groups; moves legal jeopardy and social ostracism onto plural marriage practitioners.
% ABSENT_VOICES: Women and children within plural marriage communities who might contest the distribution of costs; secular legal scholars who would frame the Manifesto as purely political accommodation lacking internal theological significance; practitioners who would prefer substitutionist peace but are identity-locked.
% DISAPPEARANCE_RATIONALE: If the continuationist reading vanished, fundamentalist splinters would lose their primary theological distinction and claim to legitimacy; practitioners would lose the doctrinal shield that frames their legal risk as faithful obedience rather than criminal deviance; mainstream authority would face intensified pressure to reconcile with substitutionist or coercion-visibility framings.
% FOUNDING_PROBLEM: How to maintain doctrinal fidelity to an original divine command authorizing plural marriage while preserving institutional survival under federal anti-polygamy enforcement and territorial integration.
% FOUNDING_PROBLEM_CORROBORATION: Fundamentalist historians and dissident theologians attest the tension between fidelity and survival. Mainstream institutional historians attest the problem was resolved by superseding revelation or necessary accommodation. No fully external corroboration exists because the founding problem is internal to the religious tradition and its documentary archive.
narrative_ontology:disappearance_verdict(divine_marriage_command__continuationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(divine_marriage_command__continuationist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_marriage_command__continuationist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(divine_marriage_command__continuationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_marriage_command__continuationist_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_marriage_command__continuationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(divine_marriage_command__continuationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(divine_marriage_command__continuationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial because the continuationist reading fuels splinter authority and social control over practitioners without returning commensurate legal safety or institutional recognition. Suppression (0.70) is high because the constraint depends on federal criminalization and mainstream ecclesiastical discipline to maintain the suspension. Theater ratio (0.52) reflects the increasingly performative nature of continuity claims as splinter groups shrink and harden. Accessibility collapse (0.60) captures the closed epistemic environment within fundamentalist communities where substitutionist or secular alternatives are theologically foreclosed. Resistance (0.55) measures moderate but persistent pushback from practitioners and splinters against mainstream and state enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The mainstream authority experiences the constraint as institutional survival; the splinters experience it as proof of their sole legitimacy; the practitioners experience it as lived legal and familial jeopardy. The engine computes these divergences from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Fundamentalist splinters are declared beneficiaries (low directionality, subsidized by the constraint's provision of distinct identity). Plural marriage practitioners are declared victims (high directionality, extraction amplified by trapped exit and local scope). Mainstream authority sits outside beneficiary/victim declarations and receives canonical fallback directionality for its institutional power atom.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled rope classification prevents mislabeling the genuine coordination (communal continuity under threat) as pure extraction, while the victim declaration and trapped exit capture the asymmetric cost-bearing. If the coordination function were absent and only the splinter leadership profited, the constraint would compute as a snare; if extraction were absent and the doctrine were purely consensual identity maintenance, it would compute as a rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manifesto_prudential_vs_doctrinal,
    'Is the Manifesto properly characterized as a prudential accommodation to duress, or does its institutional enactment give it de facto doctrinal status regardless of original intent?',
    'Comparative theological analysis of the Manifesto''s textual framing, institutional reception history, and subsequent official reinterpretation.',
    'If the Manifesto carries de facto doctrinal status, the continuationist reading is operationally false and the constraint collapses toward substitutionist or coercion-visibility forms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manifesto_prudential_vs_doctrinal, conceptual, 'Ambiguity in the theological status of the Manifesto.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the compliance of plural marriage practitioners driven primarily by structural legal threat, or by internalized theological acceptance of the prudential suspension?',
    'Post-legalization behavioral trajectory: if practice resumes or expands when structural legal barriers are removed, suppression was primarily structural; if practice remains suppressed due to internalized norms, reclassify toward identity-locked extraction.',
    'If internalized, effective suppression exceeds the structural measure and the constraint functions more as a cognitive snare than as a legally enforced rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism.').

omega_variable(
    committer_reading_boundary,
    'Does the continuationist reading represent a genuinely distinct constraint from its sibling coercion-visibility and substitutionist readings, or do they collapse into one under operational analysis?',
    'Corpus-level coupling analysis across the divine_marriage_command kernel family; if metrics converge despite distinct axioms, the decomposition may need revision.',
    'If the readings are operationally indistinguishable, the Îµ-invariance decomposition was overstated and the kernel should be treated as a single constraint with observer-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_reading_boundary, conceptual, 'Whether the kernel readings are structurally distinct or observer-variant.').

omega_variable(
    federal_coercion_causality,
    'Was federal anti-polygamy enforcement the exclusive cause of the Manifesto, or did internal institutional evolution toward monogamy-ready structure play an independent role?',
    'Archival and legislative history analysis of federal pressure timelines versus internal institutional policy drafts prior to the Manifesto.',
    'If internal evolution was independent, the ''duress'' framing loses force and the continuationist reading''s core narrative is weakened; if federal pressure was exclusive, the duress framing is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_coercion_causality, empirical, 'Relative weight of external coercion versus internal evolution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_marriage_command__continuationist_reading, 0, 134).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_marriage_command__continuationist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(divi_tr_t20, divine_marriage_command__continuationist_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(divi_tr_t40, divine_marriage_command__continuationist_reading, theater_ratio, 40, 0.35).
narrative_ontology:measurement(divi_tr_t60, divine_marriage_command__continuationist_reading, theater_ratio, 60, 0.42).
narrative_ontology:measurement(divi_tr_t90, divine_marriage_command__continuationist_reading, theater_ratio, 90, 0.48).
narrative_ontology:measurement(divi_tr_t134, divine_marriage_command__continuationist_reading, theater_ratio, 134, 0.52).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_marriage_command__continuationist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(divi_be_t20, divine_marriage_command__continuationist_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(divi_be_t40, divine_marriage_command__continuationist_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(divi_be_t60, divine_marriage_command__continuationist_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(divi_be_t90, divine_marriage_command__continuationist_reading, base_extractiveness, 90, 0.65).
narrative_ontology:measurement(divi_be_t134, divine_marriage_command__continuationist_reading, base_extractiveness, 134, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_marriage_command__continuationist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_marriage_command__continuationist_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
