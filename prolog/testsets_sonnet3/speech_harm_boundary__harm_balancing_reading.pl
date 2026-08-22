% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__harm_balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__harm_balancing_reading, []).

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
 *   constraint_id: speech_harm_boundary__harm_balancing_reading
 *   human_readable: Harm-Balancing Reading of the Speech/Harm Boundary
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint instantiates the harm-balancing reading of the
 *   speech/harm boundary kernel: speech is presumptively protected but that
 *   presumption yields once a party demonstrates concrete harm, at which
 *   point a proportionality test weighs the value of the expression against
 *   the harm caused. This reading occupies a doctrinal middle position —
 *   broader unprotected categories than the absolutist reading (hate speech,
 *   group libel, harassment can be restricted on a case-by-case showing) but
 *   without the categorical, dignity-first exclusion of the dignity reading
 *   (personhood-denying speech is not automatically unprotected; it must
 *   clear the harm-and-proportionality gate like any other category). The
 *   moderate ε (0.42) reflects that extraction here is real but bounded by
 *   the case-by-case structure: unlike a categorical ban, restriction
 *   requires an evidentiary showing, which limits (but does not eliminate)
 *   the doctrine's capacity to be weaponized against disfavored speakers.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__harm_balancing_reading, 0.42).
domain_priors:suppression_score(speech_harm_boundary__harm_balancing_reading, 0.48).
domain_priors:theater_ratio(speech_harm_boundary__harm_balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_harm_boundary__harm_balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__harm_balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__harm_balancing_reading, "Harm-Balancing Reading of the Speech/Harm Boundary").
narrative_ontology:topic_domain(speech_harm_boundary__harm_balancing_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__harm_balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__harm_balancing_reading, '70a3f569-3513-4ecd-99be-7eed582c4c02').
narrative_ontology:cs_kernel_codification('70a3f569-3513-4ecd-99be-7eed582c4c02', distributed).
narrative_ontology:cs_authority_grounding('70a3f569-3513-4ecd-99be-7eed582c4c02', practice).
narrative_ontology:cs_interpretation_layer_present('70a3f569-3513-4ecd-99be-7eed582c4c02').
narrative_ontology:cs_reading_relation('70a3f569-3513-4ecd-99be-7eed582c4c02', speech_harm_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('70a3f569-3513-4ecd-99be-7eed582c4c02', speech_harm_boundary__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('70a3f569-3513-4ecd-99be-7eed582c4c02', foundational, harm_demonstration_required_before_restriction).
narrative_ontology:cs_axiom_status(harm_demonstration_required_before_restriction, holdable).
narrative_ontology:cs_axiom_grounding('70a3f569-3513-4ecd-99be-7eed582c4c02', harm_demonstration_required_before_restriction, instrumental).
narrative_ontology:cs_axiom('70a3f569-3513-4ecd-99be-7eed582c4c02', foundational, proportionality_as_correct_adjudicative_method).
narrative_ontology:cs_axiom_status(proportionality_as_correct_adjudicative_method, holdable).
narrative_ontology:cs_axiom_grounding('70a3f569-3513-4ecd-99be-7eed582c4c02', proportionality_as_correct_adjudicative_method, conventional).
narrative_ontology:cs_reference_frame('70a3f569-3513-4ecd-99be-7eed582c4c02', post_war_rights_balancing_jurisprudence).
narrative_ontology:cs_drift_state('70a3f569-3513-4ecd-99be-7eed582c4c02', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('70a3f569-3513-4ecd-99be-7eed582c4c02', '').
narrative_ontology:cs_kernel_id(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, harassment_complainants).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__harm_balancing_reading, public_order_authorities).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, controversial_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, advocacy_organizations).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_harm_boundary__harm_balancing_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, proportionality_doctrine).
narrative_ontology:constraint_vindicates(speech_harm_boundary__harm_balancing_reading, demonstrated_harm_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of groups historically subject to group libel, incitement, and harassment campaigns. Under this reading, speech targeting them for group-based vilification can be restricted once a demonstrated harm threshold is met, giving them a legal avenue against speech that would be fully protected under an absolutist standard. They cannot exit the jurisdiction's speech regime and depend on courts and tribunals to actually apply the balancing test in their favor.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, targeted_minority_groups, beneficiary,
    moderate, generational, constrained, national).

% Individuals subjected to targeted harassment campaigns, often online, who seek relief through harm-based speech restrictions. They benefit directly when a tribunal finds demonstrated harm and orders takedown, injunction, or damages, but bear the evidentiary burden of proving harm, which is often expensive and re-traumatizing.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, harassment_complainants, beneficiary,
    powerless, immediate, trapped, local).

% Courts, human rights tribunals, and legislatures that administer the proportionality test, define what counts as demonstrated harm, and calibrate the balancing weights between speech interests and harm interests. They set and revise the standard over time and are not themselves subject to it.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, public_order_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Individuals expressing unpopular, offensive, or provocative views on group-related topics who find their speech restricted, sanctioned, or chilled once it is characterized as falling within the broadened unprotected categories (hate speech, group libel, harassment). They bear legal costs, reputational costs, and in some cases criminal or civil liability once harm is demonstrated, and cannot easily relocate their audience or platform without losing reach.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, controversial_speakers, payer,
    moderate, biographical, constrained, national).

% Organizations engaged in sharp-edged political or social advocacy — including some minority-rights groups themselves when their rhetoric is characterized as harmful to a different group — that must now weigh legal risk into their messaging strategy. They fund litigation to test the boundaries of the demonstrated-harm standard and absorb compliance costs (legal review, content moderation) that a purely permissive regime would not impose.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, advocacy_organizations, payer,
    organized, generational, constrained, national).

% Comedians, artists, and cultural commentators whose work trades on exaggeration and provocation about group identity find the proportionality standard unpredictable — the same joke may be protected in one tribunal's balancing and sanctioned in another's. They cannot afford to litigate every case and often self-censor rather than risk a harm finding.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, satirists_and_provocateurs, payer,
    powerless, biographical, constrained, national).

% Social media and hosting platforms that must operationalize the harm-balancing standard into content moderation policy, deciding in the first instance whether speech crosses the demonstrated-harm threshold before any tribunal ever sees the case. They bear compliance costs and liability exposure but can also shape the practical scope of the doctrine through their moderation algorithms, and can relocate incorporation or server infrastructure to more permissive jurisdictions.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, platform_intermediaries, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__harm_balancing_reading, platform_intermediaries, payer).

% Civil libertarians who hold that any harm-based override, however narrow in principle, inevitably expands through case-by-case balancing into a general license for authorities to suppress disfavored viewpoints. Their objection — that proportionality tests are indeterminate and self-expanding — is treated by courts operating under this reading as already answered by doctrine, so it rarely gets a hearing on its own terms in ordinary adjudication.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__harm_balancing_reading, free_expression_absolutists, excluded,
    organized, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_harm_boundary__harm_balancing_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_harm_boundary__harm_balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a workable standard for resolving genuine conflicts between free expression and demonstrable harms to third parties (incitement, group libel, targeted harassment) without either banning broad categories of speech outright or leaving demonstrably harmful speech with no remedy at all.
% TRANSFER_FUNCTION: Moves the risk of restriction from historically-targeted groups and harassment victims (who gain a remedy) onto speakers whose expression falls within the broadened unprotected categories once harm is found — legal exposure, chilling effects, and moderation costs shift toward speakers and advocacy organizations engaged in edgy or group-related expression.
% ABSENT_VOICES: Free expression absolutists who view any harm-based carve-out as structurally unstable are procedurally present as litigants but substantively excluded — their foundational objection to balancing itself is not adjudicable within a doctrine that has already committed to balancing as the correct method.
% DISAPPEARANCE_RATIONALE: If the harm-balancing standard vanished, courts would default either to a near-absolute speech protection standard (advantaging controversial speakers and provocateurs) or to categorical dignity-based bans (advantaging targeted groups more strongly but with less case-by-case nuance) — the current population of cases that turn on demonstrated harm and proportional remedy would be decided under a wholly different logic, materially changing outcomes for the people currently relying on this specific standard.
% FOUNDING_PROBLEM: Absolute speech protection left victims of group libel, incitement, and severe harassment with no legal remedy even where harm was extensively documented, while categorical harm-based bans risked suppressing legitimate political and artistic expression; the balancing standard was built to adjudicate cases individually rather than resolve the tension by fiat in either direction.
% FOUNDING_PROBLEM_CORROBORATION: Human rights tribunals and mainstream constitutional scholarship attest the underlying tension (real harm vs. real expressive value) remains live and unresolved by categorical rules. Free expression absolutists, from outside the group of beneficiaries who currently win cases under this standard, attest that the founding problem has been overstated and that the proportionality apparatus has become a vehicle for expanding restriction beyond the narrow harms it was built to address — a corroboration that directly disputes the doctrine's own self-description.
narrative_ontology:disappearance_verdict(speech_harm_boundary__harm_balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__harm_balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__harm_balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_harm_boundary__harm_balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__harm_balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__harm_balancing_reading_tests).
:- end_tests(speech_harm_boundary__harm_balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate and rising (0.28 to 0.42 over the interval) because case-by-case balancing regimes tend to accrete precedent that gradually lowers the practical bar for a harm finding — this is not fabricated for the story but reflects the well-documented judicial-doctrine pattern where standards articulated as narrow exceptions expand through successive applications. Suppression tracks similarly (0.32 to 0.48): enforcement infrastructure (tribunals, complaint mechanisms, platform moderation policies built around the standard) has matured over the interval, which is a suppression-capacity change distinct from the extraction trend and is why suppression_requirement is tracked separately. Theater ratio is comparatively low and only modestly rising (0.15 to 0.28) because the proportionality test does real adjudicative work in most cases — it is not primarily performative — though critics document a growing share of cases where the test's structure obscures rather than resolves the underlying viewpoint-selection.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (targeted minority groups, harassment complainants), the doctrine looks like overdue coordination — a remedy against harms the absolutist regime callously ignored. From the payer seats (controversial speakers, satirists), the same doctrine looks like an ever-expanding license for authorities to punish disfavored viewpoints under cover of harm-talk. The engine computes these as different seat-level classifications from the same structural data; the divergence is real and is exactly what the tangled_rope type is built to represent, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Targeted minority groups and harassment complainants are beneficiaries: the doctrine exists to give them a remedy they lack under an absolutist standard, so they sit toward the beneficiary end of directionality. Controversial speakers, advocacy organizations, and satirists are targets: once harm is demonstrated (or even alleged, given the chilling effect of case-by-case uncertainty), the costs — legal, reputational, financial — land on them. Public order authorities are the agenda-setters who administer and calibrate the standard; they neither pay nor straightforwardly collect but hold the power to shift the balance in either direction over time. Platform intermediaries occupy a dual role: they administer the standard operationally (agenda_setter) but also bear compliance costs and liability risk (payer), and their global mobility gives them meaningfully different exit options than any other seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) reflects that the coordination function is genuine and independently identifiable — a real conflict between expressive value and demonstrated harm exists, and pure speech-absolutism leaves it entirely unaddressed. The extraction is asymmetric (speakers and advocacy groups pay; targeted groups and public order authorities benefit) and requires active enforcement (tribunals, platform policy, and legal sanction), which is exactly the tangled_rope signature — this prevents mislabeling the doctrine as either pure coordination (ignoring the real cost imposed on disfavored speakers) or pure extraction (ignoring the real harms the doctrine was built to remedy).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_test_indeterminacy,
    'Does the proportionality/harm-balancing standard have enough internal discipline to prevent case-by-case expansion into a general license to suppress disfavored speech, or does its indeterminacy make expansion structurally inevitable?',
    'Longitudinal doctrinal analysis tracking whether successive applications of the harm test broaden or narrow the practical scope of restricted speech categories over multiple decades, compared across jurisdictions using this reading.',
    'If expansion is structurally inevitable, this reading converges toward the dignity reading''s outcomes over time despite its formally narrower doctrinal commitments, which would mean the ε trajectory shown here understates the reading''s long-run extraction. If the test holds its bounds, ε plateaus near current levels.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balancing_test_indeterminacy, empirical, 'Whether case-by-case balancing structurally expands or holds its bounds over time.').

omega_variable(
    demonstrated_harm_evidentiary_capture,
    'Who bears the practical burden of demonstrating harm, and does that burden fall more heavily on well-resourced advocacy organizations and platforms than on individual complainants, effectively converting the standard into a resource-access gate?',
    'Empirical study of case outcomes and litigation costs across complainant types (individual harassment victims vs. organized advocacy groups vs. state actors) under this standard.',
    'If resource asymmetry determines who successfully invokes the harm standard, the doctrine''s benefit flows disproportionately to organized/institutional actors rather than to the individually powerless harassment victims it is nominally built to protect, altering the directionality analysis for the beneficiary seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_harm_evidentiary_capture, empirical, 'Whether evidentiary burden under the harm standard functions as a resource-access gate.').

omega_variable(
    kernel_framing_alternative,
    'Is the correct framing for this constraint ''a distinct reading of the speech/harm kernel'' (as authored) or ''a procedural gloss the absolutist and dignity readings both eventually adopt in practice'' — i.e., is proportionality balancing itself a fourth axis rather than a coordinate reading?',
    'Comparative doctrinal history: do jurisdictions nominally committed to the absolutist or dignity reading in fact import proportionality-style balancing at the margins, suggesting balancing is a convergent procedural layer rather than a coordinate substantive reading?',
    'If balancing is convergent procedure rather than a distinct reading, the three-reading kernel structure understates how much the readings actually share in practice, and the ε-invariance decomposition here would need a fourth axis (procedural mechanism) crossed with the three substantive readings rather than treating them as mutually exclusive alternatives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether proportionality balancing is a coordinate reading or a cross-cutting procedural layer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__harm_balancing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_harm_boundary__harm_balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t8, speech_harm_boundary__harm_balancing_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(spee_tr_t16, speech_harm_boundary__harm_balancing_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(spee_tr_t24, speech_harm_boundary__harm_balancing_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(spee_tr_t32, speech_harm_boundary__harm_balancing_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(spee_tr_t40, speech_harm_boundary__harm_balancing_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t8, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(spee_be_t16, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 16, 0.36).
narrative_ontology:measurement(spee_be_t24, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 24, 0.39).
narrative_ontology:measurement(spee_be_t32, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 32, 0.41).
narrative_ontology:measurement(spee_be_t40, speech_harm_boundary__harm_balancing_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(spee_su_t8, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(spee_su_t16, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(spee_su_t24, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 24, 0.44).
narrative_ontology:measurement(spee_su_t32, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 32, 0.46).
narrative_ontology:measurement(spee_su_t40, speech_harm_boundary__harm_balancing_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__harm_balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__harm_balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__harm_balancing_reading, speech_harm_boundary__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the speech_harm_boundary kernel, each authored as its own ε-invariant constraint story per the ε-invariance principle: absolutist_reading (near-absolute protection, extremely high harm-override threshold, minimal ε), harm_balancing_reading (this story — presumptive protection yielding to demonstrated harm via proportionality test, moderate ε), and dignity_reading (protection categorically subordinate to human dignity, personhood-denying speech unprotected by definition, higher ε with different beneficiary/victim structure). The three do not share a single ε; each reading's ε is a distinct structural fact about how that reading's own doctrine operates, not a point on a shared spectrum.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
