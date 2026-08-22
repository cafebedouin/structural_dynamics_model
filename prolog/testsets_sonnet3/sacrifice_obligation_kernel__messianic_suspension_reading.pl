% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__messianic_suspension_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__messianic_suspension_reading, []).

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
    narrative_ontology:measurement_basis/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_kernel__messianic_suspension_reading
 *   human_readable: Sacrifice Obligation as Divinely Suspended, Awaiting Messianic Restoration (Study Maintains Readiness)
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This story instantiates the messianic-suspension reading of the sacrifice
 *   obligation kernel: the position, dominant across normative rabbinic
 *   tradition since the Talmudic period, that the mitzvah of sacrificial
 *   offering has not been abrogated, transformed, or fulfilled by substitute
 *   — it has been divinely suspended by circumstance (absence of the Temple
 *   and its apparatus) pending messianic restoration. Under this reading,
 *   study of the sacrificial tractates is not itself the mitzvah's
 *   fulfillment (contra the study_as_exercise_reading) and is not merely
 *   commemorative or identity-preserving (contra the
 *   symbolic_archive_reading); it is instrumental readiness-maintenance,
 *   keeping the operational knowledge intact for a restoration event that may
 *   occur at any time. Nor does this reading share the
 *   performance_only_reading's implication that non-performance during the
 *   suspension constitutes an unaddressed halakhic deficit — under
 *   suspension, non-performance carries no violation weight at all. ε is low
 *   because there is no present extraction: no one is coerced to perform an
 *   impossible act, no one is penalized for the impossibility, and the study
 *   obligation that does exist is genuinely low-cost and non-coercive
 *   (structured curricular time, not confiscated resources or bodily risk).
 *
 * KEY AGENTS:
 *   - contemporary_observant_jews: bear the (light) obligation to study, benefit from moral clarity that non-performance is not sin
 *   - future_restored_temple_generation: the reading's named ultimate beneficiary of preserved operational knowledge
 *   - torah_study_institutions: administer and benefit from the curricular centrality this reading assigns to the sacrificial tractates
 *   - halakhic_decisors: set and interpret the boundaries of when/whether suspension might lift
 *   - messianic_restorationist_movements: excluded voice arguing for more active preparatory acts beyond study
 *   - comparative_religion_scholars: analytical observer of the doctrine's structure across history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12).
domain_priors:suppression_score(sacrifice_obligation_kernel__messianic_suspension_reading, 0.2).
domain_priors:theater_ratio(sacrifice_obligation_kernel__messianic_suspension_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__messianic_suspension_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__messianic_suspension_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__messianic_suspension_reading, "Sacrifice Obligation as Divinely Suspended, Awaiting Messianic Restoration (Study Maintains Readiness)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__messianic_suspension_reading, "religious_law/halakhic_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__messianic_suspension_reading, '937e2942-8a36-4a12-90b4-4eba577877ae').
narrative_ontology:cs_kernel_codification('937e2942-8a36-4a12-90b4-4eba577877ae', fixed_text).
narrative_ontology:cs_authority_grounding('937e2942-8a36-4a12-90b4-4eba577877ae', lineage).
narrative_ontology:cs_interpretation_layer_present('937e2942-8a36-4a12-90b4-4eba577877ae').
narrative_ontology:cs_reading_relation('937e2942-8a36-4a12-90b4-4eba577877ae', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('937e2942-8a36-4a12-90b4-4eba577877ae', sacrifice_obligation_kernel__study_as_exercise_reading, influences).
narrative_ontology:cs_reading_relation('937e2942-8a36-4a12-90b4-4eba577877ae', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('937e2942-8a36-4a12-90b4-4eba577877ae', foundational, obligation_suspended_not_abrogated).
narrative_ontology:cs_axiom_status(obligation_suspended_not_abrogated, holdable).
narrative_ontology:cs_axiom_grounding('937e2942-8a36-4a12-90b4-4eba577877ae', obligation_suspended_not_abrogated, deontological).
narrative_ontology:cs_axiom('937e2942-8a36-4a12-90b4-4eba577877ae', foundational, study_is_instrumental_readiness_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_instrumental_readiness_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('937e2942-8a36-4a12-90b4-4eba577877ae', study_is_instrumental_readiness_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('937e2942-8a36-4a12-90b4-4eba577877ae', temple_era_operative_sacrificial_law).
narrative_ontology:cs_drift_state('937e2942-8a36-4a12-90b4-4eba577877ae', post_talmudic_diaspora_consolidation, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('937e2942-8a36-4a12-90b4-4eba577877ae', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__messianic_suspension_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, future_restored_temple_generation).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, torah_study_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_jews).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, divine_suspension_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__messianic_suspension_reading, operational_readiness_through_study).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a halakhic framework in which the sacrificial mitzvot are not currently actionable but remain part of the corpus studied (e.g. via daily/seasonal liturgical recitation and yeshiva curricula covering Zevachim, Menachot, and related tractates). They are not penalized for non-performance because performance is understood as suspended by circumstance (absence of the Temple), not abrogated by will. Their obligation is to maintain fluency, not to build an altar.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, contemporary_observant_jews, beneficiary,
    moderate, biographical, constrained, global).

% A not-yet-existing generation who, under this reading, will inherit a fully operational sacrificial system because study across intervening centuries preserved procedural detail (order of offerings, disqualifying blemishes, priestly conduct) that would otherwise have decayed through disuse. They cannot currently act but are the reading's named intended beneficiary of the maintenance function.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, future_restored_temple_generation, beneficiary,
    powerless, civilizational, analytical, global).

% Yeshivot and batei midrash that structure curricula around the sacrificial tractates under this reading's premise that such study is preparatory and instrumentally necessary, not merely historical. They receive institutional legitimacy and continuous pedagogical purpose from the doctrine that the knowledge must be kept alive against a future need; they also administer how rigorously that study is pursued.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, torah_study_institutions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_kernel__messianic_suspension_reading, torah_study_institutions, agenda_setter).

% Rabbinic authorities who rule on the status of the suspension itself — whether it remains in force, what triggers its lifting, and how contemporary practice (e.g. partial Temple Mount access debates, red heifer breeding projects) interacts with it. They administer the framework's boundaries without personally bearing extraction costs from it.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, halakhic_decisors, agenda_setter,
    institutional, generational, constrained, global).

% Groups (e.g. Temple Institute-adjacent organizations) who argue the suspension may be more actively hastened through preparatory physical acts (vessel reconstruction, priestly genealogical verification) rather than passive study-only readiness. Mainstream halakhic consensus under this reading treats their activism as premature and largely keeps their voice outside normative deliberation, though they are not suppressed by force.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, messianic_restorationist_movements, excluded,
    moderate, generational, constrained, national).

% Academic observers who analyze the suspension doctrine as a case study in how legal-religious systems maintain dormant obligations across long historical gaps without collapsing into either abrogation or fabricated performance. They take no side in the halakhic dispute but document its structure.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__messianic_suspension_reading, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves procedurally detailed, performance-ready knowledge of the sacrificial system across an indefinite historical gap, so that if messianic restoration occurs, the community is not required to reconstruct the law from scratch or perform sacrifice incorrectly.
% TRANSFER_FUNCTION: Transfers scholarly attention and curricular resources from immediately actionable law to dormant law, on the theory that this attention is not wasted but banked forward; no material transfer occurs between living parties since no one currently performs or is compelled to perform the underlying acts.
% ABSENT_VOICES: Messianic restorationist and Temple-activist movements who would argue the suspension should be actively worked toward through physical preparation rather than held in study-only abeyance; they are structurally present in the broader Jewish world but excluded from normative halakhic deliberation on this specific question.
% DISAPPEARANCE_RATIONALE: If the suspension doctrine were dropped tomorrow, contemporary observant communities would either treat the mitzvah as fully abrogated (freeing study time for other pursuits) or, under a stricter reading, treat non-performance as a live violation requiring some substitute atonement mechanism — different sub-communities would land in different places, and the study institutions that currently organize significant curricular time around these tractates would need to justify that time differently.
% FOUNDING_PROBLEM: After the Second Temple's destruction (70 CE), Jewish law faced the problem of a mitzvah category (sacrifice) that had become physically impossible to perform through no fault or choice of the community, and needed a doctrine that neither declared the commandments void nor treated ordinary Jews as perpetual sinners for non-performance.
% FOUNDING_PROBLEM_CORROBORATION: Attested across the Talmudic tradition itself (e.g., Berakhot 26b on prayer corresponding to sacrifice; Menachot 110a on study substituting for offering in terms of merit though not fulfillment) and corroborated externally by historians of religion (e.g., Jacob Neusner's work on rabbinic Judaism's reconstitution after 70 CE) who are not themselves halakhically bound by the doctrine and analyze it as a documented adaptive response to Temple loss, not merely a claim asserted by those who benefit from the study institutions it sustains.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__messianic_suspension_reading, contested).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__messianic_suspension_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__messianic_suspension_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__messianic_suspension_reading, 0.12, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).
:- end_tests(sacrifice_obligation_kernel__messianic_suspension_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits low (0.08 at Talmudic codification, drifting to 0.12 by the present) because the reading's own structure removes the primary extraction vector: there is no victim class being coerced to perform an impossible or costly act, and no one is penalized for non-performance during the suspension window. The very slow upward drift reflects increasing institutional investment (yeshiva time, publishing infrastructure around Zevachim/Menachot study) accumulating over centuries — a mild scaffolding of resource allocation around the study function, not a rise in coercion. Suppression is low-moderate (0.2): the doctrine does not forcibly prevent alternative readings, though it does occupy the overwhelming majority of normative rabbinic real estate, making alternative framings (like the excluded restorationist-activist position) marginal by consensus weight rather than by coercive suppression. Theater ratio stays low and rises only slightly (0.10 to 0.15) — the study function described here is largely genuine intellectual and legal engagement with real analytic content (the tractates are dense, technical, argued rigorously), not predominantly performative; some drift reflects the accretion of ceremonial recitation practices (liturgical mentions of sacrifice) that carry more symbolic than analytic weight over time. Accessibility collapse is moderate (0.35): within Orthodox normative Judaism the suspension reading is close to universal, but the wider Jewish world (Reform, Reconstructionist, secular) simply does not operate within this framework at all, so alternatives are far from collapsed at the pan-Jewish level — only within the specific halakhically-committed population does this reading dominate.
 *
 * PERSPECTIVAL GAP:
 *   From the halakhic decisor seat, this arrangement is a stable, settled resolution to a historical rupture — coordination that has worked for nearly two millennia. From the excluded restorationist seat, the same arrangement looks like institutional complacency dressed as piety: study-only readiness, on their view, under-serves the actual goal of restoration by not pursuing available preparatory acts. The engine should compute the decisor and institution seats as low-extraction coordination (their structural position matches the reading's own account) while the excluded seat's absence from deliberation is recorded via absent_voices rather than folded into the extraction metric — per R3, an excluded voice's objection is commentary-grade, not classification-driving.
 *
 * DIRECTIONALITY LOGIC:
 *   Contemporary observant Jews sit near the beneficiary end: the obligation they actually carry (study) is light, achievable, and explicitly declared non-sinful in its incompleteness — the suspension doctrine subsidizes their moral standing rather than extracting from them. The future restored generation is declared the ultimate beneficiary by the reading's own logic (preserved operational knowledge), though as a non-existent-yet party this is aspirational rather than currently realized. Torah study institutions benefit doubly: they hold agenda-setting power over curricular emphasis and collect the institutional legitimacy the doctrine confers. No victim group is declared, consistent with the expected structural delta for this reading — this is the central structural fact distinguishing it from the performance_only_reading, which would generate a victim class of non-performers-in-deficit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction removing the physical possibility of sacrificial performance) remains live under this reading precisely because the Temple has not been rebuilt — this is not a case of an arrangement outliving its function; the suspension condition that triggered the doctrine is, by the reading's own terms, still obtaining. This distinguishes the messianic_suspension_reading sharply from a piton pattern: there is no atrophied function being maintained by inertia, because the function (preserving operational readiness for a condition that has not yet changed) is still exactly what the arrangement was built to do. Mandatrophy would only bite if the founding problem resolved (Temple rebuilt) and the study-only posture persisted anyway without triggering active performance obligations — that would be a live test of this reading's operational-readiness claim, not an indictment of the current arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suspension_vs_transformation_ambiguity,
    'Is the sacrificial obligation genuinely suspended in the sense of remaining fully binding-but-inoperative, or has centuries of study practice functionally transformed the obligation into something the study_as_exercise_reading would recognize — i.e., has the doctrine drifted in practice even where it has not drifted in formal statement?',
    'Close textual analysis of how contemporary halakhic authorities describe the phenomenology of sacrificial-tractate study (do they speak of it as ''standing in for'' the mitzvah, or purely as ''preparation for'' it) combined with survey data on how observant practitioners themselves understand what they are doing when they study these tractates.',
    'If practice has drifted toward the study_as_exercise_reading''s framing even while the messianic_suspension_reading remains the formally declared doctrine, this constraint''s beneficiary structure (future generation as ultimate beneficiary) may be partly a legitimating fiction over what is functionally already a fulfillment-through-study arrangement — which would raise ε somewhat by introducing a live coordination/extraction tension between formal doctrine and functional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suspension_vs_transformation_ambiguity, conceptual, 'Whether formal suspension doctrine and lived study-practice have diverged.').

omega_variable(
    restoration_timeline_indeterminacy,
    'Because messianic restoration has no fixed or predictable timeline, is the ''future_restored_temple_generation'' beneficiary a coherent structural referent at all, or is it an indefinitely deferred beneficiary that functions rhetorically to justify indefinite present-day resource allocation to study institutions without ever being checked against an actual outcome?',
    'There is no empirical resolution mechanism available in principle, since the resolution event (messianic restoration) is itself a matter of theological expectation rather than a predictable historical occurrence; the closest available proxy is examining whether study institutions treat the readiness function as falsifiable in any sense (e.g., do they update curricular emphasis in response to Temple-adjacent political developments) or treat it as an unfalsifiable perpetual justification.',
    'If the beneficiary is structurally unfalsifiable, the reading''s low-extraction classification rests partly on a beneficiary claim that can never be tested — this does not by itself raise ε (no one is currently harmed), but it weakens the epistemic confidence that the coordination function, rather than institutional self-perpetuation, is what is actually being served.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_timeline_indeterminacy, conceptual, 'Whether the deferred beneficiary is a testable structural fact or an unfalsifiable legitimating device.').

omega_variable(
    coalition_with_restorationist_activism,
    'Should active preparatory measures pursued by excluded restorationist movements (vessel reconstruction, priestly genealogical registries) be understood as compatible extensions of the study-based readiness function this reading describes, or as a rival claim that the study-only posture is itself inadequate to the readiness goal?',
    'Track whether mainstream halakhic authorities who hold this reading formally endorse, tolerate, or actively discourage specific restorationist preparatory projects over time; a shift from discouragement to tolerance would indicate the boundary between this reading and restorationist activism is softening.',
    'If mainstream authorities increasingly tolerate or endorse active preparation, the messianic_suspension_reading''s premise that study alone constitutes sufficient readiness-maintenance would be partially superseded in practice, suggesting a drift toward a more performance-anticipatory posture that this reading''s current low-suppression, low-extraction profile does not yet capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_with_restorationist_activism, empirical, 'Whether the study-only readiness boundary against restorationist activism is stable or eroding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__messianic_suspension_reading, 0, 1955).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t300, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 300, 0.11).
narrative_ontology:measurement_basis(sacr_tr_t300, observed).
narrative_ontology:measurement(sacr_tr_t700, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 700, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t700, observed).
narrative_ontology:measurement(sacr_tr_t1100, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1100, 0.13).
narrative_ontology:measurement_basis(sacr_tr_t1100, observed).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1500, 0.14).
narrative_ontology:measurement_basis(sacr_tr_t1500, observed).
narrative_ontology:measurement(sacr_tr_t1955, sacrifice_obligation_kernel__messianic_suspension_reading, theater_ratio, 1955, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t1955, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t300, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 300, 0.09).
narrative_ontology:measurement_basis(sacr_be_t300, observed).
narrative_ontology:measurement(sacr_be_t700, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 700, 0.1).
narrative_ontology:measurement_basis(sacr_be_t700, observed).
narrative_ontology:measurement(sacr_be_t1100, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1100, 0.1).
narrative_ontology:measurement_basis(sacr_be_t1100, observed).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1500, 0.11).
narrative_ontology:measurement_basis(sacr_be_t1500, observed).
narrative_ontology:measurement(sacr_be_t1955, sacrifice_obligation_kernel__messianic_suspension_reading, base_extractiveness, 1955, 0.12).
narrative_ontology:measurement_basis(sacr_be_t1955, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__messianic_suspension_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__messianic_suspension_reading, symbolic_archive_reading).

% DUAL FORMULATION NOTE:
% Part of the sacrifice_obligation_kernel constraint family (4 readings). messianic_suspension_reading (this story, low ε ~0.12, rope, no victim set) is the dominant normative-rabbinic position. It coexists_with performance_only_reading (a stricter cousin sharing the suspended-not-fulfilled premise but differing on whether non-performance during suspension carries residual deficit weight — no logical contradiction, both readings can be held by different decisors simultaneously). It influences study_as_exercise_reading by establishing the instrumental framing of study that the exercise reading must argue against or reframe as constitutive rather than merely preparatory — downstream pressure without foreclosure, since a decisor could shift from instrumental to constitutive framing without abandoning the suspension premise itself. It forecloses symbolic_archive_reading: this reading's foundational claim that the obligation remains live and binding-in-abeyance is logically incompatible with the archive reading's claim that the material makes no live halakhic claim at all — a single framework cannot hold both 'this is a suspended binding obligation whose readiness must be maintained' and 'this is a cultural-historical archive with no normative force.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
