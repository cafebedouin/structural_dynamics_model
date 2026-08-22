% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__study_as_performance, []).

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
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: sacrifice_obligation_continuity__study_as_performance
 *   human_readable: Study-as-Performance Fulfillment of the Sacrifice Obligation
 *   domain: religious law/ritual studies/textual tradition
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, the rabbinic tradition faced
 *   commandments whose performance infrastructure was gone. The
 *   study_as_performance reading answers: engagement with the sacrificial
 *   laws — systematic study of the kodashim tractates, daily recitation of
 *   the korbanot passages in the liturgy — IS the fulfillment; the obligation
 *   persists and is continuously discharged through text. This story authors
 *   THAT reading as a clean, epsilon-invariant constraint: the referent of
 *   epsilon is the standing arrangement (the community's study-based
 *   discharge practice) assessed by the reading's own lights, never the
 *   arrangements the sibling readings would institute. Beneficiaries are the
 *   practitioners, liturgy participants, and academies; there is no victim
 *   set, because the burden that once fell on animals, wealth, and the
 *   priesthood is transformed into an accessible practice. The claimed type
 *   (rope) and the metrics are authored independently: the metrics describe
 *   the arrangement's actual operation as this reading sees it. The
 *   colloquial label 'the sacrifice obligation after the destruction'
 *   decomposes into four structurally distinct constraints — this file plus
 *   the three sibling readings — linked via network.affects_constraints per
 *   the epsilon-invariance principle.
 *
 * KEY AGENTS:
 *   - - torah_study_practitioners: Primary beneficiary (moderate/constrained) — their textual engagement constitutes the fulfillment; they receive discharge-standing and communal continuity
 *   - - daily_liturgy_participants: Secondary beneficiary (organized/mobile) — recite the korbanot passages; near-zero-cost fulfillment woven into existing prayer
 *   - - rabbinic_academies: Agenda-setter and beneficiary (institutional/arbitrage) — administer curriculum and transmission; collect prestige, enrollment, and institutional continuity
 *   - - temple_priestly_lineages: Excluded voice (organized/trapped) — hereditary custodians whose altar-centrality the reading displaces; no seat in the reading's frame
 *   - - traditionally_excluded_learners: Excluded voice (powerless/constrained) — those for whom 'accessible study' was historically gated by literacy and gender norms
 *   - - academic_historians_of_liturgy: Analytical observer (analytical/analytical) — document the reading's post-destruction consolidation and its contestation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:suppression_score(sacrifice_obligation_continuity__study_as_performance, 0.15).
domain_priors:theater_ratio(sacrifice_obligation_continuity__study_as_performance, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, extractiveness, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__study_as_performance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__study_as_performance, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__study_as_performance, "Study-as-Performance Fulfillment of the Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__study_as_performance, "religious law/ritual studies/textual tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__study_as_performance, '9667d57b-0f01-4d3e-9995-8bacb66872a5').
narrative_ontology:cs_kernel_codification('9667d57b-0f01-4d3e-9995-8bacb66872a5', fixed_text).
narrative_ontology:cs_authority_grounding('9667d57b-0f01-4d3e-9995-8bacb66872a5', lineage).
narrative_ontology:cs_interpretation_layer_present('9667d57b-0f01-4d3e-9995-8bacb66872a5').
narrative_ontology:cs_reading_relation('9667d57b-0f01-4d3e-9995-8bacb66872a5', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('9667d57b-0f01-4d3e-9995-8bacb66872a5', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('9667d57b-0f01-4d3e-9995-8bacb66872a5', sacrifice_obligation_continuity__archival_preservation, forecloses).
narrative_ontology:cs_axiom('9667d57b-0f01-4d3e-9995-8bacb66872a5', foundational, obligation_persists_absent_temple).
narrative_ontology:cs_axiom_status(obligation_persists_absent_temple, holdable).
narrative_ontology:cs_axiom_grounding('9667d57b-0f01-4d3e-9995-8bacb66872a5', obligation_persists_absent_temple, theological).
narrative_ontology:cs_axiom('9667d57b-0f01-4d3e-9995-8bacb66872a5', foundational, textual_engagement_constitutes_performance).
narrative_ontology:cs_axiom_status(textual_engagement_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('9667d57b-0f01-4d3e-9995-8bacb66872a5', textual_engagement_constitutes_performance, conventional).
narrative_ontology:cs_reference_frame('9667d57b-0f01-4d3e-9995-8bacb66872a5', study_as_continuous_fulfillment).
narrative_ontology:cs_drift_state('9667d57b-0f01-4d3e-9995-8bacb66872a5', contemporary_mass_literacy_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('9667d57b-0f01-4d3e-9995-8bacb66872a5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, torah_study_practitioners).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, daily_liturgy_participants).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, torah_study_substitutes_for_sacrifice).
narrative_ontology:constraint_vindicates(sacrifice_obligation_continuity__study_as_performance, prayer_replaces_temple_service).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage daily or weekly with the sacrificial-law corpus: the kodashim order of the Mishnah and Talmud, the relevant halakhic codes, and their commentaries. Under this reading their engagement is counted as the bringing of the offerings themselves. What flows to them is discharge of the commandment, standing in the learning community, and continuity of a practice they regard as commanded. Leaving the practice carries social disappointment and a revision of self-understanding, but no formal penalty; many cycle in and out over a lifetime.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, torah_study_practitioners, beneficiary,
    moderate, generational, constrained, global).

% Recite the korbanot passages appended to the morning service, often without deep analysis, as part of standard prayer. The reading counts this recitation toward fulfillment. The cost is a few minutes inside a service they attend anyway; the benefit is participation in the commandment at near-zero marginal effort. Skipping the passage draws little notice.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, daily_liturgy_participants, beneficiary,
    organized, biographical, mobile, global).

% Set the curricular weight given to sacrificial-law study, publish the texts, train the teachers, and schedule the learning cycles through which the practice reproduces itself. They collect enrollment, prestige, and institutional continuity from the practice's centrality, and their own authority descends from the same chain of transmission that authored the reading. Redirecting resources away from the corpus is organizationally easy but would undercut the lineage that grounds their standing.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies, beneficiary).

% Hereditary families whose ancestors performed the altar service and who retain purity disciplines and priestly blessings today. The reading moves the fulfillment's center from their service to any study table, leaving their distinctive role in abeyance. They did not sit in the councils that formulated the substitution doctrine, and the position that study commemorates rather than performs — keeping their service central — has no seat in the reading's frame. Exit from the lineage itself is impossible; it is inherited.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, temple_priestly_lineages, excluded,
    organized, generational, trapped, global).

% In most traditional settings until the modern era, advanced text study was gated by literacy and by gender norms; women and the working poor could recite the liturgy but rarely access the sustained learning the reading prizes. For them the promise that fulfillment is open to all held only partially. Their exclusion is documented in the tradition's own sources and addressed unevenly by modern educational expansion.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, traditionally_excluded_learners, excluded,
    powerless, biographical, constrained, global).

% Scholars of rabbinics and liturgy who trace how the substitution doctrine emerged after the destruction, how it consolidated through the Mishnah, the Talmud, and the liturgical compilations, and where it was contested. They take no side in the normative question; their analyses circulate outside the communities whose practice they describe.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__study_as_performance, academic_historians_of_liturgy, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_continuity__study_as_performance, rabbinic_academies).
narrative_ontology:fixing_cost_class(sacrifice_obligation_continuity__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a dispersed community's continuous relationship with a Temple-bound commandment once the Temple is gone: it provides a practice every member can perform (textual engagement), synchronizes communal life around it through liturgy and curriculum, and preserves the technical knowledge of the sacrificial system should performance ever again become possible.
% TRANSFER_FUNCTION: Moves time and attention from individuals into the communal textual corpus; moves scholarly standing to those who master the sacrificial tractates; moves institutional continuity and enrollment to the academies that transmit the corpus. No material wealth changes hands.
% ABSENT_VOICES: Priestly lineages would object that fulfillment properly runs through their service and that study commemorates rather than performs; they stand outside the reading's frame. Those historically gated from text study by literacy and gender norms would object that the open-to-all premise described their communities' ideal, not their experience. Performance-oriented minorities within the tradition recorded their dissent in minority positions; the objections survive in the record but hold no seat in the arrangement.
% DISAPPEARANCE_RATIONALE: If the study-as-fulfillment practice vanished overnight, the morning liturgy would lose its korbanot section, yeshiva curricula would shed the kodashim order, the academies' enrollment and self-understanding would contract, and the community would be thrown back onto the unresolved question the reading had answered — with the sibling readings competing to fill the vacuum.
% FOUNDING_PROBLEM: After the Second Temple's destruction, commandments bound to the altar became unperformable; the community needed an answer for whether and how the sacrificial obligation continued to bind.
% FOUNDING_PROBLEM_CORROBORATION: The condition is attested from outside the benefiting parties: the performance_only and messianic_suspension communities affirm the same underlying problem (they deny only the substitution's sufficiency), the contemporary response literature and Josephus document the post-destruction crisis, and academic historiography of the period corroborates it. No party claims the Temple stands.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__study_as_performance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__study_as_performance, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__study_as_performance_tests).
:- end_tests(sacrifice_obligation_continuity__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15 at interval end): the arrangement's costs are time and curricular opportunity cost, set against fulfillment-standing, identity continuity, and preserved technical knowledge — a net the reading prices well below any extractive threshold, though the late-series uptick tracks growing academy prestige concentration and rote recitation. Suppression is low (0.15): there is no coercive machinery; participation is pulled by norm and liturgical embedding, and exit carries only mild social disappointment. Suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled, by the engine, from directionality and scope. Theater ratio (0.22) is honest about a rising rote-recitation share — passages recited without comprehension — while most engagement remains comprehension-bearing; the temporal series shows the slow drift. Accessibility collapse is low (0.25): the sibling readings and simple non-participation remain live alternatives; understanding this constraint does not close off its rivals. Resistance (0.30) reflects historical contestation — priestly interests, rejection of the oral corpus, modern secular exit — now settled into low-grade pluralism. The rope claim is asserted independently of these metrics; the engine computes each seat's type from the structural data.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the academies' seat the arrangement is vocation and continuity — they administer the corpus their own authority descends from. From the practitioners' seat it is achievable holiness: a commandment kept in exile. From the excluded learners' seat the accessibility premise is a promise that historically outran access. From the priestly lineages' seat the reading displaced their households' ritual centrality without their consent. From the observer seat it is the decisive post-catastrophe adaptation of a legal tradition. Same structure, different seats — the engine computes this divergence from the power, exit, and role data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   All three declared beneficiary groups derive low directionality (subsidy-side): the practice subsidizes fulfillment, standing, and continuity for those inside it. No victims are declared, so no seat derives high directionality — there is no extraction target, which is the structural heart of the no-victim-set delta. The academies derive low d from their beneficiary declaration despite administering the arrangement; their gain-accrual is recorded separately on the receipt surface (gain_flow) rather than by inflating their directionality. The two excluded seats take the canonical fallback mid-range d, which matches their actual position: diffuse status costs and impaired access, but no material flow taken from them. No directionality overrides are needed — the derivation from beneficiary declarations and exit options lands correctly for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — post-destruction unperformability of altar-bound commandments — remains live: no rebuilt Temple exists, and every party including outsiders attests the condition. The arrangement answers it functionally rather than vestigially: theater stays low, the fulfillment function operates daily, and the status-times-verdict combination (live x world_rearranges) produces no zombie flag. The classification prevents two mislabels. First, it blocks a piton reading (consolation-theater maintaining a dead form): engagement measurably sustains knowledge, liturgy, and communal rhythm, so the function is not atrophied. Second, it blocks a snare reading premised on academy prestige-accrual: no victim set exists, extraction sits near the coordination floor for the identity-coordination type, and the gains that do accrue to academies ride a practice participants pursue for their own stated ends.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint instantiates the study_as_performance reading of the sacrifice_obligation_continuity kernel — are its epsilon, beneficiary structure, and type properties of this reading rather than of the underlying obligation?',
    'Comparative read of the four sibling stories (performance_only, messianic_suspension, archival_preservation) against this one: divergent victim sets, epsilon values, and computed types confirm reading-indexed classification.',
    'Under performance_only a victim set appears (a community bearing an unmet obligation) and extractiveness rises sharply; under archival_preservation normative force vanishes and the arrangement dissolves toward cultural-memory functions; under messianic_suspension the arrangement becomes readiness-maintenance with a built-in restoration horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Classification is indexed to the study_as_performance reading, not to the kernel itself.').

omega_variable(
    fulfillment_efficacy_ambiguity,
    'Does textual engagement actually discharge the sacrificial commandment, or does it produce a felt fulfillment while the obligation itself stands unmet?',
    'Not resolvable by evidence internal to the reading — its own axioms settle the question conventionally; a framework-external criterion would be required, which the tradition does not recognize; behaviorally, observe whether communities holding this reading treat further restorative action as obligatory.',
    'If study is commemoration rather than discharge, the community carries an invisible unmet obligation — a latent victim structure that would push this seat''s classification toward the contested, more extractive side of the kernel family.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fulfillment_efficacy_ambiguity, conceptual, 'Whether the substitution genuinely discharges the obligation or merely consoles.').

omega_variable(
    accessibility_stratification,
    'How uniformly accessible is the fulfilling practice across literacy, gender, and time poverty — does the premise that study is accessible hold for the whole community?',
    'Participation and comprehension data across demographic strata; historical literacy records; comparison of liturgical-recitation-only versus sustained text-study fulfillment rates.',
    'If access is substantially stratified, a bearer stratum emerges (people carrying the obligation without the means of fulfillment), raising effective extraction and complicating the no-victim structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(accessibility_stratification, empirical, 'Universality of the accessible-fulfillment premise.').

omega_variable(
    cs_framing_underdetermination,
    'Is the declared commitment-system framing (fixed_text kernel, lineage authority) the only defensible one, or does a distributed framing (competing academies producing rival readings with no final arbiter) fit the same structure equally?',
    'Examine whether any institution actually adjudicates the kernel authoritatively or whether readings proliferate without a final arbiter; recompute the commitment-system pattern under both framings and compare.',
    'Under a distributed framing the authority structure weakens and drift surfaces as open pluralism rather than being absorbed by an interpretive layer, changing the commitment-system pattern classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Alternative framings of the kernel''s codification and authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__study_as_performance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t4, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 4, 0.12).
narrative_ontology:measurement_basis(sacr_tr_t4, observed).
narrative_ontology:measurement(sacr_tr_t8, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 8, 0.15).
narrative_ontology:measurement_basis(sacr_tr_t8, observed).
narrative_ontology:measurement(sacr_tr_t12, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(sacr_tr_t12, observed).
narrative_ontology:measurement(sacr_tr_t16, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(sacr_tr_t16, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_continuity__study_as_performance, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 0, 0.22).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t4, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 4, 0.2).
narrative_ontology:measurement_basis(sacr_be_t4, observed).
narrative_ontology:measurement(sacr_be_t8, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 8, 0.17).
narrative_ontology:measurement_basis(sacr_be_t8, observed).
narrative_ontology:measurement(sacr_be_t12, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 12, 0.15).
narrative_ontology:measurement_basis(sacr_be_t12, observed).
narrative_ontology:measurement(sacr_be_t16, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 16, 0.14).
narrative_ontology:measurement_basis(sacr_be_t16, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_continuity__study_as_performance, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(sacr_be_t20, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__study_as_performance, sacrifice_obligation_continuity__archival_preservation).

% DUAL FORMULATION NOTE:
% The colloquial label 'the sacrifice obligation after the destruction' decomposes into four structurally distinct constraints sharing one scriptural kernel: this study_as_performance reading plus the performance_only, messianic_suspension, and archival_preservation siblings. They differ in the obligation's deontic status, victim structure, and epsilon; the epsilon-invariance principle forbids averaging them into one story. Upstream is the fixed textual corpus whose authority all four inherit; downstream, this reading's discharge doctrine is the principal target the performance_only reading argues against, so this file links all three siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
