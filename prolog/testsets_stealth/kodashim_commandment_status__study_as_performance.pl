% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Study-as-Performance Substitution for the Sacrificial Commandments
 *   domain: religious/halakhic (commitment-system)
 *
 * SUMMARY:
 *   After the destruction of the Second Temple, rabbinic Judaism confronted
 *   commandments tied to an altar that no longer existed. The tradition's
 *   answer, crystallized in the talmudic equation between study and offering
 *   (Menahot 110a, resting on Hosea 14:3), was that engaging with the laws of
 *   the sacrifices constitutes their performance. This story instantiates the
 *   study_as_performance reading of the kodashim_commandment_status kernel:
 *   study maintains FULL commandment force, the kernel remains occupied
 *   through intellectual engagement, and the performance gap generates zero
 *   extractiveness because the gap is closed rather than exploited. The ε
 *   referent is the standing arrangement under contest — the post-destruction
 *   substitution regime as this reading assesses it (study genuinely
 *   discharges the obligation) — never the restored-Temple alternative this
 *   reading does not endorse. The claim/metric posture is deliberately
 *   independent: the constraint is CLAIMED as rope (steady-state coordination
 *   with net beneficiaries) while the metrics are authored descriptively
 *   (very low extraction, near-zero suppression, low theater); the engine
 *   computes per-seat classifications from the structural data. Per the
 *   ε-invariance principle, the colloquial label 'status of the sacrifice
 *   commandments' decomposes into three structurally distinct claims — this
 *   file, plus the performance_only and messianic_deferral siblings, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - - rabbinic_academy: Agenda-setter (institutional/arbitrage) — administers the curriculum, defines what counts as study, collects the modest surplus of support and standing
 *   - - torah_scholars: Primary beneficiary (organized/identity_locked) — vocation carries full commandment force under the equivalence doctrine
 *   - - post_temple_jewish_community: Beneficiary (moderate/constrained) — discharges unperformable obligations through supported study; exit means assimilation
 *   - - kohanim_priestly_line: Excluded voice (moderate/identity_locked) — hereditary service demoted to subject matter; not consulted when the substitution was framed
 *   - - halakhic_analyst: Analytical observer — sees the full structure including the sibling framings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.06).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.06).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Study-as-Performance Substitution for the Sacrificial Commandments").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious/halakhic (commitment-system)").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '938cc28c-8153-4099-9b26-b9cc1131706c').
narrative_ontology:cs_kernel_codification('938cc28c-8153-4099-9b26-b9cc1131706c', fixed_text).
narrative_ontology:cs_authority_grounding('938cc28c-8153-4099-9b26-b9cc1131706c', lineage).
narrative_ontology:cs_interpretation_layer_present('938cc28c-8153-4099-9b26-b9cc1131706c').
narrative_ontology:cs_reading_relation('938cc28c-8153-4099-9b26-b9cc1131706c', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('938cc28c-8153-4099-9b26-b9cc1131706c', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('938cc28c-8153-4099-9b26-b9cc1131706c', foundational, study_discharges_sacrificial_obligation).
narrative_ontology:cs_axiom_status(study_discharges_sacrificial_obligation, holdable).
narrative_ontology:cs_axiom_grounding('938cc28c-8153-4099-9b26-b9cc1131706c', study_discharges_sacrificial_obligation, conventional).
narrative_ontology:cs_axiom('938cc28c-8153-4099-9b26-b9cc1131706c', secondary, kernel_occupied_without_altar).
narrative_ontology:cs_axiom_status(kernel_occupied_without_altar, holdable).
narrative_ontology:cs_axiom_grounding('938cc28c-8153-4099-9b26-b9cc1131706c', kernel_occupied_without_altar, conventional).
narrative_ontology:cs_reference_frame('938cc28c-8153-4099-9b26-b9cc1131706c', study_constituted_commandment).
narrative_ontology:cs_drift_state('938cc28c-8153-4099-9b26-b9cc1131706c', contemporary_yeshiva_practice, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('938cc28c-8153-4099-9b26-b9cc1131706c', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, torah_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, post_temple_jewish_community).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, rabbinic_academy).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, hosea_lips_offering_prooftext).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the curriculum in which the laws of offerings are taught, rules on which modes of engagement count as fulfillment, and credentials the teachers who transmit the corpus. Collects the communal support, prestige, and continuity of role that flow toward institutions of study. Because it defines what counts as study, its position inside the arrangement is self-positioned; its institutional interest is bound to the arrangement's continuation.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, rabbinic_academy, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__study_as_performance, rabbinic_academy, beneficiary).

% Devote their working lives to mastering the sacrificial corpus. Under the equivalence doctrine this devotion is itself the performance of the commandments, so their vocation carries full religious force rather than marking time against an impossible act. Their standing, livelihood, and self-concept are constituted through the study the arrangement sanctifies; leaving it would mean abandoning the identity their entire role rests on.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, torah_scholars, beneficiary,
    organized, biographical, identity_locked, global).

% Ordinary observant households discharge otherwise-unperformable obligations by participating in and supporting study. They receive covenantal continuity without the Temple and bear only the modest cost of sustaining schools and teachers. Their alternative is assimilation out of the community, which carries heavy social and familial cost, so participation is chosen but not casually reversible.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, post_temple_jewish_community, beneficiary,
    moderate, generational, constrained, global).

% Hereditary descendants of the priesthood whose ancestral function — performing the offerings — survives only as curriculum content plus a set of purity rules kept in anticipation. They retain residual honors (priestly blessing, precedence in public reading) but the arrangement converted their exclusive ritual role into a subject of study without their consent; they were not seated when the substitution was framed and would press for the primacy of actual service.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, kohanim_priestly_line, excluded,
    moderate, generational, identity_locked, global).

% Studies the arrangement comparatively — how textual communities convert unperformable rites into intellectual practice — and sees the full structure at once: the destroyed referent, the substituted practice, and the doctrine that binds them, together with the sibling framings among which the tradition divides.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_commandment_status__study_as_performance, rabbinic_academy).
narrative_ontology:fixing_cost_class(kodashim_commandment_status__study_as_performance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of maintaining a commandment-practice whose object is absent: it converts a Temple-bound ritual obligation into a distributed intellectual practice any member can perform anywhere, keeps the sacrificial corpus central to education, and preserves covenantal continuity across generations without the altar.
% TRANSFER_FUNCTION: Moves attention, teaching labor, and communal support toward the institutions and practitioners of study; returns religious merit and fulfilled-obligation status to the studier and interpretive continuity to the community. The exchange is largely internal — each studier's own effort purchases their own fulfillment.
% ABSENT_VOICES: The priestly line, whose hereditary service the substitution demotes to subject matter, and literalists who hold that only altar-blood discharges the obligation would object; both stood outside the academies that framed the doctrine. Modern secular Jews are absent from the conversation entirely.
% DISAPPEARANCE_RATIONALE: If the equivalence doctrine vanished overnight, the community would again hold commandments it cannot perform — a standing condition of transgression with no remedy — the sacrificial corpus would lose its organizing place in the curriculum, the scholarly class would lose the religious force that sanctifies its vocation, and pressure to restore actual sacrifice would intensify sharply.
% FOUNDING_PROBLEM: After the destruction of the Second Temple (70 CE), the community carried commandments tied to the altar that could no longer be performed, threatening permanent covenantal rupture and the standing condition of unremedied transgression.
% FOUNDING_PROBLEM_CORROBORATION: The rabbinic framing rests on Hosea 14:3 ('we will render for bulls the offering of our lips') — a prophetic text predating and external to the academies that invoked it — as cited in the Talmud (Menahot 110a). Academic historians of Judaism, working outside the benefiting parties, corroborate that the substitution doctrine emerged as a response to the destruction-era crisis rather than as cover for pre-existing interests.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.08, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very low (0.08) because the transfer is internal: the studier's attention purchases their own fulfillment, and the only surplus — institutional support and scholarly standing — is small and rides visibly on top of a coordination function the community values for its own sake. Suppression is near-zero (0.06) and is authored as a raw structural property, unscaled by power or scope: nothing is coerced, no alternative is barred, and a non-participant harms no one but themselves. Theater is low (0.12) within this reading's own lights because the tradition holds the substitution constitutive, not compensatory — the liturgical recitation of offering-orders edges toward the performative, but the doctrine itself is load-bearing. Accessibility_collapse is moderate (0.50): accepting the equation dissolves the unfulfillable-obligation problem for the accepter, yet the sibling readings remain live positions in the population, so alternatives do not vanish. Resistance is low (0.20): the reading achieved early consensus inside rabbinic Judaism; historical objections (Karaite rejection of the oral law wholesale, modern unease with substitution) never mounted sustained resistance to this equation specifically. The measurement series run on one shared time grid (both metrics at every point, 0–1800) and trace a deliberately flat, gently rising trajectory: the arrangement is strikingly stable across nearly two millennia, with only slow accretion of institutional surplus and mild formalization. No suppression_requirement series is authored because the enforcement picture is static — the arrangement persists through curriculum and consent, not through enforcement machinery that builds up or decays.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the scholar's seat the arrangement is pure fulfillment — their life's work IS the commandment, so the structure presents as a rope or better. From the ordinary community member's seat it is light, voluntary-feeling obligation with real continuity payoff. From the priestly line's seat it is a demotion: their exclusive hereditary function was converted into curriculum content by a body they did not sit on, which reads as loss of standing rather than extraction (they pay no levy; they forfeited primacy). From the academy's seat the arrangement is self-justifying infrastructure. The engine computes these divergences from power, exit, and role data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations map onto real structure: torah_scholars, post_temple_jewish_community, and rabbinic_academy all sit near the beneficiary end (low d, damped or inverted effective extraction) because each receives more than it surrenders — merit, continuity, and institutional surplus respectively. No victims are declared because no seat bears asymmetric costs: the studier's effort is the price of their own fulfillment (a symmetric exchange, not a transfer to another party), and the priestly line's grievance is status demotion, not extraction — nothing flows from them to anyone. This is why the expected structural delta holds: zero extractiveness from the performance gap, empty victim set. No directionality overrides are needed; the derivation from beneficiary declarations plus exit options already places every seat correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The misclassification risk runs specifically toward piton: a naive observer sees a ritual whose original function (actual sacrifice) is impossible, maintained for eighteen centuries — vestigial performance, inertial maintenance. The piton test is the cost-asymmetry plus atrophied function, and this arrangement fails both prongs within this reading: the function did not atrophy, it MIGRATED — the founding problem (unperformable obligation producing covenantal rupture) was solved by redefinition at the moment of crisis, not left to decay while the form persisted. Hence founding_problem_status is contested rather than dead-with-persistence, and the theater_ratio stays low. The classification prevents the inverse error as well: labeling this a snare would require identifiable victims and suppressed exits, and the structural data supply neither. The genuine mandatrophy question is displaced onto the sibling readings — if performance_only is right, the arrangement IS husk-maintenance (piton-flavored); if messianic_deferral is right, it is transitional readiness (scaffold-flavored). Those divergences are what the linked sibling stories exist to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_partition,
    'Does the talmudic equation transfer full commandment force (this reading, study_as_performance), mark a suspended-but-live obligation maintained in readiness (messianic_deferral), or leave a husk with no current force (performance_only)? This story instantiates the first branch; the three readings partition the same texts.',
    'Comparative adjudication across the three linked stories: the engine computes each reading''s classification from its own structural data, and convergence or divergence among the computed types locates where the disagreement carries structural weight versus where it is purely interpretive.',
    'If performance_only were adopted, commandment force drops to zero and the arrangement becomes husk-maintenance (piton-flavored); if messianic_deferral were adopted, it becomes transitional readiness (scaffold-flavored, requiring a sunset horizon); under this reading it is steady-state coordination (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_partition, conceptual, 'Which of the three readings the tradition''s texts actually bind a halakhic actor to.').

omega_variable(
    equivalence_force_attenuation,
    'Does study carry full commandment force, or attenuated (''ke''ilu'', as-if) force that falls short of full performance?',
    'Analysis of practical halakhic contexts where the equivalence has operative consequences: whether study of the sacrificial orders satisfies obligations framed in performance-shaped terms, and how decisors weigh study against actual offering in settings where both are possible.',
    'If force is attenuated, a residual gap persists between obligation and discharge — a small standing extraction in the form of unresolved obligation-anxiety — nudging effective extraction upward and the computed classification toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equivalence_force_attenuation, empirical, 'Whether the substitution is constitutive or merely compensatory.').

omega_variable(
    scholarly_rent_question,
    'Does the arrangement confer concentrated benefits on the scholarly class — standing, livelihood, interpretive authority — beyond the coordination value delivered to participants at large?',
    'Comparative status-flow analysis: whether communities that professionalize study less heavily (or reject the oral-law framework entirely) achieve comparable covenantal continuity at lower concentrated benefit to a scholarly elite.',
    'If concentrated rents are substantial, the arrangement shades toward tangled_rope — a coordinated community with an asymmetricly rewarded academy — raising effective extraction for non-scholar participants despite the empty victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholarly_rent_question, empirical, 'Whether the scholarly class captures surplus beyond the coordination cost.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 1800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_perf_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kodashim_study_perf_tr_t300, kodashim_commandment_status__study_as_performance, theater_ratio, 300, 0.11).
narrative_ontology:measurement(kodashim_study_perf_tr_t600, kodashim_commandment_status__study_as_performance, theater_ratio, 600, 0.12).
narrative_ontology:measurement(kodashim_study_perf_tr_t900, kodashim_commandment_status__study_as_performance, theater_ratio, 900, 0.13).
narrative_ontology:measurement(kodashim_study_perf_tr_t1200, kodashim_commandment_status__study_as_performance, theater_ratio, 1200, 0.14).
narrative_ontology:measurement(kodashim_study_perf_tr_t1500, kodashim_commandment_status__study_as_performance, theater_ratio, 1500, 0.16).
narrative_ontology:measurement(kodashim_study_perf_tr_t1800, kodashim_commandment_status__study_as_performance, theater_ratio, 1800, 0.18).

% Extraction over time
narrative_ontology:measurement(kodashim_study_perf_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(kodashim_study_perf_be_t300, kodashim_commandment_status__study_as_performance, base_extractiveness, 300, 0.05).
narrative_ontology:measurement(kodashim_study_perf_be_t600, kodashim_commandment_status__study_as_performance, base_extractiveness, 600, 0.06).
narrative_ontology:measurement(kodashim_study_perf_be_t900, kodashim_commandment_status__study_as_performance, base_extractiveness, 900, 0.06).
narrative_ontology:measurement(kodashim_study_perf_be_t1200, kodashim_commandment_status__study_as_performance, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement(kodashim_study_perf_be_t1500, kodashim_commandment_status__study_as_performance, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement(kodashim_study_perf_be_t1800, kodashim_commandment_status__study_as_performance, base_extractiveness, 1800, 0.09).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_commandment_status__study_as_performance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'status of the sacrifice commandments after the destruction' decomposes into three structurally distinct claims with distinct ε profiles, per the ε-invariance principle. performance_only (ε near zero — nothing is owed, nothing extracted; the arrangement is husk-keeping) is upstream in the sense that its contingency thesis is the null against which the other two define themselves. messianic_deferral (transitional readiness; scaffold-like) treats study as preparation, citing the same liturgy of restoration. study_as_performance (this file; rope-like, steady-state) treats study as discharge, and its talmudic equation is the proof-text battleground both siblings engage — deferral accepts the equation while subordinating it to restoration, performance_only rejects its binding force outright. All three files link one another via network.affects_constraints; no single story hedges ε across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
