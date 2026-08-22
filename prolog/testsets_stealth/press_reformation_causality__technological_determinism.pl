% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__technological_determinism, []).

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
 *   constraint_id: press_reformation_causality__technological_determinism
 *   human_readable: Press-Determined Reformation Thesis (Technological-Determinist Reading)
 *   domain: historiographical/media-studies/history-of-technology
 *
 * SUMMARY:
 *   The constraint under classification is not the printing press but the
 *   deterministic causal thesis about it: the claim, circulating in
 *   textbooks, documentaries, and technology rhetoric, that the press as an
 *   autonomous enabling technology made vernacular scripture and Reformation
 *   success inevitable. This story instantiates ONLY the
 *   technological_determinism reading of the kernel
 *   press_reformation_causality; the strategic_deployment and co_constitution
 *   readings are separate constraint files, not positions inside this one.
 *   The epsilon referent is the standing arrangement under contest — the
 *   dominance of the deterministic arc in historiographical pedagogy and
 *   popular culture — assessed from the authoring seat, never the reading's
 *   endorsed alternative. The claim/metric gap is deliberate: the reading
 *   internally treats the press as a mountain-like physical constraint with
 *   humans as downstream responders, while the authored metrics describe a
 *   contested, actively maintained narrative structure with identifiable
 *   beneficiaries — the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - - popular_history_media: Primary beneficiary and transmission agenda-setter (institutional/arbitrage) — packages and reproduces the causal arc
 *   - - tech_triumphalist_commentators: Secondary beneficiary (powerful/mobile) — harvests the arc as rhetorical precedent for present-day inevitability claims
 *   - - curriculum_authorities: Agenda-setter (institutional/constrained) — fixes the arc in standards, textbooks, and exams
 *   - - contingency_focused_historians: Primary target (organized/constrained) — bears the permanent correction burden
 *   - - survey_students: Target (powerless/trapped) — absorbs the arc as settled fact
 *   - - present_day_tech_policy_analysts: Downstream target (moderate/mobile) — inherits the inevitability template for contemporary technology governance
 *   - - counterfactual_methodologists: Excluded voice (moderate/mobile) — barred from survey venues by the arc's inevitability premise
 *   - - analytical_historiographers: Analytical observer (moderate/analytical) — traces the arc's career through publishing and curricular records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, 0.62).
domain_priors:suppression_score(press_reformation_causality__technological_determinism, 0.48).
domain_priors:theater_ratio(press_reformation_causality__technological_determinism, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(press_reformation_causality__technological_determinism, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__technological_determinism, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__technological_determinism, "Press-Determined Reformation Thesis (Technological-Determinist Reading)").
narrative_ontology:topic_domain(press_reformation_causality__technological_determinism, "historiographical/media-studies/history-of-technology").

domain_priors:requires_active_enforcement(press_reformation_causality__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__technological_determinism, '529dd04b-e2fb-415c-98f1-b954384c4fd9').
narrative_ontology:cs_kernel_codification('529dd04b-e2fb-415c-98f1-b954384c4fd9', distributed).
narrative_ontology:cs_authority_grounding('529dd04b-e2fb-415c-98f1-b954384c4fd9', diffuse_epistemic).
narrative_ontology:cs_reading_relation('529dd04b-e2fb-415c-98f1-b954384c4fd9', press_reformation_causality__strategic_deployment, coexists_with).
narrative_ontology:cs_reading_relation('529dd04b-e2fb-415c-98f1-b954384c4fd9', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('529dd04b-e2fb-415c-98f1-b954384c4fd9', foundational, technology_autonomously_drives_social_outcomes).
narrative_ontology:cs_axiom_status(technology_autonomously_drives_social_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('529dd04b-e2fb-415c-98f1-b954384c4fd9', technology_autonomously_drives_social_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('529dd04b-e2fb-415c-98f1-b954384c4fd9', secondary, vernacular_scripture_spread_was_structurally_unstoppable).
narrative_ontology:cs_axiom_status(vernacular_scripture_spread_was_structurally_unstoppable, holdable).
narrative_ontology:cs_axiom_grounding('529dd04b-e2fb-415c-98f1-b954384c4fd9', vernacular_scripture_spread_was_structurally_unstoppable, empirically_contingent).
narrative_ontology:cs_reference_frame('529dd04b-e2fb-415c-98f1-b954384c4fd9', press_as_exogenous_prime_mover).
narrative_ontology:cs_drift_state('529dd04b-e2fb-415c-98f1-b954384c4fd9', post_revisionist_historiography, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('529dd04b-e2fb-415c-98f1-b954384c4fd9', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__technological_determinism, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, popular_history_media).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, tech_triumphalist_commentators).
narrative_ontology:constraint_beneficiary(press_reformation_causality__technological_determinism, curriculum_authorities).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, contingency_focused_historians).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, survey_students).
narrative_ontology:constraint_victim(press_reformation_causality__technological_determinism, present_day_tech_policy_analysts).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, technological_determinism_doctrine).
narrative_ontology:constraint_vindicates(press_reformation_causality__technological_determinism, linear_progress_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trade publishers, documentary producers, and history-channel programmers package the causal link between the press and the Reformation as a compact, sellable arc: new machine, unstoppable message, transformed Europe. The arc anchors titles, commissions, and anniversary programming. Switching to a contingency-heavy account raises production and marketing costs against a proven formula, so the arc is retained across editions and seasons.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, popular_history_media, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__technological_determinism, popular_history_media, agenda_setter).

% Columnists, keynote speakers, and futurist writers reach back to Gutenberg-to-Luther as precedent whenever arguing that a current technology will inevitably remake institutions. The analogy supplies rhetorical authority at low cost; dropping it costs nothing, since the next historical analogy is always available.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, tech_triumphalist_commentators, beneficiary,
    powerful, immediate, mobile, global).

% National and state curriculum boards and examination bodies fix the survey narrative in standards, approved textbooks, and exam questions. Revising the canon means re-authoring standards, retraining teachers, and defending the change politically, while the existing narrative carries no comparable penalty for its inaccuracies.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, curriculum_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Scholars documenting printers' business calculations, reformers' patronage politics, and city-council decisions publish corrections that rarely penetrate the survey channel. Their monographs are received as refinements while the popular arc stands untouched; engaging the dominant story is unavoidable professional labor, and leaving the field would surrender the expertise needed to correct it.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, contingency_focused_historians, payer,
    organized, generational, constrained, global).

% Students meet the press-made-it-inevitable arc as settled fact in required courses and standardized examinations. They cannot opt out of the curriculum, and the arc shapes their baseline model of how social change happens long before any specialist nuance reaches them.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, survey_students, payer,
    powerless, biographical, trapped, national).

% Analysts and policymakers inherit the inevitability template and apply it to contemporary technologies, treating adaptation as futile and governance as wave-riding rather than choice-making. The template arrives through the same popular channels; adopting finer-grained models is possible but requires deliberate unlearning.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, present_day_tech_policy_analysts, payer,
    moderate, biographical, mobile, global).

% Scholars who run disciplined counterfactual analyses of the Reformation find their questions dismissed as idle speculation in survey venues, because the dominant arc declares the outcome inevitable and therefore beyond counterfactual inquiry. They publish in specialist outlets with limited reach.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, counterfactual_methodologists, excluded,
    moderate, biographical, mobile, global).

% Historians of historiography study how the causal arc rose, spread, and persisted across generations of textbooks and documentaries. They neither collect from the arc nor bear its costs directly; their seat is analytic, tracing the narrative's career through publishing and curricular records.
narrative_ontology:constraint_stakeholder(press_reformation_causality__technological_determinism, analytical_historiographers, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__technological_determinism, popular_history_media).
narrative_ontology:fixing_cost_class(press_reformation_causality__technological_determinism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, compact causal scaffold linking media technology to religious change: a common reference point for teaching, journalism, and cross-disciplinary conversation, which in its nineteenth-century origin replaced providential explanation of the Reformation with material causation.
% TRANSFER_FUNCTION: Moves explanatory authority and narrative convenience from specialist historians (who bear the cost of correction) to popular transmitters and ideological users; moves attention and status toward great-technology frames and away from human agency.
% ABSENT_VOICES: The early modern printers and reformers themselves are spoken for rather than heard: their letters, colophons, and account books show calculation and strategy, but the arc renders their testimony unnecessary. Counterfactual methodologists are excluded from survey venues, and specialist book historians rarely reach the textbook or documentary channel where the arc circulates.
% DISAPPEARANCE_RATIONALE: If the thesis vanished overnight, survey textbooks, documentary scripts, exam standards, and technology-keynote rhetoric would all need reorganization; the pedagogical and commercial economy built on the compact arc would rearrange around contingency-centered or co-constitution accounts, and the inevitability template currently feeding contemporary technology discourse would lose its principal historical anchor.
% FOUNDING_PROBLEM: Nineteenth-century secular historiography needed a materialist replacement for providential explanation of the Reformation: an account that made religious upheaval a consequence of observable causes rather than divine plan. The press thesis supplied it — secular, progressive, and teachable.
% FOUNDING_PROBLEM_CORROBORATION: Professional historians of the book corroborate from outside the benefiting parties: Febvre and Martin's synthesis, Johns's study of print culture, and Pettegree's and Rublack's work on print economies all treat the founding problem (replacing providentialism) as resolved generations ago and the monocausal remainder as superseded. No source outside the benefiting parties attests that the founding problem remains live; the popular transmitters who repeat the arc attest nothing about its genealogy at all.
narrative_ontology:disappearance_verdict(press_reformation_causality__technological_determinism, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__technological_determinism, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causality__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__technological_determinism, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__technological_determinism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__technological_determinism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.62 at interval end) because the arc's persistence taxes accuracy: specialists must repeatedly correct it, students absorb a false model of social change, and policy analysts inherit inevitability intuitions that degrade contemporary judgment. Suppression is moderate (0.48) and structural-institutional rather than coercive: alternatives are published and seminar-teachable but face editorial gatekeeping, curriculum lag, and market preference for the clean arc. Accessibility collapse is low (0.30) — understanding the arc opens rather than closes the question of whether it is true, and rival readings remain fully accessible. Resistance is high (0.70): an entire historiographical tradition (Febvre/Martin, Johns, Pettegree, Rublack) contests the arc, which is precisely why enforcement effort had to grow. Theater ratio (0.55) crosses the Goodhart threshold honestly: a majority of the arc's contemporary circulation is invocational — anniversary documentaries and keynote gestures — rather than deployed causal analysis. The suppression_requirement series is authored deliberately: the story's traced dynamic includes enforcement-capacity hardening, as gatekeeping intensified once specialist resistance matured. All three series run on one shared eight-point grid; every tracked metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   From inside the determinist frame the thesis is not a constraint at all — it is simply what happened, extraction is invisible, and the question of beneficiaries cannot even form. From the historian and student seats the same structure operates as maintained orthodoxy that charges a continuing accuracy tax. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it. The reading's expected structural delta (technology as mountain, humans as responders, beneficiary structure obscured) is exactly what the beneficiary/victim declarations here refuse to grant.
 *
 * DIRECTIONALITY LOGIC:
 *   Popular history media sits nearest the beneficiary pole: the arc is their inventory, and arbitrage-grade exit means they bear almost none of its costs. Tech triumphalist commentators likewise collect rhetorical capital at negligible cost. Curriculum authorities benefit from canon stability and administer the arc's reproduction. Contingency-focused historians sit near the target pole: organized enough to resist, constrained enough that they cannot leave without surrendering the expertise correction requires. Survey students are the fullest targets — powerless, trapped in required curricula. Present-day policy analysts are targets with mobile exit: harmed through trained intuition, but able to unlearn. The obscured-beneficiary structure the reading produces is thus reversed by declaration: the arc has concentrated beneficiaries and diffuse, real victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a materialist replacement for providential explanation — died generations ago in professional practice, yet the arrangement persists and even intensifies in popular channels; the dead-status-plus-world-rearranges mismatch is the honest signature here and should fire the zombie-persistence flag for cross-checking against the theater trajectory. Classification prevents mislabeling in both directions: calling the arc a rope ignores the asymmetric, actively enforced transfer of explanatory cost onto non-consenting seats; calling it a snare ignores the residual genuine coordination function (a first-pass narrative connecting media change to religious change remains pedagogically necessary). Tangled rope holds both facts. The rising theater ratio alongside rising extraction marks the arc's drift from working hypothesis toward performed truism — the pre-piton condition, monitored rather than declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_commitment_delta,
    'This story instantiates only the technological_determinism reading of kernel press_reformation_causality; what structural changes follow if a sibling reading is adopted instead?',
    'Compare the three reading files'' beneficiary declarations, epsilon values, and computed types side by side: strategic_deployment surfaces printer and reformer agency as central variables; co_constitution dissolves the autonomy premise into feedback-loop analysis between print economics and doctrinal controversy.',
    'Under strategic_deployment the press loses autonomous-mover status and beneficiary structure concentrates on deploying actors, shifting classification toward the deployment arrangements themselves; under co_constitution the inevitability claim dissolves entirely and the operative constraint migrates to the historiographical-practice layer this story already measures.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_commitment_delta, conceptual, 'Committer structure: one-of-three readings of the press-Reformation causality kernel; sibling readings would relocate beneficiaries and dissolve the autonomy premise.').

omega_variable(
    autonomy_premise_testability,
    'Is the press''s supposed autonomy a coherent causal category, or does the thesis rest on an unfalsifiable counterfactual (without the press, no Reformation) that no evidence could touch?',
    'Comparative analysis of late-print-adoption regions (Orthodox Europe, Ottoman millet communities) where vernacular religious movements followed different trajectories despite similar scriptural impulses; systematic counterfactual modeling of print timing against reform outcomes.',
    'If autonomy survives comparison, part of the arc''s extraction is the price of a real causal insight; if it fails, the thesis collapses toward the co_constitution reading and this constraint''s classification drifts toward inertial residue with rising theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_premise_testability, empirical, 'Whether technological autonomy is empirically grounded or counterfactually insulated.').

omega_variable(
    obscured_beneficiary_structure,
    'Who actually collects from the arc''s persistence, and is the collection concentrated enough that the reading''s beneficiary-obscuring move is load-bearing for the arc''s survival?',
    'Trace textbook-adoption revenue, documentary commissioning decisions, and citation patterns of the Gutenberg analogy in technology rhetoric; identify which seats would lose measurable value if survey narratives were revised.',
    'Concentrated collection by named seats confirms the tangled-rope reading and sharpens directionality; diffuse collection would push the structure toward inertial persistence and eventual piton characterization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(obscured_beneficiary_structure, empirical, 'Whether the arc''s gains accrue to identifiable seats or are genuinely diffuse.').

omega_variable(
    pedagogical_floor_residual,
    'How much of the arc''s continued circulation reflects an irreducible pedagogical floor — any first-pass narrative must simplify — versus extractive inertia that a better simplification could replace?',
    'Test whether contingency-centered or co-constitution survey treatments achieve equivalent retention and comprehension at comparable cost; if a superior simplification exists and is not adopted, the residual is inertia, not necessity.',
    'A high irreducible floor bounds the attributable extraction and strengthens the coordination half of the tangled-rope reading; a low floor means nearly all measured extraction is replaceable overhead, supporting drift toward snare-side assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedagogical_floor_residual, conceptual, 'Irreducible simplification cost versus replaceable extractive overhead in the arc''s circulation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__technological_determinism, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__technological_determinism, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causality__technological_determinism, theater_ratio, 10, 0.14).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causality__technological_determinism, theater_ratio, 20, 0.19).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causality__technological_determinism, theater_ratio, 30, 0.25).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causality__technological_determinism, theater_ratio, 40, 0.32).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causality__technological_determinism, theater_ratio, 50, 0.4).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causality__technological_determinism, theater_ratio, 60, 0.48).
narrative_ontology:measurement(pres_tr_t70, press_reformation_causality__technological_determinism, theater_ratio, 70, 0.55).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__technological_determinism, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(pres_be_t10, press_reformation_causality__technological_determinism, base_extractiveness, 10, 0.26).
narrative_ontology:measurement(pres_be_t20, press_reformation_causality__technological_determinism, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(pres_be_t30, press_reformation_causality__technological_determinism, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(pres_be_t40, press_reformation_causality__technological_determinism, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(pres_be_t50, press_reformation_causality__technological_determinism, base_extractiveness, 50, 0.56).
narrative_ontology:measurement(pres_be_t60, press_reformation_causality__technological_determinism, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(pres_be_t70, press_reformation_causality__technological_determinism, base_extractiveness, 70, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__technological_determinism, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(pres_su_t10, press_reformation_causality__technological_determinism, suppression_requirement, 10, 0.17).
narrative_ontology:measurement(pres_su_t20, press_reformation_causality__technological_determinism, suppression_requirement, 20, 0.23).
narrative_ontology:measurement(pres_su_t30, press_reformation_causality__technological_determinism, suppression_requirement, 30, 0.29).
narrative_ontology:measurement(pres_su_t40, press_reformation_causality__technological_determinism, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(pres_su_t50, press_reformation_causality__technological_determinism, suppression_requirement, 50, 0.41).
narrative_ontology:measurement(pres_su_t60, press_reformation_causality__technological_determinism, suppression_requirement, 60, 0.45).
narrative_ontology:measurement(pres_su_t70, press_reformation_causality__technological_determinism, suppression_requirement, 70, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__technological_determinism, information_standard).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causality__technological_determinism, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% The colloquial label 'the printing press caused the Reformation' decomposes, per the epsilon-invariance principle, into three structurally distinct constraint stories sharing the kernel press_reformation_causality. This file is the technological_determinism reading: the standing arrangement is the deterministic arc's dominance in pedagogy and popular culture, with beneficiaries declared and epsilon assessed at 0.62. The strategic_deployment reading centers actor agency and will carry a different beneficiary/victim structure centered on deploying printers and reformers; the co_constitution reading dissolves the autonomy premise and measures the feedback-loop arrangement instead. Each file carries its own epsilon, stakeholders, and classification; the edges here form the constraint family so drift and contamination propagate correctly. Upstream/downstream: the determinist reading's cultural dominance shapes the operating environment of both siblings, which is recorded in cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
