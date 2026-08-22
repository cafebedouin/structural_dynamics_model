% ============================================================================
% CONSTRAINT STORY: reformation_event_boundary__theological_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_reformation_event_boundary__theological_climb_reading, []).

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
 *   constraint_id: reformation_event_boundary__theological_climb_reading
 *   human_readable: Theological Climb Reading of the Reformation Event Boundary
 *   domain: historical epistemology/religious history/commitment systems
 *
 * SUMMARY:
 *   This story instantiates ONE reading — the theological climb — of the
 *   contested reformation_event_boundary kernel: the commitment, maintained
 *   in confessional historiography, education, and commemoration, that the
 *   Reformation was primarily a theological innovation event in which
 *   Luther's recovery of justification by faith alone constituted a genuine
 *   doctrinal breakthrough requiring institutional separation, with the event
 *   bounded tightly at 1517-1555. The constraint this story is about is that
 *   boundary as an operating structure on historical discourse: it fixes the
 *   doctrinal core, the canonical actor set, the cost-bearing assignment (the
 *   Catholic Church as the corrected object), and the period. The
 *   claim/metric gap is deliberate: the constraint is CLAIMED as rope (the
 *   reading's own framing — truth-telling coordination around a real
 *   doctrinal development) while the authored metrics describe an arrangement
 *   whose acknowledged simplification costs, defensive policing, and
 *   performative maintenance have all risen across the interval — the engine
 *   measures the divergence; do not reconcile the claim to the metrics. KEY
 *   AGENTS (by structural relationship): - reformation_theology_scholarship:
 *   agenda-setter and collector (institutional/identity_locked) — maintains
 *   the boundary through curricula, canons, and commemorations; collects
 *   interpretive authority - protestant_confessional_communities: primary
 *   beneficiary (organized/identity_locked) — receives the founding narrative
 *   and denominational legitimacy - believers_freed_from_false_doctrine:
 *   beneficiary (moderate/identity_locked) — the reading's declared freed
 *   class, holds the account as spiritual genealogy -
 *   catholic_church_magisterium: primary cost-bearer
 *   (institutional/constrained) — bears the corrected-object assignment;
 *   cannot exit the discourse - catholic_reform_movements: cost-bearer
 *   (organized/constrained) — internal renewal demoted to reaction-status -
 *   social_political_historians: cost-bearer (organized/mobile) — rival
 *   causal drivers demoted but material finds other venues -
 *   radical_reformation_communities: excluded voice
 *   (moderate/identity_locked) — would object; outside the frame the boundary
 *   organizes - ecumenical_dialogue_bodies: analytical observer
 *   (institutional/analytical) — assesses the doctrinal claims and attests
 *   convergence
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, 0.55).
domain_priors:suppression_score(reformation_event_boundary__theological_climb_reading, 0.58).
domain_priors:theater_ratio(reformation_event_boundary__theological_climb_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(reformation_event_boundary__theological_climb_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(reformation_event_boundary__theological_climb_reading, rope).
narrative_ontology:human_readable(reformation_event_boundary__theological_climb_reading, "Theological Climb Reading of the Reformation Event Boundary").
narrative_ontology:topic_domain(reformation_event_boundary__theological_climb_reading, "historical epistemology/religious history/commitment systems").

domain_priors:requires_active_enforcement(reformation_event_boundary__theological_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(reformation_event_boundary__theological_climb_reading, '0262e65d-10f4-4d64-b160-5c7b2d621822').
narrative_ontology:cs_kernel_codification('0262e65d-10f4-4d64-b160-5c7b2d621822', fixed_text).
narrative_ontology:cs_authority_grounding('0262e65d-10f4-4d64-b160-5c7b2d621822', lineage).
narrative_ontology:cs_interpretation_layer_present('0262e65d-10f4-4d64-b160-5c7b2d621822').
narrative_ontology:cs_reading_relation('0262e65d-10f4-4d64-b160-5c7b2d621822', reformation_event_boundary__political_swap_reading, forecloses).
narrative_ontology:cs_reading_relation('0262e65d-10f4-4d64-b160-5c7b2d621822', reformation_event_boundary__composite_overdetermination_reading, coexists_with).
narrative_ontology:cs_axiom('0262e65d-10f4-4d64-b160-5c7b2d621822', foundational, justification_by_faith_alone_is_genuine_breakthrough).
narrative_ontology:cs_axiom_status(justification_by_faith_alone_is_genuine_breakthrough, holdable).
narrative_ontology:cs_axiom_grounding('0262e65d-10f4-4d64-b160-5c7b2d621822', justification_by_faith_alone_is_genuine_breakthrough, theological).
narrative_ontology:cs_axiom('0262e65d-10f4-4d64-b160-5c7b2d621822', foundational, doctrinal_truth_required_institutional_separation).
narrative_ontology:cs_axiom_status(doctrinal_truth_required_institutional_separation, holdable).
narrative_ontology:cs_axiom_grounding('0262e65d-10f4-4d64-b160-5c7b2d621822', doctrinal_truth_required_institutional_separation, instrumental).
narrative_ontology:cs_reference_frame('0262e65d-10f4-4d64-b160-5c7b2d621822', confessional_breakthrough_settlement).
narrative_ontology:cs_drift_state('0262e65d-10f4-4d64-b160-5c7b2d621822', contemporary_post_jddj_quincentenary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('0262e65d-10f4-4d64-b160-5c7b2d621822', '').
narrative_ontology:cs_kernel_id(reformation_event_boundary__theological_climb_reading, reformation_event_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, protestant_confessional_communities).
narrative_ontology:constraint_beneficiary(reformation_event_boundary__theological_climb_reading, reformation_theology_scholarship).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_church_magisterium).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, catholic_reform_movements).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(reformation_event_boundary__theological_climb_reading, social_political_historians).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, sola_fide_doctrinal_primacy).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, justification_breakthrough_genuineness).
narrative_ontology:constraint_vindicates(reformation_event_boundary__theological_climb_reading, institutional_separation_necessity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the climb account through seminary curricula, canonical reading lists, confessional publishing, and commemoration organization; sets which questions about the Reformation's origin are live and which are settled. Collects interpretive authority, careers, and institutional mandate from the account's centrality. Exit would mean re-framing a life's work and losing standing in the confessional institutions that employ it — the account and the professional identity are fused.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, reformation_theology_scholarship, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(reformation_event_boundary__theological_climb_reading, reformation_theology_scholarship, beneficiary).

% Lutheran, Reformed, and wider Protestant church bodies receive a founding narrative in which their separation traces to a doctrinal recovery rather than a revolt; the account organizes their teaching, liturgical commemoration such as Reformation Day, and collective self-understanding. Membership structures and identity claims are bound to the account; abandoning it would mean re-founding collective identity rather than updating a curriculum.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, protestant_confessional_communities, beneficiary,
    organized, generational, identity_locked, global).

% Laypeople and clergy within the evangelical traditions hold the account as spiritual genealogy: the moment conscience was freed from the indulgence economy by the recovered gospel. The narrative is received through catechesis and family transmission, and questioning it tends to feel like questioning the inheritance itself rather than evaluating a historical claim.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, believers_freed_from_false_doctrine, beneficiary,
    moderate, generational, identity_locked, global).

% Bears the account's central cost: its sixteenth-century position is cast as the corrected object — the system the breakthrough exposed and required separation from. It cannot exit the arrangement: every ecumenical dialogue, shared curriculum, and historical commemoration re-imposes the frame, and responding on the frame's terms concedes the doctrinal case while declining to respond concedes the narrative. It documents internal reform and contests the framing through its own scholarship and diplomacy.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_church_magisterium, payer,
    institutional, civilizational, constrained, global).

% The internal Catholic reform currents of the sixteenth century and their modern historiographers bear demotion: within the climb account their work is subordinated to reaction-status — a counter-move rather than an independent reform trajectory — which flattens a century of internal renewal into a footnote to Protestant initiative. Their corrective material is published but rarely admitted to the confessional curricula where the account is taught.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, catholic_reform_movements, payer,
    organized, generational, constrained, continental).

% Historians of imperial politics, printing economics, urban governance, and peasant grievance find their causal material demoted to background in an account whose driver is doctrinal. Their exit is comparatively easy: the same material finds homes in early-modern studies, political history, and economic history venues outside the confessional frame, so the cost is standing within this narrative rather than livelihood.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, social_political_historians, payer,
    organized, biographical, mobile, continental).

% Anabaptist, Spiritualist, and their modern descendant communities would object that the magisterial climb narrative centers the very actors who persecuted them and erases a concurrent, distinct reformation that fits neither the two-party frame nor the tight period. They sit outside the conversation the boundary organizes: confessional curricula and commemorations rarely name them except as dissenting footnotes.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, radical_reformation_communities, excluded,
    moderate, generational, identity_locked, global).

% Bilateral commissions and dialogue processes — including the work that produced the 1999 Joint Declaration on the Doctrine of Justification — assess the doctrinal claims across the boundary, document convergence and residual difference, and publish findings bearing directly on whether the separation was required by the doctrine. They collect no rents and bear no extraction; they observe and attest.
narrative_ontology:constraint_stakeholder(reformation_event_boundary__theological_climb_reading, ecumenical_dialogue_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(reformation_event_boundary__theological_climb_reading, reformation_theology_scholarship).
narrative_ontology:fixing_cost_class(reformation_event_boundary__theological_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The boundary solves a real transmission problem: how to teach, commemorate, and transmit a forty-year transformation spanning dozens of polities and languages. Fixing a doctrinal core (justification by faith alone), a canonical actor set, and a bounded period gives schools, churches, and publishers a teachable, transmissible account of the movement's origin and meaning.
% TRANSFER_FUNCTION: Moves interpretive authority and narrative legitimacy toward the theological account and the seats that maintain it, and moves cost toward the accounts it subordinates: the Catholic position is recast as the corrected object, political and social drivers are demoted to background, and trajectories outside the period are marginalized.
% ABSENT_VOICES: Radical Reformation communities would object that the narrative centers their persecutors and erases a concurrent distinct reformation; they sit outside the curricular and commemorative conversation the boundary organizes. Sixteenth-century lay and oral believers outside the literate theological record — peasants, women, the unlettered — are structurally absent from an account built on doctrinal texts and clerical polemic.
% DISAPPEARANCE_RATIONALE: If the climb-boundary vanished overnight, confessional education, Reformation commemorations, denominational self-understanding, and a large scholarly and publishing apparatus would lose their organizing frame; the event's meaning and period would have to be re-narrated from contested first principles, and the beneficiary seats would face an identity re-founding problem rather than a curriculum update.
% FOUNDING_PROBLEM: Post-Reformation Protestant communities needed an account of their separation that grounded it in recovered truth rather than revolt — the climb narrative answered the Catholic charge of schism by recasting the break as the necessary consequence of a doctrinal breakthrough, giving the new churches a founding event worthy of the name reformation.
% FOUNDING_PROBLEM_CORROBORATION: Inside the benefiting set, confessional communities and the scholarship attest both the problem and its continuing force. Outside: ecumenical dialogue bodies attest that the doctrinal substance is real but that the inference from doctrine to required separation no longer commands the consensus it did (the 1999 Joint Declaration documented substantial convergence on justification); Catholic reform historiography attests that the narrative was framed against a rival account and flattens internal Catholic renewal; social and political historians attest that a single doctrinal driver underdetermines the record. Corroboration for the founding problem's original force is genuine; corroboration for its continuing adequacy is contested from outside the beneficiary set.
narrative_ontology:disappearance_verdict(reformation_event_boundary__theological_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(reformation_event_boundary__theological_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(reformation_event_boundary__theological_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(reformation_event_boundary__theological_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(reformation_event_boundary__theological_climb_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reformation_event_boundary__theological_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(reformation_event_boundary__theological_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(reformation_event_boundary__theological_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (interval end): the reading's own honest account concedes that the boundary flattens the Catholic position into a corrected object, demotes political, social, and economic drivers to background, and marginalizes trajectories outside the tight period — costs the reading acknowledges while holding the doctrinal core genuine. Suppression (0.58) reflects enforcement that is discursive rather than physical: confessional curricula, canonical reading lists, commemoration funding, and denominational education police the frame; rivals are disadvantaged, not eliminated. Theater (0.55) is the interval's sharpest signal: as rival scholarship accumulated, public maintenance shifted toward performative restatement — Reformation Day cycles, anniversary volumes, the 2017 quincentenary — while composite and social scholarship did much of the analytical work. Accessibility collapse (0.42) is low because the historiographical contest remains genuinely open: rival readings are thinkable, publishable, and taught. Resistance (0.62) is high: Catholic reform historiography, social and political history, and excluded Radical-Reformation voices all contest the frame. The three measurement series run on one shared grid (t = 0, 25, 50, 75, 100, 125, mapping to 1900-2025) so every metric is authored at every examined point; all three rise together — extraction, theater, and enforcement requirement climbing as the frame turned from hegemonic truth-telling to defensive maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (reformation_theology_scholarship) the boundary is truth-telling: the breakthrough was real, the separation required, the period tight because the event was. From the Catholic cost-bearing seat the same boundary operates as enforced misdescription — an institutional power with constrained exit (it cannot leave the discourse; every ecumenical and educational encounter re-imposes the frame) bearing the corrected-object assignment. The excluded Radical seat experiences erasure. The engine computes these per-seat classifications from power, exit, and role; the story-level rope claim does not adjudicate them, and a computed divergence at the constrained cost-bearing seats is exactly the signal the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real structure: confessional communities and believers receive the founding narrative (low d, amplified by identity-locked exit — they cannot trade the account without trading identity); the scholarship seat both maintains and collects, so its derived directionality sits near the beneficiary end despite its administrative role. Victim declarations: the Catholic magisterium bears the corrected-object cost with constrained exit, placing it near the full-target end; Catholic reform movements bear demotion to reaction-status with similar constrained exit. Social and political historians bear demotion of their causal accounts but hold mobile exit — their material finds homes in adjacent fields — which damps their effective extraction relative to the trapped Catholic seats. The excluded Radical seat is recorded as an authored absence: commentary-grade only, never a classification override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legitimating separation as recovered truth rather than revolt against the Catholic schism charge — remains live in confessional communities and contested in the academy; status is authored 'contested', so no dead-mandate mismatch flag fires. The mandatrophy risk is real nonetheless: if the legitimation need continues to fade while commemorative maintenance grows, the boundary drifts toward theatrical persistence — the rising theater series is the early signal, and the quincentenary restatement pattern (periodic anniversary re-enforcement of a frame whose analytical center of gravity has moved) is the observable mechanism. The classification keeps both faces visible: the rope claim preserves the genuine coordination (a teachable account organized around a real doctrinal development), while the constrained cost-bearing seats preserve the extraction (enforced misdescription of the Catholic position and demotion of rival drivers). Mislabeling in either direction — pure coordination or pure extraction — would lose the hybrid structure the corpus exists to measure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_sibling_structural_delta,
    'This constraint is one reading — theological_climb — of the reformation_event_boundary kernel; the political_swap and composite_overdetermination sibling readings would reassign the event''s causal driver, cost-bearing structure, and periodization; where exactly does the disagreement bind, and what would each sibling change structurally?',
    'Author the sibling stories over the same referent and compare per-reading epsilon, beneficiary/victim assignment, and periodization bounds; the disagreement binds at (a) the genuineness of the theological core, (b) whether institutional separation was required by the doctrine or was a contingent political outcome, and (c) whether the period closes at 1555 or runs through the confessionalization aftermath.',
    'If the political_swap sibling authors high epsilon over this same boundary (theology as post-hoc cover), this story''s reading-indexed extraction is seat-relative and the boundary computes as extractive at the constrained cost-bearing seats; if the composite sibling authors moderate epsilon with an extended period, the tight periodization is extracting simplicity from the record. This story''s rope claim would then be measuring as a false summit at those seats.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_structural_delta, conceptual, 'Committer structure: this story is one reading of a contested kernel; siblings would reassign the driver, the victim structure, and the period.').

omega_variable(
    sola_fide_breakthrough_genuineness,
    'Was Luther''s justification account a genuine doctrinal breakthrough — a novel recovery — or a development continuous with existing late-medieval Augustinian currents?',
    'Intellectual-history comparison of late-medieval Augustinian school texts with Luther''s 1515-1516 Romans lectures and the 1517 theses; trace continuity versus discontinuity in the simul iustus et peccator and forensic justification motifs.',
    'If strongly continuous, the ''breakthrough requiring separation'' premise weakens, the boundary''s coordination function shifts toward identity maintenance, and theater and extraction rise; if genuinely discontinuous, the climb reading''s core holds and the extraction stays modest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sola_fide_breakthrough_genuineness, empirical, 'Whether the doctrinal core of the climb reading is genuine novelty or continuity.').

omega_variable(
    separation_necessity_inference,
    'Does doctrinal disagreement on justification necessitate institutional separation, or is separation one possible response among several?',
    'Comparative and counterfactual analysis: doctrinal disputes resolved without separation (including the later ecumenical convergence documented in the 1999 Joint Declaration); the sixteenth-century actors'' own assessment of whether separation was required by the doctrine or chosen under concurrent political pressure.',
    'If separation was contingent rather than required, the boundary''s necessity clause is doing identity work rather than truth-tracking, and extraction and theater rise; if required, the climb reading''s inference holds and the boundary''s claim is stronger than its current metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separation_necessity_inference, conceptual, 'Whether the separation was entailed by the doctrine or contingent on political conditions.').

omega_variable(
    periodization_truncation_cost,
    'What does the tight 1517-1555 periodization exclude, and does the excluded material — Radical Reformation trajectories, the maturation of internal Catholic reform, confessionalization running to the mid-century settlements and beyond — change the event''s causal structure?',
    'Re-run the causal account on an extended period and with the Radical wing and Catholic internal reform centered; test whether the single doctrinal-driver claim survives the extension.',
    'If the account survives extension, the tight period is a legitimate focusing device; if it fails, the periodization is extracting simplicity at the record''s expense and the theater and extraction measures rise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(periodization_truncation_cost, empirical, 'The cost imposed by the reading''s tight periodization on the record it bounds.').

omega_variable(
    confessional_identity_lock_mechanism,
    'Is the beneficiary seats'' identity lock ideological (the recovered gospel as constitutive truth), institutional (denominational structures fused with the account), or professional (scholarly and clerical careers bound to it)?',
    'Observe communities where one leg has weakened — for example, mainline denominations after the 1999 Joint Declaration — and test whether the account''s grip on identity, curriculum, and commemoration persists when institutional or professional incentives shift.',
    'If the lock is ideological, the boundary outlives its institutional support and persists as performance in secularized contexts; if institutional, restructuring the institutions dissolves it; if professional, generational turnover in the scholarship releases it. The classification of the maintenance activity differs accordingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(confessional_identity_lock_mechanism, empirical, 'Which mechanism binds the beneficiary seats to the account, and what its release would look like.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(reformation_event_boundary__theological_climb_reading, 0, 125).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refo_tr_t0, reformation_event_boundary__theological_climb_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(refo_tr_t25, reformation_event_boundary__theological_climb_reading, theater_ratio, 25, 0.18).
narrative_ontology:measurement(refo_tr_t50, reformation_event_boundary__theological_climb_reading, theater_ratio, 50, 0.25).
narrative_ontology:measurement(refo_tr_t75, reformation_event_boundary__theological_climb_reading, theater_ratio, 75, 0.35).
narrative_ontology:measurement(refo_tr_t100, reformation_event_boundary__theological_climb_reading, theater_ratio, 100, 0.45).
narrative_ontology:measurement(refo_tr_t125, reformation_event_boundary__theological_climb_reading, theater_ratio, 125, 0.55).

% Extraction over time
narrative_ontology:measurement(refo_be_t0, reformation_event_boundary__theological_climb_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(refo_be_t25, reformation_event_boundary__theological_climb_reading, base_extractiveness, 25, 0.32).
narrative_ontology:measurement(refo_be_t50, reformation_event_boundary__theological_climb_reading, base_extractiveness, 50, 0.38).
narrative_ontology:measurement(refo_be_t75, reformation_event_boundary__theological_climb_reading, base_extractiveness, 75, 0.45).
narrative_ontology:measurement(refo_be_t100, reformation_event_boundary__theological_climb_reading, base_extractiveness, 100, 0.5).
narrative_ontology:measurement(refo_be_t125, reformation_event_boundary__theological_climb_reading, base_extractiveness, 125, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(refo_su_t0, reformation_event_boundary__theological_climb_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(refo_su_t25, reformation_event_boundary__theological_climb_reading, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(refo_su_t50, reformation_event_boundary__theological_climb_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement(refo_su_t75, reformation_event_boundary__theological_climb_reading, suppression_requirement, 75, 0.52).
narrative_ontology:measurement(refo_su_t100, reformation_event_boundary__theological_climb_reading, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(refo_su_t125, reformation_event_boundary__theological_climb_reading, suppression_requirement, 125, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(reformation_event_boundary__theological_climb_reading, identity_coordination).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__political_swap_reading).
narrative_ontology:affects_constraint(reformation_event_boundary__theological_climb_reading, reformation_event_boundary__composite_overdetermination_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Reformation' decomposes, per the epsilon-invariance principle, into structurally distinct causal-boundary claims. This story instantiates the theological-climb reading (genuine doctrinal breakthrough, tight 1517-1555 period, Catholic Church as corrected object); the political-swap and composite-overdetermination siblings instantiate different constraints with their own epsilon values, victim structures, and periodization. They form a constraint family linked through affects_constraints: the upstream confessional account historically supplied the narrative frame that the sibling readings contest, so this story sits upstream of both siblings in the contamination network.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
