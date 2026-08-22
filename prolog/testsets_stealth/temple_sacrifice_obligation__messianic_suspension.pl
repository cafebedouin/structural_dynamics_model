% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Messianic Suspension of Sacrificial Obligation
 *   domain: religious/halakhic/commitment-systems
 *
 * SUMMARY:
 *   Since the destruction of the Second Temple (70 CE), the rabbinic
 *   tradition has held the Torah's sacrificial obligations in a declared
 *   interim status: suspended — not fulfilled, not violated, not annulled —
 *   pending messianic restoration. Under this reading (one of three held
 *   within the same canon), the obligation remains in full normative force
 *   but cannot currently be performed; adjudication of its performance is
 *   deferred to the restoration event itself, which no seat controls. Study
 *   of sacrifice law continues throughout, but its status under this reading
 *   is precise: it is neither compliance (the duty is not currently
 *   occupiable) nor preparation (no restoration work is authorized or
 *   underway) but maintenance of knowledge-in-waiting — keeping a law alive
 *   for a community that expects to need it again. This story instantiates
 *   the messianic_suspension reading of the temple_sacrifice_obligation
 *   kernel as a clean, epsilon-invariant constraint; the sibling readings
 *   (study_as_occupation, study_as_archiving) are separate stories in the
 *   same family. Claim and metrics are authored independently: the constraint
 *   is CLAIMED as scaffold — an explicitly transitional arrangement whose
 *   declared terminus is the restoration event — while the metrics describe
 *   very low extraction, near-zero standing suppression, low theater, and an
 *   interpretive field that stays open. KEY AGENTS (by structural
 *   relationship): halakhic_authority_structure — agenda-setter
 *   (institutional/constrained), administers the doctrine and defers
 *   adjudication; observant_jewish_community — primary beneficiary
 *   (organized/identity_locked), relieved of an unperformable duty without
 *   its annulment; rabbinic_scholarship — primary payer, secondary
 *   beneficiary (moderate/identity_locked), concentrates the maintenance cost
 *   and receives standing from the same arrangement;
 *   restorationist_preparation_movements — excluded (organized/constrained),
 *   hold the obligation live and preparation mandatory;
 *   academic_observer_of_rabbinics — analytical observer.
 *
 * KEY AGENTS:
 *   - halakhic_authority_structure: Agenda-setter (institutional/constrained) — administers the suspension doctrine, sets the maintenance curriculum, defers adjudication to the restoration event
 *   - observant_jewish_community: Primary beneficiary, secondary payer (organized/identity_locked) — relieved of an unperformable duty without its annulment; pays the maintenance draw diffusely
 *   - rabbinic_scholarship: Primary payer, secondary beneficiary (moderate/identity_locked) — careers spent on unpracticable law; receives standing from the arrangement it pays into
 *   - restorationist_preparation_movements: Excluded (organized/constrained) — hold the obligation live and preparation mandatory; kept outside adjudication by non-recognition
 *   - academic_observer_of_rabbinics: Analytical observer (analytical/analytical) — documents the arrangement's interpretive structure from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.1).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.1).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, scaffold).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Messianic Suspension of Sacrificial Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic/commitment-systems").

narrative_ontology:has_sunset_clause(temple_sacrifice_obligation__messianic_suspension).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, '58b9a7f0-e31a-47e3-a518-c67e2625447c').
narrative_ontology:cs_kernel_codification('58b9a7f0-e31a-47e3-a518-c67e2625447c', fixed_text).
narrative_ontology:cs_authority_grounding('58b9a7f0-e31a-47e3-a518-c67e2625447c', lineage).
narrative_ontology:cs_interpretation_layer_present('58b9a7f0-e31a-47e3-a518-c67e2625447c').
narrative_ontology:cs_reading_relation('58b9a7f0-e31a-47e3-a518-c67e2625447c', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('58b9a7f0-e31a-47e3-a518-c67e2625447c', temple_sacrifice_obligation__study_as_archiving, influences).
narrative_ontology:cs_axiom('58b9a7f0-e31a-47e3-a518-c67e2625447c', foundational, obligation_suspended_not_annulled).
narrative_ontology:cs_axiom_status(obligation_suspended_not_annulled, holdable).
narrative_ontology:cs_axiom_grounding('58b9a7f0-e31a-47e3-a518-c67e2625447c', obligation_suspended_not_annulled, deontological).
narrative_ontology:cs_axiom('58b9a7f0-e31a-47e3-a518-c67e2625447c', foundational, deferred_adjudication_is_resolution).
narrative_ontology:cs_axiom_status(deferred_adjudication_is_resolution, holdable).
narrative_ontology:cs_axiom_grounding('58b9a7f0-e31a-47e3-a518-c67e2625447c', deferred_adjudication_is_resolution, conventional).
narrative_ontology:cs_reference_frame('58b9a7f0-e31a-47e3-a518-c67e2625447c', full_obligation_in_declared_interim).
narrative_ontology:cs_drift_state('58b9a7f0-e31a-47e3-a518-c67e2625447c', contemporary_restorationist_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('58b9a7f0-e31a-47e3-a518-c67e2625447c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholarship).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholarship).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, ones_rachmana_patrei_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, deferred_adjudication_principle).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, covenantal_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts, academies, and decisors administer the suspension doctrine: they set the curriculum that keeps sacrifice law in circulation, answer the practical questions that still arise (vows contingent on sacrifice, status questions touching Temple service), and hold the line that non-performance is not violation. They cannot end the suspension — the terminus is the restoration event, not a ruling — but they shape what the interim means: which texts are taught, how the atonement gap is answered, how restorationist pressure is met. Their exit from the framework would be exit from the tradition they constitute.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authority_structure, agenda_setter,
    institutional, generational, constrained, global).

% Holds the covenantal framework in which the sacrificial obligations remain real but unperformable. The suspension relieves it of an impossible duty without annulling the duty: no member is in violation, and liturgy, calendar, and education stay oriented toward a restored service. It pays the maintenance draw diffusely — study hours, curriculum space, liturgical attention — and carries the atonement question that sacrifice once answered directly. Exit means leaving the covenantal identity itself, not choosing another reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__messianic_suspension, observant_jewish_community, payer).

% Students and masters who carry the maintenance function in concentrated form: careers spent mastering tractates whose subject matter cannot be practiced, teaching Kodshim to students who will never offer a sacrifice. Under this reading their labor is knowledge-in-waiting — not compliance, not preparation. They receive standing and purpose from the arrangement and bear its cost in the same motion; exit would dissolve the scholarly identity their expertise constitutes.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholarship, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholarship, beneficiary).

% Organized groups — Temple-preparation institutes, Temple Mount activism, and historically the Samaritan and messianic movements — that reject the suspension's passivity: they hold the obligation live and preparation mandatory now, breeding candidates, fashioning vessels, pressing for access. The halakhic mainstream keeps them outside adjudication; their frame is treated as marginal rather than answered. They cannot force the conversation their way, and their alternative is constrained by non-recognition rather than by coercion.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, restorationist_preparation_movements, excluded,
    organized, immediate, constrained, regional).

% Scholars of rabbinics and comparative religion who study how the tradition has managed an unperformable obligation for two millennia. They take no seat in the arrangement's internal adjudication; they document its interpretive structure, its episodic confrontations with messianic movements, and the shifting justifications given for study.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, academic_observer_of_rabbinics, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_obligation__messianic_suspension, rabbinic_scholarship).
narrative_ontology:fixing_cost_class(temple_sacrifice_obligation__messianic_suspension, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Holds a covenantal community's normative order coherent while its central obligations cannot be performed: it gives every member the same answer to an impossible duty — suspended, not violated, not annulled — so that practice, liturgy, and education synchronize around one interim status instead of fragmenting into private solutions of guilt, hypocrisy, or abandonment.
% TRANSFER_FUNCTION: Moves almost nothing in the present: no goods, labor, or status change hands under the suspension itself. Its draw is attention — study effort moved from the community into maintenance of sacrifice-law knowledge, stewarded by the scholarship for a restoration no seat can schedule. The substantive transfer the law provides (sacrificial atonement) is deferred, not rerouted.
% ABSENT_VOICES: Restorationist preparation movements would object that the suspension's passivity itself violates the obligation — preparation is mandatory now — and they sit outside the adjudicative conversation. Also unseated: the penitent's interest in the atonement that sacrifice would have provided; the tradition supplies repentance, prayer, and charity as substitutes, but no stakeholder represents the deferred-atonement interest directly.
% DISAPPEARANCE_RATIONALE: Practice would barely change — no one is sacrificing now under any reading. What rearranges is the interpretive order: the question of what the community owes now reopens, the curriculum loses its framing (maintenance of what, for whom?), and the community must adopt a sibling reading — occupation or archiving — or confront the unperformable duty with no interim answer. The dependence is interpretive rather than practical, but the seats are real and the order they inhabit runs on the suspension's answer.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE left a legal order built around sacrificial worship with no altar: the founding problem was how to keep a law-centered covenant intact when its central obligations could not be performed — how to hold the duty alive without demanding the impossible.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of rabbinic Judaism — outside the arrangement's benefiting parties — corroborate both the founding problem (a legal order built around an altar lost in 70 CE) and its persistence into the present. Restorationist movements corroborate the problem's liveness while rejecting the suspension answer. Within the tradition, the liturgy itself attests daily that the problem remains open. No seat claims the founding problem is closed.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.1, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-floor (0.10): the arrangement demands nothing performable, transfers nothing, and its residual costs are the maintenance draw (attention and study effort moved into knowledge-in-waiting) and the deferred atonement question (see omega atonement_gap_accounting). Suppression is near-zero as a standing structural property — nothing enforces the suspension day to day; the impossibility enforces itself. The suppression_requirement series is cyclical, not monotonic: spikes at the post-Bar-Kokhba generation (t=1) and the Sabbatean catastrophe (t=16) mark episodes where the waiting status had to be actively held against activist restoration; between surges the requirement decays because no enforcement is needed. The modern restorationist episode (t=20) is managed interpretively rather than coercively — a change in kind, hence no spike. The oscillation is not an extraction mechanism (nothing is extracted); it is surge-management around an external terminus no seat controls, and the scalar suppression (0.10) records the steady-state structural level between surges. Theater is low (0.18) but rising across the interval: as restoration receded from living expectation, a growing share of sacrifice-law activity became liturgical-recitative — the reading still counts recitation as maintenance, and the ratio stays far below any Goodhart threshold. Accessibility collapse is low (0.25): understanding the suspension does not collapse alternatives — the canon hosts the sibling readings and interpretive space stays open. Resistance is low (0.15): no constituency resists the suspension as such; the contest is interpretive (sibling readings) plus a marginal restorationist challenge. Time units are centuries since 70 CE; all three series share one grid ({0,1,4,8,12,16,18,20}) and the end-state values match the base_properties scalars. Suppression is authored as an unscaled structural property; only extractiveness is directionality- and scope-scaled by the engine.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit one canon and experience four arrangements. From the authority seat the suspension is faithful stewardship: a binding command held intact across impossibility, its adjudication honestly deferred. From the community seat it is relief with continuity: no member is in violation, and the liturgy keeps the restored service in view. From the scholarship seat the same structure is a lifetime's cost — mastery of law with no practice domain — redeemed only by the standing the maintenance role confers. From the restorationist seat it is intolerable passivity: an obligation treated as waiting when it should be treated as imminent. The engine computes per-seat classifications from the power, exit, and role data; the authored claim does not adjudicate between these experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The community and the scholarship are declared beneficiaries and derive low directionality: the arrangement subsidizes them — relief with continuity for the community, standing and purpose for the scholarship. There is no victim set; the expected structural delta holds, and omega atonement_gap_accounting tests the one place victims could appear. Two seats need overrides because the derivation chain has no beneficiary/victim declaration for them and their canonical fallbacks would misplace them. The authority structure (institutional) collects standing and purpose from stewarding the arrangement but also bears its defense costs — a mild beneficiary-side position, d = 0.35, not the symmetric default. The restorationist movements (organized) are extracted from by nothing, but the arrangement's communal dominance marginalizes their frame — a real cost to them — placing them mildly target-side, d = 0.55. Scope note: the arrangement is global in reach but its verification burden is trivial (there is nothing to verify), so scope amplification of extraction is negligible in practice.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification guards against two misreadings. Reading the suspension as pure extraction would fabricate a victim set the structure does not contain; reading it as a timeless rope would erase the arrangement's defining feature — its justification is the transition, not a steady state, and its terminus is declared in its own terms. The mandatrophy question (has the mandate outlived its function?) is live but unresolved: the founding problem — an unperformable obligation inside a law-centered covenant — remains open, and the arrangement's function, holding the duty in waiting, is intact. The classification degrades only if omega restoration_sunset_reality resolves toward a functionally-never terminus, in which case the transitional justification becomes vestigial and the drift analysis should re-examine the arrangement as de facto permanent. No mandatrophy_resolved flag is authored: the mandate has not outlived its function. Drift note for the kernel frame: the reading's reference frame (full obligation in a declared interim) is intact in its logic, but practice has drifted — the interim has outlasted every generation that declared it, and communal life is organized around indefinite suspension; the tradition acknowledges and theologically absorbs this gap rather than treating it as frame failure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'This constraint is one reading of the temple_sacrifice_obligation kernel. Do the sibling readings — study_as_occupation (study occupies the obligation now) and study_as_archiving (study preserves knowledge without fulfilling it) — describe the operative arrangement better, and would the cost and beneficiary structure shift under them?',
    'Examine how the community and its authorities actually frame study when the question is forced: curricular justifications, responsa on why Kodshim is studied, how scholars describe their own labor. The operative reading is the one the seats use under pressure, not the one the curriculum advertises.',
    'Under study_as_occupation, current study carries compliance-significance and effective extraction rises (a live duty is being channeled, and its performance standard becomes contestable). Under study_as_archiving, the maintenance framing collapses to pure custodial preservation and the transitional justification weakens further. Under messianic_suspension, extraction stays near the coordination floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which sibling reading of the sacrifice-obligation kernel is operative.').

omega_variable(
    restoration_sunset_reality,
    'Is the declared terminus — messianic restoration — a structural sunset that will fire, or an indefinitely deferred horizon that functions as never? After roughly twenty centuries, has the transitional arrangement degraded toward a de facto permanent arrangement whose transitional justification is vestigial?',
    'Partly theological (the terminus is a faith claim not resolvable from inside the framework) and partly empirical: does communal practice treat restoration as an actionable horizon — preparation budgets, candidate pipelines, expectation in planning — or as liturgical affirmation only? The practice answer is observable; the theological answer is not.',
    'If the sunset functions as never, the scaffold classification degrades toward steady-state coordination with vestigial transitional justification, and toward piton if the maintenance turns purely theatrical; if restoration remains an operative expectation for living seats, the transitional character is genuine and the classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_sunset_reality, conceptual, 'Whether the conditional sunset is structurally live or functionally never.').

omega_variable(
    atonement_gap_accounting,
    'Does the suspension leave real costs unremediated — penitents for whom sacrificial atonement was the designed remedy — or do the tradition''s substitutes (repentance, prayer, charity) fully cover the gap the deferral opens?',
    'Responsa and penitential literature on whether penitents experience unremediated residue; comparative analysis of atonement practice before and after 70 CE; the tradition''s own rankings of the substitutes against sacrifice.',
    'If the gap is real and felt, a diffuse victim set appears (deferred-atonement bearers), extraction rises, and the no-victim structure that keeps this reading clear of tangled_rope fails; if the substitutes are held sufficient, extraction stays near the floor and the no-victim structure holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(atonement_gap_accounting, empirical, 'Whether the deferral imposes unremediated costs on penitents.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.08).
narrative_ontology:measurement(temp_tr_t1, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1, 0.09).
narrative_ontology:measurement(temp_tr_t4, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 4, 0.1).
narrative_ontology:measurement(temp_tr_t8, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 8, 0.12).
narrative_ontology:measurement(temp_tr_t12, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 12, 0.14).
narrative_ontology:measurement(temp_tr_t16, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 16, 0.15).
narrative_ontology:measurement(temp_tr_t18, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 18, 0.17).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 20, 0.18).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(temp_be_t1, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1, 0.15).
narrative_ontology:measurement(temp_be_t4, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 4, 0.11).
narrative_ontology:measurement(temp_be_t8, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 8, 0.09).
narrative_ontology:measurement(temp_be_t12, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 12, 0.08).
narrative_ontology:measurement(temp_be_t16, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 16, 0.09).
narrative_ontology:measurement(temp_be_t18, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 18, 0.08).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 20, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(temp_su_t1, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1, 0.3).
narrative_ontology:measurement(temp_su_t4, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 4, 0.12).
narrative_ontology:measurement(temp_su_t8, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 8, 0.1).
narrative_ontology:measurement(temp_su_t12, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 12, 0.1).
narrative_ontology:measurement(temp_su_t16, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 16, 0.26).
narrative_ontology:measurement(temp_su_t18, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 18, 0.12).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 20, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% The colloquial label 'the obligation of sacrifices after the Temple's destruction' covers three structurally distinct readings of one kernel (temple_sacrifice_obligation). This story instantiates messianic_suspension: the obligation's status is suspended-pending-restoration, study is maintenance of knowledge-in-waiting, extraction is near-floor, and there is no victim set. The sibling stories carry different structures: study_as_occupation makes current study compliance-adjacent (raising the stakes of performance standards and effective extraction), study_as_archiving reduces the arrangement to custodial preservation (weakening the transitional justification). Each file holds one stable epsilon; the readings are linked here as one constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_obligation__messianic_suspension, institutional, 0.35).
constraint_indexing:directionality_override(temple_sacrifice_obligation__messianic_suspension, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
