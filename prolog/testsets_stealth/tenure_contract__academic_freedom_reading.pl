% ============================================================================
% CONSTRAINT STORY: tenure_contract__academic_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tenure_contract_academic_freedom_reading, []).

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
 *   constraint_id: tenure_contract__academic_freedom_reading
 *   human_readable: Tenure as Protection of High-Risk Inquiry (Academic Freedom Reading)
 *   domain: higher education governance / labor economics / institutional theory
 *
 * SUMMARY:
 *   This story instantiates the academic_freedom_reading of the
 *   tenure_contract kernel: tenure is a credible-commitment device that
 *   decouples researcher survival from institutional and political
 *   displeasure, and thereby coordinates scientific labor toward high-risk,
 *   long-horizon inquiry. The ε referent is the standing tenure arrangement
 *   assessed by this reading's own lights — the protection function as it
 *   actually operates, not the rival arrangements the sibling readings would
 *   substitute. Per the ε-invariance principle, the kernel decomposes into
 *   three structurally distinct constraints: this reading (protection of
 *   inquiry, low extraction, rope-shaped), the
 *   institutional_extraction_reading (rent capture by early winners and
 *   cost-loading onto contingent labor — a different ε, different victim
 *   set), and the demographic_reproduction_reading (peer review as
 *   gatekeeping reproducing dominant-group composition). The contingent-labor
 *   and gatekeeping dimensions are deliberately NOT folded into this file;
 *   they belong to the siblings, linked via network.affects_constraints. The
 *   claim/metric gap is intentional and small here: this reading claims rope
 *   and the authored metrics describe low-but-nonzero extraction
 *   (probationary intensification, compensation discount, senior lock-in) —
 *   the engine measures whatever divergence exists.
 *
 * KEY AGENTS:
 *   - - tenured_faculty: Primary beneficiary (organized/identity_locked) — collects security and agenda autonomy; pays via compensation discount and lock-in
 *   - - tenure_track_junior_faculty: Probationary payer with prospective beneficiary position (moderate/constrained) — bears the entry gauntlet
 *   - - university_administrations: Agenda-setter and cost-bearer (institutional/arbitrage) — runs the process, captures the wage discount, absorbs rigidity
 *   - - governing_boards_and_legislatures: External political actors (powerful/arbitrage) — the parties whose disciplinary power the firewall removes; nearest the full-target end
 *   - - doctoral_research_trainees and undergraduate_students: Downstream beneficiaries (low power) — inherit the inquiry the protection makes possible
 *   - - aaup_and_faculty_governance_bodies: Analytical observer (organized/analytical) — maintains standards, documents violations
 *   - - contingent_faculty: Excluded party (powerless/trapped) — adjacent to the protected core, outside the conversation that drew the boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tenure_contract__academic_freedom_reading, 0.3).
domain_priors:suppression_score(tenure_contract__academic_freedom_reading, 0.24).
domain_priors:theater_ratio(tenure_contract__academic_freedom_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, suppression_requirement, 0.24).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(tenure_contract__academic_freedom_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tenure_contract__academic_freedom_reading, rope).
narrative_ontology:human_readable(tenure_contract__academic_freedom_reading, "Tenure as Protection of High-Risk Inquiry (Academic Freedom Reading)").
narrative_ontology:topic_domain(tenure_contract__academic_freedom_reading, "higher education governance / labor economics / institutional theory").

domain_priors:requires_active_enforcement(tenure_contract__academic_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tenure_contract__academic_freedom_reading, '6bd3860d-0156-46b8-8176-44dc5a51582d').
narrative_ontology:cs_kernel_codification('6bd3860d-0156-46b8-8176-44dc5a51582d', formalized).
narrative_ontology:cs_authority_grounding('6bd3860d-0156-46b8-8176-44dc5a51582d', practice).
narrative_ontology:cs_interpretation_layer_present('6bd3860d-0156-46b8-8176-44dc5a51582d').
narrative_ontology:cs_reading_relation('6bd3860d-0156-46b8-8176-44dc5a51582d', tenure_contract__institutional_extraction_reading, influences).
narrative_ontology:cs_reading_relation('6bd3860d-0156-46b8-8176-44dc5a51582d', tenure_contract__demographic_reproduction_reading, coexists_with).
narrative_ontology:cs_axiom('6bd3860d-0156-46b8-8176-44dc5a51582d', foundational, survival_independence_enables_risky_inquiry).
narrative_ontology:cs_axiom_status(survival_independence_enables_risky_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('6bd3860d-0156-46b8-8176-44dc5a51582d', survival_independence_enables_risky_inquiry, empirically_contingent).
narrative_ontology:cs_axiom('6bd3860d-0156-46b8-8176-44dc5a51582d', foundational, punishing_conclusions_is_illegitimate).
narrative_ontology:cs_axiom_status(punishing_conclusions_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('6bd3860d-0156-46b8-8176-44dc5a51582d', punishing_conclusions_is_illegitimate, deontological).
narrative_ontology:cs_reference_frame('6bd3860d-0156-46b8-8176-44dc5a51582d', inquiry_protection_steady_state).
narrative_ontology:cs_drift_state('6bd3860d-0156-46b8-8176-44dc5a51582d', contemporary_legislative_backlash, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('6bd3860d-0156-46b8-8176-44dc5a51582d', '').
narrative_ontology:cs_kernel_id(tenure_contract__academic_freedom_reading, tenure_contract).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenured_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, tenure_track_junior_faculty).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, doctoral_research_trainees).
narrative_ontology:constraint_beneficiary(tenure_contract__academic_freedom_reading, undergraduate_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, tenure_track_junior_faculty).
narrative_ontology:constraint_victim(tenure_contract__academic_freedom_reading, university_administrations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold permanent appointments that can be ended only for adequate cause shown in a faculty hearing. Receive employment security and agenda autonomy: they can pursue unfashionable questions, criticize donors and legislatures, and publish unwelcome findings without risking livelihood. Pay for this through below-market salaries accepted in exchange for the security premium, and through lock-in: resigning a tenured line forfeits the protection and resets seniority elsewhere, so departure carries a compounding career cost. Professional identity is fused with the vocation, making exit psychologically as well as financially expensive.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenured_faculty, beneficiary,
    organized, biographical, identity_locked, national).

% Serve a six-to-seven-year probationary evaluation producing publication and teaching records under intensive assessment, at compensation below what their training commands outside academia, for a chance at entering the protected core. Those who survive collect the security their seniors hold; those who do not leave with deep field-specific specialization that transfers poorly. Their exit options narrow each year of the probation as the sunk specialization accumulates.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, tenure_track_junior_faculty, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, tenure_track_junior_faculty, beneficiary).

% Apprentice inside laboratories and seminars run by protected faculty. Benefit from mentorship continuity, multi-year projects that only a secure principal investigator can sustain, and advisors whose reputations do not depend on flattering funders. Are dependent on advisor placement power and departmental politics they do not govern.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, doctoral_research_trainees, beneficiary,
    moderate, biographical, constrained, national).

% Are taught by research-active faculty whose positions do not depend on instructional conformity. Receive stable course offerings and exposure to live inquiry, and carry tuition that funds the system, but hold no seat in tenure governance. Their practical lever is institutional choice: they can enroll elsewhere.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, undergraduate_students, beneficiary,
    powerless, immediate, mobile, national).

% Administer the tenure process: run reviews, convene committees, forward cases to boards. Capture the compensation discount on senior salaries, the retention of talent that would otherwise price itself at market, and the prestige product of independent scholarship. Bear the fiscal rigidity: salary lines committed for decades, dismissal paths that run through hearings, and departments that cannot be quickly resized. Their arbitrage valve is restructuring at the margins through contingent appointments rather than dismantling the core.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, university_administrations, agenda_setter,
    institutional, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(tenure_contract__academic_freedom_reading, university_administrations, payer).

% Set the statutory and handbook terms under which public institutions grant and revoke tenure, and periodically attempt post-tenure review mandates, program-triggered dismissals, or outright abolition. When they move against an individual scholar or a disfavored field, they encounter the due-process machinery directly: cause must be shown, hearings convened, and adverse-action litigation risk absorbed. The firewall binds them more tightly than any other party, and their electoral horizon rewards visible accountability gestures over procedural patience.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, governing_boards_and_legislatures, agenda_setter,
    powerful, immediate, arbitrage, regional).

% Maintain the 1940 Statement standards, investigate and censure institutions that violate them, and staff or advise hearing committees. Neither collect the protection nor bear its costs; they observe compliance, document deviations, and supply the interpretive tradition that defines adequate cause.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, aaup_and_faculty_governance_bodies, observer,
    organized, generational, analytical, national).

% Teach and grade adjacent to the protected core on semester-to-semester contracts, without its security, hearing rights, or agenda autonomy. Would contest the boundary that draws protection around one tier of the profession and not the other, but hold no seat in the tenure governance conversation that draws it.
narrative_ontology:constraint_stakeholder(tenure_contract__academic_freedom_reading, contingent_faculty, excluded,
    powerless, immediate, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tenure_contract__academic_freedom_reading, university_administrations).
narrative_ontology:fixing_cost_class(tenure_contract__academic_freedom_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a credible-commitment problem that neither side can solve by trusting the other: institutions cannot verifiably promise not to punish future unpopular findings, and researchers therefore will not sink decades into high-risk programs whose survival depends on continued approval. The tenure contract converts the unenforceable promise into a procedural firewall, coordinating research labor toward long-horizon inquiry.
% TRANSFER_FUNCTION: Moves employment security and agenda-setting autonomy from institutions to individual scholars; moves the price of that security — below-market compensation, decade-long probationary labor, and budgetary rigidity — from scholars to institutions, with the heaviest probationary burden falling on junior faculty during the entry window.
% ABSENT_VOICES: Contingent faculty stand outside the protection boundary and are not seated where it is drawn; undergraduate students fund the system through tuition but appear nowhere in tenure deliberations; state legislators seeking accountability levers participate only as external combatants rather than governed parties. Each would describe the arrangement differently if seated.
% DISAPPEARANCE_RATIONALE: If the protection vanished overnight, research agendas would shorten toward fundable and politically safe questions, scholars in controversial fields would self-censor or emigrate to systems that retain protection, junior faculty would face open-ended vulnerability to administrative and political displeasure, and the credible-commitment problem the contract solves would reappear immediately — the arrangement's disappearance rearranges the production of knowledge, not merely its distribution.
% FOUNDING_PROBLEM: In the early twentieth century, American universities dismissed professors summarily for economic heterodoxy, criticism of donors and trustees, and unpopular public positions — the episodes that produced the 1915 AAUP organizing declaration and the 1940 Statement. The founding problem: an honest scholar's livelihood depended on the pleasure of people with the power to fire him, making truthful inquiry professionally suicidal.
% FOUNDING_PROBLEM_CORROBORATION: Courts attest it from outside the benefiting parties — the Supreme Court's Keyishian dictum describing academic freedom as a special concern of the First Amendment rests on the historical record of punitive dismissals. Historians of the pre-1940 university document the purge episodes independently of faculty interests. Most tellingly, the arrangement's adversaries corroborate it behaviorally: legislators who draft abolition and post-tenure-review bills are attesting, by their expenditure of political capital, that the firewall binds and that removing it would restore a disciplinary lever that currently does not exist. Persecuted foreign scholars seeking refuge in tenured systems provide a third-party demand signal.
narrative_ontology:disappearance_verdict(tenure_contract__academic_freedom_reading, world_rearranges).
narrative_ontology:founding_problem_status(tenure_contract__academic_freedom_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tenure_contract__academic_freedom_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tenure_contract__academic_freedom_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tenure_contract__academic_freedom_reading, 0.3, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tenure_contract__academic_freedom_reading_tests).
:- end_tests(tenure_contract__academic_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.30) because the arrangement's costs are largely accepted prices of the protection: the compensation discount is traded voluntarily for security, and the probationary gauntlet, while heavy, purchases entry into the protected core. It is not zero — probationary labor intensity has ratcheted upward in the later interval, and senior lock-in is a real cost — hence the modest late-interval rise. Suppression is low (0.24) because the constraint's defining feature is the removal of suppression capacity from institutions and political actors over scholars; the residual reflects senior lock-in and the narrowing of junior exit options. Theater is low (0.18): tenure review performs real evaluative work, with a growing ceremonial share (dossier volume, external-letter rituals) in the later interval. Accessibility collapse is moderate (0.48): industry, teaching-only, and contingent tracks exist, but for a research-committed scholar they are degraded alternatives once the tenure path is understood. Resistance (0.38) is real and episodic — legislative abolition attempts, post-tenure-review mandates, trustee interventions — concentrated in the recent interval. The suppression_requirement series is authored deliberately: this story traces an enforcement-capacity dynamic (the firewall's strength), which built up through the mid-century due-process hardening and is now being eroded by statute in several states; a static scalar would miss the arc. All three series share one time grid (points 0–80), so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently, and the divergence is the finding. From the tenured faculty seat, the arrangement is a rope: net benefit, voluntary price, protection functioning. From the governing-board and legislature seat, the same structure operates as a blocking device — it extracts their disciplinary power while offering them nothing they value, a snare-shaped experience. From the administration seat it is hybrid: genuine coordination they administer, rigidity costs they bear, and a wage discount they capture. The engine derives these per-seat classifications from the structural data; the authored rope claim describes the constraint as this reading holds it, not as every seat experiences it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive faculty, trainee, and student seats toward the beneficiary end (low d, damped or inverted effective extraction). The powerful external actors are the exception the derivation chain cannot see: because they appear as agenda-setters rather than declared victims, structural derivation would place them mid-scale as neutral governors of the arrangement. Structurally they are its targets — the firewall's entire operation consists of removing their leverage over scholars, which is why their repeal attempts concentrate there. The directionality override (powerful → 0.85) corrects this: for THIS constraint, powerful external actors sit near the full-target end, matching the expected structural delta of high effective extraction at that seat. Junior faculty are approximated through their beneficiary declaration plus their payer role; a finer per-agent differentiation is not expressible at power-atom granularity and is flagged in the probationary_window_attribution omega.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so the mandate has not outlived its function and no mandatrophy resolution is declared. The classification work this reading performs is double-edged by design: it prevents the sibling extraction framing from condemning a genuine coordination mechanism as pure extraction (the costs are prices, the victims list is empty by this reading's lights), and symmetrically it prevents this reading from rope-washing the arrangement if the metrics turn — the watch-signature is firewall decay (falling suppression_requirement) combined with rising theater and accumulating extraction, the drift path from rope toward piton or snare. The R5 mismatch consumer should find no zombie flag here: status=live paired with verdict=world_rearranges is the coherent cell.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the tenure_contract kernel (academic_freedom_reading). What would the corpus look like if a sibling reading were adopted instead — specifically, does the institutional_extraction_reading''s victim set (contingent_faculty, junior scholars) or the demographic_reproduction_reading''s victim set (underrepresented scholars evaluated through fit and collegiality) describe the standing arrangement better?',
    'Cross-reading comparison against payroll composition data, dismissal-for-cause outcome records, and tenure-batch demographic audits; the readings make different victim-set predictions that the same institutional records can discriminate between.',
    'Adopting the extraction reading moves beneficiaries to senior_tenured winners, adds contingent_faculty as victims, raises ε substantially, and flips the classification toward tangled_rope or snare; adopting the reproduction reading adds gatekept scholar groups as victims and implicates the review mechanism rather than the protection mechanism. This file''s rope classification is conditional on the academic_freedom reading being the structurally accurate account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the tenure kernel the structural evidence supports; this story''s classification is reading-indexed.').

omega_variable(
    protection_vs_shelter_ambiguity,
    'Does the firewall protect dissent, or does it shelter mediocrity and orthodoxy — i.e., is the post-probation insulation predominantly producing high-risk inquiry or producing unaccountable comfort?',
    'Post-tenure productivity distributions, comparative study of protected versus unprotected research sectors, and analysis of what dismissal-for-cause proceedings actually target; natural experiments from jurisdictions imposing post-tenure review.',
    'If shelter dominates, the coordination function atrophies while the security persists: ε rises, theater_ratio rises, and the classification drifts from rope toward piton — the protection becoming performance maintained by inertia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_vs_shelter_ambiguity, empirical, 'Whether the insulation produces inquiry or shelter; determines the rope-versus-piton trajectory.').

omega_variable(
    probationary_window_attribution,
    'Is the six-to-seven-year probationary gauntlet part of this constraint''s protection bargain (the entry price of the credible commitment), or a separable extraction mechanism that belongs to the institutional_extraction_reading''s referent?',
    'Compare junior-faculty labor hours and compensation against matched professionals and against counterfactual non-tenure research tracks; test whether probation intensity tracks the protection''s value or labor-market monopsony power.',
    'If the gauntlet is separable extraction, part of this story''s ε migrates to the sibling constraint, lowering this file''s extractiveness further; if it is integral to the commitment device, the current ε attribution stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(probationary_window_attribution, empirical, 'Whether probationary-window costs belong to this constraint or to the sibling extraction constraint.').

omega_variable(
    external_control_legitimacy,
    'Does the firewall''s removal of political actors'' disciplinary power over scholars count as extraction from them (justifying the powerful-atom directionality override toward the full-target end), or as the removal of an illegitimate power that they never rightfully held (in which case they suffer no loss at all)?',
    'Not resolvable by data alone: it turns on antecedent commitments about who may govern inquiry — the same behavioral facts (blocked purges, repeal expenditure) support either description depending on the prior. Corpus comparison across readings with different legitimacy priors is the available instrument.',
    'If removal-of-illegitimate-power is adopted, the external actors'' effective extraction collapses toward zero and the override should be withdrawn; if loss-of-control is adopted, the override stands and their seat computes as the constraint''s primary target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_control_legitimacy, preference, 'Whether blocked political control is a loss to the blockers or a null event; fixes the directionality override''s standing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tenure_contract__academic_freedom_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tenure_af_reading_tr_t0, tenure_contract__academic_freedom_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement(tenure_af_reading_tr_t10, tenure_contract__academic_freedom_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(tenure_af_reading_tr_t20, tenure_contract__academic_freedom_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(tenure_af_reading_tr_t30, tenure_contract__academic_freedom_reading, theater_ratio, 30, 0.11).
narrative_ontology:measurement(tenure_af_reading_tr_t40, tenure_contract__academic_freedom_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement(tenure_af_reading_tr_t50, tenure_contract__academic_freedom_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement(tenure_af_reading_tr_t65, tenure_contract__academic_freedom_reading, theater_ratio, 65, 0.17).
narrative_ontology:measurement(tenure_af_reading_tr_t80, tenure_contract__academic_freedom_reading, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(tenure_af_reading_be_t0, tenure_contract__academic_freedom_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(tenure_af_reading_be_t10, tenure_contract__academic_freedom_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(tenure_af_reading_be_t20, tenure_contract__academic_freedom_reading, base_extractiveness, 20, 0.24).
narrative_ontology:measurement(tenure_af_reading_be_t30, tenure_contract__academic_freedom_reading, base_extractiveness, 30, 0.23).
narrative_ontology:measurement(tenure_af_reading_be_t40, tenure_contract__academic_freedom_reading, base_extractiveness, 40, 0.24).
narrative_ontology:measurement(tenure_af_reading_be_t50, tenure_contract__academic_freedom_reading, base_extractiveness, 50, 0.26).
narrative_ontology:measurement(tenure_af_reading_be_t65, tenure_contract__academic_freedom_reading, base_extractiveness, 65, 0.28).
narrative_ontology:measurement(tenure_af_reading_be_t80, tenure_contract__academic_freedom_reading, base_extractiveness, 80, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(tenure_af_reading_su_t0, tenure_contract__academic_freedom_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(tenure_af_reading_su_t10, tenure_contract__academic_freedom_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(tenure_af_reading_su_t20, tenure_contract__academic_freedom_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement(tenure_af_reading_su_t30, tenure_contract__academic_freedom_reading, suppression_requirement, 30, 0.73).
narrative_ontology:measurement(tenure_af_reading_su_t40, tenure_contract__academic_freedom_reading, suppression_requirement, 40, 0.76).
narrative_ontology:measurement(tenure_af_reading_su_t50, tenure_contract__academic_freedom_reading, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(tenure_af_reading_su_t65, tenure_contract__academic_freedom_reading, suppression_requirement, 65, 0.63).
narrative_ontology:measurement(tenure_af_reading_su_t80, tenure_contract__academic_freedom_reading, suppression_requirement, 80, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tenure_contract__academic_freedom_reading, resource_allocation).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, institutional_extraction_reading).
narrative_ontology:affects_constraint(tenure_contract__academic_freedom_reading, demographic_reproduction_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'tenure' covers three structurally distinct claims about one codified regime, decomposed per the ε-invariance principle. This story (academic_freedom_reading) is the historically upstream member — the 1940 Statement's own account — and is routinely cited as the justification layer beneath the other two debates. The institutional_extraction_reading and demographic_reproduction_reading are downstream critiques that cite failures or side-effects of the protection mechanism as evidence; each carries its own ε, beneficiary/victim structure, and claimed type. Edges here run from this reading to both siblings because the protection claim's empirical performance (does insulation produce inquiry?) sets the legitimacy conditions under which the critiques gain or lose force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tenure_contract__academic_freedom_reading, powerful, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
