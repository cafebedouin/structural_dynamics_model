% ============================================================================
% CONSTRAINT STORY: total_war_winnability_post1945__strategic_culture_drift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_winnability_post1945__strategic_culture_drift, []).

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
 *   constraint_id: total_war_winnability_post1945__strategic_culture_drift
 *   human_readable: Strategic-Culture Drift: Total War's Discursive Exclusion from Elite Strategy
 *   domain: international relations/strategic studies
 *
 * SUMMARY:
 *   This story instantiates the strategic_culture_drift reading of the kernel
 *   total_war_winnability_post1945: total war remains materially reachable
 *   after 1945, but an ideational shift in elite strategic culture removed it
 *   from the menu of seriously considered options. The constraint under
 *   assessment is that standing discursive arrangement — the professional
 *   machinery of strategy (war colleges, doctrine bodies, flagship journals,
 *   budget categories) organized exclusively around limited-war forms —
 *   persisting by institutional inertia rather than enforcement. Its ancestor
 *   was a genuine coordination device: the early-Cold-War limited-war grammar
 *   that let nuclear-armed rivals bound conflicts and signal restraint. That
 *   function has atrophied — the bipolar problem it managed is closed — yet
 *   the frame reproduces itself through professional formation and
 *   publication incentives, leaving the total-war branch of the option space
 *   unplanned and increasingly unplannable. Per the epsilon-referent rule,
 *   extractiveness is authored for THIS standing arrangement as this reading
 *   sees it: a real but diffuse stripping of option value from those who
 *   would need the full spectrum, not a transfer to any capturer. Sibling
 *   readings (normative_reading_drop, structural_contraction_reading) are
 *   separate constraints linked in network.affects_constraints; their
 *   mechanisms are not averaged into this file. Assumptions stated: the
 *   interval maps calendar years 1950-2025; measurement values are
 *   reconstructed scholarly judgments from the doctrinal, archival, and
 *   budgetary record, marked observed because each point reports a past
 *   state.
 *
 * KEY AGENTS:
 *   - - limited_war_defense_intellectuals: Primary beneficiary (moderate/identity_locked) — careers and expertise constituted within the limited-war frame
 *   - - strategic_studies_establishment: Agenda setter (institutional/constrained) — administers curricula, doctrine, and publication gates that reproduce the frame by routine
 *   - - national_command_authorities: Primary target (powerful/trapped) — confront crises holding only limited-war playbooks
 *   - - defense_industrial_base_planners: Secondary target (organized/constrained) — steward an atrophied mobilization function they did not choose to lose
 *   - - worst_case_planning_advocates: Excluded voice (moderate/constrained) — argue for keeping total war thinkable; marginalized as unserious
 *   - - comparative_strategic_culture_scholars: Analytical observer (analytical/analytical) — see the drift recurring across cases and eras
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, 0.6).
domain_priors:suppression_score(total_war_winnability_post1945__strategic_culture_drift, 0.28).
domain_priors:theater_ratio(total_war_winnability_post1945__strategic_culture_drift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, extractiveness, 0.6).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(total_war_winnability_post1945__strategic_culture_drift, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_winnability_post1945__strategic_culture_drift, piton).
narrative_ontology:human_readable(total_war_winnability_post1945__strategic_culture_drift, "Strategic-Culture Drift: Total War's Discursive Exclusion from Elite Strategy").
narrative_ontology:topic_domain(total_war_winnability_post1945__strategic_culture_drift, "international relations/strategic studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_winnability_post1945__strategic_culture_drift, '16f3f213-fa31-47aa-931a-d60f99ba3c2b').
narrative_ontology:cs_kernel_codification('16f3f213-fa31-47aa-931a-d60f99ba3c2b', distributed).
narrative_ontology:cs_authority_grounding('16f3f213-fa31-47aa-931a-d60f99ba3c2b', distributed).
narrative_ontology:cs_reading_relation('16f3f213-fa31-47aa-931a-d60f99ba3c2b', total_war_winnability_post1945__normative_reading_drop, coexists_with).
narrative_ontology:cs_reading_relation('16f3f213-fa31-47aa-931a-d60f99ba3c2b', total_war_winnability_post1945__structural_contraction_reading, forecloses).
narrative_ontology:cs_axiom('16f3f213-fa31-47aa-931a-d60f99ba3c2b', foundational, cultural_forgetting_sustains_exclusion).
narrative_ontology:cs_axiom_status(cultural_forgetting_sustains_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('16f3f213-fa31-47aa-931a-d60f99ba3c2b', cultural_forgetting_sustains_exclusion, empirically_contingent).
narrative_ontology:cs_axiom('16f3f213-fa31-47aa-931a-d60f99ba3c2b', foundational, total_war_materially_reachable_today).
narrative_ontology:cs_axiom_status(total_war_materially_reachable_today, holdable).
narrative_ontology:cs_axiom_grounding('16f3f213-fa31-47aa-931a-d60f99ba3c2b', total_war_materially_reachable_today, empirically_contingent).
narrative_ontology:cs_reference_frame('16f3f213-fa31-47aa-931a-d60f99ba3c2b', full_spectrum_strategic_option_space).
narrative_ontology:cs_drift_state('16f3f213-fa31-47aa-931a-d60f99ba3c2b', contemporary_post_2022_reassessment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('16f3f213-fa31-47aa-931a-d60f99ba3c2b', '').
narrative_ontology:cs_kernel_id(total_war_winnability_post1945__strategic_culture_drift, total_war_winnability_post1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals).
narrative_ontology:constraint_beneficiary(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_establishment).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, national_command_authorities).
narrative_ontology:constraint_victim(total_war_winnability_post1945__strategic_culture_drift, defense_industrial_base_planners).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, limited_war_paradigm_sufficiency).
narrative_ontology:constraint_vindicates(total_war_winnability_post1945__strategic_culture_drift, graduated_escalation_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars and practitioners whose expertise — deterrence theory, arms control, insurgency, crisis bargaining — was formed inside the limited-war canon. Their standing, citations, and consultancies depend on limited war remaining the organizing question of the field. Leaving the frame would mean retraining into a specialty with no institutional home; most have spent entire careers inside it.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, limited_war_defense_intellectuals, beneficiary,
    moderate, biographical, identity_locked, global).

% War colleges, service doctrine centers, defense think tanks, and flagship journals that decide what counts as serious strategic work. They set curricula, run the wargames, review the articles, and staff the commissions. None of them voted to exclude total-war planning; the exclusion reproduces itself through ordinary editorial and curricular judgment. They could commission a total-war branch tomorrow; nothing in their incentive structure tells them to.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_establishment, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(total_war_winnability_post1945__strategic_culture_drift, strategic_studies_establishment, beneficiary).

% Presidents, prime ministers, and defense secretaries who would direct a great-power war. In a crisis they receive plans for limited options — strikes, blockades, escalation ladders — because that is what the system produces. The total-war annex is blank: not forbidden, just unwritten. They cannot exit their own unpreparedness once a crisis arrives.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, national_command_authorities, payer,
    powerful, immediate, trapped, national).

% Officials responsible for surge production, mobilization law, and stockpile depth. Their counterparts in 1942 ran war production boards; today the function survives as scattered offices with legacy authorities and no exercised playbook. They bear the gap twice: once as stewards of capacity that quietly vanished, and again as the people who would have to rebuild it from a standing start.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, defense_industrial_base_planners, payer,
    organized, generational, constrained, national).

% Heterodox strategists, historians, and a few serving officers who argue that keeping total war thinkable is a duty of serious planning. They publish at the margins of the field, are read as alarmists or romantics, and lack a permanent institutional platform. Their arguments gain traction only after shocks — 2014, 2022 — and fade as attention and budgets revert.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, worst_case_planning_advocates, excluded,
    moderate, generational, constrained, global).

% Academic observers who compare how different militaries and epochs remember and forget war. They watch the same drift recur across cases — interwar Britain, post-Vietnam America — and can name the mechanism without holding a seat inside the arrangement they study.
narrative_ontology:constraint_stakeholder(total_war_winnability_post1945__strategic_culture_drift, comparative_strategic_culture_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(total_war_winnability_post1945__strategic_culture_drift, diffuse).
narrative_ontology:fixing_cost_class(total_war_winnability_post1945__strategic_culture_drift, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its founding phase the frame solved a real problem: giving nuclear-armed rivals a shared grammar for keeping wars bounded — limited objectives, graduated force, recognizable signals of restraint — so that proxy conflict would not slide into general war. Residually it still coordinates allied planning assumptions and civil-military expectations about what war will look like.
% TRANSFER_FUNCTION: Moves attention, prestige, curriculum slots, journal pages, and budget lines toward limited-war specialties and away from total-war planning. Nothing material is delivered to any seat in exchange: the option space itself is what leaves the system, and no one receives it.
% ABSENT_VOICES: Worst-case planning advocates and the classical total-war tradition (absolute-war scholarship, mobilization economics) would object that a strategy shop unable to think the largest case is not doing strategy. They stand outside the conference circuits, journal boards, and commission rosters where the frame is reproduced; they enter only episodically, after shocks, and exit when attention reverts.
% DISAPPEARANCE_RATIONALE: If the exclusion lifted overnight, war colleges would reinstate total-war branches, doctrine bodies would reopen mobilization annexes, journals would commission the largest-case literature, and budget categories would reappear — the professional architecture of the field visibly rearranges around the restored branch, which is why the frame's incumbents preserve it without ever having to argue for it.
% FOUNDING_PROBLEM: After 1945 the operative problem was how nuclear-armed adversaries could fight at all without triggering general war: the limited-war frame was built to bound conflict, make restraint legible, and give both blocs a common vocabulary for de-escalation.
% FOUNDING_PROBLEM_CORROBORATION: Cold War diplomatic historians working from declassified archives (Korea, Suez, Cuba) corroborate both the founding problem and its closure with the bipolar order; memoirs of former senior officials describe the grammar as a product of its moment. No source outside the strategic-studies establishment contends the original problem persists in its original form; the establishment itself declines to litigate the question.
narrative_ontology:disappearance_verdict(total_war_winnability_post1945__strategic_culture_drift, world_rearranges).
narrative_ontology:founding_problem_status(total_war_winnability_post1945__strategic_culture_drift, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_winnability_post1945__strategic_culture_drift, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_winnability_post1945__strategic_culture_drift, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_winnability_post1945__strategic_culture_drift, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_winnability_post1945__strategic_culture_drift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_winnability_post1945__strategic_culture_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Claimed type is piton on structural grounds independent of the metrics: the arrangement is a former rope (the early-Cold-War limited-war grammar was a real coordination device) whose founding function has died with the bipolar order, which no one actively enforces (requires_active_enforcement false — exclusion reproduces through routine editorial and curricular judgment, not coercion), which has no concentrated capturer (the option space is destroyed, not transferred), and which exhibits the cost asymmetry — the establishment could revive the branch, but a decades-long rebuild exceeds what any seat bears. Metrics are authored independently as descriptions: extractiveness 0.60 reflects diffuse, compounding option-value loss rather than concentrated transfer; suppression 0.28 reflects soft gatekeeping (journal and career incentives) with no coercive machinery, and is authored as a raw structural property unscaled by power or scope; theater_ratio 0.68 reflects a field whose wargames stop below the largest case, whose 'full-spectrum' language carries no total-war content, and which commemorates past mobilizations it could not repeat; accessibility_collapse 0.52 reflects persistent heterodox scholarship outside the funded mainstream; resistance 0.42 reflects the post-2014 and post-2022 mobilization-rebuild movements. The measurement series share one grid (seven points, both tracked metrics at every point) so no metric borrows another's end-state. Boltzmann declares identity_coordination because the frame's surviving function is professional-boundary maintenance — deciding who counts as a serious strategist; the default floor stands, and the FNL gaming risk was checked: the coupling does not concentrate costs on powerless agents, since the paying seats are powerful and trapped.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the beneficiary/administrator seats should compute differently. From the national command authority's position the constraint appears at the worst possible moment as an absence — a blank annex in the plan, discovered under time pressure with no exit. From the defense intellectual's position the same structure is simply 'the field': normal science, the water they swim in, experienced as the natural shape of the discipline rather than as a choice anyone made. From the establishment's position, administering the frame feels like curation and standards-setting, not exclusion. The observer seat sees the drift itself as the datum. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: limited_war_defense_intellectuals sit near the beneficiary end (the frame subsidizes their relevance), and their identity_locked exit pushes them further toward insulation — they cannot cheaply reconstitute themselves outside the frame, so the frame's continuation is worth more to them than to a mobile occupant. The strategic_studies_establishment carries a dual position via secondary_role: it administers the frame (agenda-setter duties) while benefiting from it, placing its derived directionality moderately low but not minimal, since administration carries real cost. National command authorities sit near the full-target end: they absorb the entire option-space loss and are trapped at decision time — no arbitrage exists against one's own unpreparedness. Defense industrial base planners likewise sit high: they bear the atrophy and would bear the rebuild. Worst-case planning advocates hold a target-like position with no seat. Spatial scope runs national-to-global; verifying an absence (what is NOT being planned) is intrinsically hard, so scope amplifies effective extraction modestly. No directionality_overrides are authored: the beneficiary/victim plus exit declarations already yield the correct ordering, and the schema keys overrides to power atoms, which would be too coarse to improve on the derivation here.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a textbook resolved mandatrophy: the founding problem (bounding superpower conflict through a shared limited-war grammar) died with the bipolar order, and the arrangement persists by inertia. The classification prevents two mislabels. Calling it a snare would require a capturer and active enforcement — both absent: gains are diffuse, nothing receives the transferred value because nothing is transferred, and the exclusion needs no enforcement machinery. Calling it a rope would require a live coordination function performing its founding service — the founding service is gone, and the residual identity-coordination is boundary maintenance, not the original work. The receipt surface confirms the piton cell: gain_flow is affirmatively diffuse (re-read of every stakeholder situation finds no seat accruing the lost option space as gain; the intellectuals' benefit is incidental career continuity, not receipt of the extraction) and fixing_cost is prohibitive (rebuilding mobilization law, industrial surge capacity, doctrinal annexes, and a trained generation is a multi-decade project exceeding what any seat bears). The R5 mismatch — dead founding problem, world_rearranges verdict — flags exactly the zombie-inertia signature the piton category exists to catch.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mechanism_of_exclusion_kernel_reading,
    'Is the disappearance of total war from elite discourse caused by ideational drift (this reading), by normative prohibition (normative_reading_drop), or by structural impossibility (structural_contraction_reading)?',
    'Comparative archival and interview evidence holding physics constant: examine episodes where actors contemplated total war despite consolidating norms, and trace whether planning capacity decayed before, after, or independently of the legal-normative regime.',
    'If norms carry the exclusion, the constraint belongs to the normative sibling''s file with identifiable enforcers; if physics, it approaches a fixed limit with negligible epsilon; only the drift finding sustains this file''s piton profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_of_exclusion_kernel_reading, conceptual, 'Which mechanism — culture, norms, or physics — carries the exclusion of total war.').

omega_variable(
    residual_reachability_of_total_war,
    'Does total war remain materially reachable, as this reading asserts, or has industrial and organizational atrophy already converted reachability into fiction?',
    'Independent audits of mobilization capacity: surge-production studies, currency reviews of mobilization statutes, and wargames that actually run the total-war branch to completion rather than truncating at the limited threshold.',
    'If unreachable, this reading collapses toward the structural sibling and the arrangement becomes pure vestige — theater_ratio approaches 1.0 and the inertial reading hardens; if reachable, the drift account stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_reachability_of_total_war, empirical, 'Whether the claimed residual reachability of total war is real or already fictional.').

omega_variable(
    intellectual_beneficiary_status,
    'Are limited-war defense intellectuals genuine beneficiaries whose position depends on the exclusion, or incidental occupants who would thrive equally in a full-spectrum field?',
    'Counterfactual career and funding analysis: model the field''s demand for existing specialties under a revived total-war branch, using the post-2022 mobilization-funding episode as a natural experiment in how quickly the mainstream absorbs largest-case work.',
    'Genuine dependence on the exclusion would push the classification toward tangled_rope (a coordinated frame carrying positional rents); incidental occupancy leaves the inertial reading intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intellectual_beneficiary_status, empirical, 'Status of the intellectual class as beneficiary versus epiphenomenal carrier of the frame.').

omega_variable(
    drift_reversibility,
    'Is the atrophied capacity rebuildable, or has tacit-knowledge loss made the exclusion effectively irreversible?',
    'Historical reconstruction of prior rebuilds — interwar-to-WWII mobilization, the post-Vietnam professional revitalization — to price the knowledge-recovery curve against present institutional conditions and workforce continuity.',
    'Irreversibility fixes fixing_cost at prohibitive and locks the inertial cell; cheap reversibility would recast the arrangement as transient neglect rather than a durable constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(drift_reversibility, empirical, 'Reversibility of the forgotten total-war planning capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_winnability_post1945__strategic_culture_drift, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strategic_culture_drift_tr_t1950, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1950, 0.3).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t1950, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t1962, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1962, 0.41).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t1962, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t1972, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1972, 0.5).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t1972, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t1983, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1983, 0.57).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t1983, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t1991, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 1991, 0.62).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t1991, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t2003, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2003, 0.65).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t2003, observed).
narrative_ontology:measurement(strategic_culture_drift_tr_t2025, total_war_winnability_post1945__strategic_culture_drift, theater_ratio, 2025, 0.68).
narrative_ontology:measurement_basis(strategic_culture_drift_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(strategic_culture_drift_be_t1950, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1950, 0.22).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t1950, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t1962, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1962, 0.34).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t1962, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t1972, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1972, 0.44).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t1972, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t1983, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1983, 0.51).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t1983, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t1991, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 1991, 0.57).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t1991, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t2003, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2003, 0.54).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t2003, observed).
narrative_ontology:measurement(strategic_culture_drift_be_t2025, total_war_winnability_post1945__strategic_culture_drift, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(strategic_culture_drift_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(total_war_winnability_post1945__strategic_culture_drift, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_winnability_post1945__strategic_culture_drift, identity_coordination).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, normative_reading_drop).
narrative_ontology:affects_constraint(total_war_winnability_post1945__strategic_culture_drift, structural_contraction_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the obsolescence of total war after 1945.' The label conflates three structurally distinct claims with different epsilon: a normative-prohibition claim (normative_reading_drop), a physical-impossibility claim (structural_contraction_reading), and this ideational-drift claim. Each gets its own file, its own epsilon, and its own stakeholder surface; the edges here record the family linkage. This reading sits downstream in discourse: each sibling is cited as a rival explanation for the same observable — the empty total-war branch in elite planning — and this file's mechanism claim (forgetting, not forbidding or impossibility) is what the other two files deny.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
