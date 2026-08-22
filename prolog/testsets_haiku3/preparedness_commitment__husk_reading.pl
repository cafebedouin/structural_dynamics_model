% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_commitment__husk_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: preparedness_commitment__husk_reading
 *   human_readable: Preparedness Ritual Without Competence (Husk Reading)
 *   domain: institutional/social
 *
 * SUMMARY:
 *   This reading asserts that preparedness regimes in many institutions have
 *   decoupled from operational competence, becoming instead memorial
 *   performances—visible routines that communicate retention and readiness to
 *   regulators, stakeholders, and the institution itself, while actual
 *   response capacity atrophies. Drills become checked boxes. Protocols are
 *   memorized but never stress-tested against novelty. When disaster strikes,
 *   responders collapse into improvisation because the ritual training
 *   equipped them only to follow scripts, not to adapt. Leadership benefits
 *   from the appearance of preparedness (audit passes, governance narrative
 *   holds, reputational risk shifts) without bearing the cost of maintaining
 *   genuine competence. This reading sits in a contested kernel with two
 *   siblings: the competence_reading (drills and protocols genuinely maintain
 *   capacity) and the hybrid_reading (both memorial and competence functions
 *   operate simultaneously). The husk_reading claims the constraint is a
 *   piton—inertially maintained, theatrically defended, with no party
 *   benefiting enough to sustain it if the theater broke.
 *
 * KEY AGENTS:
 *   - institutional_administrators: agenda-setter, moderate power, constrained exit — maintain the rituals, benefit from form compliance
 *   - leadership_risk_managers: beneficiary, institutional power, mobile exit — design and justify the regime, shift reputational risk downward
 *   - personnel_under_false_security: payer, powerless, trapped — bear trust cost, face novelty without adaptive training
 *   - post_disaster_survivors: payer, powerless, trapped — bear response failure cost when constraint activates
 *   - competence_practitioners: excluded, moderate power, mobile exit — would advocate for stress-testing and adaptive capacity but are outside governance loop
 *   - institutional_continuity_narrative: beneficiary non-agent, vindicated by the constraint's existence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.68).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.52).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.41).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness Ritual Without Competence (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/social").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'caf9ac75-cb37-468b-a5ac-322561928b31').
narrative_ontology:cs_kernel_codification('caf9ac75-cb37-468b-a5ac-322561928b31', distributed).
narrative_ontology:cs_authority_grounding('caf9ac75-cb37-468b-a5ac-322561928b31', extraction).
narrative_ontology:cs_interpretation_layer_present('caf9ac75-cb37-468b-a5ac-322561928b31').
narrative_ontology:cs_reading_relation('caf9ac75-cb37-468b-a5ac-322561928b31', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('caf9ac75-cb37-468b-a5ac-322561928b31', preparedness_commitment__hybrid_reading, influences).
narrative_ontology:cs_axiom('caf9ac75-cb37-468b-a5ac-322561928b31', foundational, preparedness_is_memorial_performance).
narrative_ontology:cs_axiom_status(preparedness_is_memorial_performance, holdable).
narrative_ontology:cs_axiom_grounding('caf9ac75-cb37-468b-a5ac-322561928b31', preparedness_is_memorial_performance, empirically_contingent).
narrative_ontology:cs_axiom('caf9ac75-cb37-468b-a5ac-322561928b31', foundational, form_compliance_substitutes_for_capacity).
narrative_ontology:cs_axiom_status(form_compliance_substitutes_for_capacity, holdable).
narrative_ontology:cs_axiom_grounding('caf9ac75-cb37-468b-a5ac-322561928b31', form_compliance_substitutes_for_capacity, instrumental).
narrative_ontology:cs_reference_frame('caf9ac75-cb37-468b-a5ac-322561928b31', preparedness_as_visible_commitment).
narrative_ontology:cs_drift_state('caf9ac75-cb37-468b-a5ac-322561928b31', contemporary_post_incident_evaluation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('caf9ac75-cb37-468b-a5ac-322561928b31', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, institutional_continuity_narrative).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, leadership_risk_managers).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, personnel_under_false_security).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, post_disaster_survivors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain and enforce preparedness protocols—scheduling drills, certifying completion, documenting compliance. They benefit from visible form compliance (audits pass, governance structures stay intact) without bearing the cost of genuine competence maintenance. Their authority depends on the routines being *perceived* as retention, even when operational gap is documented internally.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Design and justify the preparedness regime. They benefit from a system that appears comprehensive and well-exercised in low-stakes drills, which shifts reputational risk away from them—if disaster strikes and response fails, the protocol existed and was 'followed' (even if meaningless). They have exit options: senior leadership can move between organizations or argue the scale of disaster exceeded any foreseeable plan.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, leadership_risk_managers, beneficiary,
    institutional, biographical, mobile, national).

% Are told they are prepared via completed drills and memorized protocols. They pay by investing trust in a system that has been decoupled from real competence. When actual disaster strikes, they face novel stress without the adaptive training the rituals claimed to provide. Their exit options are nearly zero—they cannot opt out of the organization, and preparedness certification happens at institutional level, not personal.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, personnel_under_false_security, payer,
    powerless, biographical, trapped, local).

% Bear the cost of response failure when the constraint activates. They depend on responders who trained in theater rather than competence. Their power is minimal during crisis and retrospective—retrospective blame falls on individuals, not on the institutional ritual structure that produced incompetence.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, post_disaster_survivors, payer,
    powerless, immediate, trapped, local).

% Certify protocol compliance without evaluating operational competence. They see checked boxes, completed training rosters, drill documentation—all the theater. They lack mandate or resources to assess whether people could actually respond under stress. Their role is captured: they certify form, not function.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, external_auditors, observer,
    institutional, biographical, analytical, national).

% Would advocate for adaptive training, novel-scenario drills, and competence assessment under stress. They are typically outside the institutional governance loop—relegated to specialized roles or lateral positions—and their concerns about the gap between ritual and capability are treated as niche expertise rather than governance priority. Some exit by moving to organizations with different preparedness cultures.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, competence_practitioners, excluded,
    moderate, generational, mobile, national).

% The doctrine that institutional preparedness is primarily a matter of visible commitment, inherited protocols, and attestable form compliance. This narrative benefits from the constraint because the constraint's existence and completion are cited as proof the narrative is real.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_continuity_narrative, beneficiary,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(preparedness_commitment__husk_reading, institutional_continuity_narrative).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, leadership_risk_managers).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains intergenerational transmission of institutional memory through repeated ritual: drills, protocols, training cycles, documentation. The coordination problem is 'how do we keep knowledge and responsibility alive across turnover?' The husk reading asserts the answer is 'through theater that *feels* like continuity,' not through adaptive competence.
% TRANSFER_FUNCTION: Transfers peace-of-mind and legal/reputational risk from leadership to personnel and eventual survivors. Leadership gives subordinates the assurance they are prepared (via drills, certificates, protocols). Personnel and survivors bear the cost when that assurance proves illusory under real stress.
% ABSENT_VOICES: Practitioners and researchers who have documented the gap between drill performance and actual-stress response are excluded from the institutional governance of preparedness. They produce evidence the protocols fail under novelty, but that evidence is treated as specialized research, not as a signal to redesign the system. Community responders, disaster survivors, and operational personnel at the individual level have no structural voice in what preparedness means.
% DISAPPEARANCE_RATIONALE: If this constraint—the ritualized performance of preparedness without competence—vanished, institutions would face a choice: either invest in genuine adaptive training and stress-testing (costly, ongoing, difficult to audit), or admit preparedness is illusory and reallocate resources. The absence of the theater would force that reckoning. Leadership risk postures would change, budget allocations would shift, and the reputational hiding place the ritual provides would collapse.
% FOUNDING_PROBLEM: After major organizational change (turnover, restructuring, technological shift, or a past disaster), how do we ensure the institution retains the knowledge and reflexes it developed? How do we communicate to stakeholders and regulators that we are not starting from scratch?
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was live when the protocols were designed (typically post-disaster or post-major-incident). Current institutional operators attest the problem is ongoing; however, independent disaster researchers, post-incident reviews, and retrospective survivor accounts document that the constraint persists not because the problem it was built to solve is live, but because the *appearance* of having solved the problem is now the incentive. The constraint's function has shifted from answering 'are we prepared?' to answering 'can we *show* we claim to be prepared?'
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_commitment__husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_commitment__husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_commitment__husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Theater ratio is exceptionally high (0.78 at interval end) because the core function—intergenerational knowledge transfer—operates almost entirely through performative ritual. Drills look like competence maintenance but lack novelty exposure or failure modes that would reveal gaps. Extractiveness is substantial (0.68) because the constraint transfers peace-of-mind and legal risk from leadership downward; personnel and survivors pay by trusting a false assurance. Suppression is moderate (0.52) because the theater is self-reinforcing—no one inside the institution has strong incentive to break it. Administrators maintain it (form compliance serves them). Leadership maintains it (risk shift serves them). Personnel and survivors lack power to demand change. Competence practitioners are excluded. The constraint persists not through active coercion but through diffuse cost distribution and absence of concentrated beneficiary maintenance. The measurement series shows extractiveness and theater_ratio rising through early interval (t=0–25) as the constraint matures and becomes normalized, then plateau as inertia takes over. Suppression rises modestly because as the constraint crystallizes, institutional effort to suppress evidence of the competence gap (silence research findings, frame drills as sufficient, suppress post-incident reviews that show the gap) increases.
 *
 * PERSPECTIVAL GAP:
 *   From the husk-reading seat (this constraint), the administrator and leader perceive preparedness as 'communicating commitment, passing audits, maintaining institutional narrative continuity.' From a competence_reading seat, the same constraint would be perceived as 'failed knowledge transfer, atrophying capacity, creating false security.' The engine will compute different directionalities: the administrator/leader sits near beneficiary (low d), while personnel/survivors sit near target (high d). The gap is structural, not opinion-based: one seat collects reputational safety and legal cover; another seat incurs trust cost and disaster-response vulnerability. Under the husk reading specifically, the gap reflects the constraint's core mechanism—using theater to shield leadership from consequence while distributing cost to those with no veto power.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional administrators and leadership risk managers are structural beneficiaries (d near 0.2–0.3): they collect the outputs of form compliance without bearing the cost of real competence maintenance. They set the rules, approve the drills, and face no direct consequence when those drills fail to translate to real-stress response. Personnel and survivors are targets (d near 0.8–0.9): they incur trust cost (believing they are prepared) and disaster-response cost (facing novelty without adaptive training) without power to change the system. The beneficiary/victim split is asymmetric and reinforced by power and exit: administrators are institutional, mobile (can shift to other posts), and constrained only by governance norms. Personnel and survivors are powerless and trapped. The husk reading specifically attributes this asymmetry to the constraint's theatricality—the constraint persists *because* it distributes cost diffusely and reserves benefits narrowly, and because the theater masks the distribution.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem—'how do we retain institutional memory across turnover?'—has been functionally dead for some time. The constraint persists not because the problem is live but because the *appearance* of having solved the problem is now institutionalized. Leadership has an incentive to maintain the theater. Administrators have a job defined by protocol compliance, not by actual capacity. The constraint satisfies the piton definition: no party is benefiting enough to actively maintain it (if forced to choose, leadership might admit its illusory nature and reallocate resources), but removing it requires concentrated effort no one is motivated to provide. The mandatrophy is resolved in the sense that the constraint's mandate has been definitively decoupled from its function—what remains is performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_vs_competence_breakdown,
    'Is the measured gap between drill performance and actual-stress response a feature of this specific constraint (poor ritual design, insufficient stress-testing) or a generic property of all preparedness training (transfer of training problem)?',
    'Comparative study of institutions with similar preparedness regimes but different drill philosophies (scripted vs. adaptive, low-fidelity vs. high-fidelity stress). Measure actual response performance post-incident and correlate to training design.',
    'If the gap is design-specific, remediation is in the constraint itself (redesign drills, add stress-testing, measure competence not form compliance). If generic, the constraint''s function may be inherently memorial rather than competence-maintaining, which would strengthen the husk reading and suggest piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_vs_competence_breakdown, empirical, 'Whether the competence gap is specific to ritualized preparedness or inherent to all training transfer.').

omega_variable(
    identity_lock_in_preparedness_administration,
    'Do institutional administrators and leadership maintain the preparedness theater because they genuinely believe it is sufficient, or because their career identity is bound to ''managing preparedness'' (where the management is performance, not capacity)?',
    'Exit analysis: when administrators leave the institution, do they advocate for changing preparedness systems in their new positions, or do they replicate the theater? Semi-structured interviews with retired administrators asking counterfactual: ''If you had discovered early in your tenure that the drills were not translating to capacity, what would you have done?''',
    'If identity-locked, the constraint is harder to dislodge—the theater is not a deliberate deception but a constitutive part of how administrators understand their role. This would shift the suppression mechanism from external coercion to internalized refusal to examine the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_in_preparedness_administration, conceptual, 'Whether administrators believe in or are psychologically bound to the theater.').

omega_variable(
    regulatory_capture_in_audit,
    'Are external auditors who certify preparedness compliance captured by the institutional regime, or are they genuinely constrained to audit form only?',
    'Audit independence study: examine auditor reports for evidence they assess competence vs. compliance. Interview auditors about mandate constraints. Cross-reference audit findings with post-incident reviews to see if auditors flagged competence gaps that materialized.',
    'If auditors are captured (they are part of the theater, their role is to certify form not question function), then suppression is higher than measured—the constraint has active institutional support from the audit regime. If constrained by mandate, then the suppression is endogenous (administrators and leadership choose not to demand competence assessment), and the constraint could be changed by expanding audit mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_in_audit, empirical, 'Whether auditors are captured or mandatarily limited.').

omega_variable(
    memorial_performance_as_reading_ambiguity,
    'Is ''preparedness as memorial performance'' (the husk reading) describing what the constraint IS, or what it has BECOME? Did it start as genuine competence maintenance and atrophy into theater, or was it always primarily about institutional legitimacy narrative?',
    'Historical analysis: examine the constraint at founding (first protocols, first drills, initial training design). Measure theater_ratio and theater_drift at t=0. Interview founders about intent. Compare founding-era incident response to contemporary response, controlling for disaster magnitude.',
    'If atrophied: the constraint transitioned from rope or tangled_rope to piton, and the husk reading is a historical account. If always memorial: the husk reading is structurally true from t=0, the constraint was never competence-focused, and the piton classification is stable across the whole interval. The distinction matters for interventions: atrophied constraints might be reactivated; always-memorial constraints need architectural change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(memorial_performance_as_reading_ambiguity, empirical, 'Whether preparedness theater is atrophied competence or original design.').

omega_variable(
    husk_vs_hybrid_sibling_distinction,
    'Does the preparedness system contain BOTH genuinely maintained competence (in specialized units or leadership core) AND theatrical protocols (for broader workforce), or is it theatrical all the way down?',
    'Audit specialized vs. general personnel. Measure stress-response competence and drill performance separately by role. Post-incident review comparing core-team actual decisions to protocol-team actual decisions. Emergency medical response analogy: do paramedics actually train to competence while EMTs drill to compliance?',
    'If layered (some genuine, some theater), the constraint is closer to the hybrid_reading. If uniformly theatrical, the husk reading is stronger. This directly addresses whether the constraint is a degraded rope or a specialized piton—a genuine question about what the institution has actually chosen to maintain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(husk_vs_hybrid_sibling_distinction, empirical, 'Whether the constraint is uniformly theatrical or contains pockets of real competence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.62).
narrative_ontology:measurement(prep_tr_t5, preparedness_commitment__husk_reading, theater_ratio, 5, 0.68).
narrative_ontology:measurement(prep_tr_t10, preparedness_commitment__husk_reading, theater_ratio, 10, 0.72).
narrative_ontology:measurement(prep_tr_t15, preparedness_commitment__husk_reading, theater_ratio, 15, 0.75).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.77).
narrative_ontology:measurement(prep_tr_t25, preparedness_commitment__husk_reading, theater_ratio, 25, 0.78).
narrative_ontology:measurement(prep_tr_t30, preparedness_commitment__husk_reading, theater_ratio, 30, 0.78).
narrative_ontology:measurement(prep_tr_t40, preparedness_commitment__husk_reading, theater_ratio, 40, 0.78).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prep_be_t5, preparedness_commitment__husk_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(prep_be_t10, preparedness_commitment__husk_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(prep_be_t15, preparedness_commitment__husk_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(prep_be_t25, preparedness_commitment__husk_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement(prep_be_t30, preparedness_commitment__husk_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(prep_be_t40, preparedness_commitment__husk_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t0, preparedness_commitment__husk_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(prep_su_t5, preparedness_commitment__husk_reading, suppression_requirement, 5, 0.4).
narrative_ontology:measurement(prep_su_t10, preparedness_commitment__husk_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement(prep_su_t15, preparedness_commitment__husk_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement(prep_su_t20, preparedness_commitment__husk_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(prep_su_t25, preparedness_commitment__husk_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement(prep_su_t30, preparedness_commitment__husk_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(prep_su_t40, preparedness_commitment__husk_reading, suppression_requirement, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the preparedness_commitment kernel. The kernel itself is contested: (1) the competence_reading asserts preparedness maintains operational capacity through live exercise; (2) the husk_reading (this file) asserts preparedness is memorial performance decoupled from capacity; (3) the hybrid_reading asserts both functions operate in a layered system. Each reading instantiates a different constraint with different ε values, beneficiary/victim structures, and type classifications. They are linked via the network.affects_constraints field: all three are structurally dependent on the same kernel (what preparedness is), and empirical resolution of one reading constrains the others. ε values: husk_reading is high (0.68, extraction via false assurance); competence_reading is low (genuine coordination); hybrid_reading is moderate (both functions, but with friction). The corpus needs all three as separate constraint stories to capture the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_commitment__husk_reading, powerless, 0.85).
constraint_indexing:directionality_override(preparedness_commitment__husk_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
