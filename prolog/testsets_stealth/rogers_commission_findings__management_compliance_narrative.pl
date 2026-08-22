% ============================================================================
% CONSTRAINT STORY: rogers_commission_findings__management_compliance_narrative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rogers_commission_findings__management_compliance_narrative, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: rogers_commission_findings__management_compliance_narrative
 *   human_readable: Post-Challenger Compliance-Process Regime (Management Reading of the Rogers Findings)
 *   domain: organizational_safety/technology_governance/regulatory_compliance
 *
 * SUMMARY:
 *   After the Challenger accident, the Rogers Commission's findings were
 *   implemented not as a technical boundary or a quantified risk-acceptance
 *   rule but as a compliance process: demonstrate documented risk awareness
 *   and mitigation effort, and launch may proceed. This file instantiates
 *   that management reading as a clean, epsilon-invariant constraint. The
 *   standing arrangement under assessment is the post-Challenger
 *   documentation-and-review regime itself; epsilon is authored for that
 *   arrangement as it operates, not for the redesign-first regime the
 *   engineering reading would install. The label 'Rogers findings' decomposes
 *   into three structurally distinct constraints held by different parties:
 *   this compliance-process reading, an engineering absolute-threshold
 *   reading (cease flight until O-ring redesign certified), and an actuarial
 *   risk-acceptance reading (fly if failure probability is documented and
 *   accepted by informed decision-makers). Each is a separate story with its
 *   own epsilon, beneficiaries, and victims; this file links to its siblings
 *   via network.affects_constraints. KEY AGENTS (by structural relationship):
 *   - nasa_program_managers: agenda setter (institutional/constrained) -
 *   administers the process, defines sufficiency, retains launch authority -
 *   shuttle_engineering_workforce: primary target (organized/constrained) -
 *   supplies the record, loses veto power through it -
 *   morton_thiokol_senior_management: secondary beneficiary (powerful/mobile)
 *   - collects liability distribution from the documented-consent record -
 *   astronaut_corps: risk bearer (moderate/identity_locked) - signs
 *   acceptance, bears residual physical risk -
 *   safety_mission_assurance_offices: enforcement administrator with
 *   organizational stake (organized/constrained) -
 *   congressional_oversight_committees and aerospace_safety_advisory_panel:
 *   observers (institutional/analytical) - attest and investigate without
 *   restructuring authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, 0.64).
domain_priors:suppression_score(rogers_commission_findings__management_compliance_narrative, 0.65).
domain_priors:theater_ratio(rogers_commission_findings__management_compliance_narrative, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, extractiveness, 0.64).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rogers_commission_findings__management_compliance_narrative, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rogers_commission_findings__management_compliance_narrative, tangled_rope).
narrative_ontology:human_readable(rogers_commission_findings__management_compliance_narrative, "Post-Challenger Compliance-Process Regime (Management Reading of the Rogers Findings)").
narrative_ontology:topic_domain(rogers_commission_findings__management_compliance_narrative, "organizational_safety/technology_governance/regulatory_compliance").

domain_priors:requires_active_enforcement(rogers_commission_findings__management_compliance_narrative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rogers_commission_findings__management_compliance_narrative, '9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3').
narrative_ontology:cs_kernel_codification('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', fixed_text).
narrative_ontology:cs_authority_grounding('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', extraction).
narrative_ontology:cs_interpretation_layer_present('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3').
narrative_ontology:cs_reading_relation('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', rogers_commission_findings__engineering_absolute_threshold, influences).
narrative_ontology:cs_reading_relation('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', rogers_commission_findings__actuarial_risk_acceptance, coexists_with).
narrative_ontology:cs_axiom('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', foundational, documented_risk_awareness_legitimizes_proceeding).
narrative_ontology:cs_axiom_status(documented_risk_awareness_legitimizes_proceeding, holdable).
narrative_ontology:cs_axiom_grounding('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', documented_risk_awareness_legitimizes_proceeding, conventional).
narrative_ontology:cs_axiom('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', secondary, engineering_voice_advisory_not_binding).
narrative_ontology:cs_axiom_status(engineering_voice_advisory_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', engineering_voice_advisory_not_binding, conventional).
narrative_ontology:cs_reference_frame('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', procedural_documentation_sufficiency).
narrative_ontology:cs_drift_state('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', post_columbia_caib_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9301b4a3-48bf-4ac0-90d5-eb07f5ae72d3', '').
narrative_ontology:cs_kernel_id(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, nasa_program_managers).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, morton_thiokol_senior_management).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, shuttle_engineering_workforce).
narrative_ontology:constraint_victim(rogers_commission_findings__management_compliance_narrative, astronaut_corps).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rogers_commission_findings__management_compliance_narrative, safety_mission_assurance_offices).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, documented_risk_awareness_sufficiency).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, informed_consent_launch_doctrine).
narrative_ontology:constraint_vindicates(rogers_commission_findings__management_compliance_narrative, management_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the Flight Readiness Review and define what counts as sufficient documented risk awareness and mitigation. Retain final launch authority, now exercised with a signed rationale trail. Careers and program funding depend on launch cadence, so the process they interpret is the process they are judged by. Exit means leaving the agency or the program line.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, nasa_program_managers, agenda_setter,
    institutional, generational, constrained, national).

% Produce the hazard analyses, anomaly dispositions, and dissent memos that populate the compliance record. Their formal recommendation power ends at documentation: a recorded objection becomes part of the file that justifies proceeding rather than a lever that stops it. Leaving the program or the industry is possible but costly mid-career.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, shuttle_engineering_workforce, payer,
    organized, biographical, constrained, national).

% Contractor senior leadership gains a documented informed-consent record that distributes legal and reputational exposure across the government-contractor interface. Schedule accommodation under customer pressure is easier when the record shows risks were disclosed and accepted. Settlement and litigation costs after failures are borne separately from the day-to-day benefit of the paper shield.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, morton_thiokol_senior_management, beneficiary,
    powerful, biographical, mobile, national).

% Sign risk-acceptance instruments acknowledging documented hazards before each flight. Bear the residual physical risk that documentation prices but does not remove. Corps membership is a vocational identity formed over a decade of selection and training; exit means abandoning the profession, not changing employers. Some members sit on review boards, which gives voice inside the process but not stop authority.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, astronaut_corps, payer,
    moderate, biographical, identity_locked, national).

% Stand up and staff the documentation, tracking, and waiver-approval machinery after the Rogers findings. Enforce completeness of the risk record and certify that mitigation actions are closed. The compliance apparatus is their organizational turf and budget justification; they enforce the process without setting launch policy.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, safety_mission_assurance_offices, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rogers_commission_findings__management_compliance_narrative, safety_mission_assurance_offices, beneficiary).

% Fund the program, hold hearings after anomalies, and receive testimony from all other seats. Can compel documents and personnel but has historically declined to restructure launch authority away from program management, treating the compliance record itself as evidence of responsible governance.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, congressional_oversight_committees, observer,
    institutional, generational, analytical, national).

% Independent statutory advisory body reviewing program safety annually. Repeatedly flags hazard-acceptance trends and documentation practices in public reports. Holds no enforcement power; its leverage is publicity and the credibility of outside attestation.
narrative_ontology:constraint_stakeholder(rogers_commission_findings__management_compliance_narrative, aerospace_safety_advisory_panel, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rogers_commission_findings__management_compliance_narrative, nasa_program_managers).
narrative_ontology:fixing_cost_class(rogers_commission_findings__management_compliance_narrative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes hazard documentation across centers and contractors so that multi-organizational launch decisions rest on a single shared record of known risks and mitigation status; creates institutional memory that survives personnel turnover and distributes explicit accountability for what was known at decision time.
% TRANSFER_FUNCTION: Moves effective go/no-go authority from engineering judgment to management sign-off backed by the documented record; moves residual operational risk onto flight crews and future flights via accepted-risk instruments; moves individual legal exposure into a distributed paper trail shared across the government-contractor interface.
% ABSENT_VOICES: No independent certification authority with unilateral stop power exists for human spaceflight; crew representatives participate inside the process they are subject to, compromising independence; engineers holding the position that flight must cease until redesign is certified have procedural room only to enter their view into the record, not to act on it.
% DISAPPEARANCE_RATIONALE: If the compliance process vanished overnight, launch decisions would revert to informal engineering-management negotiation with no shared record, accountability would re-personalize around whoever spoke loudest in the room, contractor liability arrangements built on documented consent would unravel, and oversight bodies would lose the artifact they currently treat as evidence of responsible governance.
% FOUNDING_PROBLEM: The Rogers Commission found that critical O-ring vulnerability knowledge existed in the engineering workforce but never reached senior decision-makers with force sufficient to stop the launch: an information failure in which known risk was normalized, filtered, and overridden under schedule pressure.
% FOUNDING_PROBLEM_CORROBORATION: Program management attests the process keeps risk visibility live. The Columbia Accident Investigation Board, sitting entirely outside the benefiting parties, found the same information-suppression pattern recurring in 2003 (denied imagery requests, waived debris criteria) inside a fully compliant documentation regime; the Aerospace Safety Advisory Panel's successive annual reports corroborate persistent normalization; Vaughan's organizational analysis independently documents the mechanism the process does not reach. External attestation supports the reading that the founding problem's underlying mechanism survives the process built for it.
narrative_ontology:disappearance_verdict(rogers_commission_findings__management_compliance_narrative, world_rearranges).
narrative_ontology:founding_problem_status(rogers_commission_findings__management_compliance_narrative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rogers_commission_findings__management_compliance_narrative, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rogers_commission_findings__management_compliance_narrative, 'none', 1).
narrative_ontology:epsilon_provenance(rogers_commission_findings__management_compliance_narrative, 0.64, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rogers_commission_findings__management_compliance_narrative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rogers_commission_findings__management_compliance_narrative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rogers_commission_findings__management_compliance_narrative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is substantial (0.64 at interval end) because the process converts engineering stop-authority into a disclosure duty: the same document that records an objection becomes the artifact that legitimizes proceeding despite it. Suppression (0.65) reflects the continuous enforcement load of maintaining the sufficiency doctrine against engineering pushback - review-board gatekeeping, waiver hierarchies, and cultural pressure on dissenters - not physical coercion; suppression is authored as a raw structural property and is not scaled by scope or power in the engine's computation. Theater ratio (0.62) is high because a growing share of the apparatus's output functions as liability protection and governance performance rather than as risk elimination: the record grew steadily while the underlying accept-risk-then-fly pattern persisted, as the Columbia investigation documented. Accessibility collapse is low (0.40) because alternatives remain live positions - the absolute-threshold reading persists among engineers, external escalation channels exist, and successor programs adopted stricter certification - so understanding the constraint does not foreclose acting otherwise. Resistance (0.55) is sustained: recurring engineering dissent, external investigation findings, and decades of advisory-panel criticism. Claim and metrics are independent authored facts: tangled_rope is claimed from the structure (genuine documentation coordination plus asymmetric veto extraction under active enforcement); the metric values describe observed operation. All three tracked series run on one shared six-point grid so no metric row borrows another's end-state values.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement computes as governance: a rational process the office built, staffs, and answers for, whose paper trail protects everyone including its critics. From the engineering-payer seat the identical structure operates as expropriation: expertise is harvested into a record that then authorizes overriding the experts. Observers see a third profile - rising theatrical maintenance around a fading original program context. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Program managers derive near the beneficiary pole: they collect launch continuity and personal liability shielding and control the sufficiency standard. Contractor senior management likewise sits near the beneficiary pole via the documented-consent record. The engineering workforce derives near the target pole: it supplies the extracted good (veto power converted to documentation) with constrained exit. The astronaut corps carries an explicit override: their signed risk-acceptance instruments make a naive derivation read formal consent and pull their directionality toward symmetry, but structurally they bear undiluted residual physical risk with no alternative vehicle and identity-locked exit, so the override places them near the full-target end (d = 0.78). Safety and mission assurance offices are dual-positioned: they enforce the process (agenda-setter surface) while drawing organizational turf from it (beneficiary surface), landing them mildly below symmetric.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - information failing to reach decision-makers with stopping force - is contested rather than dead: the phenomenon recurred under full compliance (Columbia), yet risk-documentation infrastructure remains genuinely load-bearing for multi-contractor coordination, so the mandate has not simply outlived its function. The tangled_rope classification is what prevents mislabeling in both directions: reading the regime as pure rope (management's framing) hides the veto extraction riding on real coordination; reading it as pure snare (the bitterest engineering framing) erases the genuine shared-record function that would survive any reform. The rising theater series is the drift signal to watch: if the original program context fades completely and documentation-to-decision causation lapses entirely, the structure degrades toward piton - maintained by inertia and liability habit rather than by anyone's concentrated gain. The founding-problem status is authored contested, so no automatic zombie flag fires; the mismatch consumer should cross-check the theater trajectory against the computed piton path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_underdetermination,
    'Is the operative meaning of the Rogers findings the compliance-process reading instantiated here, or one of the sibling readings (engineering absolute threshold, actuarial risk acceptance)?',
    'Historical implementation evidence already weighs heavily: the regime NASA actually built after 1986 is a documentation-and-review process, not a redesign gate or a quantified-probability rule. Residual resolution would come from doctrinal statements in successor-program certification standards and from which reading courts and Congress cite when adjudicating launch-authority disputes.',
    'Each sibling changes the victim set and epsilon substantially: the absolute-threshold reading makes schedule and program continuity the payer and engineers the beneficiary; the actuarial reading makes whoever bears the accepted probability the victim and quantification the coordination function. Classification of this story is conditional on the compliance reading being the live institutional one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Which reading of the shared Rogers-findings kernel governs; this story instantiates the management compliance narrative.').

omega_variable(
    sufficiency_self_certification_loop,
    'Who decides what quantity and quality of documented risk awareness is ''sufficient to proceed'', and is that decision structurally independent of the party wanting to launch?',
    'Compare waiver-approval rates and sufficiency determinations across program administrations and across periods of schedule pressure versus slack; an approval rate insensitive to circumstances indicates a genuine standard, while pressure-correlated approval indicates self-certification.',
    'If sufficiency is effectively self-certified by the launch-authority holders, the extraction component dominates the coordination component and the structure drifts from tangled_rope toward snare; a demonstrably independent standard would support the coordination-heavy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sufficiency_self_certification_loop, empirical, 'Whether the sufficiency standard is set independently of the party it constrains.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression sustaining engineering acquiescence structural (career dependency, hierarchical gatekeeping of the record) or internalized (mission identification making objection feel like disloyalty)?',
    'Post-exit trajectory comparison: engineers who left the program and speak freely about the same hazards versus those who stayed and softened their assessments; survey and oral-history data on stated reasons for not escalating.',
    'If a substantial share is internalized, effective suppression exceeds the structural measure - the constraint travels with the workforce after any procedural reform, and removing the gatekeeping would not restore veto exercise; purely structural suppression would respond to institutional redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of the suppression that maintains the compliance doctrine against engineering dissent.').

omega_variable(
    documentation_function_liveness,
    'Does the compliance record still causally influence launch outcomes - has any documented engineering objection or open item ever halted or delayed a flight - or is the apparatus now maintained for liability and legitimacy performance?',
    'Audit the documentation-to-decision causal chain across the flight history: enumerate cases where record contents changed a go/no-go outcome, and measure the fraction of review activity whose outputs feed any decision versus exist solely for the file.',
    'A near-zero causal hit rate with high maintenance cost confirms heavy theatrical maintenance and pushes the classification toward piton as the original program context fades; a live causal chain anchors the coordination half of the tangled_rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(documentation_function_liveness, empirical, 'Whether the documentation apparatus retains decision-causal function or has become liability theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rogers_commission_findings__management_compliance_narrative, 0, 38).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t0, rogers_commission_findings__management_compliance_narrative, theater_ratio, 0, 0.3).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t0, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t8, rogers_commission_findings__management_compliance_narrative, theater_ratio, 8, 0.4).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t8, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t16, rogers_commission_findings__management_compliance_narrative, theater_ratio, 16, 0.5).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t16, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t24, rogers_commission_findings__management_compliance_narrative, theater_ratio, 24, 0.56).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t24, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t32, rogers_commission_findings__management_compliance_narrative, theater_ratio, 32, 0.6).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t32, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_tr_t38, rogers_commission_findings__management_compliance_narrative, theater_ratio, 38, 0.62).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_tr_t38, observed).

% Extraction over time
narrative_ontology:measurement(rogers_mgmt_narrative_be_t0, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t0, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_be_t8, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 8, 0.52).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t8, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_be_t16, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 16, 0.58).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t16, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_be_t24, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t24, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_be_t32, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 32, 0.63).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t32, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_be_t38, rogers_commission_findings__management_compliance_narrative, base_extractiveness, 38, 0.64).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_be_t38, observed).

% Suppression requirement over time
narrative_ontology:measurement(rogers_mgmt_narrative_su_t0, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t0, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_su_t8, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t8, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_su_t16, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 16, 0.59).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t16, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_su_t24, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 24, 0.62).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t24, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_su_t32, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 32, 0.64).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t32, observed).
narrative_ontology:measurement(rogers_mgmt_narrative_su_t38, rogers_commission_findings__management_compliance_narrative, suppression_requirement, 38, 0.65).
narrative_ontology:measurement_basis(rogers_mgmt_narrative_su_t38, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rogers_commission_findings__management_compliance_narrative, enforcement_mechanism).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__engineering_absolute_threshold).
narrative_ontology:affects_constraint(rogers_commission_findings__management_compliance_narrative, rogers_commission_findings__actuarial_risk_acceptance).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Rogers Commission findings'. The label conflates three structurally distinct claims about what the findings demand: (1) this compliance-process reading - documented awareness and mitigation suffice to proceed, with program continuity as beneficiary and engineering veto power as victim; (2) an engineering absolute-threshold reading - flight ceases until redesign is certified, a near-mountain technical boundary with negligible extraction; (3) an actuarial risk-acceptance reading - flight proceeds when failure probability is quantified and accepted by informed decision-makers, transferring risk onto whoever flies. The readings have different epsilons, different victim sets, and different failure modes, so each is authored as a separate story per the epsilon-invariance principle. This reading sits downstream of the others in institutional practice: the compliance apparatus generates exactly the documentary substrate the actuarial reading consumes, and its satisfaction of documentation duties drains urgency from the absolute-threshold demand without resolving it. Family members are linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rogers_commission_findings__management_compliance_narrative, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
