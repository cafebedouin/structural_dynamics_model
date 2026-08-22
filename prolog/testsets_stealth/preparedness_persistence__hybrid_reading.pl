% ============================================================================
% CONSTRAINT STORY: preparedness_persistence__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_persistence__hybrid_reading, []).

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
 *   constraint_id: preparedness_persistence__hybrid_reading
 *   human_readable: Stratified Preparedness Apparatus: Competent Inspection, Ceremonial Drills
 *   domain: institutional/safety-governance
 *
 * SUMMARY:
 *   A statutory preparedness apparatus — mandated structural inspections and
 *   scheduled emergency drills — has stratified. Engineering inspection
 *   remains a working function: licensed engineers find real defects under
 *   standards their profession enforces, and failures remain attributable to
 *   named stamps. The evacuation drill has drifted toward ceremony:
 *   participation is mandatory and documented, but the hours produce
 *   signatures rather than assessed capability, and the documentation is what
 *   boards, insurers, and regulators actually consume. The two components are
 *   administered by the same compliance machinery, funded from the same
 *   budgets, and reported in the same assurance statements — which is how the
 *   ritualized half borrows the credibility of the working half. KEY AGENTS
 *   (by structural relationship): institutional_administrators
 *   (institutional/mobile) — schedule and sign the apparatus, collect its
 *   liability shield; oversight_regulators (institutional/constrained) —
 *   mandate and receive the record; engineering_inspection_bodies
 *   (organized/constrained) — operate the working half;
 *   emergency_management_professionals (organized/identity_locked) — design
 *   and run the drills, invested in the form;
 *   frontline_staff_drill_participants (moderate/constrained) — supply the
 *   hours; evacuation_dependent_populations (powerless/trapped) — hold the
 *   assurance and the exposure; safety_researchers (analytical) — measure the
 *   gap from outside.
 *
 * KEY AGENTS:
 *   - institutional_administrators: agenda-setting beneficiary (institutional/mobile) — converts the drill record into liability cover and audit legibility; the seat the drill-side gains accrue to
 *   - oversight_regulators: beneficiary with false-assurance exposure (institutional/constrained) — collects checkable evidence that quietly overstates what it certifies
 *   - engineering_inspection_bodies: beneficiary operating the competent subsystem (organized/constrained) — statutory market and standing tied to attributable, consequential work
 *   - emergency_management_professionals: agenda-setter and payer (organized/identity_locked) — designs and runs the drill form their own credentials are made of
 *   - frontline_staff_drill_participants: primary payer of drill-side extraction (moderate/constrained) — recurring mandatory hours with no assessed yield
 *   - evacuation_dependent_populations: payer of the assurance gap, incidental beneficiary of inspection (powerless/trapped) — holds exposure decided in rooms they are not in
 *   - safety_researchers: analytical observer — measures drill-to-incident transfer and inspection efficacy from outside the compliance conversation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, 0.48).
domain_priors:suppression_score(preparedness_persistence__hybrid_reading, 0.5).
domain_priors:theater_ratio(preparedness_persistence__hybrid_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(preparedness_persistence__hybrid_reading, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_persistence__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_persistence__hybrid_reading, "Stratified Preparedness Apparatus: Competent Inspection, Ceremonial Drills").
narrative_ontology:topic_domain(preparedness_persistence__hybrid_reading, "institutional/safety-governance").

domain_priors:requires_active_enforcement(preparedness_persistence__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_persistence__hybrid_reading, 'c2499432-2086-4441-9f23-f32c2ac6bbec').
narrative_ontology:cs_kernel_codification('c2499432-2086-4441-9f23-f32c2ac6bbec', formalized).
narrative_ontology:cs_authority_grounding('c2499432-2086-4441-9f23-f32c2ac6bbec', expertise).
narrative_ontology:cs_interpretation_layer_present('c2499432-2086-4441-9f23-f32c2ac6bbec').
narrative_ontology:cs_reading_relation('c2499432-2086-4441-9f23-f32c2ac6bbec', preparedness_persistence__husk_reading, forecloses).
narrative_ontology:cs_reading_relation('c2499432-2086-4441-9f23-f32c2ac6bbec', preparedness_persistence__competence_reading, forecloses).
narrative_ontology:cs_axiom('c2499432-2086-4441-9f23-f32c2ac6bbec', foundational, preparedness_decay_is_uneven).
narrative_ontology:cs_axiom_status(preparedness_decay_is_uneven, holdable).
narrative_ontology:cs_axiom_grounding('c2499432-2086-4441-9f23-f32c2ac6bbec', preparedness_decay_is_uneven, empirically_contingent).
narrative_ontology:cs_axiom('c2499432-2086-4441-9f23-f32c2ac6bbec', foundational, drill_form_outlives_drill_function).
narrative_ontology:cs_axiom_status(drill_form_outlives_drill_function, holdable).
narrative_ontology:cs_axiom_grounding('c2499432-2086-4441-9f23-f32c2ac6bbec', drill_form_outlives_drill_function, empirically_contingent).
narrative_ontology:cs_reference_frame('c2499432-2086-4441-9f23-f32c2ac6bbec', stratified_functional_ceremonial_portfolio).
narrative_ontology:cs_drift_state('c2499432-2086-4441-9f23-f32c2ac6bbec', contemporary_compliance_formalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2499432-2086-4441-9f23-f32c2ac6bbec', '').
narrative_ontology:cs_kernel_id(preparedness_persistence__hybrid_reading, preparedness_persistence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, oversight_regulators).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, engineering_inspection_bodies).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, frontline_staff_drill_participants).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, evacuation_dependent_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_persistence__hybrid_reading, evacuation_dependent_populations).
narrative_ontology:constraint_victim(preparedness_persistence__hybrid_reading, emergency_management_professionals).
narrative_ontology:constraint_vindicates(preparedness_persistence__hybrid_reading, uneven_institutional_decay_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Schedule, fund, and document the organization's preparedness activities: they set the drill calendar to satisfy audit and insurer requirements, sign the after-action reports, and hold the compliance record. The documented drill count is what they present to boards, regulators, and courts. Their personal tenure typically outlasts any single drill cycle, and they can rotate to another institution without the record following them.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, institutional_administrators, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, institutional_administrators, beneficiary).

% Mandate drill frequency and inspection intervals, then receive the resulting documentation as evidence of compliance. The paperwork is checkable in a way that actual readiness is not, so the record they collect is legible even where it is thin. They also carry the exposure of having accepted a record that real events may not confirm; relaxing a mandate after a failure is career-threatening, so mandates tend to ratchet rather than relax.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, oversight_regulators, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, oversight_regulators, agenda_setter).

% Licensed engineers and inspection firms carry out statutory structural inspections under technical standards their profession writes and polices. Failures are attributable — a missed defect traces to a named stamp — so the work retains consequence and the standards stay tied to physical outcomes. The statutory mandate gives them a protected market and professional standing; leaving for other engineering work is possible but forfeits the credential's premium.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, engineering_inspection_bodies, beneficiary,
    organized, generational, constrained, national).

% Design and run the exercises: they build the scenarios, write the objectives, and evaluate performance against the exercise plan. Many entered the field through the drill tradition, and their competence is credentialed in exercise design; the drill form is what their training, certifications, and professional network are made of. Redesigning drills toward competency assessment would devalue their own accumulated expertise, so the form they administer is also the form they belong to.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, emergency_management_professionals, agenda_setter,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, emergency_management_professionals, payer).

% Are required to attend scheduled drills, follow the scripted steps, and sign the participation rosters. The hours are real and recurring; what the hours build is rarely assessed afterward. They cannot opt out without employment consequences, and their channel for challenging drill design runs through the same offices that schedule the drills.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, frontline_staff_drill_participants, payer,
    moderate, biographical, constrained, national).

% Live and work in the buildings and districts the apparatus covers. They benefit where inspection catches a structural defect before it fails; they carry the gap where documented drill performance overstates how an actual evacuation would run. They do not see the drill record, cannot audit it, and cannot relocate away from the risk the record describes; their exposure is decided in rooms they are not in.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, evacuation_dependent_populations, payer,
    powerless, generational, trapped, local).
narrative_ontology:stakeholder_secondary_role(preparedness_persistence__hybrid_reading, evacuation_dependent_populations, beneficiary).

% Study the gap between exercised performance and incident performance across organizations and jurisdictions. They publish on drill transfer, inspection efficacy, and compliance formalization; they hold no seat in the scheduling or mandate process, and their findings reach practitioners mainly through the same professional bodies whose practices they evaluate.
narrative_ontology:constraint_stakeholder(preparedness_persistence__hybrid_reading, safety_researchers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_persistence__hybrid_reading, institutional_administrators).
narrative_ontology:fixing_cost_class(preparedness_persistence__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves two coordination problems with one apparatus: statutory inspection pools specialized engineering judgment against structural degradation that individual occupants cannot observe and owners have weak incentives to find; the drill program coordinates organizational response behavior and keeps emergency roles assigned and rehearsed between events. The inspection side still performs the first function; whether the drill side still performs the second is the contested part of this reading.
% TRANSFER_FUNCTION: Moves staff hours and budget into scheduled preparedness activity; moves signed drill rosters, after-action reports, and inspection certificates from institutions to regulators, insurers, and courts; moves assurance to the public — concrete on the inspection side, largely documentary on the drill side. On the drill side the net movement is staff time into paperwork whose readiness yield is the open question.
% ABSENT_VOICES: Evacuation-dependent populations have no seat: the drill record that describes their safety is not shown to them, and they have no standing to challenge drill design. Frontline staff experience the hours but review the drills only through the offices that schedule them. Survivors of incidents where documented readiness failed are consulted after the fact, in inquiries, not before the next drill cycle is set. Safety-science findings on drill transfer reach the scheduling offices filtered through the professional bodies being evaluated.
% DISAPPEARANCE_RATIONALE: If the whole apparatus vanished overnight, the inspection half would be genuinely missed within years: unmonitored dams, facades, and fire systems would accumulate uncaught defects, and the professional market built on statutory inspection would collapse. The drill half would leave mostly a paperwork gap — calendars would clear, exercise vendors would lose a line of business, and documented assurance would disappear, but actual evacuation competence, on this reading, would change little because it was already thin. The asymmetry is the reading's signature: one half rearranges the world, the other half rearranges the filing.
% FOUNDING_PROBLEM: A run of industrial and civic disasters — factory fires with locked exits, dam collapses, hotel fires — produced a reform settlement: structures must be inspected by qualified engineers on statutory intervals, and organizations must rehearse emergency response so that panic and role confusion do not multiply the harm. The founding problem was twofold: invisible physical degradation, and organizational amnesia between rare events.
% FOUNDING_PROBLEM_CORROBORATION: Post-incident investigation reports and engineering failure forensics — sources outside the benefiting parties — attest the inspection-side problem is live and the inspection response works: inquiries repeatedly document caught-and-repaired defects and, where failures occur, missed inspections. The same inquiries attest the drill-side problem is only partly addressed: evacuation performance in real incidents recurrently falls short of the documented drill record. The inspection findings are not disputed inside the apparatus; the drill-side shortfall is attested by fire and safety investigation literature rather than by the offices that schedule the drills.
narrative_ontology:disappearance_verdict(preparedness_persistence__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_persistence__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_persistence__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_persistence__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_persistence__hybrid_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_persistence__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_persistence__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_persistence__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48) and localized: the drill subsystem converts recurring staff hours and public assurance into a compliance record whose readiness yield is the open question, while the inspection subsystem operates near coordination cost — the stratified structure caps epsilon well below what a uniform-ritual reading would author and well above what a uniform-functional one would. Suppression (0.50) is structural mandate, not violence: mandatory attendance, audit consequence, and a liability ratchet that makes mandate-relaxation career-threatening. Theater (0.58) reflects activity volume: drill hours and their documentation outnumber inspection work and dominate the assurance record. Accessibility collapse is moderate (0.45): competency-based and scenario-based alternatives are visible and partly practiced in advanced organizations, but the compliance framework channels preparation toward countable, documentable forms. Resistance is low (0.28): the ritual is low-salience, its costs are diffuse, and its critics sit outside the scheduling offices. The three measurement series share one six-point grid (1985-2025). Suppression_requirement is tracked because enforcement capacity genuinely changed over the interval — mandate hardening through the 2000s (post-2001 exercise formalization, post-2005 after-action regimes), plateauing by the 2020s; this is an enforcement-infrastructure trajectory, not merely an extraction shift. All points are observed.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same apparatus. From the administrator's chair the arrangement is functioning compliance: calendars met, reports filed, certificates current. From the drill participant's chair it is mandatory hours with no assessed yield. From the regulator's chair it is legible evidence that overstates what it certifies — the regulator collects the record and carries the false-assurance exposure, which the beneficiary declaration alone does not capture; the derived directionality for that seat runs low, and the offset is recorded here rather than as an override, because the override array keys on power atoms and an institutional-atom correction would also touch the administrator seat. Evacuation-dependent populations hold the shortest end: assurance they cannot audit over exposure they cannot exit. Emergency management professionals are split by identity investment — they administer the form their careers are made of, so their seat reads the ritual as practice from the inside.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations: administrators collect the liability shield and audit legibility the drill record produces; regulators collect checkable evidence; inspection bodies collect a statutory market and professional standing anchored in attributable work. Victim declarations: frontline staff surrender recurring hours; evacuation-dependent populations carry the gap between documented and actual readiness — powerless and trapped, they sit nearest the full-target end, and their coalition potential is weak because the harmed population is diffuse, unaware of the drill record, and geographically scattered. The inspection subsystem's operation is mostly coordination cost, which damps epsilon below a uniform-ritual reading; the drill subsystem is where extraction concentrates, which keeps epsilon well above the coordination floor. Emergency management professionals carry no beneficiary/victim declaration; their seat is symmetric by situation — they give the form their careers and receive standing from it — and falls to the power-atom fallback rather than an override, since the override granularity would also reach inspection bodies. No directionality_overrides are authored: the derivation from declarations plus exit options captures every seat's primary structure, and the two genuine nuances (regulator false-assurance, professional identity symmetry) are commentary-grade.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding settlement bundled two mandates with different half-lives. The inspection mandate's problem — invisible physical degradation — is live and the response still works, so no whole-constraint obsolescence declaration is authored. The drill mandate's problem — panic and role confusion between rare events — is the contested half: the drill form persists on audit and liability logic after its training yield became an open question. The R5 fields carry that split as founding_problem_status=contested against disappearance_verdict=world_rearranges; the mismatch consumer reads no whole-constraint zombie flag, correctly, because the inspection half genuinely holds the world together — the drill-side atrophy is carried instead by the theater_ratio trajectory, which crosses the 0.5 substitution threshold mid-interval and keeps climbing. The tangled_rope claim is what keeps the two halves from being mislabeled into each other: a uniform-competence reading would launder the drill ritual as readiness maintenance, and a uniform-husk reading would discard the inspection function along with the ritual.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is the hybrid_reading instantiation of kernel preparedness_persistence; what would each sibling reading change structurally? Under preparedness_persistence__husk_reading the drill ritual generalizes to the whole apparatus (epsilon rises toward 0.8, protected populations become victims of the whole, classification trends piton/snare); under preparedness_persistence__competence_reading drill participation maintains readiness (epsilon falls toward 0.2, classification trends rope). The disagreement is located in the per-component function assessment: whether drill participation transfers to real evacuation performance, and whether inspection retains catch efficacy.',
    'Component-level outcome studies: inspection catch rates and defect interception on one side; drill-to-incident performance transfer on the other. Each sibling reading stands or falls on one component''s measured function; the hybrid reading is the residual position if the components genuinely diverge.',
    'Resolving both components fixes epsilon and the type for the kernel as a whole; a uniform result on either component collapses this story into the corresponding sibling constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, empirical, 'Which reading of the preparedness_persistence kernel the component-level evidence supports.').

omega_variable(
    drill_transfer_effectiveness,
    'Does participation in scheduled evacuation drills transfer to performance in actual evacuations, or does the drill form produce documentation and scripted familiarity that decays before an event?',
    'Post-incident evacuation forensics correlated with drill histories; controlled comparison of compliance-drill organizations against scenario-based competency-training organizations on real-incident outcomes.',
    'If transfer is near zero, the drill subsystem is ceremonial and the husk reading is vindicated for it — extraction from staff time and public assurance is higher than authored here; if transfer is real, the drill side sits closer to the competence reading and epsilon falls.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(drill_transfer_effectiveness, empirical, 'Whether the drill component builds capability or only compliance record.').

omega_variable(
    inspection_checklist_migration,
    'Is the documentation logic of the drill side migrating into engineering inspection — checklist completeness displacing engineering judgment as the inspected output?',
    'Longitudinal content analysis of inspection reports (judgment-based findings versus checklist confirmations) and inspector surveys on time pressure and liability-driven box-checking.',
    'If migrating, the competent subsystem shrinks, the apparatus trends toward the husk reading, theater_ratio and epsilon rise beyond authored values, and the moderate D5 risk resolves adverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inspection_checklist_migration, empirical, 'Whether the ritualization frontier is advancing into the competent subsystem.').

omega_variable(
    suppression_mechanism_split,
    'Is drill compliance held in place by structural mandate alone, or also by internalized professional identity among emergency management staff who would defend the drill form even without a mandate?',
    'Natural experiments in jurisdictions that relaxed drill mandates: if scheduled drills persist at mandate-level intensity without enforcement, the internalized component is substantial.',
    'If internalized, suppression outlives the mandate — the constraint''s suppressive force is understated by the structural measure, and exit for emergency management professionals is more identity_locked than the constrained-adjacent picture suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of the apparatus''s suppressive force.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_persistence__hybrid_reading, 1985, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1985, preparedness_persistence__hybrid_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement_basis(prep_tr_t1985, observed).
narrative_ontology:measurement(prep_tr_t1995, preparedness_persistence__hybrid_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement_basis(prep_tr_t1995, observed).
narrative_ontology:measurement(prep_tr_t2005, preparedness_persistence__hybrid_reading, theater_ratio, 2005, 0.5).
narrative_ontology:measurement_basis(prep_tr_t2005, observed).
narrative_ontology:measurement(prep_tr_t2015, preparedness_persistence__hybrid_reading, theater_ratio, 2015, 0.55).
narrative_ontology:measurement_basis(prep_tr_t2015, observed).
narrative_ontology:measurement(prep_tr_t2020, preparedness_persistence__hybrid_reading, theater_ratio, 2020, 0.57).
narrative_ontology:measurement_basis(prep_tr_t2020, observed).
narrative_ontology:measurement(prep_tr_t2025, preparedness_persistence__hybrid_reading, theater_ratio, 2025, 0.58).
narrative_ontology:measurement_basis(prep_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(prep_be_t1985, preparedness_persistence__hybrid_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement_basis(prep_be_t1985, observed).
narrative_ontology:measurement(prep_be_t1995, preparedness_persistence__hybrid_reading, base_extractiveness, 1995, 0.36).
narrative_ontology:measurement_basis(prep_be_t1995, observed).
narrative_ontology:measurement(prep_be_t2005, preparedness_persistence__hybrid_reading, base_extractiveness, 2005, 0.41).
narrative_ontology:measurement_basis(prep_be_t2005, observed).
narrative_ontology:measurement(prep_be_t2015, preparedness_persistence__hybrid_reading, base_extractiveness, 2015, 0.45).
narrative_ontology:measurement_basis(prep_be_t2015, observed).
narrative_ontology:measurement(prep_be_t2020, preparedness_persistence__hybrid_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement_basis(prep_be_t2020, observed).
narrative_ontology:measurement(prep_be_t2025, preparedness_persistence__hybrid_reading, base_extractiveness, 2025, 0.48).
narrative_ontology:measurement_basis(prep_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1985, preparedness_persistence__hybrid_reading, suppression_requirement, 1985, 0.3).
narrative_ontology:measurement_basis(prep_su_t1985, observed).
narrative_ontology:measurement(prep_su_t1995, preparedness_persistence__hybrid_reading, suppression_requirement, 1995, 0.34).
narrative_ontology:measurement_basis(prep_su_t1995, observed).
narrative_ontology:measurement(prep_su_t2005, preparedness_persistence__hybrid_reading, suppression_requirement, 2005, 0.44).
narrative_ontology:measurement_basis(prep_su_t2005, observed).
narrative_ontology:measurement(prep_su_t2015, preparedness_persistence__hybrid_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement_basis(prep_su_t2015, observed).
narrative_ontology:measurement(prep_su_t2020, preparedness_persistence__hybrid_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement_basis(prep_su_t2020, observed).
narrative_ontology:measurement(prep_su_t2025, preparedness_persistence__hybrid_reading, suppression_requirement, 2025, 0.5).
narrative_ontology:measurement_basis(prep_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_persistence__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__competence_reading).
narrative_ontology:affects_constraint(preparedness_persistence__hybrid_reading, preparedness_persistence__husk_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'institutional preparedness' covers three structurally distinct claims about one apparatus, decomposed per the epsilon-invariance principle into three readings of a single kernel: competence_reading (all components functional, low epsilon), husk_reading (all components ceremonial, high epsilon), and this hybrid_reading (stratified, moderate epsilon, extraction localized to the drill subsystem). The readings share a referent — the same standing apparatus — and differ only in the per-component function assessment, so each is authored as its own constraint with its own epsilon, stakeholders, and type, linked here as a constraint family. The hybrid reading is downstream of neither sibling: it is the residual position the component-level evidence selects between them, and its epsilon is bounded by the siblings' values on either side.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
