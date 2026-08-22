% ============================================================================
% CONSTRAINT STORY: preparedness_commitment__husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Preparedness as Memorial Performance (Husk Reading)
 *   domain: institutional/disaster_preparedness
 *
 * SUMMARY:
 *   An institution maintains a decades-old cycle of disaster preparedness
 *   drills, checklists, and certification renewals. Under the husk reading,
 *   the routines have calcified into scripted rehearsal: personnel execute a
 *   fixed scenario each cycle, documentation is produced, certification is
 *   renewed, and administrators present this as evidence of readiness. But
 *   the drills no longer vary scenario conditions, no longer stress-test
 *   judgment under novelty, and after-action findings from actual incidents
 *   are not fed back into drill redesign. The form of preparedness — the
 *   calendar, the paperwork, the checklist completion rate — persists and
 *   even intensifies, while the underlying operational competence it was
 *   built to sustain has eroded. The gap surfaces catastrophically only when
 *   a real event departs from the rehearsed script.
 *
 * KEY AGENTS:
 *   - senior_program_administrators: agenda_setter (institutional/constrained) — administer the drill calendar and could redesign it, but bear the cost of disruption and exposure
 *   - compliance_certifying_bodies: beneficiary (institutional/arbitrage) — collect certification revenue from a stable, repeatable inspection product
 *   - frontline_responders: payer (moderate/trapped) — execute scripted drills and discover the gap under real novel-stress conditions
 *   - affected_communities: payer (powerless/trapped) — bear the consequences of competence collapse when it matters most
 *   - institutional_memory_keepers: excluded (powerless/constrained) — hold knowledge of what adaptive training looked like, not consulted
 *   - external_auditors: observer (analytical/analytical) — see the divergence between drill logs and real incident performance, but only episodically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_commitment__husk_reading, 0.62).
domain_priors:suppression_score(preparedness_commitment__husk_reading, 0.58).
domain_priors:theater_ratio(preparedness_commitment__husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, theater_ratio, 0.81).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(preparedness_commitment__husk_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_commitment__husk_reading, piton).
narrative_ontology:human_readable(preparedness_commitment__husk_reading, "Preparedness as Memorial Performance (Husk Reading)").
narrative_ontology:topic_domain(preparedness_commitment__husk_reading, "institutional/disaster_preparedness").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_commitment__husk_reading, 'cd786395-c9d1-44dc-8450-a55d1e8d735a').
narrative_ontology:cs_kernel_codification('cd786395-c9d1-44dc-8450-a55d1e8d735a', formalized).
narrative_ontology:cs_authority_grounding('cd786395-c9d1-44dc-8450-a55d1e8d735a', practice).
narrative_ontology:cs_interpretation_layer_present('cd786395-c9d1-44dc-8450-a55d1e8d735a').
narrative_ontology:cs_reading_relation('cd786395-c9d1-44dc-8450-a55d1e8d735a', preparedness_commitment__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('cd786395-c9d1-44dc-8450-a55d1e8d735a', preparedness_commitment__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('cd786395-c9d1-44dc-8450-a55d1e8d735a', foundational, form_compliance_is_not_operational_evidence).
narrative_ontology:cs_axiom_status(form_compliance_is_not_operational_evidence, holdable).
narrative_ontology:cs_axiom_grounding('cd786395-c9d1-44dc-8450-a55d1e8d735a', form_compliance_is_not_operational_evidence, empirically_contingent).
narrative_ontology:cs_axiom('cd786395-c9d1-44dc-8450-a55d1e8d735a', secondary, memorial_ritual_provides_no_independent_coordination_value).
narrative_ontology:cs_axiom_status(memorial_ritual_provides_no_independent_coordination_value, holdable).
narrative_ontology:cs_axiom_grounding('cd786395-c9d1-44dc-8450-a55d1e8d735a', memorial_ritual_provides_no_independent_coordination_value, empirically_contingent).
narrative_ontology:cs_reference_frame('cd786395-c9d1-44dc-8450-a55d1e8d735a', post_incident_reform_competence_standard).
narrative_ontology:cs_drift_state('cd786395-c9d1-44dc-8450-a55d1e8d735a', contemporary_certification_cycle, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('cd786395-c9d1-44dc-8450-a55d1e8d735a', '').
narrative_ontology:cs_kernel_id(preparedness_commitment__husk_reading, preparedness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, compliance_certifying_bodies).
narrative_ontology:constraint_beneficiary(preparedness_commitment__husk_reading, senior_program_administrators).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, frontline_responders).
narrative_ontology:constraint_victim(preparedness_commitment__husk_reading, affected_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the annual drill calendar, sign off on compliance reports, and present the exercise cycle to oversight boards as evidence the organization remains disaster-ready. They administer the routines and could redesign them toward live adaptive testing, but redesign is costly, disruptive to the calendar, and risks exposing existing gaps to funders — so the checklist form persists.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, senior_program_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Issue certification based on documented completion of drills and checklists rather than measured performance under novel stress. Certification revenue and audit contracts depend on a stable, repeatable inspection product; a shift to messy, unpredictable competence-testing would complicate their own methodology and liability exposure.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, compliance_certifying_bodies, beneficiary,
    institutional, generational, arbitrage, national).

% Execute the scripted drills exactly as choreographed year after year. When a real event departs from the rehearsed scenario, they discover in the field that the routine trained compliance with a script, not judgment under uncertainty. They cannot opt out of the drill regime and bear the operational cost of the gap when it is exposed.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, frontline_responders, payer,
    moderate, immediate, trapped, local).

% Rely on the preparedness system functioning when disaster strikes and have no visibility into whether drills build real capacity or merely satisfy a certification calendar. They bear the consequences when scripted competence collapses against a novel hazard profile — the gap surfaces at the moment they can least absorb it.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, affected_communities, payer,
    powerless, immediate, trapped, local).

% Veteran responders and retired planners who recall earlier, more adaptive versions of the drills and can name what has been lost — improvisation training, scenario variance, honest after-action review. Their institutional knowledge is not solicited in current drill design; the calendar has outlived the people who understood why it existed.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, institutional_memory_keepers, excluded,
    powerless, generational, constrained, national).

% Independent reviewers occasionally commissioned after a real disaster reveals a competence gap. They compare drill logs against actual incident performance and can document the divergence, but their findings enter the system only episodically, after failure, not as ongoing correction.
narrative_ontology:constraint_stakeholder(preparedness_commitment__husk_reading, external_auditors, observer,
    analytical, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(preparedness_commitment__husk_reading, compliance_certifying_bodies).
narrative_ontology:fixing_cost_class(preparedness_commitment__husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under the husk reading, the routines still perform SOME coordination — they synchronize calendars, satisfy funders that due diligence occurred, and give personnel a shared vocabulary and shared artifact (the checklist) to reference. That coordination function is real but has become decoupled from the operational competence it once indexed.
% TRANSFER_FUNCTION: Moves organizational attention, budget, and personnel-hours from adaptive capacity-building toward documentation production; moves reputational cover and certification revenue to administrators and certifying bodies; moves risk exposure onto frontline responders and the communities who depend on them, concentrated at the moment of a novel-stress event.
% ABSENT_VOICES: Institutional memory keepers who could attest to what genuine competence training looked like before it hollowed out are not consulted in current drill design. Frontline responders' after-action feedback about scripted-versus-real divergence is collected but rarely acted upon; it enters files, not redesign.
% DISAPPEARANCE_RATIONALE: Administrators and certifying bodies would say the world rearranges catastrophically — funding, legal liability shields, and public confidence all depend on the certification artifact existing. Frontline responders and outside auditors would say operational readiness itself would barely change, because the actual competence the drills are supposed to produce is already largely absent; only the paperwork and reputational scaffolding would fall away.
% FOUNDING_PROBLEM: Organizations needed a repeatable way to verify and transmit disaster-response competence across staff turnover and generational change, so that readiness did not depend on any single expert's memory.
% FOUNDING_PROBLEM_CORROBORATION: Post-incident external audits (commissioned after real disasters exposed performance gaps) attest that drill completion no longer predicts field performance under novel conditions. Institutional memory keepers, from outside the current administrative apparatus, corroborate that the drills have drifted from adaptive scenario training toward fixed-script rehearsal. No corroboration for continued vitality comes from outside the certifying bodies and administrators who depend on the current format.
narrative_ontology:disappearance_verdict(preparedness_commitment__husk_reading, contested).
narrative_ontology:founding_problem_status(preparedness_commitment__husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_commitment__husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(preparedness_commitment__husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_commitment__husk_reading, 0.62, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises over the interval (0.35 to 0.62) as the checklist form increasingly substitutes for the adaptive capacity it once indexed — a classic Goodhart trajectory where the measured proxy (drill completion) diverges from the target (operational competence). Theater ratio is authored highest of all metrics (0.81 at interval end) because this is precisely a husk-reading claim: form-compliance without adaptive substance is the defining feature of this reading. Suppression (0.58) reflects that the current format is not perpetuated by force, but by institutional path-dependency, funder expectations, and the reputational risk of admitting the gap — a softer but real form of lock-in. Accessibility collapse is moderate (0.5): alternative formats (scenario-variance drills, adaptive stress-testing) are not physically barred, but the organizational incentive structure makes them costly to adopt. Resistance is moderate (0.42): frontline responders and external auditors do push back, especially post-incident, but their pushback is episodic rather than continuous.
 *
 * DIRECTIONALITY LOGIC:
 *   Compliance certifying bodies and senior administrators sit near the beneficiary end: they collect certification revenue and reputational cover from the existing format and have arbitrage-grade or institutionally-protected exit relative to the consequences of a real failure. Frontline responders and affected communities sit near the target end: they are structurally trapped (responders by employment and professional identity, communities by geography and dependency) and bear the operational and mortal cost when scripted competence proves insufficient. Institutional memory keepers occupy an unusual position — excluded rather than extracted-from directly, but epistemically disenfranchised by a system that no longer solicits the knowledge that would correct it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — transmitting disaster-response competence reliably across staff turnover — is authored as dead under this reading: the mechanism built to solve it has been decoupled from the capacity it was meant to sustain, while the mechanism itself (the calendar, the certification cycle) not only persists but intensifies. This is precisely the founding_problem_status=dead + disappearance_verdict=contested pattern the R5 mismatch-check exists to catch: administrators and certifiers experience the arrangement as indispensable (world_rearranges from their seat) while frontline and outside-auditor testimony suggests the operational world would barely notice the paperwork's disappearance — only the certification and liability shield would vanish. Classifying this as piton rather than snare matters: there is no single concentrated beneficiary extracting rents from active coercion; instead diffuse institutional inertia (nobody wants to be the one who admits the readiness program doesn't work) sustains a structure that helps no one very much and could, in principle, be fixed by the very administrators who run it — except that fixing it is reputationally and operationally expensive relative to what any one seat gains from fixing it. That cost asymmetry, not concentrated capture, is the diagnostic signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_preparedness,
    'Is ''preparedness'' as practiced here better read as husk (memorial performance decoupled from competence), competence (live exercised knowledge), or hybrid (layered system where memorial elements stabilize commitment while competence elements maintain function)? These are three distinct constraints sharing one kernel — the same drill calendar, read three ways.',
    'Compare drill scenario variance over time, after-action review incorporation rate, and real-incident performance correlation with drill completion scores. A sustained decline in scenario variance and a growing gap between drill certification and real-incident performance would corroborate the husk reading over the competence or hybrid readings.',
    'If the hybrid reading is more accurate, the memorial elements (calendar, ritual, shared vocabulary) may be doing real stabilizing work that the husk reading dismisses as pure theater, which would lower authored extraction and theater_ratio substantially and could shift classification from piton toward scaffold or rope. If the competence reading is more accurate, ε should be authored far lower across the board — this would be a materially different, and much less extractive, constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_preparedness, conceptual, 'Which of three kernel readings (husk/competence/hybrid) best describes the actual drill-and-certification arrangement.').

omega_variable(
    husk_versus_hybrid_boundary_location,
    'Where precisely does memorial performance end and residual competence begin within the current drill cycle? Is there a rump of genuine adaptive capacity embedded even in the scripted format that the husk reading is undercounting?',
    'Blind field-test: introduce a genuinely novel stress scenario into the next drill cycle without advance notice to participants, and measure whether performance degrades to the level the husk reading predicts (competence collapse) or holds closer to the hybrid reading''s prediction (partial adaptive capacity retained via memorial-stabilized routine).',
    'A near-collapse result strongly corroborates the husk reading and this story''s high theater_ratio and extractiveness. A partial-retention result would suggest the hybrid_reading constraint is a better fit for at least part of the same institutional arrangement, and this story''s ε may be authored too high for the full population of affected responders.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(husk_versus_hybrid_boundary_location, empirical, 'Whether meaningful residual competence survives inside the scripted drill format, bearing on husk vs hybrid classification boundary.').

omega_variable(
    certifying_body_natural_vs_constructed,
    'Is the certifying bodies'' preference for stable, repeatable inspection artifacts a neutral methodological constraint of any auditing profession, or a constructed self-interest that actively resists competence-based redesign because it would complicate their business model?',
    'Examine whether certifying bodies have historically supported or opposed proposals to move toward variable-scenario, harder-to-standardize competence testing; compare fee structures under each model.',
    'If certifying bodies actively lobby against harder-to-audit competence testing, this strengthens the beneficiary classification and supports treating the arrangement''s persistence as partially interest-driven rather than purely inertial, which would push the classification from piton toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certifying_body_natural_vs_constructed, empirical, 'Whether certifying-body preference for standardized drills reflects neutral methodology or self-interested resistance to reform.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_commitment__husk_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t0, preparedness_commitment__husk_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(prep_tr_t4, preparedness_commitment__husk_reading, theater_ratio, 4, 0.51).
narrative_ontology:measurement(prep_tr_t8, preparedness_commitment__husk_reading, theater_ratio, 8, 0.6).
narrative_ontology:measurement(prep_tr_t12, preparedness_commitment__husk_reading, theater_ratio, 12, 0.68).
narrative_ontology:measurement(prep_tr_t16, preparedness_commitment__husk_reading, theater_ratio, 16, 0.73).
narrative_ontology:measurement(prep_tr_t20, preparedness_commitment__husk_reading, theater_ratio, 20, 0.78).
narrative_ontology:measurement(prep_tr_t24, preparedness_commitment__husk_reading, theater_ratio, 24, 0.81).

% Extraction over time
narrative_ontology:measurement(prep_be_t0, preparedness_commitment__husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(prep_be_t4, preparedness_commitment__husk_reading, base_extractiveness, 4, 0.41).
narrative_ontology:measurement(prep_be_t8, preparedness_commitment__husk_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(prep_be_t12, preparedness_commitment__husk_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(prep_be_t16, preparedness_commitment__husk_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(prep_be_t20, preparedness_commitment__husk_reading, base_extractiveness, 20, 0.59).
narrative_ontology:measurement(prep_be_t24, preparedness_commitment__husk_reading, base_extractiveness, 24, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(preparedness_commitment__husk_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_commitment__husk_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(preparedness_commitment__husk_reading, 0.12).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__competence_reading).
narrative_ontology:affects_constraint(preparedness_commitment__husk_reading, preparedness_commitment__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the preparedness_commitment kernel. competence_reading models the same drill-and-certification apparatus as live exercised knowledge with genuine, low-extraction coordination function (near-rope). hybrid_reading models it as a layered system where memorial elements (ritual, shared calendar, shared vocabulary) stabilize institutional commitment while separable competence elements maintain actual operational function (likely tangled_rope or rope depending on the balance). This story (husk_reading) authors the highest ε and theater_ratio of the three, on the premise that the memorial form has fully decoupled from competence. Per the ε-invariance principle, these are three distinct constraints sharing a kernel, not one constraint measured three ways — each carries its own stable ε from its own reading's lights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
