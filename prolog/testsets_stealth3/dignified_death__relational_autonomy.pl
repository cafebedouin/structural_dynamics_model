% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__relational_autonomy, []).

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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational-Autonomy Triad for End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   In contemporary hospital end-of-life care, decision authority over
 *   treatment intensity and timing of death is deliberately distributed
 *   across a patient-family-clinician triad operating under procedural
 *   safeguards: structured family meetings, documented consensus, and
 *   ethics-committee consultation when consensus fails. This story
 *   instantiates the relational_autonomy reading of the contested
 *   dignified_death kernel — dignity understood as emerging from relational
 *   context rather than residing in self-determination alone
 *   (autonomy_primary) or in life's intrinsic sanctity (sanctity_primary).
 *   Per the epsilon-invariance discipline, the sibling readings are separate
 *   constraints (linked via network.affects_constraints), not hedges folded
 *   into this one; this story carries one stable epsilon over one referent,
 *   the standing triad arrangement. The manifest hypothesized rope; analysis
 *   found the declared victim set plus active enforcement satisfy the hybrid
 *   structure, so the claim is authored as tangled_rope — the refinement is
 *   the finding, not a tuning. The claim and the metrics are independent
 *   authored facts: the engine computes per-seat classifications from the
 *   structural data, and divergence between claim and computation is signal,
 *   not error.
 *
 * KEY AGENTS:
 *   - capacitated_patients_with_directives: primary target (powerless/trapped) — bears authority dilution through surrogate reinterpretation
 *   - unbefriended_end_of_life_patients: excluded target (powerless/trapped) — outside the relational network the process presumes
 *   - dissenting_family_members: intra-family target (moderate/identity_locked) — present in meetings but procedurally weightless
 *   - designated_family_surrogates: primary beneficiary (moderate/identity_locked) — granted interpretive authority over the patient's wishes
 *   - attending_clinicians: beneficiary (powerful/mobile) — shared responsibility and documented legal cover
 *   - hospital_ethics_committees: agenda_setter (institutional/constrained) — writes and administers the procedural rules
 *   - healthcare_institutions: beneficiary (institutional/arbitrage) — converts disputes into administrable, litigation-resistant records
 *   - disability_rights_organizations: excluded challenger (organized/constrained)
 *   - religious_sanctity_communities: excluded challenger (organized/identity_locked stance)
 *   - national_bioethics_commissions: analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.42).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.44).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.42).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, tangled_rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational-Autonomy Triad for End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, '8d3890a4-ed13-4909-97d3-99de0a5f34dc').
narrative_ontology:cs_kernel_codification('8d3890a4-ed13-4909-97d3-99de0a5f34dc', distributed).
narrative_ontology:cs_authority_grounding('8d3890a4-ed13-4909-97d3-99de0a5f34dc', distributed).
narrative_ontology:cs_reading_relation('8d3890a4-ed13-4909-97d3-99de0a5f34dc', dignified_death__autonomy_primary, influences).
narrative_ontology:cs_reading_relation('8d3890a4-ed13-4909-97d3-99de0a5f34dc', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('8d3890a4-ed13-4909-97d3-99de0a5f34dc', foundational, dignity_constituted_by_relational_context).
narrative_ontology:cs_axiom_status(dignity_constituted_by_relational_context, holdable).
narrative_ontology:cs_axiom_grounding('8d3890a4-ed13-4909-97d3-99de0a5f34dc', dignity_constituted_by_relational_context, deontological).
narrative_ontology:cs_axiom('8d3890a4-ed13-4909-97d3-99de0a5f34dc', foundational, authority_legitimately_distributed_across_triad).
narrative_ontology:cs_axiom_status(authority_legitimately_distributed_across_triad, holdable).
narrative_ontology:cs_axiom_grounding('8d3890a4-ed13-4909-97d3-99de0a5f34dc', authority_legitimately_distributed_across_triad, instrumental).
narrative_ontology:cs_reference_frame('8d3890a4-ed13-4909-97d3-99de0a5f34dc', relational_dignity_shared_authority).
narrative_ontology:cs_drift_state('8d3890a4-ed13-4909-97d3-99de0a5f34dc', contemporary_liability_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8d3890a4-ed13-4909-97d3-99de0a5f34dc', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, designated_family_surrogates).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, attending_clinicians).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, healthcare_institutions).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, capacitated_patients_with_directives).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, unbefriended_end_of_life_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, dissenting_family_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, capacitated_patients_with_directives).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_dignity_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with decision-making capacity and explicit advance directives at the end of life. They state their wishes directly, watch those statements enter family meetings alongside surrogate reinterpretations and clinical recommendations, and cannot compel literal adherence. They cannot leave the hospital system late in illness; their practical recourses are repetition, persuasion, and complaint channels. The procedural safeguards also shield them: unilateral clinician withdrawal of treatment and arbitrary family override both have to pass through steps where their stated word is on record.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, capacitated_patients_with_directives, payer,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, capacitated_patients_with_directives, beneficiary).

% Dying patients with no identifiable family or representative. The deliberative structure presumes a relational network that does not exist for them; decisions proceed through guardianship appointments or institutional defaults, often after long delays. They have no one in the room and no channel into it.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, unbefriended_end_of_life_patients, payer,
    powerless, immediate, trapped, national).

% Family members — often adult children from prior marriages, or siblings — whose read of the patient's wishes conflicts with the designated surrogate or the clinical consensus. They attend the meetings, are heard, and carry no procedural weight against the surrogate's authority. Walking away would mean abandoning a dying parent or sibling, which for most is not a real option.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, dissenting_family_members, payer,
    moderate, biographical, identity_locked, national).

% Spouses and adult children formally designated to speak for the patient. The process hands them interpretive authority over the patient's expressed wishes, a formal seat at every decision point, and the standing to convene clinicians. They carry the emotional weight of the choice and live afterward with its memory. Stepping outside the process would mean abandoning the person they speak for.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, designated_family_surrogates, beneficiary,
    moderate, biographical, identity_locked, national).

% Physicians and nurses who present prognoses, lay out options, and facilitate consensus. Shared, documented decision-making distributes responsibility for irreversible choices that would otherwise rest on a single signature, and the resulting record is what malpractice defense and regulatory review read. Individually they can change employers; mid-case they cannot change the process.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, attending_clinicians, beneficiary,
    powerful, biographical, mobile, national).

% Multidisciplinary committees convened when the triad cannot reach consensus. They adjudicate between directives, surrogate judgments, and clinical recommendations; they write the policies the meetings run on; and their recommendations effectively settle disputes short of litigation. They administer the arrangement and are the seat that could rewrite it.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter,
    institutional, generational, constrained, national).

% Hospitals and health systems operating the process at scale. Documented shared decisions convert potentially litigious standoffs into administrable events, reduce malpractice exposure, and keep beds moving. They shape policy through accreditation requirements and lobbying, and can relocate or redesign services in ways bedside participants cannot.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, healthcare_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% Advocacy organizations arguing that any structure which lets others reinterpret a disabled person's expressed wishes endangers disabled people, whose directives are routinely second-guessed. They testify, litigate, and campaign from outside the bedside process; no seat in it is open to them.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, disability_rights_organizations, excluded,
    organized, generational, constrained, national).

% Faith communities holding that intentional life-termination violates a moral law irrespective of consent. They contest the entire deliberative frame from outside — through chaplaincy presence, institutional affiliation, and public advocacy — while maintaining parallel care structures governed by their own directives, which lets them decline the process without leaving the healthcare system.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, religious_sanctity_communities, excluded,
    organized, generational, identity_locked, national).

% Governmental and scholarly commissions that study end-of-life decision frameworks, issue reports, and recommend statutory adjustments. They take testimony from every seat, commission empirical work, and hold no operational stake in any single case.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, national_bioethics_commissions, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, healthcare_institutions).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Pools three kinds of knowledge needed for irreversible end-of-life decisions — clinical prognosis, biographical knowledge of the patient's values held by family, and the capacitated patient's first-person statements — and routes conflicts through documented, reviewable procedures (structured family meetings, ethics consultation) so that no single party's error or interest silently determines the outcome.
% TRANSFER_FUNCTION: Moves decision authority over treatment intensity and timing of death out of exclusive patient hands and out of unilateral clinician judgment into a shared documented process; confers interpretive authority over the patient's expressed wishes on designated surrogates; assigns dispute-resolution authority to ethics committees; and relocates legal responsibility from individual clinicians onto the collective record.
% ABSENT_VOICES: Unbefriended dying patients have no relational seat at all and enter only through guardianship defaults. Disability-rights organizations and autonomy-committed bioethicists would object that the process systematically discounts capacitated patients' explicit directives in favor of surrogate reinterpretation. Sanctity-committed religious communities would object that the entire frame treats the timing of death as a legitimate object of choice. Dissenting family members are physically present in the meetings but carry no procedural weight.
% DISAPPEARANCE_RATIONALE: If the triad-plus-safeguards vanished overnight, hospitals would lose their dispute-resolution channel: conflicts now absorbed by family meetings and ethics consults would spill into litigation and unilateral clinician action, surrogates would lose formal standing, and documented-consensus liability shielding would evaporate. End-of-life care would reorganize around either directive-literalism or revived paternalism within months.
% FOUNDING_PROBLEM: After the Quinlan and Cruzan cases exposed the illegitimacy of purely paternalistic end-of-life decisions, medicine needed a way to legitimate shared decision authority — honoring patient self-determination without stranding incapacitated patients or abandoning families to impossible solitary burdens.
% FOUNDING_PROBLEM_CORROBORATION: State courts (Cruzan, Schiavo litigation), the legislative record behind the Patient Self-Determination Act and successor statutes, and an independent clinical-ethics literature all attest the founding problem and its continuing salience. Hospital administrators also attest it, but the external seats suffice — the genealogy does not rest on the benefiting parties alone.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__relational_autonomy_tests).
:- end_tests(dignified_death__relational_autonomy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): the same meetings that confer standing on surrogates and clinicians dilute the capacitated patient's final authority, and the dilution is structural — the process cannot honor a directive literally without dissolving itself. Suppression (0.44) is structural rather than coercive: a dying patient cannot exit the institution, a family member cannot exit the dying, and dissent is metabolized by escalation to the very committee that administers the rules. Theater (0.31) reflects the growing share of consult-and-document activity that produces records rather than changed decisions. Accessibility_collapse (0.48): directive-literal alternatives survive in statute and portable-order practice but collapse in acute settings where the process is embedded. Resistance (0.52): disability-rights and autonomy-purist challenges are persistent and organized but hold no seat; a patient-dissenting-family-advocacy coalition is conceivable but chronically unrealized because the primary payers die before they can organize. All three measurement series share one time grid (t=0..36, roughly the PSDA era to present); the rising trajectories model extraction accumulation and enforcement hardening as the process institutionalized — no cyclical dynamics are claimed.
 *
 * PERSPECTIVAL GAP:
 *   From the surrogate seat the process is inclusion — finally being asked. From the capacitated patient's seat the same meeting is dilution — being asked and then out-argued. From the committee seat it is due process functioning; from the excluded advocacy seats it is a closed room with minutes. The engine computes these per-seat classifications from power, exit, and role declarations; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (surrogates, clinicians, institutions) drive d toward the beneficiary end; victim declarations (capacitated patients, unbefriended patients, dissenting family) drive d toward the target end, amplified by trapped and identity_locked exits. The patient's dual position — payer primary, beneficiary secondary via the safeguards — is recorded as secondary_role rather than by polluting the structural arrays, following the convention that dual agents appear in the array matching their primary role. No directionality_overrides are authored: overrides key on power atom, so correcting the patient seat would simultaneously distort the other powerless seats (unbefriended patients deserve full-target d), and the derivation from declarations plus exits already captures the patient's position.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope claim prevents two symmetrical mislabels: reading the triad as pure rope (everyone nets out ahead) erases the measurable dilution borne by capacitated patients; reading it as snare (institutions steal death decisions) erases the coordination function that no rival arrangement currently performs. Mandatrophy: the founding problem — legitimating shared authority after paternalism's collapse — remains live, so no sunset is declared and the mandate is not resolved. The theater_ratio series tracks the early drift by which the mandate could atrophy into documentation ritual; the drift_state records that gap as substantial and unacknowledged by the administering seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_locus_of_dignity_contest,
    'This story instantiates the relational_autonomy reading of the dignified_death kernel: how would the sibling readings restructure the same bedside arrangement?',
    'Compare against the sibling stories dignified_death__autonomy_primary and dignified_death__sanctity_primary: autonomy_primary concentrates final authority in the capacitated patient (converting surrogates and clinicians into appropriators and shrinking the victim set to non-patient parties); sanctity_primary removes death-timing choice entirely (dissolving the triad''s object and re-victimizing the suffering patient).',
    'Classification is stable within this reading; cross-reading comparison is the intended consumption path — the same bedside arrangement computes differently from each reading''s seat, and that divergence measures the readings, not the ward.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_locus_of_dignity_contest, conceptual, 'Committer structure: one of three readings of the dignified_death kernel; the disagreement is located in the locus of dignity, which determines who holds decision authority.').

omega_variable(
    epsilon_reading_indexed_referent,
    'Is epsilon (0.42) invariant across readings of the same bedside arrangement, or reading-indexed over a fixed referent?',
    'Per OQ-26 the referent is fixed (the standing triad arrangement under contest) while values are reading-indexed: the autonomy_primary sibling authors the same arrangement at substantially higher epsilon (patient authority appropriated); sanctity_primary authors a different referent altogether (any deliberate life-shortening). Cross-story comparison resolves the apparent inconsistency.',
    'Cross-story epsilon comparisons measure the readings, not the arrangement; only within-reading drift (this story''s measurement series) measures the arrangement itself. Averaging epsilon across sibling readings would be a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_reading_indexed_referent, conceptual, 'Reading-indexed epsilon over a fixed referent; guards against folding sibling readings into one constraint or averaging their values.').

omega_variable(
    surrogate_interpretive_fidelity,
    'How faithfully do designated surrogates and clinicians reconstruct the patient''s actual wishes when reinterpreting directives through the triad process?',
    'Paired preference studies comparing patients'' contemporaneous statements with surrogate predictions and subsequent decisions; the existing surrogate-accuracy literature supplies a baseline and replication pathway.',
    'Low fidelity means the dilution borne by capacitated patients is larger than the safeguards admit and epsilon is understated; high fidelity means the process performs benign interpretation and the payer declaration overstates harm.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surrogate_interpretive_fidelity, empirical, 'Accuracy of surrogate reconstruction of patient wishes — the load-bearing empirical unknown beneath the dilution cost.').

omega_variable(
    unbefriended_patient_pathways,
    'Does the arrangement''s machinery extend to patients with no relational network (via guardianship defaults and institutional ethics consultation), or does it abandon them?',
    'Audit of unbefriended-patient outcomes: guardianship latency, decision delay, and whether ethics committees assume the missing relational seat or defer indefinitely.',
    'If abandoned, the victim set is incomplete and the coordination claim covers fewer parties than declared; if covered, the triad generalizes beyond its presumed network and the payer set contracts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unbefriended_patient_pathways, empirical, 'Coverage of the relational framework for patients outside any relational network.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 36).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dd_relational_autonomy_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.16).
narrative_ontology:measurement(dd_relational_autonomy_tr_t6, dignified_death__relational_autonomy, theater_ratio, 6, 0.19).
narrative_ontology:measurement(dd_relational_autonomy_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.22).
narrative_ontology:measurement(dd_relational_autonomy_tr_t18, dignified_death__relational_autonomy, theater_ratio, 18, 0.24).
narrative_ontology:measurement(dd_relational_autonomy_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.27).
narrative_ontology:measurement(dd_relational_autonomy_tr_t30, dignified_death__relational_autonomy, theater_ratio, 30, 0.29).
narrative_ontology:measurement(dd_relational_autonomy_tr_t36, dignified_death__relational_autonomy, theater_ratio, 36, 0.31).

% Extraction over time
narrative_ontology:measurement(dd_relational_autonomy_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(dd_relational_autonomy_be_t6, dignified_death__relational_autonomy, base_extractiveness, 6, 0.29).
narrative_ontology:measurement(dd_relational_autonomy_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(dd_relational_autonomy_be_t18, dignified_death__relational_autonomy, base_extractiveness, 18, 0.34).
narrative_ontology:measurement(dd_relational_autonomy_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(dd_relational_autonomy_be_t30, dignified_death__relational_autonomy, base_extractiveness, 30, 0.4).
narrative_ontology:measurement(dd_relational_autonomy_be_t36, dignified_death__relational_autonomy, base_extractiveness, 36, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(dd_relational_autonomy_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(dd_relational_autonomy_su_t6, dignified_death__relational_autonomy, suppression_requirement, 6, 0.33).
narrative_ontology:measurement(dd_relational_autonomy_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.35).
narrative_ontology:measurement(dd_relational_autonomy_su_t18, dignified_death__relational_autonomy, suppression_requirement, 18, 0.38).
narrative_ontology:measurement(dd_relational_autonomy_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.4).
narrative_ontology:measurement(dd_relational_autonomy_su_t30, dignified_death__relational_autonomy, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(dd_relational_autonomy_su_t36, dignified_death__relational_autonomy, suppression_requirement, 36, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).

% DUAL FORMULATION NOTE:
% 'Dignified death' colloquially names one thing but decomposes into three structurally distinct constraints — one per reading of the kernel — because the locus-of-dignity premise determines who holds decision authority and therefore who pays. Each reading carries its own epsilon, victim set, and classification; this story authors the relational_autonomy instance (moderate epsilon, dilution victims, enforced triad). Links run horizontally to both siblings: the readings coexist and influence one another's operating environments rather than forming an evidential upstream/downstream chain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
