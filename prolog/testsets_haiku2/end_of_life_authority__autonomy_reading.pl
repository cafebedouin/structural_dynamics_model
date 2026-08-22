% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Grounded End-of-Life Authority (Contested Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This is the autonomy-grounded reading of the contested end-of-life
 *   authority kernel. The reading instantiates individual autonomy as the
 *   foundational moral claim: competent patients facing unbearable suffering
 *   possess a right to control the timing and circumstances of their own
 *   death, and this right overrides paternalistic medical and legal
 *   prohibitions. The constraint's operation: regulatory authorities (medical
 *   boards, legislatures, ethics committees) establish criteria (terminal
 *   illness, unbearable suffering, decisional capacity) and procedural
 *   safeguards (waiting periods, multiple physician consultations, witness
 *   requirements), then enforce patient access to end-of-life options meeting
 *   those criteria. Suppression operates in two directions: (1) where
 *   autonomy-based authority is NOT institutionalized, suppression blocks
 *   patients from autonomous choice; (2) where it IS institutionalized,
 *   suppression of religious/sanctity objections occurs — alternative
 *   readings are excluded from the governing framework. The claim/metric
 *   independence is intentional: the constraint is CLAIMED as rope
 *   (coordination + modest extraction) while measurements document
 *   substantial suppression and rising theater (eligibility-expansion debates
 *   increasingly divorced from the founding coordination problem). The
 *   engine's per-seat computation will show divergence: patients benefiting
 *   from autonomy protection experience low/negative extraction; gatekeepers
 *   and restricted populations experience high suppression; sanctity
 *   advocates experience exclusion.
 *
 * KEY AGENTS:
 *   - competent_patients_facing_terminal_suffering: Primary beneficiaries (powerless, immediate horizon, trapped exit — autonomy authority benefits them directly)
 *   - patients_denied_autonomy_choice: Primary victims in restrictive jurisdictions (powerless, immediate horizon, identity-locked exit — suppression prevents autonomous choice)
 *   - bioethics_regulators: Agenda-setters (institutional power, generational horizon, arbitrage exit — they set and enforce criteria)
 *   - religious_and_sanctity_advocates: Excluded from the autonomy-reading framework (organized power, generational horizon, constrained exit — their voices are not invited into deliberation once autonomy reading is institutionalized)
 *   - analytical_observer: Measurement seat (analytical power, generational horizon — tracks whether the autonomy reading is stable or pressured toward expansion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.38).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded End-of-Life Authority (Contested Reading)").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '737daaf2-855b-4eda-8529-5496fcc2a323').
narrative_ontology:cs_kernel_codification('737daaf2-855b-4eda-8529-5496fcc2a323', fixed_text).
narrative_ontology:cs_authority_grounding('737daaf2-855b-4eda-8529-5496fcc2a323', lineage).
narrative_ontology:cs_interpretation_layer_present('737daaf2-855b-4eda-8529-5496fcc2a323').
narrative_ontology:cs_reading_relation('737daaf2-855b-4eda-8529-5496fcc2a323', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('737daaf2-855b-4eda-8529-5496fcc2a323', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('737daaf2-855b-4eda-8529-5496fcc2a323', foundational, individual_autonomy_overrides_paternalism).
narrative_ontology:cs_axiom_status(individual_autonomy_overrides_paternalism, holdable).
narrative_ontology:cs_axiom_grounding('737daaf2-855b-4eda-8529-5496fcc2a323', individual_autonomy_overrides_paternalism, deontological).
narrative_ontology:cs_axiom('737daaf2-855b-4eda-8529-5496fcc2a323', foundational, competence_sufficient_for_authority).
narrative_ontology:cs_axiom_status(competence_sufficient_for_authority, holdable).
narrative_ontology:cs_axiom_grounding('737daaf2-855b-4eda-8529-5496fcc2a323', competence_sufficient_for_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('737daaf2-855b-4eda-8529-5496fcc2a323', individual_autonomy_framework).
narrative_ontology:cs_drift_state('737daaf2-855b-4eda-8529-5496fcc2a323', contemporary_post_legalization_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('737daaf2-855b-4eda-8529-5496fcc2a323', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, competent_patients_facing_terminal_suffering).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_denied_autonomy_choice).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, families_prevented_supporting_choice).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, palliative_care_providers).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, patients_without_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals with capacity to make medical decisions facing terminal illness or unbearable suffering who, under this reading's framework, possess the right to determine the timing and circumstances of their own death. They exercise this right within jurisdictions or institutional contexts that recognize autonomy-based end-of-life authority. The constraint benefits them by validating their self-determination and enabling exit from suffering.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, competent_patients_facing_terminal_suffering, beneficiary,
    powerless, immediate, trapped, local).

% Individuals in jurisdictions or institutions where autonomy-based end-of-life authority is NOT recognized or is severely restricted — where paternalistic medical and legal frameworks prevent patients from controlling the timing/circumstances of death despite capacity and unbearable suffering. They bear the cost of suppression: continuation of suffering against their explicit wishes, loss of autonomy over the final period of life, vulnerability to state/medical coercion to continue living despite their rational choice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_denied_autonomy_choice, payer,
    powerless, immediate, identity_locked, local).

% Family members and caregivers in restrictive jurisdictions who wish to support a loved one's autonomous choice to end unbearable suffering but are legally/institutionally prevented from doing so. They bear the psychological cost of witnessing suffering they could alleviate, legal jeopardy if they assist, and moral injury from being forced to violate their own ethical commitments to respect their loved one's autonomy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, families_prevented_supporting_choice, payer,
    powerless, immediate, constrained, local).

% Medical professionals providing end-of-life care in jurisdictions recognizing autonomy-based authority. The constraint clarifies their role: they coordinate with patients on symptom management and offer all available options, including patient-controlled death where criteria are met. Some benefit from reduced moral ambiguity; others bear stress from implementing a controversial practice.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_providers, beneficiary,
    organized, biographical, constrained, regional).

% Individuals and organizations committed to doctrines of human sanctity and the intrinsic value of life who are structurally excluded from the conversation about end-of-life authority once the autonomy reading is institutionalized. Their voices — arguing that autonomy cannot override the duty to preserve life — are not invited into the deliberation; they are presented with a legal/institutional fait accompli.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, religious_and_sanctity_advocates, excluded,
    organized, generational, constrained, regional).

% Individuals with cognitive impairment, dementia, mental illness, or developmental disability who cannot participate in autonomy-based decision-making frameworks. They are structurally excluded from the autonomy reading's protective scope, leaving them vulnerable to either continued suffering (if protective restrictions apply) or to surrogate decisions on their behalf (if the framework expands to allow substituted judgment).
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_with_diminished_capacity, excluded,
    powerless, immediate, trapped, local).

% Legislatures, medical boards, institutional ethics committees, and courts that establish and enforce the criteria for autonomy-based end-of-life authority. They define what constitutes 'terminal illness,' 'unbearable suffering,' 'competence,' and the procedural safeguards (waiting periods, physician evaluation, witness requirements). They actively enforce the constraint through licensing decisions, criminal law, and institutional policy.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethics_regulators, agenda_setter,
    institutional, generational, arbitrage, regional).

% Individuals in jurisdictions where the autonomy reading is not institutionalized (the global majority), or those with socioeconomic barriers to accessing the procedure even where legal. They bear the cost of non-implementation: continued suffering without recourse, no legal option for self-determination at end of life.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_without_access, payer,
    powerless, immediate, trapped, global).

% Researchers, ethicists, and policy analysts who examine the autonomy reading's structural properties, empirical outcomes, and contradictions with competing readings. They measure drift in eligibility criteria over time, track outcome heterogeneity across jurisdictions, and assess whether the autonomy framing is sustainable or empirically pressured toward expansion.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, bioethics_regulators).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a transparent, procedurally uniform framework for end-of-life decision-making in the face of terminal illness or unbearable suffering, replacing ad hoc, covert practices with formalized patient-physician dialogue, multiple verification steps, and institutional oversight. Coordinates the interests of patients (control), families (participation), medical staff (liability protection), and society (prevention of abuse) around explicit criteria.
% TRANSFER_FUNCTION: Transfers authority over end-of-life decisions from paternalistic medical/state authority to individual competent patients. What moves: the right to determine the timing and circumstances of death. From: institutional gatekeepers (medical boards, legal prohibition, family override). To: the individual facing unbearable suffering.
% ABSENT_VOICES: Patients with diminished decision-making capacity; individuals from religious/sanctity traditions who hold that autonomy cannot override the duty to preserve life; jurisdictions where the autonomy reading has never been institutionalized and where alternative frameworks (sanctity-based, suffering-alleviation-only) are still dominant; future potential beneficiaries if the eligibility criteria expand beyond terminal illness.
% DISAPPEARANCE_RATIONALE: If the autonomy-grounded end-of-life authority vanished overnight, patients currently exercising it would face continued suffering without legal recourse; medical practice would revert to paternalistic frameworks; end-of-life decision-making would become covert and irregular again; institutional oversight and procedural protections would collapse. The world does not remain unchanged because real people's final months are structured around this authority.
% FOUNDING_PROBLEM: Patients facing terminal illness or unbearable suffering were trapped in a system where their own rational choices about death were overridden by medical paternalism, legal prohibition, and religious doctrine — even when competent, even when suffering was extreme, even when they explicitly refused continuation. The founding problem: how to respect individual self-determination in the face of death without abandoning all protective guardrails.
% FOUNDING_PROBLEM_CORROBORATION: Patients' families, palliative care ethics committees, and court testimony from outside the benefiting parties (judges ruling on constitutional grounds, independent medical ethicists, human rights organizations) attest that the founding problem was real and urgent. However, proponents of the sanctity reading and religious advocates dispute the problem's framing — they deny the premise that autonomy should override life-preservation duty, so they challenge the problem's legitimacy rather than its empirical reality.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.38, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end) because the constraint coordinates genuine patient benefit (autonomy, suffering relief) but simultaneously extracts compliance with eligibility/procedural criteria that gatekeepers control. Suppression is high (0.72) because paternalistic restrictions on autonomy persist in most jurisdictions, identity-locking patients to roles as passive recipients of medical decisions. Theater is low-to-moderate (0.28) because the autonomy reading's procedural apparatus is partly functional (genuine deliberation, multiple safeguards) but increasingly shows performative elements (eligibility debates that expand scope without changing the core coordination problem). The measurement series show suppression DECLINING over the interval (0.88 → 0.72) as legalization spreads and paternalistic barriers erode, while extractiveness PLATEAUS after initial rise (0.22 → 0.38 → stable), suggesting gatekeeping stabilizes at moderate intensity rather than intensifying. Theater RISES slightly (0.08 → 0.28) as the constraint's scope expands and legitimation narratives proliferate. This trajectory is consistent with the constraint stabilizing in rope territory after a transition from suppression-dominated snare-like operations to coordinated autonomy-plus-gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary seat and the victim seat should compute to fundamentally different types: a patient in a jurisdiction where autonomy is recognized and death options are available experiences this as rope or even benevolent coordination — suffering ends, autonomy is honored, the constraint enables rather than restricts. The same regulatory framework, viewed from a victim seat (restrictive jurisdiction, or failed competence assessment), operates as snare — suppression of autonomous choice by paternalistic authority, with procedural theater ('we respect autonomy, but competence is lacking') masking denial. The analytical observer sees both simultaneously and measures the divergence as structural fact. This is not indeterminacy; it is multivalent organization — the constraint genuinely produces different types from different seats, and that multiplicity is what asymmetric extraction looks like.
 *
 * DIRECTIONALITY LOGIC:
 *   Patients benefiting from autonomy protection (competent, in jurisdictions with legalization) are at d ≈ 0.25 (beneficiaries; the constraint subsidizes their autonomy). Patients denied autonomy choice (trapped in restrictive jurisdictions or failed competence assessments) are at d ≈ 0.85 (targets; suppression extracts their self-determination). Bioethics regulators are at d ≈ 0.35 (modest asymmetry: they set the rules and extract gatekeeping authority, but they also bear enforcement costs and legitimation burden). Families are at d ≈ 0.70 (substantial targets: prevented from supporting loved one's choice, bearing psychological/legal costs). The divergence between beneficiary and victim seats is the core structural fact: from one position the constraint is liberation; from another it is suppression. This is not a defect in the story — it is the measurement the asymmetry test exists to capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (patients trapped by paternalistic override of their autonomous wishes) is CONTESTED in status. The autonomy reading says the problem is live and growing — more patients face terminal suffering without recourse; the problem is urgent. The sanctity reading says the 'problem' is a false framing — the real problem is the erosion of life-protection duties, and autonomy-based authority makes it worse, not better. The slippery-slope reading says the founding problem TRANSFORMS as the constraint operates: eligibility expands, and the problem shifts from 'how to respect terminal patients' autonomy' to 'how to prevent life-ending from spreading beyond terminal cases to chronic/mental conditions.' These are genuinely different problem claims, not disagreements about facts. The mandatrophy test: does the autonomy reading persist in defending its founding problem, or has function drifted? Evidence suggests moderate drift — early institutionalization focused narrowly on terminal illness and severe suffering; contemporary debates include chronic conditions, mental illness, and assisted suicide for existential suffering. The theater metric (rising to 0.28) reflects this: more conversation about eligibility expansion, less about the founding coordination problem itself. Moderate theater suggests the constraint has not yet reached piton status (purely performative), but it is moving in that direction if expansion continues.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_autonomy_vs_sanctity,
    'Does individual autonomy ground a right that overrides doctrines of human sanctity and the duty to preserve life? Or does sanctity override autonomy in the end-of-life context?',
    'This is a kernel-level conceptual dispute between two incommensurable normative frameworks. No empirical data will resolve it — the frameworks make different foundational claims about what matters morally. Resolution would require either meta-ethical agreement on which framework takes precedence, or political/institutional choice to privilege one framework within a jurisdiction.',
    'If sanctity is held to override autonomy, the constraint should be reclassified as a snare (suppression of autonomy by paternalistic authority). If autonomy is held to override sanctity, this reading stands as rope. If neither framework is privileged (genuine pluralism), the constraint is best analyzed as contested terrain — multiple readings of the same kernel coexist.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_autonomy_vs_sanctity, conceptual, 'Kernel-level foundational normative conflict between autonomy and sanctity doctrines.').

omega_variable(
    eligibility_expansion_mechanism,
    'Will eligibility criteria for autonomy-based end-of-life authority empirically expand beyond competent, terminal cases to include incompetent individuals, chronic (non-terminal) suffering, and mental illness?',
    'Historical/comparative analysis of jurisdictions where the autonomy reading has been institutionalized (Netherlands, Belgium, Canada, Oregon) showing whether eligibility thresholds have narrowed, remained stable, or expanded over time. Interviews with policymakers about the normative logic pulling toward expansion.',
    'If expansion occurs, the constraint exhibits the slippery-slope mechanism identified in the sibling reading — the autonomy framework itself generates pressure toward broader application. This would suggest the autonomy reading is less stable than presented, or that foundational tensions exist between the reading''s core premise and its boundaries. If expansion is resisted and eligibility remains narrow, the reading''s stability is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_expansion_mechanism, empirical, 'Whether autonomy-based end-of-life authority empirically expands beyond its founding scope.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) structural (external legal/institutional barriers preventing patient choice) or partially internalized (patients internalizing paternalistic messaging and autonomy-denying norms even where legal barriers weaken)?',
    'Post-legalization study design: measure decision-making patterns and patient narratives in jurisdictions before and after legal authorization of autonomy-based end-of-life authority. If suppression persists even after barriers are removed (patients continue to accept paternalistic override despite legal permission for autonomy), the suppression is substantially internalized. If suppression drops sharply after legalization, it was primarily structural.',
    'High internalization would suggest the constraint''s persistence depends on deep cultural/religious normative capture, not just legal coercion. This would elevate the constraint''s effective extractiveness — the target carries the suppression with them even after institutional barriers dissolve. Low internalization suggests structural barriers are the primary lever; removing them would substantially alter the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural vs. internalized suppression in end-of-life autonomy denial.').

omega_variable(
    reading_framing_dependence,
    'Is the autonomy reading''s classification as rope (coordination + some extraction) dependent on framing the ''problem'' as patient suffering and autonomy denial? If the problem were framed as ''preservation of life against premature death wishes,'' would the reading necessarily shift to snare?',
    'Alternative framing analysis: restate the same institutional arrangement (regulatory authority over end-of-life decisions) but frame the problem as ''how to prevent suicide contagion and protect vulnerable populations.'' Under that framing, the same constraint''s ε and suppression values might look like pure extraction of life-extension against patient wishes — a snare, not a rope.',
    'If the classification is highly sensitive to problem framing, the reading''s objectivity is questionable — it becomes more a reflection of the analyst''s normative commitments than of the constraint''s structural properties. This omega documents that framing-neutrality is not achieved, and that sibling readings (sanctity, slippery-slope) are partially powered by alternative problem framings, not just different empirical claims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_framing_dependence, conceptual, 'Dependence of autonomy-reading classification on the problem''s normative framing.').

omega_variable(
    competence_assessment_boundary,
    'Where is the boundary between ''competent patient making an autonomous choice'' and ''depressed patient making a coerced/irrational wish''? How is that boundary drawn, and who draws it?',
    'Audit of institutional gatekeeping practices: how do medical boards and ethics committees evaluate competence? What proportion of requests are denied on competence grounds? Longitudinal analysis: do patients denied on competence grounds experience changed circumstances/preferences over time, validating the denial? Or does denial prevent patients from exercising a genuine autonomous choice?',
    'If the competence boundary is drawn permissively (most requests approved), the constraint operates as a genuine autonomy-honoring framework and suppression is lower. If drawn restrictively (most requests denied on competence grounds), the constraint is substantially gatekeeping autonomy under the cover of competence assessment — effectively a snare with medical legitimation. The boundary is where agenda-setters exercise greatest extractive power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(competence_assessment_boundary, empirical, 'Competence-assessment gatekeeping in autonomy-based end-of-life authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(end__tr_t8, end_of_life_authority__autonomy_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(end__tr_t16, end_of_life_authority__autonomy_reading, theater_ratio, 16, 0.21).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.25).
narrative_ontology:measurement(end__tr_t32, end_of_life_authority__autonomy_reading, theater_ratio, 32, 0.27).
narrative_ontology:measurement(end__tr_t40, end_of_life_authority__autonomy_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(end__be_t8, end_of_life_authority__autonomy_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(end__be_t16, end_of_life_authority__autonomy_reading, base_extractiveness, 16, 0.33).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.37).
narrative_ontology:measurement(end__be_t32, end_of_life_authority__autonomy_reading, base_extractiveness, 32, 0.38).
narrative_ontology:measurement(end__be_t40, end_of_life_authority__autonomy_reading, base_extractiveness, 40, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement(end__su_t8, end_of_life_authority__autonomy_reading, suppression_requirement, 8, 0.84).
narrative_ontology:measurement(end__su_t16, end_of_life_authority__autonomy_reading, suppression_requirement, 16, 0.79).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.75).
narrative_ontology:measurement(end__su_t32, end_of_life_authority__autonomy_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(end__su_t40, end_of_life_authority__autonomy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.18).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end-of-life authority kernel decomposes into three structurally distinct constraints: (1) autonomy_reading (this story) — individual autonomy grounds the right to control death circumstances; substantially extractive suppression of paternalistic restrictions; beneficiaries = patients exercising choice; victims = denied/suppressed patients. (2) sanctity_reading — intrinsic life value overrides autonomy; beneficiaries = institutional gatekeepers maintaining paternalism; victims = patients denied autonomous choice; opposite normative foundation, different ε and victim set. (3) slippery_slope_mechanism — empirical drift in scope as autonomy logic expands; documents that the autonomy reading's apparent stability may be illusory; feeds into mandatrophy analysis. All three readings are live simultaneously in different jurisdictions and institutional contexts. The ε-invariance principle requires separate story files: a welfarist reading and a sanctity reading of the same kernel would both use the same referent (the standing arrangement under contest) but author different ε values reflecting different structural assessments. This decomposition ensures each reading's classification is independent; the engine computes per-seat types without averaging across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
