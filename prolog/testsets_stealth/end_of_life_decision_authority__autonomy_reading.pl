% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Individual Sovereign Authority over Death (Autonomy Reading)
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   The natural-language debate over 'who may decide death' conflates at
 *   least three structurally distinct commitments, decomposed here per the
 *   epsilon-invariance principle into a three-story constraint family. This
 *   file authors ONLY the autonomy reading: the arrangement, as instituted in
 *   assisted-dying statutes, under which a competent individual's settled
 *   decision about their own death is authoritative and binds state and
 *   profession to provide a lawful channel. The epsilon referent is that
 *   standing administered arrangement, assessed by the autonomy reading's own
 *   lights: against the standard of sovereignty, every eligibility exclusion,
 *   waiting period, and conscience-driven access gap registers as residual
 *   extraction, while voluntary use by qualifying patients registers as the
 *   arrangement working. The sibling readings - sanctity_reading (life's
 *   intrinsic value bars intentional ending; its victim set would include
 *   every intentionally ended life) and vulnerability_protection_reading
 *   (authority distributed across institutional checkpoints; its metric
 *   surface is checkpoint density) - are separate constraints with their own
 *   epsilon, beneficiaries, and victims, linked via
 *   network.affects_constraints and never averaged into this file. Structural
 *   delta realized here: denied sufferers enter the victim set, healthcare
 *   professionals shift from gatekeepers to facilitators-with-residue, and
 *   slippery-slope pressure is attributed to the surrounding care economy
 *   rather than the arrangement (an attribution carried as an omega, not
 *   assumed).
 *
 * KEY AGENTS:
 *   - qualifying_terminal_patients: primary beneficiary (organized/trapped) - gains the lawful exit, pays the procedural weeks
 *   - denied_suffering_patients: primary target (powerless/trapped) - bears the eligibility line that criminalizes their preferred exit
 *   - facilitating_physicians: dual-positioned facilitator (organized/mobile) - collects legal cover, carries assessment and moral load
 *   - legislative_and_judicial_authorities: agenda setter (institutional/constrained) - draws and polices the line under coalition pressure
 *   - families_of_qualifying_patients: secondary beneficiary (moderate/constrained) - lawful closure and liability protection
 *   - burden_averse_dependent_elders: diffuse-cost bearer (powerless/trapped) - absorbs the option's ambient pressure
 *   - conscientious_objecting_providers: exempted beneficiary (organized/mobile) - collects conscience protection purchased by others' reduced access
 *   - disability_rights_advocates: analytical observer (organized/analytical) - monitors and contests without operating or bearing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.52).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.45).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Individual Sovereign Authority over Death (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'd56f4196-4726-4c81-9dcb-af818450d1e7').
narrative_ontology:cs_kernel_codification('d56f4196-4726-4c81-9dcb-af818450d1e7', formalized).
narrative_ontology:cs_authority_grounding('d56f4196-4726-4c81-9dcb-af818450d1e7', expertise).
narrative_ontology:cs_interpretation_layer_present('d56f4196-4726-4c81-9dcb-af818450d1e7').
narrative_ontology:cs_reading_relation('d56f4196-4726-4c81-9dcb-af818450d1e7', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d56f4196-4726-4c81-9dcb-af818450d1e7', end_of_life_decision_authority__vulnerability_protection_reading, coexists_with).
narrative_ontology:cs_axiom('d56f4196-4726-4c81-9dcb-af818450d1e7', foundational, competence_confers_death_authority).
narrative_ontology:cs_axiom_status(competence_confers_death_authority, holdable).
narrative_ontology:cs_axiom_grounding('d56f4196-4726-4c81-9dcb-af818450d1e7', competence_confers_death_authority, deontological).
narrative_ontology:cs_axiom('d56f4196-4726-4c81-9dcb-af818450d1e7', foundational, denial_of_exit_harms_the_competent_patient).
narrative_ontology:cs_axiom_status(denial_of_exit_harms_the_competent_patient, holdable).
narrative_ontology:cs_axiom_grounding('d56f4196-4726-4c81-9dcb-af818450d1e7', denial_of_exit_harms_the_competent_patient, empirically_contingent).
narrative_ontology:cs_reference_frame('d56f4196-4726-4c81-9dcb-af818450d1e7', competent_individual_sovereign_choice).
narrative_ontology:cs_drift_state('d56f4196-4726-4c81-9dcb-af818450d1e7', contemporary_expansion_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d56f4196-4726-4c81-9dcb-af818450d1e7', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, qualifying_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, families_of_qualifying_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, facilitating_physicians).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, conscientious_objecting_providers).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, denied_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, burden_averse_dependent_elders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, qualifying_terminal_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, facilitating_physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, personal_autonomy_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, informed_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with terminal diagnoses and decision-making capacity who petition for a prescribed life-ending. The arrangement grants them a lawful, medically supervised exit their circumstances otherwise deny. Reaching it costs them scarce final weeks of petitions, waiting periods, and repeated attestations, and requires finding a willing provider in a profession where many decline. Leaving the process means continuing the dying they sought to shorten.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, qualifying_terminal_patients, beneficiary,
    organized, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, qualifying_terminal_patients, payer).

% People whose suffering is severe and durable but whose conditions fall outside the eligibility line - degenerative illness short of a terminal prognosis, treatment-resistant psychiatric suffering, accumulated frailty. The same statute that authorizes the exit for others marks their preferred exit as unlawful, and anyone who would help them faces prosecution. Their options are endurance, travel to another jurisdiction where that is recognized, or clandestine routes that carry legal risk for everyone involved.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, denied_suffering_patients, payer,
    powerless, immediate, trapped, national).

% Physicians who assess requests, prescribe, and in some regimes administer. The arrangement converts what was prosecutable into regulated practice, giving them legal cover and a defined protocol; in exchange they carry assessment responsibility, documentation duties, and sometimes the act itself, with the moral weight colleagues and institutions attach to it. They can decline individual cases or stop participating altogether; their willingness is the hinge of access.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, facilitating_physicians, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, facilitating_physicians, payer).

% Close relatives of people using the channel. They gain a lawful, witnessed death and protection from the liability that clandestine assistance would carry. Some also bear disagreement with the choice, grief conducted under procedural scrutiny, and the logistics of the final weeks.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_of_qualifying_patients, beneficiary,
    moderate, biographical, constrained, national).

% Legislatures write the eligibility line, waiting periods, and reporting duties; courts police the boundary between authorized and criminal assistance and hear challenges from both directions - expansion suits from denied patients, restraint suits from opponents. Every adjustment reopens a coalition fight they must win again. They operate inside constitutional review and electoral accountability and cannot simply step away from the question.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislative_and_judicial_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Elderly or disabled people dependent on others' care who never request the option but live in a world where it exists. The visible availability of a dignified exit changes the ambient arithmetic of being a burden; some report feeling quietly steered toward it by care costs and caregiver exhaustion. Whether that pressure originates in this arrangement or in the surrounding care economy is disputed. Their dependence leaves little room to opt out of the changed atmosphere.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, burden_averse_dependent_elders, payer,
    powerless, biographical, trapped, national).

% Clinicians and institutions that decline participation on moral or religious grounds, protected by conscience clauses written into the same statutes. They receive exemption from duties their colleagues must perform. Where they dominate a region's hospital market, their refusals lengthen or sever the access path for nearby patients, who must search elsewhere while ill.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, conscientious_objecting_providers, beneficiary,
    organized, biographical, mobile, regional).

% Organizations that monitor the arrangement's operation and contest its expansion, arguing that eligibility lines encode judgments about whose lives are worth living and that safeguards under-protect people whose dependence can be mistaken for consent. They produce analyses, testify, and litigate; they neither operate the channel nor bear its direct costs.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Creates a single lawful, medically supervised channel for a competent person's chosen death: competence and voluntariness are verified once under shared rules, facilitators receive legal protection, families receive certainty, and the state obtains a monitored alternative to clandestine suicide and prosecutable mercy.
% TRANSFER_FUNCTION: Moves decision authority over the timing and manner of death from state prohibition and professional gatekeeping to the competent individual; moves procedural burden (petitions, waiting periods, assessments) onto the requester; moves legal risk off families and onto a regulated clinical record.
% ABSENT_VOICES: Those the eligibility line excludes - the degeneratively ill, the psychiatrically suffering, the frail elderly - had no seat when the line was drawn and rarely acquire standing until near death; residents of non-adopting jurisdictions live under arrangements they cannot vote into existence; the severely cognitively impaired cannot appear for themselves at all.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, qualifying patients would return to clandestine routes or coerced endurance, physicians to prosecutable mercy, families to liability exposure, and prosecutors to charging compassionate helpers - end-of-life practice in adopting jurisdictions would visibly reorganize within months.
% FOUNDING_PROBLEM: Criminalized assistance left dying people to endure deaths they had settled against, drove some to solitary suicide while still capable of acting alone, and exposed the relatives and doctors who helped them to prosecution - with no lawful way to honor a competent person's considered wish.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: palliative-care literature documents symptom burdens that resist treatment; coroner statistics recorded rises in elderly solo suicide where assistance remained prohibited; the plaintiff cohort in Carter v. Canada testified to the denied-exit harm in court; criminal dockets supply records of prosecuted family members. The problem's persistence for the ineligible population rests on these sources, not on the arrangement's supporters.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction sits just above midpoint: the channel delivers real, wanted exits to qualifying users while the eligibility line and conscience geography push a comparably sized population into continued unwanted dying, and every user spends scarce final weeks on procedure. Suppression is moderate and unscaled by scope in the engine's arithmetic: the machinery keeping assistance inside statutory channels (criminal backstop, professional discipline) presses hardest on the excluded and on would-be helpers beyond the line, hardly at all on qualifying users. Theater is under one-half - competence screening intercepts real incapacity and coercion - but a growing share of safeguard activity is ritual that legitimizes the practice politically rather than protecting anyone. Accessibility collapse is moderate: palliative sedation, treatment refusal, hospice, and cross-border travel remain live alternatives, so understanding the constraint does not annihilate the option set. Resistance is sustained and bidirectional - expansion litigation from below, restraint campaigns from above. Receipt check performed before authoring 'diffuse': qualifying patients receive access, physicians legal cover, objectors conscience exemption - each a designed benefit rather than the arrangement's excess cost; the excess (foregone exits of the denied, procedural weeks of users) dissipates into the political settlement holding the coalition together, accruing to no single seat. Fixing cost is prohibitive for the agenda setter: redrawing the eligibility line to match the sovereignty principle requires re-winning a coalition fight against organized opposition each time, while the chief beneficiaries of fixing (denied sufferers) hold the least political weight. All three series share one seven-point grid (1997-2026 mapped to 0-29) so no metric is sampled against another's end-state.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the arrangement is a calibrated compromise that holds a coalition together; from the qualifying patient's seat it is a hard-won liberty; from the denied patient's seat the same statute reads as a wall with a door built into someone else's room; from the objecting provider's seat it is a tolerable regime precisely because the conscience clause holds. The engine computes these divergent per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Qualifying patients and their families sit near the beneficiary pole: the statute subsidizes their exit. Facilitating physicians collect legal cover but pay assessment, documentation, and moral costs, placing them nearer symmetric than the raw beneficiary declaration alone would suggest. Denied suffering patients sit at the target pole: the eligibility architecture that authorizes others' exits is exactly what criminalizes theirs, and their exit from the constraint is nil. Dependent elders absorb a diffuse pressure whose attribution is contested (see the burden_pressure_attribution omega). Objecting providers collect conscience protection whose availability is purchased by others' reduced access - a beneficiary position the simple derivation would underweight. No directionality overrides are authored: the structural declarations plus exit options already place each seat correctly, and the override mechanism keys on power atoms that several differently-positioned stakeholders share, so an override would misfire across seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live for the population the eligibility line excludes, so the arrangement has not outlived its mandate outright - but the mandate has narrowed relative to the reading's own principle, which recognizes no competence-respecting line at all. Reading the structure as pure coordination would erase the denied population; reading it as pure extraction would erase the genuine liberation qualifying patients obtain from the identical statute. The hybrid classification keeps both facts on the books and locates the asymmetry in the line itself - the single lever the agenda setter controls and the one whose movement is most politically expensive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Which reading of the end_of_life_decision_authority kernel should govern - individual sovereignty, sanctity, or distributed checkpoint authority?',
    'Constitutional adjudication and cross-jurisdiction legislative comparison; whichever reading is adopted determines which population enters the victim set.',
    'Under sanctity_reading the victim set flips to every person whose death is intentionally hastened plus the facilitating clinicians; under vulnerability_protection_reading procedural checkpoints multiply and the user''s autonomy share shrinks; this file''s victim set (denied sufferers) exists only under the autonomy reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which sibling reading governs the shared kernel.').

omega_variable(
    eligibility_line_placement,
    'Where does the eligibility line belong - terminal prognosis only, intolerable and irremediable suffering, or any settled competent request?',
    'Outcome comparison across jurisdictions that drew the line differently (Oregon-model, Benelux-model, and tracked-expansion regimes).',
    'Each widening converts denied_suffering_patients into beneficiaries and shifts residual extraction toward the procedural burden all users carry; each narrowing does the reverse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_line_placement, empirical, 'Location of the eligibility line as the master variable of the victim set.').

omega_variable(
    burden_pressure_attribution,
    'Does the ambient pressure felt by dependent elders originate in this arrangement''s existence or in the surrounding care economy''s underfunding?',
    'Compare request patterns and reported pressure across regions matched for the arrangement but differing in home-care funding.',
    'If internal, burden_averse_dependent_elders are genuine victims and measured extraction rises; if external, they are casualties of neighboring policy and this constraint''s victim set contracts to denied_suffering_patients.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_pressure_attribution, conceptual, 'Internal versus externalized attribution of the slippery-slope pressure.').

omega_variable(
    safeguard_functionality_ratio,
    'What share of safeguard activity (repeat petitions, waiting periods, reporting regimes) performs protection versus legitimating performance?',
    'Audit detected-coercion and incapacity interceptions per safeguard-hour against randomized file review.',
    'A high performance share would push theater_ratio upward and date a drift toward maintained-form operation; a low share supports reading the safeguards as genuine coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(safeguard_functionality_ratio, empirical, 'Protective versus theatrical composition of the safeguard apparatus.').

omega_variable(
    competence_assessment_validity,
    'Can clinicians reliably distinguish a settled competent death wish from depressive capitulation at the point of assessment?',
    'Longitudinal follow-up of declined applicants and psychological-autopsy studies of completed procedures.',
    'If reliability is low, the facilitator role is unstable and pressure grows toward the vulnerability_protection_reading''s checkpoint density; if high, physician gatekeeping is defensible residue rather than extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_validity, empirical, 'Epistemic soundness of the competence screen underwriting the facilitator role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 29).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t5, end_of_life_decision_authority__autonomy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(end__tr_t5, observed).
narrative_ontology:measurement(end__tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement_basis(end__tr_t10, observed).
narrative_ontology:measurement(end__tr_t15, end_of_life_decision_authority__autonomy_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(end__tr_t15, observed).
narrative_ontology:measurement(end__tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(end__tr_t20, observed).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(end__tr_t24, observed).
narrative_ontology:measurement(end__tr_t29, end_of_life_decision_authority__autonomy_reading, theater_ratio, 29, 0.34).
narrative_ontology:measurement_basis(end__tr_t29, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t5, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(end__be_t5, observed).
narrative_ontology:measurement(end__be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement_basis(end__be_t10, observed).
narrative_ontology:measurement(end__be_t15, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement_basis(end__be_t15, observed).
narrative_ontology:measurement(end__be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement_basis(end__be_t20, observed).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.51).
narrative_ontology:measurement_basis(end__be_t24, observed).
narrative_ontology:measurement(end__be_t29, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 29, 0.52).
narrative_ontology:measurement_basis(end__be_t29, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t5, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(end__su_t5, observed).
narrative_ontology:measurement(end__su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(end__su_t10, observed).
narrative_ontology:measurement(end__su_t15, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(end__su_t15, observed).
narrative_ontology:measurement(end__su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement_basis(end__su_t20, observed).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.45).
narrative_ontology:measurement_basis(end__su_t24, observed).
narrative_ontology:measurement(end__su_t29, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 29, 0.44).
narrative_ontology:measurement_basis(end__su_t29, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% Decomposition of the 'end-of-life authority' label into three readings per the epsilon-invariance principle: one colloquial label, three constraints with distinct epsilon, victim sets, and failure modes. The autonomy reading supplies the doctrinal premise (competence confers authority) that both siblings must accept, amend, or reject, so its edges point at both; contamination propagates along these edges when any reading's purity degrades.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
