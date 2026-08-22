% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Prohibition/Gatekeeping of Assisted Death Against Competent Individual Sovereignty
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the autonomy reading of the
 *   end_of_life_decision_authority kernel: competent individuals possess
 *   sovereign authority over the timing and manner of their own death, and
 *   existing legal/institutional gatekeeping structures that deny or
 *   substantially delay this authority constitute an extractive constraint on
 *   that sovereignty. Under this reading, patients suffering unbearably from
 *   terminal or incurable conditions who are denied timely access to assisted
 *   dying are the primary victims; physicians willing to honor patient
 *   sovereignty are recast as facilitators bearing displaced legal risk; and
 *   the slippery-slope/coercion-risk concerns that motivate stricter
 *   gatekeeping are treated, from within this reading, as a risk properly
 *   externalized to and addressed by a different constraint (the
 *   vulnerability-protection reading), not as grounds for restricting
 *   sovereignty itself. This is a sibling of two other constraints in the
 *   same kernel family — sanctity_reading (which holds intentional
 *   life-ending intrinsically wrong regardless of consent) and
 *   vulnerability_protection_reading (which holds that authority must be
 *   distributed across institutional checkpoints to prevent both wrongful
 *   denial and wrongful coercion). All three share the same underlying kernel
 *   — who holds legitimate authority over a person's death — but instantiate
 *   structurally distinct constraints with different ε, different
 *   beneficiary/victim sets, and different classifications; they are not the
 *   same constraint viewed from different angles.
 *
 * KEY AGENTS:
 *   - terminally_ill_competent_patients: primary victims (powerless/trapped) — bear the cost of denial or delay
 *   - physicians_as_facilitators: recast agents bearing displaced legal/career risk for honoring patient sovereignty
 *   - medical_licensing_boards: institutional beneficiaries of continued gatekeeping authority
 *   - religious_institutions_with_standing: ideological beneficiaries of sanctity-doctrine legal force
 *   - disability_rights_advocates: excluded/discounted voice whose coercion concern is routed to a sibling reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.68).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Prohibition/Gatekeeping of Assisted Death Against Competent Individual Sovereignty").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '26b55567-1727-4c2a-a36f-cba95d3e49a0').
narrative_ontology:cs_kernel_codification('26b55567-1727-4c2a-a36f-cba95d3e49a0', distributed).
narrative_ontology:cs_authority_grounding('26b55567-1727-4c2a-a36f-cba95d3e49a0', distributed).
narrative_ontology:cs_reading_relation('26b55567-1727-4c2a-a36f-cba95d3e49a0', end_of_life_decision_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('26b55567-1727-4c2a-a36f-cba95d3e49a0', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('26b55567-1727-4c2a-a36f-cba95d3e49a0', foundational, competent_individual_will_is_sovereign_over_own_death).
narrative_ontology:cs_axiom_status(competent_individual_will_is_sovereign_over_own_death, holdable).
narrative_ontology:cs_axiom_grounding('26b55567-1727-4c2a-a36f-cba95d3e49a0', competent_individual_will_is_sovereign_over_own_death, deontological).
narrative_ontology:cs_axiom('26b55567-1727-4c2a-a36f-cba95d3e49a0', secondary, prolonged_unwanted_suffering_is_a_harm_the_state_must_not_impose).
narrative_ontology:cs_axiom_status(prolonged_unwanted_suffering_is_a_harm_the_state_must_not_impose, holdable).
narrative_ontology:cs_axiom_grounding('26b55567-1727-4c2a-a36f-cba95d3e49a0', prolonged_unwanted_suffering_is_a_harm_the_state_must_not_impose, instrumental).
narrative_ontology:cs_reference_frame('26b55567-1727-4c2a-a36f-cba95d3e49a0', physician_led_paternalist_authority).
narrative_ontology:cs_drift_state('26b55567-1727-4c2a-a36f-cba95d3e49a0', contemporary_assisted_dying_reform_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('26b55567-1727-4c2a-a36f-cba95d3e49a0', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, religious_institutions_with_standing).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, risk_averse_health_systems).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, terminally_ill_competent_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, families_bearing_prolonged_dying).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, physicians_as_facilitators).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, physician_non_maleficence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Have a diagnosed terminal or unbearable condition, retain decision-making capacity, and wish to control the timing and manner of their death. Where legal gatekeeping (waiting periods, multi-physician sign-off, psychiatric review, residency requirements) blocks or delays access, they must continue living through suffering they judge worse than death, seek clandestine or violent means, or travel to permissive jurisdictions if they have the means to do so. Exit is trapped for those without money, mobility, or a cooperative physician.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, terminally_ill_competent_patients, payer,
    powerless, immediate, trapped, national).

% A subset of the above whose specific harm is the extension of suffering caused directly by denial or delay in the authority structure — every day the gatekeeping process consumes is a day of unwanted, unrelieved suffering that would not exist under an autonomy-respecting regime. This population exists only because the constraint under contest denies access; under the autonomy reading, they are the constraint's clearest victims.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients, payer,
    powerless, immediate, trapped, national).

% Caregivers and family members who absorb the emotional, financial, and logistical cost of an extended dying process the patient did not choose. They can advocate, relocate, or exhaust savings on travel to permissive jurisdictions, but cannot unilaterally alter the legal gatekeeping structure.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, families_bearing_prolonged_dying, payer,
    moderate, biographical, constrained, local).

% Willing physicians who would act on a patient's sovereign decision are themselves constrained by licensing boards, criminal liability, and institutional policy. Under this reading they are recast from independent moral agents into facilitators of a patient right; where the structure denies them protected participation, they bear career and legal risk for honoring patient sovereignty. Some administer the gatekeeping process itself (capacity assessment, waiting periods) and thereby also set the terms of access.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, physicians_as_facilitators, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, physicians_as_facilitators, payer).

% Define and enforce the professional standards, liability exposure, and disciplinary consequences that govern physician participation. Their institutional authority and risk-management function are strengthened by their gatekeeping role; loosening it toward pure patient sovereignty would reduce their adjudicative relevance over life-ending decisions.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__autonomy_reading, medical_licensing_boards, agenda_setter).

% Advocate for legal restriction grounded in sanctity-of-life doctrine and participate in legislative and judicial processes shaping the gatekeeping structure. They neither bear the cost of prolonged individual suffering nor face legal jeopardy; they benefit from the doctrine's continued legal force and public legitimacy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, religious_institutions_with_standing, beneficiary,
    organized, civilizational, analytical, national).

% Hospitals and insurers avoid liability and reputational exposure by maintaining conservative, heavily gatekept protocols rather than a clean patient-authority standard. Slow, multi-checkpoint processes reduce the system's institutional risk even where they extend individual patient suffering.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, risk_averse_health_systems, beneficiary,
    institutional, generational, arbitrage, national).

% Argue that expanded individual authority over death, absent robust social supports, functions as coerced or economically pressured 'choice' for disabled and chronically ill people who lack real alternatives. Their objection is largely externalized from this reading's framing, which treats slippery-slope and coercion risk as a cost borne by a different constraint (the vulnerability-protection reading), not this one.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, analytical, national).

% Adjudicate the boundary between individual sovereignty and state interest in preserving life, hearing testimony from all sides and periodically revising statute or case law that defines the gatekeeping apparatus.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Insofar as any gatekeeping survives even under an autonomy framing, it coordinates verification that the individual exercising the sovereignty claim is genuinely competent, informed, and free of external coercion — a real coordination problem when authority is exercised at the boundary of life and death and cannot be revisited after the fact.
% TRANSFER_FUNCTION: The current arrangement moves the burden of proof and the cost of delay from the state/profession onto the individual: patients must repeatedly demonstrate their competence and intent to the satisfaction of gatekeepers, while the institutions that impose the checkpoints bear none of the suffering cost of the additional time consumed.
% ABSENT_VOICES: Disability rights advocates are structurally present in this kernel's debate but their coercion-risk argument is routed to a sibling reading (vulnerability_protection_reading) rather than engaged within this reading's own frame; within this reading their voice is present but discounted as addressing a different, distributionally weighted problem.
% DISAPPEARANCE_RATIONALE: If the current gatekeeping structure vanished and individual sovereignty over the timing/manner of death were fully recognized without institutional checkpoints, physicians would face fundamentally different liability exposure, licensing boards would lose an entire domain of adjudicative authority, health systems would restructure end-of-life protocols, and a currently-suffering population would gain immediate access to relief they are now denied.
% FOUNDING_PROBLEM: Historically the state and medical profession asserted authority over life-ending decisions to prevent wrongful killing, protect vulnerable populations from coercion, and preserve public trust in medicine as a healing rather than killing profession.
% FOUNDING_PROBLEM_CORROBORATION: Palliative care physicians and bioethicists outside the disability-advocacy and religious-institution camps (e.g., authors of comparative studies from permissive jurisdictions such as Oregon, Belgium, and Canada) attest that documented abuse/coercion rates under regulated frameworks remain low, suggesting the founding protective problem is substantially, though not completely, resolved by verification protocols short of full prohibition; religious and disability-advocacy bodies dispute this and are themselves interested parties on one side or the other of the underlying value question.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) reflects that the gatekeeping apparatus, evaluated from this reading's own premises, extends suffering for a population that has already met a demanding competence and diagnosis standard — the marginal restriction beyond verification of genuine, informed, uncoerced intent is read as pure cost with no offsetting benefit to the patient. Suppression (0.72) is high and largely structural: criminal liability for physicians, multi-step institutional sign-off, and jurisdictional restriction actively foreclose the exercised sovereignty rather than merely making it harder. Theater ratio (0.40) reflects a genuine but partially performative verification layer — some capacity/coercion screening is authentic coordination, but escalating procedural requirements in several jurisdictions have outpaced any incremental safety gain and increasingly function as delay for its own sake. Accessibility collapse (0.60) is moderate rather than extreme because determined and resourced patients can sometimes route around restriction (medical tourism, sympathetic physicians); resistance (0.75) is high because patients, families, and reform advocates actively litigate and lobby against the gatekeeping structure. The claim (tangled_rope) and the metrics are authored independently: the metrics describe substantial, actively enforced extraction riding on a genuine (if narrower than currently implemented) verification function.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter/beneficiary seats (licensing boards, religious institutions, risk-averse systems), the arrangement reads as prudent, protective coordination consistent with professional ethics and social values. From the payer seats (patients, families), the identical structure reads as enforced denial of a right they hold to be sovereign, imposing suffering with no corresponding benefit to them. The engine computes this divergence from the structural power/exit/time_horizon data authored per seat; this story does not resolve which seat is 'correct' — that adjudication belongs to the omega variables and to the kernel's sibling readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Terminally ill competent patients and the suffering-prolonged subset sit at the target end of directionality — trapped exit, immediate time horizon, direct and undiluted cost of denial. Medical licensing boards, religious institutions, and risk-averse health systems sit at the beneficiary end — institutional or organized power, arbitrage-level exit from the consequences of the policies they help maintain, and either legitimacy, adjudicative authority, or liability protection flowing to them from the constraint's persistence. Physicians-as-facilitators sit in between: they administer parts of the verification process (making them partial agenda-setters) while also bearing career and legal risk when patient sovereignty and professional liability collide (making them partial payers) — this dual role is captured with a secondary_role rather than forcing a single directionality bucket.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing wrongful killing and protecting vulnerable people from coercion — is genuinely contested rather than dead: comparative evidence from permissive jurisdictions suggests low documented abuse rates under well-designed verification regimes, which from this reading's perspective indicates the founding protective function can be satisfied by a narrower, competence/coercion-focused verification layer than currently exists in more restrictive jurisdictions. Where gatekeeping exceeds what verification requires — multi-month waiting periods, redundant multi-physician sign-off cascades, residency requirements untethered to fraud prevention — the classification of tangled_rope over pure mountain or pure rope is doing real work: it distinguishes the genuine, still-live coordination function (verifying competence and absence of coercion) from the extractive residue (delay and denial beyond what verification requires) that the founding problem no longer justifies. Treating the whole apparatus as a mountain would launder the extractive residue as natural law; treating it as a pure snare would erase the real verification function that even patient-sovereignty advocates generally accept is necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_vs_denial_boundary,
    'Where does genuine coordination (verifying competence, diagnosis, and absence of coercion) end and pure extractive delay/denial begin within the current gatekeeping apparatus?',
    'Comparative empirical study of jurisdictions with varying gatekeeping intensity (e.g., Oregon-style single-track vs. Belgium-style multi-checkpoint vs. highly restrictive regimes), measuring documented coercion/abuse incidence against time-to-access and patient-reported suffering during the waiting period.',
    'If low-friction verification regimes show no meaningfully higher abuse rate than high-friction regimes, most of the measured extractiveness in high-friction jurisdictions is extractive residue rather than coordination cost, strengthening the tangled_rope classification''s extraction component; if abuse rates rise sharply as friction decreases, more of the current structure is justified coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_vs_denial_boundary, empirical, 'Empirically distinguishing genuine safety coordination from extractive delay in gatekeeping design.').

omega_variable(
    kernel_reading_incommensurability,
    'Is the underlying disagreement among the three kernel readings (autonomy, sanctity, vulnerability-protection) resolvable by evidence, or does it rest on incommensurable value premises about the source of authority over death (individual will vs. intrinsic life value vs. distributed institutional trust)?',
    'This is likely not empirically resolvable in the way the verification-boundary question is; it depends on which normative premise (individual sovereignty, sanctity of life, or distributed institutional legitimacy) a given legal or moral framework treats as foundational. Philosophical and comparative-constitutional analysis can clarify the structure of disagreement without resolving it.',
    'If the disagreement is genuinely value-incommensurable, the three kernel readings will persist as coexisting, non-converging constraints indefinitely, and classification work should focus on measuring each reading''s internal coherence and extraction pattern rather than adjudicating which reading is ''true.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_incommensurability, conceptual, 'Whether the kernel''s sibling readings are empirically adjudicable or reflect incommensurable value premises.').

omega_variable(
    coercion_risk_externalization,
    'By routing slippery-slope and disability-coercion risk to the vulnerability_protection_reading rather than engaging it within this reading''s own frame, does the autonomy_reading understate a cost that properly belongs to its own ε?',
    'Track whether jurisdictions that adopt autonomy-forward policy (minimal gatekeeping) subsequently show measurable increases in coercion of disabled, elderly, or economically pressured populations, as opposed to jurisdictions with distributed-checkpoint models.',
    'If coercion risk rises measurably under low-gatekeeping regimes, this reading''s ε is understated by treating that risk as belonging entirely to a sibling constraint; the two readings may need a shared coupling term rather than clean separation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_risk_externalization, conceptual, 'Whether externalizing coercion-risk to a sibling reading is a clean decomposition or an ε-understatement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__autonomy_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__autonomy_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.37).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__autonomy_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.4).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 16, 0.62).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.65).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 8, 0.69).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.71).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__autonomy_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the end_of_life_decision_authority kernel. autonomy_reading treats individual competent will as the governing authority and finds current gatekeeping substantially extractive (ε=0.68, tangled_rope). sanctity_reading treats intentional life-ending as intrinsically wrong regardless of consent and would author a very different ε and beneficiary/victim structure (likely reading the current restriction as protective rather than extractive). vulnerability_protection_reading treats distributed institutional checkpoints as the correct locus of authority to balance denial-risk against coercion-risk, and would author a different, more moderate ε reflecting genuine coordination value in the checkpoint structure itself. All three share the same underlying kernel object (who holds legitimate authority over a person's death) but are authored as separate constraints with independent ε per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
