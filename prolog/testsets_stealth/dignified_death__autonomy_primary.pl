% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Final Authority over Death Timing — Autonomy-Primary Reading
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The standing arrangement under contest is the legal-medical complex
 *   governing final authority over the timing and method of death: criminal
 *   prohibition of assisted dying in most jurisdictions, and tightly gated
 *   medical access where prohibition has softened. This file instantiates the
 *   autonomy_primary reading of the dignified_death kernel — dignity resides
 *   in self-determination, so the suffering individual holds final authority
 *   — and authors epsilon for the standing arrangement AS THIS READING SEES
 *   IT: a regime that denies competent adults the exit they judge
 *   constitutive of their dignity, while retaining a real
 *   protective-coordination core. The referent is the existing arrangement,
 *   never the reading's endorsed alternative (per the epsilon-referent rule);
 *   a rights-respecting regime would show epsilon near zero and is not this
 *   story. The colloquial label 'dignified death' decomposes into three
 *   structurally distinct constraints — this reading plus sanctity_primary
 *   and relational_autonomy — linked via network.affects_constraints per the
 *   epsilon-invariance decomposition rule. Claim/metric independence holds:
 *   the tangled_rope claim and the authored metrics are independent
 *   descriptive commitments, and the engine computes per-seat types from the
 *   structural data.
 *
 * KEY AGENTS:
 *   - competent_suffering_adults_denied_exit: Primary target (powerless/trapped) — bears the denial of exit; suffering prolonged against their will
 *   - eligible_assisted_dying_recipients: Conditional beneficiary (moderate/constrained) — receive access only by passing the gate others administer
 *   - medical_gatekeeping_institutions: Agenda-setter (institutional/arbitrage) — draft criteria, certify, approve, discipline; the decision point sits with them
 *   - state_criminal_authorities: Agenda-setter and secondary beneficiary (institutional/arbitrage) — define and enforce the criminal line
 *   - religious_institutions_defending_prohibition: Beneficiary (organized/mobile) — public moral authority tied to prohibition persisting
 *   - vulnerable_patients_shielded_from_coercion: Beneficiary (powerless/constrained) — the genuine protective-coordination core of the arrangement
 *   - disability_rights_advocates: Beneficiary (organized/mobile) — oppose expansion; benefit from restrictive status quo
 *   - physicians_willing_to_assist: Target (powerful/identity_locked) — license and criminal exposure; healer identity makes the question identity-bearing
 *   - family_members_aiding_loved_ones: Target (moderate/trapped) — bear witnessing burden and legal exposure for helping
 *   - non_eligible_chronic_sufferers: Excluded voice (powerless/trapped) — refused categorically, absent from the eligibility conversation
 *   - constitutional_courts: Analytical observer (institutional/analytical) — adjudicate autonomy-versus-state-interest claims; rulings have driven expansion and entrenchment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.56).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.56).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Final Authority over Death Timing — Autonomy-Primary Reading").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '3eb52690-ad8c-46bd-895e-63c43f1c1091').
narrative_ontology:cs_kernel_codification('3eb52690-ad8c-46bd-895e-63c43f1c1091', distributed).
narrative_ontology:cs_authority_grounding('3eb52690-ad8c-46bd-895e-63c43f1c1091', distributed).
narrative_ontology:cs_reading_relation('3eb52690-ad8c-46bd-895e-63c43f1c1091', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('3eb52690-ad8c-46bd-895e-63c43f1c1091', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('3eb52690-ad8c-46bd-895e-63c43f1c1091', foundational, self_determination_constitutes_dignity).
narrative_ontology:cs_axiom_status(self_determination_constitutes_dignity, holdable).
narrative_ontology:cs_axiom_grounding('3eb52690-ad8c-46bd-895e-63c43f1c1091', self_determination_constitutes_dignity, deontological).
narrative_ontology:cs_axiom('3eb52690-ad8c-46bd-895e-63c43f1c1091', secondary, competent_suffering_patient_holds_final_authority_over_death_timing_and_method).
narrative_ontology:cs_axiom_status(competent_suffering_patient_holds_final_authority_over_death_timing_and_method, holdable).
narrative_ontology:cs_axiom_grounding('3eb52690-ad8c-46bd-895e-63c43f1c1091', competent_suffering_patient_holds_final_authority_over_death_timing_and_method, deontological).
narrative_ontology:cs_reference_frame('3eb52690-ad8c-46bd-895e-63c43f1c1091', patient_final_authority_self_determination).
narrative_ontology:cs_drift_state('3eb52690-ad8c-46bd-895e-63c43f1c1091', contemporary_assisted_dying_expansion_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3eb52690-ad8c-46bd-895e-63c43f1c1091', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, eligible_assisted_dying_recipients).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_gatekeeping_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, religious_institutions_defending_prohibition).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, vulnerable_patients_shielded_from_coercion).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, competent_suffering_adults_denied_exit).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, physicians_willing_to_assist).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, family_members_aiding_loved_ones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, state_criminal_authorities).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, disability_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adults with terminal or incurable conditions experiencing sustained suffering they judge unbearable. They have requested control over the timing and manner of their death; the law refuses those requests or permits them only after passing gates they cannot reach. Their remaining options — refusing treatment, voluntarily stopping eating and drinking, traveling abroad, attempting suicide alone — are slower, riskier, or carry legal exposure for anyone who helps them. Exit from the situation, in every sense, is precisely what is withheld.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, competent_suffering_adults_denied_exit, payer,
    powerless, biographical, trapped, national).

% Patients who meet the narrow statutory criteria in permissive jurisdictions. They receive medically supervised death after physician sign-off, waiting periods, and documentation. Access is real but conditional: they hold it only by satisfying criteria written and administered by others, and the criteria can move.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, eligible_assisted_dying_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Hospital ethics boards, medical licensing colleges, and assisted-death review panels. They draft eligibility criteria, certify prognoses, approve or refuse requests, and discipline practitioners who step outside protocol. Their institutional role, staffing, and caseload exist because the decision point sits with them. Internal dissent, litigation exposure, and staff attrition are recurring costs they absorb.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_gatekeeping_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Legislatures and prosecutors who define and enforce the criminal line on assisted dying. They decide which jurisdictions prohibit outright, which permit narrow exceptions, and whom to charge. Charging decisions against compassionate family members or physicians periodically surface the human cost of the line they administer.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_criminal_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, state_criminal_authorities, beneficiary).

% Churches and faith communities whose teaching holds that life's ending lies beyond individual disposal. Persistence of prohibition preserves their public moral authority and their institutional role in end-of-life care provision. They fund opposition campaigns and litigate against expansion.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, religious_institutions_defending_prohibition, beneficiary,
    organized, generational, mobile, global).

% Elderly and dependent patients whose families or caregivers stand to gain from their deaths. Eligibility screening, waiting periods, and independent-witness requirements give them a procedural buffer against pressure that would otherwise operate privately and invisibly.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, vulnerable_patients_shielded_from_coercion, beneficiary,
    powerless, biographical, constrained, national).

% Organizations representing disabled people who oppose expanding assisted dying, arguing that a legal option to die becomes systemic pressure to die once society frames dependency as undignified. They benefit from the restrictive status quo and lobby against widened eligibility. Some members dissent and support individual choice.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, beneficiary,
    organized, generational, mobile, national).

% Doctors who conclude that some patients' suffering justifies assistance and are willing to provide it. Practicing where prohibited exposes them to license revocation and prosecution; practicing where permitted binds them to protocols they may find arbitrary. The healer identity of the profession makes the question identity-bearing whichever way they answer.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, physicians_willing_to_assist, payer,
    powerful, biographical, identity_locked, national).

% Spouses, children, and friends of the dying. They absorb the daily reality of prolonged suffering, and where they help — accompanying travel, gathering information, simply being present at the end — they carry legal exposure. There is no exit from the situation except the death itself.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, family_members_aiding_loved_ones, payer,
    moderate, biographical, trapped, local).

% People with locked-in syndrome, treatment-resistant depression, advanced degenerative disease, or other enduring conditions who fall outside every current eligibility line. Their requests are refused categorically, and the eligibility debates proceed without them; they enter the conversation only as hypotheticals invoked by others.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, non_eligible_chronic_sufferers, excluded,
    powerless, biographical, trapped, national).

% High courts adjudicating whether prohibitions survive autonomy and equality challenges. Their rulings have driven expansion in several jurisdictions and entrenched prohibition in others. They hear the structured versions of every other seat's position and issue binding answers.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, medical_gatekeeping_institutions).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement solves real problems: verifying that a death wish is informed, settled, and uncoerced; confirming medical facts about prognosis and capacity; giving families and clinicians a shared procedure instead of improvised private decisions; and maintaining a boundary around medical practice that practitioners and patients rely on for trust.
% TRANSFER_FUNCTION: Moves decision authority over death timing from the suffering individual to legislatures, courts, and medical gatekeepers; moves the individual's remaining lifespan onto a schedule others administer; and where assistance is lawful, moves the act itself from private hands into medically supervised protocol.
% ABSENT_VOICES: Non-eligible chronic sufferers are absent from eligibility debates — they appear only as hypotheticals. The dying in prohibition jurisdictions speak mainly through advocacy proxies or posthumous testimony. Future patients whose conditions sit outside today's criteria have no seat at all.
% DISAPPEARANCE_RATIONALE: If the gatekeeping-and-prohibition arrangement vanished overnight, end-of-life practice would reorganize immediately: advance directives would govern, physicians would assist openly where conscience allowed, hospice and palliative norms would shift, and the medical-legal review apparatus would dissolve — the positions, caseloads, and campaigns of nearly every named seat depend on the arrangement's continued existence.
% FOUNDING_PROBLEM: Assisted dying was criminalized to protect vulnerable people from coercion and predation, to preserve medicine's identity as a healing profession, and to maintain the state's interest in preserving life; the gatekeeping apparatus was later built to manage the exception safely where prohibition softened.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by elder-abuse and coercion research, disability-rights testimony before legislative commissions, and comparative reviews of permissive jurisdictions (Oregon annual reports, Dutch regional review committees). No fully disinterested corroborator exists — every institutional seat in this domain holds a position in the underlying contest, which is itself signal.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.56 sits in the expected 0.45-0.60 band: the arrangement denies exit to a competent adult class whose members judge continuation unbearable, and life-prolongation technology has raised the cost of that denial over the interval. Suppression 0.68 reflects the enforcement machinery — criminal liability for assistance, license revocation, prosecution of family members — and is authored as a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream. Theater_ratio 0.38: ethics-committee proliferation, form-work, and performative review have grown faster than the protective function they nominally serve, but the screening core retains real function. Accessibility_collapse 0.52: alternatives exist (treatment refusal, VSED, travel to permissive jurisdictions) but are degraded, slow, risky, or legally hazardous for helpers — partially collapsed, not fully. Resistance 0.62: sustained litigation, advocacy movements, and physician civil disobedience meet the arrangement continuously. The three measurement series run on ONE shared time grid (t=0..30, all metrics at every point) so the engine samples without scalar substitution; all series rise — extraction accumulates as technology extends lifespans, suppression ratchets as autonomy claims mounted, theater grows as committee layers multiplied. Boltzmann coordination_type is enforcement_mechanism: the dominant coordination function is legal-regulatory governance of the death decision; medicine's identity maintenance is real but secondary to it.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats compute differently from the same structure. From the gatekeeping seat the arrangement is a protective protocol it built and administers; from the trapped payer seat the same protocol operates as denial of the only exit that matters. Physicians split the gap by identity: the healer identity fuses with the prohibition for some (identity_locked in compliance) and against it for others (identity_locked in transgression) — either way exit is identity-priced, which is why their exit_options is identity_locked rather than constrained. Coalition dynamics matter for the powerless: right-to-die organizations aggregate isolated dying individuals into organized litigants, converting powerless-class resistance into powerful institutional challenge — the mechanism behind several court-driven expansions. Sanctity-adherents experience a different constraint entirely; that experience lives in the sibling file, not here.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared structure maps to directionality as follows: competent_suffering_adults_denied_exit, family_members_aiding_loved_ones, and non_eligible_chronic_sufferers sit near the full-target end (trapped exit amplifies); physicians_willing_to_assist sit high-d with identity-lock modulation; eligible_assisted_dying_recipients and vulnerable_patients_shielded_from_coercion sit near the beneficiary end (subsidized by the arrangement, though the former only conditionally); religious_institutions_defending_prohibition and disability_rights_advocates derive low d as beneficiaries of the restrictive status quo; medical_gatekeeping_institutions and state_criminal_authorities combine agenda-setting with benefit collection. No directionality_overrides were authored: the schema keys overrides by power atom alone, which is too coarse to differentiate the institutional seats (gatekeepers, prosecutors, courts) without distorting seats the derivation already handles correctly; the declared beneficiary/victim and exit data capture the structure, and per-seat divergence is left to engine computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting the vulnerable from coercion while preserving medicine's healing identity — is CONTESTED, not dead: coercion risk is empirically real (corroborated by elder-abuse research), but the parties dispute whether the current apparatus addresses it proportionately. The tangled_rope classification prevents both mislabels: a pure-snare reading would erase the genuine protective function that shields vulnerable patients from predation; a pure-rope reading would erase the asymmetric denial borne by competent sufferers who fail arbitrary eligibility lines. The R5 mismatch consumer reads founding_problem_status=contested x disappearance_verdict=world_rearranges: no dead-mandate/zombie flag fires, correctly — the mandate is disputed, not obsolete. Theater is rising (0.22 to 0.38) and worth watching: if screening collapses into ritual while denial persists, the arrangement drifts toward piton-or-worse territory; the temporal series exists to date that transition if it comes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'Does the computed classification track the autonomy_primary reading''s victim/beneficiary structure specifically, or the shared dignified_death kernel as any reading would render it?',
    'Compile the sibling stories (sanctity_primary, relational_autonomy) and compare victim sets, beneficiary structures, and epsilon values across the family; divergent victim sets confirm reading-indexed classification.',
    'If the sanctity reading prevailed socially, the victim set inverts — assisted deaths become the harm and prohibition becomes the protection — so the classification is valid only relative to this reading''s axioms, not the topic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Reading-indexed nature of the classification within the dignified_death kernel family.').

omega_variable(
    coercion_screening_genuine_function,
    'What fraction of the gatekeeping apparatus addresses empirically real coercion of vulnerable patients, versus paternalistic overreach that blocks competent voluntary requests?',
    'Coercion-rate audits in permissive jurisdictions (Oregon annual reports, Ontario MAiD reviews, Dutch regional committee findings) cross-referenced with elder-abuse incidence data.',
    'If documented coercion is rare, much of the safeguard layer is protective theater and effective extraction rises toward the snare boundary; if coercion is common, a substantial share of the measured extraction is genuine coordination cost and the tangled_rope reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_screening_genuine_function, empirical, 'Genuine protective function versus theater in the eligibility-screening layer.').

omega_variable(
    eligibility_boundary_moral_relevance,
    'Does the terminal-prognosis eligibility line track a morally relevant distinction, or does it function as institutional self-protection that arbitrarily enlarges the denied population?',
    'Compare request profiles, regret rates, and safeguard performance across jurisdictions with different boundaries (Benelux vs Oregon-style statutes); test whether outcomes degrade when the line moves.',
    'An arbitrary boundary enlarges the victim set (non-eligible chronic sufferers) and raises effective extraction for the excluded; a principled boundary supports the coordination half of the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_moral_relevance, empirical, 'Whether the eligibility line is a moral distinction or an administrative convenience.').

omega_variable(
    request_authenticity_under_suffering,
    'Are the expressed death-wishes of suffering patients authentic stable preferences, or artifacts of depression, pain, or situational distortion that lift with treatment?',
    'Longitudinal desire-for-death studies in palliative populations and capacity-assessment outcome data; track whether wishes persist after symptom control.',
    'Authenticity supports high extraction for denial of exit; systematic distortion supports the protective-coordination reading and lowers the autonomy reading''s assessment of the standing arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(request_authenticity_under_suffering, empirical, 'Authenticity of end-of-life death-wishes as the pivot between extraction and protection readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dignified_death_ap_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dignified_death_ap_tr_t6, dignified_death__autonomy_primary, theater_ratio, 6, 0.26).
narrative_ontology:measurement(dignified_death_ap_tr_t12, dignified_death__autonomy_primary, theater_ratio, 12, 0.3).
narrative_ontology:measurement(dignified_death_ap_tr_t18, dignified_death__autonomy_primary, theater_ratio, 18, 0.33).
narrative_ontology:measurement(dignified_death_ap_tr_t24, dignified_death__autonomy_primary, theater_ratio, 24, 0.36).
narrative_ontology:measurement(dignified_death_ap_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(dignified_death_ap_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(dignified_death_ap_be_t6, dignified_death__autonomy_primary, base_extractiveness, 6, 0.47).
narrative_ontology:measurement(dignified_death_ap_be_t12, dignified_death__autonomy_primary, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(dignified_death_ap_be_t18, dignified_death__autonomy_primary, base_extractiveness, 18, 0.53).
narrative_ontology:measurement(dignified_death_ap_be_t24, dignified_death__autonomy_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(dignified_death_ap_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.56).

% Suppression requirement over time
narrative_ontology:measurement(dignified_death_ap_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dignified_death_ap_su_t6, dignified_death__autonomy_primary, suppression_requirement, 6, 0.59).
narrative_ontology:measurement(dignified_death_ap_su_t12, dignified_death__autonomy_primary, suppression_requirement, 12, 0.63).
narrative_ontology:measurement(dignified_death_ap_su_t18, dignified_death__autonomy_primary, suppression_requirement, 18, 0.66).
narrative_ontology:measurement(dignified_death_ap_su_t24, dignified_death__autonomy_primary, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(dignified_death_ap_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The colloquial label 'dignified death' decomposes into three structurally distinct constraints — readings of one kernel — per the epsilon-invariance principle: autonomy_primary (this file; victim set = those denied exit), sanctity_primary (victim set inverted: those whose deaths are intentionally hastened), and relational_autonomy (victim set mediated by triad procedure). They differ in epsilon, beneficiary structure, and failure modes, so they are modeled as separate stories linked by affects_constraints, not one story with a measurement parameter. Upstream/downstream: sanctity_primary historically grounded the prohibition this reading contests; relational_autonomy operates as the mediating position in clinical practice and absorbs structural pressure from both.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
