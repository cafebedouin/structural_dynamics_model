% ============================================================================
% CONSTRAINT STORY: dignified_death__relational_autonomy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: dignified_death__relational_autonomy
 *   human_readable: Relational-Autonomy Triad for End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested dignified_death
 *   kernel: the relational-autonomy reading, under which dignity is
 *   constituted relationally and terminal-decision authority is distributed
 *   across the patient-family-clinician triad behind procedural safeguards.
 *   Per the epsilon-referent rule, extractiveness is authored for the
 *   STANDING triad arrangement as this reading assesses it - not for the
 *   pure-autonomy or sanctity arrangements the sibling stories price
 *   separately. The claim/metric gap is deliberate and untuned: the SCOPE
 *   manifest hypothesized rope with moderate epsilon (0.30-0.45); my
 *   descriptive analysis agrees on the epsilon band but lands the type at
 *   tangled_rope, because the arrangement exhibits all three canonical
 *   markers - a genuine coordination function (multi-party legitimacy under
 *   mortal stakes), asymmetric extraction through the same structure
 *   (competent patients' authority diluted to the benefit of the other two
 *   seats; delay costs billed to the least mobile party), and active
 *   enforcement (statutes, ethics committees, certification requirements).
 *   The divergence from the manifest hypothesis is recorded, not reconciled.
 *   KEY AGENTS (by structural relationship): - dying_patients: Primary target
 *   (powerless/trapped) - bear dilution of decision authority and procedural
 *   delay at the end of life - family_caregivers: Primary beneficiary
 *   (organized/constrained) - gain standing, continued influence, and shared
 *   moral weight - attending_clinicians: Agenda-setting beneficiary
 *   (institutional/mobile) - administer the procedure, gatekeep requests,
 *   share liability - hospital_ethics_committees: Secondary agenda-setter
 *   (institutional/constrained) - adjudicate disputes and certify compliance
 *   - unrepresented_patients: Target without advocate (powerless/trapped) -
 *   interests voiced by no triad seat - disability_rights_advocates: Excluded
 *   critic (organized/mobile) - contest gatekeeping from outside the consult
 *   room - clinical_bioethicists: Analytical observer (analytical/analytical)
 *   - sees the full structure including seats that cannot see each other
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__relational_autonomy, 0.4).
domain_priors:suppression_score(dignified_death__relational_autonomy, 0.38).
domain_priors:theater_ratio(dignified_death__relational_autonomy, 0.27).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, extractiveness, 0.4).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, theater_ratio, 0.27).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(dignified_death__relational_autonomy, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__relational_autonomy, tangled_rope).
narrative_ontology:human_readable(dignified_death__relational_autonomy, "Relational-Autonomy Triad for End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__relational_autonomy, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__relational_autonomy).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__relational_autonomy, 'e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6').
narrative_ontology:cs_kernel_codification('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', distributed).
narrative_ontology:cs_authority_grounding('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', expertise).
narrative_ontology:cs_interpretation_layer_present('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6').
narrative_ontology:cs_reading_relation('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_axiom('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', foundational, dignity_is_relationally_constituted).
narrative_ontology:cs_axiom_status(dignity_is_relationally_constituted, holdable).
narrative_ontology:cs_axiom_grounding('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', dignity_is_relationally_constituted, deontological).
narrative_ontology:cs_axiom('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', foundational, triad_concurrence_confers_legitimacy).
narrative_ontology:cs_axiom_status(triad_concurrence_confers_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', triad_concurrence_confers_legitimacy, conventional).
narrative_ontology:cs_reference_frame('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', relational_triad_consensus).
narrative_ontology:cs_drift_state('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', contemporary_assisted_dying_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e4fa0ffd-1464-4ba9-9d8d-e85cd8d9f8e6', '').
narrative_ontology:cs_kernel_id(dignified_death__relational_autonomy, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, family_caregivers).
narrative_ontology:constraint_beneficiary(dignified_death__relational_autonomy, attending_clinicians).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, dying_patients).
narrative_ontology:constraint_victim(dignified_death__relational_autonomy, unrepresented_patients).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, relational_autonomy_theory).
narrative_ontology:constraint_vindicates(dignified_death__relational_autonomy, shared_decision_making_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Bear the illness and hold preferences about timing, method, and continuation of treatment. May refuse treatment unilaterally, but affirmative requests (hastened relief, withdrawal contested by others) must route through triad concurrence: family input, clinician agreement, and procedural steps such as waiting periods and second assessments. Physical dependence and shortened timelines mean they cannot wait out the process or shop for a different decision forum.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, dying_patients, payer,
    powerless, immediate, trapped, local).

% Provide daily care, absorb grief and often financial cost, and hold formal standing in the decision process. The arrangement preserves their voice over the timing and manner of a loved one's death and distributes moral weight that would otherwise fall on them alone or on the patient alone. Kinship is not resignable: they cannot exit the bedside, and their standing persists even where their interests (caregiver burden, inheritance, family reputation) diverge from the patient's stated wishes.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, family_caregivers, beneficiary,
    organized, biographical, constrained, local).

% Administer the procedure: convene family meetings, judge capacity, initiate or withhold ethics consultation, and effectively gatekeep affirmative requests by granting or withholding professional concurrence. The arrangement shares moral and legal responsibility that would otherwise concentrate on them, and shields them from unilateral-demand scenarios. They can transfer difficult cases, refer out, or leave practice; the patient cannot transfer out of dying.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, attending_clinicians, agenda_setter,
    institutional, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(dignified_death__relational_autonomy, attending_clinicians, beneficiary).

% Adjudicate disputes among the three seats, certify that procedural safeguards were followed, and set precedent through written opinions that shape later cases. They are part of the machinery they administer: their caseload and authority exist because the distributed-authority arrangement requires an arbiter, and they do not dissolve if any single case ends badly.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, hospital_ethics_committees, agenda_setter,
    institutional, generational, constrained, national).

% Dying patients with no family advocate: estranged elders, outliving spouses, isolated adults. The arrangement presumes a relational network that does not exist for them; their interests are voiced by no seat in the triad, so procedure defaults to clinician judgment or public-guardian routines. They bear the same dilution of authority as represented patients without any of the compensating voice.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, unrepresented_patients, payer,
    powerless, immediate, trapped, local).

% Organizations that contest the gatekeeping architecture from outside the consult room: they argue that requiring clinician and family concurrence converts a personal decision into a permission slip granted by parties with stakes in the requester's continued existence, and that procedural 'safeguards' screen out exactly the disabled and dependent requesters the process claims to protect. They litigate, testify, and publish but hold no seat in the triad itself.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% Scholars and consultants who analyze the arrangement across cases and jurisdictions: they document override patterns, compare jurisdictions that locate final authority differently, and supply the theoretical vocabulary the participants use. They collect no rents from any particular resolution and can see the whole structure, including the seats that cannot see each other.
narrative_ontology:constraint_stakeholder(dignified_death__relational_autonomy, clinical_bioethicists, observer,
    analytical, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__relational_autonomy, family_caregivers).
narrative_ontology:fixing_cost_class(dignified_death__relational_autonomy, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Terminal decisions implicate multiple legitimately-interested parties at once: the patient who bears the experience, the family who bears the caregiving and the grief, and the clinicians who must administer whatever is decided. The triad procedure coordinates their concurrence so that a death decision carries relational legitimacy, and so that no single party's interest silently dominates - neither clinician convenience, nor family preference, nor an isolated patient's worst hour.
% TRANSFER_FUNCTION: Moves decision authority, and the moral responsibility attached to it, from the dying patient toward the family-clinician collective; moves deference and accommodation from family and clinicians toward the patient's protected refusal rights; and moves time - procedural days and weeks - from everyone into deliberation, with the delay bill landing disproportionately on the person with the least time.
% ABSENT_VOICES: Unrepresented patients have no seat and no advocate in the room. Disability-rights critics of gatekeeping stand outside the consult and are heard only when they litigate. The patient's own earlier, capacitated voice is absent whenever advance documentation was never made. Conscientious-objecting clinicians are inside the institution but outside the procedure's premises. Each absence shapes outcomes the present seats then record as consensus.
% DISAPPEARANCE_RATIONALE: Hospice protocols, surrogate-decision statutes, ethics-committee jurisdiction, and assisted-dying eligibility pathways all presuppose triad concurrence. Remove the arrangement overnight and every terminal decision falls back to whichever single locus each jurisdiction's default law selects - physician judgment, next-of-kin fiat, or patient demand - reopening the exact conflicts the arrangement was built to settle, case by case, with no shared procedure.
% FOUNDING_PROBLEM: Mid-twentieth-century medicine produced two documented failure modes at the deathbed: physician paternalism deciding life and death unilaterally over family objection, and later, atomistic patient sovereignty stranding incapacitated and isolated patients with no one authorized to speak or act for them. The arrangement was built to solve the legitimacy problem of terminal decisions made in a web of mutual dependency, where the decider is never the sole bearer of the consequences.
% FOUNDING_PROBLEM_CORROBORATION: The founding failures are attested outside the benefiting parties: published appellate opinions on withdrawal of treatment and surrogate authority, legislative hearing records, and the palliative-care literature on unwanted overtreatment and abandoned incapacitated patients. These sources corroborate that the problem the arrangement addresses is real and ongoing. No source wholly outside the family and clinician beneficiary set, however, certifies the triad as the correct remedy rather than one contested answer among the kernel's readings - the corroboration covers the problem, not the solution.
narrative_ontology:disappearance_verdict(dignified_death__relational_autonomy, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__relational_autonomy, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__relational_autonomy, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__relational_autonomy, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__relational_autonomy, 0.4, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness 0.40 sits in the upper-middle of the manifest band: the arrangement's costs concentrate on precisely the agents with zero exit capacity (the dying cannot wait out a committee, appeal to another jurisdiction mid-course in most systems, or re-enter the conversation after incapacity), while its benefits spread across the surviving seats. Suppression 0.38 is authored as a raw structural property, unscaled by power or scope: patients retain strong negative rights (unilateral refusal of treatment), so the coercive edge applies mainly to affirmative requests, enforced through soft institutional gating rather than hard sanction. Theater_ratio 0.27 is low-to-moderate: the deliberative function is mostly real, but a growing share of activity is compliance documentation - checkbox second opinions, boilerplate capacity findings, meeting minutes produced for the file - which the temporal series shows rising steadily (0.10 to 0.27) as the arrangement formalized. Accessibility_collapse 0.35: alternatives persist and are used - unilateral refusal, advance directives, jurisdictional travel to patient-sovereign regimes - so understanding the arrangement does not close the option space. Resistance 0.45: sustained contest from autonomy-primary advocates and disability-rights critics on one flank and sanctity-side objectors on the other. All three tracked series run on ONE shared grid (t=0,4,8,12,16,20,24) so every metric is authored at every examined point; the flat tail (t=20 to t=24) represents the current equilibrium after the formalization ratchet. The suppression_requirement series is included because the story specifically traces enforcement-capacity growth - statutory waiting periods, mandatory second assessments, and committee certification hardened over the interval - not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats should compute differently, and the structural data supports that divergence. From the family seat, the arrangement is care: it keeps them at the table, prevents both abandonment and solo moral crushing, and honors the web the patient actually lives in. From the competent patient's seat, the same procedure converts a personal decision into a negotiation in which the other two voters hold standing, information, and time advantages - and in which a unanimous family can bury a documented wish. From the clinician seat it is legitimate shared governance; from the unrepresented patient's seat it is a quorum they were never added to. The engine computes per-seat classifications from the declared positions and exit profiles; nothing in the authored claim adjudicates which seat's experience is 'the' constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Family_caregivers and attending_clinicians are declared beneficiaries and derive low directionality (subsidized seats): the arrangement preserves their voice and distributes burden onto shared machinery. Dying_patients and unrepresented_patients are declared victims with trapped exit, placing them near the full-target end - extraction amplified by their inability to leave. Attending_clinicians carry a secondary beneficiary role marking their dual position (they also absorb workload and moral distress), but their net structural position remains collector-side: deference and liability-sharing exceed what the procedure costs them, so no directionality override is warranted - the derivation from the beneficiary declaration captures the true sign. Hospital_ethics_committees derive near-symmetric: they administer rather than collect. The excluded and observer seats (disability_rights_advocates, clinical_bioethicists) feed the absent-voices and consensus-provenance checks rather than the directionality arithmetic. National-scale statutory scope modestly amplifies effective extraction through verification difficulty; that scaling is the engine's arithmetic, not an authored value.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live: aging populations, life-extending technology, and capacity loss guarantee a continuing stream of terminal decisions made inside webs of dependency, so no mandatrophy is declared and none is due. The tangled_rope classification earns its keep by blocking both symmetrical errors: reading the arrangement as pure extraction (snare) erases the documented protective function for isolated and incapacitated patients that judicial records corroborate; reading it as pure coordination (rope) erases the equally documented override victims and the gatekept requesters the disability-rights literature records. Watch item: if the safeguard apparatus continues its theater_ratio climb past 0.5 - safeguards performed as signature rituals rather than deliberation - the arrangement drifts toward inertial maintenance, and the classification should be revisited with the piton cost-asymmetry test: administrators who could simplify the procedure but bear little of its delay costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint is one reading of the dignified_death kernel (reading: relational_autonomy). What would the sibling readings change structurally, and where exactly does the disagreement bite?',
    'Comparative classification of the sibling stories (dignified_death__autonomy_primary, dignified_death__sanctity_primary): victim sets, epsilon, and type shift with the locus of final authority. Under autonomy_primary the victim set collapses toward third parties overridden by patient will and the triad machinery dissolves; under sanctity_primary the procedural machinery itself becomes the violation and the victim set expands to all intentionally terminated lives.',
    'This story''s epsilon (0.40) and victim set (dying_patients, unrepresented_patients) are valid only for the relational reading. Cross-reading epsilon comparison without decomposition would average structurally distinct constraints and fabricate a verdict.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer structure: which reading of the dignified_death kernel this constraint instantiates and what siblings would change.').

omega_variable(
    family_authority_valence,
    'Is family authority within the triad predominantly protective (guarding against clinician haste, system indifference, and the isolated patient''s worst hour) or predominantly extractive (advancing caregiver-burden, inheritance, and family-standing interests against the patient''s stated wishes)?',
    'Audit of documented cases where triad consensus departed from the patient''s capacitated, documented wishes: classify outcomes by who gained (relief and fidelity vs. continued control, preserved estate, reduced care obligation), controlling for capacity disputes.',
    'If protective dominates, effective extraction drops toward the rope boundary and the coordination floor absorbs more of the measured cost; if extractive dominates, epsilon rises toward the snare boundary and the family seat''s directionality should be overridden upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_authority_valence, empirical, 'Valence of family influence: protection versus interest-driven override.').

omega_variable(
    internalized_burden_deference,
    'Is the measured non-contention of dying patients consent or suppressed dissent - specifically, how much of their acquiescence to triad outcomes is internalized (''a burden on my family must not extend my life'') rather than a response to any external barrier?',
    'Post-procedure wish-stability interviews conducted with patients alone, separated from family presence and clinician framing: if stated preferences revert or surface for the first time in solo settings, the suppression was carried internally, not imposed by the procedure''s barriers.',
    'If substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests - patients carry the deference with them into every forum, and no procedural reform alone releases it; the omega then splits suppression into structural and internalized components for reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_burden_deference, empirical, 'Structural versus internalized suppression mechanism in patient acquiescence.').

omega_variable(
    attachment_frame_gaming,
    'Does the attachment_coordination framing of the arrangement launder control interests as care - that is, is ''family involvement'' invoked where the operative interest is continued authority over the dying member''s body, timing, and estate?',
    'Discourse and outcome analysis of family objections: distinguish objections grounded in the patient''s own prior expressed values from objections grounded in family interest, and test whether the arrangement''s rhetoric tracks the former while its outcomes track the latter.',
    'If the attachment frame functions as cover, the coordination-type floor (0.08) is too generous for this constraint and excess extraction should be flagged for review; the identity-coordination gaming warning applies symmetrically to attachment framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attachment_frame_gaming, conceptual, 'Whether relational framing masks extraction as care.').

omega_variable(
    delay_cost_attribution,
    'Are the procedural delay costs borne by requesting patients an inherent price of legitimate multi-party deliberation, or strategically deployed gatekeeping that filters out weak, poor, and dependent requesters?',
    'Compare approval latency and attrition across jurisdictions with different waiting-period lengths and concurrence requirements, stratified by requester resources and advocacy support: if attrition concentrates among the least resourced as delay lengthens, the delay is functioning as a filter, not a safeguard.',
    'If inherent, part of the measured extraction is the true coordination price and epsilon should be read at the low end of its band; if strategic, the delay component is extraction riding the procedure and epsilon should be read at the high end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(delay_cost_attribution, empirical, 'Attribution of procedural delay: deliberation cost versus gatekeeping filter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__relational_autonomy, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dd_relational_auto_tr_t0, dignified_death__relational_autonomy, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dd_relational_auto_tr_t4, dignified_death__relational_autonomy, theater_ratio, 4, 0.13).
narrative_ontology:measurement(dd_relational_auto_tr_t8, dignified_death__relational_autonomy, theater_ratio, 8, 0.16).
narrative_ontology:measurement(dd_relational_auto_tr_t12, dignified_death__relational_autonomy, theater_ratio, 12, 0.19).
narrative_ontology:measurement(dd_relational_auto_tr_t16, dignified_death__relational_autonomy, theater_ratio, 16, 0.22).
narrative_ontology:measurement(dd_relational_auto_tr_t20, dignified_death__relational_autonomy, theater_ratio, 20, 0.25).
narrative_ontology:measurement(dd_relational_auto_tr_t24, dignified_death__relational_autonomy, theater_ratio, 24, 0.27).

% Extraction over time
narrative_ontology:measurement(dd_relational_auto_be_t0, dignified_death__relational_autonomy, base_extractiveness, 0, 0.26).
narrative_ontology:measurement(dd_relational_auto_be_t4, dignified_death__relational_autonomy, base_extractiveness, 4, 0.31).
narrative_ontology:measurement(dd_relational_auto_be_t8, dignified_death__relational_autonomy, base_extractiveness, 8, 0.35).
narrative_ontology:measurement(dd_relational_auto_be_t12, dignified_death__relational_autonomy, base_extractiveness, 12, 0.37).
narrative_ontology:measurement(dd_relational_auto_be_t16, dignified_death__relational_autonomy, base_extractiveness, 16, 0.39).
narrative_ontology:measurement(dd_relational_auto_be_t20, dignified_death__relational_autonomy, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(dd_relational_auto_be_t24, dignified_death__relational_autonomy, base_extractiveness, 24, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(dd_relational_auto_su_t0, dignified_death__relational_autonomy, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(dd_relational_auto_su_t4, dignified_death__relational_autonomy, suppression_requirement, 4, 0.26).
narrative_ontology:measurement(dd_relational_auto_su_t8, dignified_death__relational_autonomy, suppression_requirement, 8, 0.3).
narrative_ontology:measurement(dd_relational_auto_su_t12, dignified_death__relational_autonomy, suppression_requirement, 12, 0.33).
narrative_ontology:measurement(dd_relational_auto_su_t16, dignified_death__relational_autonomy, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(dd_relational_auto_su_t20, dignified_death__relational_autonomy, suppression_requirement, 20, 0.38).
narrative_ontology:measurement(dd_relational_auto_su_t24, dignified_death__relational_autonomy, suppression_requirement, 24, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__relational_autonomy, attachment_coordination).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__relational_autonomy, informed_consent_doctrine).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the dignified_death kernel per the epsilon-invariance principle: the colloquial label 'a dignified death' conflates three structurally distinct claims about the locus of decision authority and the permissibility of intentional termination. Each reading gets its own story, epsilon, victim set, and classification; this file prices the relational-autonomy arrangement only. Upstream/downstream structure: relational_autonomy arises historically downstream of autonomy_primary (as a critique of atomistic self-determination) and inherits informed_consent_doctrine's procedural vocabulary; sanctity_primary stands outside the permissive lineage entirely. Edges here link the family members for contamination-propagation analysis; they do not merge the readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
