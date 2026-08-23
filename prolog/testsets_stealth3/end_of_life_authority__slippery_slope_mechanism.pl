% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__slippery_slope_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__slippery_slope_mechanism, []).

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
 *   constraint_id: end_of_life_authority__slippery_slope_mechanism
 *   human_readable: Assisted-Dying Eligibility Expansion Ratchet (Slippery-Slope Reading)
 *   domain: bioethics/end-of-life policy
 *
 * SUMMARY:
 *   A jurisdiction (analyzed here as a composite of the Netherlands, Belgium,
 *   and Canada, whose trajectories dominate the empirical literature) enacts
 *   assisted dying for competent, terminally ill adults under strict
 *   safeguards. Over the following two decades the eligibility boundary
 *   moves: to chronic degenerative illness without terminal prognosis, to
 *   psychiatric suffering as a sole condition, to minors in one jurisdiction,
 *   to dementia patients acting on previously signed advance directives, and
 *   to newborns under a separate pediatric protocol. Annual case volumes grow
 *   by orders of magnitude, monitoring data begin recording 'burden on
 *   others' among stated motivations, and official bodies schedule further
 *   widening. This story instantiates the SLIPPERY-SLOPE-MECHANISM reading of
 *   the end-of-life-authority kernel: it treats the expansion dynamic itself
 *   as the operative constraint. The epsilon referent is the STANDING
 *   arrangement under contest - the actual expanding regime as it operates
 *   today - assessed by this reading's own lights: as a structure that
 *   coordinates genuine access for its founding class while transferring
 *   life-deciding authority over people who did not and cannot consent.
 *   Assumptions stated: T=0 maps to 2002 (Dutch and Belgian acts in force);
 *   T=12 to the 2014-2015 cluster (Belgian minor extension, Quebec statute,
 *   Carter decision); T=19 to Canada's removal of the
 *   reasonably-foreseeable-death requirement; T=27 projects the scheduled
 *   mental-disorder track and comparable pending extensions. Claim/metric
 *   independence holds: this seat CLAIMS tangled_rope (real coordination
 *   function for the founding class, asymmetric extraction through expansion)
 *   while the metrics are authored descriptively of the observed trajectory,
 *   and the engine computes per-seat classifications from the structural data
 *   without reference to this claim.
 *
 * KEY AGENTS:
 *   - - competent_terminal_patients: Founding beneficiary class (powerless/trapped) - receives lawful supervised death on request
 *   - - right_to_die_advocacy_organizations: Structural beneficiary (organized/mobile) - mission, funding, and docket grow with each widening
 *   - - health_system_cost_controllers: Contested beneficiary (institutional/arbitrage) - records fiscal relief from substitution of procedure for prolonged care
 *   - - incompetent_dementia_patients_under_directives: Primary target (powerless/trapped) - bound by banked consent, cannot revoke at the operative moment
 *   - - chronic_nonterminal_disabled_patients: Primary target (organized/constrained) - swept inside eligibility without terminal prognosis; organize resistance
 *   - - psychiatric_suffering_applicants: Target with dual position (powerless/constrained) - lose protective presumption; some individually seek access
 *   - - burden_motivated_vulnerable_patients: Target (powerless/constrained) - consent formed under deprivation the eligibility form does not record
 *   - - groningen_protocol_infants: Excluded party (powerless/trapped) - affected without possibility of consultation at any point
 *   - - eligibility_review_bodies: Agenda-setter (institutional/constrained) - interprets criteria, audits retrospectively, recommends further adjustment
 *   - - palliative_care_practitioners: Agenda-setter with dual position (organized/constrained) - executes the practice; bears moral distress and conscience pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, 0.67).
domain_priors:suppression_score(end_of_life_authority__slippery_slope_mechanism, 0.6).
domain_priors:theater_ratio(end_of_life_authority__slippery_slope_mechanism, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, extractiveness, 0.67).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_authority__slippery_slope_mechanism, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__slippery_slope_mechanism, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__slippery_slope_mechanism, "Assisted-Dying Eligibility Expansion Ratchet (Slippery-Slope Reading)").
narrative_ontology:topic_domain(end_of_life_authority__slippery_slope_mechanism, "bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__slippery_slope_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__slippery_slope_mechanism, '9c9c6b70-2388-4175-9e47-81f8a56c0108').
narrative_ontology:cs_kernel_codification('9c9c6b70-2388-4175-9e47-81f8a56c0108', formalized).
narrative_ontology:cs_authority_grounding('9c9c6b70-2388-4175-9e47-81f8a56c0108', expertise).
narrative_ontology:cs_interpretation_layer_present('9c9c6b70-2388-4175-9e47-81f8a56c0108').
narrative_ontology:cs_reading_relation('9c9c6b70-2388-4175-9e47-81f8a56c0108', end_of_life_authority__autonomy_reading, influences).
narrative_ontology:cs_reading_relation('9c9c6b70-2388-4175-9e47-81f8a56c0108', end_of_life_authority__sanctity_reading, influences).
narrative_ontology:cs_axiom('9c9c6b70-2388-4175-9e47-81f8a56c0108', foundational, eligibility_ratchet_irreversibility).
narrative_ontology:cs_axiom_status(eligibility_ratchet_irreversibility, holdable).
narrative_ontology:cs_axiom_grounding('9c9c6b70-2388-4175-9e47-81f8a56c0108', eligibility_ratchet_irreversibility, empirically_contingent).
narrative_ontology:cs_axiom('9c9c6b70-2388-4175-9e47-81f8a56c0108', foundational, vulnerable_protection_lexical_priority).
narrative_ontology:cs_axiom_status(vulnerable_protection_lexical_priority, holdable).
narrative_ontology:cs_axiom_grounding('9c9c6b70-2388-4175-9e47-81f8a56c0108', vulnerable_protection_lexical_priority, deontological).
narrative_ontology:cs_reference_frame('9c9c6b70-2388-4175-9e47-81f8a56c0108', safeguarded_terminal_compact).
narrative_ontology:cs_drift_state('9c9c6b70-2388-4175-9e47-81f8a56c0108', contemporary_expansion_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9c9c6b70-2388-4175-9e47-81f8a56c0108', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, right_to_die_advocacy_organizations).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, health_system_cost_controllers).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, incompetent_dementia_patients_under_directives).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, chronic_nonterminal_disabled_patients).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_applicants).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, burden_motivated_vulnerable_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, chronic_nonterminal_disabled_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_applicants).
narrative_ontology:constraint_victim(end_of_life_authority__slippery_slope_mechanism, palliative_care_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face unbearable suffering from a terminal illness with intact decision-making capacity. The statutory framework gives them a lawful, medically supervised, reported death on request, with waiting periods, second opinions, and capacity screening defining them as the paradigm case the original statutes were written for. Declining the procedure remains open at every stage.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, competent_terminal_patients, beneficiary,
    powerless, immediate, trapped, national).

% Mission-driven organizations that litigated and lobbied for the original narrow statutes and continue litigating and lobbying afterward. Each eligibility widening enlarges their client class, membership, funding base, and litigation docket, and they coordinate strategy across jurisdictions through international networks. Their continued institutional existence depends on there being further boundaries to move.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, right_to_die_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Ministries, insurers, and regional health authorities responsible for budgets. Published analyses in several jurisdictions show a medically assisted death costs a fraction of prolonged palliative or acute care, and auditors have recorded system-level savings. Whether these actors actively prefer expansion or merely record the arithmetic is disputed; the incentive gradient exists regardless, and it sharpens wherever home care and palliative services are rationed.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, health_system_cost_controllers, beneficiary,
    institutional, generational, arbitrage, national).

% People diagnosed with dementia who signed advance directives while competent requesting euthanasia at a later stage of decline. When the appointed stage arrives they can no longer affirm, renegotiate, or revoke the instruction; physicians interpret the old document against their current condition, sometimes with sedation to manage resistance to the procedure. Nothing in the present can be exchanged for withdrawal of consent because the consent was banked years earlier.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, incompetent_dementia_patients_under_directives, payer,
    powerless, biographical, trapped, national).

% Live for decades with serious disabilities or chronic illnesses that are not terminal. Once eligibility widens beyond terminal prognosis, their diagnoses fall inside the boundary; parliamentary committees and media investigations in several jurisdictions have collected testimony of clinicians raising assisted death unprompted during conversations about care options, and of people choosing it because adequate home support was unavailable. They organize through disability rights movements to oppose widening while some among them individually seek access.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, chronic_nonterminal_disabled_patients, payer,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, chronic_nonterminal_disabled_patients, beneficiary).

% People whose sole qualifying condition is a mental disorder or chronic psychiatric suffering. They stand where capacity assessment is least settled and where treatability judgments are most contested; several have been approved and die under frameworks originally justified exclusively by physical terminal illness. Some genuinely seek release and experience the widening as access; the class as a whole loses the protective presumption that psychiatric crisis is presumptively treated rather than ended.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_applicants, payer,
    powerless, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, psychiatric_suffering_applicants, beneficiary).

% Patients who cite feeling like a burden on family, caregivers, or the system among their reasons for requesting assisted death. Official monitoring data in at least one major jurisdiction record this motivation in a substantial and growing share of cases. Their choice environment is shaped by income support levels, home-care wait times, and caregiver exhaustion, none of which appear anywhere on the statutory eligibility form.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, burden_motivated_vulnerable_patients, payer,
    powerless, immediate, constrained, national).

% Newborns with severe congenital conditions covered by pediatric end-of-life protocols permitting deliberate life-ending after multidisciplinary and parental agreement, reported to prosecutorial review boards after the fact. They are the clearest case of parties affected by the framework who cannot be consulted at any point before the decision, and whose inclusion was never put to the electorate that approved the adult statutes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, groningen_protocol_infants, excluded,
    powerless, immediate, trapped, national).

% Statutory review committees, regional euthanasia review boards, and federal expert panels that interpret the criteria, audit reported cases, issue compliance findings, and recommend legislative adjustment. Their staffing, mandate, and case volumes grow with each widening, and their retrospective rather than prospective review structure means borderline cases surface only after the practice has normalized.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, eligibility_review_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Physicians and nurses who administer the practice at the bedside, perform the capacity assessments, and deliver the lethal substances. Professional surveys record persistent moral distress, conscientious-objector accommodation disputes, and pressure on objecting physicians to make effective referrals. They execute the arrangement daily while a large minority of their professional bodies formally opposed each successive widening.
narrative_ontology:constraint_stakeholder(end_of_life_authority__slippery_slope_mechanism, palliative_care_practitioners, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__slippery_slope_mechanism, palliative_care_practitioners, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__slippery_slope_mechanism, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__slippery_slope_mechanism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single lawful, standardized, medically supervised and publicly reported pathway to voluntary death for patients meeting statutory criteria, replacing clandestine aid-in-dying and unregulated physician conduct with capacity screening, waiting periods, second opinions, mandatory reporting, and retrospective audit.
% TRANSFER_FUNCTION: Moves decision authority over the timing and circumstances of death from the protective prohibition (state and medical tradition acting for everyone) to the individual requester and the assessing physician; moves final-phase care spending from prolonged treatment and palliative budgets toward a low-cost procedure; and moves the moral labor of killing from the anonymous institution to identifiable individual clinicians.
% ABSENT_VOICES: Newborns under pediatric end-of-life protocols, children below assent thresholds in jurisdictions that have extended to minors, the future incompetent selves of today's directive signers, and disability communities that entered consultations late and describe their submissions as noted rather than heeded. Palliative medicine voices describe marginalization during drafting in several jurisdictions.
% DISAPPEARANCE_RATIONALE: Tens of thousands of deaths per year across the affected jurisdictions currently flow through these statutes. Overnight repeal would strand patients mid-process, dissolve review machinery and clinician training programs, push demand back into covert aid-in-dying and cross-border travel, and relaunch the constitutional litigation that produced the frameworks, since courts in at least two jurisdictions have framed access in rights terms that repeal would immediately re-test.
% FOUNDING_PROBLEM: Competent, terminally ill patients suffering unbearably had no lawful way to end their lives with medical assistance, forcing clandestine practices, exposing physicians to prosecution, and leaving some deaths badly managed.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: parliamentary hearing transcripts and expert-panel reports record disability scholars and palliative specialists attesting that the narrow founding problem was substantially addressed and that subsequent persistence of 'unsolved' status tracks redefinition of the qualifying class rather than new facts; provincial auditor and parliamentary budget office analyses document the changed fiscal profile; Council of Europe assembly resolutions and peer-reviewed epidemiology of practice growth independently attest the expansion pattern the founding-problem debate now turns on.
narrative_ontology:disappearance_verdict(end_of_life_authority__slippery_slope_mechanism, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__slippery_slope_mechanism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__slippery_slope_mechanism, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__slippery_slope_mechanism, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__slippery_slope_mechanism, 0.67, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__slippery_slope_mechanism_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__slippery_slope_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__slippery_slope_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.67 for the standing arrangement because the transfer now regularly reaches people who never consented in the present tense: directive-bound dementia patients, sole-condition psychiatric applicants, and patients whose stated motivation includes burden they attribute to unsupported caregiving. Suppression (0.60, unscaled by construction - only extractiveness is scaled by directionality and scope in the engine) reflects that the boundary is held open by active legal machinery: rights-framed jurisprudence that recasts every re-narrowing as a rights violation, retrospective-only review that normalizes borderline cases after the fact, and referral obligations that discipline professional dissent. Theater ratio rises across the interval because the safeguard apparatus (criteria, waiting periods, second opinions) continues to be displayed as the guarantee against expansion while expansion proceeds through the apparatus itself - each widening is ratified by the same committees that certify the safeguards. Accessibility collapse is moderate-low (0.40): prohibition persists in many jurisdictions, palliative-first models operate elsewhere, and the comparison between systems remains visible, so alternatives do not vanish upon understanding. Resistance is high (0.68): disability rights organizations, palliative care bodies, faith coalitions, and minority factions of medical associations mount sustained, documented opposition - though the trajectory shows them losing ground. The measurement series run on ONE SHARED GRID (T=0,4,8,12,16,19,23,27) with all three tracked metrics authored at every point; the trajectory is monotonic rather than cyclic - this is a ratchet, not an oscillation - and the scalar base_properties values describe the current standing arrangement (approximately T=24 on the grid), with the final grid point carrying a projected value for the scheduled next expansion. Enforcement-capacity intensification is the traced dynamic here, hence suppression_requirement is tracked; its rise models the maturing compliance and normalization machinery, not changing extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is structural. From the founding beneficiary seat, the arrangement is a delivered promise: lawful release at the end of terminal illness, exactly as voted. From the directive-bound dementia seat, the same statutes operate as a mechanism by which a past self extinguishes the present self's standing to refuse. From the advocacy-organization seat, each widening is mission progress; from the disability-community seat, each widening is the removal of another wall between their diagnosis and a state-supervised death offered inside a system that declined to fund their home care. Between institutional seats: courts read widening as equal-treatment fulfillment, review bodies as administrative continuity, and cost controllers as neutral arithmetic - while bedside clinicians absorb the moral residue. Same-power divergence: competent terminal patients and incompetent directive-bound patients sit at identical nominal power (powerless) yet occupy opposite positions, differentiated not by power but by WHEN consent is possible relative to the irreversible act - the constraint-specific factor the derivation chain reads through exit options (both trapped, but only one retains present-tense refusal).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionalities: the founding patient class is subsidized (receives the service, pays nothing the statute counts), advocacy organizations collect mission-capital with mobile exit (nearest the beneficiary end), and cost controllers derive partial benefit with arbitrage-grade flexibility. Victim declarations drive high directionalities, amplified by exit structure: dementia patients under directives are trapped with zero present-tense revocation (nearest the full-target end); chronic non-terminal disabled patients are constrained (cannot exit the diagnosis or the governing health system); burden-motivated patients are constrained by economic dependence on the very system offering the procedure; psychiatric applicants are constrained and additionally carry the contested-capacity discount on the validity of whatever consent they give. The excluded infant class sits outside derivation as a non-consenting party but anchors the reading's claim that the boundary's outer edge has already moved past every population the original vote contemplated. Spatial scope is national for most seats, producing modest verification-hardening amplification in the engine's computation; the advocacy network's global scope lowers its effective coupling to any single statute. No directionality overrides are authored: the derivation from declared roles plus exit options produces the correct ordering, and the one genuinely ambiguous seat (cost controllers, beneficiary-but-contested) is handled by an omega rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   This is mandate METASTASIS, not mandate atrophy, and the distinction governs the classification. A piton is a constraint whose function died while the shell persisted; here the function did not die - it annexed adjacent territory. The founding problem (competent terminal suffering with no lawful option) was addressed, and the arrangement persists by continuously re-authoring the problem it solves: each expansion redefines 'unbearable,' 'irremediable,' and 'reasonably foreseeable' so the mandate never completes. The mandatrophy consumer therefore reads founding_problem_status=contested (not dead): beneficiaries attest the problem is live because their definition of it keeps moving, while outside corroborators attest the original formulation was satisfied. Because the verdict is world_rearranges and the status is contested rather than dead, no zombie-flag mismatch fires - correctly, since the arrangement is actively growing, not inertially maintained. The tangled_rope claim prevents the opposite error: reading the expansion as proof the entire framework was always a snare would erase the genuine, documented coordination benefit delivered to the founding class; reading the founding-class benefit as the whole story would erase the four populations now bearing costs they never agreed to. The hybrid classification holds both facts in one structure, which is precisely what the slippery-slope reading asserts about reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story is one reading (slippery_slope_mechanism) of the end_of_life_authority kernel. At WHICH structural element do the sibling readings locate their disagreement, and what would each sibling re-author?',
    'Not resolvable by data alone; the disagreement is located in the victim-set definition and the sign of epsilon. The autonomy_reading would re-author epsilon low (liberation extended, victims shrinking to the unwillingly-blocked) over the same standing arrangement; the sanctity_reading would extend the victim set to EVERY recipient of an intentional life-ending act and author epsilon near maximal. Comparative outcome data on vulnerable-class harm rates informs but does not settle which reading''s authorship is correct.',
    'If the autonomy reading prevails, this constraint recomputes as a low-extraction rope and the expansion reads as rights fulfillment; if the sanctity reading prevails, it recomputes toward snare with a universal victim set. The present story''s tangled_rope authorship is the middle position: real coordination for the founding class, asymmetric transfer to the newly swept-in.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one of three readings of the end-of-life-authority kernel; sibling readings re-author epsilon and victim sets over the same referent.').

omega_variable(
    capacity_assessment_validity,
    'Can clinicians validly assess decision-making capacity for an irrevocable act in the populations added by expansion - psychiatric suffering as sole condition, and requests executed on behalf of currently incompetent patients - or does the eligibility machinery systematically overestimate capacity exactly where it matters most?',
    'Independent audit of capacity determinations in sole-condition and directive-executed cases, with blinded re-assessment against structured instruments and long-term outcome follow-up where the act is averted.',
    'Systematic overestimation would invalidate the consent foundation of the expanded categories, raise effective extraction sharply for those classes, and push the computed type toward snare for the expanded tracks while leaving the founding track''s coordination intact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_assessment_validity, empirical, 'Whether the capacity screen discriminates reliably in the newly added populations.').

omega_variable(
    burden_motivation_contamination,
    'Are ''burden on others'' motivations recorded in official monitoring data autonomous expressions, or are they structurally induced by home-care rationing, disability poverty, and caregiver collapse - and what share of requests do they materially drive?',
    'Linked-dataset analysis correlating request timing and stated motivation with local home-care wait times, income support levels, and caregiver support indices; qualitative interview studies of withdrawn and completed requests.',
    'If a material share of consent is deprivation-formed, the competent-consent foundation fails for that subset, effective extraction rises above the authored value, and palliative-funding remedies become classification-relevant rather than peripheral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_motivation_contamination, empirical, 'Whether burden-citing requests reflect free preference or engineered preference.').

omega_variable(
    ratchet_reversibility,
    'Is the outward drift of eligibility boundaries structurally inevitable once autonomy-rights framing is established (each re-narrowing litigable as a rights violation), or contingent on specific jurisprudence and coalition configurations that a sufficiently determined legislature could stabilize?',
    'Comparative analysis of jurisdictions that attempted re-narrowing or indefinite delay versus those that did not, tracking whether rights-framed litigation, court reinterpretation, or committee recommendation reopened the boundary despite legislative intent.',
    'If the ratchet is structural, the authored trajectory extends and the constraint hardens toward enforced extraction; if contingent, targeted jurisprudential or statutory counterweights could pin the boundary and the measured drift decays.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_reversibility, conceptual, 'Whether the expansion dynamic is a one-way mechanism or a contingent path.').

omega_variable(
    cost_containment_driver_status,
    'Is the documented fiscal advantage of assisted death over prolonged care an active driver of institutional preference for widening, or an incidental accounting fact that never enters deliberation?',
    'Disclosure analysis of internal deliberations, budget projections referencing substitution effects, and differential funding trajectories for palliative versus assisted-death infrastructure in expanding jurisdictions.',
    'If cost containment functions as a driver, health_system_cost_controllers'' directionality is understated by the structural derivation and the extraction asymmetry is deeper than the beneficiary declaration suggests; if incidental, the hedge in that stakeholder''s situation resolves toward pure artifact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cost_containment_driver_status, empirical, 'Driver-versus-artifact status of the fiscal substitution incentive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__slippery_slope_mechanism, 0, 27).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_slope_tr_t0, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 0, 0.18).
narrative_ontology:measurement(eol_slope_tr_t4, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 4, 0.21).
narrative_ontology:measurement(eol_slope_tr_t8, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 8, 0.26).
narrative_ontology:measurement(eol_slope_tr_t12, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 12, 0.31).
narrative_ontology:measurement(eol_slope_tr_t16, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 16, 0.36).
narrative_ontology:measurement(eol_slope_tr_t19, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 19, 0.4).
narrative_ontology:measurement(eol_slope_tr_t23, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 23, 0.45).
narrative_ontology:measurement(eol_slope_tr_t27, end_of_life_authority__slippery_slope_mechanism, theater_ratio, 27, 0.5).

% Extraction over time
narrative_ontology:measurement(eol_slope_be_t0, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eol_slope_be_t4, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 4, 0.35).
narrative_ontology:measurement(eol_slope_be_t8, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 8, 0.43).
narrative_ontology:measurement(eol_slope_be_t12, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(eol_slope_be_t16, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(eol_slope_be_t19, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 19, 0.63).
narrative_ontology:measurement(eol_slope_be_t23, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 23, 0.66).
narrative_ontology:measurement(eol_slope_be_t27, end_of_life_authority__slippery_slope_mechanism, base_extractiveness, 27, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(eol_slope_su_t0, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 0, 0.34).
narrative_ontology:measurement(eol_slope_su_t4, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 4, 0.37).
narrative_ontology:measurement(eol_slope_su_t8, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 8, 0.43).
narrative_ontology:measurement(eol_slope_su_t12, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(eol_slope_su_t16, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 16, 0.53).
narrative_ontology:measurement(eol_slope_su_t19, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 19, 0.56).
narrative_ontology:measurement(eol_slope_su_t23, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 23, 0.59).
narrative_ontology:measurement(eol_slope_su_t27, end_of_life_authority__slippery_slope_mechanism, suppression_requirement, 27, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__slippery_slope_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__slippery_slope_mechanism, end_of_life_authority__sanctity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'end-of-life authority debate' covers three structurally distinct constraints sharing one kernel. This story decomposes it per the epsilon-invariance principle: the AUTONOMY reading (upstream - grounds the framework, low epsilon, founding class as beneficiary) enables the arrangement whose DYNAMICS this story tracks (downstream - the expansion mechanism, epsilon 0.67, four victim classes); the SANCTITY reading stands parallel with a maximally wide victim set and maximal epsilon. Upstream influences downstream: the autonomy framework's existence and its rights-framing jurisprudence are the substrate the ratchet operates on, which is why this story links to both siblings and why its axioms presuppose the framework rather than disputing its existence. Each file carries its own stable epsilon; none averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
